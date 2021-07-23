/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/

//-------------------------------------------------------------
// Usually we include all sources one file so no internal 
// symbols are public in the libray.
// 
// You can compile the entire library just as: 
// $ gcc -c src/repline.c
//-------------------------------------------------------------
#if !defined(RP_SEPARATE_OBJS)
# define _CRT_SECURE_NO_WARNINGS  // for msvc
# include "editline.c"
# include "undo.c"
# include "history.c"
# include "completers.c"
# include "completions.c"
# include "term.c"
# include "tty_esc.c"
# include "tty.c"
# include "stringbuf.c"
# include "common.c"
#endif

//-------------------------------------------------------------
// includes
//-------------------------------------------------------------
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "../include/repline.h"
#include "common.h"
#include "env.h"


//-------------------------------------------------------------
// Readline
//-------------------------------------------------------------

static char*  rp_getline( alloc_t* mem );

rp_public char* rp_readline(rp_env_t* env, const char* prompt_text) 
{
  if (env == NULL) return NULL;
  if (!env->noedit) {
    // terminal editing enabled
    return rp_editline(env, prompt_text);   // in editline.c
  } 
  else {
    // no editing capability (pipe, dumb terminal, etc)
    if (env->tty != NULL) {
      // if the terminal is not interactive, but we are reading from the tty (keyboard), we display a prompt
      if (prompt_text != NULL) term_write(env->term, prompt_text);
      term_write(env->term, (env->prompt_marker != NULL ? env->prompt_marker : "> "));    
    }
    // read directly from stdin
    return rp_getline(env->mem);
  }
}


//-------------------------------------------------------------
// Read a line from stdin if there is no editing support 
// (like from a pipe, file, or dumb terminal).
//-------------------------------------------------------------

static char* rp_getline(alloc_t* mem)
{  
  // read until eof or newline
  stringbuf_t* sb = sbuf_new(mem,true);
  int c;
  while (true) {
    c = fgetc(stdin);
    if (c==EOF || c=='\n') {
      break;
    }
    else {
      sbuf_append_char(sb, (char)c);
    }
  }
  return sbuf_free_dup(sb);
}


//-------------------------------------------------------------
// Interface
//-------------------------------------------------------------

rp_public void rp_set_prompt_marker( rp_env_t* env, const char* prompt_marker ) {
  if (prompt_marker == NULL) prompt_marker = "> ";
  mem_free(env->mem, env->prompt_marker);
  env->prompt_marker = mem_strdup(env->mem,prompt_marker);  
}

rp_public void rp_set_prompt_color( rp_env_t* env, rp_color_t color ) {
  env->prompt_color = color;
}

rp_public void rp_enable_multiline( rp_env_t* env, bool enable ) {
  env->singleline_only = !enable;
}

rp_public void rp_enable_beep( rp_env_t* env, bool enable ) {
  term_enable_beep(env->term, enable);
}

rp_public void rp_enable_color( rp_env_t* env, bool enable ) {
  term_enable_color( env->term, enable );
}

rp_public void rp_enable_history_duplicates( rp_env_t* env, bool enable ) {
  history_enable_duplicates(env->history, enable);
}

rp_public void rp_set_history(rp_env_t* env, const char* fname, long max_entries ) {
  history_load_from(env->history, fname, max_entries );
}

rp_public void rp_history_remove_last(rp_env_t* env) {
  history_remove_last(env->history);
}

rp_public void rp_history_add( rp_env_t* env, const char* entry ) {
  history_push( env->history, entry );
}

rp_public void rp_history_clear(rp_env_t* env) {
  history_clear(env->history);
}

rp_public void rp_enable_auto_tab( rp_env_t* env, bool enable ) {
  env->complete_autotab = enable;
}

rp_public void rp_enable_completion_preview( rp_env_t* env, bool enable ) {
  env->complete_nopreview = !enable;
}

rp_public void rp_set_iface_colors( rp_env_t* env, rp_color_t color_info, rp_color_t color_diminish, rp_color_t color_highlight ) {
  env->color_info = (color_info == RP_COLOR_NONE ? RP_DARKGRAY : color_info);
  env->color_diminish = (color_diminish == RP_COLOR_NONE ? RP_LIGHTGRAY : color_diminish);
  env->color_highlight = (color_highlight == RP_COLOR_NONE ? RP_WHITE : color_highlight);
}

rp_public void rp_free( rp_env_t* env, void* p ) {
  mem_free(env->mem, p);
}



//-------------------------------------------------------------
// Initialize
//-------------------------------------------------------------

// Keep a list of environments to ensure every env is deallocated at the end
static rp_env_t* envs; // = NULL

static void rp_atexit(void) {
  rp_env_t* env;
  while ( (env = envs) != NULL ) {
    rp_done(env);  // removes itself from the list
  }
}

rp_public void rp_done( rp_env_t* env ) {
  if (env == NULL) return;
  history_save(env->history);
  history_free(env->history);
  completions_free(env->completions);
  term_free(env->term);
  tty_free(env->tty);
  sbuf_free(env->input);
  sbuf_free(env->extra);
  mem_free(env->mem,env->prompt_marker); 
  env->prompt_marker = NULL;
  
  // remove from list
  rp_env_t* prev = NULL;
  rp_env_t* cur = envs;
  while( cur != NULL ) {
    if (cur == env) {
      if (prev == NULL) envs = env->next;
                   else prev->next = env->next;
      break;
    }
    else {
      prev = cur;
      cur = cur->next;
    }
  }

  // and deallocate ourselves
  alloc_t* mem = env->mem;  
  mem_free(mem, env);
  mem_free(mem, mem);
}

rp_public rp_env_t* rp_init_custom_alloc( rp_malloc_fun_t* _malloc, rp_realloc_fun_t* _realloc, rp_free_fun_t* _free )  
{
  // allocate
  alloc_t* mem = (alloc_t*)_malloc(sizeof(alloc_t));
  if (mem == NULL) return NULL;
  mem->malloc = _malloc;
  mem->realloc = _realloc;
  mem->free = _free;
  rp_env_t* env = mem_zalloc_tp(mem, rp_env_t);
  if (env==NULL) {
    _free(mem);
    return NULL;
  }
  env->mem = mem;

  // Initialize
  env->tty         = tty_new(env->mem, -1);  // can return NULL
  env->term        = term_new(env->mem, env->tty, false, false, -1 );  
  env->history     = history_new(env->mem);
  env->completions = completions_new(env->mem);
  env->input       = sbuf_new(env->mem, env->tty==NULL || tty_is_utf8(env->tty));
  env->extra       = sbuf_new(env->mem, env->tty==NULL || tty_is_utf8(env->tty));

  if (env->extra == NULL || env->input == NULL 
      || env->tty == NULL || env->term==NULL
      || env->completions == NULL || env->history == NULL
      || !term_is_interactive(env->term)) 
  {
    env->noedit = true;
  }
  env->prompt_marker = NULL;
  env->multiline_eol = '\\';
  env->prompt_color  = RP_COLOR_DEFAULT;
  rp_set_iface_colors( env, RP_COLOR_NONE, RP_COLOR_NONE, RP_COLOR_NONE);
  
  // install atexit handler
  if (envs==NULL) atexit(&rp_atexit);
  
  // push on env list
  env->next = envs;
  envs = env; 
  return env;
}

rp_public rp_env_t* rp_init(void) {
  return rp_init_custom_alloc( &malloc, &realloc, &free );
}


