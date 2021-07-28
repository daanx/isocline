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
# include "highlight.c"
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

rp_public char* rp_readline(const char* prompt_text) 
{
  rp_env_t* env = rp_get_env();
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
      term_write(env->term, env->prompt_marker);    
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
  stringbuf_t* sb = sbuf_new(mem);
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

static void set_prompt_marker(rp_env_t* env, const char* prompt_marker, const char* cprompt_marker) {
  if (prompt_marker == NULL) prompt_marker = "> ";
  if (cprompt_marker == NULL) cprompt_marker = prompt_marker;
  mem_free(env->mem, env->prompt_marker);
  mem_free(env->mem, env->cprompt_marker);
  env->prompt_marker = mem_strdup(env->mem, prompt_marker);
  env->cprompt_marker = mem_strdup(env->mem, cprompt_marker);
}

rp_public void rp_set_prompt_marker( const char* prompt_marker, const char* cprompt_marker ) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return;
  set_prompt_marker(env, prompt_marker, cprompt_marker);
}

rp_public void rp_set_prompt_color( rp_color_t color ) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return;
  env->prompt_color = color;
}

rp_public void rp_enable_multiline( bool enable ) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return;
  env->singleline_only = !enable;
}

rp_public void rp_enable_beep( bool enable ) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return;
  term_enable_beep(env->term, enable);
}

rp_public void rp_enable_color( bool enable ) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return;
  term_enable_color( env->term, enable );
}

rp_public void rp_enable_history_duplicates( bool enable ) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return;
  history_enable_duplicates(env->history, enable);
}

rp_public void rp_set_history(const char* fname, long max_entries ) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return;
  history_load_from(env->history, fname, max_entries );
}

rp_public void rp_history_remove_last(void) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return;
  history_remove_last(env->history);
}

rp_public void rp_history_add( const char* entry ) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return;
  history_push( env->history, entry );
}

rp_public void rp_history_clear(void) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return;
  history_clear(env->history);
}

rp_public void rp_enable_auto_tab( bool enable ) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return;
  env->complete_autotab = enable;
}

rp_public void rp_enable_completion_preview( bool enable ) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return;
  env->complete_nopreview = !enable;
}

rp_public void rp_enable_multiline_indent(bool enable) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return;
  env->no_multiline_indent = !enable;
}

rp_public void rp_enable_hint(bool enable) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return;
  env->no_hint = !enable;
}

rp_public void rp_enable_highlight(bool enable) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return;
  env->no_highlight = !enable;
}

rp_public void rp_enable_inline_help(bool enable) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return;
  env->no_help = !enable;
}

rp_public void rp_set_highlighter(rp_highlight_fun_t* highlighter, void* arg) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return;
  env->highlighter = highlighter;
  env->highlighter_arg = arg;
}

static void set_iface_colors(rp_env_t* env, rp_color_t color_info, rp_color_t color_diminish, rp_color_t color_highlight, rp_color_t color_hint) {
  env->color_info = (color_info == RP_COLOR_NONE ? RP_DARKGRAY : color_info);
  env->color_diminish = (color_diminish == RP_COLOR_NONE ? RP_LIGHTGRAY : color_diminish);
  env->color_highlight = (color_highlight == RP_COLOR_NONE ? RP_WHITE : color_highlight);
  env->color_hint = (color_hint == RP_COLOR_NONE ? RP_DARKGRAY : color_hint);
}

rp_public void rp_set_iface_colors( rp_color_t color_info, rp_color_t color_diminish, rp_color_t color_highlight, rp_color_t color_hint ) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return;
  set_iface_colors(env, color_info, color_diminish, color_highlight, color_hint);
}

rp_public void rp_free( void* p ) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return;
  mem_free(env->mem, p);
}


//-------------------------------------------------------------
// Terminal
//-------------------------------------------------------------

rp_public void rp_write(const char* s) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return;
  term_write(env->term, s);
}

rp_public void rp_writeln(const char* s) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return;
  term_writeln(env->term, s);
}



//-------------------------------------------------------------
// Initialize
//-------------------------------------------------------------

static void rp_atexit(void);

static void rp_env_free(rp_env_t* env) {
  if (env == NULL) return;
  history_save(env->history);
  history_free(env->history);
  completions_free(env->completions);
  term_free(env->term);
  tty_free(env->tty);
  sbuf_free(env->input);
  sbuf_free(env->extra);
  sbuf_free(env->hint);
  mem_free(env->mem, env->cprompt_marker);
  mem_free(env->mem,env->prompt_marker);
  env->prompt_marker = NULL;
  
  // and deallocate ourselves
  alloc_t* mem = env->mem;  
  mem_free(mem, env);
  mem_free(mem, mem);
}


static rp_env_t* rp_env_create( rp_malloc_fun_t* _malloc, rp_realloc_fun_t* _realloc, rp_free_fun_t* _free )  
{
  if (_malloc == NULL)  _malloc = &malloc;
  if (_realloc == NULL) _realloc = &realloc;
  if (_free == NULL)    _free = &free;
  // allocate
  alloc_t* mem = (alloc_t*)_malloc(sizeof(alloc_t));
  if (mem == NULL) return NULL;
  mem->malloc = _malloc;
  mem->realloc = _realloc;
  mem->free = _free;
  rp_env_t* env = mem_zalloc_tp(mem, rp_env_t);
  if (env==NULL) {
    mem->free(mem);
    return NULL;
  }
  env->mem = mem;

  // Initialize
  env->tty         = tty_new(env->mem, -1);  // can return NULL
  env->term        = term_new(env->mem, env->tty, false, false, -1 );  
  env->history     = history_new(env->mem);
  env->completions = completions_new(env->mem);
  env->input       = sbuf_new(env->mem);
  env->extra       = sbuf_new(env->mem);
  env->hint        = sbuf_new(env->mem);

  if (env->extra == NULL || env->input == NULL || env->hint == NULL
      || env->tty == NULL || env->term==NULL
      || env->completions == NULL || env->history == NULL
      || !term_is_interactive(env->term)) 
  {
    env->noedit = true;
  }
  env->multiline_eol = '\\';
  env->prompt_color  = RP_COLOR_DEFAULT;
  set_iface_colors(env,RP_COLOR_NONE, RP_COLOR_NONE, RP_COLOR_NONE, RP_COLOR_NONE);
  set_prompt_marker(env, NULL, NULL);
  return env;
}

static rp_env_t* rpenv;

static void rp_atexit(void) {
  if (rpenv != NULL) {
    rp_env_free(rpenv);
    rpenv = NULL;
  }
}

rp_private rp_env_t* rp_get_env(void) {  
  if (rpenv==NULL) {
    rpenv = rp_env_create( NULL, NULL, NULL );
    if (rpenv != NULL) { atexit( &rp_atexit ); }
  }
  return rpenv;
}

rp_public void rp_init_custom_malloc( rp_malloc_fun_t* _malloc, rp_realloc_fun_t* _realloc, rp_free_fun_t* _free ) {
  assert(rpenv == NULL);
  if (rpenv != NULL) {
    rp_env_free(rpenv);    
    rpenv = rp_env_create( _malloc, _realloc, _free ); 
  }
  else {
    rpenv = rp_env_create( _malloc, _realloc, _free ); 
    if (rpenv != NULL) {
      atexit( &rp_atexit );
    }
  }
}

