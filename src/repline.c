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
    if (env->tty != NULL && env->term != NULL) {
      // if the terminal is not interactive, but we are reading from the tty (keyboard), we display a prompt
      term_start_raw(env->term);  // set utf8 mode on windows
      if (prompt_text != NULL) {
        term_write(env->term, prompt_text);
      }
      term_write(env->term, env->prompt_marker);    
      term_end_raw(env->term);
    }
    // read directly from stdin
    return rp_getline(env->mem);
  }
}


//-------------------------------------------------------------
// Read a line from the stdin stream if there is no editing 
// support (like from a pipe, file, or dumb terminal).
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

rp_public const char* rp_get_prompt_marker(void) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return NULL;
  return env->prompt_marker;
}

rp_public const char* rp_get_continuation_prompt_marker(void) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return NULL;
  return env->cprompt_marker;
}

rp_public void rp_set_prompt_marker( const char* prompt_marker, const char* cprompt_marker ) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return;
  set_prompt_marker(env, prompt_marker, cprompt_marker);
}

rp_public rp_color_t rp_set_prompt_color( rp_color_t color ) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return RP_COLOR_NONE;
  rp_color_t prev = env->prompt_color;
  env->prompt_color = color;
  return prev;
}

rp_public bool rp_enable_multiline( bool enable ) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return false;
  bool prev = env->singleline_only;
  env->singleline_only = !enable;
  return !prev;
}

rp_public bool rp_enable_beep( bool enable ) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return false;
  return term_enable_beep(env->term, enable);
}

rp_public bool rp_enable_color( bool enable ) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return false;
  return term_enable_color( env->term, enable );
}

rp_public bool rp_enable_history_duplicates( bool enable ) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return false;
  return history_enable_duplicates(env->history, enable);
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

rp_public bool rp_enable_auto_tab( bool enable ) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return false;
  bool prev = env->complete_autotab;
  env->complete_autotab = enable;
  return prev;
}

rp_public bool rp_enable_completion_preview( bool enable ) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return false;
  bool prev = env->complete_nopreview;
  env->complete_nopreview = !enable;
  return !prev;
}

rp_public bool rp_enable_multiline_indent(bool enable) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return false;
  bool prev = env->no_multiline_indent;
  env->no_multiline_indent = !enable;
  return !prev;
}

rp_public bool rp_enable_hint(bool enable) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return false;
  bool prev = env->no_hint;
  env->no_hint = !enable;
  return !prev;
}

rp_public bool rp_enable_highlight(bool enable) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return false;
  bool prev = env->no_highlight;
  env->no_highlight = !enable;
  return !prev;
}

rp_public bool rp_enable_inline_help(bool enable) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return false;
  bool prev = env->no_help;
  env->no_help = !enable;
  return !prev;
}

rp_public void rp_set_default_highlighter(rp_highlight_fun_t* highlighter, void* arg) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return;
  env->highlighter = highlighter;
  env->highlighter_arg = arg;
}

static void set_style_color(rp_env_t* env, rp_style_t iface_element, rp_color_t color) {
  switch (iface_element) {
    case RP_STYLE_INFO:     env->color_info = (color == RP_COLOR_NONE ? RP_ANSI_DARKGRAY : color); break;
    case RP_STYLE_DIMINISH: env->color_diminish = (color == RP_COLOR_NONE ? RP_ANSI_LIGHTGRAY : color); break;
    case RP_STYLE_EMPHASIS: env->color_emphasis = (color == RP_COLOR_NONE ? RP_RGB(0xFFFFD7) : color); break;
    case RP_STYLE_HINT:     env->color_hint = (color == RP_COLOR_NONE ? RP_ANSI_DARKGRAY : color); break;
    default: break;
  }
}

rp_public void rp_set_style_color(rp_style_t iface_element, rp_color_t color) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return;
  set_style_color(env, iface_element, color);
}

rp_public rp_color_t rp_get_style_color(rp_style_t iface_element) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return RP_COLOR_NONE;
  switch (iface_element) {
  case RP_STYLE_INFO:     return env->color_info;
  case RP_STYLE_DIMINISH: return env->color_diminish;
  case RP_STYLE_EMPHASIS: return env->color_emphasis;
  case RP_STYLE_HINT:     return env->color_hint;
  default: break;
  }
  return RP_COLOR_NONE;
}

rp_public void rp_free( void* p ) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return;
  mem_free(env->mem, p);
}

rp_public void* rp_malloc(size_t sz) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return NULL;
  return mem_malloc(env->mem, to_ssize_t(sz));
}

rp_public const char* rp_strdup( const char* s ) {
  if (s==NULL) return NULL;
  rp_env_t* env = rp_get_env(); if (env==NULL) return NULL;
  ssize_t len = rp_strlen(s);
  char* p = mem_malloc_tp_n( env->mem, char, len + 1 );
  if (p == NULL) return NULL;
  rp_memcpy( p, s, len );
  p[len] = 0;
  return p;
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

rp_public void rp_term_color( rp_color_t color ) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return;
  term_color(env->term, color);
}

rp_public void rp_term_bgcolor( rp_color_t color ) {
  rp_env_t* env = rp_get_env(); if (env==NULL) return;
  term_bgcolor(env->term, color);
}

rp_public void rp_term_reset( void )  {
  rp_env_t* env = rp_get_env(); if (env==NULL) return;
  term_attr_reset(env->term);
}

//-------------------------------------------------------------
// Readline with temporary completer and highlighter
//-------------------------------------------------------------

rp_public char* rp_readline_ex(const char* prompt_text,
                                rp_completer_fun_t* completer, void* completer_arg,
                                 rp_highlight_fun_t* highlighter, void* highlighter_arg )
{
  rp_env_t* env = rp_get_env(); if (env == NULL) return NULL;
  // save previous
  rp_completer_fun_t* prev_completer;
  void* prev_completer_arg;
  completions_get_completer(env->completions, &prev_completer, &prev_completer_arg);
  rp_highlight_fun_t* prev_highlighter = env->highlighter;
  void* prev_highlighter_arg = env->highlighter_arg;
  // call with current
  if (completer != NULL)   { rp_set_default_completer(completer, completer_arg); }
  if (highlighter != NULL) { rp_set_default_highlighter(highlighter, highlighter_arg); }
  char* res = rp_readline(prompt_text);
  // restore previous
  rp_set_default_completer(prev_completer, prev_completer_arg);
  rp_set_default_highlighter(prev_highlighter, prev_highlighter_arg);
  return res;
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
  env->prompt_color  = RP_ANSI_DEFAULT;
  for (rp_style_t style = 0; style < RP_STYLE_LAST; style++) {
    set_style_color(env, style, RP_COLOR_NONE);  // set default colors
  }
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

