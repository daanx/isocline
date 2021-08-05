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
// $ gcc -c src/isocline.c
//-------------------------------------------------------------
#if !defined(IC_SEPARATE_OBJS)
# define _CRT_SECURE_NO_WARNINGS  // for msvc
# define _XOPEN_SOURCE   700      // for wcwidth
# define _DEFAULT_SOURCE          // ensure usleep stays visible with _XOPEN_SOURCE >= 700
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

#include "../include/isocline.h"
#include "common.h"
#include "env.h"


//-------------------------------------------------------------
// Readline
//-------------------------------------------------------------

static char*  ic_getline( alloc_t* mem );

ic_public char* ic_readline(const char* prompt_text) 
{
  ic_env_t* env = ic_get_env();
  if (env == NULL) return NULL;
  if (!env->noedit) {
    // terminal editing enabled
    return ic_editline(env, prompt_text);   // in editline.c
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
    return ic_getline(env->mem);
  }
}


//-------------------------------------------------------------
// Read a line from the stdin stream if there is no editing 
// support (like from a pipe, file, or dumb terminal).
//-------------------------------------------------------------

static char* ic_getline(alloc_t* mem)
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

ic_public bool ic_async_stop(void) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return false;
  if (env->tty==NULL) return false;
  return tty_async_stop(env->tty);
}

static void set_prompt_marker(ic_env_t* env, const char* prompt_marker, const char* cprompt_marker) {
  if (prompt_marker == NULL) prompt_marker = "> ";
  if (cprompt_marker == NULL) cprompt_marker = prompt_marker;
  mem_free(env->mem, env->prompt_marker);
  mem_free(env->mem, env->cprompt_marker);
  env->prompt_marker = mem_strdup(env->mem, prompt_marker);
  env->cprompt_marker = mem_strdup(env->mem, cprompt_marker);
}

ic_public const char* ic_get_prompt_marker(void) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return NULL;
  return env->prompt_marker;
}

ic_public const char* ic_get_continuation_prompt_marker(void) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return NULL;
  return env->cprompt_marker;
}

ic_public void ic_set_prompt_marker( const char* prompt_marker, const char* cprompt_marker ) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return;
  set_prompt_marker(env, prompt_marker, cprompt_marker);
}

ic_public bool ic_enable_multiline( bool enable ) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return false;
  bool prev = env->singleline_only;
  env->singleline_only = !enable;
  return !prev;
}

ic_public bool ic_enable_beep( bool enable ) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return false;
  return term_enable_beep(env->term, enable);
}

ic_public bool ic_enable_color( bool enable ) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return false;
  return term_enable_color( env->term, enable );
}

ic_public bool ic_enable_history_duplicates( bool enable ) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return false;
  return history_enable_duplicates(env->history, enable);
}

ic_public void ic_set_history(const char* fname, long max_entries ) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return;
  history_load_from(env->history, fname, max_entries );
}

ic_public void ic_history_remove_last(void) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return;
  history_remove_last(env->history);
}

ic_public void ic_history_add( const char* entry ) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return;
  history_push( env->history, entry );
}

ic_public void ic_history_clear(void) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return;
  history_clear(env->history);
}

ic_public bool ic_enable_auto_tab( bool enable ) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return false;
  bool prev = env->complete_autotab;
  env->complete_autotab = enable;
  return prev;
}

ic_public bool ic_enable_completion_preview( bool enable ) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return false;
  bool prev = env->complete_nopreview;
  env->complete_nopreview = !enable;
  return !prev;
}

ic_public bool ic_enable_multiline_indent(bool enable) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return false;
  bool prev = env->no_multiline_indent;
  env->no_multiline_indent = !enable;
  return !prev;
}

ic_public bool ic_enable_hint(bool enable) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return false;
  bool prev = env->no_hint;
  env->no_hint = !enable;
  return !prev;
}

ic_public bool ic_enable_highlight(bool enable) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return false;
  bool prev = env->no_highlight;
  env->no_highlight = !enable;
  return !prev;
}

ic_public bool ic_enable_inline_help(bool enable) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return false;
  bool prev = env->no_help;
  env->no_help = !enable;
  return !prev;
}

ic_public void ic_set_default_highlighter(ic_highlight_fun_t* highlighter, void* arg) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return;
  env->highlighter = highlighter;
  env->highlighter_arg = arg;
}

static void set_style_color(ic_env_t* env, ic_style_t iface_element, ic_color_t color) {
  switch (iface_element) {
    case IC_STYLE_PROMPT:   env->color_prompt = (color == IC_COLOR_NONE ? IC_ANSI_GREEN : color); break;
    case IC_STYLE_INFO:     env->color_info = (color == IC_COLOR_NONE ? IC_ANSI_DARKGRAY : color); break;
    case IC_STYLE_DIMINISH: env->color_diminish = (color == IC_COLOR_NONE ? IC_ANSI_LIGHTGRAY : color); break;
    case IC_STYLE_EMPHASIS: env->color_emphasis = (color == IC_COLOR_NONE ? IC_RGB(0xFFFFD7) : color); break;
    case IC_STYLE_HINT:     env->color_hint = (color == IC_COLOR_NONE ? IC_ANSI_DARKGRAY : color); break;
    default: break;
  }
}

ic_public void ic_set_style_color(ic_style_t iface_element, ic_color_t color) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return;
  set_style_color(env, iface_element, color);
}

ic_public ic_color_t ic_get_style_color(ic_style_t iface_element) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return IC_COLOR_NONE;
  switch (iface_element) {
    case IC_STYLE_PROMPT:   return env->color_prompt;    
    case IC_STYLE_INFO:     return env->color_info;
    case IC_STYLE_DIMINISH: return env->color_diminish;
    case IC_STYLE_EMPHASIS: return env->color_emphasis;
    case IC_STYLE_HINT:     return env->color_hint;
    default: break;
  }
  return IC_COLOR_NONE;
}

ic_public void ic_free( void* p ) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return;
  mem_free(env->mem, p);
}

ic_public void* ic_malloc(size_t sz) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return NULL;
  return mem_malloc(env->mem, to_ssize_t(sz));
}

ic_public const char* ic_strdup( const char* s ) {
  if (s==NULL) return NULL;
  ic_env_t* env = ic_get_env(); if (env==NULL) return NULL;
  ssize_t len = ic_strlen(s);
  char* p = mem_malloc_tp_n( env->mem, char, len + 1 );
  if (p == NULL) return NULL;
  ic_memcpy( p, s, len );
  p[len] = 0;
  return p;
}

//-------------------------------------------------------------
// Terminal
//-------------------------------------------------------------

ic_public void ic_term_write(const char* s) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return;
  term_write(env->term, s);
}

ic_public void ic_term_writeln(const char* s) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return;
  term_writeln(env->term, s);
}

ic_public void ic_term_color( ic_color_t color ) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return;
  term_color(env->term, color);
}

ic_public void ic_term_bgcolor( ic_color_t color ) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return;
  term_bgcolor(env->term, color);
}

ic_public void ic_term_underline( bool enable ) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return;
  term_underline(env->term, enable);
}

ic_public void ic_term_reverse( bool enable ) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return;
  term_reverse(env->term, enable);
}

ic_public void ic_term_reset( void )  {
  ic_env_t* env = ic_get_env(); if (env==NULL) return;
  term_attr_reset(env->term);
}

ic_public int ic_term_get_color_bits(void) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return 4;
  return term_get_color_bits(env->term);
}


//-------------------------------------------------------------
// Readline with temporary completer and highlighter
//-------------------------------------------------------------

ic_public char* ic_readline_ex(const char* prompt_text,
                                ic_completer_fun_t* completer, void* completer_arg,
                                 ic_highlight_fun_t* highlighter, void* highlighter_arg )
{
  ic_env_t* env = ic_get_env(); if (env == NULL) return NULL;
  // save previous
  ic_completer_fun_t* prev_completer;
  void* prev_completer_arg;
  completions_get_completer(env->completions, &prev_completer, &prev_completer_arg);
  ic_highlight_fun_t* prev_highlighter = env->highlighter;
  void* prev_highlighter_arg = env->highlighter_arg;
  // call with current
  if (completer != NULL)   { ic_set_default_completer(completer, completer_arg); }
  if (highlighter != NULL) { ic_set_default_highlighter(highlighter, highlighter_arg); }
  char* res = ic_readline(prompt_text);
  // restore previous
  ic_set_default_completer(prev_completer, prev_completer_arg);
  ic_set_default_highlighter(prev_highlighter, prev_highlighter_arg);
  return res;
}


//-------------------------------------------------------------
// Initialize
//-------------------------------------------------------------

static void ic_atexit(void);

static void ic_env_free(ic_env_t* env) {
  if (env == NULL) return;
  history_save(env->history);
  history_free(env->history);
  completions_free(env->completions);
  term_free(env->term);
  tty_free(env->tty);
  mem_free(env->mem, env->cprompt_marker);
  mem_free(env->mem,env->prompt_marker);
  env->prompt_marker = NULL;
  
  // and deallocate ourselves
  alloc_t* mem = env->mem;  
  mem_free(mem, env);

  // and clear the sbuf cache
  sbuf_clear_cache();

  // and finally the custom memory allocation structure
  mem_free(mem, mem);
}


static ic_env_t* ic_env_create( ic_malloc_fun_t* _malloc, ic_realloc_fun_t* _realloc, ic_free_fun_t* _free )  
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
  ic_env_t* env = mem_zalloc_tp(mem, ic_env_t);
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
  
  if (env->tty == NULL || env->term==NULL ||
      env->completions == NULL || env->history == NULL ||
      !term_is_interactive(env->term)) 
  {
    env->noedit = true;
  }
  env->multiline_eol = '\\';
  for (int style = 0; style < (int)IC_STYLE_LAST; style++) {
    set_style_color(env, (ic_style_t)style, IC_COLOR_NONE);  // set default colors
  }
  set_prompt_marker(env, NULL, NULL);
  return env;
}

static ic_env_t* rpenv;

static void ic_atexit(void) {
  if (rpenv != NULL) {
    ic_env_free(rpenv);
    rpenv = NULL;
  }
}

ic_private ic_env_t* ic_get_env(void) {  
  if (rpenv==NULL) {
    rpenv = ic_env_create( NULL, NULL, NULL );
    if (rpenv != NULL) { atexit( &ic_atexit ); }
  }
  return rpenv;
}

ic_public void ic_init_custom_malloc( ic_malloc_fun_t* _malloc, ic_realloc_fun_t* _realloc, ic_free_fun_t* _free ) {
  assert(rpenv == NULL);
  if (rpenv != NULL) {
    ic_env_free(rpenv);    
    rpenv = ic_env_create( _malloc, _realloc, _free ); 
  }
  else {
    rpenv = ic_env_create( _malloc, _realloc, _free ); 
    if (rpenv != NULL) {
      atexit( &ic_atexit );
    }
  }
}

