/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#pragma once
#ifndef RL_ENV_H
#define RL_ENV_H

#include "../include/repline.h"
#include "common.h"
#include "term.h"
#include "tty.h"

//-------------------------------------------------------------
// Environment
//-------------------------------------------------------------

typedef struct alloc_s {
  malloc_fun_t*  malloc;
  realloc_fun_t* realloc;
  free_fun_t*    free;
} alloc_t;

//-------------------------------------------------------------
// Completions
//-------------------------------------------------------------

typedef struct completion_s {
  const char* display;
  const char* replacement;
  ssize_t     delete_before;
  ssize_t     delete_after;
} completion_t;

typedef struct completions_s {
  rl_completion_fun_t* completer;
  void*   completer_arg;
  ssize_t completer_max;
  ssize_t count;
  ssize_t len;
  completion_t* elems;  
} completions_t;

//-------------------------------------------------------------
// History
//-------------------------------------------------------------

typedef struct history_s {
  ssize_t count;
  ssize_t len;
  const char** elems;
  const char* fname;
} history_t;

//-------------------------------------------------------------
// Environment
//-------------------------------------------------------------

struct rl_env_s {
  rl_env_t* next;
  term_t    term;
  tty_t     tty;
  alloc_t   alloc;
  completions_t completions;
  history_t     history;
  const char*   prompt_marker;
  rl_color_t    prompt_color;
  char      multiline;
  bool      initialized;
  bool      noedit;
};

internal void* env_zalloc( rl_env_t* env, ssize_t sz );
internal void* env_realloc( rl_env_t* env, void* p, ssize_t newsz );
internal void  env_free( rl_env_t* env, const void* p );
internal char* env_strdup( rl_env_t* env, const char* s);

#define env_zalloc_tp(env,tp)  (tp*)env_zalloc(env->alloc,ssizeof(tp))

internal char* rl_editline(rl_env_t* env, const char* prompt);

#define RL_MAX_HISTORY (200)

internal void history_done( rl_env_t* env );
internal void history_load( rl_env_t* env );
internal void history_save( rl_env_t* env );
internal const char* history_get( rl_env_t* env, ssize_t n );
internal bool history_push( rl_env_t* env, const char* entry );
internal bool history_update( rl_env_t* env, const char* entry );

internal void completions_done( rl_env_t* env );
internal void completions_clear( rl_env_t* env );
internal void completions_push(rl_env_t* env, const char* display, const char* replacement, ssize_t delete_before, ssize_t delete_after);
internal ssize_t completions_count(rl_env_t* env);
internal ssize_t completions_generate(rl_env_t* env, const char* input, ssize_t pos, ssize_t max);

internal completion_t* completions_get( rl_env_t* env, ssize_t index );
internal ssize_t completion_extra_needed( completion_t* cm );
internal ssize_t completion_apply( completion_t* cm, char* buf, ssize_t len, ssize_t pos, ssize_t* endpos );

internal int utf8_width( const char* s, ssize_t n );

#endif // RL_ENV_H
