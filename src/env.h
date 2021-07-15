/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#pragma once
#ifndef RP_ENV_H
#define RP_ENV_H

#include "../include/repline.h"
#include "common.h"
#include "term.h"
#include "tty.h"



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
  rp_completion_fun_t* completer;
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
  const char*  fname;
} history_t;

//-------------------------------------------------------------
// Environment
//-------------------------------------------------------------

struct rp_env_s {
  rp_env_t*     next;
  term_t        term;
  tty_t         tty;
  alloc_t       alloc;
  completions_t completions;
  history_t     history;
  const char*   prompt_marker;
  rp_color_t    prompt_color;
  char          multiline_eol;
  bool          initialized;
  bool          noedit;
};


internal char*    rp_editline(rp_env_t* env, const char* prompt_text);

#define RP_MAX_HISTORY (200)

internal void     history_done( rp_env_t* env );
internal void     history_load( rp_env_t* env );
internal void     history_save( rp_env_t* env );
internal bool     history_push( rp_env_t* env, const char* entry );
internal bool     history_update( rp_env_t* env, const char* entry );
internal const char* history_get( rp_env_t* env, ssize_t n );

internal void     completions_done( rp_env_t* env );
internal void     completions_clear( rp_env_t* env );
internal void     completions_push(rp_env_t* env, const char* display, const char* replacement, ssize_t delete_before, ssize_t delete_after);
internal ssize_t  completions_count(rp_env_t* env);
internal ssize_t  completions_generate(rp_env_t* env, const char* input, ssize_t pos, ssize_t max);

internal completion_t* completions_get( rp_env_t* env, ssize_t index );
internal ssize_t  completion_extra_needed( completion_t* cm );
internal ssize_t  completion_apply( completion_t* cm, char* buf, ssize_t len, ssize_t pos, ssize_t* endpos );

internal int      utf8_width( const char* s, ssize_t n );
internal bool     skip_csi_esc( const char* s, ssize_t len, ssize_t* esclen );
internal ssize_t  skip_next_code( const char* s, ssize_t len, ssize_t pos, bool utf8 );

#define env_malloc(env,sz)        mem_malloc(&(env)->alloc,sz)
#define env_zalloc(env,sz)        mem_zalloc(&(env)->alloc,sz)
#define env_realloc(env,p,sz)     mem_realloc(&(env)->alloc,p,sz)
#define env_free(env,p)           mem_free(&(env)->alloc,p)
#define env_strdup(env,s)         mem_strdup(&(env)->alloc,s)
#define env_zalloc_tp(env,tp)     mem_zalloc_tp(&(env)->alloc,tp)
#define env_malloc_tp_n(env,tp,n) mem_malloc_tp_n(&(env)->alloc,tp,n)


#endif // RP_ENV_H
