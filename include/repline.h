/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#pragma once
#ifndef RP_REPLINE_H
#define RP_REPLINE_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>    // size_t
#include <stdbool.h>   // bool

//--------------------------------------------------------------
// main interface
//--------------------------------------------------------------

struct rp_env_s;
typedef struct rp_env_s rp_env_t;   // abstract environment

rp_env_t* rp_init(void);
void      rp_done(rp_env_t* env);
char*     rp_readline(rp_env_t* env, const char* prompt_text);


//--------------------------------------------------------------
// history
//--------------------------------------------------------------

void      rp_set_history(rp_env_t* env, const char* fname, long max_entries );
void      rp_history_remove_last(rp_env_t* env);
void      rp_history_clear(rp_env_t* env);


//--------------------------------------------------------------
// completion
//--------------------------------------------------------------
typedef void (rp_completion_fun_t)(rp_env_t* env, const char* input, long cursor, void* arg );

void      rp_set_completer( rp_env_t* env, rp_completion_fun_t* completer, void* arg);
bool      rp_add_completion( rp_env_t* env, const char* display, const char* completion, long delete_before, long delete_after);


//--------------------------------------------------------------
// set prompt
//--------------------------------------------------------------
typedef enum rp_color_e {
  RP_BLACK  = 30,
  RP_MAROON,
  RP_GREEN,
  RP_ORANGE,
  RP_NAVY,
  RP_PURPLE,
  RP_TEAL,
  RP_LIGHTGRAY,
  RP_DARKGRAY  = 90,
  RP_RED,
  RP_LIME,
  RP_YELLOW,
  RP_BLUE,
  RP_MAGENTA,
  RP_CYAN,
  RP_WHITE,
  RP_DEFAULT = 39
} rp_color_t;

void rp_set_prompt_marker( rp_env_t* env, const char* prompt_marker );
void rp_set_prompt_color( rp_env_t* env, rp_color_t color );


//--------------------------------------------------------------
// register allocation functions for custom allocators
//--------------------------------------------------------------

typedef void* (malloc_fun_t)( size_t size );
typedef void* (realloc_fun_t)( void* p, size_t newsize );
typedef void  (free_fun_t)( void* p );

rp_env_t* rp_init_ex( malloc_fun_t* _malloc, realloc_fun_t* _realloc, free_fun_t* _free );


#ifdef __cplusplus
}
#endif

#endif // RP_REPLINE_H
