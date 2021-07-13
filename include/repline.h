/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#pragma once
#ifndef RL_REPLINE_H
#define RL_REPLINE_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>    // size_t
#include <stdbool.h>   // bool

//--------------------------------------------------------------
// main interface
//--------------------------------------------------------------

struct rl_env_s;
typedef struct rl_env_s rl_env_t;   // abstract environment

rl_env_t* rl_init(void);
void      rl_done(rl_env_t* env);
char*     rl_readline(rl_env_t* env, const char* prompt_text);


//--------------------------------------------------------------
// history
//--------------------------------------------------------------

void      rl_set_history(rl_env_t* env, const char* fname, long max_entries );
void      rl_history_remove_last(rl_env_t* env);
void      rl_history_clear(rl_env_t* env);


//--------------------------------------------------------------
// completion
//--------------------------------------------------------------
typedef void (rl_completion_fun_t)(rl_env_t* env, const char* input, long cursor, void* arg );

void      rl_set_completer( rl_env_t* env, rl_completion_fun_t* completer, void* arg);
bool      rl_add_completion( rl_env_t* env, const char* display, const char* completion, long delete_before, long delete_after);


//--------------------------------------------------------------
// set prompt
//--------------------------------------------------------------
typedef enum rl_color_e {
  RL_BLACK  = 30,
  RL_MAROON,
  RL_GREEN,
  RL_ORANGE,
  RL_NAVY,
  RL_PURPLE,
  RL_TEAL,
  RL_LIGHTGRAY,
  RL_DARKGRAY  = 90,
  RL_RED,
  RL_LIME,
  RL_YELLOW,
  RL_BLUE,
  RL_MAGENTA,
  RL_CYAN,
  RL_WHITE,
  RL_DEFAULT = 39
} rl_color_t;

void rl_set_prompt_marker( rl_env_t* env, const char* prompt_marker );
void rl_set_prompt_color( rl_env_t* env, rl_color_t color );


//--------------------------------------------------------------
// register allocation functions for custom allocators
//--------------------------------------------------------------

typedef void* (malloc_fun_t)( size_t size );
typedef void* (realloc_fun_t)( void* p, size_t newsize );
typedef void  (free_fun_t)( void* p );

rl_env_t* rl_init_ex( malloc_fun_t* _malloc, realloc_fun_t* _realloc, free_fun_t* _free );


#ifdef __cplusplus
}
#endif

#endif // RL_REPLINE_H
