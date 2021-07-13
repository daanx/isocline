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
#if !defined(RL_SEPARATE_OBJS)
# include "editline.c"
# include "history.c"
# include "completions.c"
# include "term.c"
# include "tty.c"
# include "wcwidth.c"
#endif

//-------------------------------------------------------------
// includes
//-------------------------------------------------------------
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#include "../include/repline.h"
#include "common.h"
#include "term.h"
#include "tty.h"
#include "env.h"

#if defined(_WIN32)
#else
#include <unistd.h>
#endif


//-------------------------------------------------------------
// Readline
//-------------------------------------------------------------

static char*  rl_getline( rl_env_t* env, const char* prompt_text );

exported char* rl_readline(rl_env_t* env, const char* prompt_text) {
  if (env == NULL) return NULL;
  if (env->noedit) {
    return rl_getline(env, prompt_text);
  }
  else {
    return rl_editline(env, prompt_text);
  }
}


//-------------------------------------------------------------
// Allocation
//-------------------------------------------------------------

internal void* env_zalloc( rl_env_t* env, ssize_t sz ) {
  void* p = env->alloc.malloc((size_t)sz);
  if (p != NULL) memset(p,0, to_size_t(sz));
  return p;
}

internal void* env_realloc( rl_env_t* env, void* p, ssize_t newsz ) {
  return env->alloc.realloc(p, to_size_t(newsz) );
}

internal void env_free( rl_env_t* env, const void* p ) {
  env->alloc.free( (void*)p);
}

internal char* env_strdup( rl_env_t* env, const char* s) {
  if (s==NULL) return NULL;
  ssize_t n = rl_strlen(s);
  char* p = (char*)env_zalloc( env, (n+1)*ssizeof(char));
  if (p == NULL) return NULL;
  rl_memcpy(p, s, n+1);
  return p;
}

//-------------------------------------------------------------
// Initialize
//-------------------------------------------------------------

// Keep a list of environments to ensure every env is deallocated at the end
static rl_env_t* envs; // = NULL

static void rl_atexit(void) {
  rl_env_t* env;
  while ( (env = envs) != NULL ) {
    rl_done(env);  // removes itself from the list
  }
}

exported void rl_done( rl_env_t* env ) {
  if (env == NULL) return;
  history_save(env);
  history_done(env);
  completions_done(env);
  term_done(&env->term);
  tty_done(&env->tty);
  env_free(env,env->prompt_marker); env->prompt_marker = NULL;
  
  // remove from list
  rl_env_t* prev = NULL;
  rl_env_t* cur = envs;
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
  env_free(env,env);
}

exported rl_env_t* rl_init_ex( malloc_fun_t* _malloc, realloc_fun_t* _realloc, free_fun_t* _free )  
{
  // allocate
  rl_env_t* env = (rl_env_t*)_malloc(sizeof(rl_env_t));
  if (env==NULL) return NULL;
  memset(env,0,sizeof(*env));
  env->alloc.malloc  = _malloc;
  env->alloc.realloc = _realloc;
  env->alloc.free    = _free;
  int fin = STDIN_FILENO;
  // initialize term & tty
  if (!tty_init(&env->tty, fin) || !term_init(&env->term,&env->tty,false,false,-1))
  {
    env->noedit = true;
  }
  env->prompt_marker = NULL;
  env->prompt_color = RL_DEFAULT;
  env->multiline = '\\';
  // install atexit handler
  if (envs==NULL) atexit(&rl_atexit);
  // push on env list
  env->next = envs;
  envs = env;
  rl_set_history(env, NULL, -1);  
  return env;
}

exported rl_env_t* rl_init(void) {
  return rl_init_ex( &malloc, &realloc, &free );
}

exported void rl_set_prompt_marker( rl_env_t* env, const char* prompt_marker ) {
  if (prompt_marker == NULL) prompt_marker = "> ";
  env_free(env, env->prompt_marker);
  env->prompt_marker = env_strdup(env,prompt_marker);  
}

exported void rl_set_prompt_color( rl_env_t* env, rl_color_t color ) {
  env->prompt_color = color;
}


//-------------------------------------------------------------
// Read a line from stdin if there is no editing support 
// (like from a pipe, file, or dumb terminal).
//-------------------------------------------------------------

static char* rl_getline( rl_env_t* env, const char* prompt_text ) {
  ssize_t buflen = 32;
  char*  buf = (char*)env_zalloc(env,buflen);
  if (buf==NULL) return NULL;
  ssize_t len = 0;

  // display prompt
  if (prompt_text != NULL) term_write(&env->term, prompt_text);
  term_write( &env->term, (env->prompt_marker != NULL ? env->prompt_marker : "> ") );

  // read until eof or newline
  int c;
  while(true) {
    c = fgetc(stdin);
    if (c==EOF || c=='\n') {
      break;
    }
    else {
      buf[len] = (char)c;
      len++;
      if (len >= buflen) {
        buflen *= 2;
        char* newbuf = (char*)env_realloc( env, buf, buflen );
        if (newbuf == NULL) {
          len = -1;
          break;
        }
        buf = newbuf;
      }
    }
  }

  // zero-terminate and return
  if (len<=0) {
    env_free(env, buf);
    return NULL;
  }
  else {
    assert(len < buflen);
    buf[len] = 0;
    return buf;
  }
}

#ifdef RL_DEBUG_MSG
internal void debug_msg( const char* fmt, ... ) {
  static bool debug_init;
  FILE* fdbg = fopen("repline.debug.txt",(debug_init ? "a" : "w"));        
  debug_init = true;
  if (fdbg==NULL) return;
  va_list args;
  va_start(args,fmt);
  vfprintf(fdbg,fmt,args);
  fclose(fdbg);
  va_end(args);
}
#endif
