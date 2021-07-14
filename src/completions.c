/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#include <string.h>
#include <stdio.h>

#include "../include/repline.h"
#include "common.h"
#include "env.h"

internal void completions_clear(rl_env_t* env) {
  completions_t* cms = &env->completions;
  while (cms->count > 0) {
    completion_t* cm = cms->elems + cms->count - 1;
    env_free( env, cm->display);
    env_free( env, cm->replacement);
    memset(cm,0,sizeof(*cm));
    cms->count--;    
  }
}

internal void completions_done(rl_env_t* env) {
  completions_clear(env);
  completions_t* cms = &env->completions;
  if (cms->elems != NULL) {
    env_free( env, cms->elems );
    cms->elems = NULL;
    cms->count = 0;
    cms->len   = 0;
  }
}

internal void completions_push(rl_env_t* env, const char* display, const char* replacement, ssize_t delete_before, ssize_t delete_after) 
{
  completions_t* cms = &env->completions;
  if (cms->count >= cms->len) {
    ssize_t newlen = (cms->len <= 0 ? 32 : cms->len*2);
    completion_t* newelems = (completion_t*)env_realloc( env, cms->elems, newlen*ssizeof(completion_t) );
    if (newelems == NULL) return;
    cms->elems = newelems;
    cms->len   = newlen;
  }
  assert(cms->count < cms->len);
  completion_t* cm = &cms->elems[cms->count];
  cm->display       = env_strdup(env,display);
  cm->replacement   = env_strdup(env,replacement);
  cm->delete_before = delete_before;
  cm->delete_after  = delete_after;
  cms->count++;
}

internal ssize_t completions_count(rl_env_t* env) {
  return env->completions.count;
}

internal ssize_t completions_generate(rl_env_t* env, const char* input, ssize_t pos, ssize_t max) {
  completions_clear(env);
  if (env->completions.completer == NULL || input == NULL || input[0] == 0 || rl_strlen(input) < pos) return 0;
  env->completions.completer_max = max;
  env->completions.completer(env,input,(long)pos,env->completions.completer_arg);
  return completions_count(env);
}

exported bool rl_add_completion( rl_env_t* env, const char* display, const char* replacement, long delete_before, long delete_after ) {
  if (env->completions.completer_max <= 0) return false;
  env->completions.completer_max--;
  //debug_msg("completion: add: %d,%d, %s\n", delete_before, delete_after, replacement);
  completions_push( env, display, replacement, delete_before, delete_after );
  return true;
}

internal completion_t* completions_get( rl_env_t* env, ssize_t index ) {
  if (index < 0 || env->completions.count <= 0 || index >= env->completions.count ) return NULL;
  return &env->completions.elems[index];
}

internal ssize_t completion_extra_needed( completion_t* cm ) {
  return (rl_strlen(cm->replacement) - cm->delete_before - cm->delete_after);  
}

internal ssize_t completion_apply( completion_t* cm, char* buf, ssize_t len, ssize_t pos, ssize_t* endpos ) {
  debug_msg( "completion: apply: %s at %zd\n", cm->replacement, pos);
  ssize_t start = pos - cm->delete_before;
  if (start < 0) return len;
  ssize_t rlen = rl_strlen(cm->replacement);
  ssize_t end  = start + rlen;
  ssize_t next = pos + cm->delete_after;
  if (next != end) {
    rl_memmove( buf+end, buf+next, len - pos );
  }
  rl_memcpy( buf+start, cm->replacement, rlen );
  ssize_t extra = end - next;
  assert( completion_extra_needed(cm) == extra);
  if (endpos != NULL) *endpos = end;
  return (len + extra);
}

exported void rl_set_completer( rl_env_t* env, rl_completion_fun_t* completer, void* arg) {
  env->completions.completer = completer;
  env->completions.completer_arg = arg;
}

