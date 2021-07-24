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
#include "stringbuf.h"
#include "completions.h"


//-------------------------------------------------------------
// Completions
//-------------------------------------------------------------

typedef struct completion_s {
  const char* display;
  const char* replacement;
  ssize_t     delete_before;
  ssize_t     delete_after;
} completion_t;

struct completions_s {
  rp_completer_fun_t* completer;
  void* completer_arg;
  ssize_t completer_max;
  ssize_t count;
  ssize_t len;
  completion_t* elems;
  alloc_t* mem;
};

static void default_filename_completer( rp_completion_env_t* cenv, const char* prefix );

rp_private completions_t* completions_new(alloc_t* mem) {
  completions_t* cms = mem_zalloc_tp(mem, completions_t);
  if (cms == NULL) return NULL;
  cms->mem = mem;
  cms->completer = &default_filename_completer;
  return cms;
}

rp_private void completions_free(completions_t* cms) {
  if (cms == NULL) return;
  completions_clear(cms);  
  if (cms->elems != NULL) {
    mem_free(cms->mem, cms->elems);
    cms->elems = NULL;
    cms->count = 0;
    cms->len = 0;
  }
  mem_free(cms->mem, cms); // free ourselves
}


rp_private void completions_clear(completions_t* cms) {  
  while (cms->count > 0) {
    completion_t* cm = cms->elems + cms->count - 1;
    mem_free( cms->mem, cm->display);
    mem_free( cms->mem, cm->replacement);
    memset(cm,0,sizeof(*cm));
    cms->count--;    
  }
}

static void completions_push(completions_t* cms, const char* display, const char* replacement, ssize_t delete_before, ssize_t delete_after) 
{
  if (cms->count >= cms->len) {
    ssize_t newlen = (cms->len <= 0 ? 32 : cms->len*2);
    completion_t* newelems = mem_realloc_tp(cms->mem, completion_t, cms->elems, newlen );
    if (newelems == NULL) return;
    cms->elems = newelems;
    cms->len   = newlen;
  }
  assert(cms->count < cms->len);
  completion_t* cm  = cms->elems + cms->count;
  cm->display       = mem_strdup(cms->mem,display);
  cm->replacement   = mem_strdup(cms->mem,replacement);
  cm->delete_before = delete_before;
  cm->delete_after  = delete_after;
  cms->count++;
}

rp_private ssize_t completions_count(completions_t* cms) {
  return cms->count;
}


rp_private bool completions_add(completions_t* cms, const char* display, const char* replacement, ssize_t delete_before, ssize_t delete_after) {
  if (cms->completer_max <= 0) return false;
  cms->completer_max--;
  //debug_msg("completion: add: %d,%d, %s\n", delete_before, delete_after, replacement);
  completions_push(cms, display, replacement, delete_before, delete_after);
  return true;
}

static completion_t* completions_get(completions_t* cms, ssize_t index) {
  if (index < 0 || cms->count <= 0 || index >= cms->count) return NULL;
  return &cms->elems[index];
}

rp_private const char* completions_get_display( completions_t* cms, ssize_t index ) {
  completion_t* cm = completions_get(cms, index);
  if (cm == NULL) return NULL;
  return (cm->display != NULL ? cm->display : cm->replacement);
}

static void completions_set_completer(completions_t* cms, rp_completer_fun_t* completer, void* arg) {
  cms->completer = completer;
  cms->completer_arg = arg;
}

static void completions_get_completer(completions_t* cms, rp_completer_fun_t** completer, void** arg) {
  *completer = cms->completer;
  *arg = cms->completer_arg;
}

rp_public bool rp_has_completions( rp_completion_env_t* cenv ) {
  return (cenv->env->completions->count > 0);
}


static ssize_t completion_apply( completion_t* cm, stringbuf_t* sbuf, ssize_t pos ) {
  if (cm == NULL) return -1;  
  debug_msg( "completion: apply: %s at %zd\n", cm->replacement, pos);
  ssize_t start = pos - cm->delete_before;
  if (start < 0) start = 0;
  ssize_t n = cm->delete_before + cm->delete_after;
  if (rp_strlen(cm->replacement) == n && strncmp(sbuf_string_at(sbuf,start), cm->replacement, to_size_t(n)) == 0) {
    // no changes
    return -1;
  }
  else {
    sbuf_delete_from_to( sbuf, start, pos + cm->delete_after );
    return sbuf_insert_at(sbuf, cm->replacement, start); 
  }
}

rp_private ssize_t completions_apply( completions_t* cms, ssize_t index, stringbuf_t* sbuf, ssize_t pos ) {
  completion_t* cm = completions_get(cms, index);
  return completion_apply( cm, sbuf, pos );
}



#define RP_MAX_PREFIX  (256)

// find longest common prefix and complete with that.
rp_private ssize_t completions_apply_longest_prefix(completions_t* cms, stringbuf_t* sbuf, ssize_t pos) {
  if (cms->count <= 1) {
    return completions_apply(cms,0,sbuf,pos);
  }

  // set initial prefix to the first entry
  completion_t* cm = completions_get(cms, 0);
  if (cm == NULL) return -1;

  char prefix[RP_MAX_PREFIX+1];
  ssize_t delete_before = cm->delete_before;
  rp_strncpy( prefix, RP_MAX_PREFIX+1, cm->replacement, RP_MAX_PREFIX );
  prefix[RP_MAX_PREFIX] = 0;
  
  // and visit all others to find the longest common prefix
  for(ssize_t i = 1; i < cms->count; i++) {
    cm = completions_get(cms,i);
    if (cm->delete_before != delete_before) {  // deletions must match delete_before
      prefix[0] = 0;
      break;
    }
    // check if it is still a prefix
    const char* r = cm->replacement;    
    ssize_t j;
    for(j = 0; prefix[j] != 0 && r[j] != 0; j++) {
      if (prefix[j] != r[j]) break;
    }
    prefix[j] = 0;
    if (j <= 0) break;
  }

  // check the length
  ssize_t len = rp_strlen(prefix);
  if (len <= 0 || len < delete_before) return -1;

  // we found a prefix :-)
  completion_t cprefix;
  cprefix.delete_after = 0;
  cprefix.delete_before = delete_before;
  cprefix.display = NULL;
  cprefix.replacement = prefix;
  ssize_t newpos = completion_apply( &cprefix, sbuf, pos);
  if (newpos < 0) return newpos;  

  // adjust all delete_before for the new replacement
  for( ssize_t i = 0; i < cms->count; i++) {
    cm = completions_get(cms,i);
    cm->delete_before += len;
  }

  return newpos;
}


//-------------------------------------------------------------
// Completer functions
//-------------------------------------------------------------

rp_public bool rp_add_completion( rp_completion_env_t* cenv, const char* display, const char* replacement ) {
  return rp_add_completion_ex(cenv,display,replacement,0,0);
}

rp_public bool rp_add_completion_ex(rp_completion_env_t* cenv, const char* display, const char* replacement, long delete_before, long delete_after) {
  return (*cenv->complete)(cenv->env, cenv->closure, display, replacement, delete_before, delete_after );
}

static bool prim_add_completion(rp_env_t* env, void* funenv, const char* display, const char* replacement, long delete_before, long delete_after) {
  rp_unused(funenv);
  return completions_add(env->completions, display, replacement, delete_before, delete_after);
}

rp_public void rp_set_completer(rp_completer_fun_t* completer, void* arg) {
  rp_env_t* env = rp_get_env(); if (env == NULL) return;
  completions_set_completer(env->completions, completer, arg);
}


rp_public char* rp_readline_with_completer(const char* prompt_text, 
                                           rp_completer_fun_t* completer, void* completer_arg ) 
{
  rp_env_t* env = rp_get_env(); if (env == NULL) return NULL;
  rp_completer_fun_t* prev_completer;
  void* prev_completer_arg;
  completions_get_completer(env->completions, &prev_completer, &prev_completer_arg);
  rp_set_completer( completer, completer_arg);
  char* res = rp_readline( prompt_text );
  rp_set_completer( prev_completer, prev_completer_arg);
  return res;
}


rp_private ssize_t completions_generate(struct rp_env_s* env, completions_t* cms, const char* input, ssize_t pos, ssize_t max) {
  completions_clear(cms);
  if (cms->completer == NULL || input == NULL || rp_strlen(input) < pos) return 0;

  // set up env
  rp_completion_env_t cenv;
  cenv.env = env;
  cenv.input = input,
  cenv.cursor = (long)pos;
  cenv.arg = cms->completer_arg;
  cenv.complete = &prim_add_completion;
  cenv.closure  = NULL;
  const char* prefix = mem_strndup(cms->mem, input, pos);
  cms->completer_max = max;
  
  // and complete
  cms->completer(&cenv,prefix);

  // restore
  mem_free(cms->mem,prefix);
  return completions_count(cms);
}

// The default completer is no completion is set
static void default_filename_completer( rp_completion_env_t* cenv, const char* prefix ) {
  #ifdef _WIN32
  const char sep = '\\';
  #else
  const char sep = '/';
  #endif
  rp_complete_filename( cenv, prefix, sep, ".", NULL);
}
