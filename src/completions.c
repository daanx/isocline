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
  rp_completion_fun_t* completer;
  void* completer_arg;
  ssize_t completer_max;
  ssize_t count;
  ssize_t len;
  completion_t* elems;
  alloc_t* mem;
};



internal completions_t* completions_new(alloc_t* mem) {
  completions_t* cms = mem_zalloc_tp(mem, completions_t);
  if (cms == NULL) return NULL;
  cms->mem = mem;
  return cms;
}

internal void completions_free(completions_t* cms) {
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


internal void completions_clear(completions_t* cms) {  
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

internal ssize_t completions_count(completions_t* cms) {
  return cms->count;
}

internal ssize_t completions_generate(struct rp_env_s* env, completions_t* cms, const char* input, ssize_t pos, ssize_t max) {
  completions_clear(cms);
  if (cms->completer == NULL || input == NULL || input[0] == 0 || rp_strlen(input) < pos) return 0;
  cms->completer_max = max;
  cms->completer(env,input,(long)pos,cms->completer_arg);
  return completions_count(cms);
}


internal bool completions_add(completions_t* cms, const char* display, const char* replacement, ssize_t delete_before, ssize_t delete_after) {
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

internal const char* completions_get_display( completions_t* cms, ssize_t index ) {
  completion_t* cm = completions_get(cms, index);
  if (cm == NULL) return NULL;
  return (cm->display != NULL ? cm->display : cm->replacement);
}


internal ssize_t completions_apply( completions_t* cms, ssize_t index, stringbuf_t* sbuf, ssize_t pos ) {
  completion_t* cm = completions_get(cms, index);
  if (cm == NULL) return -1;
  debug_msg( "completion: apply: %s at %zd\n", cm->replacement, pos);
  ssize_t start = pos - cm->delete_before;
  if (start < 0) start = 0;
  sbuf_delete_from_to( sbuf, start, pos + cm->delete_after );
  return sbuf_insert_at(sbuf, cm->replacement, start); 
}

internal void completions_set_completer(completions_t* cms, rp_completion_fun_t* completer, void* arg) {
  cms->completer = completer;
  cms->completer_arg = arg;
}


