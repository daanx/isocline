/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/

#include <string.h>
#include "common.h"


//-------------------------------------------------------------
// String wrappers for ssize_t
//-------------------------------------------------------------

rp_private ssize_t rp_strlen( const char* s ) {
  if (s==NULL) return 0;
  return to_ssize_t(strlen(s));
}

rp_private void rp_memmove( void* dest, const void* src, ssize_t n ) {
  assert(dest!=NULL && src != NULL);
  if (n <= 0) return;
  memmove(dest,src,to_size_t(n));
}


rp_private void rp_memcpy( void* dest, const void* src, ssize_t n ) {
  assert(dest!=NULL && src != NULL);
  if (n <= 0) return;
  memcpy(dest,src,to_size_t(n));
}


rp_private bool rp_memnmove( void* dest, ssize_t dest_size, const void* src, ssize_t n ) {
  assert(dest!=NULL && src != NULL);
  if (n <= 0) return true;
  if (dest_size < n) { assert(false); return false; }
  memmove(dest,src,to_size_t(n));
  return true;
}

rp_private bool rp_strcpy( char* dest, ssize_t dest_size /* including 0 */, const char* src) {
  assert(dest!=NULL && src != NULL);
  if (dest == NULL || dest_size <= 0) return false;
  ssize_t slen = rp_strlen(src);
  if (slen >= dest_size) return false;
  strcpy(dest,src);
  assert(dest[slen] == 0);
  return true;
}

rp_private bool rp_strncpy( char* dest, ssize_t dest_size /* including 0 */, const char* src, ssize_t n) {
  assert(dest!=NULL && src != NULL && n < dest_size);
  if (dest == NULL || dest_size <= 0) return false;
  if (n >= dest_size) return false;
  strncpy(dest,src,to_size_t(n));
  dest[n] = 0;
  return true;
}


//-------------------------------------------------------------
// Debug
//-------------------------------------------------------------


#ifdef RP_DEBUG_MSG
rp_private void debug_msg(const char* fmt, ...) {
  static bool debug_init;
  FILE* fdbg = fopen("repline.debug.txt", (debug_init ? "a" : "w"));
  debug_init = true;
  if (fdbg==NULL) return;
  va_list args;
  va_start(args, fmt);
  vfprintf(fdbg, fmt, args);
  fclose(fdbg);
  va_end(args);
}
#endif


//-------------------------------------------------------------
// Allocation
//-------------------------------------------------------------

rp_private void* mem_malloc(alloc_t* mem, ssize_t sz) {
  return mem->malloc(to_size_t(sz));
}

rp_private void* mem_zalloc(alloc_t* mem, ssize_t sz) {
  void* p = mem_malloc(mem, sz);
  if (p != NULL) memset(p, 0, to_size_t(sz));
  return p;
}

rp_private void* mem_realloc(alloc_t* mem, void* p, ssize_t newsz) {
  return mem->realloc(p, to_size_t(newsz));
}

rp_private void mem_free(alloc_t* mem, const void* p) {
  mem->free((void*)p);
}

rp_private char* mem_strdup(alloc_t* mem, const char* s) {
  if (s==NULL) return NULL;
  ssize_t n = rp_strlen(s);
  char* p = mem_malloc_tp_n(mem, char, n+1);
  if (p == NULL) return NULL;
  rp_memcpy(p, s, n+1);
  return p;
}

rp_private char* mem_strndup(alloc_t* mem, const char* s, ssize_t n) {
  if (s==NULL || n < 0) return NULL;
  char* p = mem_malloc_tp_n(mem, char, n+1);
  if (p == NULL) return NULL;
  ssize_t i;
  for (i = 0; i < n && s[i] != 0; i++) {
    p[i] = s[i];
  }
  assert(i <= n);
  p[i] = 0;
  return p;
}

