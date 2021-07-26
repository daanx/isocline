/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/

#include <string.h>
#include <stdio.h>
#include <stdarg.h>
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
// Unicode
// QUTF-8: See <https://github.com/koka-lang/koka/blob/master/kklib/include/kklib/string.h>
// Raw bytes are code points 0xEE000 - 0xEE0FF
//-------------------------------------------------------------
#define RP_UNICODE_RAW   ((unicode_t)(0xEE000U))

rp_private unicode_t unicode_from_raw(uint8_t c) {
  return (RP_UNICODE_RAW + c);
}

static bool unicode_is_raw_utf8(unicode_t u, uint8_t* c) {
  if (u >= RP_UNICODE_RAW && u <= RP_UNICODE_RAW + 0xFF) {
    *c = (uint8_t)(u - RP_UNICODE_RAW);
    return true;
  }
  else {
    return false;
  }
}

rp_private void unicode_to_qutf8(unicode_t u, uint8_t buf[5]) {
  memset(buf, 0, 5);
  if (u <= 0x7F) {
    buf[0] = (uint8_t)u;
  }
  else if (u <= 0x07FF) {
    buf[0] = (0xC0 | ((uint8_t)(u >> 6)));
    buf[1] = (0x80 | (((uint8_t)u) & 0x3F));
  }
  else if (u <= 0xFFFF) {
    buf[0] = (0xE0 |  ((uint8_t)(u >> 12)));
    buf[1] = (0x80 | (((uint8_t)(u >>  6)) & 0x3F));
    buf[2] = (0x80 | (((uint8_t)u) & 0x3F));
  }
  else if (u <= 0x10FFFF) {
    if (unicode_is_raw_utf8(u, &buf[0])) {
      buf[1] = 0;
    }
    else {
      buf[0] = (0xF0 |  ((uint8_t)(u >> 18)));
      buf[1] = (0x80 | (((uint8_t)(u >> 12)) & 0x3F));
      buf[2] = (0x80 | (((uint8_t)(u >>  6)) & 0x3F));
      buf[3] = (0x80 | (((uint8_t)u) & 0x3F));
    }
  }
}

// is this a utf8 continuation byte?
static inline bool is_cont(uint8_t c) {
  return ((c & 0xC0) == 0x80);
}

rp_private unicode_t unicode_from_qutf8(const uint8_t* s, ssize_t len, ssize_t* count) {
  unicode_t c0 = 0;
  if (len <= 0 || s == NULL) {
    goto fail;
  }
  // 1 byte
  c0 = s[0];
  if (c0 <= 0x7F && len >= 1) {
    if (count != NULL) *count = 1;
    return c0; 
  }
  else if (c0 <= 0xC1) { // invalid continuation byte or invalid 0xC0, 0xC1
    goto fail;
  }
  // 2 bytes
  else if (c0 <= 0xDF && len >= 2 && is_cont(s[1])) { 
    if (count != NULL) *count = 2;
    return (((c0 & 0x1F) << 6) | (s[1] & 0x3F));
  }
  // 3 bytes: reject overlong and surrogate halves
  else if (len >= 3 && 
           ((c0 == 0xE0 && s[1] >= 0xA0 && s[1] <= 0xBF && is_cont(s[2])) ||
            (c0 >= 0xE1 && c0 <= 0xEC && is_cont(s[1]) && is_cont(s[2])) 
          ))
  {
    if (count != NULL) *count = 3;
    return (((c0 & 0x0F) << 12) | ((unicode_t)(s[1] & 0x3F) << 6) | (s[2] & 0x3F));
  }
  // 4 bytes: reject overlong
  else if (len >= 4 && 
           (((c0 == 0xF0 && s[1] >= 0x90 && s[1] <= 0xBF && is_cont(s[2]) && is_cont(s[3])) ||
            (c0 >= 0xF1 && c0 <= 0xF3 && is_cont(s[1]) && is_cont(s[2]) && is_cont(s[3])) ||
            (c0 == 0xF4 && s[1] >= 0x80 && s[1] <= 0x8F && is_cont(s[2]) && is_cont(s[3]))) 
          )) 
  {
    if (count != NULL) *count = 4;
    return (((c0 & 0x07) << 18) | ((unicode_t)(s[1] & 0x3F) << 12) | ((unicode_t)(s[2] & 0x3F) << 6) | (s[3] & 0x3F));
  }  
fail:
  if (count != NULL) *count = 1;
  return unicode_from_raw(s[0]);
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

