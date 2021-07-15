/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/

#pragma once
#ifndef RP_COMMON_H
#define RP_COMMON_H

#include <sys/types.h>  // ssize_t
#include <limits.h>
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include "../include/repline.h"  // rp_malloc_fun_t, rp_color_t etc.

# ifdef __cplusplus
#  define rp_extern_c   extern "C"
# else
#  define rp_extern_c
# endif

#if defined(RP_SEPARATE_OBJS)
#  define exported  rp_extern_c 
# if defined(__GNUC__) // includes clang and icc      
#  define internal  __attribute__((visibility("hidden")))
# else
#  define internal  
# endif
#else
# define internal   static
# define exported   rp_extern_c
#endif

#define unused(x)  (void)(x)


//-------------------------------------------------------------
// ssize_t
//-------------------------------------------------------------

#if defined(_MSC_VER)
typedef intptr_t ssize_t;
#endif

#define ssizeof(tp)   (ssize_t)(sizeof(tp))
static inline size_t  to_size_t(ssize_t sz) { return (sz >= 0 ? (size_t)sz : 0); }
static inline ssize_t to_ssize_t(size_t sz) { return (sz <= SIZE_MAX/2 ? (ssize_t)sz : 0); }

static inline ssize_t rp_strlen( const char* s ) {
  if (s==NULL) return 0;
  return to_ssize_t(strlen(s));
}

static inline void rp_memmove( void* dest, const void* src, ssize_t n ) {
  assert(dest!=NULL && src != NULL);
  if (n <= 0) return;
  memmove(dest,src,to_size_t(n));
}


static inline void rp_memcpy( void* dest, const void* src, ssize_t n ) {
  assert(dest!=NULL && src != NULL);
  if (n <= 0) return;
  memcpy(dest,src,to_size_t(n));
}


static inline bool rp_memnmove( void* dest, ssize_t dest_size, const void* src, ssize_t n ) {
  assert(dest!=NULL && src != NULL);
  if (n <= 0) return true;
  if (dest_size < n) { assert(false); return false; }
  memmove(dest,src,to_size_t(n));
  return true;
}

static inline bool rp_strcpy( char* dest, ssize_t dest_size /* including 0 */, const char* src) {
  assert(dest!=NULL && src != NULL);
  if (dest == NULL || dest_size <= 0) return false;
  ssize_t slen = rp_strlen(src);
  if (slen >= dest_size) return false;
  strcpy(dest,src);
  assert(dest[slen] == 0);
  return true;
}


//-------------------------------------------------------------
// Debug
//-------------------------------------------------------------

#ifdef RP_DEBUG_MSG
internal void debug_msg( const char* fmt, ... );
#else
#define debug_msg(...)
#endif

//-------------------------------------------------------------
// Allocation
//-------------------------------------------------------------

typedef struct alloc_s {
  rp_malloc_fun_t*  malloc;
  rp_realloc_fun_t* realloc;
  rp_free_fun_t*    free;
} alloc_t;


internal void* mem_malloc( alloc_t* mem, ssize_t sz );
internal void* mem_zalloc( alloc_t* mem, ssize_t sz );
internal void* mem_realloc( alloc_t* mem, void* p, ssize_t newsz );
internal void  mem_free( alloc_t* mem, const void* p );
internal char* mem_strdup( alloc_t* mem, const char* s);

#define mem_zalloc_tp(mem,tp)     (tp*)mem_zalloc(mem,ssizeof(tp))
#define mem_malloc_tp_n(mem,tp,n) (tp*)mem_malloc(mem,(n)*ssizeof(tp))

#endif // RP_COMMON_H
