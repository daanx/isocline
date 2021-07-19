/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/

#pragma once
#ifndef RP_COMMON_H
#define RP_COMMON_H

//-------------------------------------------------------------
// Headers and defines
//-------------------------------------------------------------

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
#  define rp_public     rp_extern_c 
# if defined(__GNUC__) // includes clang and icc      
#  define rp_private    __attribute__((visibility("hidden")))
# else
#  define rp_private  
# endif
#else
# define rp_private     static
# define rp_public      rp_extern_c
#endif

#define rp_unused(x)    (void)(x)


//-------------------------------------------------------------
// ssize_t
//-------------------------------------------------------------

#if defined(_MSC_VER)
typedef intptr_t ssize_t;
#endif

#define ssizeof(tp)   (ssize_t)(sizeof(tp))
static inline size_t  to_size_t(ssize_t sz) { return (sz >= 0 ? (size_t)sz : 0); }
static inline ssize_t to_ssize_t(size_t sz) { return (sz <= SIZE_MAX/2 ? (ssize_t)sz : 0); }

rp_private ssize_t rp_strlen(const char* s);
rp_private void    rp_memmove(void* dest, const void* src, ssize_t n);
rp_private void    rp_memcpy(void* dest, const void* src, ssize_t n);
rp_private bool    rp_memnmove(void* dest, ssize_t dest_size, const void* src, ssize_t n);
rp_private bool    rp_strcpy(char* dest, ssize_t dest_size /* including 0 */, const char* src);
rp_private bool    rp_strncpy(char* dest, ssize_t dest_size /* including 0 */, const char* src, ssize_t n);


//-------------------------------------------------------------
// Debug
//-------------------------------------------------------------

#ifdef RP_DEBUG_MSG
rp_private void debug_msg( const char* fmt, ... );
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


rp_private void* mem_malloc( alloc_t* mem, ssize_t sz );
rp_private void* mem_zalloc( alloc_t* mem, ssize_t sz );
rp_private void* mem_realloc( alloc_t* mem, void* p, ssize_t newsz );
rp_private void  mem_free( alloc_t* mem, const void* p );
rp_private char* mem_strdup( alloc_t* mem, const char* s);
rp_private char* mem_strndup( alloc_t* mem, const char* s, ssize_t n);

#define mem_zalloc_tp(mem,tp)        (tp*)mem_zalloc(mem,ssizeof(tp))
#define mem_malloc_tp_n(mem,tp,n)    (tp*)mem_malloc(mem,(n)*ssizeof(tp))
#define mem_zalloc_tp_n(mem,tp,n)    (tp*)mem_zalloc(mem,(n)*ssizeof(tp))
#define mem_realloc_tp(mem,tp,p,n)   (tp*)mem_realloc(mem,p,(n)*ssizeof(tp))


#endif // RP_COMMON_H
