/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#pragma once
#ifndef IC_HISTORY_H
#define IC_HISTORY_H

#include "common.h"

//-------------------------------------------------------------
// History
//-------------------------------------------------------------

struct history_s;
typedef struct history_s history_t;

ic_private history_t* history_new(alloc_t* mem);
ic_private void     history_free(history_t* h);
ic_private void     history_clear(history_t* h);   /// Called from public API
ic_private bool     history_enable_duplicates(history_t* h, bool enable);   /// Called from public API
ic_private ssize_t  history_count_with_prefix(const history_t* h, const char *prefix);

ic_private void     history_load_from(history_t* h, const char* fname, long max_entries);   /// Called from public API
ic_private void     history_load(history_t* h);
ic_private void     history_save(const history_t* h);

ic_private bool     history_push(history_t* h, const char* entry);   /// Called from public API
ic_private const char* history_get(const history_t* h, ssize_t n);
ic_private const char* history_get_with_prefix(const history_t* h, ssize_t n, const char* prefix);
ic_private void     history_remove_last(history_t* h);   /// Called from public API

#endif // IC_HISTORY_H
