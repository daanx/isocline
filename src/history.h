/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#pragma once
#ifndef RP_HISTORY_H
#define RP_HISTORY_H

#include "common.h"

//-------------------------------------------------------------
// History
//-------------------------------------------------------------

struct history_s;
typedef struct history_s history_t;

internal history_t* history_new(alloc_t* mem);
internal void     history_free(history_t* h);
internal void     history_clear(history_t* h);
internal void     history_enable_duplicates( history_t* h, bool enable );

internal void     history_load( history_t* h );
internal void     history_save( const history_t* h );
internal void     history_load_from(history_t* h, const char* fname, long max_entries );

internal bool     history_push( history_t* h, const char* entry );
internal bool     history_update( history_t* h, const char* entry );
internal const char* history_get( const history_t* h, ssize_t n );
internal void     history_remove_last(history_t* h);

internal bool     history_search( const history_t* h, ssize_t from, const char* search, bool backward, ssize_t* hidx, ssize_t* hpos);


#endif // RP_HISTORY_H
