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

rp_private history_t* history_new(alloc_t* mem);
rp_private void     history_free(history_t* h);
rp_private void     history_clear(history_t* h);
rp_private void     history_enable_duplicates( history_t* h, bool enable );

rp_private void     history_load_from(history_t* h, const char* fname, long max_entries);
rp_private void     history_load( history_t* h );
rp_private void     history_save( const history_t* h );

rp_private bool     history_push( history_t* h, const char* entry );
rp_private bool     history_update( history_t* h, const char* entry );
rp_private const char* history_get( const history_t* h, ssize_t n );
rp_private void     history_remove_last(history_t* h);

rp_private bool     history_search( const history_t* h, ssize_t from, const char* search, bool backward, ssize_t* hidx, ssize_t* hpos);


#endif // RP_HISTORY_H
