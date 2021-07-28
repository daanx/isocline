/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#pragma once
#ifndef RP_HIGHLIGHT_H
#define RP_HIGHLIGHT_H

#include "common.h"
#include "term.h"

//-------------------------------------------------------------
// Syntax highlighting
//-------------------------------------------------------------

rp_private rp_highlight_env_t* highlight_new( alloc_t* mem );
rp_private void highlight_free( rp_highlight_env_t* henv );
rp_private bool highlight_insert_at( rp_highlight_env_t* henv, ssize_t pos, ssize_t len, rp_color_t color );
rp_private void highlight_clear( rp_highlight_env_t* henv );
rp_private bool highlight_init( rp_highlight_env_t* henv, const char* s, rp_highlight_fun_t* highlighter );
rp_private void highlight_term_write( rp_highlight_env_t* henv, term_t* term, const char* s, ssize_t start, ssize_t len );

#endif // RP_HIGHLIGHT_H
