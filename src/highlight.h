/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#pragma once
#ifndef IC_HIGHLIGHT_H
#define IC_HIGHLIGHT_H

#include "common.h"
#include "term.h"

//-------------------------------------------------------------
// Syntax highlighting
//-------------------------------------------------------------

ic_private ic_highlight_env_t* highlight_new( alloc_t* mem );
ic_private void highlight_free( ic_highlight_env_t* henv );
ic_private bool highlight_insert_at( ic_highlight_env_t* henv, ssize_t pos, ssize_t len, ic_color_t color );
ic_private void highlight_clear( ic_highlight_env_t* henv );
ic_private bool highlight_init( ic_highlight_env_t* henv, const char* s, ic_highlight_fun_t* highlighter, void* arg );
ic_private void highlight_term_write( ic_highlight_env_t* henv, term_t* term, const char* s, ssize_t start, ssize_t len );

#endif // IC_HIGHLIGHT_H
