/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#pragma once
#ifndef RL_TERM_H
#define RL_TERM_H

#include "../include/repline.h"  // rl_color_t
#include "common.h"
#include "tty.h"

#define RL_MAX_LINE 4096

#ifdef _WIN32
#include <windows.h>
#endif

typedef struct term_s {
  int     fout;
  ssize_t width;
  ssize_t height;
  bool    monochrome;
  bool    silent;
  bool    raw_enabled;
  bool    buffered;
  char*   buf;
  ssize_t bufcount;
  ssize_t buflen;
  alloc_t* mem;  
  #ifdef _WIN32
  HANDLE  hcon;
  WORD    hcon_default_attr;  
  WORD    hcon_orig_attr;
  DWORD   hcon_orig_mode;
  UINT    hcon_orig_cp;  
  #endif
} term_t;

// Primitives
internal bool term_init(term_t* term, tty_t* tty, alloc_t* mem, bool monochrome, bool silent, int fout);
internal void term_done(term_t* term);
internal void term_start_raw(term_t* term);
internal void term_end_raw(term_t* term);
internal bool term_write(term_t* term, const char* s);
internal void term_beep(term_t* term);
internal bool term_update_dim(term_t* term, tty_t* tty);
internal ssize_t term_get_width(term_t* term);
internal ssize_t term_get_height(term_t* term);

// Helpers
internal bool term_writef(term_t* term, const char* fmt, ...);
internal void term_left(term_t* term, ssize_t n);
internal void term_right(term_t* term, ssize_t n);
internal void term_up(term_t* term, ssize_t n);
internal void term_down(term_t* term, ssize_t n);
internal void term_start_of_line(term_t* term );
internal void term_clear_line(term_t* term);

internal void term_start_buffered(term_t* term);
internal bool term_end_buffered(term_t* term);

/*
internal void term_end_of_line(term_t* term );
internal void term_clear_line_from_cursor(term_t* term);
internal void term_clear_screen(term_t* term);
internal void term_clear(term_t* term, ssize_t n);
internal void term_bold(term_t* term);
internal void term_italic(term_t* term);
internal void term_bgcolor(term_t* term, rl_color_t color);
*/

internal void term_reset(term_t* term);
internal void term_underline(term_t* term);
internal void term_color(term_t* term, rl_color_t color);

#endif // RL_TERM_H
