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


typedef struct term_s {
  int  fout;
  int  width;
  int  height;
  bool monochrome;
  bool silent;
} term_t;

// Primitives
internal bool term_init(term_t* term, tty_t* tty, bool monochrome, bool silent, int fout);
internal void term_done(term_t* term);
internal bool term_write(const term_t* term, const char* s);
internal void term_beep(const term_t* term);
internal bool term_update_dim(term_t* term, tty_t* tty);
internal ssize_t term_get_width(const term_t* term);
internal ssize_t term_get_height(const term_t* term);

// Helpers
internal bool term_writef(const term_t* term, const char* fmt, ...);
internal void term_left(const term_t* term, ssize_t n);
internal void term_right(const term_t* term, ssize_t n);
internal void term_up(const term_t* term, ssize_t n);
internal void term_down(const term_t* term, ssize_t n);
internal void term_start_of_line(const term_t* term );
internal void term_clear_line(const term_t* term);
/*
internal void term_end_of_line(const term_t* term );
internal void term_clear_line_from_cursor(const term_t* term);
internal void term_clear_screen(const term_t* term);
internal void term_clear(const term_t* term, ssize_t n);
internal void term_bold(const term_t* term);
internal void term_italic(const term_t* term);
internal void term_bgcolor(const term_t* term, rl_color_t color);
*/

internal void term_reset(const term_t* term);
internal void term_underline(const term_t* term);
internal void term_color(const term_t* term, rl_color_t color);

#endif // RL_TERM_H
