/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#pragma once
#ifndef RP_TERM_H
#define RP_TERM_H

#include "../include/repline.h"  // rp_color_t
#include "common.h"
#include "tty.h"

struct term_s;
typedef struct term_s term_t;

// Primitives
internal term_t* term_new(alloc_t* mem, tty_t* tty, bool monochrome, bool silent, int fout);
internal void term_free(term_t* term);

internal bool term_is_interactive(const term_t* term);
internal void term_start_raw(term_t* term);
internal void term_end_raw(term_t* term);

internal void term_enable_beep(term_t* term, bool enable);
internal void term_enable_color(term_t* term, bool enable);

internal bool term_write_n(term_t* term, const char* s, ssize_t n);
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
internal void term_bgcolor(term_t* term, rp_color_t color);
*/

internal void term_attr_reset(term_t* term);
internal void term_underline(term_t* term, bool on);
internal void term_color(term_t* term, rp_color_t color);

#endif // RP_TERM_H
