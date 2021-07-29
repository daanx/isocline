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
rp_private term_t* term_new(alloc_t* mem, tty_t* tty, bool nocolor, bool silent, int fd_out);
rp_private void term_free(term_t* term);

rp_private bool term_is_interactive(const term_t* term);
rp_private void term_start_raw(term_t* term);
rp_private void term_end_raw(term_t* term);

rp_private bool term_enable_beep(term_t* term, bool enable);
rp_private bool term_enable_color(term_t* term, bool enable);

rp_private bool term_write_n(term_t* term, const char* s, ssize_t n);
rp_private bool term_write(term_t* term, const char* s);
rp_private bool term_writeln(term_t* term, const char* s);
rp_private bool term_write_char(term_t* term, char c);

rp_private bool term_write_repeat(term_t* term, const char* s, ssize_t count );
rp_private void term_beep(term_t* term);

rp_private bool term_update_dim(term_t* term);

rp_private ssize_t term_get_width(term_t* term);
rp_private ssize_t term_get_height(term_t* term);

// Helpers
rp_private bool term_writef(term_t* term, ssize_t max_needed, const char* fmt, ...);
rp_private void term_left(term_t* term, ssize_t n);
rp_private void term_right(term_t* term, ssize_t n);
rp_private void term_up(term_t* term, ssize_t n);
rp_private void term_down(term_t* term, ssize_t n);
rp_private void term_start_of_line(term_t* term );
rp_private void term_clear_line(term_t* term);

rp_private void term_start_buffered(term_t* term);
rp_private bool term_end_buffered(term_t* term);

/*
internal void term_end_of_line(term_t* term );
internal void term_clear_line_from_cursor(term_t* term);
internal void term_clear_screen(term_t* term);
internal void term_clear(term_t* term, ssize_t n);
internal void term_bold(term_t* term);
internal void term_italic(term_t* term);
*/

rp_private void term_attr_reset(term_t* term);
rp_private void term_underline(term_t* term, bool on);
rp_private void term_reverse(term_t* term, bool on);
rp_private void term_color(term_t* term, rp_color_t color);
rp_private void term_bgcolor(term_t* term, rp_color_t color);

#endif // RP_TERM_H
