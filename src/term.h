/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#pragma once
#ifndef IC_TERM_H
#define IC_TERM_H

#include "../include/isocline.h"  // ic_color_t
#include "common.h"
#include "tty.h"
#include "stringbuf.h"

struct term_s;
typedef struct term_s term_t;

typedef enum buffer_mode_e {
  UNBUFFERED,
  LINEBUFFERED,
  BUFFERED,
} buffer_mode_t;

// Primitives
ic_private term_t* term_new(alloc_t* mem, tty_t* tty, bool nocolor, bool silent, int fd_out);
ic_private void term_free(term_t* term);

ic_private bool term_is_interactive(const term_t* term);
ic_private void term_start_raw(term_t* term);
ic_private void term_end_raw(term_t* term);

ic_private bool term_enable_beep(term_t* term, bool enable);
ic_private bool term_enable_color(term_t* term, bool enable);

ic_private void term_flush(term_t* term);
ic_private buffer_mode_t term_set_buffer_mode(term_t* term, buffer_mode_t mode);

ic_private void term_write_n(term_t* term, const char* s, ssize_t n);
ic_private void term_write(term_t* term, const char* s);
ic_private void term_writeln(term_t* term, const char* s);
ic_private void term_write_char(term_t* term, char c);

ic_private void term_write_repeat(term_t* term, const char* s, ssize_t count );
ic_private void term_beep(term_t* term);

ic_private bool term_update_dim(term_t* term);

ic_private ssize_t term_get_width(term_t* term);
ic_private ssize_t term_get_height(term_t* term);
ic_private int  term_get_color_bits(term_t* term);

// Helpers
ic_private void term_writef(term_t* term, const char* fmt, ...);
ic_private void term_vwritef(term_t* term, const char* fmt, va_list args);

ic_private void term_left(term_t* term, ssize_t n);
ic_private void term_right(term_t* term, ssize_t n);
ic_private void term_up(term_t* term, ssize_t n);
ic_private void term_down(term_t* term, ssize_t n);
ic_private void term_start_of_line(term_t* term );
ic_private void term_clear_line(term_t* term);
// ic_private void term_clear_lines_to_end(term_t* term);


ic_private void term_attr_reset(term_t* term);
ic_private void term_underline(term_t* term, bool on);
ic_private void term_reverse(term_t* term, bool on);
ic_private void term_bold(term_t* term, bool on);
ic_private void term_italic(term_t* term, bool on);

ic_private void term_color(term_t* term, ic_color_t color);
ic_private void term_bgcolor(term_t* term, ic_color_t color);

ic_private void term_append_color(term_t* term, stringbuf_t* sbuf, ic_color_t color);
ic_private void term_append_bgcolor(term_t* term, stringbuf_t* sbuf, ic_color_t color);


#define IC_ON   (1)
#define IC_OFF  (-1)
#define IC_NONE (0)

typedef struct term_attr_s {
  ic_color_t color;
  ic_color_t bgcolor;
  int8_t     bold;
  int8_t     reverse;
  int8_t     underline;
  int8_t     italic;
} term_attr_t;

ic_private term_attr_t term_attr_none(void);
ic_private term_attr_t term_attr_default(void);

ic_private term_attr_t term_get_attr( const term_t* term );
ic_private void        term_set_attr( term_t* term, term_attr_t attr );

#endif // IC_TERM_H
