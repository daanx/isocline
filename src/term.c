/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>  // getenv

#include "common.h"
#include "tty.h"
#include "term.h"

#if defined(_WIN32)
#else
#include <unistd.h>
#include <sys/ioctl.h>
#endif

#define RL_CSI      "\x1B["


static bool term_vwritef(const term_t* term, const char* fmt, va_list args );



//-------------------------------------------------------------
// Helpers
//-------------------------------------------------------------

internal void term_left(const term_t* term, ssize_t n) {
  if (n <= 0) return;
  term_writef( term, RL_CSI "%zdD", n );
}

internal void term_right(const term_t* term, ssize_t n) {
  if (n <= 0) return;
  term_writef( term, RL_CSI "%zdC", n );
}

internal void term_up(const term_t* term, ssize_t n) {
  if (n <= 0) return;
  term_writef( term, RL_CSI "%zdA", n );
}

internal void term_down(const term_t* term, ssize_t n) {
  if (n <= 0) return;
  term_writef( term, RL_CSI "%zdB", n );
}

internal void term_clear_line(const term_t* term) {
  term_write( term, "\r" RL_CSI "2K");
}

internal void term_start_of_line(const term_t* term) {
  term_write( term, "\r" );
}

internal ssize_t term_get_width(const term_t* term) {
  return term->width;
}

internal ssize_t term_get_height(const term_t* term) {
  return term->height;
}

internal void term_reset(const term_t* term) {
  term_write(term, RL_CSI "0m" );
}

internal void term_underline(const term_t* term) {
  term_write(term, RL_CSI "4m" );
}

internal void term_color(const term_t* term, rl_color_t color) {
  term_writef(term, RL_CSI "%dm", color );
}


// Unused for now
/*
internal void term_bold(const term_t* term) {
  term_write(term, RL_CSI "1m" );
}

internal void term_italic(const term_t* term) {
  term_write(term, RL_CSI "2m" );
}

internal void term_bgcolor(const term_t* term, rl_color_t color) {
  term_writef(term, RL_CSI "%dm", color + 10 );
}

internal void term_end_of_line(const term_t* term) {
  term_right( term, 999 );
}

internal void term_clear_screen(const term_t* term) {
  term_write( term, RL_CSI "2J" RL_CSI "H" );
}

internal void term_clear_line_from_cursor(const term_t* term) {
  term_write( term, RL_CSI "0K");
}

internal void term_clear(const term_t* term, ssize_t n) {
  if (n <= 0) return;
  char buf[RL_MAX_LINE];
  memset(buf,' ',(n >= RL_MAX_LINE ? RL_MAX_LINE-1 : n));
  buf[RL_MAX_LINE-1] = 0;
  term_write( term, buf );
}
*/



//-------------------------------------------------------------
// Formatted output
//-------------------------------------------------------------

internal bool term_writef(const term_t* term, const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  int err = term_vwritef(term,fmt,ap);
  va_end(ap);
  return err;
}

internal bool term_vwritef(const term_t* term, const char* fmt, va_list args ) {
  char buf[RL_MAX_LINE];
  vsnprintf( buf, RL_MAX_LINE-1, fmt, args );
  buf[RL_MAX_LINE-1] = 0;
  return term_write(term,buf);
}


//-------------------------------------------------------------
// Primitive
//-------------------------------------------------------------

internal void term_beep(const term_t* term) {
  if (term->silent) return;
  fprintf(stderr,"\x7");
  fflush(stderr);
}

internal bool term_write(const term_t* term, const char* s) {
  // todo: strip colors on monochrome
  ssize_t n = rl_strlen(s);
  return (write(term->fout, s, to_size_t(n)) == n);
}

static bool term_get_cursor_pos( const term_t* term, tty_t* tty, int* row, int* col) 
{
  // send request
  if (!term_write(term, RL_CSI "6n")) return false;
 
  // parse response ESC[%d;%dR
  char buf[64];
  int len = 0;
  char c;
  if (!tty_readc_peek(tty,&c) || c != '\x1B') return false;
  if (!tty_readc_peek(tty,&c) || c != '[')    return false;
  while( len < 63 ) {
    if (!tty_readc_peek(tty,&c)) return false;
    if (c == 'R') break;
    buf[len] = c;
    len++;
  }
  buf[len] = 0;
  return (sscanf(buf,"%d;%d",row,col) == 2);
}

static void term_set_cursor_pos( const term_t* term, int row, int col ) {
  term_writef( term, RL_CSI "%d;%dH", row, col );
}

internal bool term_update_dim(term_t* term, tty_t* tty) {
  int cols = 0;
  int rows = 0;
  struct winsize ws;
  if (ioctl(1, TIOCGWINSZ, &ws) >= 0) {
    // ioctl succeeded
    cols = ws.ws_col;
    rows = ws.ws_row;
  }
  else {
    // determine width by querying the cursor position
    debug_msg("term: ioctl term-size failed: %d,%d\n", ws.ws_row, ws.ws_col);
    int col0 = 0;
    int row0 = 0;
    if (term_get_cursor_pos(term,tty,&row0,&col0)) {
      term_set_cursor_pos(term,999,999);
      int col1 = 0;
      int row1 = 0;
      if (term_get_cursor_pos(term,tty,&row1,&col1)) {
        cols = col1;
        rows = row1;
      }
      term_set_cursor_pos(term,row0,col0);
    }
    else {
      // cannot query position
      return false;
    }
  }

  // update width and return if it changed.
  if (cols <= 0 || rows <= 0) return false;  // debuggers return 0 columns
  debug_msg("terminal dim: %d,%d\n", rows, cols);
  bool changed = (term->width != cols || term->height != rows);
  term->width = cols;
  term->height = rows;
  return changed;  
}

internal bool term_init(term_t* term, tty_t* tty, bool monochrome, bool silent, int fout) 
{
  term->fout = (fout < 0 ? STDOUT_FILENO : fout);
  term->monochrome = monochrome;
  term->silent = silent;
  term->width = 80;
  term->height = 25;
  // check dimensions
  if (!term_update_dim(term,tty)) {
    return false; 
  }  
  // check editing support
  const char* eterm = getenv("TERM");
  debug_msg("term: TERM=%s\n", eterm);
  if (eterm != NULL &&
      (strstr(eterm,"dumb|DUMB|cons25|CONS25|emacs|EMACS") != NULL)) {
    return false;
  }

  return true;
}

internal void term_done(term_t* term) {
  (void)(term);
  // nothing to do
}
