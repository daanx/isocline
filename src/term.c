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
#define write(fd,s,n)   _write(fd,s,n)
#define STDOUT_FILENO 1
#else
#include <unistd.h>
#include <sys/ioctl.h>
#endif

#define RL_CSI      "\x1B["


static bool term_vwritef(term_t* term, const char* fmt, va_list args );
static bool term_buffered_ensure( term_t* term, ssize_t extra );


//-------------------------------------------------------------
// Helpers
//-------------------------------------------------------------

internal void term_left(term_t* term, ssize_t n) {
  if (n <= 0) return;
  term_writef( term, RL_CSI "%zdD", n );
}

internal void term_right(term_t* term, ssize_t n) {
  if (n <= 0) return;
  term_writef( term, RL_CSI "%zdC", n );
}

internal void term_up(term_t* term, ssize_t n) {
  if (n <= 0) return;
  term_writef( term, RL_CSI "%zdA", n );
}

internal void term_down(term_t* term, ssize_t n) {
  if (n <= 0) return;
  term_writef( term, RL_CSI "%zdB", n );
}

internal void term_clear_line(term_t* term) {
  term_write( term, "\r" RL_CSI "2K");
}

internal void term_start_of_line(term_t* term) {
  term_write( term, "\r" );
}

internal ssize_t term_get_width(term_t* term) {
  return term->width;
}

internal ssize_t term_get_height(term_t* term) {
  return term->height;
}

internal void term_reset(term_t* term) {
  term_write(term, RL_CSI "0m" );
}

internal void term_underline(term_t* term) {
  term_write(term, RL_CSI "4m" );
}

internal void term_color(term_t* term, rl_color_t color) {
  term_writef(term, RL_CSI "%dm", color );
}



// Unused for now
/*
internal void term_bold(term_t* term) {
  term_write(term, RL_CSI "1m" );
}

internal void term_italic(term_t* term) {
  term_write(term, RL_CSI "2m" );
}

internal void term_bgcolor(term_t* term, rl_color_t color) {
  term_writef(term, RL_CSI "%dm", color + 10 );
}

internal void term_end_of_line(term_t* term) {
  term_right( term, 999 );
}

internal void term_clear_screen(term_t* term) {
  term_write( term, RL_CSI "2J" RL_CSI "H" );
}

internal void term_clear_line_from_cursor(term_t* term) {
  term_write( term, RL_CSI "0K");
}

internal void term_clear(term_t* term, ssize_t n) {
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

internal bool term_writef(term_t* term, const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  int err = term_vwritef(term,fmt,ap);
  va_end(ap);
  return err;
}

internal bool term_vwritef(term_t* term, const char* fmt, va_list args ) {
  if (!term_buffered_ensure(term, RL_MAX_LINE)) return false;
  bool buffering = term->buffered;
  term_start_buffered(term);
  vsnprintf( term->buf + term->bufcount, to_size_t(term->buflen - term->bufcount), fmt, args );
  ssize_t written = rl_strlen(term->buf + term->bufcount);
  term->bufcount += written;
  assert(term->bufcount <= term->buflen);
  if (!buffering) term_end_buffered(term);
  return true;
}


//-------------------------------------------------------------
// Primitive
//-------------------------------------------------------------

internal void term_beep(term_t* term) {
  if (term->silent) return;
  fprintf(stderr,"\x7");
  fflush(stderr);
}

static bool term_buffered_ensure( term_t* term, ssize_t extra ) {
  // note: should work whether buffering or not.  
  ssize_t avail = term->buflen - term->bufcount;
  if (avail < extra) {
    ssize_t newlen = (term->buflen > 0 ? 2*term->buflen : 1024);
    if (newlen < term->bufcount + extra) newlen = term->bufcount + extra;
    term->buf = (char*)mem_realloc( term->mem, term->buf, newlen + 1 /* termination 0 */);
    if (term->buf == NULL) return false;
    term->buf[newlen] = 0;
    term->buf[term->bufcount] = 0;
    term->buflen = newlen;
  }
  assert(term->buflen - term->bufcount >= extra);
  return true;
}

internal bool term_write(term_t* term, const char* s) {
  // todo: strip colors on monochrome
  ssize_t n = rl_strlen(s);
  if (!term->buffered) {
    return (write(term->fout, s, to_size_t(n)) == n);
  }
  else {
    // write to buffer to reduce flicker
    if (!term_buffered_ensure(term, n)) return false;
    if (!rl_strcpy( term->buf + term->bufcount, term->buflen - term->bufcount, s)) {
      assert(false);
      return false;
    };
    term->bufcount += n;
    assert(term->buf[term->bufcount] == 0);
    return true;
  }
}


internal void term_start_buffered(term_t* term) {
  term->buffered = true;
}

internal void term_end_buffered(term_t* term) {
  if (!term->buffered) return;
  term->buffered = false;
  if (term->buf != NULL && term->bufcount > 0) {
    assert(term->buf[term->bufcount] == 0);
    //term_write(term,term->buf);
    write(term->fout, term->buf, to_size_t(term->bufcount));
    term->bufcount = 0;
    term->buf[0] = 0;
  }
}

static bool term_get_cursor_pos( term_t* term, tty_t* tty, int* row, int* col) 
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

static void term_set_cursor_pos( term_t* term, int row, int col ) {
  term_writef( term, RL_CSI "%d;%dH", row, col );
}

#ifdef _WIN32
internal bool term_update_dim(term_t* term, tty_t* tty) {
  return true;
}
#else
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
  term->width = cols;
  term->height = rows;
  return true;  
}
#endif

internal bool term_init(term_t* term, tty_t* tty, alloc_t* mem, bool monochrome, bool silent, int fout ) 
{
  term->fout = (fout < 0 ? STDOUT_FILENO : fout);
  term->monochrome = monochrome;
  term->silent = silent;
  term->width = 80;
  term->height = 25;
  term->mem = mem;
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
  term_end_buffered(term);
  mem_free(term->mem, term->buf);
  // nothing to do
}
