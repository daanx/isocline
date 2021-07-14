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
#include <windows.h>
#define STDOUT_FILENO 1
#else
#include <unistd.h>
#include <sys/ioctl.h>
#endif

#define RL_CSI      "\x1B["


static bool term_write_direct(term_t* term, const char* s, ssize_t n );
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
    return term_write_direct(term,s,n);
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

internal bool term_end_buffered(term_t* term) {
  if (!term->buffered) return true;
  term->buffered = false;
  if (term->buf != NULL && term->bufcount > 0) {
    assert(term->buf[term->bufcount] == 0);
    bool ok = term_write_direct(term, term->buf, term->bufcount);
    term->bufcount = 0;
    term->buf[0] = 0;
    if (!ok) return false;
  }
  return true;
}

static void term_init_raw(term_t* term);

internal bool term_init(term_t* term, tty_t* tty, alloc_t* mem, bool monochrome, bool silent, int fout ) 
{
  term->fout = (fout < 0 ? STDOUT_FILENO : fout);
  term->monochrome = monochrome;
  term->silent = silent;
  term->width = 80;
  term->height = 25;
  term->mem = mem;
  term_init_raw(term);
  term_update_dim(term,tty);

  // check dimensions
  if (term->width <= 0) return false; 
  
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
  term_end_raw(term);

  mem_free(term->mem, term->buf);
  // nothing to do
}


//-------------------------------------------------------------
// Write to terminal
//-------------------------------------------------------------

#if !defined(_WIN32)

static bool term_write_direct(term_t* term, const char* s, ssize_t n ) {
  return (write(term->fout, s, to_size_t(n)) == n);
}

#else

//-------------------------------------------------------------
// On windows we do ansi escape emulation ourselves.
// (for compat pre-win10 systems)
//-------------------------------------------------------------

static bool term_write_console(term_t* term, const char* s, ssize_t n ) {
  DWORD written;
  WriteConsoleA(term->hcon, s, (DWORD)(to_size_t(n)), &written, NULL);
  return (written == (DWORD)(to_size_t(n)));
}

static void term_move_cursor_to( term_t* term, ssize_t row, ssize_t col ) {
  CONSOLE_SCREEN_BUFFER_INFO info;
  if (!GetConsoleScreenBufferInfo( term->hcon, &info )) return;
  if (col >= info.dwSize.X) col = info.dwSize.X - 1;
  if (row >= info.dwSize.Y) row = info.dwSize.Y - 1;
  COORD coord;
  coord.X = (SHORT)col;
  coord.Y = (SHORT)row;
  SetConsoleCursorPosition( term->hcon, coord);
}

static void term_move_cursor( term_t* term, ssize_t drow, ssize_t dcol, ssize_t n ) {
  CONSOLE_SCREEN_BUFFER_INFO info;
  if (!GetConsoleScreenBufferInfo( term->hcon, &info )) return;
  COORD cur = info.dwCursorPosition;
  ssize_t col = cur.X + n*dcol;
  ssize_t row = cur.Y + n*drow;
  term_move_cursor_to( term, row, col );
}

static void term_cursor_visible( term_t* term, bool visible ) {
  CONSOLE_CURSOR_INFO info;
  if (!GetConsoleCursorInfo(term->hcon,&info)) return;
  info.bVisible = visible;
  SetConsoleCursorInfo(term->hcon,&info);
}

static void term_erase_line( term_t* term, ssize_t mode ) {  
  CONSOLE_SCREEN_BUFFER_INFO info;
  if (!GetConsoleScreenBufferInfo( term->hcon, &info )) return;
  DWORD written;
  COORD start;
  DWORD length;
  if (mode == 2) {
    // to end of line    
    length = info.srWindow.Right - info.dwCursorPosition.X;
    start  = info.dwCursorPosition;
  }
  else if (mode == 1) {
    // to start of line
    start.X = 0;
    start.Y = info.dwCursorPosition.Y;
    length  = info.dwCursorPosition.X;
  }
  else {
    // entire line
    start.X = 0;
    start.Y = info.dwCursorPosition.Y;
    length = info.srWindow.Right + 1;
  }
  FillConsoleOutputAttribute( term->hcon, 0, length, start, &written );
  FillConsoleOutputCharacter( term->hcon, ' ', length, start, &written );
}

static WORD attr_color[8] = {
  0,                                  // black
  FOREGROUND_RED,                     // maroon
  FOREGROUND_GREEN,                   // green
  FOREGROUND_RED | FOREGROUND_GREEN,  // orange
  FOREGROUND_BLUE,                    // navy
  FOREGROUND_RED | FOREGROUND_BLUE,   // purple
  FOREGROUND_GREEN | FOREGROUND_BLUE, // teal
  FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE, // light gray
};

static void term_esc_attr( term_t* term, ssize_t cmd ) {
  WORD def_attr = term->hcon_default_attr;
  CONSOLE_SCREEN_BUFFER_INFO info;
  if (!GetConsoleScreenBufferInfo( term->hcon, &info )) return;  
  WORD cur_attr = info.wAttributes;
  WORD attr = cur_attr; 
  if (cmd==0) {
    attr = def_attr;
  }
  else if (cmd >= 30 && cmd <= 37) {  // fore ground
    attr = (attr & ~0x0F) | attr_color[cmd - 30];
  }
  else if (cmd >= 90 && cmd <= 97) {  // fore ground bright
    attr = (attr & ~0x0F) | attr_color[cmd - 90] | FOREGROUND_INTENSITY;
  }
  else if (cmd >= 40 && cmd <= 47) {  // back ground
    attr = (attr & ~0xF0) | (attr_color[cmd - 40] << 4);
  }
  else if (cmd >= 90 && cmd <= 97) {  // back ground bright
    attr = (attr & ~0xF0) | (attr_color[cmd - 90] << 4) | BACKGROUND_INTENSITY;
  }
  else if (cmd == 39) {  // default fore ground
    attr = (attr & ~0x0F) | (def_attr & 0x0F);
  }
  else if (cmd == 39) {  // default back ground
    attr = (attr & ~0xF0) | (def_attr & 0xF0);
  }
  else if (cmd == 4) {  // underline
    attr |= COMMON_LVB_UNDERSCORE;
  }
  else if (cmd == 24) {  // not underline
    attr &= ~COMMON_LVB_UNDERSCORE;
  }
  else if (cmd == 7) {  // reverse
    attr |= COMMON_LVB_REVERSE_VIDEO;
  }
  else if (cmd == 24) {  // not reverse
    attr &= ~COMMON_LVB_REVERSE_VIDEO;
  }
  else {
    return; // ignore
  }
  if (attr != cur_attr) SetConsoleTextAttribute( term->hcon, attr );
}

static ssize_t esc_param( const char* s, ssize_t len, ssize_t def ) {
  ssize_t n = def;
  sscanf(s, "%zd", &n);
  return n;
}

static void esc_param2( const char* s, ssize_t len, ssize_t* p1, ssize_t* p2, ssize_t def ) {
  *p1 = def;
  *p2 = def;
  sscanf(s, "%zd;%zd", p1, p2);  
}

static void term_write_esc( term_t* term, const char* s, ssize_t len ) {
  switch( s[len-1] ) {
    case 'A': 
      term_move_cursor( term, -1, 0, esc_param( s+2, len, 1 ) ); 
      break;
    case 'B': 
      term_move_cursor( term, 1, 0, esc_param( s+2, len, 1 ) ); 
      break;
    case 'C': 
      term_move_cursor( term, 0, 1, esc_param( s+2, len, 1 ) ); 
      break;
    case 'D': 
      term_move_cursor( term, 0, -1, esc_param( s+2, len, 1 ) ); 
      break;
    case 'H': 
      ssize_t row;
      ssize_t col;
      esc_param2( s+2, len, &row, &col, 1 );
      term_move_cursor_to( term, row, col ); 
      break;
    case 'K':
      term_erase_line( term, esc_param( s+2, len, 0 ) );
      break;
    case 'm':
      term_esc_attr( term, esc_param( s+2, len, 0 ) );
      break;
  }
  // otherwise ignore
}

static bool term_write_direct(term_t* term, const char* s, ssize_t len ) {
  term_cursor_visible(term,false); // reduce flicker
  ssize_t pos = 0;
  while( pos < len ) {
    // handle non-control in bulk
    ssize_t nonctrl = 0;
    ssize_t next;
    while( (next = skip_next_code( s, len, pos+nonctrl, true )) > 0 && (uint8_t)s[pos + nonctrl] >= ' ') {
      nonctrl += next;
    }
    if (nonctrl > 0) {
      term_write_console(term, s+pos, nonctrl);
      pos += nonctrl;
    }    
    if (next <= 0) break;

    // handle control
    if (next > 1 && s[pos] == '\x1B') {
      term_write_esc(term, s+pos, next);
    }
    else {
      term_write_console( term, s+pos, next);
    }
    pos += next;
  }
  term_cursor_visible(term,true);
  assert(pos == len);
  return (pos == len); 
}

#endif


//-------------------------------------------------------------
// Enable/disable terminal raw mode
//-------------------------------------------------------------

#if defined(_WIN32)
internal void term_start_raw(term_t* term) {
  if (term->raw_enabled) return;
  CONSOLE_SCREEN_BUFFER_INFO info;
  if (GetConsoleScreenBufferInfo( term->hcon, &info )) {
    term->hcon_orig_attr = info.wAttributes;
  }
	GetConsoleMode( term->hcon, &term->hcon_orig_mode );
  term->hcon_orig_cp = GetConsoleOutputCP(); 
  SetConsoleOutputCP(65001);  
  SetConsoleMode( term->hcon,  ENABLE_PROCESSED_OUTPUT   // for \r \n and \b
                             | ENABLE_LVB_GRID_WORLDWIDE // for underline
                             // | ENABLE_VIRTUAL_TERMINAL_PROCESSING // we already emulate ourselves 
                             );
  term->raw_enabled = true;  
}

internal void term_end_raw(term_t* term) {
  if (!term->raw_enabled) return;
  SetConsoleMode( term->hcon, term->hcon_orig_mode );
  SetConsoleOutputCP(term->hcon_orig_cp);
  SetConsoleTextAttribute(term->hcon, term->hcon_orig_attr);
  term->raw_enabled = false;
}

static void term_init_raw(term_t* term) {
  term->hcon = GetStdHandle( STD_OUTPUT_HANDLE );
  CONSOLE_SCREEN_BUFFER_INFO info;
  if (GetConsoleScreenBufferInfo( term->hcon, &info )) {
    term->hcon_default_attr = info.wAttributes;
  }
}

#else

internal void term_start_raw(term_t* term) {
  if (term->raw_enabled) return; 
  term->raw_enabled = true;  
}

internal void term_end_raw(term_t* term) {
  if (!term->raw_enabled) return;
  term->raw_enabled = false;
}

static void term_init_raw(term_t* term) {
  unused(term);
}

#endif

//-------------------------------------------------------------
// Update terminal dimensions
//-------------------------------------------------------------

#ifdef _WIN32

internal bool term_update_dim(term_t* term, tty_t* tty) {
  if (term->hcon == 0) {
    term->hcon = GetConsoleWindow();
  }
  ssize_t rows = 0;
  ssize_t cols = 0;  
  CONSOLE_SCREEN_BUFFER_INFO sbinfo;  
  if (GetConsoleScreenBufferInfo(term->hcon, &sbinfo)) {
     cols = sbinfo.srWindow.Right - sbinfo.srWindow.Left + 1;
     rows = sbinfo.srWindow.Bottom - sbinfo.srWindow.Top + 1;
  }
  bool changed = (term->width != cols || term->height != rows);
  term->width = cols;
  term->height = rows;
  debug_msg("term: update dim: %zd, %zd\n", term->height, term->width );
  return changed;
}

#else

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
    if (!((c >= '0' && c <= '9') || (c == ';'))) break;
    buf[len] = c;
    len++;    
  }
  buf[len] = 0;
  return (sscanf(buf,"%d;%d",row,col) == 2);
}

static void term_set_cursor_pos( term_t* term, int row, int col ) {
  term_writef( term, RL_CSI "%d;%dH", row, col );
}

internal bool term_update_dim(term_t* term, tty_t* tty) {
  int cols = 0;
  int rows = 0;
  struct winsize ws;
  if (ioctl(1, TIOCGWINSZ, &ws) >= 0) {
    // ioctl succeeded
    cols = ws.ws_col;  // debuggers return 0 for the column
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
      // return 0 column
    }
  }

  // update width and return if it changed.
  debug_msg("terminal dim: %d,%d\n", rows, cols);  
  bool changed = (term->width != cols || term->height != rows);
  term->width = cols;
  term->height = rows;
  return changed;  
}
#endif
