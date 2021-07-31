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
#include <inttypes.h>

#include "common.h"
#include "tty.h"
#include "term.h"
#include "stringbuf.h" // str_next_ofs

#if defined(_WIN32)
#include <windows.h>
#define STDOUT_FILENO 1
#if !defined(ENABLE_VIRTUAL_TERMINAL_PROCESSING)
#define ENABLE_VIRTUAL_TERMINAL_PROCESSING (0)
#endif
#if !defined(ENABLE_LVB_GRID_WORLDWIDE)
#define ENABLE_LVB_GRID_WORLDWIDE (0)
#endif
#else
#include <unistd.h>
#include <errno.h>
#include <sys/ioctl.h>
#endif

#define RP_CSI      "\x1B["

// color support
typedef enum palette_e {
  MONOCHROME,
  ANSI8,
  ANSI16,
  ANSI256,
  ANSIRGB
} palette_t;

// The terminal screen
struct term_s {
  int         fd_out;             // output handle
  ssize_t     width;              // screen column width
  ssize_t     height;             // screen row height
  bool        nocolor;            // show colors?
  bool        silent;             // enable beep?
  bool        raw_enabled;        // is raw mode active?
  bool        buffered;           // are we buffering output (to reduce flicker)?
  bool        is_utf8;            // utf-8 output? determined by the tty
  palette_t   palette;            // color support
  stringbuf_t* buf;               // buffer for buffered output
  tty_t*      tty;                // used on posix to get the cursor position
  alloc_t*    mem;                // allocator
  #ifdef _WIN32
  HANDLE      hcon;               // output console handler
  WORD        hcon_default_attr;  // default text attributes
  WORD        hcon_orig_attr;     // original text attributes
  DWORD       hcon_orig_mode;     // original console mode
  DWORD       hcon_mode;          // used console mode
  UINT        hcon_orig_cp;       // original console code-page (locale)
  COORD       hcon_save_cursor;   // saved cursor position (for escape sequence emulation)
  #endif
};

static bool term_write_direct(term_t* term, const char* s, ssize_t n );
static bool term_vwritef(term_t* term, ssize_t max_needed, const char* fmt, va_list args );

//-------------------------------------------------------------
// Colors
//-------------------------------------------------------------

#include "term_color.c"

//-------------------------------------------------------------
// Helpers
//-------------------------------------------------------------

rp_private void term_left(term_t* term, ssize_t n) {
  if (n <= 0) return;
  term_writef( term, 64, RP_CSI "%zdD", n );
}

rp_private void term_right(term_t* term, ssize_t n) {
  if (n <= 0) return;
  term_writef( term, 64, RP_CSI "%zdC", n );
}

rp_private void term_up(term_t* term, ssize_t n) {
  if (n <= 0) return;
  term_writef( term, 64, RP_CSI "%zdA", n );
}

rp_private void term_down(term_t* term, ssize_t n) {
  if (n <= 0) return;
  term_writef( term, 64, RP_CSI "%zdB", n );
}

rp_private void term_clear_line(term_t* term) {
  term_write( term, "\r" RP_CSI "2K");
}

rp_private void term_start_of_line(term_t* term) {
  term_write( term, "\r" );
}

rp_private ssize_t term_get_width(term_t* term) {
  return term->width;
}

rp_private ssize_t term_get_height(term_t* term) {
  return term->height;
}

rp_private void term_attr_reset(term_t* term) {
  term_write(term, RP_CSI "0m" );
}

rp_private void term_underline(term_t* term, bool on) {
  term_write(term, on ? RP_CSI "4m" : RP_CSI "24m" );
}

rp_private void term_reverse(term_t* term, bool on) {
  term_write(term, on ? RP_CSI "7m" : RP_CSI "27m");
}

rp_private bool term_writeln(term_t* term, const char* s) {
  bool ok = term_write(term,s);
  if (ok) { ok = term_write(term,"\n"); }
  return ok;
}

rp_private bool term_write_char(term_t* term, char c) {
  char buf[2];
  buf[0] = c;
  buf[1] = 0;
  return term_write_n(term, buf, 1 );
}


//-------------------------------------------------------------
// Formatted output
//-------------------------------------------------------------

rp_private bool term_writef(term_t* term, ssize_t max_needed, const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  int err = term_vwritef(term,max_needed,fmt,ap);
  va_end(ap);
  return err;
}

static bool term_vwritef(term_t* term, ssize_t max_needed, const char* fmt, va_list args ) {
  rp_unused(max_needed);
  bool buffering = term->buffered;
  term_start_buffered(term);
  sbuf_append_vprintf(term->buf, max_needed, fmt, args);
  if (!buffering) term_end_buffered(term);
  return true;
}


//-------------------------------------------------------------
// Write to the terminal
// The buffered functions are used to reduce cursor flicker
// during refresh
//-------------------------------------------------------------

rp_private void term_beep(term_t* term) {
  if (term->silent) return;
  fprintf(stderr,"\x7");
  fflush(stderr);
}

rp_private bool term_write_repeat(term_t* term, const char* s, ssize_t count) {
  for (; count > 0; count--) {
    if (!term_write(term, s)) return false;
  }
  return true;
}

rp_private bool term_write(term_t* term, const char* s) {
  if (s == NULL || s[0] == 0) return true;
  ssize_t n = rp_strlen(s);
  return term_write_n(term,s,n);
}

rp_private bool term_write_n(term_t* term, const char* s, ssize_t n) {
  if (s == NULL || n <= 0) return true;
  if (!term->buffered) {
    return term_write_direct(term,s,n);
  }
  else {
    // write to buffer to reduce flicker
    sbuf_append_n(term->buf, s, n);
    return true;
  }
}

rp_private void term_start_buffered(term_t* term) {
  if (term->buf == NULL) {
    term->buf = sbuf_new(term->mem);
    if (term->buf == NULL) {
      term->buffered = false;
      return;
    }
  }
  term->buffered = true;
}

rp_private bool term_end_buffered(term_t* term) {
  if (!term->buffered) return true;
  term->buffered = false;
  if (term->buf != NULL && sbuf_len(term->buf) > 0) {
    bool ok = term_write_direct(term, sbuf_string(term->buf), sbuf_len(term->buf));
    sbuf_clear(term->buf);
    if (!ok) return false;
  }
  return true;
}


//-------------------------------------------------------------
// Init
//-------------------------------------------------------------

static void term_init_raw(term_t* term);

rp_private term_t* term_new(alloc_t* mem, tty_t* tty, bool nocolor, bool silent, int fd_out ) 
{
  term_t* term = mem_zalloc_tp(mem, term_t);
  if (term == NULL) return NULL;

  term->fd_out  = (fd_out < 0 ? STDOUT_FILENO : fd_out);
  term->nocolor = nocolor;
  term->silent  = silent;  
  term->mem     = mem;
  term->tty     = tty;
  term->width   = 80;
  term->height  = 25;
  term->is_utf8 = tty_is_utf8(tty);
  term->palette = ANSI16; // almost universally supported

  // respect NO_COLOR
  if (getenv("NO_COLOR") != NULL) {
    term->nocolor = true;
  }

  // detect color palette
  // COLORTERM takes precedence
  const char* colorterm = getenv("COLORTERM");  
  if (rp_contains(colorterm,"24bit") || rp_contains(colorterm,"truecolor"))      { term->palette = ANSIRGB; }
  else if (rp_contains(colorterm,"8bit") || rp_contains(colorterm,"256color"))   { term->palette = ANSI256; } 
  else if (rp_contains(colorterm,"4bit") || rp_contains(colorterm,"16color"))    { term->palette = ANSI16; }
  else if (rp_contains(colorterm,"3bit") || rp_contains(colorterm,"8color"))     { term->palette = ANSI8; }
  else if (rp_contains(colorterm,"1bit") || rp_contains(colorterm,"monochrome")) { term->palette = MONOCHROME; }
  else if (getenv("WT_SESSION") != NULL) { term->palette = ANSIRGB; } // Windows terminal
  else {
    // fall back to checking TERM
    const char* eterm = getenv("TERM");
    if (rp_contains(eterm,"xterm") || rp_contains(eterm,"256color") || 
        rp_contains(eterm,"gnome") || rp_contains(eterm,"kitty")) 
    { 
      term->palette = ANSI256; 
    }  
    else if (rp_contains(eterm,"16color")){ term->palette = ANSI16; }
    else if (rp_contains(eterm,"8color")) { term->palette = ANSI8; }
    else if (rp_contains(eterm,"dumb"))   { term->palette = MONOCHROME; }
  }
  debug_msg("term; palette: %d (COLORTERM=%s, TERM=%s)\n", term->palette, colorterm, term);
  
  // read COLUMS/LINES from the environment for a better initial guess.
  const char* env_columns = getenv("COLUMNS");
  if (env_columns != NULL) { rp_atoz(env_columns, &term->width); }
  const char* env_lines = getenv("LINES");
  if (env_lines != NULL)   { rp_atoz(env_lines, &term->height); }
  
  // initialize raw terminal output and terminal dimensions
  term_init_raw(term);
  term_update_dim(term);

  return term;
}

rp_private bool term_is_interactive(const term_t* term) {
  rp_unused(term);
  // check dimensions (0 is used for debuggers)
  // if (term->width <= 0) return false; 
  
  // check editing support
  const char* eterm = getenv("TERM");
  debug_msg("term: TERM=%s\n", eterm);
  if (eterm != NULL &&
      (strstr("dumb|DUMB|cons25|CONS25|emacs|EMACS",eterm) != NULL)) {
    return false;
  }

  return true;
}

rp_private bool term_enable_beep(term_t* term, bool enable) {
  bool prev = term->silent;
  term->silent = !enable;
  return prev;
}

rp_private bool term_enable_color(term_t* term, bool enable) {
  bool prev = !term->nocolor;
  term->nocolor = !enable;
  return prev;
}


rp_private void term_free(term_t* term) {
  if (term == NULL) return;
  term_end_buffered(term);  
  term_end_raw(term);
  sbuf_free(term->buf); term->buf = NULL;
  mem_free(term->mem, term);
}


//-------------------------------------------------------------
// Platform dependent: Write directly to the terminal
//-------------------------------------------------------------

#if !defined(_WIN32)
static bool term_write_console(term_t* term, const char* s, ssize_t n) {
  ssize_t count = 0; 
  while( count < n ) {
    ssize_t nwritten = write(term->fd_out, s + count, to_size_t(n - count));
    if (nwritten > 0) {
      count += nwritten;
    }
    else if (errno != EINTR && errno != EAGAIN) {
      debug_msg("term: write failed: length %i, errno %i: \"%s\"\n", n, errno, s);
      return false;
    }
  }
  return true;
}

static bool term_write_esc(term_t* term, const char* s, ssize_t len) {
  if (term->nocolor && s[1]=='[' && s[len-1] == 'm') {
    ssize_t n = 1;
    rp_atoz(s + 2, &n);
    if ((n >= 30 && n <= 49) || (n >= 90 && n <= 109)) {
      // ignore color
      return true;
    }
  }  
  return term_write_console(term, s, len);
}

static bool term_write_utf8(term_t* term, const char* s, ssize_t len) {
  ssize_t nread;
  unicode_t uchr = unicode_from_qutf8((const uint8_t*)s, len, &nread);
  uint8_t c;
  if (unicode_is_raw(uchr,&c)) {
    // write bytes as is; this also ensure that on non-utf8 terminals characters between 0x80-0xFF
    // go through _as is_ due to the qutf8 encoding.
    char buf[2];
    buf[0] = (char)c;
    buf[1] = 0;
    return term_write_console(term, buf, 1);
  }
  else if (!term->is_utf8) {
    // on non-utf8 terminals send unicode escape sequences and hope for the best
    // todo: we could try to convert to the locale first?
    char buf[64+1];
    snprintf(buf, 64, "\x1B[%" PRIu32 "u", uchr);
    buf[64] = 0;
    return term_write_console(term, buf, rp_strlen(buf));
  }
  else {
    // write utf-8 as is
    return term_write_console(term, s, len);
  }
}

static bool term_write_direct(term_t* term, const char* s, ssize_t len ) {
  if (!term->nocolor) {
    return term_write_console(term, s, len);
  }
  else {
    // strip CSI color sequences
    ssize_t pos = 0;
    while (pos < len) {
      // handle ascii sequences in bulk
      ssize_t ascii = 0;
      ssize_t next;
      while ((next = str_next_ofs(s, len, pos+ascii, NULL)) > 0 && 
              s[pos + ascii] != '\x1B' && (uint8_t)s[pos + ascii] <= 0x7F ) 
      {
        ascii += next;
      }
      if (ascii > 0) {
        term_write_console(term, s+pos, ascii);
        pos += ascii;
      }
      if (next <= 0) break;

      // handle utf8 sequences (for non-utf8 terminals)
      if ((uint8_t)s[pos] >= 0x80) {
        term_write_utf8(term, s+pos, next);
      }
      // handle escape sequence (note: str_next_ofs considers whole CSI escape sequences at a time)
      else if (next > 1 && s[pos] == '\x1B') {
        term_write_esc(term, s+pos, next);
      }
      else {
        term_write_console(term, s+pos, next);
      }
      pos += next;
    }
    return (pos == len);
  }
}

#else

//----------------------------------------------------------------------------------
// On windows we do ansi escape emulation ourselves.
// (for compat pre-win10 systems)
//
// note: we use row/col as 1-based ANSI escape while windows X/Y coords are 0-based.
//-----------------------------------------------------------------------------------

static bool term_write_console(term_t* term, const char* s, ssize_t n ) {
  DWORD written;
  WriteConsoleA(term->hcon, s, (DWORD)(to_size_t(n)), &written, NULL);
  return (written == (DWORD)(to_size_t(n)));
}

static bool term_get_cursor_pos( term_t* term, ssize_t* row, ssize_t* col) {
  *row = 0;
  *col = 0;
  CONSOLE_SCREEN_BUFFER_INFO info;
  if (!GetConsoleScreenBufferInfo(term->hcon, &info)) return false;
  *row = (ssize_t)info.dwCursorPosition.Y + 1;
  *col = (ssize_t)info.dwCursorPosition.X + 1;
  return true;
}

static void term_move_cursor_to( term_t* term, ssize_t row, ssize_t col ) {
  CONSOLE_SCREEN_BUFFER_INFO info;
  if (!GetConsoleScreenBufferInfo( term->hcon, &info )) return;
  if (col > info.dwSize.X) col = info.dwSize.X;
  if (row > info.dwSize.Y) row = info.dwSize.Y;
  if (col <= 0) col = 1;
  if (row <= 0) row = 1;
  COORD coord;
  coord.X = (SHORT)col - 1;
  coord.Y = (SHORT)row - 1;
  SetConsoleCursorPosition( term->hcon, coord);
}

static void term_cursor_save(term_t* term) {
  memset(&term->hcon_save_cursor, 0, sizeof(term->hcon_save_cursor));
  CONSOLE_SCREEN_BUFFER_INFO info;
  if (!GetConsoleScreenBufferInfo(term->hcon, &info)) return;
  term->hcon_save_cursor = info.dwCursorPosition;
}

static void term_cursor_restore(term_t* term) {
  if (term->hcon_save_cursor.X == 0) return;
  SetConsoleCursorPosition(term->hcon, term->hcon_save_cursor);
}

static void term_move_cursor( term_t* term, ssize_t drow, ssize_t dcol, ssize_t n ) {
  CONSOLE_SCREEN_BUFFER_INFO info;
  if (!GetConsoleScreenBufferInfo( term->hcon, &info )) return;
  COORD cur = info.dwCursorPosition;
  ssize_t col = (ssize_t)cur.X + 1 + n*dcol;
  ssize_t row = (ssize_t)cur.Y + 1 + n*drow;
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
  ssize_t length;
  if (mode == 2) {
    // to end of line    
    length = (ssize_t)info.srWindow.Right - info.dwCursorPosition.X + 1;
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
    length = (ssize_t)info.srWindow.Right + 1;
  }
  FillConsoleOutputAttribute( term->hcon, 0, (DWORD)length, start, &written );
  FillConsoleOutputCharacterA( term->hcon, ' ', (DWORD)length, start, &written );
}

static void term_clear_screen(term_t* term, ssize_t mode) {
  CONSOLE_SCREEN_BUFFER_INFO info;
  if (!GetConsoleScreenBufferInfo(term->hcon, &info)) return;
  COORD start;
  start.X = 0;
  start.Y = 0;
  ssize_t length;
  ssize_t width = (ssize_t)info.dwSize.X;
  if (mode == 2) {
    // entire screen
    length = width * info.dwSize.Y;    
  }
  else if (mode == 1) {
    // to cursor
    length = (width * ((ssize_t)info.dwCursorPosition.Y - 1)) + info.dwCursorPosition.X;
  }
  else {
    // from cursor
    start  = info.dwCursorPosition;
    length = (width * ((ssize_t)info.dwSize.Y - info.dwCursorPosition.Y)) + (width - info.dwCursorPosition.X + 1);
  }
  DWORD written;
  FillConsoleOutputAttribute(term->hcon,   0, (DWORD)length, start, &written);
  FillConsoleOutputCharacterA(term->hcon, ' ', (DWORD)length, start, &written);
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
  else if (cmd == 4) {  // underline
    attr |= COMMON_LVB_UNDERSCORE;
  }
  else if (cmd == 24) {  // not underline
    attr &= ~COMMON_LVB_UNDERSCORE;
  }
  else if (cmd == 7) {  // reverse
    attr |= COMMON_LVB_REVERSE_VIDEO;
  }
  else if (cmd == 27) {  // not reverse
    attr &= ~COMMON_LVB_REVERSE_VIDEO;
  }
  else if (!term->nocolor) {
    if (cmd >= 30 && cmd <= 37) {  // fore ground
      attr = (attr & ~0x0F) | attr_color[cmd - 30];
    }
    else if (cmd >= 90 && cmd <= 97) {  // fore ground bright
      attr = (attr & ~0x0F) | attr_color[cmd - 90] | FOREGROUND_INTENSITY;
    }
    else if (cmd >= 40 && cmd <= 47) {  // back ground
      attr = (attr & ~0xF0) | (WORD)(attr_color[cmd - 40] << 4);
    }
    else if (cmd >= 100 && cmd <= 107) {  // back ground bright
      attr = (attr & ~0xF0u) | (WORD)(attr_color[cmd - 100] << 4) | BACKGROUND_INTENSITY;
    }
    else if (cmd == 39) {  // default fore ground
      attr = (attr & ~0x0F) | (def_attr & 0x0F);
    }
    else if (cmd == 49) {  // default back ground
      attr = (attr & ~0xF0) | (def_attr & 0xF0);
    }
  }
  if (attr != cur_attr) {
    SetConsoleTextAttribute(term->hcon, attr);
  }
}

static ssize_t esc_param( const char* s, ssize_t len, ssize_t def ) {
  rp_unused(len);
  if (*s == '?') s++;
  ssize_t n = def;
  rp_atoz(s, &n);
  return n;
}

static void esc_param2( const char* s, ssize_t len, ssize_t* p1, ssize_t* p2, ssize_t def ) {
  rp_unused(len);
  if (*s == '?') s++; 
  *p1 = def;
  *p2 = def;
  rp_atoz2(s, p1, p2);  
}

static void term_write_esc( term_t* term, const char* s, ssize_t len ) {
  ssize_t row;
  ssize_t col;

  // ignore color?
  if (term->nocolor && s[1] == '[' && s[len-1] == 'm') {
    ssize_t code = esc_param(s+2, len, 0);
    if ((code >= 30 && code <= 49) || (code >= 90 && code <= 107)) {
      return;  
    }
  }
  
  // use the builtin terminal processing? (enables truecolor for example)
  if ((term->hcon_mode & ENABLE_VIRTUAL_TERMINAL_PROCESSING) != 0) {
    term_write_console(term, s, len);
    return;
  }

  // otherwise emulate ourselves
  if (s[1] == '[') {
    switch (s[len-1]) {
    case 'A':
      term_move_cursor(term, -1, 0, esc_param(s+2, len, 1));
      break;
    case 'B':
      term_move_cursor(term, 1, 0, esc_param(s+2, len, 1));
      break;
    case 'C':
      term_move_cursor(term, 0, 1, esc_param(s+2, len, 1));
      break;
    case 'D':
      term_move_cursor(term, 0, -1, esc_param(s+2, len, 1));
      break;
    case 'H': 
      esc_param2(s+2, len, &row, &col, 1);
      term_move_cursor_to(term, row, col);
      break;
    case 'K':
      term_erase_line(term, esc_param(s+2, len, 0));
      break;
    case 'm':
      term_esc_attr(term, esc_param(s+2, len, 0));
      break;

    // support some less standard escape codes (currently not used by repline)
    case 'E':  // line down
      term_get_cursor_pos(term, &row, &col);
      row += esc_param(s+2, len, 1);
      term_move_cursor_to(term, row, 1);
      break;
    case 'F':  // line up
      term_get_cursor_pos(term, &row, &col);
      row -= esc_param(s+2, len, 1);
      term_move_cursor_to(term, row, 1);
      break;
    case 'G':  // absolute column
      term_get_cursor_pos(term, &row, &col);
      col = esc_param(s+2, len, 1);
      term_move_cursor_to(term, row, col);
      break;
    case 'J': 
      term_clear_screen(term, esc_param(s+2, len, 0));
      break;
    case 'h':
      if (strncmp(s+2, "?25h", 4) == 0) {
        term_cursor_visible(term, true);
      }
      break;
    case 'l': 
      if (strncmp(s+2, "?25l", 4) == 0) {
        term_cursor_visible(term, false);
      }
      break;
    case 's': 
      term_cursor_save(term);
      break;    
    case 'u':
      term_cursor_restore(term);
      break;
    // otherwise ignore
    }
  }
  else if (s[1] == '7') {
    term_cursor_save(term);
  }
  else if (s[1] == '8') {
    term_cursor_restore(term);
  }
  else {
    // otherwise ignore
  }
}

static bool term_write_direct(term_t* term, const char* s, ssize_t len ) {
  term_cursor_visible(term,false); // reduce flicker
  ssize_t pos = 0;
  while( pos < len ) {
    // handle non-control in bulk (including utf-8 sequences)
    // (We don't need to handle utf-8 separately as we set the codepage to always be in utf-8 mode)
    ssize_t nonctrl = 0;
    ssize_t next;
    while( (next = str_next_ofs( s, len, pos+nonctrl, NULL )) > 0 && (uint8_t)s[pos + nonctrl] >= ' ') {
      nonctrl += next;
    }
    if (nonctrl > 0) {
      term_write_console(term, s+pos, nonctrl);
      pos += nonctrl;
    }    
    if (next <= 0) break;

    // handle control (note: str_next_ofs considers whole CSI escape sequences at a time)
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

#if !defined(_WIN32)

// On non-windows, the terminal is set in raw mode by the tty.

rp_private void term_start_raw(term_t* term) {
  if (term->raw_enabled) return; 
  //term_write(term,"\x1B[?7l");
  term->raw_enabled = true;    
}

rp_private void term_end_raw(term_t* term) {
  if (!term->raw_enabled) return;
  //term_write(term,"\x1B[?7h");
  term->raw_enabled = false;
}

static void term_init_raw(term_t* term) {
  rp_unused(term);
}

#else

rp_private void term_start_raw(term_t* term) {
  if (term->raw_enabled) return;
  CONSOLE_SCREEN_BUFFER_INFO info;
  if (GetConsoleScreenBufferInfo( term->hcon, &info )) {
    term->hcon_orig_attr = info.wAttributes;
  }
	GetConsoleMode( term->hcon, &term->hcon_orig_mode );
  term->hcon_orig_cp = GetConsoleOutputCP(); 
  SetConsoleOutputCP(CP_UTF8);
  if (term->hcon_mode == 0) {
    // first time initialization
    DWORD mode = ENABLE_PROCESSED_OUTPUT | ENABLE_LVB_GRID_WORLDWIDE;   // for \r \n and \b    
    // use escape sequence handling if available (so we can use rgb colors in Windows terminal)
    if (SetConsoleMode(term->hcon, mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING)) {
      term->hcon_mode = mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING;
    }
    // no terminal processing fallback
    else if (SetConsoleMode(term->hcon, mode)) {
      term->hcon_mode = mode;
      if (term->palette > ANSI16) { term->palette = ANSI16; }
    }    
  }
  else {
    SetConsoleMode(term->hcon, term->hcon_mode );
  }
  term->raw_enabled = true;  
}

rp_private void term_end_raw(term_t* term) {
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
  term_start_raw(term); // initialize the hcon_mode
  term_end_raw(term);
}

#endif

//-------------------------------------------------------------
// Update terminal dimensions
//-------------------------------------------------------------

#if !defined(_WIN32)

static bool term_raw_get_cursor_pos( term_t* term, ssize_t* row, ssize_t* col) 
{
  if (!term_write_console(term, "\x1B[6n", 4)) return false;
  debug_msg("term: read tty esponse\n");

  // parse query response ESC[%d;%dR
  char buf[64];
  ssize_t len = 0;
  uint8_t c = 0;
  if (!tty_readc_pause_noblock(term->tty,&c) || c != '\x1B') return false;
  if (!tty_readc_noblock(term->tty,&c) || c != '[')    return false;
  while( len < 63 ) {
    if (!tty_readc_noblock(term->tty,&c)) return false;
    if (!((c >= '0' && c <= '9') || (c == ';'))) break;
    buf[len++] = (char)c; 
  }
  buf[len] = 0;
  debug_msg("term: parse cursor response: %s\n", buf);
  return rp_atoz2(buf,row,col);
}

static bool term_get_cursor_pos( term_t* term, ssize_t* row, ssize_t* col) 
{
  // send escape query
  if (!tty_start_raw(term->tty)) return false;
  bool ok = term_raw_get_cursor_pos(term,row,col);  
  tty_end_raw(term->tty);
  return ok;
}

static void term_set_cursor_pos( term_t* term, ssize_t row, ssize_t col ) {
  term_writef( term, 128, RP_CSI "%zd;%zdH", row, col );
}

rp_private bool term_update_dim(term_t* term) {  
  ssize_t cols = 0;
  ssize_t rows = 0;
  struct winsize ws;
  if (ioctl(1, TIOCGWINSZ, &ws) >= 0) {
    // ioctl succeeded
    cols = ws.ws_col;  // debuggers return 0 for the column
    rows = ws.ws_row;
  }
  else {
    // determine width by querying the cursor position
    debug_msg("term: ioctl term-size failed: %d,%d\n", ws.ws_row, ws.ws_col);
    ssize_t col0 = 0;
    ssize_t row0 = 0;
    if (term_get_cursor_pos(term,&row0,&col0)) {
      term_set_cursor_pos(term,999,999);
      ssize_t col1 = 0;
      ssize_t row1 = 0;
      if (term_get_cursor_pos(term,&row1,&col1)) {
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
  bool changed = (term->width != cols || term->height != rows);
  debug_msg("terminal dim: %zd,%zd: %s\n", rows, cols, changed ? "changed" : "unchanged");  
  if (cols > 0) { 
    term->width = cols;
    term->height = rows;
  }
  return changed;  
}

#else

rp_private bool term_update_dim(term_t* term) {
  if (term->hcon == 0) {
    term->hcon = GetConsoleWindow();
  }
  ssize_t rows = 0;
  ssize_t cols = 0;  
  CONSOLE_SCREEN_BUFFER_INFO sbinfo;  
  if (GetConsoleScreenBufferInfo(term->hcon, &sbinfo)) {
     cols = (ssize_t)sbinfo.srWindow.Right - (ssize_t)sbinfo.srWindow.Left + 1;
     rows = (ssize_t)sbinfo.srWindow.Bottom - (ssize_t)sbinfo.srWindow.Top + 1;
  }
  bool changed = (term->width != cols || term->height != rows);
  term->width = cols;
  term->height = rows;
  debug_msg("term: update dim: %zd, %zd\n", term->height, term->width );
  return changed;
}

#endif
