/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#include <string.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdarg.h>
#include <locale.h>

#include "tty.h"

#if defined(_WIN32)
#include <windows.h>
#define isatty(fd)     _isatty(fd)
#define read(fd,s,n)   _read(fd,s,n)
#define STDIN_FILENO 0
#else
#include <termios.h>
#include <unistd.h>
#include <sys/ioctl.h>
#endif

#define TTY_PUSH_MAX (64)

struct tty_s {
  int     fin;  
  bool    raw_enabled;
  bool    is_utf8;
  code_t  pushbuf[TTY_PUSH_MAX];
  ssize_t pushed;
  char    cpushbuf[TTY_PUSH_MAX];
  ssize_t cpushed;
  #if defined(_WIN32)
  HANDLE  hcon;
  DWORD   hcon_orig_mode;
  #else
  struct termios default_ios;
  struct termios raw_ios;
  #endif
  alloc_t* mem;
};


//-------------------------------------------------------------
// Forward declarations of platform dependent primitives below
//-------------------------------------------------------------

static bool tty_has_available(tty_t* tty);   // characters available?
static bool tty_readc(tty_t* tty, char* c);  // read one byte
static bool tty_code_pop( tty_t* tty, code_t* code ); 

//-------------------------------------------------------------
// Key code helpers
//-------------------------------------------------------------

internal bool code_is_char(tty_t* tty, code_t c, char* chr ) {
  if (c >= 0x20 && c <= (tty->is_utf8 ? 0x7FU : 0xFFU)) {
    if (chr != NULL) *chr = (char)c;
    return true;
  }
  else {
    if (chr != NULL) *chr = 0;
    return false;
  }
}

internal bool code_is_extended( tty_t* tty, code_t c, char* chr, int* tofollow) {
  if (tty->is_utf8 && c >= 0x80 && c <= 0xFF) {
    if (chr != NULL) *chr = (char)c;
    if (tofollow != NULL) {
      if (c <= 0xC1) *tofollow = 0;
      else if (c <= 0xDF) *tofollow = 1;
      else if (c <= 0xEF) *tofollow = 2;
      else *tofollow = 3;
    }
    return true;
  }
  else {
    if (chr != NULL) *chr = 0;
    if (tofollow != NULL) *tofollow = 0;
    return false;
  }
}

internal bool code_is_follower( tty_t* tty, code_t c, char* chr) {
  if (tty->is_utf8 && c >= 0x80 && c <= 0xBF) {
    if (chr != NULL) *chr = (char)c;
    return true;
  }
  else {
    if (chr != NULL) *chr = 0;
    return false;
  }
}

//-------------------------------------------------------------
// Read a key code
//-------------------------------------------------------------

internal bool tty_readc_noblock(tty_t* tty, char* c) {
  if (!tty_has_available(tty)) return false;  // do not modify c if nothing available (see `tty_readc_csi_num`)  
  return tty_readc(tty,c);
}

// read a single char/key
internal code_t tty_read(tty_t* tty) {
  // is there a pushed back code?
  code_t code;
  if (tty_code_pop(tty,&code)) {
    return code;
  }

  // read a single char/byte from a character stream
  char c;
  if (!tty_readc(tty, &c)) return KEY_NONE;  
  
  // Escape sequence?
  if (c == KEY_ESC) {
    code = tty_read_esc(tty);
  }
  else {
    code = KEY_CHAR(c);
  }

  code_t key  = KEY_NOMODS(code);
  code_t mods = KEY_MODS(code);
  debug_msg( "tty: readc %s%s%s 0x%03x ('%c')\n", 
              mods&MOD_SHIFT ? "shift+" : "",  mods&MOD_CTRL  ? "ctrl+" : "", mods&MOD_ALT   ? "alt+" : "",
              key, (key >= ' ' && key <= '~' ? key : ' '));

  // treat KEY_RUBOUT (0x7F) as KEY_BACKSP
  if (key == KEY_RUBOUT) {
    code = KEY_BACKSP | mods;
  }
  // treat ctrl/shift + enter always as KEY_LINEFEED for portability
  else if (key == KEY_ENTER && (mods == MOD_SHIFT || mods == MOD_ALT || mods == MOD_CTRL)) {
    code = KEY_LINEFEED;
  }
  // treat ctrl+tab always as shift+tab for portability
  else if (code == WITH_CTRL(KEY_TAB)) {
    code = KEY_SHIFT_TAB;
  }
  // treat ctrl+end/alt+>/alt-down and ctrl+home/alt+</alt-up always as pagedown/pageup for portability
  else if (code == WITH_ALT(KEY_DOWN) || code == WITH_ALT('>') || code == WITH_CTRL(KEY_END)) {
    code = KEY_PAGEDOWN;
  }
  else if (code == WITH_ALT(KEY_UP) || code == WITH_ALT('<') || code == WITH_CTRL(KEY_HOME)) {
    code = KEY_PAGEUP;
  }
  
  // treat C0 codes without MOD_CTRL
  if (key < ' ' && (mods&MOD_CTRL) != 0) {
    code &= ~MOD_CTRL; 
  }
  
  return code;
}



//-------------------------------------------------------------
// High level code pushback
//-------------------------------------------------------------

static bool tty_code_pop( tty_t* tty, code_t* code ) {
  if (tty->pushed <= 0) return false;
  tty->pushed--;
  *code = tty->pushbuf[tty->pushed];
  return true;
}

internal void tty_code_pushback( tty_t* tty, code_t c ) {
  if (tty->pushed >= TTY_PUSH_MAX) return;
  tty->pushbuf[tty->pushed] = c;
  tty->pushed++;
}


//-------------------------------------------------------------
// low-level character pushback (for escape sequences and windows)
//-------------------------------------------------------------

internal bool tty_cpop(tty_t* tty, char* c) {  
  if (tty->cpushed <= 0) {  // do not modify c on failure (see `tty_decode_unicode`)
    return false;
  }
  else {
    tty->cpushed--;
    *c = tty->cpushbuf[tty->cpushed];
    return true;
  }
}

static void tty_cpush(tty_t* tty, const char* s) {
  ssize_t len = rp_strlen(s);
  if (tty->pushed + len > TTY_PUSH_MAX) {
    assert(false);
    debug_msg("tty: cpush buffer full! (pushing %s)\n", s);
    return;
  }
  for (ssize_t i = 0; i < len; i++) {
    tty->cpushbuf[tty->cpushed + i] = s[len - i - 1];
  }
  tty->cpushed += len;
  return;
}

// convenience function for small sequences
static void tty_cpushf(tty_t* tty, const char* fmt, ...) {
  va_list args;
  va_start(args,fmt);
  char buf[128+1];
  vsnprintf(buf,128,fmt,args);
  buf[128] = 0;
  tty_cpush(tty,buf);
  va_end(args);
  return;
}

internal void tty_cpush_char(tty_t* tty, char c) {  
  char buf[2];
  buf[0] = c;
  buf[1] = 0;
  tty_cpush(tty,buf);
}


internal void tty_cpush_unicode(tty_t* tty, uint32_t c) {
  uint8_t buf[5];
  memset(buf,0,5);
  if (c <= 0x7F) {
    buf[0] = (uint8_t)c;
  }
  else if (c <= 0x07FF) {
    buf[0] = (0xC0 | ((uint8_t)(c >> 6)));
    buf[1] = (0x80 | (((uint8_t)c) & 0x3F));
  }
  else if (c <= 0xFFFF) {
    buf[0] = (0xE0 |  ((uint8_t)(c >> 12)));
    buf[1] = (0x80 | (((uint8_t)(c >>  6)) & 0x3F));
    buf[2] = (0x80 | (((uint8_t)c) & 0x3F));
  }
  else if (c <= 0x10FFFF) {
    buf[0] = (0xF0 |  ((uint8_t)(c >> 18)));
    buf[1] = (0x80 | (((uint8_t)(c >> 12)) & 0x3F));
    buf[2] = (0x80 | (((uint8_t)(c >>  6)) & 0x3F));
    buf[3] = (0x80 | (((uint8_t)c) & 0x3F));
  }
  tty_cpush(tty, (char*)buf);
}

//-------------------------------------------------------------
// Push escape codes (used on Windows)
//-------------------------------------------------------------

static unsigned csi_mods(code_t mods) {
  unsigned m = 1;
  if (mods&MOD_SHIFT) m += 1;
  if (mods&MOD_ALT)   m += 2;
  if (mods&MOD_CTRL)  m += 4;
  return m;
}

// Push ESC [ <vtcode> ; <mods> ~
static void tty_cpush_csi_vt( tty_t* tty, code_t mods, uint32_t vtcode ) {
  tty_cpushf(tty,"\x1B[%u;%u~", vtcode, csi_mods(mods) );
}

// push ESC [ 1 ; <mods> <xcmd>
static void tty_cpush_csi_xterm( tty_t* tty, code_t mods, char xcode ) {
  tty_cpushf(tty,"\x1B[1;%u%c", csi_mods(mods), xcode );
}

// push ESC [ <unicode> ; <mods> u
static void tty_cpush_csi_unicode( tty_t* tty, code_t mods, uint32_t unicode ) {
  if ((unicode < 0x80 && mods == 0) || 
      (mods == MOD_CTRL && unicode < ' ' && unicode != KEY_TAB && unicode != KEY_ENTER 
                        && unicode != KEY_LINEFEED && unicode != KEY_BACKSP) ||
      (mods == MOD_SHIFT && unicode >= ' ' && unicode <= KEY_RUBOUT)) {
    tty_cpush_char(tty,(char)unicode);
  }
  else {
    tty_cpushf(tty,"\x1B[%u;%uu", unicode, csi_mods(mods) );
  }
}

//-------------------------------------------------------------
// Init
//-------------------------------------------------------------

static bool tty_init_raw(tty_t* tty);

static bool tty_init_utf8(tty_t* tty) {
  #ifdef _WIN32
  tty->is_utf8 = true;
  #else
  char* loc = setlocale(LC_ALL,"");
  tty->is_utf8 = (loc != NULL && (strstr(loc,"UTF-8") != NULL || strstr(loc,"utf8") != NULL));
  debug_msg("tty: utf8: %s (loc=%s)\n", tty->is_utf8 ? "true" : "false", loc);
  #endif
  return true;
}

internal tty_t* tty_new(alloc_t* mem, int fin) 
{
  tty_t* tty = mem_zalloc_tp(mem, tty_t);
  tty->mem = mem;
  tty->fin = (fin < 0 ? STDIN_FILENO : fin);
  if (!(isatty(fin) && tty_init_raw(tty) && tty_init_utf8(tty))) {
    tty_free(tty);
    return NULL;
  }
  return tty;
}

internal void tty_free(tty_t* tty) {
  if (tty==NULL) return;
  tty_end_raw(tty);
  mem_free(tty->mem,tty);
}

internal bool tty_is_utf8(tty_t* tty) {
  return tty->is_utf8;
}
//-------------------------------------------------------------
// Unix
//-------------------------------------------------------------
#if !defined(_WIN32)

static bool tty_readc(tty_t* tty, char* c) {
  if (tty_cpop(tty,c)) return true;
  if (read(tty->fin, c, 1) != 1) return false;  
  return true;
}

static bool tty_has_available(tty_t* tty) {
  if (tty->cpushed > 0) return true;
  int n = 0;
  return (ioctl(0, FIONREAD, &n) == 0 && n > 0);
}

internal void tty_start_raw(tty_t* tty) {
  if (tty->raw_enabled) return;
  if (tcsetattr(tty->fin,TCSAFLUSH,&tty->raw_ios) < 0) return;
  tty->raw_enabled = true;
}

internal void tty_end_raw(tty_t* tty) {
  if (!tty->raw_enabled) return;
  tty->cpushed = 0;
  if (tcsetattr(tty->fin,TCSAFLUSH,&tty->default_ios) < 0) return;
  tty->raw_enabled = false;
}

static bool tty_init_raw(tty_t* tty) 
{  
  if (tcgetattr(tty->fin,&tty->default_ios) == -1) return false;
  tty->raw_ios = tty->default_ios; 
  tty->raw_ios.c_iflag &= ~(unsigned long)(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
  tty->raw_ios.c_oflag &= ~(unsigned long)OPOST;
  tty->raw_ios.c_cflag |= CS8;
  tty->raw_ios.c_lflag &= ~(unsigned long)(ECHO | ICANON | IEXTEN | ISIG);
  tty->raw_ios.c_cc[VTIME] = 0;
  tty->raw_ios.c_cc[VMIN] = 1;   
  return true;
}


#else

//-------------------------------------------------------------
// Windows
//-------------------------------------------------------------

static bool tty_has_available(tty_t* tty) {
  if (tty->cpushed > 0) return true;
  DWORD  count = 0;
  GetNumberOfConsoleInputEvents(tty->hcon, &count);  
  return (count > 0);
}

static void tty_waitc_console(tty_t* tty);

static bool tty_readc(tty_t* tty, char* c) {
  if (tty_cpop(tty,c)) return true;
  // The following does not work as one cannot paste unicode characters this way :-(
  //   DWORD nread;
  //   ReadConsole(tty->hcon, c, 1, &nread, NULL);
  //   if (nread != 1) return false;
  // so instead we read directly from the console input events and cpush into the tty:
  tty_waitc_console(tty); 
  return tty_cpop(tty,c);
}

// Read from the console input events and push escape codes into the tty cbuffer.
static void tty_waitc_console(tty_t* tty) 
{
  //  wait for a key down event
  INPUT_RECORD inp;
	DWORD count;
  uint32_t surrogate_hi;
  while (true) {
		if (!ReadConsoleInputW( tty->hcon, &inp, 1, &count)) return;
    if (count != 1) return;
    // wait for key down events 
    if (inp.EventType != KEY_EVENT) continue;

    // the modifier state
    DWORD modstate = inp.Event.KeyEvent.dwControlKeyState;
    
    // we need to handle shift up events separately
    if (!inp.Event.KeyEvent.bKeyDown && inp.Event.KeyEvent.wVirtualKeyCode == VK_SHIFT) {
      modstate &= ~SHIFT_PRESSED;
    }

    // ignore AltGr
    DWORD altgr = LEFT_CTRL_PRESSED | RIGHT_ALT_PRESSED;
    if ((modstate & altgr) == altgr) { modstate &= ~altgr; }

    
    // get modifiers
    code_t mods = 0;
    if ((modstate & ( RIGHT_CTRL_PRESSED | LEFT_CTRL_PRESSED )) != 0) mods |= MOD_CTRL;
    if ((modstate & ( RIGHT_ALT_PRESSED | LEFT_ALT_PRESSED )) != 0)   mods |= MOD_ALT;
    if ((modstate & SHIFT_PRESSED) != 0)                              mods |= MOD_SHIFT;

    // virtual keys
    uint32_t chr = (uint32_t)inp.Event.KeyEvent.uChar.UnicodeChar;
    WORD     virt = inp.Event.KeyEvent.wVirtualKeyCode;
    debug_msg("tty: console %s: %s%s%s virt 0x%04x, chr 0x%04x ('%c')\n", inp.Event.KeyEvent.bKeyDown ? "down" : "up", mods&MOD_CTRL ? "ctrl-" : "", mods&MOD_ALT ? "alt-" : "", mods&MOD_SHIFT ? "shift-" : "", virt, chr, chr);

    // only process keydown events (except for Alt-up which is used for unicode pasting...)
    if (!inp.Event.KeyEvent.bKeyDown && virt != VK_MENU) {
			continue;
		}
    
    if (chr == 0) { 
      switch (virt) {
        case VK_LEFT:   tty_cpush_csi_xterm(tty,mods,'D'); return; 
        case VK_RIGHT:  tty_cpush_csi_xterm(tty,mods,'C'); return;
        case VK_UP:     tty_cpush_csi_xterm(tty,mods,'A'); return; 
        case VK_DOWN:   tty_cpush_csi_xterm(tty,mods,'B'); return; 
        case VK_HOME:   tty_cpush_csi_xterm(tty,mods,'H'); return; 
        case VK_END:    tty_cpush_csi_xterm(tty,mods,'F'); return; 
        case VK_DELETE: tty_cpush_csi_vt(tty,mods,3); return; 
        case VK_PRIOR:  tty_cpush_csi_vt(tty,mods,5); return;   //page up
        case VK_NEXT:   tty_cpush_csi_vt(tty,mods,6); return;   //page down
        case VK_TAB:    tty_cpush_csi_unicode(tty,mods,9);  return; 
        case VK_RETURN: tty_cpush_csi_unicode(tty,mods,13); return;         
        default: {
          uint32_t vtcode = 0;
          if (virt >= VK_F1 && virt <= VK_F5) {
            vtcode = 10 + (virt - VK_F1);
          }
          else if (virt >= VK_F6 && virt <= VK_F10) {
            vtcode = 17 + (virt - VK_F6);
          }
          else if (virt >= VK_F11 && virt <= VK_F12) {
            vtcode = 13 + (virt - VK_F11);
          }
          if (vtcode > 0) {
            tty_cpush_csi_vt(tty,mods,vtcode);
            return;
          }
        }
      }    
      continue;  // ignore other control keys (shift etc).
    }
    // high surrogate pair
    else if (chr >= 0xD800 && chr <= 0xDBFF) {
			surrogate_hi = (chr - 0xD800);
			continue;
    }
    // low surrogate pair
    else if (chr >= 0xDC00 && chr <= 0xDFFF) {
			chr = ((surrogate_hi << 10) + (chr - 0xDC00) + 0x10000);
      tty_cpush_unicode(tty,chr);
      surrogate_hi = 0;
      return;
		}
    // regular character
    else {
      tty_cpush_csi_unicode(tty,mods,chr);
			return;
    }
  }
}  

internal void tty_start_raw(tty_t* tty) {
  if (tty->raw_enabled) return;
  GetConsoleMode(tty->hcon,&tty->hcon_orig_mode);
  DWORD mode =  ENABLE_QUICK_EDIT_MODE; // | ENABLE_VIRTUAL_TERMINAL_INPUT ; // | ENABLE_PROCESSED_INPUT ;
  SetConsoleMode(tty->hcon, mode );
  tty->raw_enabled = true;
}

internal void tty_end_raw(tty_t* tty) {
  if (!tty->raw_enabled) return;
  SetConsoleMode(tty->hcon, tty->hcon_orig_mode );
  tty->raw_enabled = false;
}

static bool tty_init_raw(tty_t* tty) {
  tty->hcon = GetStdHandle( STD_INPUT_HANDLE );  
  return true;
}

#endif


