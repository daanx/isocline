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
#include <unistd.h>
#include <sys/ioctl.h>
#endif



internal bool code_is_char(tty_t* tty, code_t c, char* chr ) {
  if (c >= 0x20 && c <= (tty->is_utf8 ? 0x7F : 0xFF)) {
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

internal bool code_is_key( tty_t* tty, code_t c ) {
  unused(tty);
  return (c <= KEY_CTRL('Z') || c >= KEY_UP);
}

//-------------------------------------------------------------
// Read a code point
//-------------------------------------------------------------

static bool tty_has_available(tty_t* tty);
static bool tty_readc(tty_t* tty, char* c);
static void tty_cpush_char(tty_t* tty, char c);

// read ANSI key escape sequences
static code_t tty_read_esc(tty_t* tty) {
  // <https://en.wikipedia.org/wiki/ANSI_escape_code#Control_characters>
  char c1 = 0;
  char c2 = 0;
  char c3 = 0;
  if (!tty_readc_peek(tty, &c1)) goto fail;
  if (c1 != '[' && c1 != 'O')    goto fail;
  if (!tty_readc_peek(tty, &c2)) goto fail;
  debug_msg("tty: read escape: %c%c\n", c1, c2 );  
  if (c1 == '[') {
    if (c2 >= '0' && c2 <= '9') {
      if (!tty_readc_peek(tty, &c3)) goto fail;
      debug_msg("    third char: %c\n", c3 );  
      if (c3 != '~') goto fail;
      switch(c2) {  
        case '3': return KEY_DEL;
        case '1': 
        case '7': return KEY_HOME;
        case '4': 
        case '8': return KEY_END;
        case '5': return KEY_PAGEUP;
        case '6': return KEY_PAGEDOWN;
      }
    }
    else switch(c2) {
      case 'A': return KEY_UP;
      case 'B': return KEY_DOWN;
      case 'C': return KEY_RIGHT;
      case 'D': return KEY_LEFT;
      case 'H': return KEY_HOME; 
      case 'F': return KEY_END; 
    }
  }
  else if (c1 == 'O') {
    switch(c2) {
      case 'H': return KEY_HOME;
      case 'F': return KEY_END;
    }
  }
fail:
  if (c3 != 0) tty_cpush_char(tty,c3);    
  if (c2 != 0) tty_cpush_char(tty,c2);    
  if (c1 != 0) tty_cpush_char(tty,c1);    
  return KEY_ESC;
}


internal bool tty_readc_peek(tty_t* tty, char* c) {
  if (!tty_has_available(tty)) {
    if (c!=NULL) *c = 0;
    return false;
  }
  else {
    return tty_readc(tty,c);
  }
}

// read a single char/key
internal code_t tty_read(tty_t* tty) {
  // is there a pushed back code?
  if (tty->pushed > 0) {
    code_t code = tty->pushbuf[0];
    tty->pushed--;
    rl_memmove(tty->pushbuf, tty->pushbuf + 1, tty->pushed * ssizeof(code_t));
    return code;
  }

  // read from a character stream
  char c;
  if (!tty_readc(tty, &c)) return -1;  
  debug_msg( "tty: key: 0x%04x ('%c',%d)\n", c, (c >= ' ' && c <= '~' ? c : '-'), c );
  if (c == KEY_ESC) {
    return tty_read_esc(tty);
  }
  else {
    return (code_t)((uint8_t)c);
  }
}

internal void tty_pushback( tty_t* tty, code_t c ) {
  if (tty->pushed >= TTY_PUSH_MAX) return;
  tty->pushbuf[tty->pushed] = c;
  tty->pushed++;
}




//-------------------------------------------------------------
// low-level character pushback (for escape sequences and windows)
//-------------------------------------------------------------

static bool tty_cpop(tty_t* tty, char* c) {  
  if (tty->cpushed <= 0) {
    *c = 0;
    return false;
  }
  else {
    *c = tty->cpushbuf[0];
    tty->cpushed--;
    rl_memmove(tty->cpushbuf, tty->cpushbuf + 1, tty->cpushed );
    return true;
  }
}

static void tty_cpush(tty_t* tty, const char* s) {
  ssize_t len = rl_strlen(s);
  if (tty->pushed + len > TTY_PUSH_MAX) {
    assert(false);
    debug_msg("tty: cpush buffer full! (pushing %s)\n", s);
    return;
  }
  rl_memcpy( tty->cpushbuf, s, len );
  tty->cpushed += len;
  return;
}


static void tty_cpush_char(tty_t* tty, char c) {  
  char buf[2];
  buf[0] = c;
  buf[1] = 0;
  tty_cpush(tty,buf);
}


static void tty_cpush_unicode(tty_t* tty, uint32_t c) {
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

internal bool tty_init(tty_t* tty, int fin) 
{
  tty->fin = (fin < 0 ? STDIN_FILENO : fin);
  return (isatty(fin) && tty_init_raw(tty) && tty_init_utf8(tty));
}

internal void tty_done(tty_t* tty) {
  tty_end_raw(tty);
}


//-------------------------------------------------------------
// Posix
//-------------------------------------------------------------
#if !defined(_WIN32)

static bool tty_readc(tty_t* tty, char* c) {
  if (tty_cpop(tty,c)) return true;
  return (read(tty->fin, c, 1) == 1);
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
  /*
  // The following does not work as one cannot paste unicode characters this way :-(
  DWORD nread;
  ReadConsole(tty->hcon, c, 1, &nread, NULL);
  if (nread != 1) return false;
  debug_msg("tty: readc: \\x%02x\n", *c);
  */
  
  if (tty_cpop(tty,c)) return true;
  tty_waitc_console(tty);
  return tty_cpop(tty,c);
}

static void tty_waitc_console(tty_t* tty) 
{
  //  wait for a key down event
  INPUT_RECORD inp;
	DWORD count;
  uint32_t surrogate_hi;
  while (true) {
		if (!ReadConsoleInputW( tty->hcon, &inp, 1, &count)) return;
    if (count != 1) return;
    // wait for key down events (except for Alt-key-up which is used for unicode pasting...)
    if (inp.EventType != KEY_EVENT) continue;
    if (!inp.Event.KeyEvent.bKeyDown && inp.Event.KeyEvent.wVirtualKeyCode != VK_MENU) {
			continue;
		}

    // ignore AltGr
    DWORD state = inp.Event.KeyEvent.dwControlKeyState;
    DWORD altgr = LEFT_CTRL_PRESSED | RIGHT_ALT_PRESSED;
    if ((state & altgr) == altgr) { state &= ~altgr; }
    
    // get modifiers
    bool ctrl = (state & ( RIGHT_CTRL_PRESSED | LEFT_CTRL_PRESSED )) != 0;
    bool alt  = (state & ( RIGHT_ALT_PRESSED | LEFT_ALT_PRESSED )) != 0;

    // virtual keys
    uint32_t chr = (uint32_t)inp.Event.KeyEvent.uChar.UnicodeChar;
    debug_msg("tty: console read: %s%s 0x%04x ('%c')\n", ctrl ? "ctrl-" : "", alt ? "alt-" : "", chr);
    if (chr == 0) { 
      if (!ctrl && !alt) {
        switch (inp.Event.KeyEvent.wVirtualKeyCode) {
          case VK_LEFT:   tty_cpush(tty, "^[[D"); return; 
          case VK_RIGHT:  tty_cpush(tty, "^[[C"); return;
          case VK_UP:     tty_cpush(tty, "^[[A"); return;
          case VK_DOWN:   tty_cpush(tty, "^[[B"); return;
          case VK_HOME:   tty_cpush(tty, "^[[H"); return;
          case VK_END:    tty_cpush(tty, "^[[F"); return;
          case VK_DELETE: tty_cpush(tty, "^[[3~"); return;
          case VK_PRIOR:  tty_cpush(tty, "^[[5~"); return;  //page up
          case VK_NEXT:   tty_cpush(tty, "^[[6~"); return;  //page down          
        }
      }
      continue;  // ignore other control keys (shift etc).
    }
    // surrogate pairs
    else if (chr >= 0xD800 && chr <= 0xDBFF) {
			surrogate_hi = (chr - 0xD800);
			continue;
    }
    else if (chr >= 0xDC00 && chr <= 0xDFFF) {
			chr = ((surrogate_hi << 10) + (chr - 0xDC00) + 0x10000);
      tty_cpush_unicode(tty,chr);
      return;
		}
    // regular character
    else {
			tty_cpush_unicode(tty,chr);
			return;
    }
  }
}  

internal void tty_start_raw(tty_t* tty) {
  if (tty->raw_enabled) return;
  GetConsoleMode(tty->hcon,&tty->hcon_orig_mode);
  DWORD mode =  ENABLE_VIRTUAL_TERMINAL_INPUT | ENABLE_QUICK_EDIT_MODE | ENABLE_PROCESSED_INPUT ;
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


