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

//-------------------------------------------------------------
// Forward declarations
//-------------------------------------------------------------

static bool tty_has_available(tty_t* tty);
static bool tty_readc(tty_t* tty, char* c);
static void tty_cpush_char(tty_t* tty, char c);


//-------------------------------------------------------------
// Key code helpers
//-------------------------------------------------------------

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
// Translate escape sequence to key code
//-------------------------------------------------------------

static bool esc_ctrl( char c, code_t* code ) {
  switch(c) {
    case 'A': *code = KEY_CTRL_UP; return true;
    case 'B': *code = KEY_CTRL_DOWN; return true;
    case 'C': *code = KEY_CTRL_RIGHT; return true;
    case 'D': *code = KEY_CTRL_LEFT; return true;
    case 'F': *code = KEY_CTRL_END; return true;
    case 'H': *code = KEY_CTRL_HOME; return true;
    case 'M': *code = KEY_LINEFEED; return true;
    case 'Z': *code = KEY_CTRL_TAB; return true;
    default : *code = 0; return false;
  }  
}

// Read ANSI key escape sequences
// Read non-blocking an push back characters if the escape code was not valid.
static code_t tty_read_esc(tty_t* tty) {
  // <https://en.wikipedia.org/wiki/ANSI_escape_code#Control_characters>
  char c1 = 0;
  char c2 = 0;
  char c3 = 0;
  char c4 = 0;
  char c5 = 0;
  code_t code;
  if (!tty_readc_peek(tty, &c1)) goto fail;
  if (c1 != '[' && c1 != 'O' && c1 != 'o')    goto fail;
  if (!tty_readc_peek(tty, &c2)) goto fail;
  debug_msg("tty: read escape: %c%c\n", c1, c2 );  
  if (c1 == '[') {
    if (c2 >= '0' && c2 <= '9') {      
      if (!tty_readc_peek(tty, &c3)) {
        if (c2 == '9') return KEY_DEL;  // ESC [ 9
        goto fail;
      }
      debug_msg("tty: read more escape: %c%c%c\n", c1, c2, c3 ); 
      if (c3 == '~') {
        // ESC [ ? ~
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
      else if (c3 == '^') {
        // ESC [ ? ^
        switch(c2) {
          case '3': return KEY_CTRL_DEL;  // urxvt
          case '5': return KEY_CTRL_PAGEUP;    // Eterm/rxvt
          case '6': return KEY_CTRL_PAGEDOWN;  // Eterm/rxvt
          case '7': return KEY_CTRL_HOME; // Eterm/rxvt
          case '8': return KEY_CTRL_END;  // Eterm/rxvt
        }
      }
      else if (c3 == ';') {
        if (!tty_readc_peek(tty, &c4)) goto fail;
        debug_msg("tty: read more escape: %c%c%c%c\n", c1, c2, c3, c4 ); 
        if (!tty_readc_peek(tty, &c5)) goto fail;
        debug_msg("tty: read more escape: %c%c%c%c%c\n", c1, c2, c3, c4, c5 ); 
        if (c2 == '1' && c4 == '5') {  // ctrl+
          // ESC [ 1 ; 5 ? 
          if (esc_ctrl(c5,&code)) return code;          
        }
        if (c2 == '1' && c4 == '2') {  // shift+ 
          // ESC [ 1 ; 2 ? 
          if (esc_ctrl(c5,&code)) return code;          
        }
        else if (c2 == '3' && c4 == '3' && c5 == '~') {
          // ESC [ 3 ; 3 ~
          return KEY_CTRL_DEL;
        }
      }
    }
    // ESC [ ?
    else switch(c2) {
      case 'A': return KEY_UP;
      case 'B': return KEY_DOWN;
      case 'C': return KEY_RIGHT;
      case 'D': return KEY_LEFT;
      case 'E': return '5';       // numpad 5
      case 'F': return KEY_END;
      case 'H': return KEY_HOME;
      case 'M': return KEY_LINEFEED;  // ctrl+enter 
      case 'Z': return KEY_CTRL_TAB;  // shift+tab
      // Freebsd:
      case 'I': return KEY_PAGEUP;  
      case 'L': return KEY_INS;     
      // case 'M': F1      
      // ...
      // case 'T': F8
      // Mach:
      case 'U': return KEY_PAGEDOWN;   
      case 'V': return KEY_PAGEUP;     
      case 'Y': return KEY_END;       
      case '9': return KEY_DEL;   // unreachable due to previous if
      case '@': return KEY_INS;   
    }
  }
  else if (c1 == 'O') {
    // ESC O ?   
    switch(c2) {
      case 'A': return KEY_UP;
      case 'B': return KEY_DOWN;
      case 'C': return KEY_RIGHT;
      case 'D': return KEY_LEFT;
      case 'F': return KEY_END;
      case 'H': return KEY_HOME;     
      case 'a': return KEY_CTRL_UP;
      case 'b': return KEY_CTRL_DOWN;
      case 'c': return KEY_CTRL_RIGHT;
      case 'd': return KEY_CTRL_LEFT;
      case 'z': return KEY_CTRL_TAB;
      // numpad 
      case 'E': return '5';       
      case 'M': return KEY_ENTER; 
      case 'j': return '*';
      case 'k': return '+';
      case 'l': return ',';
      case 'm': return '-'; 
      case 'n': return KEY_DEL;
      case 'o': return '/'; 
      case 'p': return KEY_INS;
      case 'q': return KEY_END;  
      case 'r': return KEY_DOWN; 
      case 's': return KEY_PAGEDOWN; 
      case 't': return KEY_LEFT; 
      case 'u': return '5';
      case 'v': return KEY_RIGHT;
      case 'w': return KEY_HOME;  
      case 'x': return KEY_UP; 
      case 'y': return KEY_PAGEUP;   
      case '1': {
        if (!tty_readc_peek(tty, &c3)) goto fail;
        if (c3 != ';') goto fail;
        if (!tty_readc_peek(tty, &c4)) goto fail;
        if (c4 != '5') goto fail;
        if (!tty_readc_peek(tty, &c5)) goto fail;
        // ESC O 1 ; 5 ?  ()
        if (esc_ctrl(c5,&code)) return code;
        break;
      }
      case '5': {
        if (!tty_readc_peek(tty, &c3)) goto fail;
        // ESC O 5 ?   (on haiku)
        if (esc_ctrl(c3,&code)) return code;
      }                       
    }
  }
  else if (c1 == 'o') {
    // ESC o ?  (on Eterm)
    if (c2 >= 'a' && c2 <= 'z') {
      c2 = c2 - 'a' + 'A'; // to uppercase
    }
    if (esc_ctrl(c2,&code)) return code;    
  }
fail:
  debug_msg("tty: unknown escape sequence: ESC %c %c %c %c %c\n", (c1==0 ? ' ' : c1), (c2==0 ? ' ' : c2), (c3==0 ? ' ' : c3), (c4==0 ? ' ' : c4), (c5==0 ? '-' : c5) );
  if (c5 != 0) tty_cpush_char(tty,c5);    
  if (c4 != 0) tty_cpush_char(tty,c4);    
  if (c3 != 0) tty_cpush_char(tty,c3);    
  if (c2 != 0) tty_cpush_char(tty,c2);    
  if (c1 != 0) tty_cpush_char(tty,c1);    
  return KEY_ESC;
}


//-------------------------------------------------------------
// Code point buffer
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
// Read a key code
//-------------------------------------------------------------

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
  code_t code;
  if (tty_code_pop(tty,&code)) {
    return code;
  }

  // read from a character stream
  char c;
  if (!tty_readc(tty, &c)) return -1;  
  
  if (c == KEY_ESC) {
    code = tty_read_esc(tty);
  }
  else {
    code = (code_t)((uint8_t)c);
  }
  debug_msg( "tty: key: code: 0x%04x ('%c')\n", code, (code >= ' ' && code <= '~' ? code : ' '));
  return code;
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
    tty->cpushed--;
    *c = tty->cpushbuf[tty->cpushed];
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
  for (ssize_t i = 0; i < len; i++) {
    tty->cpushbuf[tty->cpushed + i] = s[len - i - 1];
  }
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
  DWORD modstate = 0;
  uint32_t surrogate_hi;
  while (true) {
		if (!ReadConsoleInputW( tty->hcon, &inp, 1, &count)) return;
    if (count != 1) return;
    // wait for key down events 
    if (inp.EventType != KEY_EVENT) continue;

    // maintain modifier state
    DWORD state = inp.Event.KeyEvent.dwControlKeyState;
    if (inp.Event.KeyEvent.uChar.UnicodeChar == 0) {
      if (inp.Event.KeyEvent.bKeyDown) {
        modstate |= state;      
      }
      else {
        modstate &= ~state;
      }
    }

    // ignore AltGr
    DWORD altgr = LEFT_CTRL_PRESSED | RIGHT_ALT_PRESSED;
    if ((modstate & altgr) == altgr) { modstate &= ~altgr; }

    // only process keydown events (except for Alt-up which is used for unicode pasting...)
    if (!inp.Event.KeyEvent.bKeyDown && inp.Event.KeyEvent.wVirtualKeyCode != VK_MENU) {
			continue;
		}
    
    // get modifiers
    bool ctrl = (modstate & ( RIGHT_CTRL_PRESSED | LEFT_CTRL_PRESSED )) != 0;
    bool alt  = (modstate & ( RIGHT_ALT_PRESSED | LEFT_ALT_PRESSED )) != 0;
    bool shift= (modstate & SHIFT_PRESSED) != 0;

    // virtual keys
    uint32_t chr = (uint32_t)inp.Event.KeyEvent.uChar.UnicodeChar;
    //debug_msg("tty: console %s: %s%s virt 0x%04x, chr 0x%04x ('%c')\n", inp.Event.KeyEvent.bKeyDown ? "down" : "up", ctrl ? "ctrl-" : "", alt ? "alt-" : "", inp.Event.KeyEvent.wVirtualKeyCode, chr, chr);

    if (chr == 0) { 
      if (!ctrl && !alt) {
        switch (inp.Event.KeyEvent.wVirtualKeyCode) {
          case VK_LEFT:   tty_cpush(tty, "\x1B[D"); return; 
          case VK_RIGHT:  tty_cpush(tty, "\x1B[C"); return;
          case VK_UP:     tty_cpush(tty, "\x1B[A"); return;
          case VK_DOWN:   tty_cpush(tty, "\x1B[B"); return;
          case VK_HOME:   tty_cpush(tty, "\x1B[H"); return;
          case VK_END:    tty_cpush(tty, "\x1B[F"); return;
          case VK_DELETE: tty_cpush(tty, "\x1B[3~"); return;
          case VK_PRIOR:  tty_cpush(tty, "\x1B[5~"); return;  //page up
          case VK_NEXT:   tty_cpush(tty, "\x1B[6~"); return;  //page down          
        }
      }
      else if (ctrl && !alt) {
        // ctrl+?
        switch (inp.Event.KeyEvent.wVirtualKeyCode) {
          case VK_LEFT:   tty_cpush(tty, "\x1B[1;5D"); return; 
          case VK_RIGHT:  tty_cpush(tty, "\x1B[1;5C"); return;
          case VK_UP:     tty_cpush(tty, "\x1B[1;5A"); return;
          case VK_DOWN:   tty_cpush(tty, "\x1B[1;5B"); return;
          case VK_HOME:   tty_cpush(tty, "\x1B[1;5H"); return;
          case VK_END:    tty_cpush(tty, "\x1B[1;5F"); return;
          case VK_TAB:    tty_cpush(tty, "\x1B[1;5Z"); return;
          case VK_RETURN: tty_cpush(tty, "\n");      return;
          case VK_DELETE: tty_cpush(tty, "\x1B[3^"); return;
          case VK_PRIOR:  tty_cpush(tty, "\x1B[5^"); return;  //page up
          case VK_NEXT:   tty_cpush(tty, "\x1B[6^"); return;  //page down          
        }        
      }
      continue;  // ignore other control keys (shift etc).
    }
    // non-virtual keys
    // ctrl/shift+ENTER
    if (chr == KEY_ENTER && (ctrl || shift)) {  
      chr = '\n';   // shift/ctrl+enter becomes linefeed
    }
    // ctrl+TAB
    if (chr == KEY_TAB && ctrl) {  
      tty_cpush(tty, "\x1B[1;5Z");
      return;
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


