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
#define KEY_OFS    (((code_t)1) << 21)


internal bool tty_readc(tty_t* tty, char* c) {
  return (read(tty->fin, c, 1) == 1);
}

static bool tty_has_available(tty_t* tty) {
  unused(tty);
  int n = 0;
  return (ioctl(0, FIONREAD, &n) == 0 && n > 0);
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


// read escape sequence
static code_t tty_read_esc(tty_t* tty) {
  char c1;
  if (!tty_readc_peek(tty, &c1)) goto fail;
  char c2;
  if (!tty_readc_peek(tty, &c2)) goto fail;
  debug_msg("tty: read escape: %c%c\n", c1, c2 );  
  if (c1 == '[') {
    if (c2 >= '0' && c2 <= '9') {
      char c3;
      if (!tty_readc_peek(tty, &c3)) goto fail;
      debug_msg("    third char: %c\n", c3 );  
      if (c3 == '~' && c2 == '3') return KEY_DEL;
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
  return KEY_ESC;
}

// read a single char/key
internal code_t tty_read(tty_t* tty) {
  // pushed back?
  if (tty->pushed > 0) {
    code_t code = tty->pushbuf[0];
    tty->pushed--;
    rl_memmove(tty->pushbuf, tty->pushbuf + 1, tty->pushed * ssizeof(code_t));
    return code;
  }

  // read 
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
// Init 
//-------------------------------------------------------------
internal void tty_start_raw(tty_t* tty) {
  if (tty->raw_enabled) return;
  if (tcsetattr(tty->fin,TCSAFLUSH,&tty->raw_ios) < 0) return;
  tty->raw_enabled = true;
}

internal void tty_end_raw(tty_t* tty) {
  if (!tty->raw_enabled) return;
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

static bool tty_init_utf8(tty_t* tty) {
  char* loc = setlocale(LC_ALL,"");
  tty->is_utf8 = (loc != NULL && (strstr(loc,"UTF-8")));
  debug_msg("tty: utf8: %s\n", tty->is_utf8 ? "true" : "false");
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
