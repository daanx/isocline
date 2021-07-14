/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#pragma once
#ifndef RL_TTY_H
#define RL_TTY_H

#include "common.h"

#if defined(_WIN32)
#include <windows.h>
#include <io.h>
#else
#include <termios.h>
#endif

typedef int code_t;

#define KEY_NONE   (0)
#define KEY_TAB    (9)
#define KEY_ENTER  (13)
#define KEY_ESC    (27)
#define KEY_BACKSP (127)
#define KEY_UP     (0x100)
#define KEY_DOWN   (KEY_UP+1)
#define KEY_LEFT   (KEY_UP+2)
#define KEY_RIGHT  (KEY_UP+3)
#define KEY_HOME   (KEY_UP+4)
#define KEY_END    (KEY_UP+5)
#define KEY_NEXT   (KEY_UP+6)
#define KEY_PREV   (KEY_UP+7)
#define KEY_DEL    (KEY_UP+8)
#define KEY_PAGEUP (KEY_UP+9)
#define KEY_PAGEDOWN (KEY_UP+10)

#define KEY_CTRL(x)  (x - 'A' + 1)

#define TTY_PUSH_MAX (32)

typedef struct tty_s {
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
} tty_t;

// Primitives
internal bool tty_init(tty_t* tty, int fin);
internal void tty_done(tty_t* tty);
internal void tty_start_raw(tty_t* tty);
internal void tty_end_raw(tty_t* tty);
internal code_t tty_read(tty_t* tty);
internal bool tty_readc_peek(tty_t* tty, char* c);   // used in term.c
internal void tty_pushback( tty_t* tty, code_t c );

internal bool code_is_char(tty_t*, code_t c, char* chr );
internal bool code_is_follower( tty_t*, code_t c, char* chr);
internal bool code_is_extended( tty_t*, code_t c, char* chr, int* tofollow);
internal bool code_is_key( tty_t*, code_t c );

#endif // RL_TTY_H
