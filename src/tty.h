/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#pragma once
#ifndef RP_TTY_H
#define RP_TTY_H

#include "common.h"

#if defined(_WIN32)
#include <windows.h>
#include <io.h>
#else
#include <termios.h>
#endif

typedef uint32_t  code_t;

#define ESC               "\x1B"

#define KEY_NONE          (0)
#define KEY_TAB           (9)
#define KEY_LINEFEED      (10)  // ctrl/shift + enter/tab is considered KEY_LINEFEED
#define KEY_ENTER         (13)
#define KEY_ESC           (27)
#define KEY_SPACE         (32)
#define KEY_BACKSP        (127)

#define MOD_SHIFT         0x1000
#define MOD_ALT           0x2000
#define MOD_CTRL          0x4000

#define KEY_CHAR(c)       ((code_t)c)
#define KEY_CTRL(x)       (x | MOD_CTRL)
#define KEY_ALT(x)        (x | MOD_ALT)
#define KEY_SHIFT(x)      (x | MOD_SHIFT)

#define KEY_NOMODS(k)     (k & 0x0FFF)
#define KEY_MODS(k)       (k & 0xF000)

#define KEY_VIRT          0x0100   
#define KEY_UP            (KEY_VIRT+0)
#define KEY_DOWN          (KEY_VIRT+1)
#define KEY_LEFT          (KEY_VIRT+2)
#define KEY_RIGHT         (KEY_VIRT+3)
#define KEY_HOME          (KEY_VIRT+4)
#define KEY_END           (KEY_VIRT+5)
#define KEY_DEL           (KEY_VIRT+6)
#define KEY_PAGEUP        (KEY_VIRT+7)
#define KEY_PAGEDOWN      (KEY_VIRT+8)
#define KEY_INS           (KEY_VIRT+9)

#define KEY_F1            (KEY_VIRT+11)
#define KEY_F2            (KEY_VIRT+12)
#define KEY_F3            (KEY_VIRT+13)
#define KEY_F4            (KEY_VIRT+14)
#define KEY_F5            (KEY_VIRT+15)
#define KEY_F6            (KEY_VIRT+16)
#define KEY_F7            (KEY_VIRT+17)
#define KEY_F8            (KEY_VIRT+18)
#define KEY_F9            (KEY_VIRT+19)
#define KEY_F10           (KEY_VIRT+20)
#define KEY_F11           (KEY_VIRT+21)
#define KEY_F12           (KEY_VIRT+22)
#define KEY_F(n)          (KEY_F1 + n)

// Convenience

#define KEY_CTRL_UP       (KEY_UP | MOD_CTRL)
#define KEY_CTRL_DOWN     (KEY_DOWN | MOD_CTRL)
#define KEY_CTRL_LEFT     (KEY_LEFT | MOD_CTRL)
#define KEY_CTRL_RIGHT    (KEY_RIGHT | MOD_CTRL)
#define KEY_CTRL_HOME     (KEY_HOME | MOD_CTRL)
#define KEY_CTRL_END      (KEY_END | MOD_CTRL)
#define KEY_CTRL_DEL      (KEY_DEL | MOD_CTRL)
#define KEY_CTRL_PAGEUP   (KEY_PAGEUP | MOD_CTRL)
#define KEY_CTRL_PAGEDOWN (KEY_PAGEDOWN | MOD_CTRL))
#define KEY_CTRL_INS      (KEY_INS | MOD_CTRL)

// We treat ctrl+<tab/enter> and shift+<tab/enter> as '\n' for portability. 
// - shift+tab  works across linux/macos/windows.
// - ctrl+enter works on linux/windows but not macos.


#define KEY_EVENT_RESIZE  (KEY_VIRT+1000)



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
internal bool tty_readc_noblock(tty_t* tty, char* c);   // used in term.c
internal void tty_code_pushback( tty_t* tty, code_t c );

internal bool code_is_char(tty_t*, code_t c, char* chr );
internal bool code_is_follower( tty_t*, code_t c, char* chr);
internal bool code_is_extended( tty_t*, code_t c, char* chr, int* tofollow);
internal bool code_is_key( tty_t*, code_t c );

#endif // RP_TTY_H
