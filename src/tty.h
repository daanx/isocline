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

//-------------------------------------------------------------
// TTY/Keyboard input 
//-------------------------------------------------------------

// Key code
typedef uint32_t  code_t;

// TTY interface
struct tty_s;
typedef struct tty_s tty_t;


internal tty_t* tty_new(alloc_t* mem, int fin);
internal void   tty_free(tty_t* tty);

internal bool   tty_is_utf8(tty_t* tty);
internal void   tty_start_raw(tty_t* tty);
internal void   tty_end_raw(tty_t* tty);
internal code_t tty_read(tty_t* tty);
internal bool   tty_readc_noblock(tty_t* tty, char* c);   // used in term.c
internal void   tty_code_pushback( tty_t* tty, code_t c );

internal bool   code_is_char(tty_t*, code_t c, char* chr );
internal bool   code_is_follower( tty_t*, code_t c, char* chr);
internal bool   code_is_extended( tty_t*, code_t c, char* chr, int* tofollow);


// shared between tty.c and tty_esc.c
internal void   tty_cpush_char(tty_t* tty, char c);
internal void   tty_cpush_unicode(tty_t* tty, uint32_t c);
internal bool   tty_cpop(tty_t* tty, char* c);
internal code_t tty_read_esc(tty_t* tty); // in tty_esc.c

//-------------------------------------------------------------
// Key codes
//-------------------------------------------------------------

#define KEY_CHAR(c)       ((code_t)c)

#define MOD_SHIFT         0x1000U
#define MOD_ALT           0x2000U
#define MOD_CTRL          0x4000U

#define KEY_NOMODS(k)     (k & 0x0FFFU)
#define KEY_MODS(k)       (k & 0xF000U)

#define WITH_SHIFT(x)     (x | MOD_SHIFT)
#define WITH_ALT(x)       (x | MOD_ALT)
#define WITH_CTRL(x)      (x | MOD_CTRL)

#define KEY_NONE          (0)
#define KEY_CTRL_A        (1)
#define KEY_CTRL_B        (2)
#define KEY_CTRL_C        (3)
#define KEY_CTRL_D        (4)
#define KEY_CTRL_E        (5)
#define KEY_CTRL_F        (6)
#define KEY_BELL          (7)
#define KEY_BACKSP        (8)
#define KEY_TAB           (9)
#define KEY_LINEFEED      (10)   // ctrl/shift + enter is considered KEY_LINEFEED
#define KEY_CTRL_K        (11)
#define KEY_CTRL_L        (12)
#define KEY_ENTER         (13)
#define KEY_CTRL_N        (14)
#define KEY_CTRL_O        (15)
#define KEY_CTRL_P        (16)
#define KEY_CTRL_Q        (17)
#define KEY_CTRL_R        (18)
#define KEY_CTRL_S        (19)
#define KEY_CTRL_T        (20)
#define KEY_CTRL_U        (21)
#define KEY_CTRL_V        (22)
#define KEY_CTRL_W        (23)
#define KEY_CTRL_X        (24)
#define KEY_CTRL_Y        (25)
#define KEY_CTRL_Z        (26)
#define KEY_ESC           (27)
#define KEY_SPACE         (32)
#define KEY_RUBOUT        (127)  // always translated to KEY_BACKSP

#define KEY_VIRT          (0x100)  
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
#define KEY_F(n)          (KEY_F1 + n - 1)

#define KEY_EVENT_BASE    (0x200)
#define KEY_EVENT_RESIZE  (KEY_EVENT_BASE+0)  // not use for now


// Convenience
#define KEY_CTRL_UP       (WITH_CTRL(KEY_UP))
#define KEY_CTRL_DOWN     (WITH_CTRL(KEY_DOWN))
#define KEY_CTRL_LEFT     (WITH_CTRL(KEY_LEFT))
#define KEY_CTRL_RIGHT    (WITH_CTRL(KEY_RIGHT))
#define KEY_CTRL_HOME     (WITH_CTRL(KEY_HOME))
#define KEY_CTRL_END      (WITH_CTRL(KEY_END))
#define KEY_CTRL_DEL      (WITH_CTRL(KEY_DEL))
#define KEY_CTRL_PAGEUP   (WITH_CTRL(KEY_PAGEUP))
#define KEY_CTRL_PAGEDOWN (WITH_CTRL(KEY_PAGEDOWN)))
#define KEY_CTRL_INS      (WITH_CTRL(KEY_INS))

#define KEY_SHIFT_TAB     (WITH_SHIFT(KEY_TAB))

#endif // RP_TTY_H
