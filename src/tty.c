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
#include <io.h>
#define isatty(fd)     _isatty(fd)
#define read(fd,s,n)   _read(fd,s,n)
#define STDIN_FILENO   0
#else
#include <termios.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <signal.h>
#include <errno.h>
#if !defined(FIONREAD)
#include <fcntl.h>
#endif
#endif

#define TTY_PUSH_MAX (64)

struct tty_s {
  int       fd_in;                  // input handle
  bool      raw_enabled;            // is raw mode enabled?
  bool      is_utf8;                // is the input stream in utf-8 mode?
  bool      has_term_resize_event;  // are resize events generated?
  bool      term_resize_event;      // did a term resize happen?
  alloc_t*  mem;                    // memory allocator
  code_t    pushbuf[TTY_PUSH_MAX];  // push back buffer for full key codes  
  ssize_t   push_count;               
  uint8_t   cpushbuf[TTY_PUSH_MAX]; // low level push back buffer for bytes
  ssize_t   cpush_count;
  #if defined(_WIN32)               
  HANDLE    hcon;                   // console input handle
  DWORD     hcon_orig_mode;         // original console mode
  #else
  struct termios  orig_ios;         // original terminal settings
  struct termios  raw_ios;          // raw terminal settings
  #endif
};


//-------------------------------------------------------------
// Forward declarations of platform dependent primitives below
//-------------------------------------------------------------

rp_private bool tty_readc_noblock(tty_t* tty, uint8_t* c);  // does not modify `c` when no input (false is returned)
rp_private bool tty_readc(tty_t* tty, uint8_t* c);          

//-------------------------------------------------------------
// Key code helpers
//-------------------------------------------------------------

rp_private bool code_is_ascii_char(code_t c, char* chr ) {
  if (c >= ' ' && c <= 0x7F) {
    if (chr != NULL) *chr = (char)c;
    return true;
  }
  else {
    if (chr != NULL) *chr = 0;
    return false;
  }
}

rp_private bool code_is_unicode(code_t c, unicode_t* uchr) {
  if (c <= KEY_UNICODE_MAX) {
    if (uchr != NULL) *uchr = c;
    return true;
  }
  else {
    if (uchr != NULL) *uchr = 0;
    return false;
  }
}

rp_private bool code_is_virt_key(code_t c ) {
  return (KEY_NO_MODS(c) <= 0x20 || KEY_NO_MODS(c) >= KEY_VIRT);
}


//-------------------------------------------------------------
// Read a key code
//-------------------------------------------------------------
static code_t modify_code( code_t code );

static code_t tty_read_utf8( tty_t* tty, uint8_t c0 ) {
  uint8_t buf[5];
  memset(buf, 0, 5);

  // try to read as many bytes as potentially needed
  buf[0] = c0;
  ssize_t count = 1;
  if (c0 > 0x7F) {
    if (tty_readc_noblock(tty, buf+count)) {
      count++;
      if (c0 > 0xDF) {
        if (tty_readc_noblock(tty, buf+count)) {
          count++;
          if (c0 > 0xEF) {
            if (tty_readc_noblock(tty, buf+count)) {
              count++;
            }
          }
        }
      }
    }
  }

  // decode the utf8 to unicode
  ssize_t read = 0;
  code_t code = key_unicode(unicode_from_qutf8(buf, count, &read));

  // push back unused bytes (in the case of invalid utf8)
  while (count > read) {
    count--;
    if (count >= 0 && count <= 4) {  // to help the static analyzer
      tty_cpush_char(tty, buf[count]);
    }
  }
  return code;
}

// pop a code from the pushback buffer.
static bool tty_code_pop(tty_t* tty, code_t* code);

// read a single char/key (data is used for events only)
rp_private code_t tty_read(tty_t* tty) 
{
  // is there a push_count back code?
  code_t code;
  if (tty_code_pop(tty,&code)) {
    return code;
  }

  // read a single char/byte from a character stream
  uint8_t c;
  if (!tty_readc(tty, &c)) return KEY_NONE;  
  
  if (c == KEY_ESC) {
    // escape sequence?
    code = tty_read_esc(tty);
  }
  else if (c <= 0x7F) {
    // ascii
    code = key_unicode(c);
  }
  else if (tty->is_utf8) {
    // utf8 sequence
    code = tty_read_utf8(tty,c);
  }
  else {
    // c >= 0x80 but tty is not utf8; use raw plane so we can translate it back in the end
    code = key_unicode( unicode_from_raw(c) );
  }

  return modify_code(code);
}

// Transform virtual keys to be more portable across platforms
static code_t modify_code( code_t code ) {
  code_t key  = KEY_NO_MODS(code);
  code_t mods = KEY_MODS(code);
  debug_msg( "tty: readc %s%s%s 0x%03x ('%c')\n", 
              mods&KEY_MOD_SHIFT ? "shift+" : "",  mods&KEY_MOD_CTRL  ? "ctrl+" : "", mods&KEY_MOD_ALT   ? "alt+" : "",
              key, (key >= ' ' && key <= '~' ? key : ' '));

  // treat KEY_RUBOUT (0x7F) as KEY_BACKSP
  if (key == KEY_RUBOUT) {
    code = KEY_BACKSP | mods;
  }
  // ctrl+'_' is translated to '\x1F' on Linux, translate it back 
  else if (key == key_char('\x1F') && (mods & KEY_MOD_ALT) == 0) {
    key = '_';
    code = WITH_CTRL(key_char('_'));
  }
  // treat ctrl/shift + enter always as KEY_LINEFEED for portability
  else if (key == KEY_ENTER && (mods == KEY_MOD_SHIFT || mods == KEY_MOD_ALT || mods == KEY_MOD_CTRL)) {
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
  
  // treat C0 codes without KEY_MOD_CTRL
  if (key < ' ' && (mods&KEY_MOD_CTRL) != 0) {
    code &= ~KEY_MOD_CTRL; 
  }
  
  return code;
}



//-------------------------------------------------------------
// High level code pushback
//-------------------------------------------------------------

static bool tty_code_pop( tty_t* tty, code_t* code ) {
  if (tty->push_count <= 0) return false;
  tty->push_count--;
  *code = tty->pushbuf[tty->push_count];
  return true;
}

rp_private void tty_code_pushback( tty_t* tty, code_t c ) {   
  // note: must be signal safe
  if (tty->push_count >= TTY_PUSH_MAX) return;
  tty->pushbuf[tty->push_count] = c;
  tty->push_count++;
}


//-------------------------------------------------------------
// low-level character pushback (for escape sequences and windows)
//-------------------------------------------------------------

rp_private bool tty_cpop(tty_t* tty, uint8_t* c) {  
  if (tty->cpush_count <= 0) {  // do not modify c on failure (see `tty_decode_unicode`)
    return false;
  }
  else {
    tty->cpush_count--;
    *c = tty->cpushbuf[tty->cpush_count];
    return true;
  }
}

static void tty_cpush(tty_t* tty, const char* s) {
  ssize_t len = rp_strlen(s);
  if (tty->push_count + len > TTY_PUSH_MAX) {
    debug_msg("tty: cpush buffer full! (pushing %s)\n", s);
    assert(false);
    return;
  }
  for (ssize_t i = 0; i < len; i++) {
    tty->cpushbuf[tty->cpush_count + i] = (uint8_t)( s[len - i - 1] );
  }
  tty->cpush_count += len;
  return;
}

// convenience function for small sequences
static void tty_cpushf(tty_t* tty, const char* fmt, ...) {
  va_list args;
  va_start(args,fmt);
  char buf[TTY_PUSH_MAX+1];
  vsnprintf(buf,TTY_PUSH_MAX,fmt,args);
  buf[TTY_PUSH_MAX] = 0;
  tty_cpush(tty,buf);
  va_end(args);
  return;
}

rp_private void tty_cpush_char(tty_t* tty, uint8_t c) {  
  uint8_t buf[2];
  buf[0] = c;
  buf[1] = 0;
  tty_cpush(tty, (const char*)buf);
}


//-------------------------------------------------------------
// Push escape codes (used on Windows to insert keys)
//-------------------------------------------------------------

static unsigned csi_mods(code_t mods) {
  unsigned m = 1;
  if (mods&KEY_MOD_SHIFT) m += 1;
  if (mods&KEY_MOD_ALT)   m += 2;
  if (mods&KEY_MOD_CTRL)  m += 4;
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
      (mods == KEY_MOD_CTRL && unicode < ' ' && unicode != KEY_TAB && unicode != KEY_ENTER 
                        && unicode != KEY_LINEFEED && unicode != KEY_BACKSP) ||
      (mods == KEY_MOD_SHIFT && unicode >= ' ' && unicode <= KEY_RUBOUT)) {
    tty_cpush_char(tty,(uint8_t)unicode);
  }
  else {
    tty_cpushf(tty,"\x1B[%u;%uu", unicode, csi_mods(mods) );
  }
}

//-------------------------------------------------------------
// Init
//-------------------------------------------------------------

static bool tty_init_raw(tty_t* tty);
static void tty_done_raw(tty_t* tty);

static bool tty_init_utf8(tty_t* tty) {
  #ifdef _WIN32
  tty->is_utf8 = true;
  #else
  char* loc = setlocale(LC_ALL,"");
  tty->is_utf8 = (loc != NULL && (rp_stristr(loc,"UTF-8") != NULL || rp_stristr(loc,"UTF8") != NULL));
  debug_msg("tty: utf8: %s (loc=%s)\n", tty->is_utf8 ? "true" : "false", loc);
  #endif
  return true;
}

rp_private tty_t* tty_new(alloc_t* mem, int fd_in) 
{
  tty_t* tty = mem_zalloc_tp(mem, tty_t);
  tty->mem = mem;
  tty->fd_in = (fd_in < 0 ? STDIN_FILENO : fd_in);
  if (!(isatty(tty->fd_in) && tty_init_raw(tty) && tty_init_utf8(tty))) {
    tty_free(tty);
    return NULL;
  }
  return tty;
}

rp_private void tty_free(tty_t* tty) {
  if (tty==NULL) return;
  tty_end_raw(tty);
  tty_done_raw(tty);
  mem_free(tty->mem,tty);
}

rp_private bool tty_is_utf8(const tty_t* tty) {
  if (tty == NULL) return true;
  return (tty->is_utf8);
}

rp_private bool tty_term_resize_event(tty_t* tty) {
  if (tty == NULL) return true;
  if (tty->has_term_resize_event) {
    if (!tty->term_resize_event) return false;
    tty->term_resize_event = false;  // reset.   
  }
  return true;  // always return true on systems without a resize event (more expensive but still ok)
}

//-------------------------------------------------------------
// Unix
//-------------------------------------------------------------
#if !defined(_WIN32)

static bool tty_readc(tty_t* tty, uint8_t* c) {
  if (tty_cpop(tty,c)) return true;
  *c = 0;
  ssize_t nread = read(tty->fd_in, (char*)c, 1);
  if (nread < 0 && errno == EINTR) {
    // can happen on SIGWINCH signal for terminal resize
  }
  return (nread == 1);
}

rp_private bool tty_readc_noblock(tty_t* tty, uint8_t* c) {
  // in our pushback buffer?
  if (tty_cpop(tty, c)) return true;
  
  #if defined(FIONREAD)
  // peek if any char is available.
  int navail = 0;
  if (ioctl(0, FIONREAD, &navail) == 0 || navail >= 1) {
    return tty_readc(tty, c);
  }
  #elif defined(O_NONBLOCK)
  // set temporarily to non-blocking read mode
  int fstatus = fcntl(tty->fd_in, F_GETFL, 0);
  if (fstatus != -1) {
    if (fcntl(tty->fd_in, F_SETFL, (fstatus | O_NONBLOCK)) != -1) {
      char buf[2] = { 0, 0 };
      ssize_t nread = read(tty->fd_in, buf, 1);
      fcntl(tty->fd_in, F_SETFL, fstatus);
      if (nread >= 1) {
        *c = (uint8_t)buf[0];
        return true;
      }
    }
  }
  #else
  # error "define non-blocking read for this platform"
  #endif
  return false;  
}

// We install various signal handlers to restore the terminal settings
// in case of a terminating signal. This is also used to catch terminal window resizes.
// This is not strictly needed so this can be disabled on 
// (older) platforms that do not support signal handling well.
#if defined(SIGWINCH) && defined(SA_RESTART)  // ensure basic signal functionality is defined

// store the tty in a global so we access it on unexpected termination
static tty_t* sig_tty; // = NULL

// Catch all termination signals (and SIGWINCH)
typedef struct sighandler_s {
  int signum;
  struct sigaction previous;
} sighandler_t;

static sighandler_t sighandlers[] = {
  { SIGWINCH, {0} },
  { SIGTERM , {0} }, 
  { SIGINT  , {0} }, 
  { SIGQUIT , {0} }, 
  { SIGHUP  , {0} },
  { SIGSEGV , {0} }, 
  { SIGTRAP , {0} }, 
  { SIGBUS  , {0} }, 
  { SIGTSTP , {0} },
  { SIGTTIN , {0} }, 
  { SIGTTOU , {0} },
  { 0       , {0} }
};

static bool sigaction_is_valid( struct sigaction* sa ) {
  return (sa->sa_sigaction != NULL && sa->sa_handler != SIG_DFL && sa->sa_handler != SIG_IGN);
}

// Generic signal handler
static void sig_handler(int signum, siginfo_t* siginfo, void* uap ) {
  if (signum == SIGWINCH) {
    if (sig_tty != NULL) {
      sig_tty->term_resize_event = true;
    }
  }
  else {
    // the rest are termination signals; restore the terminal mode. (`tcsetattr` is signal-safe)
    if (sig_tty != NULL && sig_tty->raw_enabled) {
      tcsetattr(sig_tty->fd_in, TCSAFLUSH, &sig_tty->orig_ios);
      sig_tty->raw_enabled = false;
    }
  }
  // call previous handler
  sighandler_t* sh = sighandlers;
  while( sh->signum != 0 && sh->signum != signum) { sh++; }
  if (sh->signum == signum) {
    if (sigaction_is_valid(&sh->previous)) {
      (sh->previous.sa_sigaction)(signum, siginfo, uap);
    }
  }
}

static void signals_install(tty_t* tty) {
  sig_tty = tty;
  // generic signal handler
  struct sigaction handler;
  memset(&handler,0,sizeof(handler));
  sigemptyset(&handler.sa_mask);
  handler.sa_sigaction = &sig_handler; 
  handler.sa_flags = SA_RESTART;
  // install for all signals
  for( sighandler_t* sh = sighandlers; sh->signum != 0; sh++ ) {
    if (sigaction( sh->signum, NULL, &sh->previous) == 0) {            // get previous
      if (sh->previous.sa_handler != SIG_IGN) {                        // if not to be ignored
        if (sigaction( sh->signum, &handler, &sh->previous ) < 0) {    // install our handler
          sh->previous.sa_sigaction = NULL;       // do not restore on error
        }
        else if (sh->signum == SIGWINCH) {
          sig_tty->has_term_resize_event = true;
        };
      }
    }    
  }
}

static void signals_restore(void) {
  // restore all signal handlers
  for( sighandler_t* sh = sighandlers; sh->signum != 0; sh++ ) {
    if (sigaction_is_valid(&sh->previous)) {
      sigaction( sh->signum, &sh->previous, NULL );
    };
  }
  sig_tty = NULL;
}

#else
static void signals_install(tty_t* tty) {
  rp_unused(tty);
  // nothing
}
static void signals_restore(void) {
  // nothing
}

#endif

rp_private void tty_start_raw(tty_t* tty) {
  if (tty->raw_enabled) return;
  if (tcsetattr(tty->fd_in,TCSAFLUSH,&tty->raw_ios) < 0) return;  
  tty->raw_enabled = true;
}

rp_private void tty_end_raw(tty_t* tty) {
  if (!tty->raw_enabled) return;
  tty->cpush_count = 0;
  if (tcsetattr(tty->fd_in,TCSAFLUSH,&tty->orig_ios) < 0) return;
  tty->raw_enabled = false;
}

static bool tty_init_raw(tty_t* tty) 
{  
  // Set input to raw mode. See <https://man7.org/linux/man-pages/man3/termios.3.html>.
  if (tcgetattr(tty->fd_in,&tty->orig_ios) == -1) return false;
  tty->raw_ios = tty->orig_ios; 
  // input: no break signal, no \r to \n, no parity check, no 8-bit to 7-bit, no flow control
  tty->raw_ios.c_iflag &= ~(unsigned long)(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
  // control: allow 8-bit
  tty->raw_ios.c_cflag |= CS8;
  // local: no echo, no line-by-line (canonical), no extended input processing, no signals for ^z,^c
  tty->raw_ios.c_lflag &= ~(unsigned long)(ECHO | ICANON | IEXTEN | ISIG);
  // 1 byte at a time, no delay.
  tty->raw_ios.c_cc[VTIME] = 0;
  tty->raw_ios.c_cc[VMIN] = 1;

  // store in global so our signal handlers can restore the terminal mode
  signals_install(tty);
  
  return true;
}

static void tty_done_raw(tty_t* tty) {
  rp_unused(tty);
  signals_restore();
}


#else

//-------------------------------------------------------------
// Windows
// For best portability we push CSI escape sequences directly
// to the character stream (instead of returning key codes).
//-------------------------------------------------------------

static void tty_waitc_console(tty_t* tty);

static bool tty_readc(tty_t* tty, uint8_t* c) {
  if (tty_cpop(tty,c)) return true;
  // The following does not work as one cannot paste unicode characters this way :-(
  //   DWORD nread; ReadConsole(tty->hcon, c, 1, &nread, NULL);
  // so instead we read directly from the console input events and cpush into the tty:
  tty_waitc_console(tty); 
  return tty_cpop(tty,c);
}

rp_private bool tty_readc_noblock(tty_t* tty, uint8_t* c) {  // don't modify `c` if there is no input
  // in our pushback buffer?
  if (tty_cpop(tty, c)) return true;
  // any events in the input queue?
  DWORD count = 0;
  GetNumberOfConsoleInputEvents(tty->hcon, &count);
  if (count > 0) return tty_readc(tty, c);
  return false;
}

// Read from the console input events and push escape codes into the tty cbuffer.
static void tty_waitc_console(tty_t* tty) 
{
  //  wait for a key down event
  INPUT_RECORD inp;
	DWORD count;
  uint32_t surrogate_hi = 0;
  while (true) {
		if (!ReadConsoleInputW( tty->hcon, &inp, 1, &count)) return;
    if (count != 1) return;

    // resize event?
    if (inp.EventType == WINDOW_BUFFER_SIZE_EVENT) {
      tty->term_resize_event = true;
      continue;
    }

    // wait for key down events 
    if (inp.EventType != KEY_EVENT) continue;

    // the modifier state
    DWORD modstate = inp.Event.KeyEvent.dwControlKeyState;
    
    // we need to handle shift up events separately
    if (!inp.Event.KeyEvent.bKeyDown && inp.Event.KeyEvent.wVirtualKeyCode == VK_SHIFT) {
      modstate &= (DWORD)~SHIFT_PRESSED;
    }

    // ignore AltGr
    DWORD altgr = LEFT_CTRL_PRESSED | RIGHT_ALT_PRESSED;
    if ((modstate & altgr) == altgr) { modstate &= ~altgr; }

    
    // get modifiers
    code_t mods = 0;
    if ((modstate & ( RIGHT_CTRL_PRESSED | LEFT_CTRL_PRESSED )) != 0) mods |= KEY_MOD_CTRL;
    if ((modstate & ( RIGHT_ALT_PRESSED | LEFT_ALT_PRESSED )) != 0)   mods |= KEY_MOD_ALT;
    if ((modstate & SHIFT_PRESSED) != 0)                              mods |= KEY_MOD_SHIFT;

    // virtual keys
    uint32_t chr = (uint32_t)inp.Event.KeyEvent.uChar.UnicodeChar;
    WORD     virt = inp.Event.KeyEvent.wVirtualKeyCode;
    debug_msg("tty: console %s: %s%s%s virt 0x%04x, chr 0x%04x ('%c')\n", inp.Event.KeyEvent.bKeyDown ? "down" : "up", mods&KEY_MOD_CTRL ? "ctrl-" : "", mods&KEY_MOD_ALT ? "alt-" : "", mods&KEY_MOD_SHIFT ? "shift-" : "", virt, chr, chr);

    // only process keydown events (except for Alt-up which is used for unicode pasting...)
    if (!inp.Event.KeyEvent.bKeyDown && virt != VK_MENU) {
			continue;
		}
    
    if (chr == 0) { 
      switch (virt) {
        case VK_UP:     tty_cpush_csi_xterm(tty, mods, 'A'); return;
        case VK_DOWN:   tty_cpush_csi_xterm(tty, mods, 'B'); return;
        case VK_RIGHT:  tty_cpush_csi_xterm(tty, mods, 'C'); return;
        case VK_LEFT:   tty_cpush_csi_xterm(tty, mods, 'D'); return;
        case VK_END:    tty_cpush_csi_xterm(tty, mods, 'F'); return; 
        case VK_HOME:   tty_cpush_csi_xterm(tty, mods, 'H'); return;
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
      tty_cpush_csi_unicode(tty,mods,chr);
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

rp_private void tty_start_raw(tty_t* tty) {
  if (tty->raw_enabled) return;
  GetConsoleMode(tty->hcon,&tty->hcon_orig_mode);
  DWORD mode = ENABLE_QUICK_EDIT_MODE   // cut&paste allowed 
             | ENABLE_WINDOW_INPUT      // to catch resize events 
             // | ENABLE_VIRTUAL_TERMINAL_INPUT 
             // | ENABLE_PROCESSED_INPUT
             ;
  SetConsoleMode(tty->hcon, mode );
  tty->raw_enabled = true;  
}

rp_private void tty_end_raw(tty_t* tty) {
  if (!tty->raw_enabled) return;
  SetConsoleMode(tty->hcon, tty->hcon_orig_mode );
  tty->raw_enabled = false;
}

static bool tty_init_raw(tty_t* tty) {
  tty->hcon = GetStdHandle( STD_INPUT_HANDLE );  
  tty->has_term_resize_event = true;
  return true;
}

static void tty_done_raw(tty_t* tty) {
  rp_unused(tty);
}

#endif


