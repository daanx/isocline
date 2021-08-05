/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/

#include <string.h>
#include "common.h"
#include "term.h"
#include "stringbuf.h"

//-------------------------------------------------------------
// Syntax highlighting
//-------------------------------------------------------------

// text attributes.
// (zero values keep the current attribute)
typedef struct attr_s {
  int8_t     underline;       // -1 = off, 0 = keep as is, 1 = on
  int8_t     reverse;         // -1 = off, 0 = keep as is, 1 = on
  ic_color_t color;           
  ic_color_t bgcolor;
} attr_t;

// static const attr_t attr_zero    = { 0, 0, IC_COLOR_NONE, IC_COLOR_NONE };
static const attr_t attr_default = { -1, -1, IC_ANSI_DEFAULT, IC_ANSI_DEFAULT };

struct ic_highlight_env_s {
  attr_t*  attrs;
  ssize_t  attr_capacity;  
  ssize_t  attr_len;
  attr_t   cur_attr;
  const char* input;        // only valid during initial highlight  
  alloc_t* mem;
};


//-------------------------------------------------------------
// Private interface
//-------------------------------------------------------------

ic_private ic_highlight_env_t* highlight_new( alloc_t* mem ) {
  ic_highlight_env_t* henv = mem_zalloc_tp( mem, ic_highlight_env_t );
  if (henv == NULL) return NULL;
  henv->mem = mem;
  return henv;
}

ic_private void highlight_free( ic_highlight_env_t* henv ) {
  if (henv == NULL) return;
  mem_free( henv->mem, henv->attrs );
  mem_free( henv->mem, henv );
}

static bool highlight_ensure_extra( ic_highlight_env_t* henv, ssize_t extra ) {
  if (henv==NULL) return false;
  ssize_t needed = henv->attr_len + extra;
  if (henv->attr_capacity < needed) {
    ssize_t capacity = (henv->attr_capacity <= 0 ? 128 : 2*henv->attr_capacity);
    if (capacity < needed) { capacity = needed; }
    attr_t* newattrs = mem_realloc_tp( henv->mem, attr_t, henv->attrs, capacity );
    if (newattrs == NULL) return false;
    henv->attrs = newattrs;
    henv->attr_capacity = capacity;
  }
  assert(henv->attr_capacity >= needed);
  return true;
}

ic_private bool highlight_insert_at( ic_highlight_env_t* henv, ssize_t pos, ssize_t len, ic_color_t color ) {
  if (henv == NULL) return false;
  if (pos < 0 || pos > henv->attr_len) return false;
  if (!highlight_ensure_extra(henv,len)) return false;
  ic_memmove(henv->attrs + pos + len, henv->attrs + pos, ssizeof(attr_t)*(henv->attr_len - pos));
  henv->attr_len += len;
  attr_t attr = attr_default;
  attr.color = color;
  for (ssize_t i = 0; i < len; i++) {
    henv->attrs[pos + i] = attr;
  }
  return true;
}

ic_private void highlight_clear( ic_highlight_env_t* henv ) {
  if (henv == NULL) return;
  henv->attr_len = 0;
  henv->cur_attr = attr_default;
}

static void highlight_fillout( ic_highlight_env_t* henv ) {
  if (henv == NULL) return;
  attr_t attr = attr_default;
  for( ssize_t i = 0; i < henv->attr_len; i++ ) {
    attr_t* cur = &henv->attrs[i];
    // propagate attribute
    if (cur->color     == 0) { cur->color = attr.color; }
    if (cur->bgcolor   == 0) { cur->bgcolor = attr.bgcolor; }
    if (cur->underline == 0) { cur->underline = attr.underline; }
    if (cur->reverse   == 0) { cur->reverse = attr.reverse; }
    attr = *cur;
  }
}

ic_private bool highlight_init( ic_highlight_env_t* henv, const char* s, ic_highlight_fun_t* highlighter, void* arg ) {
  if (henv == NULL) return false;
  highlight_clear(henv);
  const ssize_t len = ic_strlen(s);
  if (len > 0) {
    if (!highlight_ensure_extra(henv,len)) return false;
    henv->attr_len = len;
    ic_memset(henv->attrs, 0, henv->attr_len * ssizeof(attr_t));
    if (highlighter != NULL) {
      henv->input = s;
      (*highlighter)( henv, s, arg );
      henv->input = NULL;
    }
  }
  highlight_fillout(henv);
  return true;
}

//-------------------------------------------------------------
// Writing to the terminal
//-------------------------------------------------------------

static void term_update_attr(term_t* term, attr_t term_attr, attr_t new_attr, attr_t* cur_attr) {
  // update current attribute
  if (new_attr.color != 0 && cur_attr->color != new_attr.color) {
    cur_attr->color = new_attr.color;
  }
  if (new_attr.bgcolor != 0 && cur_attr->bgcolor != new_attr.bgcolor) {
    cur_attr->bgcolor = new_attr.bgcolor;
  }
  if (new_attr.reverse != 0 && cur_attr->reverse != new_attr.reverse) {
    cur_attr->reverse = new_attr.reverse;
  }
  if (new_attr.underline != 0 && cur_attr->underline != new_attr.underline) {
    cur_attr->underline = new_attr.underline;
  }
  // and write out escape sequences for any changes
  if (term_attr.color != cur_attr->color) {
    term_color(term, cur_attr->color);
  }
  if (term_attr.bgcolor != cur_attr->bgcolor) {
    term_bgcolor(term, cur_attr->bgcolor);
  }
  if (term_attr.underline != cur_attr->underline) {
    term_underline(term, cur_attr->underline > 0);
  }
  if (term_attr.reverse != cur_attr->reverse) {
    term_reverse(term, cur_attr->reverse > 0);
  }
}

ic_private void highlight_term_write( ic_highlight_env_t* henv, term_t* term, const char* s, ssize_t start, ssize_t len ) 
{
  if (henv == NULL || henv->attr_len <= 0) {
    term_write_n( term, s + start, len );    
  }
  else {
    attr_t term_attr = attr_default;
    for (ssize_t i = start; i < start + len; i++) {
      char c = s[i];
      if (i >= 0 && i < henv->attr_len && !utf8_is_cont((uint8_t)c)) {
        term_update_attr(term, term_attr, henv->attrs[i], &henv->cur_attr);
        term_attr = henv->cur_attr;
      }
      term_write_char(term, c);
    }    
  }
  term_attr_reset(term);
}


//-------------------------------------------------------------
// Client interface
//-------------------------------------------------------------

static long pos_adjust( ic_highlight_env_t* henv, long pos ) {
  if (pos >= henv->attr_len) return -1;
  if (pos >= 0) return pos;
  // negative position is used as the unicode character position (for easy interfacing with Haskell)
  if (henv->input == NULL) return -1;
  long ucount = -pos;
  long cpos = 0;
  long upos = 0;
  while ( upos < ucount ) {
    ssize_t next = str_next_ofs(henv->input, henv->attr_len, cpos, NULL);
    if (next <= 0) return -1;
    upos++;
    cpos += (long)next;
  }
  // assert(cpos < henv->attr_len);
  return cpos;
}

// Set the color of characters starting at position `pos` to `color`.
ic_public void ic_highlight_color(ic_highlight_env_t* henv, long pos, ic_color_t color ) {
  if (henv==NULL) return;
  pos = pos_adjust(henv,pos);
  if (pos < 0) return;
  henv->attrs[pos].color = color;
}

// Set the background color of characters starting at position `pos` to `bgcolor`.
ic_public void ic_highlight_bgcolor(ic_highlight_env_t* henv, long pos, ic_color_t bgcolor) {
  if (henv==NULL) return;
  pos = pos_adjust(henv,pos);
  if (pos < 0) return;
  henv->attrs[pos].bgcolor = bgcolor;
}

// Enable/Disable underlining for characters starting at position `pos`.
ic_public void ic_highlight_underline(ic_highlight_env_t* henv, long pos, bool enable ) {
  if (henv==NULL) return;
  pos = pos_adjust(henv,pos);
  if (pos < 0) return;
  henv->attrs[pos].underline = (enable ? 1 : -1);
}

// Enable/Disable reverse video for characters starting at position `pos`.
ic_public void ic_highlight_reverse(ic_highlight_env_t* henv, long pos, bool enable) {
  if (henv==NULL) return;
  pos = pos_adjust(henv,pos);
  if (pos < 0) return;
  henv->attrs[pos].underline = (enable ? 1 : -1);
}


// Convenience function for highlighting with escape sequences.
ic_public void ic_highlight_esc(ic_highlight_env_t* henv, const char* input, ic_highlight_esc_fun_t* highlight, void* arg) {
  if (henv == NULL || henv->attr_len <= 0) return;
  if (input == NULL || highlight == NULL)  return;
  char* s = (*highlight)(input, arg);
  if (s == NULL) return;
  // go through `s` and simulate ansi color escape sequences
  ssize_t len = ic_strlen(s);
  ssize_t i = 0;    // position in highlighted input
  long    pos = 0;  // position in original input
  while( i < len) {
    ssize_t next = str_next_ofs(s, len, i, NULL);
    if (next <= 0) break;
    if (s[i] == input[pos]) {
      // matches with original input
      pos += (long)next;      
    }
    else {
      // new escaped input
      ssize_t code = 0;
      if (s[i] == '\x1B' && s[i+1] == '[' && s[i+next-1] == 'm' && ic_atoz(s+i+2,&code)) {
        // CSI escape
        if (code == 4) {
          ic_highlight_underline(henv, pos, true);
        }
        else if (code == 24) {
          ic_highlight_underline(henv, pos, false);
        }
        else if (code == 7) {
          ic_highlight_reverse(henv, pos, true);
        }
        else if (code == 27) {
          ic_highlight_reverse(henv, pos, false);
        }
        else if ((code >= 30 && code <= 37) || code == 39 || (code >= 90 && code <= 97)) {
          ic_highlight_color(henv, pos, (ic_color_t)code);
        }
        else if ((code >= 40 && code <= 47) || code == 49 || (code >= 100 && code <= 107)) {
          ic_highlight_bgcolor(henv, pos, (ic_color_t)(code - 10));
        }
        else if (code == 0) {
          ic_highlight_color(henv, pos, IC_ANSI_DEFAULT);
          ic_highlight_bgcolor(henv, pos, IC_ANSI_DEFAULT);
          ic_highlight_underline(henv, pos, false);
          ic_highlight_reverse(henv, pos, false);
        }
      }
    }
    i += next;
  }
  mem_free(henv->mem, s);
}


//-------------------------------------------------------------
// Brace matching
//-------------------------------------------------------------
#define MAX_NESTING (64)

typedef struct brace_s {
  char close;
  bool show_match;
  long pos;
} brace_t;

ic_private void highlight_match_braces(ic_highlight_env_t* henv, const char* s, ssize_t cursor_pos, const char* braces, ic_color_t match_color, ic_color_t error_color) 
{
  brace_t open[MAX_NESTING+1];
  ssize_t nesting = 0;
  const ssize_t brace_len = ic_strlen(braces);
  for (long i = 0; i < ic_strlen(s); i++) {
    const char c = s[i];
    // push open brace
    bool found_open = false;
    for (ssize_t b = 0; b < brace_len; b += 2) {
      if (c == braces[b]) {
        // open brace
        if (nesting >= MAX_NESTING) return; // give up
        open[nesting].close = braces[b+1];
        open[nesting].pos = i;
        open[nesting].show_match = (i == cursor_pos - 1);
        nesting++;
        found_open = true;
        break;
      }
    }
    if (found_open) continue;

    // pop to closing brace and potentially highlight
    for (ssize_t b = 1; b < brace_len; b += 2) {
      if (c == braces[b]) {
        // close brace
        if (nesting <= 0) {
          // unmatched close brace
          ic_highlight_color(henv, i, error_color);
        }
        else {
          // can we fix an unmatched brace where we can match by popping just one?
          if (open[nesting-1].close != c && nesting > 1 && open[nesting-2].close == c) {
            // assume previous open brace was wrong
            ic_highlight_color(henv, open[nesting-1].pos, error_color);
            nesting--;
          }
          if (open[nesting-1].close != c) {
            // unmatched open brace
            ic_highlight_color(henv, i, error_color);
          }
          else {
            // matching brace
            nesting--;
            if (i == cursor_pos - 1 || open[nesting].show_match) {
              // highlight matching brace
              ic_highlight_color(henv, open[nesting].pos, match_color);
              ic_highlight_color(henv, i, match_color);
            }
          }
        }
        break;
      }
    }
  }
};