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

struct ic_highlight_env_s {
  term_attr_t*  attrs;
  ssize_t       attr_capacity;  
  ssize_t       attr_len;  
  const char*   input;        // only valid during initial highlight  
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
    term_attr_t* newattrs = mem_realloc_tp( henv->mem, term_attr_t, henv->attrs, capacity );
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
  ic_memmove(henv->attrs + pos + len, henv->attrs + pos, ssizeof(term_attr_t)*(henv->attr_len - pos));
  henv->attr_len += len;
  term_attr_t attr = term_attr_none();
  attr.x.color = color;
  for (ssize_t i = 0; i < len; i++) {
    henv->attrs[pos + i] = attr;
  }
  return true;
}

ic_private void highlight_clear( ic_highlight_env_t* henv ) {
  if (henv == NULL) return;
  henv->attr_len = 0;
}

static void highlight_fillout( ic_highlight_env_t* henv ) {
  if (henv == NULL) return;
  term_attr_t attr = term_attr_default();
  for( ssize_t i = 0; i < henv->attr_len; i++ ) {
    term_attr_t* cur = &henv->attrs[i];
    // propagate attribute
    if (cur->x.color     == IC_COLOR_NONE) { cur->x.color = attr.x.color; }
    if (cur->x.bgcolor   == IC_COLOR_NONE) { cur->x.bgcolor = attr.x.bgcolor; }
    if (cur->x.underline == IC_NONE) { cur->x.underline = attr.x.underline; }
    if (cur->x.reverse   == IC_NONE) { cur->x.reverse = attr.x.reverse; }
    if (cur->x.bold      == IC_NONE) { cur->x.bold = attr.x.bold; }
    if (cur->x.italic    == IC_NONE) { cur->x.italic = attr.x.italic; }
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
    ic_memset(henv->attrs, 0, henv->attr_len * ssizeof(term_attr_t));
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

ic_private void highlight_term_write( ic_highlight_env_t* henv, term_t* term, const char* s, ssize_t start, ssize_t len ) 
{
  if (henv == NULL || henv->attr_len <= 0) {
    term_write_n( term, s + start, len );    
  }
  else {
    for (ssize_t i = start; i < start + len; i++) {
      char c = s[i];
      if (i >= 0 && i < henv->attr_len && !utf8_is_cont((uint8_t)c)) {
        term_set_attr(term, henv->attrs[i]);
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
  henv->attrs[pos].x.color = color;
}

// Set the background color of characters starting at position `pos` to `bgcolor`.
ic_public void ic_highlight_bgcolor(ic_highlight_env_t* henv, long pos, ic_color_t bgcolor) {
  if (henv==NULL) return;
  pos = pos_adjust(henv,pos);
  if (pos < 0) return;
  henv->attrs[pos].x.bgcolor = bgcolor;
}

// Enable/Disable underlining for characters starting at position `pos`.
ic_public void ic_highlight_underline(ic_highlight_env_t* henv, long pos, bool enable ) {
  if (henv==NULL) return;
  pos = pos_adjust(henv,pos);
  if (pos < 0) return;
  henv->attrs[pos].x.underline = (enable ? 1 : -1);
}

// Enable/Disable reverse video for characters starting at position `pos`.
ic_public void ic_highlight_reverse(ic_highlight_env_t* henv, long pos, bool enable) {
  if (henv==NULL) return;
  pos = pos_adjust(henv,pos);
  if (pos < 0) return;
  henv->attrs[pos].x.underline = (enable ? 1 : -1);
}

// Enable/Disable bold for characters starting at position `pos`.
ic_public void ic_highlight_bold(ic_highlight_env_t* henv, long pos, bool enable) {
  if (henv==NULL) return;
  pos = pos_adjust(henv,pos);
  if (pos < 0) return;
  henv->attrs[pos].x.bold = (enable ? 1 : -1);
}

// Enable/Disable italic for characters starting at position `pos`.
ic_public void ic_highlight_italic(ic_highlight_env_t* henv, long pos, bool enable) {
  if (henv==NULL) return;
  pos = pos_adjust(henv,pos);
  if (pos < 0) return;
  henv->attrs[pos].x.italic = (enable ? 1 : -1);
}


//-------------------------------------------------------------
// Brace matching
//-------------------------------------------------------------
#define MAX_NESTING (64)

typedef struct brace_s {
  char close;
  bool at_cursor;
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
        open[nesting].at_cursor = (i == cursor_pos - 1);
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
            if (i == cursor_pos - 1 || (open[nesting].at_cursor && open[nesting].pos != i - 1)) {
              // highlight matching brace
              ic_highlight_color(henv, open[nesting].pos, match_color);
              ic_highlight_color(henv, i, match_color);
              //ic_highlight_bold(henv, open[nesting].pos, true);
              //ic_highlight_bold(henv, i, true);
            }
          }
        }
        break;
      }
    }
  }
  // note: don't mark further unmatched open braces as in error
}


ic_private ssize_t find_matching_brace(const char* s, ssize_t cursor_pos, const char* braces, bool* is_balanced) 
{
  if (is_balanced != NULL) { *is_balanced = false; }
  bool balanced = true;
  ssize_t match = -1;
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
        if (nesting >= MAX_NESTING) return -1; // give up
        open[nesting].close = braces[b+1];
        open[nesting].pos = i;
        open[nesting].at_cursor = (i == cursor_pos - 1);
        nesting++;
        found_open = true;
        break;
      }
    }
    if (found_open) continue;

    // pop to closing brace 
    for (ssize_t b = 1; b < brace_len; b += 2) {
      if (c == braces[b]) {
        // close brace
        if (nesting <= 0) {
          // unmatched close brace
          balanced = false;          
        }
        else {
          if (open[nesting-1].close != c) {
            // unmatched open brace
            balanced = false;
          }
          else {
            // matching brace
            nesting--;
            if (i == cursor_pos - 1) {
              // found matching open brace
              match = open[nesting].pos + 1;
            }
            else if (open[nesting].at_cursor) {
              // found matching close brace
              match = i + 1;
            }
          }
        }
        break; 
      }
    }
  }
  if (nesting != 0) { balanced = false; }
  if (is_balanced != NULL) { *is_balanced = balanced; }
  return match;
}
