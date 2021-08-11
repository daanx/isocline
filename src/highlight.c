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
#include "attr.h"

//-------------------------------------------------------------
// Syntax highlighting
//-------------------------------------------------------------

struct ic_highlight_env_s {
  attrbuf_t*    attrs;
  const char*   input;   
  ssize_t       input_len;     
};


ic_private void highlight( const char* s, attrbuf_t* attrs, ic_highlight_fun_t* highlighter, void* arg ) {
  const ssize_t len = ic_strlen(s);
  if (len <= 0) return;
  attrbuf_set_at(attrs,0,len,attr_none()); // fill to length of s
  if (highlighter != NULL) {
    ic_highlight_env_t henv;
    henv.attrs = attrs;
    henv.input = s;     
    henv.input_len = len;
    (*highlighter)( &henv, s, arg );    
  }
}


//-------------------------------------------------------------
// Client interface
//-------------------------------------------------------------

static long pos_adjust( ic_highlight_env_t* henv, long pos ) {
  if (pos >= henv->input_len) return -1;
  if (pos >= 0) return pos;
  if (henv->input == NULL) return -1;
  // negative `pos` is used as the unicode character position (for easy interfacing with Haskell)
  long ucount = -pos;
  long cpos = 0;
  long upos = 0;
  while ( upos < ucount ) {
    ssize_t next = str_next_ofs(henv->input, henv->input_len, cpos, NULL);
    if (next <= 0) return -1;
    upos++;
    cpos += (long)next;
  }
  // assert(cpos < henv->attr_len);
  return cpos;
}

static void highlight_attr(ic_highlight_env_t* henv, long pos, long count, attr_t attr ) {
  if (henv==NULL) return;
  pos = pos_adjust(henv,pos);
  if (pos < 0 || count <= 0) return;
  attrbuf_update_at(henv->attrs, pos, count, attr);
}

// Set the color of characters starting at position `pos` to `color`.
ic_public void ic_highlight_color(ic_highlight_env_t* henv, long pos, long count, ic_color_t color ) {
  attr_t attr = attr_none();
  attr.x.color = color;
  highlight_attr(henv,pos,count,attr);
}
  

// Set the background color of characters starting at position `pos` to `bgcolor`.
ic_public void ic_highlight_bgcolor(ic_highlight_env_t* henv, long pos, long count, ic_color_t bgcolor) {
  attr_t attr = attr_none();
  attr.x.bgcolor = bgcolor;
  highlight_attr(henv,pos,count,attr);
}

// Enable/Disable underlining for characters starting at position `pos`.
ic_public void ic_highlight_underline(ic_highlight_env_t* henv, long pos, long count, bool enable ) {
  attr_t attr = attr_none();
  attr.x.underline = (enable ? IC_ON : IC_OFF);
  highlight_attr(henv,pos,count,attr);  
}

// Enable/Disable reverse video for characters starting at position `pos`.
ic_public void ic_highlight_reverse(ic_highlight_env_t* henv, long pos, long count, bool enable) {
  attr_t attr = attr_none();
  attr.x.reverse = (enable ? IC_ON : IC_OFF);
  highlight_attr(henv,pos,count,attr);
}

// Enable/Disable bold for characters starting at position `pos`.
ic_public void ic_highlight_bold(ic_highlight_env_t* henv, long pos, long count, bool enable) {
  attr_t attr = attr_none();
  attr.x.bold = (enable ? IC_ON : IC_OFF);
  highlight_attr(henv,pos,count,attr);
}

// Enable/Disable italic for characters starting at position `pos`.
ic_public void ic_highlight_italic(ic_highlight_env_t* henv, long pos, long count, bool enable) {
  attr_t attr = attr_none();
  attr.x.italic = (enable ? IC_ON : IC_OFF);
  highlight_attr(henv,pos,count,attr);
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

ic_private void highlight_match_braces(const char* s, attrbuf_t* attrs, ssize_t cursor_pos, const char* braces, attr_t match_attr, attr_t error_attr) 
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
          attrbuf_update_at( attrs, i, 1, error_attr);
        }
        else {
          // can we fix an unmatched brace where we can match by popping just one?
          if (open[nesting-1].close != c && nesting > 1 && open[nesting-2].close == c) {
            // assume previous open brace was wrong
            attrbuf_update_at(attrs, open[nesting-1].pos, 1, error_attr);
            nesting--;
          }
          if (open[nesting-1].close != c) {
            // unmatched open brace
            attrbuf_update_at( attrs, i, 1, error_attr);
          }
          else {
            // matching brace
            nesting--;
            if (i == cursor_pos - 1 || (open[nesting].at_cursor && open[nesting].pos != i - 1)) {
              // highlight matching brace
              attrbuf_update_at(attrs, open[nesting].pos, 1, match_attr);
              attrbuf_update_at(attrs, i, 1, match_attr);              
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
