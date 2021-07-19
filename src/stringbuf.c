/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>

#include "common.h"
#include "stringbuf.h"


struct stringbuf_s {
  char*     buf;
  ssize_t   buflen;
  ssize_t   count;  
  alloc_t*  mem;
  bool      is_utf8;
};


//-------------------------------------------------------------
// String navigation 
//-------------------------------------------------------------

// The column with of a codepoint (0, 1, or 2)
static ssize_t char_column_width( const char* s, ssize_t n, bool is_utf8 ) {
  if (s == NULL || n <= 0) return 0;
  else if ((uint8_t)(*s) < ' ') return 0;   // also for CSI escape sequences
  else if (!is_utf8) return 1;
  else {
    ssize_t w = utf8_char_width(s, n);
    return (w <= 0 ? 1 : w); // consoles seem to use at least one column
  }
}

// get offset of the previous codepoint. does not skip back over CSI sequences.
static ssize_t str_prev_ofs( const char* s, ssize_t pos, bool is_utf8, ssize_t* width ) {
  ssize_t ofs = 0;
  if (s != NULL && pos > 0) {
    ofs = 1;
    if (is_utf8) {
      while (pos > ofs) {
        uint8_t u = (uint8_t)s[pos - ofs];
        if (u < 0x80 || u > 0xBF) break;  // continue while follower
        ofs++;
      }
    }
  }
  if (width != NULL) *width = char_column_width( s+(pos-ofs), ofs, is_utf8 );
  return ofs;
}

// skip a CSI sequence
internal bool skip_csi_esc( const char* s, ssize_t len, ssize_t* esclen ) {  
  // <https://en.wikipedia.org/wiki/ANSI_escape_code#CSI_(Control_Sequence_Introducer)_sequences>
  if (esclen != NULL) *esclen = 0;
  if (s == NULL || len < 2|| s[0] != '\x1B' || s[1] != '[') return false;
  ssize_t n = 2;
  bool intermediate = false;
  while( len > n ) {
    char c = s[n];
    if (c >= 0x30 && c <= 0x3F) {       // parameter bytes: 0–9:;<=>?
      if (intermediate) break;          // cannot follow intermediate bytes
      n++;
    }
    else if (c >= 0x20 && c <= 0x2F) {  // intermediate bytes: ' ',!"#$%&'()*+,-./
      intermediate = true;
      n++;
    }
    else if (c >= 0x40 && c <= 0x7E) {  // terminating byte: @A–Z[\]^_`a–z{|}~
      n++;
      if (esclen != NULL) *esclen = n;
      return true;
    }
    else {
      break; // illegal character for an escape sequence.
    }
  }
  return false;
}

// Offset to the next codepoint, treats CSI escape sequences as a single code point.
internal ssize_t str_next_ofs( const char* s, ssize_t len, ssize_t pos, bool is_utf8, ssize_t* cwidth ) {
  ssize_t ofs = 0;
  if (s != NULL && len > pos) {
    if (skip_csi_esc(s+pos,len-pos,&ofs)) {
      // CSI escape sequence      
    }
    else {
      ofs = 1;
      if (is_utf8) {
        // utf8 extended character?
        while(len > pos + ofs) {
          uint8_t u = (uint8_t)s[pos + ofs];
          if (u < 0x80 || u > 0xBF) break;  // break if not a follower
          ofs++;
        }
      }
    } 
  }
  if (cwidth != NULL) *cwidth = char_column_width( s+pos, ofs, is_utf8 );
  return ofs;
}

//-------------------------------------------------------------
// String column width
//-------------------------------------------------------------

static ssize_t str_column_width_n( const char* s, ssize_t len, bool is_utf8 ) {
  if (s == NULL || len <= 0) return 0;
  ssize_t pos = 0;
  ssize_t cwidth = 0;
  ssize_t cw;
  ssize_t ofs;
  while (s[pos] != 0 && (ofs = str_next_ofs(s, len, pos, is_utf8, &cw)) > 0) {
    cwidth += cw;
    pos += ofs;
  }  
  return cwidth;
}

internal ssize_t str_column_width( const char* s, bool is_utf8 ) {
  return str_column_width_n( s, rp_strlen(s), is_utf8 );
}

internal const char* str_skip_until_fit( const char* s, ssize_t max_width, bool is_utf8) {
  if (s == NULL) return s;
  ssize_t cwidth = str_column_width(s, is_utf8);
  ssize_t len    = rp_strlen(s);
  ssize_t pos = 0;
  ssize_t next;
  ssize_t cw;
  while (cwidth > max_width && (next = str_next_ofs(s, len, pos, is_utf8, &cw)) > 0) {
    cwidth -= cw;
    pos += next;
  }
  return (s + pos);
}

static ssize_t str_limit_to_length( const char* s, ssize_t n ) {
  ssize_t i;
  for(i = 0; i < n && s[i] != 0; i++) { /* nothing */ }
  return i;
}


//-------------------------------------------------------------
// String searching prev/next word, line, ws_word
//-------------------------------------------------------------

typedef bool (match_fun_t)(const char* s, ssize_t len);

static ssize_t str_find_backward( const char* s, ssize_t len, ssize_t pos, match_fun_t* match, bool skip_immediate_matches, bool is_utf8 ) {
  if (pos > len) pos = len;
  if (pos < 0) pos = 0;
  ssize_t i = pos;
  // skip matching first (say, whitespace in case of the previous start-of-word)
  if (skip_immediate_matches) {
    do {
      ssize_t prev = str_prev_ofs(s, i, is_utf8, NULL); 
      if (prev <= 0) break;
      assert(i - prev >= 0);
      if (!match(s + i - prev, prev)) break;
      i -= prev;
    } while (i > 0);  
  }
  // find match
  do {
    ssize_t prev = str_prev_ofs(s, i, is_utf8, NULL); 
    if (prev <= 0) break;
    assert(i - prev >= 0);
    if (match(s + i - prev, prev)) {
      return i;  // found;
    }
    i -= prev;
  } while (i > 0);
  return -1; // not found
}

static ssize_t str_find_forward( const char* s, ssize_t len, ssize_t pos, match_fun_t* match, bool skip_immediate_matches, bool is_utf8 ) {
  if (s == NULL || len < 0) return -1;
  if (pos > len) pos = len;
  if (pos < 0) pos = 0;  
  ssize_t i = pos;
  ssize_t next;
  // skip matching first (say, whitespace in case of the next end-of-word)
  if (skip_immediate_matches) {
    do {
      next = str_next_ofs(s, len, i, is_utf8, NULL); 
      if (next <= 0) break;
      assert( i + next <= len);
      if (!match(s + i, next)) break;
      i += next;
    } while (i < len);  
  }
  // and then look
  do {
    next = str_next_ofs(s, len, i, is_utf8, NULL); 
    if (next <= 0) break;
    assert( i + next <= len);
    if (match(s + i, next)) {
      return i; // found
    }
    i += next;
  } while (i < len);
  return -1;
} 

static bool match_linefeed( const char* s, ssize_t n ) {  
  return (n == 1 && (*s == '\n' || *s == 0));
}

static ssize_t str_find_line_start( const char* s, ssize_t len, ssize_t pos, bool is_utf8) {
  ssize_t start = str_find_backward(s,len,pos,&match_linefeed,false /* don't skip immediate matches */, is_utf8);
  return (start < 0 ? 0 : start); 
}

static ssize_t str_find_line_end( const char* s, ssize_t len, ssize_t pos, bool is_utf8) {
  ssize_t end = str_find_forward(s,len,pos, &match_linefeed, false, is_utf8);
  return (end < 0 ? len : end);
}

static bool match_nonletter( const char* s, ssize_t n ) {  
  char c = s[0];
  return !(n > 1 || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_' || c == '-' || c > '~');
}

static ssize_t str_find_word_start( const char* s, ssize_t len, ssize_t pos, bool is_utf8) {
  ssize_t start = str_find_backward(s,len,pos,&match_nonletter,true /* skip immediate matches */, is_utf8);
  return (start < 0 ? 0 : start); 
}

static ssize_t str_find_word_end( const char* s, ssize_t len, ssize_t pos, bool is_utf8) {
  ssize_t end = str_find_forward(s,len,pos,&match_nonletter,true /* skip immediate matches */, is_utf8);
  return (end < 0 ? len : end); 
}

static bool match_whitespace( const char* s, ssize_t n ) {  
  char c = s[0];
  return (n == 1 && (c == ' ' || c == '\t' || c == '\n' || c == '\r'));
}

static ssize_t str_find_ws_word_start( const char* s, ssize_t len, ssize_t pos, bool is_utf8) {
  ssize_t start = str_find_backward(s,len,pos,&match_whitespace,true /* skip immediate matches */, is_utf8);
  return (start < 0 ? 0 : start); 
}

static ssize_t str_find_ws_word_end( const char* s, ssize_t len, ssize_t pos, bool is_utf8) {
  ssize_t end = str_find_forward(s,len,pos,&match_whitespace,true /* skip immediate matches */, is_utf8);
  return (end < 0 ? len : end); 
}


//-------------------------------------------------------------
// String row/column iteration
//-------------------------------------------------------------

// invoke a function for each terminal row; returns total row count.
static ssize_t str_for_each_row( const char* s, ssize_t len, ssize_t termw, ssize_t promptw, 
                                 row_fun_t* fun, bool is_utf8, const void* arg, void* res ) 
{
  if (s == NULL) s = "";
  ssize_t i;
  ssize_t rcount = 0;
  ssize_t rcol = 0;
  ssize_t rstart = 0;  
  for(i = 0; i < len; ) {
    ssize_t w;
    ssize_t next = str_next_ofs(s, len, i, is_utf8, &w);    
    if (next <= 0) {
      debug_msg("str: foreach row: next<=0: len %zd, i %zd, w %zd, buf %s\n", len, i, w, s );
      assert(false);
      break;
    }
    if (termw != 0 && i != 0 && (rcol + w + promptw + 1 /* for the cursor */) > termw) {  
      // wrap
      if (fun != NULL) {
        if (fun(s,rcount,rstart,i - rstart,true,is_utf8,arg,res)) return rcount;
      }
      rcount++;
      rstart = i;
      rcol   = 0;
    }
    if (s[i] == '\n') {
      // newline
      if (fun != NULL) {
        if (fun(s,rcount,rstart,i - rstart,false,is_utf8,arg,res)) return rcount;
      }
      rcount++;
      rstart = i+1;
      rcol = 0;
    }
    assert (s[i] != 0);
    i += next;
    rcol += w;
  }
  if (fun != NULL) {
    if (fun(s,rcount,rstart,i - rstart,false,is_utf8,arg,res)) return rcount;
  }
  return rcount+1;
}

//-------------------------------------------------------------
// String: get row/column position
//-------------------------------------------------------------


static bool str_get_current_pos_iter(
    const char* s,
    ssize_t row, ssize_t row_start, ssize_t row_len, 
    bool is_wrap, bool is_utf8, const void* arg, void* res)
{
  unused(is_wrap);
  rowcol_t* rc = (rowcol_t*)res;
  ssize_t pos = *((ssize_t*)arg);

  if (pos >= row_start && pos <= (row_start + row_len)) {
    // found the cursor row
    rc->row_start = row_start;
    rc->row_len   = row_len;
    rc->row = row;
    rc->col = str_column_width_n( s + row_start, pos - row_start, is_utf8 );
    rc->first_on_row = (pos == row_start);
    //ssize_t adjust = (is_wrap  /* wrap has no newline at end */ || 
    //                 (row_len > 0 && s[row_start + row_len - 1] == 0) /* end of user input */ ? 1 : 0);
    rc->last_on_row  = (pos == row_start + row_len);
    // debug_msg("edit: pos iter%s%s, row %zd, pos: %zd, row_start: %zd, rowlen: %zd\n", in_extra ? " inextra" : "", is_wrap ? " wrap" : "", row, eb->pos, row_start, row_len);
  }  
  return false; // always continue to count all rows
}

static ssize_t str_get_rc_at_pos(const char* s, ssize_t len, ssize_t termw, ssize_t promptw, ssize_t pos, bool is_utf8, rowcol_t* rc) {
  ssize_t rows = str_for_each_row(s, len, termw, promptw, &str_get_current_pos_iter, is_utf8, &pos, rc);
  // debug_msg("edit: current pos: (%d, %d) %s %s\n", rc->row, rc->col, rc->first_on_row ? "first" : "", rc->last_on_row ? "last" : "");
  return rows;
}

//-------------------------------------------------------------
// Set position
//-------------------------------------------------------------

static bool str_set_pos_iter(
    const char* s,
    ssize_t row, ssize_t row_start, ssize_t row_len, 
    bool is_wrap, bool is_utf8, const void* arg, void* res)
{
  unused(arg); unused(is_wrap);
  rowcol_t* rc = (rowcol_t*)arg;
  if (rc->row != row) return false; // keep searching
  // we found our row
  ssize_t col = 0; 
  ssize_t i   = row_start;
  ssize_t end = row_start + row_len;
  while (col < rc->col && i < end) {
    ssize_t cw;
    ssize_t next = str_next_ofs(s, row_start + row_len, i, is_utf8, &cw);
    if (next <= 0) break;
    i   += next;
    col += cw;
  }
  *((ssize_t*)res) = i;
  return true; // stop iteration
}

static ssize_t str_get_pos_at_rc(const char* s, ssize_t len, ssize_t termw, ssize_t promptw, ssize_t row, ssize_t col /* without prompt */, bool is_utf8) {
  rowcol_t rc;
  memset(&rc,0,ssizeof(rc));
  rc.row = row;
  rc.col = col;
  ssize_t pos = -1;
  str_for_each_row(s,len,termw,promptw,&str_set_pos_iter,is_utf8,&rc,&pos);  
  return pos;
}


//-------------------------------------------------------------
// String buffer
//-------------------------------------------------------------

static void sbuf_init( stringbuf_t* sbuf, alloc_t* mem, bool is_utf8 ) {
  sbuf->mem = mem;
  sbuf->buf = NULL;
  sbuf->buflen = 0;
  sbuf->count = 0;
  sbuf->is_utf8 = is_utf8;
}

static void sbuf_done( stringbuf_t* sbuf ) {
  mem_free( sbuf->mem, sbuf->buf );
  sbuf->buf = NULL;
  sbuf->buflen = 0;
  sbuf->count = 0;
}

internal stringbuf_t*  sbuf_new( alloc_t* mem, bool is_utf8 ) {
  stringbuf_t* sbuf = mem_zalloc_tp(mem,stringbuf_t);
  if (sbuf == NULL) return NULL;
  sbuf_init(sbuf,mem,is_utf8);
  return sbuf;
}

internal void sbuf_free( stringbuf_t* sbuf ) {
  if (sbuf==NULL) return;
  sbuf_done(sbuf);
  mem_free(sbuf->mem, sbuf);
}

internal char* sbuf_free_dup(stringbuf_t* sbuf) {
  if (sbuf == NULL) return NULL;
  char* s = (char*)mem_realloc(sbuf->mem, sbuf->buf, sbuf_len(sbuf)+1);
  mem_free(sbuf->mem, sbuf);
  return s;
}

internal const char* sbuf_string_at( stringbuf_t* sbuf, ssize_t pos ) {
  if (sbuf->buf == NULL || pos < 0 || sbuf->count < pos) return NULL;
  assert(sbuf->buf[sbuf->count] == 0);
  return sbuf->buf + pos;
}

internal const char* sbuf_string( stringbuf_t* sbuf ) {
  return sbuf_string_at( sbuf, 0 );
}

internal char sbuf_char_at(stringbuf_t* sbuf, ssize_t pos) {
  if (sbuf->buf == NULL || pos < 0 || sbuf->count < pos) return 0;
  return sbuf->buf[pos];
}

internal char* sbuf_strdup_at( stringbuf_t* sbuf, ssize_t pos ) {
  return mem_strdup(sbuf->mem, sbuf_string_at(sbuf,pos));
}

internal char* sbuf_strdup( stringbuf_t* sbuf ) {
  return mem_strdup(sbuf->mem, sbuf_string(sbuf));
}

static bool sbuf_ensure_extra(stringbuf_t* s, ssize_t extra) 
{
  if (s->buflen >= s->count + extra) return true;   
  // reallocate
  ssize_t newlen = (s->buflen == 0 ? 124 : 2*s->buflen);
  if (newlen <= s->count + extra) newlen = s->count + extra;
  debug_msg("stringbuf: reallocate: old %zd, new %zd\n", s->buflen, newlen);
  char* newbuf = (char*)mem_realloc(s->mem, s->buf, newlen+1);
  if (newbuf == NULL) {
    assert(false);
    return false;
  }
  s->buf = newbuf;
  s->buflen = newlen;
  s->buf[s->count] = s->buf[s->buflen] = 0;
  assert(s->buflen >= s->count + extra);
  return true;
}

internal ssize_t sbuf_len(const stringbuf_t* s) {
  return s->count;
}

internal ssize_t sbuf_insert_at_n(stringbuf_t* sbuf, const char* s, ssize_t n, ssize_t pos ) {
  if (pos < 0 || pos > sbuf->count || s == NULL) return pos;
  n = str_limit_to_length(s,n);
  if (n <= 0 || !sbuf_ensure_extra(sbuf,n)) return pos;
  rp_memmove(sbuf->buf + pos + n, sbuf->buf + pos, sbuf->count - pos);
  rp_memcpy(sbuf->buf + pos, s, n);
  sbuf->count += n;
  sbuf->buf[sbuf->count] = 0;
  return (pos + n);
}

internal ssize_t sbuf_insert_at(stringbuf_t* sbuf, const char* s, ssize_t pos ) {
  return sbuf_insert_at_n( sbuf, s, rp_strlen(s), pos );
}

internal void sbuf_delete_at( stringbuf_t* sbuf, ssize_t pos, ssize_t count ) {
  if (pos < 0 || pos >= sbuf->count) return;
  if (pos + count > sbuf->count) count = sbuf->count - pos;
  rp_memmove(sbuf->buf + pos, sbuf->buf + pos + count, sbuf->count - pos - count);
  sbuf->count -= count;
  sbuf->buf[sbuf->count] = 0;
}

internal void sbuf_delete_from_to( stringbuf_t* sbuf, ssize_t pos, ssize_t end ) {
  if (end <= pos) return;
  sbuf_delete_at( sbuf, pos, end - pos);
}


internal void sbuf_clear( stringbuf_t* sbuf ) {
  sbuf_delete_at(sbuf, 0, sbuf_len(sbuf));
}

internal ssize_t sbuf_append_n( stringbuf_t* sbuf, const char* s, ssize_t n ) {
  return sbuf_insert_at_n( sbuf, s, n, sbuf_len(sbuf));
}

internal ssize_t sbuf_append( stringbuf_t* sbuf, const char* s ) {
  return sbuf_insert_at( sbuf, s, sbuf_len(sbuf));
}

internal ssize_t sbuf_append_char( stringbuf_t* sbuf, char c ) {
  char buf[2];
  buf[0] = c;
  buf[1] = 0;
  return sbuf_append( sbuf, buf );
}

internal void sbuf_replace(stringbuf_t* sbuf, const char* s) {
  sbuf_clear(sbuf);
  sbuf_append(sbuf,s);
}

static ssize_t sbuf_next_ofs( stringbuf_t* sbuf, ssize_t pos, ssize_t* cwidth ) {
  return str_next_ofs( sbuf->buf, sbuf->count, pos, sbuf->is_utf8, cwidth);
}

static ssize_t sbuf_prev_ofs( stringbuf_t* sbuf, ssize_t pos, ssize_t* cwidth ) {
  return str_prev_ofs( sbuf->buf, pos, sbuf->is_utf8, cwidth);
}

internal ssize_t sbuf_next( stringbuf_t* sbuf, ssize_t pos, ssize_t* cwidth) {
  ssize_t ofs = sbuf_next_ofs(sbuf,pos,cwidth);
  if (ofs <= 0) return -1;
  assert(pos + ofs <= sbuf->count);
  return pos + ofs; 
}

internal ssize_t sbuf_prev( stringbuf_t* sbuf, ssize_t pos, ssize_t* cwidth) {
  ssize_t ofs = sbuf_prev_ofs(sbuf,pos,cwidth);
  if (ofs <= 0) return -1;
  assert(pos - ofs >= 0);
  return pos - ofs;
}

internal ssize_t sbuf_delete_char_before( stringbuf_t* sbuf, ssize_t pos ) {
  ssize_t n = sbuf_prev_ofs(sbuf, pos, NULL);
  if (n <= 0) return 0;  
  assert( pos - n >= 0 );
  sbuf_delete_at(sbuf, pos - n, n);
  return pos - n;
}

internal void sbuf_delete_char_at( stringbuf_t* sbuf, ssize_t pos ) {
  ssize_t n = sbuf_next_ofs(sbuf, pos, NULL);
  if (n <= 0) return;  
  assert( pos + n <= sbuf->count );
  sbuf_delete_at(sbuf, pos, n);
  return;
}

internal ssize_t sbuf_swap_char( stringbuf_t* sbuf, ssize_t pos ) {
  ssize_t next = sbuf_next_ofs(sbuf, pos, NULL);
  if (next <= 0) return 0;  
  ssize_t prev = sbuf_prev_ofs(sbuf, pos, NULL);
  if (prev <= 0) return 0;  
  char buf[64];
  if (prev >= 63) return 0;
  rp_memcpy(buf, sbuf->buf + pos - prev, prev );
  rp_memmove(sbuf->buf + pos - prev, sbuf->buf + pos, next);
  rp_memmove(sbuf->buf + pos - prev + next, buf, prev);
  return pos - prev;
}

internal ssize_t sbuf_find_line_start( stringbuf_t* sbuf, ssize_t pos ) {
  return str_find_line_start( sbuf->buf, sbuf->count, pos, sbuf->is_utf8);
}

internal ssize_t sbuf_find_line_end( stringbuf_t* sbuf, ssize_t pos ) {
  return str_find_line_end( sbuf->buf, sbuf->count, pos, sbuf->is_utf8);
}

internal ssize_t sbuf_find_word_start( stringbuf_t* sbuf, ssize_t pos ) {
  return str_find_word_start( sbuf->buf, sbuf->count, pos, sbuf->is_utf8);
}

internal ssize_t sbuf_find_word_end( stringbuf_t* sbuf, ssize_t pos ) {
  return str_find_word_end( sbuf->buf, sbuf->count, pos, sbuf->is_utf8);
}

internal ssize_t sbuf_find_ws_word_start( stringbuf_t* sbuf, ssize_t pos ) {
  return str_find_ws_word_start( sbuf->buf, sbuf->count, pos, sbuf->is_utf8);
}

internal ssize_t sbuf_find_ws_word_end( stringbuf_t* sbuf, ssize_t pos ) {
  return str_find_ws_word_end( sbuf->buf, sbuf->count, pos, sbuf->is_utf8);
}

// find row/col position
internal ssize_t sbuf_get_pos_at_rc( stringbuf_t* sbuf, ssize_t termw, ssize_t promptw, ssize_t row, ssize_t col ) {
  return str_get_pos_at_rc( sbuf->buf, sbuf->count, termw, promptw, row, col, sbuf->is_utf8);
}

// get row/col for a given position
internal ssize_t sbuf_get_rc_at_pos( stringbuf_t* sbuf, ssize_t termw, ssize_t promptw, ssize_t pos, rowcol_t* rc ) {
  return str_get_rc_at_pos( sbuf->buf, sbuf->count, termw, promptw, pos, sbuf->is_utf8, rc);
}

internal ssize_t sbuf_for_each_row( stringbuf_t* sbuf, ssize_t termw, ssize_t promptw, row_fun_t* fun, void* arg, void* res ) {
  return str_for_each_row( sbuf->buf, sbuf->count, termw, promptw, fun, sbuf->is_utf8, arg, res);
}
