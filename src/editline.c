/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>

#include "common.h"
#include "term.h"
#include "tty.h"
#include "env.h"

#if defined(_WIN32)
#else
#include <unistd.h>
#endif

typedef struct editbuf_s {
  char*   buf;
  ssize_t buflen;
  ssize_t len;
  ssize_t pos;
  ssize_t prev_rows;
  ssize_t prev_row;
  bool    modified;
  bool    is_utf8;
  const char* prompt;
  ssize_t prompt_width;
  int     history_idx;
} editbuf_t;




//-------------------------------------------------------------
// Main edit line 
//-------------------------------------------------------------
static char* edit_line( rp_env_t* env, const char* prompt );
static void edit_refresh(rp_env_t* env, editbuf_t* eb);

internal char* rp_editline(rp_env_t* env, const char* prompt) {
  tty_start_raw(&env->tty);
  term_start_raw(&env->term);
  char* line = edit_line(env,prompt);
  term_end_raw(&env->term);
  tty_end_raw(&env->tty);
  term_write(&env->term,"\r\n");
  return line;
}


//-------------------------------------------------------------
// Edit buffer
//-------------------------------------------------------------

static ssize_t editbuf_cwidth( editbuf_t* eb, const char* s, ssize_t n ) {
  if (s == NULL || n <= 0) return 0;
  else if ((uint8_t)(*s) < ' ') return 0;  
  else if (!eb->is_utf8) return 1;
  else return utf8_width(s,n); // todo: use wcwidth
}

static ssize_t editbuf_previous_ofs( editbuf_t* eb, const char* s, ssize_t pos, ssize_t* width ) {
  ssize_t n = 0;
  if (pos > n) {
    n = 1;
    if (eb->is_utf8) {
      while (pos > n) {
        uint8_t u = (uint8_t)s[pos - n];
        if (u < 0x80 || u > 0xBF) break;  // continue while follower
        n++;
      }
    }
  }
  if (width != NULL) *width = editbuf_cwidth( eb, s+(pos-n), n );
  return n;
}

internal bool skip_csi_esc( const char* s, ssize_t len, ssize_t* esclen ) {
  if (esclen != NULL) *esclen = 0;
  if (s == NULL || len < 2|| s[0] != '\x1B' || s[1] != '[') return false;
  ssize_t n = 2;
  bool intermediate = false;
  while( len > n ) {
    //char buf[32]; strncpy(buf,s,(31 > len ? len : 31));
    //debug_msg("skip esc: len %zd, n %zd, %s\n", len, n, buf );
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

internal ssize_t skip_next_code( const char* s, ssize_t len, ssize_t pos, bool utf8 ) {
  ssize_t n = 0;
  if (len > pos) {
    if (skip_csi_esc(s+pos,len-pos,&n)) {
      // CSI escape sequence      
    }
    else {
      n = 1;
      if (utf8) {
        // utf8 extended character
        while(len > pos + n) {
          uint8_t u = (uint8_t)s[pos + n];
          if (u < 0x80 || u > 0xBF) break;  // break if not a follower
          n++;
        }
      }
    } 
  }
  return n;
}

static ssize_t editbuf_next_ofs( editbuf_t* eb, const char* s, ssize_t len, ssize_t pos, ssize_t* width ) {
  ssize_t n = skip_next_code( s, len, pos, eb->is_utf8 );  
  if (n > 0 && width != NULL) *width = editbuf_cwidth( eb, s+pos, n );
  //debug_msg("edit: next ofs '%s' %d %d %d\n", s, pos, n, (width != NULL ? *width : -1));
  return n;
}

static ssize_t editbuf_width( editbuf_t* eb, const char* s ) {
  if (s == NULL) return 0;
  ssize_t len = rp_strlen(s);
  ssize_t pos = 0;
  ssize_t width = 0;
  ssize_t w;
  ssize_t n;
  while ((n = editbuf_next_ofs(eb, s, len, pos, &w)) > 0) {
    width += w;
    pos += n;
  }
  // debug_msg("edit: width of '%s' = %zd\n", s, width);
  return width;
}

static bool editbuf_ensure_space(rp_env_t* env, editbuf_t* eb, ssize_t extra) 
{
  if (eb->buflen < eb->len + extra) {
    // reallocate
    ssize_t newlen = (eb->buflen == 0 ? 1024 : 2*eb->buflen);
    if (newlen <= eb->len + extra) newlen = eb->len + extra + 1;
    debug_msg("edit: reallocate edit buffer: old %zd, new %zd\n", eb->buflen, newlen);
    char* newbuf = (char*)env_realloc(env, eb->buf, newlen+1);
    if (newbuf == NULL) {
      assert(false);
      return false;
    }
    eb->buf = newbuf;
    eb->buflen = newlen;
    eb->buf[eb->buflen] = 0;
  }
  assert(eb->buflen >= eb->len + extra);
  return true;
}

static ssize_t editbuf_input_len(editbuf_t* eb) {
  return rp_strlen(eb->buf);
}

static bool editbuf_pos_is_at_end(editbuf_t* eb) {
  return (eb->buf[eb->pos] == 0);
}


static void editbuf_clear_extra( editbuf_t* eb ) {
  ssize_t ilen = editbuf_input_len(eb);
  assert(eb->buf[ilen] == 0);
  eb->len = ilen + 1;
}

static bool editbuf_append_extra( rp_env_t* env, editbuf_t* eb, const char* s ) {
  if (s == NULL) return true;
  ssize_t len = rp_strlen(s);
  if (!editbuf_ensure_space(env,eb,len)) return false;
  assert(eb->buflen - eb->len >= len);
  if (!rp_memnmove( eb->buf + eb->len, eb->buflen - eb->len, s, len )) {
    assert(false);
    return false;
  }
  eb->len += len;
  return true;
}

static const char* editbuf_drop_until_fit( editbuf_t* eb, const char* s, ssize_t max_width) {
  if (s == NULL) return s;
  ssize_t width = editbuf_width(eb,s);
  ssize_t len   = rp_strlen(s);
  ssize_t pos = 0;
  ssize_t next;
  ssize_t w;
  while (width > max_width && (next = editbuf_next_ofs(eb, s, len, pos, &w)) > 0) {
    width -= w;
    pos += next;
  }
  return (s + pos);
}

typedef bool (match_fun_t)(const char* s, ssize_t len);

static ssize_t editbuf_get_start_of( editbuf_t* eb, ssize_t pos, match_fun_t* match ) {
  assert(pos >= 0 && pos < eb->len);
  if (pos >= eb->len) pos = eb->len-1;
  if (pos < 0) pos = 0;
  ssize_t i = pos;
  // skip matching first (say, whitespace in case of the previous start-of-word)
  do {
    ssize_t prev = editbuf_previous_ofs(eb, eb->buf, i, NULL); 
    if (prev <= 0) break;
    if (!match(eb->buf + i - prev, prev)) break;
    i -= prev;
  } while (i > 0);  
  // find match
  do {
    ssize_t prev = editbuf_previous_ofs(eb, eb->buf, i, NULL); 
    if (prev <= 0) break;
    if (match(eb->buf + i - prev, prev)) {
      return i;  // found;
    }
    i -= prev;
  } while (i > 0);
  return -1;
}

static ssize_t editbuf_get_end_of( editbuf_t* eb, ssize_t pos, match_fun_t* match) {
  assert(pos >= 0 && pos < eb->len);
  if (pos >= eb->len) pos = eb->len-1;
  if (pos < 0) pos = 0;  
  ssize_t i = pos;
  ssize_t next;
  // skip matching first (say, whitespace in case of the next end-of-word)
  do {
    next = editbuf_next_ofs(eb, eb->buf, eb->len, i, NULL); 
    if (!match(eb->buf + i, next)) break;
    i += next;
  } while (next > 0);  
  // and then look
  do {
    next = editbuf_next_ofs(eb, eb->buf, eb->len, i, NULL); 
    if (match(eb->buf + i, next)) {
      return i; // found
    }
    i += next;
  } while (next > 0);
  return -1;
} 

static bool match_newline( const char* s, ssize_t n ) {  
  return (n == 1 && (*s == '\n' || *s == 0));
}

static ssize_t editbuf_get_line_start( editbuf_t* eb, ssize_t pos) {
  ssize_t start = editbuf_get_start_of(eb,pos,&match_newline);
  return (start < 0 ? 0 : start); 
}

static ssize_t editbuf_get_line_end( editbuf_t* eb, ssize_t pos) {
  return editbuf_get_end_of(eb,pos,&match_newline);
}

static bool match_word_boundary( const char* s, ssize_t n ) {  
  char c = s[0];
  return !(n != 1 || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c > '~'));
}

static ssize_t editbuf_get_word_start( editbuf_t* eb, ssize_t pos) {
  ssize_t start = editbuf_get_start_of(eb,pos,&match_word_boundary);
  return (start < 0 ? 0 : start); 
}

static ssize_t editbuf_get_word_end( editbuf_t* eb, ssize_t pos) {
  return editbuf_get_end_of(eb,pos,&match_word_boundary);
}


//-------------------------------------------------------------
// Row iterator
//-------------------------------------------------------------

typedef bool (edit_line_fun_t)(rp_env_t* env, editbuf_t* eb, 
                               ssize_t row, ssize_t row_start, ssize_t row_len, 
                               bool in_extra, bool is_wrap,
                               void* arg);

static ssize_t edit_for_each_row( rp_env_t* env, editbuf_t* eb, edit_line_fun_t* fun, void* arg ) {
  ssize_t i;
  ssize_t termw = term_get_width(&env->term);
  ssize_t rcount = 0;
  ssize_t rcol = 0;
  ssize_t rstart = 0;  
  bool    in_extra = false;
  bool    after_user = false;  
  for(i = 0; i < eb->len; ) {
    ssize_t w;
    ssize_t next = editbuf_next_ofs(eb, eb->buf, eb->len, i, &w);    
    if (next <= 0) {
      debug_msg("edit: foreach row: next<=0: len %zd, i %zd, w %zd, buf %s\n", eb->len, i, w, eb->buf );
      assert(false);
      break;
    }
    ssize_t pwidth = (in_extra ? 0 : eb->prompt_width);
    if (termw != 0 && i != 0 && (rcol + w + pwidth + 1 /* for the cursor */) > termw) {  
      // wrap
      if (fun != NULL) {
        if (fun(env,eb,rcount,rstart,i - rstart,in_extra,true,arg)) return rcount;
      }
      rcount++;
      rstart = i;
      rcol   = 0;
    }
    if (eb->buf[i] == '\n') {
      // newline
      if (fun != NULL) {
        if (fun(env,eb,rcount,rstart,i - rstart,in_extra,false,arg)) return rcount;
      }
      rcount++;
      rstart = i+1;
      rcol = 0;
      if (after_user) in_extra = true;
    }
    if (eb->buf[i] == 0) {
      after_user = true;
    }
    i += next;
    rcol += w;
  }
  if (fun != NULL) {
    if (fun(env,eb,rcount,rstart,i - rstart,in_extra,false,arg)) return rcount;
  }
  return rcount+1;
}

//-------------------------------------------------------------
// Get position
//-------------------------------------------------------------

typedef struct rowcol_s {
  ssize_t row;
  ssize_t col;
  ssize_t col_start;
  ssize_t row_start;
  ssize_t row_len;
  bool    first_on_row;
  bool    last_on_row;
} rowcol_t;

static bool edit_get_current_pos_iter(
    rp_env_t* env, editbuf_t* eb,
    ssize_t row, ssize_t row_start, ssize_t row_len, 
    bool in_extra, bool is_wrap, void* arg)
{
  unused(env); unused(arg);
  if (eb->pos >= row_start && eb->pos <= (row_start + row_len)) {
    // found the cursor row
    rowcol_t* rc = (rowcol_t*)arg;
    rc->row_start = row_start;
    rc->row_len   = row_len;
    rc->row = row;
    rc->col = rc->col_start = (in_extra ? 0 : eb->prompt_width);
    rc->first_on_row = (eb->pos == row_start);
    ssize_t adjust = (is_wrap  /* wrap has no newline at end */ || 
                      (row_len > 0 && eb->buf[row_start + row_len - 1] == 0) /* end of user input */ ? 1 : 0);
    rc->last_on_row  = (eb->pos == row_start + row_len - adjust);
    // debug_msg("edit: pos iter%s%s, row %zd, pos: %zd, row_start: %zd, rowlen: %zd\n", in_extra ? " inextra" : "", is_wrap ? " wrap" : "", row, eb->pos, row_start, row_len);
    for(ssize_t i = row_start; i < eb->pos; ) {
      ssize_t w;
      ssize_t next = editbuf_next_ofs(eb, eb->buf, eb->len, i, &w);
      if (next <= 0) break;
      i += next;
      rc->col += w;
    }
  }  
  return false; // always continue to count all rows
}

static ssize_t edit_get_current_pos(rp_env_t* env, editbuf_t* eb, rowcol_t* rc) {
  ssize_t rows = edit_for_each_row(env,eb,&edit_get_current_pos_iter,rc);
  debug_msg("edit: current pos: (%d, %d) %s %s\n", rc->row, rc->col, rc->first_on_row ? "first" : "", rc->last_on_row ? "last" : "");
  return rows;
}

//-------------------------------------------------------------
// Set position
//-------------------------------------------------------------

static bool edit_set_pos_iter(
    rp_env_t* env, editbuf_t* eb,
    ssize_t row, ssize_t row_start, ssize_t row_len, 
    bool in_extra, bool is_wrap, void* arg)
{
  unused(env); unused(arg); unused(is_wrap); unused(in_extra);
  rowcol_t* rc = (rowcol_t*)arg;
  if (rc->row != row) return false; // keep searching
  ssize_t col = 0; 
  ssize_t i = row_start;
  ssize_t end = row_start + row_len;
  while (col < rc->col && i < end) {
    ssize_t w;
    ssize_t next = editbuf_next_ofs(eb, eb->buf, eb->len, i, &w);
    i += next;
    col += w;
  }
  eb->pos = i;
  return true;
}

static void edit_set_pos_at(rp_env_t* env, editbuf_t* eb, ssize_t row, ssize_t col /* without prompt */) {
  rowcol_t rc;
  memset(&rc,0,ssizeof(rc));
  rc.row = row;
  rc.col = col;
  edit_for_each_row(env,eb,&edit_set_pos_iter,&rc);  
  edit_refresh(env,eb);
}

static bool edit_pos_is_at_row_end(rp_env_t* env, editbuf_t* eb) {
  rowcol_t rc;
  edit_get_current_pos(env,eb,&rc);
  return rc.last_on_row;
}




//-------------------------------------------------------------
// Refresh
//-------------------------------------------------------------

static void edit_write_prompt( rp_env_t* env, editbuf_t* eb, ssize_t row, bool in_extra ) {
  if (!in_extra) { 
    if (env->prompt_color != RP_DEFAULT) term_color( &env->term, env->prompt_color );
    if (row==0) {
      term_write(&env->term, eb->prompt);
    }
    else {
      term_writef(&env->term, "%*c", editbuf_width(eb,eb->prompt), ' ' );
    }
    term_attr_reset( &env->term );
    if (env->prompt_color != RP_DEFAULT) term_color( &env->term, env->prompt_color );
    term_write( &env->term, (env->prompt_marker == NULL ? "> " : env->prompt_marker )); 
    term_attr_reset( &env->term );
  }
}

typedef struct refresh_info_s {
  ssize_t first_row;
  ssize_t last_row;
} refresh_info_t;

static bool edit_refresh_rows_iter(
    rp_env_t* env, editbuf_t* eb,
    ssize_t row, ssize_t row_start, ssize_t row_len, 
    bool in_extra, bool is_wrap, void* arg)
{
  refresh_info_t* info = (refresh_info_t*)(arg);
  // debug_msg("edit: line refresh: row %zd, len: %zd\n", row, row_len);
  if (row < info->first_row) return false;
  if (row > info->last_row)  return true; // should not occur
  
  term_clear_line(&env->term);
  edit_write_prompt(env, eb, row, in_extra);

  char* p = &eb->buf[row_start + row_len];
  char c = *p; *p = 0;
  term_write( &env->term, eb->buf + row_start );
  *p = c;
  if (row < info->last_row) {
    if (is_wrap && eb->is_utf8) { 
      term_color( &env->term, RP_DARKGRAY );
      #ifdef _WIN32
      term_write( &env->term, "\xE2\x86\x90");  // left arrow 
      #else
      term_write( &env->term, "\xE2\x86\xB5" ); // return symbol
      #endif
      term_attr_reset( &env->term );
    }
    term_write(&env->term, "\r\n");
  }
  return (row >= info->last_row);
}

static ssize_t edit_refresh_rows(rp_env_t* env, editbuf_t* eb, ssize_t first_row, ssize_t last_row) {
  refresh_info_t info;
  info.first_row  = first_row;
  info.last_row   = last_row;
  return edit_for_each_row(env,eb,&edit_refresh_rows_iter, &info );
}

static void edit_refresh(rp_env_t* env, editbuf_t* eb) {
  rowcol_t rc;
  ssize_t rows = edit_get_current_pos( env, eb, &rc );
  debug_msg("edit: start refresh: rows %zd, pos: %zd,%zd (previous rows %zd, row %zd)\n", rows, rc.row, rc.col, eb->prev_rows, eb->prev_row);
  
  // only render at most terminal height rows
  ssize_t ht = term_get_height(&env->term);
  ssize_t first_row = 0;
  ssize_t last_row = rows - 1;
  if (rows > ht) {
    first_row = rc.row - ht + 1;          // ensure cursor is visible
    if (first_row < 0) first_row = 0;
    last_row = first_row + ht - 1;
  }
 
  term_start_buffered(&env->term);        // reduce flicker
  term_up(&env->term, eb->prev_row);
  
  // render rows
  edit_refresh_rows( env, eb, first_row, last_row );

  // overwrite trailing rows we do not use anymore
  ssize_t rrows = last_row - first_row + 1;  // rendered rows
  if (rrows < ht && rows < eb->prev_rows) {
    ssize_t clear = eb->prev_rows - rows;
    while (rrows < ht && clear > 0) {
      clear--;
      rrows++;
      term_write(&env->term, "\r\n");
      term_clear_line(&env->term);
    }
  }
  
  // move cursor back to edit position
  term_start_of_line(&env->term);
  term_up(&env->term, first_row + rrows - 1 - rc.row );
  term_right(&env->term, rc.col);
  term_end_buffered(&env->term);


  // update previous
  eb->prev_rows = rows;
  eb->prev_row = rc.row;
}


static void edit_clear(rp_env_t* env, editbuf_t* eb ) {
  term_attr_reset(&env->term);  
  term_up(&env->term, eb->prev_row);
  
  // overwrite all rows
  for( ssize_t i = 0; i < eb->prev_rows; i++) {
    term_clear_line(&env->term);
    term_write(&env->term, "\r\n");    
  }
  
  // move cursor back 
  term_up(&env->term, eb->prev_rows );  
}

static void edit_clear_screen(rp_env_t* env, editbuf_t* eb ) {
  ssize_t prev_rows = eb->prev_rows;
  eb->prev_rows = term_get_height(&env->term) - 1;
  edit_clear(env,eb);
  eb->prev_rows = prev_rows;
  edit_refresh(env,eb);
}

//-------------------------------------------------------------
// Edit operations
//-------------------------------------------------------------


static void edit_history_at(rp_env_t* env, editbuf_t* eb, int ofs ) 
{
  if (eb->modified) { history_update(env, eb->buf); }
  const char* entry = history_get(env,eb->history_idx + ofs);
  debug_msg( "edit: history: at: %d + %d, found: %s\n", eb->history_idx, ofs, entry);
  if (entry == NULL) return;
  eb->history_idx += ofs;
  eb->len = 0;
  ssize_t len = rp_strlen(entry);
  editbuf_ensure_space(env,eb,len + 1);
  assert(eb->buflen >= len + 1);
  if (rp_strcpy(eb->buf, eb->buflen, entry)) {
    eb->len = len + 1;
    eb->pos = len;
  }
  eb->modified = false;  
  edit_refresh(env,eb);
}

static void edit_history_prev(rp_env_t* env, editbuf_t* eb) {
  edit_history_at(env,eb, 1 );
}

static void edit_history_next(rp_env_t* env, editbuf_t* eb) {
  edit_history_at(env,eb, -1 );
}

static void edit_cursor_left(rp_env_t* env, editbuf_t* eb) {
  ssize_t w;
  ssize_t n = editbuf_previous_ofs(eb,eb->buf,eb->pos,&w);
  if (n <= 0) return;
  rowcol_t rc;
  edit_get_current_pos( env, eb, &rc);
  eb->pos -= n;    // go back one character
  if (!rc.first_on_row) {
    // if we were not at a start column we do not need a full refresh
    term_left(&env->term,w);
  }
  else {
    edit_refresh(env,eb);
  }
}

static void edit_cursor_right(rp_env_t* env, editbuf_t* eb) {
  ssize_t w;
  ssize_t n = editbuf_next_ofs(eb,eb->buf,editbuf_input_len(eb),eb->pos,&w);
  if (n <= 0) return;
  rowcol_t rc;
  edit_get_current_pos( env, eb, &rc);
  eb->pos += n;    // go forward one character
  if (!rc.last_on_row) {
    // if we were not at an end column we do not need a full refresh
    term_right(&env->term,w);
  }
  else {
    edit_refresh(env,eb);
  }
}

static void edit_cursor_line_end(rp_env_t* env, editbuf_t* eb) {
  ssize_t end = editbuf_get_line_end(eb,eb->pos);
  if (end < 0) return;  
  eb->pos = end; 
  edit_refresh(env,eb);
}

static void edit_cursor_line_start(rp_env_t* env, editbuf_t* eb) {
  ssize_t start = editbuf_get_line_start(eb,eb->pos);
  if (start < 0) return;
  eb->pos = start;
  edit_refresh(env,eb);
}

static void edit_cursor_next_word(rp_env_t* env, editbuf_t* eb) {
  ssize_t end = editbuf_get_word_end(eb,eb->pos);
  if (end < 0) return;
  eb->pos = end;
  edit_refresh(env,eb);
}

static void edit_cursor_prev_word(rp_env_t* env, editbuf_t* eb) {
  ssize_t start = editbuf_get_word_start(eb,eb->pos);
  if (start < 0) return;
  eb->pos = start;
  edit_refresh(env,eb);
}

static void edit_cursor_to_start(rp_env_t* env, editbuf_t* eb) {
  eb->pos = 0; 
  edit_refresh(env,eb);
}

static void edit_cursor_to_end(rp_env_t* env, editbuf_t* eb) {
  eb->pos = editbuf_input_len(eb); 
  edit_refresh(env,eb);
}


static void edit_cursor_row_up(rp_env_t* env, editbuf_t* eb) {
  rowcol_t rc;
  edit_get_current_pos( env, eb, &rc);
  if (rc.row == 0) {
    edit_history_prev(env,eb);
  }
  else {
    edit_set_pos_at( env, eb, rc.row - 1, rc.col - rc.col_start );
  }
}

static void edit_cursor_row_down(rp_env_t* env, editbuf_t* eb) {
  rowcol_t rc;
  ssize_t rows = edit_get_current_pos( env, eb, &rc);
  if (rc.row + 1 >= rows) {
    edit_history_next(env,eb);
  }
  else {
    edit_set_pos_at( env, eb, rc.row + 1, rc.col - rc.col_start );
  }
}

static void edit_backspace(rp_env_t* env, editbuf_t* eb) {
  if (eb->pos <= 0) return;
  eb->modified = true;
  ssize_t n = editbuf_previous_ofs(eb, eb->buf, eb->pos, NULL);
  if (n <= 0) return;  
  rp_memmove( eb->buf + eb->pos - n, eb->buf + eb->pos, eb->len - eb->pos );
  eb->len -= n;
  eb->pos -= n;
  eb->buf[eb->len] = 0;

  edit_refresh(env,eb);
}

static void edit_delete(rp_env_t* env, editbuf_t* eb) {
  ssize_t n = editbuf_next_ofs(eb, eb->buf, editbuf_input_len(eb), eb->pos, NULL);
  if (n <= 0) return;  
  eb->modified = true;
  rp_memmove( eb->buf + eb->pos, eb->buf + eb->pos + n, eb->len - eb->pos - n);
  eb->len -= n;
  eb->buf[eb->len] = 0;
  edit_refresh(env,eb);
}

static void edit_delete_all(rp_env_t* env, editbuf_t* eb) {
  eb->len = 1;
  eb->pos = 0;
  eb->buf[0] = 0;
  edit_refresh(env,eb);
}

static void edit_delete_from_to(rp_env_t* env, editbuf_t* eb, ssize_t start, ssize_t end) 
{   
  if (end >= eb->len) end = eb->len-1; // preserve final 0
  ssize_t n = end - start;
  if (n <= 0) return;
  rp_memmove( eb->buf + start, eb->buf + end, eb->len - end );
  //debug_msg("edit: del from to: %zd -> %zd (len %zd, pos %zd)", start, end, eb->len, eb->pos);
  eb->len -= n;
  if (eb->pos > start && eb->pos < end) {
    eb->pos = start;
  }
  else if (eb->pos >= end) {
    eb->pos -= n;
  }
  assert(eb->pos >= 0 && eb->pos < eb->len);
  edit_refresh(env,eb);
}

static void edit_delete_line_from(rp_env_t* env, editbuf_t* eb, ssize_t start) 
{ 
  ssize_t end = editbuf_get_line_end(eb,start);
  if (end < 0) return;
  edit_delete_from_to( env, eb, start, end );
}

static void edit_delete_to_end_of_line(rp_env_t* env, editbuf_t* eb) {
  edit_delete_line_from(env, eb, eb->pos);
}

static void edit_delete_line(rp_env_t* env, editbuf_t* eb) {
  ssize_t start = editbuf_get_line_start(eb,eb->pos);
  if (start < 0) return;
  ssize_t end   = editbuf_get_line_end(eb,start);
  if (end < 0) return;
  // delete newline as well so no empty line is left;
  if (eb->buf[end] == '\n') end++;
  else if (start > 0 && eb->buf[start-1] == '\n') start--;
  edit_delete_from_to(env,eb,start,end);
}
 
static void edit_delete_to_start_of_word(rp_env_t* env, editbuf_t* eb) {
  ssize_t start = editbuf_get_word_start(eb,eb->pos);
  if (start < 0) return;
  ssize_t end = eb->pos;
  eb->pos = start;
  edit_delete_from_to(env,eb,start,end);
}

static void edit_swap( rp_env_t* env, editbuf_t* eb ) {
  if (eb->pos <= 0 || eb->pos == eb->len-1) return;
  ssize_t next = editbuf_next_ofs( eb, eb->buf, eb->len, eb->pos, NULL );
  ssize_t prev = editbuf_previous_ofs( eb, eb->buf, eb->pos, NULL );
  if (next <= 0 || prev <= 0 || prev > 32) return;
  eb->pos -= prev;
  assert(eb->pos >= 0);
  char buf[32];
  rp_memcpy( buf, eb->buf + eb->pos, prev );
  rp_memcpy( eb->buf + eb->pos, eb->buf + eb->pos + prev, next );
  rp_memcpy( eb->buf + eb->pos + prev, buf, prev );
  edit_refresh(env,eb);
}

static void edit_insert_char(rp_env_t* env, editbuf_t* eb, char c, bool refresh) {
  if (!editbuf_ensure_space(env,eb,1)) return;
  eb->modified = true;

  // insert in buffer
  if (eb->pos < eb->len) {
    rp_memmove( eb->buf + eb->pos + 1, eb->buf + eb->pos, eb->len - eb->pos );
  }
  eb->buf[eb->pos] = c;
  eb->pos++;
  eb->len++;
  eb->buf[eb->len] = 0;

  // output to terminal
  if (refresh) edit_refresh(env,eb);
}

//-------------------------------------------------------------
// Help
//-------------------------------------------------------------

static const char* help[] = {
  "","",
  "","Repline v1.0, copyright (c) 2021 Daan Leijen.", 
  "","This is free software; you can redistribute it and/or",
  "","modify it under the terms of the MIT License.",
  "","See <https://github.com/daanx/repline> for further information.",
  "","",
  "","Navigation:",
  "left, "
  "^b",         "go one character to the left",
  "right, "
  "^f",         "go one character to the right",
  "up",         "go one row up, or back in the history",
  "down",       "go one row down, or forward in the history",  
  #ifdef __APPLE__
  "shift+left",
  #else
  "^left",     
  #endif
                "go to the start of the previous word",
  #ifdef __APPLE__
  "shift+right",
  #else
  "^right",
  #endif
                "go to the end the current word",
  "home, "
  "^a",         "go to the start of the current line",  
  "end, "
  "^e",         "go to the end of the current line",
  "^home, "
  "pgup",       "go to the start of the current input",
  "^end, "
  "pgdn",       "go to the end of the current input",
  "^p",         "go back in the history",
  "^n",         "go forward in the history",
  "","",
  "", "Editing:",
  "enter",      "accept current input",
  #ifndef __APPLE__
  "^enter, ^j", "",
  #else
  "^j,", "",
  #endif
  "shift+tab",  "create a new line for multi-line input",
  //" ",          "(or type '\\' followed by enter)",
  "^l",         "clear screen",
  "del",        "delete the current character",
  "backspace",  "delete the previour character",
  "^k",         "delete to the end of a line",
  "^w",         "delete to start of a word",
  "esc, "
  "^u",         "delete the current line",
  "^t",         "swap with previous character (move character backward)",
  "^d",         "done with empty input, or delete current character",
  //"^C",         "done with empty input",
  //"F1",         "show this help",
  "tab",        "try to complete the current input",
  "","",
  "","Inside the completion menu:",
  "enter, "
  "space",      "use the currently selected completion",
  "1 - 9",      "use completion N from the menu",
  "tab",        "select the next completion",
  "cursor keys","select completion N in the menu",
  "pgdn, ", "",
  "shift-tab",  "show all further possible completions",
  "esc",        "exit menu without completing",
  "","\x1B[0m",
  NULL, NULL
};

static void edit_show_help( rp_env_t* env, editbuf_t* eb ) {
  for (ssize_t i = 0; help[i] != NULL && help[i+1] != NULL; i+=2) {
    if (help[i][0] == 0) {
      term_writef(&env->term, "\x1B[90m%s\x1B[0m\r\n", help[i+1]);
    }
    else {
      term_writef(&env->term, "  \x1B[97m%-12s\x1B[0m%s%s\r\n", help[i], (help[i+1][0] == 0 ? "" : ": "), help[i+1]);
    }
  }
  eb->prev_rows = 0;
  edit_refresh(env,eb);   
}

//-------------------------------------------------------------
// Completion
//-------------------------------------------------------------

static void edit_complete(rp_env_t* env, editbuf_t* eb, int idx) {
  completion_t* cm = completions_get(env,idx);
  if (cm == NULL) return;
  editbuf_ensure_space(env,eb,completion_extra_needed(cm));
  eb->len = completion_apply(cm, eb->buf, eb->len, eb->pos, &eb->pos);
  edit_refresh(env,eb);
}

static void editbuf_append_completion(rp_env_t* env, editbuf_t* eb, ssize_t idx, ssize_t width, bool numbered, bool selected ) {
  completion_t* cm = completions_get(env,idx);
  if (cm == NULL) return;
  if (numbered) {
    char buf[32];
    snprintf(buf, 32, "\x1B[90m%s%zd \x1B[0m", (selected ? (eb->is_utf8 ? "\xE2\x86\x92" : "*") : " "), 1 + idx);
    editbuf_append_extra(env, eb, buf);
    width -= 3;
  }
  const char* s = (cm->display==NULL ? cm->replacement : cm->display);
  if (width <= 0) {
    editbuf_append_extra(env, eb, s);
  }
  else {
    // fit to width
    const char* sc = editbuf_drop_until_fit( eb, s, width);
    if (sc != s) {
      editbuf_append_extra(env, eb, "...");
      sc = editbuf_drop_until_fit( eb, s, width - 3);
    }
    editbuf_append_extra(env, eb, sc);
    // fill out with spaces
    ssize_t n = width - editbuf_width(eb,sc);
    while( n-- > 0 ) { editbuf_append_extra(env, eb," "); }  
  }
}

// 2 and 3 column output up to 80 wide
#define RP_DISPLAY2_MAX    35
#define RP_DISPLAY2_COL    (3+RP_DISPLAY2_MAX)
#define RP_DISPLAY2_WIDTH  (2*RP_DISPLAY2_COL + 2)    // 78

#define RP_DISPLAY3_MAX    22
#define RP_DISPLAY3_COL    (3+RP_DISPLAY3_MAX)
#define RP_DISPLAY3_WIDTH  (3*RP_DISPLAY3_COL + 2*2)  // 79

static void editbuf_append_completion2(rp_env_t* env, editbuf_t* eb, ssize_t idx1, ssize_t idx2, ssize_t selected ) {  
  editbuf_append_completion(env, eb, idx1, RP_DISPLAY2_COL, true, (idx1 == selected) );
  editbuf_append_extra(env, eb, "  ");
  editbuf_append_completion(env, eb, idx2, RP_DISPLAY2_COL, true, (idx2 == selected) );
}

static void editbuf_append_completion3(rp_env_t* env, editbuf_t* eb, ssize_t idx1, ssize_t idx2, ssize_t idx3, ssize_t selected ) {  
  editbuf_append_completion(env, eb, idx1, RP_DISPLAY3_COL, true, (idx1 == selected) );
  editbuf_append_extra(env, eb, "  ");
  editbuf_append_completion(env, eb, idx2, RP_DISPLAY3_COL, true, (idx2 == selected) );
  editbuf_append_extra(env, eb, "  ");
  editbuf_append_completion(env, eb, idx3, RP_DISPLAY3_COL, true, (idx3 == selected) );
}

static ssize_t edit_completions_max_width( rp_env_t* env, editbuf_t* eb, ssize_t count ) {
  ssize_t max_width = 0;
  for( ssize_t i = 0; i < count; i++) {
    completion_t* cm = completions_get(env,i);
    if (cm != NULL) {
      ssize_t w = editbuf_width(eb, (cm->display != NULL ? cm->display : cm->replacement));
      if (w > max_width) max_width = w;
    }
  }
  return max_width;
}

static void edit_completion_menu(rp_env_t* env, editbuf_t* eb) {
  ssize_t count  = completions_count( env );
  ssize_t count_displayed = count;
  assert(count > 1);
  ssize_t selected = 0;
  ssize_t columns  = 1;
  ssize_t percolumn= count;
  
again: 
  // show first 9 (or 8) completions
  editbuf_clear_extra(eb);
  ssize_t twidth = term_get_width(&env->term);   
  if (count > 3 && twidth > RP_DISPLAY3_WIDTH && edit_completions_max_width(env,eb,9) <= RP_DISPLAY3_MAX) {
    // display as a 3 column block
    count_displayed = (count > 9 ? 9 : count);
    columns = 3;
    percolumn = 3;
    for( ssize_t rw = 0; rw < percolumn; rw++ ) {
      editbuf_append_extra( env, eb, "\n");
      editbuf_append_completion3( env, eb, rw, percolumn+rw, (2*percolumn)+rw, selected );
    }
  }
  else if (count > 4 && twidth > RP_DISPLAY2_WIDTH && edit_completions_max_width(env,eb,8) <= RP_DISPLAY2_MAX) {
    // display as a 2 column block if some entries are too wide for three columns
    count_displayed = (count > 8 ? 8 : count);
    columns = 2;
    percolumn = (count_displayed <= 6 ? 3 : 4);
    for( ssize_t rw = 0; rw < percolumn; rw++ ) {
      editbuf_append_extra( env, eb, "\n");
      editbuf_append_completion2( env, eb, rw, percolumn+rw, selected );
    }
  }
  else {
    // display as a list
    count_displayed = (count > 9 ? 9 : count);    
    columns = 1;
    percolumn = count_displayed;
    for(ssize_t i = 0; i < count_displayed; i++) {
      editbuf_append_extra( env, eb, "\n");
      editbuf_append_completion(env, eb, i, -1, true /* numbered */, selected == i );        
    }
  }
  char buf[128];
  //snprintf(buf,128,"\n\x1B[90m(enter or 1-%zd to complete, tab/cursor to change selection)\x1B[0m", count9);
  //editbuf_append_extra( env, eb, buf);       
  if (count > 9) {
    snprintf(buf,128,"\n\x1B[90m(press shift-tab to see all %zd completions)\x1B[0m", count);
    editbuf_append_extra( env, eb, buf);      
  }   
  edit_refresh(env,eb);
  
  // read here; if not a valid key, push it back and return to main event loop
  code_t c = tty_read(&env->tty);
  editbuf_clear_extra(eb);      
  if (c >= '1' && c <= '9' && c - '1' < count) {
    selected = (c - '1');
    c = KEY_SPACE;
  }   
  else if (c == KEY_TAB || c == KEY_DOWN) {
    selected++;
    if (selected >= count_displayed) selected = 0;
    goto again;
  }
  else if (c == KEY_UP) {
    selected--;
    if (selected < 0) selected = count_displayed - 1;
    goto again;
  }
  if (c == KEY_RIGHT) {
    if (columns > 1 && selected + percolumn < count_displayed) selected += percolumn;
    goto again;
  }
  if (c == KEY_LEFT) {
    if (columns > 1 && selected - percolumn >= 0) selected -= percolumn;
    goto again;
  }
  else if (c == KEY_END) {
    selected = count_displayed - 1;
    goto again;
  }
  else if (c == KEY_HOME) {
    selected = 0;
    goto again;
  }
  else if (c == KEY_ESC) {
    completions_clear(env);
    edit_refresh(env,eb);
    c = 0; // ignore and return
  }
  else if (c == KEY_ENTER || c == KEY_SPACE) {  
    // select the current entry
    assert(selected < count);
    eb->modified = true;
    c = 0;      
    completion_t* cm = completions_get(env,selected);
    editbuf_ensure_space(env,eb,completion_extra_needed(cm));
    eb->len = completion_apply(cm, eb->buf, eb->len, eb->pos, &eb->pos);        
    edit_refresh(env,eb);    
  }
  else if ((c == KEY_PAGEDOWN || c == KEY_LINEFEED || c == KEY_CTRP_END) && count > 9) {
    // show all completions
    c = 0;
    rowcol_t rc;
    edit_get_current_pos(env,eb,&rc);
    edit_clear(env,eb);
    edit_write_prompt(env,eb,0,false);
    term_write(&env->term, "\r\n");
    for(ssize_t i = 0; i < count; i++) {
      completion_t* cm = completions_get(env,i);
      if (cm != NULL) {
        // term_writef(&env->term, "\x1B[90m%3d \x1B[0m%s\r\n", i+1, (cm->display != NULL ? cm->display : cm->replacement ));          
        term_write(&env->term, (cm->display != NULL ? cm->display : cm->replacement ));         
        term_write(&env->term, "\r\n"); 
      }
    }
    for(ssize_t i = 0; i < rc.row+1; i++) {
      term_write(&env->term, " \r\n");
    }
    eb->prev_rows = 0;
    edit_refresh(env,eb);      
  }
  // done
  completions_clear(env);      
  if (c != 0) tty_code_pushback(&env->tty,c);
}

static void edit_generate_completions(rp_env_t* env, editbuf_t* eb) {
  debug_msg( "edit: complete: %zd: %s\n", eb->pos, eb->buf );
  if (eb->pos <= 0) return;
  ssize_t count = completions_generate(env,eb->buf,eb->pos,1000);
  if (count <= 0) {
    // no completions
    term_beep(&env->term); 
  }
  else if (count == 1) {
    // complete if only one match    
    eb->modified = true;
    edit_complete(env,eb,0);
  }
  else {
    edit_completion_menu( env, eb );    
  }
}


//-------------------------------------------------------------
// Edit line
//-------------------------------------------------------------

static char* edit_line( rp_env_t* env, const char* prompt )
{
  // set up an edit buffer
  editbuf_t eb;
  eb.buflen   = 120;
  eb.buf      = (char*)env_zalloc(env,eb.buflen);
  eb.pos      = 0;
  eb.len      = 1; // ending zero  
  eb.prev_rows= 1;
  eb.prev_row = 0;
  eb.modified = false;
  eb.is_utf8  = env->tty.is_utf8;
  eb.prompt   = (prompt != NULL ? prompt : "");
  eb.prompt_width  = editbuf_width( &eb, eb.prompt ) + (env->prompt_marker != NULL ? editbuf_width( &eb, env->prompt_marker) : 2);
  eb.history_idx   = 0;
  
  // show prompt
  edit_write_prompt(env, &eb, 0, false); 

  // always a history entry for the current input
  history_push(env, "");

  // process keys
  code_t c;
  int tofollow = 0;
  while(true) {    
    // read a character
    c = tty_read(&env->tty);
    if (c < 0) break;

    // update width as late as possible so a user can resize even if the prompt is already visible
    //if (eb.len == 1) 
    if (term_update_dim(&env->term,&env->tty)) {
      // eb.prev_rows = env->term.height;
      edit_refresh(env,&eb);   
    }

    // followers (for utf8)
    if (tofollow > 0) {
      char chr;
      if (code_is_follower(&env->tty, c, &chr)) {
        tofollow--;
        edit_insert_char( env, &eb, chr, tofollow == 0);
        continue;
      }
      else {
        // not a follower! recover.
        tofollow = 0;
      }
    }
    assert(tofollow==0);

    if (c == KEY_ENTER) {
      if (eb.pos > 0 && eb.buf[eb.pos-1] == env->multiline && edit_pos_is_at_row_end(env,&eb)) {
        // replace line-continuation with newline
        eb.buf[eb.pos-1] = '\n';
        edit_refresh(env,&eb);
      }
      else {
        // otherwise done
        break;
      }
    } 
    else if (c == KEY_CTRL('D')) {
      if (eb.pos == 0 && editbuf_pos_is_at_end(&eb)) 
        break;                    // ctrl+D on empty quits with NULL
      else 
        edit_delete(env,&eb);     // otherwise it is like delete
    } 
    else if (c == KEY_CTRL('C')) {
      break; // ctrl+C quits with NULL
    }  
    else switch(c) {
      case KEY_LINEFEED: // '\n' (ctrl+J, shift+enter, ctrl+tab)
        edit_insert_char(env,&eb,'\n',true);
        break;
      case KEY_TAB:
        edit_generate_completions(env,&eb);
        break;
      case KEY_LEFT:
      case KEY_CTRL('B'):
        edit_cursor_left(env,&eb);
        break;
      case KEY_RIGHT:
      case KEY_CTRL('F'):
        edit_cursor_right(env,&eb);
        break;
      case KEY_UP:
        edit_cursor_row_up(env,&eb);
        break;
      case KEY_DOWN:
        edit_cursor_row_down(env,&eb);
        break;                 
      case KEY_HOME:
      case KEY_CTRL('A'):
        edit_cursor_line_start(env,&eb);
        break;
      case KEY_END:
      case KEY_CTRL('E'):
        edit_cursor_line_end(env,&eb);
        break;
      case KEY_CTRP_LEFT:
        edit_cursor_prev_word(env,&eb);
        break;
      case KEY_CTRP_RIGHT:
        edit_cursor_next_word(env,&eb);
        break;      
      case KEY_CTRP_HOME:
      case KEY_PAGEUP:
        edit_cursor_to_start(env,&eb);
        break;
      case KEY_CTRP_END:
      case KEY_PAGEDOWN:
        edit_cursor_to_end(env,&eb);
        break;
      case KEY_DEL:
        edit_delete(env,&eb);
        break;
      case KEY_BACKSP:
        edit_backspace(env,&eb);
        break;
      case KEY_ESC:
      case KEY_CTRL('U'):
        edit_delete_line(env,&eb);
        break;
      case KEY_CTRL('K'):
        edit_delete_to_end_of_line(env,&eb);
        break;
      case KEY_CTRL('W'):
        edit_delete_to_start_of_word(env,&eb);
        break;      
      case KEY_CTRL('P'):
        edit_history_prev(env,&eb);
        break;
      case KEY_CTRL('N'):
        edit_history_next(env,&eb);
        break;
      case KEY_CTRL('L'): 
        edit_clear_screen(env,&eb);
        break; 
      case KEY_CTRL('T'):
        edit_swap(env,&eb);
        break;
      case KEY_F1:
        edit_show_help(env,&eb);
        break;
      default: {
        char chr;
        if (code_is_char(&env->tty,c,&chr)) {
          edit_insert_char(env,&eb,chr, true /* refresh */);
        }
        else if (code_is_extended(&env->tty,c,&chr,&tofollow)) {
          edit_insert_char(env,&eb,chr, (tofollow > 0));
        }
        else {
          debug_msg( "edit: ignore code: %d\n", c);
        }
        break;
      }
    }
  }

  // goto end
  ssize_t rlen = editbuf_input_len(&eb);
  eb.pos = rlen;  
  assert(eb.buf[rlen] == 0);
  edit_refresh(env,&eb);
  
  // return the captured buffer (return NULL on error or Ctrl-D without input)
  char* res = ((c == KEY_CTRL('D') && rlen == 0) || c == KEY_CTRL('C')) ? NULL : (char*)env_realloc(env,eb.buf,rlen+1);
  if (res == NULL) {
    rp_history_remove_last(env);
    env_free(env,eb.buf);
    return NULL;
  }
  assert(res[rlen] == 0);
  res[rlen] = 0;
  debug_msg("edit: input: %s\n", res);

  // update history
  history_update(env,res);
  history_save(env);
  return res;
}

