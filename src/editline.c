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
#include "stringbuf.h"

#if defined(_WIN32)
#else
#include <unistd.h>
#endif

// Undo buffer
typedef struct editstate_s {
  struct editstate_s* next;
  const char* input;          // input
  ssize_t     pos;            // cursor position
} editstate_t;


static void editstate_init( editstate_t** es ) {
  *es = NULL;
}

static void editstate_done( alloc_t* mem, editstate_t** es ) {
  while (*es != NULL) {
    editstate_t* next = (*es)->next;
    mem_free(mem, (*es)->input);
    mem_free(mem, *es );
    *es = next;
  }
  *es = NULL;
}

internal void editstate_capture( alloc_t* mem, editstate_t** es, const char* input, ssize_t pos) {
  if (input==NULL) input = "";
  // alloc
  editstate_t* entry = mem_zalloc_tp(mem, editstate_t);
  if (entry == NULL) return;
  // initialize
  entry->input = mem_strdup( mem, input);
  entry->pos   = pos;
  if (entry->input == NULL) { mem_free(mem, entry); return; }
  // and push
  entry->next = *es;
  *es = entry;
}

// caller should free *input
internal bool editstate_restore( alloc_t* mem, editstate_t** es, const char** input, ssize_t* pos ) {
  if (*es == NULL) return false;
  // pop 
  editstate_t* entry = *es;
  *es = entry->next;
  *input = entry->input;
  *pos = entry->pos;
  mem_free(mem, entry);
  return true;
}



// The edit buffer
typedef struct editor_s {
  stringbuf_t* input;    // current user input
  stringbuf_t* extra;    // extra displayed info (for completion menu etc)
  ssize_t pos;          // current cursor position in the input
  ssize_t cur_rows;     // current used rows to display our content (including extra content)
  ssize_t cur_row;      // current row that has the cursor (0 based, relative to the prompt)
  bool    modified;     // has a modification happened? (used for history navigation for example)  
  bool    is_utf8;      // terminal is utf8
  int     history_idx;  // current index in the history 
  editstate_t* undo;    // undo buffer  
  editstate_t* redo;    // redo buffer
  const char*  prompt_text;   // text of the prompt before the prompt marker  
  alloc_t* mem;
} editor_t;




//-------------------------------------------------------------
// Main edit line 
//-------------------------------------------------------------
static char* edit_line( rp_env_t* env, const char* prompt_text );
static void edit_refresh(rp_env_t* env, editor_t* eb);

internal char* rp_editline(rp_env_t* env, const char* prompt_text) {
  tty_start_raw(&env->tty);
  term_start_raw(&env->term);
  char* line = edit_line(env,prompt_text);
  term_end_raw(&env->term);
  tty_end_raw(&env->tty);
  term_write(&env->term,"\r\n");
  return line;
}


static void editor_capture(editor_t* eb, editstate_t** es ) {
  editstate_capture( eb->mem, es, sbuf_string(eb->input), eb->pos );
}

static void editor_undo_capture(editor_t* eb ) {
  editor_capture(eb, &eb->undo );
}

static void editor_restore(editor_t* eb, editstate_t** from, editstate_t** to ) {
  if (*from == NULL) return;
  const char* input;
  editor_capture( eb, to );
  if (!editstate_restore( eb->mem, from, &input, &eb->pos )) return;
  sbuf_replace( eb->input, input );
  mem_free(eb->mem, input);
  eb->modified = false;
}

static void editor_undo_restore(editor_t* eb ) {
  editor_restore(eb, &eb->undo, &eb->redo);
}

static void editor_redo_restore(editor_t* eb ) {
  editor_restore(eb, &eb->redo, &eb->undo);
  eb->modified = false;
}

static void editor_start_modify(editor_t* eb ) {
  editor_undo_capture(eb);
  editstate_done(eb->mem, &eb->redo);  // clear redo
  eb->modified = true;
}

static bool editor_pos_is_at_end(editor_t* eb ) {
  return (eb->pos == sbuf_len(eb->input));  
}



static ssize_t edit_get_prompt_width( rp_env_t* env, editor_t* eb, bool in_extra, ssize_t* termw ) {
  if (termw!=NULL) *termw = term_get_width( &env->term );
  return (in_extra ? 0 : str_column_width(eb->prompt_text,eb->is_utf8) + 
                         (env->prompt_marker == NULL ? 2 : str_column_width(env->prompt_marker,eb->is_utf8)));
}

static ssize_t edit_get_rowcol( rp_env_t* env, editor_t* eb, rowcol_t* rc ) {
  ssize_t termw;
  ssize_t promptw = edit_get_prompt_width( env, eb, false, &termw );
  return sbuf_get_rc_at_pos( eb->input, termw, promptw, eb->pos, rc );
}

static void edit_set_pos_at_rowcol( rp_env_t* env, editor_t* eb, ssize_t row, ssize_t col ) {
  ssize_t termw;
  ssize_t promptw = edit_get_prompt_width( env, eb, false, &termw );
  ssize_t pos = sbuf_get_pos_at_rc( eb->input, termw, promptw, row, col );
  if (pos < 0) return;
  eb->pos = pos;
}

static bool edit_pos_is_at_row_end( rp_env_t* env, editor_t* eb ) {
  rowcol_t rc;
  edit_get_rowcol( env, eb, &rc );
  return rc.last_on_row;
}

//-------------------------------------------------------------
// Refresh
//-------------------------------------------------------------

static void edit_write_prompt( rp_env_t* env, editor_t* eb, ssize_t row, bool in_extra ) {
  if (!in_extra) { 
    if (env->prompt_color != RP_DEFAULT_COLOR) term_color( &env->term, env->prompt_color );
    if (row==0) {
      term_write(&env->term, eb->prompt_text);
    }
    else {
      term_writef(&env->term, "%*c", str_column_width(eb->prompt_text,eb->is_utf8), ' ' );
    }
    term_attr_reset( &env->term );
    if (env->prompt_color != RP_DEFAULT_COLOR) term_color( &env->term, env->prompt_color );
    term_write( &env->term, (env->prompt_marker == NULL ? "> " : env->prompt_marker )); 
    term_attr_reset( &env->term );
  }
}

typedef struct refresh_info_s {
  rp_env_t* env;
  editor_t* eb;
  bool      in_extra;
  ssize_t first_row;
  ssize_t last_row;
} refresh_info_t;

static bool edit_refresh_rows_iter(
    const char* s,
    ssize_t row, ssize_t row_start, ssize_t row_len, 
    bool is_wrap, bool is_utf8, const void* arg, void* res)
{
  unused(is_utf8); unused(res);
  const refresh_info_t* info = (const refresh_info_t*)(arg);
  term_t* term = &info->env->term;

  // debug_msg("edit: line refresh: row %zd, len: %zd\n", row, row_len);
  if (row < info->first_row) return false;
  if (row > info->last_row)  return true; // should not occur
  
  term_clear_line(term);
  edit_write_prompt(info->env, info->eb, row, info->in_extra);

  term_write_n( term, s + row_start, row_len );  
  if (row < info->last_row) {
    if (is_wrap && info->eb->is_utf8) { 
      term_color( term, RP_DARKGRAY );
      #ifdef _WIN32
      term_write( term, "\xE2\x86\x90");  // left arrow 
      #else
      term_write( term, "\xE2\x86\xB5" ); // return symbol
      #endif
      term_attr_reset( term );
    }
    term_write(term, "\r\n");
  }
  return (row >= info->last_row);  
}

static void edit_refresh_rows(rp_env_t* env, editor_t* eb, 
                                  ssize_t termw, ssize_t promptw, 
                                  bool in_extra, ssize_t first_row, ssize_t last_row) 
{
  refresh_info_t info;
  info.env = env;
  info.eb  = eb;
  info.in_extra = in_extra;
  info.first_row  = first_row;
  info.last_row   = last_row;
  sbuf_for_each_row( (in_extra ? eb->extra : eb->input), termw, promptw,
                            &edit_refresh_rows_iter, &info, NULL );
}



static void edit_refresh(rp_env_t* env, editor_t* eb) 
{
  ssize_t termw;
  ssize_t promptw = edit_get_prompt_width( env, eb, false, &termw );

  rowcol_t rc;
  ssize_t rows_input = sbuf_get_rc_at_pos( eb->input, termw, promptw, eb->pos, &rc );
  rowcol_t rc_extra;
  ssize_t rows_extra = sbuf_get_rc_at_pos( eb->extra, termw, 0, 0, &rc_extra );
  if (sbuf_len(eb->extra) == 0) rows_extra = 0;
  ssize_t rows = rows_input + rows_extra; 
  debug_msg("edit: start refresh: rows %zd, pos: %zd,%zd (previous rows %zd, row %zd)\n", rows, rc.row, rc.col, eb->cur_rows, eb->cur_row);
  
  // only render at most terminal height rows
  ssize_t termh = term_get_height(&env->term);
  ssize_t first_row = 0;
  ssize_t last_row = rows - 1;
  if (rows > termh) {
    first_row = rc.row - termh + 1;          // ensure cursor is visible
    if (first_row < 0) first_row = 0;
    last_row = first_row + termh - 1;
  }
 
  term_start_buffered(&env->term);        // reduce flicker
  term_up(&env->term, eb->cur_row);
  
  // render rows
  edit_refresh_rows( env, eb, termw, promptw, false, first_row, last_row );
  if (rows_extra > 0) edit_refresh_rows( env, eb, termw, promptw, true, first_row + rows_input, last_row );

  // overwrite trailing rows we do not use anymore
  ssize_t rrows = last_row - first_row + 1;  // rendered rows
  if (rrows < termh && rows < eb->cur_rows) {
    ssize_t clear = eb->cur_rows - rows;
    while (rrows < termh && clear > 0) {
      clear--;
      rrows++;
      term_write(&env->term, "\r\n");
      term_clear_line(&env->term);
    }
  }
  
  // move cursor back to edit position
  term_start_of_line(&env->term);
  term_up(&env->term, first_row + rrows - 1 - rc.row );
  term_right(&env->term, rc.col + promptw);
  term_end_buffered(&env->term);

  // update previous
  eb->cur_rows = rows;
  eb->cur_row = rc.row;
}


static void edit_clear(rp_env_t* env, editor_t* eb ) {
  term_attr_reset(&env->term);  
  term_up(&env->term, eb->cur_row);
  
  // overwrite all rows
  for( ssize_t i = 0; i < eb->cur_rows; i++) {
    term_clear_line(&env->term);
    term_write(&env->term, "\r\n");    
  }
  
  // move cursor back 
  term_up(&env->term, eb->cur_rows );  
}

static void edit_clear_screen(rp_env_t* env, editor_t* eb ) {
  ssize_t cur_rows = eb->cur_rows;
  eb->cur_rows = term_get_height(&env->term) - 1;
  edit_clear(env,eb);
  eb->cur_rows = cur_rows;
  edit_refresh(env,eb);
}


//-------------------------------------------------------------
// Edit operations
//-------------------------------------------------------------
static void edit_history_prev(rp_env_t* env, editor_t* eb);
static void edit_history_next(rp_env_t* env, editor_t* eb);

static void edit_undo_restore(rp_env_t* env, editor_t* eb) {
  editor_undo_restore(eb);
  edit_refresh(env,eb);
}

static void edit_redo_restore(rp_env_t* env, editor_t* eb) {
  editor_redo_restore(eb);
  edit_refresh(env,eb);
}

static void edit_cursor_left(rp_env_t* env, editor_t* eb) {
  ssize_t prev = sbuf_prev(eb->input,eb->pos);
  if (prev < 0) return;
  rowcol_t rc;
  edit_get_rowcol( env, eb, &rc);
  ssize_t w = eb->pos - prev;
  eb->pos = prev;  
  if (!rc.first_on_row) {
    // if we were not at a start column we do not need a full refresh
    term_left(&env->term,w);
  }
  else {
    edit_refresh(env,eb);
  }
}

static void edit_cursor_right(rp_env_t* env, editor_t* eb) {
  ssize_t next = sbuf_next(eb->input,eb->pos);
  if (next <  0) return;
  rowcol_t rc;
  edit_get_rowcol( env, eb, &rc);
  ssize_t w = next - eb->pos;
  eb->pos = next;  
  if (!rc.last_on_row) {
    // if we were not at the end column we do not need a full refresh
    term_right(&env->term,w);
  }
  else {
    edit_refresh(env,eb);
  }
}

static void edit_cursor_line_end(rp_env_t* env, editor_t* eb) {
  ssize_t end = sbuf_find_line_end(eb->input,eb->pos);
  if (end < 0) return;  
  eb->pos = end; 
  edit_refresh(env,eb);
}

static void edit_cursor_line_start(rp_env_t* env, editor_t* eb) {
  ssize_t start = sbuf_find_line_start(eb->input,eb->pos);
  if (start < 0) return;
  eb->pos = start;
  edit_refresh(env,eb);
}

static void edit_cursor_next_word(rp_env_t* env, editor_t* eb) {
  ssize_t end = sbuf_find_word_end(eb->input,eb->pos);
  if (end < 0) return;
  eb->pos = end;
  edit_refresh(env,eb);
}

static void edit_cursor_prev_word(rp_env_t* env, editor_t* eb) {
  ssize_t start = sbuf_find_word_start(eb->input,eb->pos);
  if (start < 0) return;
  eb->pos = start;
  edit_refresh(env,eb);
}

static void edit_cursor_to_start(rp_env_t* env, editor_t* eb) {
  eb->pos = 0; 
  edit_refresh(env,eb);
}

static void edit_cursor_to_end(rp_env_t* env, editor_t* eb) {
  eb->pos = sbuf_len(eb->input); 
  edit_refresh(env,eb);
}


static void edit_cursor_row_up(rp_env_t* env, editor_t* eb) {
  rowcol_t rc;
  edit_get_rowcol( env, eb, &rc);
  if (rc.row == 0) {
    edit_history_prev(env,eb);
  }
  else {
    edit_set_pos_at_rowcol( env, eb, rc.row - 1, rc.col );
  }
}

static void edit_cursor_row_down(rp_env_t* env, editor_t* eb) {
  rowcol_t rc;
  ssize_t rows = edit_get_rowcol( env, eb, &rc);
  if (rc.row + 1 >= rows) {
    edit_history_next(env,eb);
  }
  else {
    edit_set_pos_at_rowcol( env, eb, rc.row + 1, rc.col );
  }
}

static void edit_backspace(rp_env_t* env, editor_t* eb) {
  if (eb->pos <= 0) return;
  editor_start_modify(eb);
  eb->pos -= sbuf_delete_char_before(eb->input,eb->pos);
  edit_refresh(env,eb);
}

static void edit_delete_char(rp_env_t* env, editor_t* eb) {
  if (eb->pos >= sbuf_len(eb->input)) return;
  editor_start_modify(eb);
  eb->pos -= sbuf_delete_char_at(eb->input,eb->pos);
  edit_refresh(env,eb);
}

static void edit_delete_all(rp_env_t* env, editor_t* eb) {
  if (sbuf_len(eb->input) <= 0) return;
  editor_start_modify(eb);
  sbuf_clear(eb->input);
  eb->pos = 0;
  edit_refresh(env,eb);
}

static void edit_delete_to_end_of_line(rp_env_t* env, editor_t* eb) { 
  ssize_t end = sbuf_find_line_end(eb->input,eb->pos);
  if (end < 0) return;
  editor_start_modify(eb);
  sbuf_delete_from_to( eb->input, eb->pos, end );
  edit_refresh(env,eb);
}

static void edit_delete_to_start_of_line(rp_env_t* env, editor_t* eb) {
  ssize_t start = sbuf_find_line_start(eb->input,eb->pos);
  if (start < 0) return;
  editor_start_modify(eb);
  sbuf_delete_from_to( eb->input, start, eb->pos );
  eb->pos = start;
  edit_refresh(env,eb);
}

static void edit_delete_line(rp_env_t* env, editor_t* eb) {
  ssize_t start = sbuf_find_line_start(eb->input,eb->pos);
  if (start < 0) return;
  ssize_t end   = sbuf_find_line_end(eb->input,eb->pos);
  if (end < 0) return;
  editor_start_modify(eb);
  // delete newline as well so no empty line is left;
  const char* s = sbuf_string(eb->input);
  bool goright = false;
  if (start > 0 && s[start-1] == '\n') {
    start--;
    // afterwards, move to start of next line if it exists (so the cursor stays on the same row)
    goright = true;
  }
  else if (s[end] == '\n') end++;
  sbuf_delete_from_to(eb->input,start,end);
  eb->pos = start;
  if (goright) edit_cursor_right(env,eb); 
  edit_refresh(env,eb);
}
 
static void edit_delete_to_start_of_word(rp_env_t* env, editor_t* eb) {
   ssize_t start = sbuf_find_word_start(eb->input,eb->pos);
  if (start < 0) return;
  editor_start_modify(eb);
  sbuf_delete_from_to( eb->input, start, eb->pos );
  eb->pos = start;
  edit_refresh(env,eb);
}

static void edit_delete_to_end_of_word(rp_env_t* env, editor_t* eb) {
  ssize_t end = sbuf_find_word_end(eb->input,eb->pos);
  if (end < 0) return;
  editor_start_modify(eb);
  sbuf_delete_from_to( eb->input, eb->pos, end );
  edit_refresh(env,eb);
}

static void edit_delete_word(rp_env_t* env, editor_t* eb) {
  ssize_t start = sbuf_find_word_start(eb->input,eb->pos);
  if (start < 0) return;
  ssize_t end   = sbuf_find_word_end(eb->input,eb->pos);
  if (end < 0) return;
  editor_start_modify(eb);  
  sbuf_delete_from_to(eb->input,start,end);
  eb->pos = start;
  edit_refresh(env,eb);
}

static void edit_swap_char( rp_env_t* env, editor_t* eb ) { 
  if (eb->pos <= 0 || eb->pos == sbuf_len(eb->input)) return;
  editor_start_modify(eb);
  eb->pos -= sbuf_swap_char(eb->input,eb->pos);
  edit_refresh(env,eb);
}

static void edit_multiline_eol(rp_env_t* env, editor_t* eb) {
  if (eb->pos < 0 || eb->pos >= sbuf_len(eb->input)) return;
  if (sbuf_string(eb->input)[eb->pos-1] != env->multiline_eol) return;
  editor_start_modify(eb);
  // replace line continuation with a real newline
  sbuf_delete_at( eb->input, eb->pos, 1);
  sbuf_insert_at( eb->input, "\n", eb->pos);  
  edit_refresh(env,eb);
}

static void edit_insert_char(rp_env_t* env, editor_t* eb, char c, bool refresh) {
  if (refresh) {
    editor_start_modify(eb);
  }
  char buf[2];
  buf[0] = c;
  buf[1] = 0;
  sbuf_insert_at( eb->input, buf, eb->pos );
  eb->pos++;
  
  // output to terminal
  if (refresh) {
    edit_refresh(env,eb);
  }
}

//-------------------------------------------------------------
// History
//-------------------------------------------------------------

static void edit_history_at(rp_env_t* env, editor_t* eb, int ofs ) 
{
  if (eb->modified) { 
    history_update(env, sbuf_string(eb->input)); // update first entry if modified
    eb->history_idx = 0;          // and start again 
    eb->modified = false;    
  }
  const char* entry = history_get(&env->history,eb->history_idx + ofs);
  debug_msg( "edit: history: at: %d + %d, found: %s\n", eb->history_idx, ofs, entry);
  if (entry == NULL) return;
  sbuf_replace( eb->input, entry);
  eb->pos = sbuf_len(eb->input);
  edit_refresh(env,eb);
}

static void edit_history_prev(rp_env_t* env, editor_t* eb) {
  edit_history_at(env,eb, 1 );
}

static void edit_history_next(rp_env_t* env, editor_t* eb) {
  edit_history_at(env,eb, -1 );
}

typedef struct hsearch_s {
  struct hsearch_s* next;
  ssize_t hidx;
  ssize_t match_pos;
  ssize_t match_len;
  bool cinsert;
} hsearch_t;

static void hsearch_push( alloc_t* mem, hsearch_t** hs, ssize_t hidx, ssize_t mpos, ssize_t mlen, bool cinsert ) {
  hsearch_t* h = mem_zalloc_tp( mem, hsearch_t );
  if (h == NULL) return;
  h->hidx = hidx;
  h->match_pos = mpos;
  h->match_len = mlen;
  h->cinsert = cinsert;
  h->next = *hs;
  *hs = h;
}

static bool hsearch_pop( hsearch_t** hs, ssize_t* hidx, ssize_t* match_pos, ssize_t* match_len, bool* cinsert ) {
  hsearch_t* h = *hs;
  if (h == NULL) return false;
  *hs = h->next;
  if (hidx != NULL)      *hidx = h->hidx;
  if (match_pos != NULL) *match_pos = h->match_pos;
  if (match_len != NULL) *match_len = h->match_len;
  if (cinsert != NULL)   *cinsert = h->cinsert;
  return true;
}

static void hsearch_done( alloc_t* mem, hsearch_t* hs ) {
  while (hs != NULL) {
    hsearch_t* next = hs->next;
    mem_free(mem, hs);
    hs = next;
  }
}

static void edit_history_search(rp_env_t* env, editor_t* eb, char* initial ) {
  // update history
  if (eb->modified) { 
    history_update(env, sbuf_string(eb->input)); // update first entry if modified
    eb->history_idx = 0;               // and start again 
    eb->modified = false;
  }

  // set a search prompt
  ssize_t old_pos = eb->pos;
  const char* prompt_text = eb->prompt_text;
  eb->prompt_text = "history search";
  
  // search state
  hsearch_t* hs = NULL;        // search undo 
  ssize_t hidx = 1;            // current history entry
  ssize_t match_pos = 0;       // current matched position
  ssize_t match_len = 0;       // length of the match
  const char* hentry = NULL;   // current history entry
  char buf[32];                // for formatting the index number

  // Simulate per character searches for each letter in `initial` (so backspace works)
  if (initial != NULL) {
    const ssize_t initial_len = rp_strlen(initial);
    ssize_t ipos = 0;
    while( ipos < initial_len ) {
      ssize_t next = str_next_ofs( initial, initial_len, ipos, eb->is_utf8, NULL );
      if (next < 0) break;
      hsearch_push( eb->mem, &hs, hidx, match_pos, match_len, true);
      char c = initial[ipos + next];  // terminate temporarily
      initial[ipos + next] = 0;
      if (history_search( &env->history, hidx, initial, true, &hidx, &match_pos )) {
        match_len = ipos + next;
      }      
      else if (ipos + next >= initial_len) {
        term_beep(&env->term);
      }
      initial[ipos + next] = c;       // restore
      ipos += next;
    }
    sbuf_replace( eb->input, initial);
    eb->pos = ipos;
  }
  else {
    sbuf_clear( eb->input );
    eb->pos = 0;
  }

  // Incremental search
again:
  hentry = history_get(&env->history,hidx);
  snprintf(buf,32,"\n\x1B[97m%zd. ", hidx);
  sbuf_append(eb->extra, buf );
  sbuf_append(eb->extra, "\x1B[90m" );         // dark gray
  sbuf_append_n( eb->extra, hentry, match_pos );  
  sbuf_append(eb->extra, "\x1B[4m\x1B[97m" );  // underline bright white
  sbuf_append_n( eb->extra, hentry + match_pos, match_len );
  sbuf_append(eb->extra, "\x1B[90m\x1B[24m" ); // no underline dark gray
  sbuf_append(eb->extra, hentry + match_pos + match_len );
  sbuf_append(eb->extra, "\n\n(use tab for the next match and backspace to go back)" );
  sbuf_append(eb->extra, "\x1B[0m\n" );
  edit_refresh(env, eb);

  // Process commands
  code_t c = tty_read(&env->tty);
  sbuf_clear(eb->extra);
  if (c == KEY_ESC || c == KEY_BELL /* ^G */ || c == KEY_CTRL_C) {
    c = 0;  
    sbuf_replace( eb->input, history_get(&env->history,0) );
    eb->pos = old_pos;
  } 
  else if (c == KEY_ENTER) {
    c = 0;
    sbuf_replace( eb->input, hentry );
    eb->pos = sbuf_len(eb->input);
    eb->modified = false;
    eb->history_idx = hidx;
  }  
  else if (c == KEY_BACKSP || c == KEY_CTRL_Z) {
    // undo last search action
    bool cinsert;
    if (hsearch_pop(&hs, &hidx, &match_pos, &match_len, &cinsert)) {
      if (cinsert) edit_backspace(env,eb);
    }
    goto again;
  }
  else if (c == KEY_CTRL_R || c == KEY_TAB || c == KEY_UP) {    
    // search backward
    hsearch_push(&env->alloc, &hs, hidx, match_pos, match_len, false);
    if (!history_search( &env->history, hidx+1, sbuf_string(eb->input), true, &hidx, &match_pos )) {
      hsearch_pop(&hs,NULL,NULL,NULL,NULL);
      term_beep(&env->term);
    };
    goto again;
  }  
  else if (c == KEY_CTRL_S || c == KEY_SHIFT_TAB || c == KEY_DOWN) {    
    // search forward
    hsearch_push(&env->alloc, &hs, hidx, match_pos, match_len, false);
    if (!history_search( &env->history, hidx-1, sbuf_string(eb->input), false, &hidx, &match_pos )) {
      hsearch_pop(&hs,NULL,NULL,NULL,NULL);
      term_beep(&env->term);
    };
    goto again;
  }  
  else {
    // insert character and search further backward
    int tofollow;
    char chr;
    if (code_is_char(&env->tty,c,&chr)) {
      hsearch_push(&env->alloc, &hs, hidx, match_pos, match_len, true);
      edit_insert_char(env,eb,chr, false /* refresh */);      
    }
    else if (code_is_extended(&env->tty,c,&chr,&tofollow)) {
      hsearch_push(&env->alloc, &hs, hidx, match_pos, match_len, true);
      edit_insert_char(env,eb,chr,false);
      while (tofollow-- > 0) {
        c = tty_read(&env->tty);
        if (code_is_follower(&env->tty,c,&chr)) {
          edit_insert_char(env,eb,chr, false);
        }
        else {
          // recover bad utf8
          tty_code_pushback(&env->tty,c);
          break;
        }
      }
      edit_refresh(env,eb);
    }
    else {
      // ignore command
      term_beep(&env->term);
      goto again;
    }
    // search for the new input
    if (history_search( &env->history, hidx, sbuf_string(eb->input), true, &hidx, &match_pos )) {
      match_len = sbuf_len(eb->input);
    }
    else {
      term_beep(&env->term);
    };
    goto again;
  }

  // done
  hsearch_done(&env->alloc,hs);
  eb->prompt_text = prompt_text;
  edit_refresh(env,eb);
  if (c != 0) tty_code_pushback(&env->tty, c);
}

// Start an incremental search with the current word 
static void edit_history_search_with_current_word(rp_env_t* env, editor_t* eb) {
  char* initial = NULL;
  ssize_t start = sbuf_find_word_start( eb->input, eb->pos );
  if (start >= 0) {
    initial = mem_strndup( eb->mem, sbuf_string(eb->input) + start, eb->pos - start);
  }
  edit_history_search( env, eb, initial);
  mem_free(&env->alloc, initial);
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
  "","We use ^<key> as a shorthand for ctrl-<key>.",
  "","",
  "","Navigation:",
  "left,"
  "^b",         "go one character to the left",
  "right,"
  "^f",         "go one character to the right",
  "up",         "go one row up, or back in the history",
  "down",       "go one row down, or forward in the history",  
  #ifdef __APPLE__
  "shift-left",
  #else
  "^left",     
  #endif
                "go to the start of the previous word",
  #ifdef __APPLE__
  "shift-right",
  #else
  "^right",
  #endif
                "go to the end the current word",
  "home,"
  "^a",         "go to the start of the current line",  
  "end,"
  "^e",         "go to the end of the current line",
  "pgup,"
  "^home",       "go to the start of the current input",
  "pgdn,"
  "^end",       "go to the end of the current input",
  "^p",         "go back in the history",
  "^n",         "go forward in the history",
  "^r,^s",      "search the history starting with the current word",
  "","",
  
  "", "Deletion:",
  "del,^d",     "delete the current character",
  "backsp,^h",  "delete the previous character",
  "^w",         "delete to preceding white space",
  "alt-backsp", "delete to the start of the current word",
  "alt-d",      "delete to the end of the current word",
  "^u",         "delete to the start of the current line",
  "^k",         "delete to the end of the current line",
  "esc",        "delete the current line, or done with empty input",  
  "","",
  
  "", "Editing:",
  "enter",      "accept current input",
  #ifndef __APPLE__
  "^enter, ^j", "", "shift-tab"
  #else
  "shift-tab,^j",
  #endif
                "create a new line for multi-line input",
  //" ",          "(or type '\\' followed by enter)",
  "^l",         "clear screen",
  "^t",         "swap with previous character (move character backward)",
  "^z,^_",      "undo",
  "^y",         "redo",
  //"^C",         "done with empty input",
  //"F1",         "show this help",
  "tab",        "try to complete the current input",
  "","",
  "","In the completion menu:",
  "enter,"
  "space",      "use the currently selected completion",
  "1 - 9",      "use completion N from the menu",
  "tab",        "select the next completion",
  "cursor keys","select completion N in the menu",
  "esc",        "exit menu without completing",
  "pgdn,", "",
  "shift-tab",  "show all further possible completions",
  "","",
  "","In incremental history search:",
  "enter",      "use the currently found history entry",
  "backsp,"
  "^z",         "go back to the previous match (undo)",
  "tab,"
  "^r",         "find the next match",
  "shift-tab,"
  "^s",         "find an earlier match",
  "esc",        "exit search",
  " ","",
  NULL, NULL
};
 
static void edit_show_help( rp_env_t* env, editor_t* eb ) {
  for (ssize_t i = 0; help[i] != NULL && help[i+1] != NULL; i+=2) {
    if (help[i][0] == 0) {
      term_writef(&env->term, "\x1B[90m%s\x1B[0m\r\n", help[i+1]);
    }
    else {
      term_writef(&env->term, "  \x1B[97m%-13s\x1B[0m%s%s\r\n", help[i], (help[i+1][0] == 0 ? "" : ": "), help[i+1]);
    }
  }
  eb->cur_rows = 0;
  eb->cur_row = 0;
  edit_refresh(env,eb);   
}

//-------------------------------------------------------------
// Completion
//-------------------------------------------------------------

static void edit_complete(rp_env_t* env, editor_t* eb, ssize_t idx) {
  completion_t* cm = completions_get(env,idx);
  if (cm == NULL) return;
  editor_start_modify(eb);

  eb->pos = completion_apply(cm, eb->input, eb->pos);
  edit_refresh(env,eb);
}

static void editor_append_completion(editor_t* eb, completion_t* cm, ssize_t idx, ssize_t width, bool numbered, bool selected ) {
  if (cm == NULL) return;
  if (numbered) {
    char buf[32];
    snprintf(buf, 32, "\x1B[90m%s%zd \x1B[0m", (selected ? (eb->is_utf8 ? "\xE2\x86\x92" : "*") : " "), 1 + idx);
    sbuf_append(eb->extra, buf);
    width -= 3;
  }
  const char* s = (cm->display==NULL ? cm->replacement : cm->display);
  if (width <= 0) {
    sbuf_append(eb->extra, s);
  }
  else {
    // fit to width
    const char* sc = str_skip_until_fit( s, width, eb->is_utf8);
    if (sc != s) {
      sbuf_append( eb->extra, "...");
      sc = str_skip_until_fit( s, width - 3, eb->is_utf8);
    }
    sbuf_append( eb->extra, sc);
    // fill out with spaces
    ssize_t n = width - str_column_width(sc, eb->is_utf8);
    while( n-- > 0 ) { sbuf_append( eb->extra," "); }  
  }
}

// 2 and 3 column output up to 80 wide
#define RP_DISPLAY2_MAX    35
#define RP_DISPLAY2_COL    (3+RP_DISPLAY2_MAX)
#define RP_DISPLAY2_WIDTH  (2*RP_DISPLAY2_COL + 2)    // 78

#define RP_DISPLAY3_MAX    22
#define RP_DISPLAY3_COL    (3+RP_DISPLAY3_MAX)
#define RP_DISPLAY3_WIDTH  (3*RP_DISPLAY3_COL + 2*2)  // 79

static void editor_append_completion2(rp_env_t* env, editor_t* eb, ssize_t idx1, ssize_t idx2, ssize_t selected ) {  
  editor_append_completion(eb, completions_get(env,idx1), idx1, RP_DISPLAY2_COL, true, (idx1 == selected) );
  sbuf_append( eb->extra, "  ");
  editor_append_completion(eb, completions_get(env,idx2), idx2, RP_DISPLAY2_COL, true, (idx2 == selected) );
}

static void editor_append_completion3(rp_env_t* env, editor_t* eb, ssize_t idx1, ssize_t idx2, ssize_t idx3, ssize_t selected ) {  
  editor_append_completion(eb, completions_get(env,idx1), idx1, RP_DISPLAY3_COL, true, (idx1 == selected) );
  sbuf_append( eb->extra, "  ");
  editor_append_completion(eb, completions_get(env,idx2), idx2, RP_DISPLAY3_COL, true, (idx2 == selected) );
  sbuf_append( eb->extra, "  ");
  editor_append_completion(eb, completions_get(env,idx3), idx3, RP_DISPLAY3_COL, true, (idx3 == selected) );
}

static ssize_t edit_completions_max_width( rp_env_t* env, editor_t* eb, ssize_t count ) {
  ssize_t max_width = 0;
  for( ssize_t i = 0; i < count; i++) {
    completion_t* cm = completions_get(env,i);
    if (cm != NULL) {
      ssize_t w = str_column_width( (cm->display != NULL ? cm->display : cm->replacement), eb->is_utf8);
      if (w > max_width) max_width = w;
    }
  }
  return max_width;
}

static void edit_completion_menu(rp_env_t* env, editor_t* eb, bool more_available) {
  ssize_t count  = completions_count( env );
  ssize_t count_displayed = count;
  assert(count > 1);
  ssize_t selected = 0;
  ssize_t columns  = 1;
  ssize_t percolumn= count;
  
again: 
  // show first 9 (or 8) completions
  sbuf_clear(eb->extra);
  ssize_t twidth = term_get_width(&env->term);   
  if (count > 3 && twidth > RP_DISPLAY3_WIDTH && edit_completions_max_width(env,eb,9) <= RP_DISPLAY3_MAX) {
    // display as a 3 column block
    count_displayed = (count > 9 ? 9 : count);
    columns = 3;
    percolumn = 3;
    for( ssize_t rw = 0; rw < percolumn; rw++ ) {
      sbuf_append( eb->extra, "\n");
      editor_append_completion3( env, eb, rw, percolumn+rw, (2*percolumn)+rw, selected );
    }
  }
  else if (count > 4 && twidth > RP_DISPLAY2_WIDTH && edit_completions_max_width(env,eb,8) <= RP_DISPLAY2_MAX) {
    // display as a 2 column block if some entries are too wide for three columns
    count_displayed = (count > 8 ? 8 : count);
    columns = 2;
    percolumn = (count_displayed <= 6 ? 3 : 4);
    for( ssize_t rw = 0; rw < percolumn; rw++ ) {
      sbuf_append( eb->extra, "\n");
      editor_append_completion2( env, eb, rw, percolumn+rw, selected );
    }
  }
  else {
    // display as a list
    count_displayed = (count > 9 ? 9 : count);    
    columns = 1;
    percolumn = count_displayed;
    for(ssize_t i = 0; i < count_displayed; i++) {
      sbuf_append( eb->extra, "\n");
      editor_append_completion(eb, completions_get(env,i), i, -1, true /* numbered */, selected == i );        
    }
  }
  if (count > count_displayed) {
    sbuf_append(eb->extra, "\n\x1B[90m(press shift-tab to see all further completions)\x1B[0m");
  }   
  edit_refresh(env,eb);
  
  // read here; if not a valid key, push it back and return to main event loop
  code_t c = tty_read(&env->tty);
  sbuf_clear(eb->extra);      
  if (c >= '1' && c <= '9' && (ssize_t)(c - '1') < count) {
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
    c = 0;      
    edit_complete(env, eb, selected);
  }
  else if ((c == KEY_PAGEDOWN || c == KEY_SHIFT_TAB || c == KEY_LINEFEED) && count > 9) {
    // show all completions
    c = 0;
    if (more_available) {
      // generate all entries (up to the max (= 1000))
      count = completions_generate(env, sbuf_string(eb->input), eb->pos, RP_MAX_COMPLETIONS_TO_SHOW);
    }
    rowcol_t rc;
    edit_get_rowcol(env,eb,&rc);
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
    if (count >= RP_MAX_COMPLETIONS_TO_SHOW) {
      term_write(&env->term, "\x1B[90m... and more.\x1B[0m\r\n");
    }
    for(ssize_t i = 0; i < rc.row+1; i++) {
      term_write(&env->term, " \r\n");
    }
    eb->cur_rows = 0;
    edit_refresh(env,eb);      
  }
  // done
  completions_clear(env);      
  if (c != 0) tty_code_pushback(&env->tty,c);
}

static void edit_generate_completions(rp_env_t* env, editor_t* eb) {
  debug_msg( "edit: complete: %zd: %s\n", eb->pos, sbuf_string(eb->input) );
  if (eb->pos <= 0) return;
  ssize_t count = completions_generate(env, sbuf_string(eb->input), eb->pos, 10);
  if (count <= 0) {
    // no completions
    term_beep(&env->term); 
  }
  else if (count == 1) {
    // complete if only one match    
    edit_complete(env,eb,0);
  }
  else {
    edit_completion_menu( env, eb, count>=10 /* possibly more available? */);    
  }
}


//-------------------------------------------------------------
// Edit line
//-------------------------------------------------------------

static char* edit_line( rp_env_t* env, const char* prompt_text )
{
  // set up an edit buffer
  editor_t eb;
  eb.mem      = &env->alloc;
  eb.input    = sbuf_alloc(eb.mem, env->tty.is_utf8);
  eb.extra    = sbuf_alloc(eb.mem, env->tty.is_utf8);
  eb.pos      = 0;
  eb.cur_rows = 1; 
  eb.cur_row  = 0; 
  eb.modified = false;
  eb.is_utf8  = env->tty.is_utf8;
  eb.prompt_text   = (prompt_text != NULL ? prompt_text : "");
  eb.history_idx   = 0;
  editstate_init(&eb.undo);
  editstate_init(&eb.redo);

  // show prompt
  edit_write_prompt(env, &eb, 0, false); 

  // always a history entry for the current input
  history_push(env, "");

  // process keys
  code_t c;          // current key code
  int tofollow = 0;  // utf8 extended characters to still follow (to delay refresh)
  while(true) {    
    // read a character
    c = tty_read(&env->tty);
    if (c < 0) break;

    // update width as late as possible so a user can resize even if the prompt is already visible
    //if (eb.len == 1) 
    if (term_update_dim(&env->term,&env->tty)) {
      // eb.cur_rows = env->term.height;
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

    // Operations that may return
    if (c == KEY_ENTER) {
      if (!env->singleline_only && eb.pos > 0 && sbuf_string(eb.input)[eb.pos-1] == env->multiline_eol && edit_pos_is_at_row_end(env,&eb)) {
        // replace line-continuation with newline
        edit_multiline_eol(env,&eb);        
      }
      else {
        // otherwise done
        break;
      }
    } 
    else if (c == KEY_CTRL_D) {
      if (eb.pos == 0 && editor_pos_is_at_end(&eb)) break; // ctrl+D on empty quits with NULL
      edit_delete_char(env,&eb);     // otherwise it is like delete
    } 
    else if (c == KEY_CTRL_C) {
      break; // ctrl+C quits with NULL
    }
    else if (c == KEY_ESC) {
      if (eb.pos == 0 && editor_pos_is_at_end(&eb)) break;  // ESC on empty input returns with empty input
      edit_delete_line(env,&eb);  // otherwise delete the current line
    }
    else if (c == KEY_BELL /* ^G */) {
      edit_delete_all(env,&eb);
      break; // ctrl+G cancels (and returns empty input)
    }
    // Editing Operations
    else switch(c) {
      case KEY_SHIFT_TAB:
      case KEY_LINEFEED: // '\n' (ctrl+J, shift+enter)
        if (!env->singleline_only) { edit_insert_char(env,&eb,'\n',true); }
        break;
      case KEY_TAB:
      case WITH_ALT('?'):
        edit_generate_completions(env,&eb);
        break;
      case KEY_CTRL_R:
      case KEY_CTRL_S:
        edit_history_search_with_current_word(env,&eb);
        break;
      case KEY_LEFT:
      case KEY_CTRL_B:
        edit_cursor_left(env,&eb);
        break;
      case KEY_RIGHT:
      case KEY_CTRL_F:
        edit_cursor_right(env,&eb);
        break;
      case KEY_UP:
        edit_cursor_row_up(env,&eb);
        break;
      case KEY_DOWN:
        edit_cursor_row_down(env,&eb);
        break;                 
      case KEY_HOME:
      case KEY_CTRL_A:
        edit_cursor_line_start(env,&eb);
        break;
      case KEY_END:
      case KEY_CTRL_E:
        edit_cursor_line_end(env,&eb);
        break;
      case KEY_CTRL_LEFT:
      case WITH_SHIFT(KEY_LEFT):    
      case WITH_ALT('b'):
        edit_cursor_prev_word(env,&eb);
        break;
      case KEY_CTRL_RIGHT:
      case WITH_SHIFT(KEY_RIGHT):      
      case WITH_ALT('f'):
        edit_cursor_next_word(env,&eb);
        break;      
      case KEY_CTRL_HOME:
      case WITH_SHIFT(KEY_HOME):      
      case KEY_PAGEUP:
      case WITH_ALT('<'):
        edit_cursor_to_start(env,&eb);
        break;
      case KEY_CTRL_END:
      case WITH_SHIFT(KEY_END):      
      case KEY_PAGEDOWN:
      case WITH_ALT('>'):
        edit_cursor_to_end(env,&eb);
        break;
      case KEY_BACKSP:
        edit_backspace(env,&eb);
        break;
      case KEY_DEL:
        edit_delete_char(env,&eb);
        break;
      case WITH_ALT('d'):
        edit_delete_to_end_of_word(env,&eb);
        break;
      case KEY_CTRL_W:
      case WITH_ALT(KEY_DEL):
      case WITH_ALT(KEY_BACKSP):
        edit_delete_to_start_of_word(env,&eb);
        break;      
        // edit_delete_word?
      case KEY_CTRL_U:
        edit_delete_to_start_of_line(env,&eb);
        break;
      case KEY_CTRL_K:
        edit_delete_to_end_of_line(env,&eb);
        break;
      case KEY_CTRL_P:
        edit_history_prev(env,&eb);
        break;
      case KEY_CTRL_N:
        edit_history_next(env,&eb);
        break;
      case KEY_CTRL_L: 
        edit_clear_screen(env,&eb);
        break;
      case KEY_CTRL_T:
        edit_swap_char(env,&eb);
        break;
      case KEY_CTRL_Z:
      case WITH_CTRL('_'):
        edit_undo_restore(env,&eb);
        break;
      case KEY_CTRL_Y:
        edit_redo_restore(env,&eb);
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
          debug_msg( "edit: ignore code: 0x%04x\n", c);
        }
        break;
      }
    }
  }

  // goto end
  eb.pos = sbuf_len(eb.input);  
  edit_refresh(env,&eb);

  // save result
  char* res = ((c == KEY_CTRL_D && sbuf_len(eb.input) == 0) || c == KEY_CTRL_C) ? NULL : sbuf_strdup(eb.input);

  // free resources 
  editstate_done(&env->alloc, &eb.undo);
  editstate_done(&env->alloc, &eb.redo);
  sbuf_free(eb.input);
  sbuf_free(eb.extra);
  
  // update history
  if (res == NULL || res[0] == 0 || res[1] == 0) rp_history_remove_last(env);  // no empty or single-char entries
  history_update(env,res);
  history_save(env);
  return res;
}

