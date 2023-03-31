/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/

//-------------------------------------------------------------
// History search: this file is included in editline.c
//-------------------------------------------------------------

static void edit_history_at(ic_env_t* env, editor_t* eb, int ofs)
{
  // eb->history_idx += ofs;
/// TODO disabled pushing history entry if input buffer is modified ... check if this is ok
#if 0
  if (eb->modified) { 
    history_push(env->history, sbuf_string(eb->input)); // update first entry if modified
    eb->history_idx = 0;                                // and start again 
    eb->modified = false;    
  }
#endif
  if (ofs < 0 && eb->history_idx + ofs < 0) return;
  if (ofs > 0 && eb->history_idx + ofs >
  // if (ofs < 0 && eb->history_idx < 0) return;
  // if (ofs > 0 && eb->history_idx >
    history_count_with_prefix(env->history, sbuf_string(eb->input))) return;
  const char* entry = history_get_with_prefix(env->history,
    eb->history_idx + ofs, sbuf_string(eb->input));
  debug_msg( "edit history at: %d + %d, found: %s\n", eb->history_idx, ofs, entry);
  if (entry == NULL) {
    term_beep(env->term);
    sbuf_replace(eb->hint, "");
    // edit_refresh(env, eb);
  }
  else {
    // eb->history_idx += ofs;
    sbuf_replace(eb->hint, entry + sbuf_len(eb->input));
#ifdef IC_HIST_IMPL_SQLITE
    env->mem->free((char *)entry);
#endif
  /// TODO disabled setting cursor pos when browsing history ... check if this is ok
#if 0
    if (ofs > 0) {
      // at end of first line when scrolling up
      ssize_t end = sbuf_find_line_end(eb->input,0);
      eb->pos = (end < 0 ? 0 : end);
    }
    else {
      eb->pos = sbuf_len(eb->input);    // at end of last line when scrolling down
    }
#endif
    // edit_refresh(env, eb);
  }
  edit_refresh(env, eb);
  eb->history_idx += ofs;
}

static void edit_history_prev(ic_env_t* env, editor_t* eb) {
  edit_history_at(env, eb, 1);
}

static void edit_history_next(ic_env_t* env, editor_t* eb) {
  edit_history_at(env, eb, -1);
}

static void edit_history_prev_word(ic_env_t* env, editor_t* eb) {
  if (eb->history_wordpos == 0) eb->history_idx++;
  const char* entry = history_get_with_prefix(env->history, eb->history_idx, "");
  if (entry == NULL) {
    term_beep(env->term);
    return;
  }
  stringbuf_t *entry_s = sbuf_new(eb->mem);
  sbuf_append(entry_s, entry);
  ssize_t word_start = eb->history_wordpos;
  ssize_t word_end = eb->history_wordpos;
  if (word_start == 0) {
    word_end = sbuf_len(entry_s);
  }
  word_start = sbuf_find_word_start(entry_s, word_end);
  ssize_t word_start_ws = sbuf_find_ws_word_start(entry_s, word_end);
  debug_msg( "edit history: prev word: %d, entry: %s, start: %d, start_ws: %d, end: %d\n",
    eb->history_idx, entry, word_start, word_start_ws, word_end);
  sbuf_clear(eb->hint);
  sbuf_append_n(eb->hint, entry + word_start_ws, word_end - word_start_ws);
  eb->history_wordpos = word_start;
  edit_refresh(env, eb);
  sbuf_free(entry_s);
#ifdef IC_HIST_IMPL_SQLITE
  env->mem->free((char *)entry);
#endif
}
