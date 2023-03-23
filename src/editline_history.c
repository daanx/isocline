/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/

//-------------------------------------------------------------
// History search: this file is included in editline.c
//-------------------------------------------------------------

static void edit_history_at(ic_env_t* env, editor_t* eb, int ofs ) 
{
  if (eb->modified) { 
    history_push(env->history, sbuf_string(eb->input)); // update first entry if modified
    eb->history_idx = 0;                                // and start again 
    eb->modified = false;    
  }
  const char* entry = history_get_with_prefix(env->history,eb->history_idx + ofs, sbuf_string(eb->input));
  debug_msg( "edit: history: at: %d + %d, found: %s, edit_buf: %s\n", eb->history_idx, ofs, entry, sbuf_string(eb->input));
  if (entry == NULL) {
    term_beep(env->term);
  }
  else {
    eb->history_idx += ofs;
    sbuf_replace(eb->hint, entry + sbuf_len(eb->input));
    env->mem->free((char *)entry);
    if (ofs > 0) {
      // at end of first line when scrolling up
      ssize_t end = sbuf_find_line_end(eb->input,0);
      eb->pos = (end < 0 ? 0 : end);
    }
    else {
      eb->pos = sbuf_len(eb->input);    // at end of last line when scrolling down
    }
    edit_refresh(env, eb);
  }
}

static void edit_history_prev(ic_env_t* env, editor_t* eb) {
  edit_history_at(env,eb, 1);
}

static void edit_history_next(ic_env_t* env, editor_t* eb) {
  edit_history_at(env,eb, -1);
}
