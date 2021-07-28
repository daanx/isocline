/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#pragma once
#ifndef RP_ENV_H
#define RP_ENV_H

#include "../include/repline.h"
#include "common.h"
#include "term.h"
#include "tty.h"
#include "stringbuf.h"
#include "history.h"
#include "completions.h"

//-------------------------------------------------------------
// Environment
//-------------------------------------------------------------

struct rp_env_s {
  alloc_t*        mem;              // potential custom allocator
  rp_env_t*       next;             // next environment (used for proper deallocation)
  term_t*         term;             // terminal
  tty_t*          tty;              // keyboard (NULL if stdin is a pipe, file, etc)
  completions_t*  completions;      // current completions
  history_t*      history;          // edit history
  stringbuf_t*    input;            // keep a current input buffer to avoid reallocation
  stringbuf_t*    extra;            // keep a current extra buffer to avoid reallocation
  stringbuf_t*    hint;             // keep a current hint buffer to avoid reallocation
  const char*     prompt_marker;    // the prompt marker (defaults to "> ")
  const char*     cprompt_marker;   // prompt marker for continuation lines (defaults to `prompt_marker`)
  rp_color_t      prompt_color;     // color used to display the prompt
  char            multiline_eol;    // character used for multiline input ("\")
  bool            initialized;      // are we initialized?
  bool            noedit;           // is rich editing possible (tty != NULL)
  bool            singleline_only;  // allow only single line editing?
  bool            complete_nopreview; // do not show completion preview for each selection in the completion menu?
  bool            complete_autotab; // try to keep completing after a completion?
  bool            no_multiline_indent; // indent continuation lines to line up under the initial prompt 
  bool            no_help;          // show short help line for history search etc.
  bool            no_hint;          // allow hinting?
  rp_color_t      color_info;       // information color, for example numbers in the completion menu. (=RP_DARKGRAY)
  rp_color_t      color_diminish;   // diminish color, for example the non-highlighted part in a history search (=RP_DARKGRAY)
  rp_color_t      color_highlight;  // highlighted color, for example, the current match in a history search (=RP_DEFAULT_COLOR)
};

rp_private char*      rp_editline(rp_env_t* env, const char* prompt_text);

rp_private rp_env_t*  rp_get_env(void);

#endif // RP_ENV_H
