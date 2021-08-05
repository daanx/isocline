/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#pragma once
#ifndef IC_ENV_H
#define IC_ENV_H

#include "../include/isocline.h"
#include "common.h"
#include "term.h"
#include "tty.h"
#include "stringbuf.h"
#include "history.h"
#include "completions.h"

//-------------------------------------------------------------
// Environment
//-------------------------------------------------------------

struct ic_env_s {
  alloc_t*        mem;              // potential custom allocator
  ic_env_t*       next;             // next environment (used for proper deallocation)
  term_t*         term;             // terminal
  tty_t*          tty;              // keyboard (NULL if stdin is a pipe, file, etc)
  completions_t*  completions;      // current completions
  history_t*      history;          // edit history
  const char*     prompt_marker;    // the prompt marker (defaults to "> ")
  const char*     cprompt_marker;   // prompt marker for continuation lines (defaults to `prompt_marker`)
  ic_highlight_fun_t* highlighter;  // highlight callback
  void*           highlighter_arg;  // user state for the highlighter.
  ic_color_t      color_prompt;     // color used to display the prompt
  ic_color_t      color_info;       // information color, for example numbers in the completion menu. (=IC_DARKGRAY)
  ic_color_t      color_diminish;   // diminish color, for example the non-highlighted part in a history search (=IC_DARKGRAY)
  ic_color_t      color_emphasis;   // highlighted color, for example, the current match in a history search (=IC_DEFAULT_COLOR)
  ic_color_t      color_hint;       // hint color.
  ic_color_t      color_error;      // error color.
  ic_color_t      color_bracematch; // brace match color.
  const char*     brace_pairs;      // matching braces, e.g "()[]{}"
  char            multiline_eol;    // character used for multiline input ("\") (set to 0 to disable)
  bool            initialized;      // are we initialized?
  bool            noedit;           // is rich editing possible (tty != NULL)
  bool            singleline_only;  // allow only single line editing?
  bool            complete_nopreview; // do not show completion preview for each selection in the completion menu?
  bool            complete_autotab; // try to keep completing after a completion?
  bool            no_multiline_indent; // indent continuation lines to line up under the initial prompt 
  bool            no_help;          // show short help line for history search etc.
  bool            no_hint;          // allow hinting?
  bool            no_highlight;     // enable highlighting?
  bool            no_bracematch;    // enable brace matching?
  long            hint_delay;       // delay before displaying a hint in milliseconds
};

ic_private char*        ic_editline(ic_env_t* env, const char* prompt_text);

ic_private ic_env_t*    ic_get_env(void);
ic_private const char*  ic_env_get_brace_pairs(ic_env_t* env);

#endif // IC_ENV_H
