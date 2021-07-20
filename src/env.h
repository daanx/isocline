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
  const char*     prompt_marker;    // the prompt marker (NULL is default "> ")
  rp_color_t      prompt_color;     // color used to display the prompt
  char            multiline_eol;    // character used for multiline input ("\")
  bool            initialized;      // are we initialized?
  bool            noedit;           // is rich editing possible (tty != NULL)
  bool            singleline_only;  // allow only single line editing?
};

rp_private char*    rp_editline(rp_env_t* env, const char* prompt_text);



#endif // RP_ENV_H
