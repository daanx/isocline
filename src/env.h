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
  alloc_t*        mem;
  rp_env_t*       next;
  term_t*         term;
  tty_t*          tty;
  completions_t*  completions;
  history_t*      history;  
  const char*     prompt_marker;
  rp_color_t      prompt_color;
  char            multiline_eol;  
  bool            initialized;
  bool            noedit;
  bool            singleline_only;
};

rp_private char*    rp_editline(rp_env_t* env, const char* prompt_text);



#endif // RP_ENV_H
