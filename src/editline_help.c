/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/

//-------------------------------------------------------------
// Help: this is included into editline.c
//-------------------------------------------------------------

static const char* help[] = {
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
  "^enter, ^j", "",
  "shift-tab",
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
  "enter,left", "use the currently selected completion",
  "1 - 9",      "use completion N from the menu",
  "tab,down",   "select the next completion",
  "shift-tab,up","select the previous completion",
  "esc",        "exit menu without completing",
  "pgdn,^j",    "show all further possible completions",
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

static const char* help_initial[] = {
  "\x1B[90m"
  "Isocline v1.0, copyright (c) 2021 Daan Leijen.",
  "This is free software; you can redistribute it and/or",
  "modify it under the terms of the MIT License.",
  "See <\x1B[4mhttps://github.com/daanx/isocline\x1B[24m> for further information.",
  "We use ^<key> as a shorthand for ctrl-<key>.",
  "",
  "\x1B[90m"  
  "Overview:",
  "\x1B[90m"
  "",
  "       home,ctrl-a      cursor     end,ctrl-e",
  "         ┌────────────────┼───────────────┐    (navigate)",
  //"       │                │               │",
  #ifndef __APPLE__
  "         │    ctrl-left   │  ctrl-right   │",
  #else
  "         │     alt-left   │   alt-right   │",
  #endif
  "         │        ┌───────┼──────┐        │    ctrl+r   : search history",
  "         ▼        ▼       ▼      ▼        ▼    tab      : complete word",
  "  \x1B[90mprompt> \x1B[37mit's the quintessential language" "\x1B[90m" "     shift-tab: insert new line",
  "         ▲        ▲              ▲        ▲    esc      : delete line, done",
  "         │        └──────────────┘        │    ctrl+z   : undo",
  "         │   alt-backsp        alt-d      │",
  //"       │                │               │",
  "         └────────────────────────────────┘    (delete)",
  "       ctrl-u                          ctrl-k",
  "\x1B[0m",
  NULL
};


static void edit_show_help(ic_env_t* env, editor_t* eb) {
  edit_clear(env, eb);
  for( ssize_t i = 0; help_initial[i] != NULL; i++ ) {
    term_writeln(env->term, help_initial[i]);
  }
  for (ssize_t i = 0; help[i] != NULL && help[i+1] != NULL; i += 2) {
    if (help[i][0] == 0) {  
      term_color(env->term, IC_ANSI_DARKGRAY);         
      term_writef(env->term, 256, "%s\x1B[0m\n", help[i+1]);
    }
    else {
      term_color(env->term, IC_RGB(0xFFFFD7));
      term_writef(env->term, 256, "  %-13s\x1B[0m%s%s\n", help[i], (help[i+1][0] == 0 ? "" : ": "), help[i+1]);
    }
  }
  term_attr_reset(env->term);

  eb->cur_rows = 0;
  eb->cur_row = 0;
  edit_refresh(env, eb);
}
