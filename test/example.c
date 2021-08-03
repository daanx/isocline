/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.

  Example use of the Isocline API.
-----------------------------------------------------------------------------*/
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "isocline.h"

// completion function defined below
static void completer(ic_completion_env_t* cenv, const char* prefix );

// highlighter function defined below
static void highlighter(ic_highlight_env_t* henv, const char* input, void* arg);


static void show_color( ic_color_t color, const char* name ) {
  printf("\x1B[%dm%16s\x1B[0m | \x1B[1;%dmbold\x1B[0m | \x1B[%dmbright\x1B[0m\n", color, name, color, color+60);  
}


// main example
int main() 
{
  // ic_writeln handles basic escape sequences in a portable way
  ic_writeln(
    "\n\x1B[33mIsocline sample program:\x1B[0m\n"
    "- Type 'exit' to quit. (or use ctrl+d).\n"
    "- Press F1 for help on editing commands.\n"
    "- Use shift+tab for multiline input. (or ctrl+enter, or ctrl+j)\n"
    "- Type 'p' (or 'id', 'f', or 'h') followed by tab for completion.\n");
  
  printf("terminal color bits: %d\n", ic_term_get_color_bits());

#define AT "@"

  ic_writeln("colors rgb:"); 
  ic_write("\n\n  // 9x9x17 colors");
  for (int b = 0; b <= 256; b += 16) {
    ic_term_reset();
    ic_write("\n  ");
    for (int g = 0; g <= 256; g += 32) {
      for (int r = 0; r <= 256; r += 32) {
        ic_term_color(IC_RGBX(r, g, b));
        ic_write(AT);
      }
      ic_write(" ");
    }
  }
  ic_term_reset();
  ic_write("\n\n");
  
  ic_write("\n\n  // 9x17x9 colors");
  for (int g = 0; g <= 256; g += 16) {
    ic_term_reset();
    ic_write("\n  ");
    for (int r = 0; r <= 256; r += 32) {
      for (int b = 0; b <= 256; b += 32) {
        ic_term_color(IC_RGBX(r, g, b));
        ic_write(AT);
      }
      ic_write(" ");
    }
  }
  ic_term_reset();
  ic_write("\n\n");
  
  ic_write("\n\n  // 17x9x9 colors");
  for (int r = 0; r <= 256; r += 16) {    
    ic_term_reset();
    ic_write("\n  ");
    for(int g = 0; g <= 256; g += 32) {
      for (int b = 0; b <= 256; b += 32) {
        ic_term_color(IC_RGBX(r,g,b));      
        ic_write(AT);        
      }
      ic_write(" ");
    }    
  } 
  ic_term_reset();

  ic_writeln("\n\nansi shades:");
  ic_term_color(IC_ANSI_MAROON); ic_write("ansi8 ");
  ic_term_color(IC_ANSI_RED); ic_write("ansi16 ");
  ic_term_color(IC_RGB(0xDFAF87)); ic_write("ansi256 ");
  ic_term_color(IC_RGBX(100, 255, 180)); ic_write("rgb");
  ic_term_reset(); ic_writeln("");

  // shades
  for (int i = 0; i <= 64; i++) {
    ic_term_color(IC_RGBX((i==64 ? 255 : i*4), 0, 0)); ic_write((i%8==0 ? "#" : AT));
  }
  ic_writeln("");  
  for (int i = 0; i <= 64; i++) {
    ic_term_color(IC_RGBX(0, (i==64 ? 255 : i*4), 0)); ic_write((i%8==0 ? "#" : AT));
  }
  ic_writeln("");  
  for (int i = 0; i <= 64; i++) {
    ic_term_color(IC_RGBX(0, 0, (i==64 ? 255 : i*4))); ic_write((i%8==0 ? "#" : AT));
  }
  ic_writeln("");  
  for (int i = 0; i <= 64; i++) {
    int g = (i==64 ? 255 : i*4);
    ic_term_color(IC_RGBX(g, g, g)); ic_write((i%8==0 ? "#" : AT));
  }
  ic_writeln("");
  ic_term_reset();
  
  ic_write("\n\nansi escape colors:\n");
  
  show_color(IC_ANSI_BLACK,"black");
  show_color(IC_ANSI_MAROON,"maroon");
  show_color(IC_ANSI_GREEN,"green");
  show_color(IC_ANSI_ORANGE,"orange/brown");
  show_color(IC_ANSI_NAVY,"navy");
  show_color(IC_ANSI_PURPLE,"purple");
  show_color(IC_ANSI_TEAL,"teal");
  show_color(IC_ANSI_LIGHTGRAY,"lighgray/white");
  show_color(IC_ANSI_DEFAULT,"default");
  
  ic_term_reset();
  ic_writeln("");

  // enable history; use a NULL filename to not persist history to disk
  ic_set_history("history.txt", -1 /* default entries (= 200) */);

  // enable completion with a default completion function
  ic_set_default_completer(&completer, NULL);

  // enable syntax highlighting with a highlight function
  ic_set_default_highlighter(highlighter, NULL);

  // try to auto complete after a completion as long as the completion is unique
  ic_enable_auto_tab(true );

  // change interface colors (prompt info, diminish, emphasis, hint)
  // ic_set_style_color( IC_STYLE_PROMPT,   IC_ANSI_MAROON);
  // ic_set_style_color( IC_STYLE_EMPHASIS, IC_RGB(0xD7FF00));
  
  // run until empty input
  char* input;
  while((input = ic_readline("isoclinε")) != NULL)    // ctrl-d return NULL (as well as errors)
  {
    bool stop = (strcmp(input,"exit") == 0 || strcmp(input,"") == 0); 
    printf("-----\n"           // echo the input
           "%s\n"
           "-----\n", input );    
    free(input);               // do not forget to free the returned input!
    if (stop) break;
  }
  printf("done\n");
  return 0;
}

// -------------------------------------------------------------------------------
// Completion
// -------------------------------------------------------------------------------

// A custom completer function.
// Use `ic_add_completion( env, display, replacement)` to add actual completions.
static void word_completer(ic_completion_env_t* cenv, const char* prefix ) 
{
  // complete with list of words; only if the input is a prefix it will be a completion candidate
  static const char* completions[] = { "print", "println", "printer", "printsln", "prompt", NULL };
  ic_add_completions(cenv, prefix, completions);

  // examples of more customized completions
  if (strcmp(prefix,"id") == 0) {
    // display vs. replacement
    ic_add_completion(cenv,"D — (x) => x",       "(x) => x");                
    ic_add_completion(cenv,"Haskell — \\x -> x", "\\x -> x");
    ic_add_completion(cenv,"Idris — \\x => x",   "\\x => x");
    ic_add_completion(cenv,"Koka — fn(x){ x }",  "fn(x){ x }");    
    ic_add_completion(cenv,"Ocaml — fun x -> x", "fun x -> x");
  }  
  else if (strcmp(prefix,"f") == 0) {  
    // unicode for f completion
    ic_add_completion(cenv,NULL,"banana 🍌 etc.");
    ic_add_completion(cenv,NULL,"〈pear〉with brackets"); 
    ic_add_completion(cenv,NULL,"猕猴桃 wide");
    ic_add_completion(cenv,NULL,"apples 🍎");
    ic_add_completion(cenv, NULL, "zero\xE2\x80\x8Dwidth-joiner");    
  }
  else if (prefix[0] != 0 && ic_istarts_with("hello isocline ",prefix)) {
    // many completions for hello isocline
    for(int i = 0; i < 100000; i++) {
      char buf[32];
      snprintf(buf,32,"hello isocline %03d", i+1);
      if (!ic_add_completion(cenv, NULL, buf)) break;  // break early if not all completions are needed (for better latency)
    }
  }
}

// A completer function is called by isocline to complete on input.
// We use `ic_complete_word` to handle escape characters and quoted words.
static void completer(ic_completion_env_t* cenv, const char* prefix ) 
{
  // try to complete file names from the roots "." and "/usr/local"
  ic_complete_filename(cenv, prefix, 0, "/usr/local;c:\\Program Files" , NULL /* any extension */);

  // and also use our custom completer  
  ic_complete_word( cenv, prefix, &word_completer );        
  
  // ic_complete_quoted_word( cenv, prefix, &word_completer, &ic_char_is_nonwhite, '\\', "'\"" );        
}


// -------------------------------------------------------------------------------
// Syntax highlighting
// -------------------------------------------------------------------------------

// A highlight function is called by isocline when input can be highlighted.
// Use `ic_highlight_color` (or `bgcolor`, `underline`) to highlight characters from
// a given position. Here we use some convenience functions to easily highlight
// simple tokens but a full-fledged highlighter probably needs regular expressions.
static void highlighter(ic_highlight_env_t* henv, const char* input, void* arg) {
  (void)(arg); // unused
  // for all characters in the input..
  long len = (long)strlen(input);
  for (long i = 0; i < len; ) {
    static const char* keywords[] = { "fun", "return", "static", "const", "if", "else", NULL };
    static const char* types[]    = { "int", "double", "char", "void", NULL };
    long tlen;  // token length
    if ((tlen = ic_match_any_token(input, i, &ic_char_is_idletter, keywords)) > 0) {
      ic_highlight_color(henv, i, IC_RGB(0xFFFFAF));  // rgb colors are auto translated on terminals with less color support
      i += tlen;
    }
    else if ((tlen = ic_match_any_token(input, i, &ic_char_is_idletter, types)) > 0) {
      ic_highlight_color(henv, i, IC_RGB(0x00AFAF));
      i += tlen;
    }
    else if ((tlen = ic_is_token(input, i, &ic_char_is_digit)) > 0) {  // digits
      ic_highlight_color(henv, i, IC_ANSI_PURPLE);
      i += tlen;
    }
    else if (ic_starts_with(input + i,"//")) {       // line comment
      ic_highlight_color(henv, i, IC_RGB(0x408700));
      while (i < len && input[i] != '\n') { i++; }
    }
    else {
      ic_highlight_color(henv, i, IC_ANSI_DEFAULT);  // anything else (including utf8 continuation bytes)
      i++;
    }
  }
}
