/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  Example usage of repline.
-----------------------------------------------------------------------------*/
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "repline.h"

// completion function defined below
static void completer(rp_completion_env_t* cenv, const char* prefix );

// highlighter function defined below
static void highlighter(rp_highlight_env_t* henv, const char* input, void* arg);

static void show_color( rp_color_t color, const char* name ) {
  printf("\x1B[%dm%20s\x1B[0m | \x1B[1;%dmbold\x1B[0m | \x1B[%dmbright\x1B[0m\n", color, name, color, color+60);  
}

// main example
int main() 
{
  // rp_writeln handles basic escape sequences in a portable way
  rp_writeln(
    "\n\x1B[33mRepline sample program:\x1B[0m\n"
    "- Type 'exit' to quit. (or use ctrl+d).\n"
    "- Press F1 for help on editing commands.\n"
    "- Use shift+tab for multiline input. (or ctrl+enter, or ctrl+j)\n"
    "- Type 'p' (or 'id', 'f', or 'h') followed by tab for completion.\n");

  rp_writeln("colors:");
  
  rp_term_color(RP_ANSI_MAROON); rp_write("ansi8 ");
  rp_term_color(RP_ANSI_RED); rp_write("ansi16 ");
  rp_term_color(RP_RGB(0xDF,0xAF,0x87)); rp_write("ansi256 ");
  rp_term_color(RP_RGB(100,255,180)); rp_write("rgb");
  rp_term_reset(); rp_writeln("");

  for(int i = 0; i <= 64; i++) {
    rp_term_color(RP_RGB( (i==64? 255 : i*4),0,0 )); rp_write((i%8==0?"*":"x"));
  }
  rp_writeln("\x1B[0m");
  for(int i = 0; i <= 64; i++) {
    int g = (i==64? 255 : i*4);
    rp_term_color(RP_RGB( g,g,g )); rp_write((i%8==0?"*":"x"));
  }
  rp_writeln("\x1B[0m");
  
  rp_writeln("\x1b[35mansi8 \x1b[38;5;180mansi256 \x1b[38;2;100;255;180mrgb\x1b[0m\n");

  show_color(RP_ANSI_BLACK,"black");
  show_color(RP_ANSI_MAROON,"maroon");
  show_color(RP_ANSI_GREEN,"greena");
  show_color(RP_ANSI_ORANGE,"orange/brown");
  show_color(RP_ANSI_NAVY,"navy");
  show_color(RP_ANSI_PURPLE,"purple");
  show_color(RP_ANSI_TEAL,"teal");
  show_color(RP_ANSI_LIGHTGRAY,"lighgray/white");
  show_color(RP_ANSI_DEFAULT,"default");
  

  // enable history; use a NULL filename to not persist history to disk
  rp_set_history("history.txt", -1 /* default entries (= 200) */);

  // enable completion with a default completion function
  rp_set_default_completer(&completer, NULL);

  // enable syntax highlighting with a highlight function
  rp_set_default_highlighter(highlighter, NULL);

  // set a nice color for the prompt and the prompt marker (>)
  rp_set_prompt_color(RP_ANSI_GREEN);

  // try to auto complete after a completion as long as the completion is unique
  rp_enable_auto_tab(true );

  // change interface colors (info, diminish, emphasis, hint)
  // rp_set_style_color( RP_STYLE_INFO, RP_YELLOW );

  // run until empty input
  char* input;
  while((input = rp_readline("rεplinε")) != NULL)    // ctrl-d/ctrl-c return NULL (as well as errors)
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
// Use `rp_add_completion( env, display, replacement)` to add actual completions.
static void word_completer(rp_completion_env_t* cenv, const char* prefix ) 
{
  // complete with list of words; only if the input is a prefix it will be a completion candidate
  static const char* completions[] = { "print", "println", "printer", "printsln", "prompt", NULL };
  rp_add_completions(cenv, prefix, completions);

  // examples of more customized completions
  if (prefix[0] != 0 && rp_istarts_with("hello repline ",prefix)) {
    // many completions for hello repline
    for(int i = 0; i < 100000; i++) {
      char buf[32];
      snprintf(buf,32,"hello repline %03d", i+1);
      if (!rp_add_completion(cenv, NULL, buf)) break;  // break early if not all completions are needed (for better latency)
    }
  }
  else if (strcmp(prefix,"f") == 0) {  
    // unicode for f completion
    rp_add_completion(cenv,NULL,"banana 🍌 etc.");
    rp_add_completion(cenv,NULL,"〈pear〉with brackets"); 
    rp_add_completion(cenv,NULL,"猕猴桃 wide");
    rp_add_completion(cenv,NULL,"apples 🍎");
    rp_add_completion(cenv, NULL, "zero\xE2\x80\x8Dwidth-joiner");    
  }
  else if (strcmp(prefix,"id") == 0) {
    // display vs. replacement
    rp_add_completion(cenv,"D — (x) => x",       "(x) => x");                
    rp_add_completion(cenv,"Haskell — \\x -> x", "\\x -> x");
    rp_add_completion(cenv,"Idris — \\x => x",   "\\x => x");
    rp_add_completion(cenv,"Koka — fn(x){ x }",  "fn(x){ x }");    
    rp_add_completion(cenv,"Ocaml — fun x -> x", "fun x -> x");
  }  
}

// A completer function is called by repline to complete on input.
// We use `rp_complete_word` to handle escape characters and quoted words.
static void completer(rp_completion_env_t* cenv, const char* prefix ) 
{
  // try to complete file names from the roots "." and "/usr/local"
  rp_complete_filename(cenv, prefix, 0, ".;/usr/local;c:\\Program Files" , NULL /* any extension */);

  // and also use our custom completer  
  rp_complete_word( cenv, prefix, &word_completer );        
  
  // rp_complete_quoted_word( cenv, prefix, &word_completer, &rp_char_is_nonwhite, '\\', "'\"" );        
}


// -------------------------------------------------------------------------------
// Syntax highlighting
// -------------------------------------------------------------------------------

// A highlight function is called by repline when input can be highlighted.
// Use `rp_highlight_color` (or `bgcolor`, `underline`) to highlight characters from
// a given position. Here we use some convenience functions to easily highlight
// simple tokens but a full-fledged highlighter probably needs regular expressions.
static void highlighter(rp_highlight_env_t* henv, const char* input, void* arg) {
  (void)(arg); // unused
  // for all characters in the input..
  long len = (long)strlen(input);
  for (long i = 0; i < len; ) {
    static const char* keywords[] = { "fun", "return", "static", "const", "if", "else", NULL };
    static const char* types[]    = { "int", "double", "char", "void", NULL };
    long tlen;  // token length
    if ((tlen = rp_match_any_token(input, i, &rp_char_is_idletter, keywords)) > 0) {
      rp_highlight_color(henv, i, RP_ANSI_YELLOW);
      i += tlen;
    }
    else if ((tlen = rp_match_any_token(input, i, &rp_char_is_idletter, types)) > 0) {
      rp_highlight_color(henv, i, RP_ANSI_CYAN);
      i += tlen;
    }
    else if ((tlen = rp_is_token(input, i, &rp_char_is_digit)) > 0) {  // digits
      rp_highlight_color(henv, i, RP_ANSI_PURPLE);
      i += tlen;
    }
    else if (rp_starts_with(input + i,"//")) {       // line comment
      rp_highlight_color(henv, i, RP_ANSI_DARKGRAY);
      while (i < len && input[i] != '\n') { i++; }
    }
    else {
      rp_highlight_color(henv, i, RP_ANSI_DEFAULT);  // anything else (including utf8 continuation bytes)
      i++;
    }
  }
}
