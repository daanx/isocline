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
#include <locale.h>
#include "isocline.h"

// completion function defined below
static void completer(ic_completion_env_t* cenv, const char* prefix );

// highlighter function defined below
static void highlighter(ic_highlight_env_t* henv, const char* input, void* arg);

// main example
int main() 
{
  setlocale(LC_ALL,"C.UTF-8");  // we use utf-8 in this example

  // use `ic_print` functions to use bbcode's for markup
  ic_style_def("kbd","gray underline");     // you can define your own styles
  ic_style_def("ic-prompt","ansi-maroon");  // or re-define system styles
  
  ic_printf( "[b]Isocline[/b] sample program:\n"
              "- Type 'exit' to quit. (or use [kbd]ctrl-d[/]).\n"
              "- Press [kbd]F1[/] for help on editing commands.\n"
              "- Use [kbd]shift-tab[/] for multiline input. (or [kbd]ctrl-enter[/], or [kbd]ctrl-j[/])\n"
              "- Type 'p' (or 'id', 'f', or 'h') followed by tab for completion.\n"
              "- Type 'fun' or 'int' to see syntax highlighting\n"
              "- Use [kbd]ctrl-r[/] to search the history.\n\n" );
  
  // enable history; use a NULL filename to not persist history to disk
  ic_set_history("history.txt", -1 /* default entries (= 200) */);

  // enable completion with a default completion function
  ic_set_default_completer(&completer, NULL);

  // enable syntax highlighting with a highlight function
  ic_set_default_highlighter(highlighter, NULL);

  // try to auto complete after a completion as long as the completion is unique
  ic_enable_auto_tab(true );

  // inline hinting is enabled by default
  // ic_enable_hint(false);

  // run until empty input
  char* input;
  while((input = ic_readline("isoclinε")) != NULL)    // ctrl-d returns NULL (as well as errors)
  {
    bool stop = (strcmp(input,"exit") == 0 || strcmp(input,"") == 0); 
    ic_printf("[gray]-----[/]\n"           // echo the input
              "%s\n"
              "[gray]-----[/]\n", input );    
    free(input);               // do not forget to free the returned input!
    if (stop) break;
  }
  ic_println("done");
  return 0;
}

// -------------------------------------------------------------------------------
// Completion
// -------------------------------------------------------------------------------

// A custom completer function.
// Use `ic_add_completion( env, replacement, display, help)` to add actual completions.
static void word_completer(ic_completion_env_t* cenv, const char* word ) 
{
  // complete with list of words; only if the input is a word it will be a completion candidate
  static const char* completions[] = { "print", "println", "printer", "printsln", "prompt", NULL };
  ic_add_completions(cenv, word, completions);

  // examples of more customized completions
  if (strcmp(word,"f") == 0) {  
    // test unicode, and replace "f" with something else completely (useful for templating)
    ic_add_completion(cenv, "banana 🍌 etc.");
    ic_add_completion(cenv, "〈pear〉with brackets");
    ic_add_completion(cenv, "猕猴桃 wide");
    ic_add_completion(cenv, "apples 🍎");   
    ic_add_completion(cenv, "zero\xE2\x80\x8Dwidth-joiner");
  }
  else if (strcmp(word, "id") == 0) {
    // replacement, display, and help
    ic_add_completion_ex(cenv, "(x) => x",   "D — (x) => x",       "identity function in D");
    ic_add_completion_ex(cenv, "\\x -> x",   "Haskell — \\x -> x", "identity_bot function in Haskell");
    ic_add_completion_ex(cenv, "\\x => x",   "Idris — \\x => x",   "dependent identity function in Idris");
    ic_add_completion_ex(cenv, "fn(x){ x }", "Koka — fn(x){ x }",  "total identity function in Koka");
    ic_add_completion_ex(cenv, "fun x -> x", "Ocaml — fun x -> x", "identity lambda in OCaml");
  }
  else if (word[0] != 0 && ic_istarts_with("hello isocline ",word)) {
    // many completions for hello isocline
    for(int i = 0; i < 100000; i++) {
      char buf[32];
      snprintf(buf,32,"hello isocline %03d", i+1);
      if (!ic_add_completion(cenv, buf)) break;  // break early if not all completions are needed (for better latency)
    }
  }
}

// A completer function is called by isocline to complete. The input parameter is the input up to the cursor.
// We use `ic_complete_word` to only consider the final token on the input. 
// (almost all user defined completers should use this)
static void completer(ic_completion_env_t* cenv, const char* input ) 
{
  // try to complete file names from the roots "." and "/usr/local"
  ic_complete_filename(cenv, input, 0, ".;/usr/local;c:\\Program Files" , NULL /* any extension */);

  // and also use our custom completer  
  ic_complete_word( cenv, input, &word_completer, NULL /* from default word boundary; whitespace or separator */ );        
  
  // ic_complete_word( cenv, input, &word_completer, &ic_char_is_idletter );        
  // ic_complete_qword( cenv, input, &word_completer, &ic_char_is_idletter  );        
}


// -------------------------------------------------------------------------------
// Syntax highlighting
// -------------------------------------------------------------------------------

// A highlight function is called by isocline when input can be highlighted.
// Use `ic_highlight_color` (or `bgcolor`, `underline`) to highlight characters from
// a given position until another attribute is set. 
// Here we use some convenience functions to easily highlight
// simple tokens but a full-fledged highlighter probably needs regular expressions.
static void highlighter(ic_highlight_env_t* henv, const char* input, void* arg) {
  (void)(arg); // unused
  // for all characters in the input..
  long len = (long)strlen(input);
  for (long i = 0; i < len; ) {
    static const char* keywords[] = { "fun", "static", "const", "struct", NULL };
    static const char* controls[] = { "return", "if", "then", "else", NULL };    
    static const char* types[]    = { "int", "double", "char", "void", NULL };
    long tlen;  // token length
    if ((tlen = ic_match_any_token(input, i, &ic_char_is_idletter, keywords)) > 0) {
      ic_highlight(henv, i, tlen, "keyword"); 
      i += tlen;
    }
    else if ((tlen = ic_match_any_token(input, i, &ic_char_is_idletter, controls)) > 0) {
      ic_highlight(henv, i, tlen, "plum");      // html color (or use the `control` style)
      i += tlen;
    }
    else if ((tlen = ic_match_any_token(input, i, &ic_char_is_idletter, types)) > 0) {
      ic_highlight(henv, i, tlen, "type"); 
      i += tlen;
    }
    else if ((tlen = ic_is_token(input, i, &ic_char_is_digit)) > 0) {  // digits
      ic_highlight(henv, i, tlen, "number");
      i += tlen;
    }
    else if (ic_starts_with(input + i,"//")) {  // line comment
      tlen = 2;
      while (i+tlen < len && input[i+tlen] != '\n') { tlen++; }
      ic_highlight(henv, i, tlen, "comment");   // or use a spefic color like "#408700"
      i += tlen;
    }
    else {
      ic_highlight(henv, i, 1, NULL);  // anything else (including utf8 continuation bytes)
      i++;
    }
  }
}
