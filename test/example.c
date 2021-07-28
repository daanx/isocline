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

static void highlighter(rp_highlight_env_t* henv, const char* input, void* arg);

// main example
int main() 
{
  printf("\nRepline sample program:\n"
         "- Type 'exit' to quit. (or use ctrl+d).\n"
         "- Press F1 for help on editing commands.\n"
         "- Use shift+tab for multiline input. (or ctrl+enter, or ctrl+j)\n"
         "- Type 'id' (or 'ex', 'f', or 'h') followed by tab for completion.\n"
         "\n");

  // enable completion with a default completion function
  rp_set_default_completer(&completer, NULL);

  // enable history; use a NULL filename to not persist history to disk
  rp_set_history("history.txt", -1 /* default entries (= 200) */);

  // set a nice color for the prompt and the prompt marker (>)
  rp_set_prompt_color(RP_GREEN);

  // try to auto complete after a completion as long as the completion is unique
  rp_enable_auto_tab(true );

  rp_set_highlighter(highlighter, NULL);

  //rp_set_iface_colors( env, RP_MAROON, RP_DARKGRAY, RP_YELLOW );

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

// A custom completer function.
// Use `rp_add_completion( env, display, replacement)` to add actual completions.
static void word_completer(rp_completion_env_t* cenv, const char* prefix ) {
  if (prefix[0] != 0 && rp_istarts_with("hello repline",prefix)) {
    for(int i = 0; i < 100000; i++) {
      char buf[32];
      snprintf(buf,32,"hello repline (%d)", i+1);
      if (!rp_add_completion(cenv, NULL, buf)) break;  // break early if not all completions are needed (for better latency)
    }
  }
  else if (strcmp(prefix,"f") == 0) {  
    rp_add_completion(cenv,NULL,"banana 🍌 etc.");
    rp_add_completion(cenv,NULL,"〈pear〉with brackets"); 
    rp_add_completion(cenv,NULL,"猕猴桃 wide");
    rp_add_completion(cenv,NULL,"apples 🍎");
    rp_add_completion(cenv, NULL, "zero\xE2\x80\x8Dwidth-joiner");    
  }
  else if (strcmp(prefix,"id") == 0) {
    // rp_add_completion(cenv,"C++ - [](auto x){ return x; }", "c++", 2, 0);
    rp_add_completion(cenv,"D — (x) => x",       "(x) => x");                
    rp_add_completion(cenv,"Haskell — \\x -> x", "\\x -> x");
    rp_add_completion(cenv,"Idris — \\x => x",   "\\x => x");
    rp_add_completion(cenv,"Koka — fn(x){ x }",  "fn(x){ x }");    
    rp_add_completion(cenv,"Ocaml — fun x -> x", "fun x -> x");
  }
  else if (strcmp(prefix,"ex") == 0) {
    rp_add_completion(cenv, NULL, "excellent");
  }
  else {
    static const char* completions[] = { "print", "println", "printer", "printsln", "prompt", NULL };
    rp_add_completions(cenv, prefix, completions);
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
  
  // rp_complete_quoted_word( cenv, prefix, &word_completer, " !=+,`@#&^*.()\r\t\n", '\\', "'\"" );        
}
  
static void highlighter(rp_highlight_env_t* henv, const char* input, void* arg) {
  size_t len = strlen(input);
  for (size_t i = 0; i < len; ) {
    long tlen;
    if (rp_match_token(input, i, &rp_char_is_idletter, "fun")) {
      rp_highlight_color(henv, i, RP_YELLOW);
      i += 3;
    }
    if (rp_match_token(input, i, &rp_char_is_idletter, "int")) {
      rp_highlight_color(henv, i, RP_CYAN);
      i += 3;
    }
    else if ((tlen = rp_is_token_start(input, i, &rp_char_is_digit)) > 0) {
      rp_highlight_color(henv, i, RP_PURPLE);
      i += tlen;
    }
    else {
      rp_highlight_color(henv, i, RP_COLOR_DEFAULT);
      i++;
    }
  }
}