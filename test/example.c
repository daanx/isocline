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
  rp_set_completer(&completer, NULL);

  // enable history; use a NULL filename to not persist history to disk
  rp_set_history("history.txt", -1 /* default entries (= 200) */);

  // set a nice color for the prompt and the prompt marker (>)
  rp_set_prompt_color(RP_GREEN);

  // try to auto complete after a completion as long as the completion is unique
  rp_enable_auto_tab(true );

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
    rp_add_completion(cenv,"D — (x) => x", "d");                
    rp_add_completion(cenv,"Haskell — \\x -> x", "haskell");
    rp_add_completion(cenv,"Idris — \\x => x", "idris");
    rp_add_completion(cenv,"Koka — fn(x){ x }", "koka");    
    rp_add_completion(cenv,"Ocaml — fun x -> x", "ocaml");
  }
  else if (strcmp(prefix,"ex") == 0) {
    rp_add_completion(cenv, NULL, "excellent");
  }
}

// A completer function is called by repline to complete on input.
// We use `rp_complete_word` to handle escape characters and quoted words.
static void completer(rp_completion_env_t* cenv, const char* prefix ) 
{
  rp_complete_filename( cenv, prefix, 0, ".;/usr/local" );   // try to complete file names from the roots "." and "/usr/local"
  rp_complete_word( cenv, prefix, &word_completer );         // and also use our custom completer  
}
  
