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
static void completer(rp_env_t* env, const char* input, long cur, void* arg ); 

// main example
int main() 
{
  printf("\nRepline sample program:\n"
         "- Type 'exit' to quit. (or use ctrl+d).\n"
         "- Press F1 for help on editing commands.\n"
         "- Use shift+tab for multiline input. (or ctrl+enter, or ctrl+j)\n"
         "- Type 'id' (or 'ex', 'f', or 'h') followed by tab for completion.\n"
         "\n");

  // initialize and get a repline environment         
  rp_env_t* env = rp_init();

  // enable completion with a completion function
  rp_set_completer(env, &completer, NULL);

  // enable history; use a NULL filename to not persist history to disk
  rp_set_history(env, "history.txt", -1 /* default entries (= 200) */);

  // set a nice color for the prompt and the prompt marker (>)
  rp_set_prompt_color(env, RP_GREEN);

  // run until empty input
  char* input;
  while((input = rp_readline(env,"rεplinε")) != NULL)    // ctrl-d/ctrl-c return NULL (as well as errors)
  {
    bool stop = (strcmp(input,"exit") == 0 || strcmp(input,"") == 0); 
    printf("-----\n"           // echo the input
           "%s\n"
           "-----\n", input );    
    free(input);               // do not forget to free the returned input!
    if (stop) break;
  }
  printf("done\n");
  
  // release the repline environment 
  // (not required as it is otherwise released at the end through an atexit handler)
  rp_done(env);
  return 0;
}

// A completer function is called by repline to complete on input.
// Use `rp_add_completion( env, display, replacement, delete_before, delete_after )` to
// add actual completions.
static void completer(rp_env_t* env, const char* input, long cur, void* arg ) 
{
  (void)(arg);
  assert(cur > 0);
  assert(input != NULL && strlen(input) >= (size_t)cur);

  size_t len = strlen(input);
  if (len <= 0 || cur <= 0) return;  // should never happen

  if (len >= 1 && input[cur-1] == 'h') {
    for(int i = 0; i < 100000; i++) {
      char buf[32];
      snprintf(buf,32,"hello repline (%d)", i+1);
      if (!rp_add_completion(env, NULL, buf, 1, 0)) break;  // break early if not all completions are needed (for better latency)
    }
  }
  else if (len >= 1 && input[cur-1] == 'f') {  
    rp_add_completion(env,NULL,"banana 🍌 etc.", 1, 0);
    rp_add_completion(env,NULL,"〈pear〉with brackets", 1, 0); 
    rp_add_completion(env,NULL,"猕猴桃 wide", 1, 0);
    rp_add_completion(env,NULL,"apples 🍎", 1, 0);
    rp_add_completion(env, NULL, "zero\xE2\x80\x8Dwidth-joiner",1,0);    
  }
  else if (len >= 2 && strncmp( input+cur-2, "id", 2) == 0) {
    // rp_add_completion(env,"C++ - [](auto x){ return x; }", "c++", 2, 0);
    rp_add_completion(env,"D — (x) => x", "d", 2, 0);                
    rp_add_completion(env,"Haskell — \\x -> x", "haskell", 2, 0);
    rp_add_completion(env,"Idris — \\x => x", "ris", 0, 0);          // keep the initial "id" :-)
    rp_add_completion(env,"Koka — fn(x){ x }", "koka", 2, 0);    
    rp_add_completion(env,"Ocaml — fun x -> x", "ocaml", 2, 0);
  }
  else if (len >= 2 && strncmp( input+cur-2, "ex", 2) == 0) {
    rp_add_completion(env, NULL, "excellent", 2, 0);
  }
}
