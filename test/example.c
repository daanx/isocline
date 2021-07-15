#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "repline.h"

static void completer(rl_env_t* env, const char* input, long cur, void* arg ) 
{
  (void)(arg);
  assert(cur > 0);
  assert(input != NULL && strlen(input) >= (size_t)cur);
  if (cur <= 0) return;
  if (input[cur-1] == 'h') {
    for(int i = 0; i < 100; i++) {
      char buf[32];
      snprintf(buf,32,"hëllo≈%2d", i+1);
      rl_add_completion(env, NULL, buf, 1, 0);
    }
  }
  else if (input[cur-1] == 'f') {
    rl_add_completion(env,NULL,"banana 🍌 etc.", 1, 0);
    rl_add_completion(env,NULL,"〈pear〉with brackets", 1, 0); 
    rl_add_completion(env,NULL,"猕猴桃 wide", 1, 0);
    rl_add_completion(env,NULL,"apples 🍎", 1, 0);
    rl_add_completion(env,NULL,"with a zero‍width", 1, 0);
  }
  else if (input[cur-1] == 'e' || input[cur-1] == 'E') {
    rl_add_completion(env, "excëllent", "excëllent", 1, 0);
  }
  else if (input[cur-1] == 'x' && strncmp(input+cur,"tra",3) == 0) {
    rl_add_completion(env, NULL, "extra excellent", 1, 3);
    rl_add_completion(env, "extra excellent (with two 'x's)", "exxtra excellent", 1, 3);
  }
}

int main() 
{
  printf("start repline example:\n"
         "- use ctrl+D or empty input to quit.\n"
         "- use shift+TAB (or ctrl+J, or '\\' followed by ENTER) for multiline input.\n"
         "\n");
  rl_env_t* env = rl_init();
  rl_set_completer(env, &completer, NULL);
  rl_set_history(env, "history.txt", -1 /* default entries (200) */);
  rl_set_prompt_color(env, RL_GREEN);
  char *line;
  while((line = rl_readline(env,"promπt")) != NULL) {   // ctrl-D returns NULL (as well as errors)
    if (line[0] == 0) {
      free(line);
      break;
    }
    printf("-----\n%s\n-----\n", line);
    free(line);
  }
  printf("done\n");
  return 0;
}
