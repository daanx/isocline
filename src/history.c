/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#include <stdio.h>
#include <ctype.h>
#include <string.h>  
#include "../include/repline.h"
#include "common.h"
#include "env.h"

#ifdef _WIN32
#else
#include <sys/stat.h>
#endif

//-------------------------------------------------------------
// push/clear
//-------------------------------------------------------------

internal bool history_update( rl_env_t* env, const char* entry ) {
  if (entry==NULL) return false;
  rl_history_remove_last(env);
  history_push(env,entry);
  debug_msg("history: update: with %s; now at %s\n", entry, history_get(env,0));
  return true;
}

internal bool history_push( rl_env_t* env, const char* entry ) {
  history_t* h = &env->history; 
  if (h->len <= 0 || entry==NULL)  return false;
  if (h->count == h->len) {
    // delete oldest entry
    env_free(env, h->elems[0]);
    rl_memmove( h->elems, h->elems + 1, (h->count-1)*ssizeof(h->elems[0]) );
    h->count--;
  }
  assert(h->count < h->len);
  h->elems[h->count] = env_strdup(env,entry);
  h->count++;
  return true;
}


static void history_remove_last_n( rl_env_t* env, ssize_t n ) {
  history_t* h = &env->history;
  if (n <= 0) return;
  if (n > h->count) n = h->count;
  for( ssize_t i = h->count - n; i < h->count; i++) {
    env_free( env, h->elems[i] );
  }
  h->count -= n;
  assert(h->count >= 0);    
}

exported void rl_history_remove_last(rl_env_t* env) {
  history_remove_last_n(env,1);
}

exported void rl_history_clear(rl_env_t* env) {
  history_remove_last_n( env, env->history.count );
}

internal const char* history_get( rl_env_t* env, ssize_t n ) {
  history_t* h = &env->history;
  if (n < 0 || n >= h->count) return NULL;
  return h->elems[h->count - n - 1];
}


//-------------------------------------------------------------
// 
//-------------------------------------------------------------

internal void history_done(rl_env_t* env) {
  rl_history_clear(env);
  history_t* h = &env->history;
  if (h->len > 0) {
    env_free( env, h->elems );
    h->elems = NULL;
    h->len = 0;
  }
  env_free(env, h->fname);
  h->fname = NULL;
}

exported void rl_set_history(rl_env_t* env, const char* fname, long max_entries ) {
  history_done(env);
  history_t* h = &env->history;
  h->fname = env_strdup(env,fname);
  if (max_entries == 0) {
    assert(h->elems == NULL);
    return;
  }
  if (max_entries < 0 || max_entries > RL_MAX_HISTORY) max_entries = RL_MAX_HISTORY;
  h->elems = (const char**)env_zalloc(env, max_entries * ssizeof(h->elems[0]) );
  if (h->elems == NULL) return;
  h->len = max_entries;
  history_load(env);
}




//-------------------------------------------------------------
// save/load history to file
//-------------------------------------------------------------

static char from_xdigit( int c ) {
  if (c >= '0' && c <= '9') return (char)(c - '0');
  if (c >= 'A' && c <= 'F') return (char)(10 + (c - 'A'));
  if (c >= 'a' && c <= 'f') return (char)(10 + (c - 'a'));
  return 0;
}

static char to_xdigit( uint8_t c ) {
  if (c <= 9) return ((char)c + '0');
  if (c >= 10 && c <= 15) return ((char)c - 10 + 'A');
  return '0';
}

static bool history_read_entry( rl_env_t* env, FILE* f ) {
  char buf[RL_MAX_LINE + 1];
  int count = 0;
  while( !feof(f) && count < RL_MAX_LINE) {
    int c = fgetc(f);
    if (c == EOF || c == '\n') break;
    if (c == '\\') {
      c = fgetc(f);
      if (c == 'n')       buf[count++] = '\n';
      else if (c == 'r')  buf[count++] = '\r';
      else if (c == 't')  buf[count++] = '\t';
      else if (c == '\\') buf[count++] = '\\';
      else if (c == 'x') {
        int c1 = fgetc(f);         
        int c2 = fgetc(f);
        if (isxdigit(c1) && isxdigit(c2)) {
          char chr = from_xdigit(c1)*16 + from_xdigit(c2);
          buf[count++] = chr;
        }
        else return false;
      }
      else return false;
    }
    else buf[count++] = (char)c;
  }
  assert(count <= RL_MAX_LINE);
  buf[count] = 0;
  if (count == 0 || buf[0] == '#') return true;
  return history_push(env,buf);
}

static bool history_write_entry( const char* entry, FILE* f ) {
  char buf[RL_MAX_LINE + 5];
  int count = 0;
  while( entry != NULL && *entry != 0 && count < RL_MAX_LINE ) {
    char c = *entry++;
    if (c == '\\')      { buf[count++] = '\\'; buf[count++] = '\\'; }
    else if (c == '\n') { buf[count++] = '\\'; buf[count++] = 'n'; }
    else if (c == '\r') { buf[count++] = '\\'; buf[count++] = 'r'; }
    else if (c == '\t') { buf[count++] = '\\'; buf[count++] = 't'; }    
    else if (c < ' ' || c > '~' || c == '#') {
      char c1 = to_xdigit( (uint8_t)c / 16 );
      char c2 = to_xdigit( (uint8_t)c % 16 );
      buf[count++] = '\\'; buf[count++] = 'x';
      buf[count++] = c1; buf[count++] = c2;  
    }
    else buf[count++] = (char)c;
  }
  assert( count < RL_MAX_LINE + 5 );
  buf[count] = 0;
  if (count > 0) {
    fputs(buf,f);
    fputc('\n',f);
  }
  return true;
}

internal void history_load( rl_env_t* env ) {
  history_t* h = &env->history;
  if (h->fname == NULL) return;
  FILE* f = fopen(h->fname, "r");
  if (f == NULL) return;
  while (!feof(f)) {
    if (!history_read_entry(env,f)) break; // error
  }
  fclose(f);
}

internal void history_save( rl_env_t* env ) {
  history_t* h = &env->history;
  if (h->fname == NULL) return;
  FILE* f = fopen(h->fname, "w");
  if (f == NULL) return;
  chmod(h->fname,S_IRUSR|S_IWUSR);
  for( int i = 0; i < h->count; i++ )  {
    if (!history_write_entry(h->elems[i],f)) break;  // error
  }
  fclose(f);  
}
