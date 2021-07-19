/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#include <stdio.h>
#include <ctype.h>
#include <string.h>  
#include <sys/stat.h>
#include "../include/repline.h"
#include "common.h"
#include "env.h"


//-------------------------------------------------------------
// push/clear
//-------------------------------------------------------------

internal bool history_update( rp_env_t* env, const char* entry ) {
  if (entry==NULL) return false;
  rp_history_remove_last(env);
  history_push(env,entry);
  debug_msg("history: update: with %s; now at %s\n", entry, history_get(&env->history,0));
  return true;
}

static void history_delete_at( rp_env_t* env, history_t* h, int idx ) {
  if (idx < 0 || idx >= h->count) return;
  env_free(env, h->elems[idx]);
  for(ssize_t i = idx+1; i < h->count; i++) {
    h->elems[i-1] = h->elems[i];
  }
  h->count--;
}

internal bool history_push( rp_env_t* env, const char* entry ) {
  history_t* h = &env->history; 
  if (h->len <= 0 || entry==NULL)  return false;
  // remove any older duplicate
  if (!h->allow_duplicates) {
    for( int i = 0; i < h->count; i++) {
      if (strcmp(h->elems[i],entry) == 0) {
        history_delete_at(env,h,i);
      }
    }
  }
  // insert at front
  if (h->count == h->len) {
    // delete oldest entry
    history_delete_at(env,h,0);    
  }
  assert(h->count < h->len);
  h->elems[h->count] = env_strdup(env,entry);
  h->count++;
  return true;
}


static void history_remove_last_n( rp_env_t* env, ssize_t n ) {
  history_t* h = &env->history;
  if (n <= 0) return;
  if (n > h->count) n = h->count;
  for( ssize_t i = h->count - n; i < h->count; i++) {
    env_free( env, h->elems[i] );
  }
  h->count -= n;
  assert(h->count >= 0);    
}

exported void rp_history_remove_last(rp_env_t* env) {
  history_remove_last_n(env,1);
}

exported void rp_history_clear(rp_env_t* env) {
  history_remove_last_n( env, env->history.count );
}

internal const char* history_get( const history_t* h, ssize_t n ) {
  if (n < 0 || n >= h->count) return NULL;
  return h->elems[h->count - n - 1];
}

internal bool history_search( const history_t* h, ssize_t from /*including*/, const char* search, bool backward, ssize_t* hidx, ssize_t* hpos ) {
  const char* p = NULL;
  ssize_t i;
  if (backward) {
    for( i = from; i < h->count; i++ ) {
      p = strstr( history_get(h,i), search);
      if (p != NULL) break;
    }
  }
  else {
    for( i = from; i >= 0; i-- ) {
      p = strstr( history_get(h,i), search);
      if (p != NULL) break;
    }
  }
  if (p == NULL) return false;
  if (hidx != NULL) *hidx = i;
  if (hpos != NULL) *hpos = (p - history_get(h,i));
  return true;
}

//-------------------------------------------------------------
// 
//-------------------------------------------------------------

internal void history_done(rp_env_t* env) {
  rp_history_clear(env);
  history_t* h = &env->history;
  if (h->len > 0) {
    env_free( env, h->elems );
    h->elems = NULL;
    h->len = 0;
  }
  env_free(env, h->fname);
  h->fname = NULL;
}

exported void rp_set_history(rp_env_t* env, const char* fname, long max_entries ) {
  history_done(env);
  history_t* h = &env->history;
  h->fname = env_strdup(env,fname);
  if (max_entries == 0) {
    assert(h->elems == NULL);
    return;
  }
  if (max_entries < 0 || max_entries > RP_MAX_HISTORY) max_entries = RP_MAX_HISTORY;
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

#define RP_MAX_LINE 1024

static bool history_read_entry( rp_env_t* env, FILE* f ) {
  char buf[RP_MAX_LINE + 1];
  int count = 0;
  while( !feof(f) && count < RP_MAX_LINE) {
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
  assert(count <= RP_MAX_LINE);
  buf[count] = 0;
  if (count == 0 || buf[0] == '#') return true;
  return history_push(env,buf);
}

static bool history_write_entry( const char* entry, FILE* f ) {
  char buf[RP_MAX_LINE + 5];
  int count = 0;
  while( entry != NULL && *entry != 0 && count < RP_MAX_LINE ) {
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
  assert( count < RP_MAX_LINE + 5 );
  buf[count] = 0;
  if (count > 0) {
    fputs(buf,f);
    fputc('\n',f);
  }
  return true;
}

internal void history_load( rp_env_t* env ) {
  history_t* h = &env->history;
  if (h->fname == NULL) return;
  FILE* f = fopen(h->fname, "r");
  if (f == NULL) return;
  while (!feof(f)) {
    if (!history_read_entry(env,f)) break; // error
  }
  fclose(f);
}

internal void history_save( rp_env_t* env ) {
  history_t* h = &env->history;
  if (h->fname == NULL) return;
  FILE* f = fopen(h->fname, "w");
  if (f == NULL) return;
  #ifndef _WIN32
  chmod(h->fname,S_IRUSR|S_IWUSR);
  #endif
  for( int i = 0; i < h->count; i++ )  {
    if (!history_write_entry(h->elems[i],f)) break;  // error
  }
  fclose(f);  
}
