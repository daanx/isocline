/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#include <string.h>
#include <stdio.h>

#include "../include/repline.h"
#include "common.h"
#include "env.h"
#include "stringbuf.h"
#include "completions.h"

//-------------------------------------------------------------
// Completions
//-------------------------------------------------------------

typedef struct completion_s {
  const char* display;
  const char* replacement;
  ssize_t     delete_before;
  ssize_t     delete_after;
} completion_t;

struct completions_s {
  rp_completer_fun_t* completer;
  void* completer_arg;
  ssize_t completer_max;
  ssize_t count;
  ssize_t len;
  completion_t* elems;
  alloc_t* mem;
};



rp_private completions_t* completions_new(alloc_t* mem) {
  completions_t* cms = mem_zalloc_tp(mem, completions_t);
  if (cms == NULL) return NULL;
  cms->mem = mem;
  return cms;
}

rp_private void completions_free(completions_t* cms) {
  if (cms == NULL) return;
  completions_clear(cms);  
  if (cms->elems != NULL) {
    mem_free(cms->mem, cms->elems);
    cms->elems = NULL;
    cms->count = 0;
    cms->len = 0;
  }
  mem_free(cms->mem, cms); // free ourselves
}


rp_private void completions_clear(completions_t* cms) {  
  while (cms->count > 0) {
    completion_t* cm = cms->elems + cms->count - 1;
    mem_free( cms->mem, cm->display);
    mem_free( cms->mem, cm->replacement);
    memset(cm,0,sizeof(*cm));
    cms->count--;    
  }
}

static void completions_push(completions_t* cms, const char* display, const char* replacement, ssize_t delete_before, ssize_t delete_after) 
{
  if (cms->count >= cms->len) {
    ssize_t newlen = (cms->len <= 0 ? 32 : cms->len*2);
    completion_t* newelems = mem_realloc_tp(cms->mem, completion_t, cms->elems, newlen );
    if (newelems == NULL) return;
    cms->elems = newelems;
    cms->len   = newlen;
  }
  assert(cms->count < cms->len);
  completion_t* cm  = cms->elems + cms->count;
  cm->display       = mem_strdup(cms->mem,display);
  cm->replacement   = mem_strdup(cms->mem,replacement);
  cm->delete_before = delete_before;
  cm->delete_after  = delete_after;
  cms->count++;
}

rp_private ssize_t completions_count(completions_t* cms) {
  return cms->count;
}


rp_private bool completions_add(completions_t* cms, const char* display, const char* replacement, ssize_t delete_before, ssize_t delete_after) {
  if (cms->completer_max <= 0) return false;
  cms->completer_max--;
  //debug_msg("completion: add: %d,%d, %s\n", delete_before, delete_after, replacement);
  completions_push(cms, display, replacement, delete_before, delete_after);
  return true;
}

static completion_t* completions_get(completions_t* cms, ssize_t index) {
  if (index < 0 || cms->count <= 0 || index >= cms->count) return NULL;
  return &cms->elems[index];
}

rp_private const char* completions_get_display( completions_t* cms, ssize_t index ) {
  completion_t* cm = completions_get(cms, index);
  if (cm == NULL) return NULL;
  return (cm->display != NULL ? cm->display : cm->replacement);
}


rp_private ssize_t completions_apply( completions_t* cms, ssize_t index, stringbuf_t* sbuf, ssize_t pos ) {
  completion_t* cm = completions_get(cms, index);
  if (cm == NULL) return -1;
  debug_msg( "completion: apply: %s at %zd\n", cm->replacement, pos);
  ssize_t start = pos - cm->delete_before;
  if (start < 0) start = 0;
  sbuf_delete_from_to( sbuf, start, pos + cm->delete_after );
  return sbuf_insert_at(sbuf, cm->replacement, start); 
}

rp_private void completions_set_completer(completions_t* cms, rp_completer_fun_t* completer, void* arg) {
  cms->completer = completer;
  cms->completer_arg = arg;
}

//-------------------------------------------------------------
// Completer functions
//-------------------------------------------------------------
typedef bool (rp_completion_fun_t)( rp_env_t* env, void* funenv, const char* display, const char* replacement, long delete_before, long delete_after );

struct rp_completion_env_s {
  rp_env_t*   env;
  const char* input;
  long        cursor;
  void*       arg;
  void*       funenv;
  rp_completion_fun_t* complete;
};

rp_public bool rp_add_completion( rp_completion_env_t* cenv, const char* display, const char* replacement ) {
  return rp_add_completion_ex(cenv,display,replacement,0,0);
}

rp_public bool rp_add_completion_ex(rp_completion_env_t* cenv, const char* display, const char* replacement, long delete_before, long delete_after) {
  return (*cenv->complete)(cenv->env, cenv->funenv, display, replacement, delete_before, delete_after );
}

static bool rpenv_add_completion(rp_env_t* env, void* funenv, const char* display, const char* replacement, long delete_before, long delete_after) {
  rp_unused(funenv);
  return completions_add(env->completions, display, replacement, delete_before, delete_after);
}

rp_public void rp_set_completer(rp_env_t* env, rp_completer_fun_t* completer, void* arg) {
  completions_set_completer(env->completions, completer, arg);
}


rp_private ssize_t completions_generate(struct rp_env_s* env, completions_t* cms, const char* input, ssize_t pos, ssize_t max) {
  completions_clear(cms);
  if (cms->completer == NULL || input == NULL || input[0] == 0 || rp_strlen(input) < pos) return 0;

  // set up env
  rp_completion_env_t cenv;
  cenv.env = env;
  cenv.input = input,
  cenv.cursor = (long)pos;
  cenv.arg = cms->completer_arg;
  cenv.complete = &rpenv_add_completion;
  cenv.funenv = NULL;
  const char* prefix = mem_strndup(cms->mem, input, pos);
  cms->completer_max = max;
  
  // and complete
  cms->completer(&cenv,prefix);

  // restore
  mem_free(cms->mem,prefix);
  return completions_count(cms);
}


//-------------------------------------------------------------
// Completion transformers
//-------------------------------------------------------------

rp_public long rp_prev_char( const char* s, long pos ) {
  ssize_t len = rp_strlen(s);
  if (pos < 0 || pos > len) return -1;
  ssize_t ofs = str_prev_ofs( s, pos, true, NULL );
  if (ofs <= 0) return -1;
  return (long)(pos - ofs);
}

rp_public long rp_next_char( const char* s, long pos ) {
  ssize_t len = rp_strlen(s);
  if (pos < 0 || pos > len) return -1;
  ssize_t ofs = str_next_ofs( s, len, pos, true, NULL );
  if (ofs <= 0) return -1;
  return (long)(pos + ofs);
}

rp_public bool rp_starts_with( const char* s, const char* prefix ) {
  if (s==prefix) return true;
  if (prefix==NULL) return true;
  if (s==NULL) return false;

  ssize_t i;
  for( i = 0; s[i] != 0 && prefix[i] != 0; i++) {
    if (s[i] != prefix[i]) return false;
  }
  return (prefix[i] == 0);
}


//-------------------------------------------------------------
// Word completion (quoted and with escape characters)
//-------------------------------------------------------------

// free variables for word completion
typedef struct complete_word_env_s {
  const char* non_word_chars;
  char        escape_char;
  char        quote;
  long        delete_before_adjust;
  rp_completion_fun_t* prev_complete;
  stringbuf_t* sbuf;
  void*        prev_env;
} complete_word_env_t;

// word completion callback
rp_private bool word_add_completion_ex(rp_env_t* env, void* funenv, const char* display, const char* replacement, long delete_before, long delete_after) {
  complete_word_env_t* wenv = (complete_word_env_t*)(funenv);
  sbuf_replace( wenv->sbuf, replacement );   
  if (wenv->quote != 0) {
    // add end quote
    sbuf_append_char( wenv->sbuf, wenv->quote);
  }
  else {
    // escape white space if it was not quoted
    ssize_t len = sbuf_len(wenv->sbuf);
    ssize_t pos = 0;
    while( pos < len ) {
      if (strchr(wenv->non_word_chars, sbuf_char_at( wenv->sbuf, pos )) != NULL) {
        sbuf_insert_char_at( wenv->sbuf, wenv->escape_char, pos);
        pos++;
      }
      pos = sbuf_next( wenv->sbuf, pos, NULL );
      if (pos <= 0) break;
    }
  }
  // and call the previous completion function
  return (*wenv->prev_complete)( env, funenv, (display!=NULL ? display : replacement), sbuf_string(wenv->sbuf), wenv->delete_before_adjust + delete_before, delete_after );  
}

rp_public bool rp_complete_word( rp_completion_env_t* cenv, const char* prefix, rp_completer_fun_t* fun ) {
  return rp_complete_quoted_word( cenv, prefix, fun, NULL, '\\', NULL);
}

rp_public bool rp_complete_quoted_word( rp_completion_env_t* cenv, const char* prefix, rp_completer_fun_t* fun, const char* non_word_chars, char escape_char, const char* quote_chars ) {
  if (non_word_chars == NULL) non_word_chars = " \t\r\n";  
  if (quote_chars == NULL) quote_chars = "'\"";

  ssize_t len = rp_strlen(prefix);
  ssize_t pos = len;
  char quote = 0;
  
  // 1. look for a starting quote
  if (quote_chars[0] != 0) {
    while(pos > 0) {
      // go back one code point
      ssize_t ofs = str_prev_ofs(prefix, pos, true, NULL );
      if (ofs <= 0) break;
      if (strchr(quote_chars, prefix[pos - ofs]) != NULL) {
        // quote char, break if it is not escaped
        if (pos <= ofs || prefix[pos - ofs - 1] != escape_char) {
          // found a quote
          quote = prefix[pos - ofs];
          break;
        }
        // otherwise go on
      }
      pos -= ofs;
    }
    // pos points to the word start just after the quote.
  }

  // 2. if we did not find a quoted word, look for non-word-chars
  if (quote == 0) {
    pos = len;
    while(pos > 0) {
      // go back one code point
      ssize_t ofs = str_prev_ofs(prefix, pos, true, NULL );
      if (ofs <= 0) break;
      if (strchr(non_word_chars, prefix[pos - ofs]) != NULL) {
        // non word char, break if it is not escaped
        if (pos <= ofs || prefix[pos - ofs - 1] != escape_char) break; 
        // otherwise go on
      }
      pos -= ofs;
    }
  }

  // stop if empty word
  if (len == pos) return false;

  // allocate new unescaped word prefix
  char* word = mem_strdup( cenv->env->mem, prefix + pos );
  if (word == NULL) return false;

  // unescape prefix
  if (quote == 0) {
    ssize_t wlen = len - pos;
    ssize_t wpos = 0;
    while( wpos < wlen ) {
      ssize_t ofs = str_next_ofs(word, wlen, wpos, true, NULL);
      if (ofs <= 0) break;
      if (word[wpos] == escape_char && strchr(non_word_chars, word[wpos+1]) != NULL) {
        rp_memmove( word + wpos, word + wpos + 1, wlen - wpos /* including 0 */ );
      }
      wpos += ofs;
    }
  }

  complete_word_env_t wenv;
  wenv.quote          = quote;
  wenv.non_word_chars = non_word_chars;
  wenv.escape_char    = escape_char;
  wenv.delete_before_adjust = (len - pos);
  wenv.prev_complete  = cenv->complete;
  wenv.prev_env       =  cenv->env;
  wenv.sbuf = sbuf_new(cenv->env->mem, true);
  if (wenv.sbuf == NULL) { mem_free(cenv->env->mem, word); return false; }
  cenv->complete = &word_add_completion_ex;
  cenv->funenv = &wenv;

  ssize_t count = cenv->env->completions->count;
  (*fun)( cenv, word );

  cenv->complete = wenv.prev_complete;
  cenv->funenv = wenv.prev_env;

  sbuf_free(wenv.sbuf);
  mem_free(cenv->env->mem, word);
  return (cenv->env->completions->count > count);
}


