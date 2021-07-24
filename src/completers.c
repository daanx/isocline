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
// Word completion (quoted and with escape characters)
//-------------------------------------------------------------

// free variables for word completion
typedef struct word_closure_s {
  const char* non_word_chars;
  char        escape_char;
  char        quote;
  long        delete_before_adjust;
  rp_completion_fun_t* prev_complete;
  stringbuf_t* sbuf;
  void*        prev_env;
} word_closure_t;


// word completion callback
static bool word_add_completion_ex(rp_env_t* env, void* closure, const char* display, const char* replacement, long delete_before, long delete_after) {
  word_closure_t* wenv = (word_closure_t*)(closure);
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
  return (*wenv->prev_complete)( env, closure, (display!=NULL ? display : replacement), sbuf_string(wenv->sbuf), wenv->delete_before_adjust + delete_before, delete_after );  
}


rp_public void rp_complete_word( rp_completion_env_t* cenv, const char* prefix, rp_completer_fun_t* fun ) {
  rp_complete_quoted_word( cenv, prefix, fun, NULL, '\\', NULL);
}


rp_public void rp_complete_quoted_word( rp_completion_env_t* cenv, const char* prefix, rp_completer_fun_t* fun, const char* non_word_chars, char escape_char, const char* quote_chars ) {
  if (non_word_chars == NULL) non_word_chars = " \t\r\n";  
  if (quote_chars == NULL) quote_chars = "'\"";

  ssize_t len = rp_strlen(prefix);
  ssize_t pos; // will be start of the 'word' (excluding a potential start quote)
  char quote = 0;
  ssize_t quote_len = 0;
  
  // 1. look for a starting quote
  if (quote_chars[0] != 0) {
    // we go forward and count all quotes; if it is uneven, we need to complete quoted.
    ssize_t qpos_open = -1;
    ssize_t qpos_close = -1;
    ssize_t qcount = 0;
    pos = 0; 
    while(pos < len) {
      if (prefix[pos] == escape_char && prefix[pos+1] != 0 && strchr(non_word_chars,prefix[pos+1]) != NULL) {       
        pos++; // skip next char
      }
      else if (qcount % 2 == 0 && strchr(quote_chars, prefix[pos]) != NULL) {
        // open quote 
        qpos_open = pos;
        quote = prefix[pos];
        qcount++;
      }
      else if (qcount % 2 == 1 && prefix[pos] == quote) {
        // close quote
        qpos_close = pos;
        qcount++;
      }
      else if (strchr(non_word_chars, prefix[pos]) != NULL) {
        qpos_close = -1;
      }
      ssize_t ofs = str_next_ofs( prefix, len, pos, true, NULL );
      if (ofs <= 0) break;
      pos += ofs;
    }    
    if ((qcount % 2 == 0 && qpos_close >= 0) || // if the last quote is only followed by word chars, we still complete it
        (qcount % 2 == 1))                     // opening quote found
    {
      quote_len = (len - qpos_open - 1);
      pos = qpos_open + 1;  // pos points to the word start just after the quote.
    }
    else {
      quote = 0;
    }
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
  // if (len == pos) return;

  // allocate new unescaped word prefix
  char* word = mem_strndup( cenv->env->mem, prefix + pos, (quote==0 ? len - pos : quote_len));
  if (word == NULL) return;

  if (quote == 0) {
    // unescape prefix
    ssize_t wlen = len - pos;
    ssize_t wpos = 0;
    while( wpos < wlen ) {
      ssize_t ofs = str_next_ofs(word, wlen, wpos, true, NULL);
      if (ofs <= 0) break;
      if (word[wpos] == escape_char && word[wpos+1] != 0 && strchr(non_word_chars, word[wpos+1]) != NULL) {
        rp_memmove(word + wpos, word + wpos + 1, wlen - wpos /* including 0 */);
      }
      wpos += ofs;
    }
  }
  #ifdef _WIN32
  else {
    // remove inner quote: "c:\Program Files\"Win
    ssize_t wlen = len - pos;
    ssize_t wpos = 0;
    while (wpos < wlen) {
      ssize_t ofs = str_next_ofs(word, wlen, wpos, true, NULL);
      if (ofs <= 0) break;
      if (word[wpos] == escape_char && word[wpos+1] == quote) {
        word[wpos+1] = escape_char;
        rp_memmove(word + wpos, word + wpos + 1, wlen - wpos /* including 0 */);
      }
      wpos += ofs;
    }
  }
  #endif

  // set up the closure
  word_closure_t wenv;
  wenv.quote          = quote;
  wenv.non_word_chars = non_word_chars;
  wenv.escape_char    = escape_char;
  wenv.delete_before_adjust = (long)(len - pos);
  wenv.prev_complete  = cenv->complete;
  wenv.prev_env       =  cenv->env;
  wenv.sbuf = sbuf_new(cenv->env->mem, true);
  if (wenv.sbuf == NULL) { mem_free(cenv->env->mem, word); return; }
  cenv->complete = &word_add_completion_ex;
  cenv->closure = &wenv;

  // and call the user completion routine
  (*fun)( cenv, word );

  // restore the original environment
  cenv->complete = wenv.prev_complete;
  cenv->closure = wenv.prev_env;

  sbuf_free(wenv.sbuf);
  mem_free(cenv->env->mem, word);  
}




//-------------------------------------------------------------
// Complete file names
// Listing files
//-------------------------------------------------------------

#if defined(_WIN32)
#include <io.h>
#include <sys/stat.h>

static bool os_is_dir(const char* cpath) {  
  struct _stat64 st = { 0 };
  _stat64(cpath, &st);
  return ((st.st_mode & _S_IFDIR) != 0);  // true for symbolic link as well
}

#define dir_cursor intptr_t
#define dir_entry  struct __finddata64_t

static bool os_findfirst(alloc_t* mem, const char* path, dir_cursor* d, dir_entry* entry) {
  stringbuf_t* spath = sbuf_new(mem,true);
  if (spath == NULL) return false;
  sbuf_append(spath, path);
  sbuf_append(spath, "\\*");
  *d = _findfirsti64(sbuf_string(spath), entry);
  mem_free(mem,spath);
  return (*d != -1);
}

static bool os_findnext(dir_cursor d, dir_entry* entry) {
  return (_findnexti64(d, entry) == 0);  
}

static void os_findclose(dir_cursor d) {
  _findclose(d);
}

static const char* os_direntry_name(dir_entry* entry) {
  return entry->name;  
}

static bool os_path_is_absolute( const char* path ) {
  if (path != NULL && path[0] != 0 && path[1] == ':' && (path[2] == '\\' || path[2] == '/' || path[2] == 0)) {
    char drive = path[0];
    return ((drive >= 'A' && drive <= 'Z') || (drive >= 'a' && drive <= 'z'));
  }
  else return false;
}

rp_private char rp_dirsep(void) {
  return '\\';
}
#else

#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <errno.h>

static bool os_is_dir(const char* cpath) {
  struct stat st = { 0 };
  stat(cpath, &st);
  return ((st.st_mode & S_IFDIR) != 0);
}

#define dir_cursor DIR*
#define dir_entry  struct dirent*

static bool os_findnext(dir_cursor d, dir_entry* entry) {
  *entry = readdir(d);
  return (*entry != NULL);
}

static bool os_findfirst(alloc_t* mem, const char* cpath, dir_cursor* d, dir_entry* entry) {
  rp_unused(mem);
  *d = opendir(cpath);
  if (*d == NULL) {
    return false;
  }
  else {
    return os_findnext(*d, entry);
  }
}

static void os_findclose(dir_cursor d) {
  closedir(d);
}

static const char* os_direntry_name(dir_entry* entry) {
  return (*entry)->d_name;  
}

static bool os_path_is_absolute( const char* path ) {
  return (path != NULL && path[0] == '/');
}

rp_private char rp_dirsep(void) {
  return '/';
}
#endif



//-------------------------------------------------------------
// File completion 
//-------------------------------------------------------------

static bool ends_with_n(const char* name, ssize_t name_len, const char* ending, ssize_t len) {
  if (name_len < len) return false;
  if (ending == NULL || len <= 0) return true;
  for (ssize_t i = 1; i <= len; i++) {
    char c1 = name[name_len - i];
    char c2 = ending[len - i];
    #ifdef _WIN32
    if (rp_tolower(c1) != rp_tolower(c2)) return false;
    #else
    if (c1 != c2) return false;
    #endif
  }
  return true;
}

static bool match_extension(const char* name, const char* extensions) {
  if (extensions == NULL || extensions[0] == 0) return true;
  if (name == NULL) return false;
  ssize_t name_len = rp_strlen(name);
  ssize_t len = rp_strlen(extensions);
  ssize_t cur = 0;  
  for (ssize_t end = 0; end <= len; end++) {
    if (extensions[end] == ';' || extensions[end] == 0) {
      if (ends_with_n(name, name_len, extensions+cur, (end - cur))) {
        return true;
      }
      cur = end+1;
    }
  }
  return false;
}

static bool filename_complete_indir( rp_completion_env_t* cenv, stringbuf_t* dir, 
                                      stringbuf_t* dir_prefix, const char* base_prefix, 
                                        char dir_sep, const char* extensions ) 
{
  dir_cursor d = 0;
  dir_entry entry;
  bool cont = true;
  if (os_findfirst(cenv->env->mem, sbuf_string(dir), &d, &entry)) {
    do {
      const char* name = os_direntry_name(&entry);
      if (name != NULL && strcmp(name, ".") != 0 && strcmp(name, "..") != 0 && 
          rp_istarts_with(name, base_prefix))
      {
        // possible match, first check if it is a directory
        bool isdir;
        const ssize_t plen = sbuf_len(dir_prefix);
        sbuf_append(dir_prefix, name);
        { // check directory and potentially add a dirsep to the dir_prefix
          const ssize_t dlen = sbuf_len(dir);
          sbuf_append_char(dir,rp_dirsep());
          sbuf_append(dir,name);
          isdir = os_is_dir(sbuf_string(dir));
          if (isdir && dir_sep != 0) {
            sbuf_append_char(dir_prefix,dir_sep); 
          }
          sbuf_delete_from(dir,dlen);  // restore dir
        }
        if (isdir || match_extension(name, extensions)) {
          cont = rp_add_completion(cenv, NULL, sbuf_string(dir_prefix));
        }
        sbuf_delete_from( dir_prefix, plen ); // restore dir_prefix
      }
    } while (cont && os_findnext(d, &entry));
    os_findclose(d);
  }
  return cont;
}

typedef struct filename_closure_s {
  const char* roots;
  const char* extensions;
  char        dir_sep;
} filename_closure_t;

static void filename_completer( rp_completion_env_t* cenv, const char* prefix ) {
  if (prefix == NULL) return;
  filename_closure_t* fclosure = (filename_closure_t*)cenv->arg;  
  stringbuf_t* root_dir = sbuf_new(cenv->env->mem,true);
  stringbuf_t* dir_prefix = sbuf_new(cenv->env->mem,true);
  if (root_dir!=NULL && dir_prefix != NULL) 
  {
    // split prefix in dir_prefix / base.
    const char* base = strrchr(prefix,'/');
    #ifdef _WIN32
    const char* base2 = strrchr(prefix,'\\');
    if (base == NULL || base2 > base) base = base2;
    #endif
    if (base != NULL) {
      base++; 
      sbuf_append_n(dir_prefix, prefix, base - prefix ); // includes dir separator
    }

    // absolute path
    if (os_path_is_absolute(prefix)) {
      // do not use roots but try to complete directly
      if (base != NULL) {
        sbuf_append_n( root_dir, prefix, (base - prefix));  // include dir separator
      }
      filename_complete_indir( cenv, root_dir, dir_prefix, (base != NULL ? base : prefix), 
                                  fclosure->dir_sep, fclosure->extensions );   
    }
    else {
      // relative path, complete with respect to every root.
      const char* next;
      const char* root = fclosure->roots;
      while ( root != NULL ) {
        // create full root in `root_dir`
        sbuf_clear(root_dir);
        next = strchr(root,';');
        if (next == NULL) {
          sbuf_append( root_dir, root );
          root = NULL;
        }
        else {
          sbuf_append_n( root_dir, root, next - root );
          root = next + 1;
        }      
        sbuf_append_char( root_dir, rp_dirsep());
          
        // add the dir_prefix to the root
        if (base != NULL) {
          sbuf_append_n( root_dir, prefix, (base - prefix) - 1);
        }

        // and complete in this directory    
        filename_complete_indir( cenv, root_dir, dir_prefix, (base != NULL ? base : prefix), 
                                   fclosure->dir_sep, fclosure->extensions);
      }
    }
  }
  mem_free(cenv->env->mem, root_dir);
  mem_free(cenv->env->mem, dir_prefix);
}

rp_public void rp_complete_filename( rp_completion_env_t* cenv, const char* prefix, char dir_sep, const char* roots, const char* extensions ) {
  if (roots == NULL) roots = ".";
  if (extensions == NULL) extensions = "";
  if (dir_sep == 0) dir_sep = rp_dirsep();
  filename_closure_t fclosure;
  fclosure.dir_sep = dir_sep;
  fclosure.roots = roots; 
  fclosure.extensions = extensions;
  cenv->arg = &fclosure;
  rp_complete_quoted_word( cenv, prefix, &filename_completer, " \t\r\n`@$><=;|&{(", '\\', "'\"");  
}
