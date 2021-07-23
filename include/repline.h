/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#pragma once
#ifndef RP_REPLINE_H
#define RP_REPLINE_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>    // size_t
#include <stdbool.h>   // bool

//--------------------------------------------------------------
// Main interface
//--------------------------------------------------------------

// The readline abstract environment.
struct rp_env_s;
typedef struct rp_env_s rp_env_t;

// Initialize and create a repline environment.
// See also: `rp_init_custom_alloc`.
rp_env_t* rp_init(void);

// Release the repline environment. 
// This is called automatically on program exit for unreleased environments.
void rp_done(rp_env_t* env);

// Read input from the user using rich editing abilities.
// The `prompt_text` can be NULL for the default (""). 
// The displayed prompt becomes `prompt_text` followed by the `prompt_marker` ("> "). 
// Returns the heap allocated input on succes, which should be `free`d by the caller.  
// Returns NULL on error, or if the user typed ctrl+d or ctrl+c.
//
// If the standard input (`stdin`) has no editing capability 
// (like a dumb terminal (e.g. `TERM`=`dumb`), running in a debuggen, a pipe or redirected file, etc.)
// the input is read directly from the input stream up to the 
// next line without editing capability.
//
// See also: `rp_set_prompt_marker`, `rp_set_prompt_color`.
char* rp_readline(rp_env_t* env, const char* prompt_text);   


//--------------------------------------------------------------
// History
//--------------------------------------------------------------

// Enable history. Use a NULL filename to not persist the history. Use -1 for max_entries to get the default (200).
void rp_set_history(rp_env_t* env, const char* fname, long max_entries );

// The last returned input from `rp_readline` is automatically added to the history; this function removes it.
void rp_history_remove_last(rp_env_t* env);

// Clear the history.
void rp_history_clear(rp_env_t* env);


//--------------------------------------------------------------
// Basic Completion
//--------------------------------------------------------------

// A completion environment
struct rp_completion_env_s;
typedef struct rp_completion_env_s rp_completion_env_t;

// A completion callback that is called by repline when tab is pressed.
// It is passed a completion environment (containing the current input and the current cursor position), 
// the current input up-to the cursor (`prefix`)
// and the user given argument when the callback was set.
// When using completion transformers, like `rp_complete_quoted_word` the `prefix` contains the
// the word to be completed without escape characters or quotes.
typedef void (rp_completer_fun_t)(rp_completion_env_t* cenv, const char* prefix );

// Set the completion handler.
// The `arg` is passed to every `completer` call by repline as is (and can be NULL).
// This can be used to propagate user state to the `completer`.
// There can only be one completion function, setting it again disables the previous one.
// The initial completer use `rp_complete_filename`.
void rp_set_completer( rp_env_t* env, rp_completer_fun_t* completer, void* arg);

// In a completion callback, use this function to add completions.
// The `display` is used to display the completion in the completion menu.
// (both `display` and `completion` are copied by repline and do not need to be preserved or allocated).
//
// Returns `true` if the callback should continue trying to find more possible completions.
// If `false` is returned, the callback should try to return and not add more completions (for improved latency).
bool rp_add_completion( rp_completion_env_t* cenv, const char* display, const char* completion );


// Complete a filename given a semi-colon separated list of root directories `roots`. 
// If `roots` is NULL, the current directory it the root ("."). 
// Each root directory should _not_ end with a directory separator.
// If a directory is completed, the `dir_separator` is added at the end if it is not `0`.
// Usually the `dir_separator` is `/` but it can be set to `\\` on Windows systems.
// For example:
// ```
// /ho         --> /home/
// /home/.ba   --> /home/.bashrc
// ```
void rp_complete_filename( rp_completion_env_t* cenv, const char* prefix, char dir_separator, const char* roots );

// Complete a _word_; calls the user provided function `fun` to complete while taking
// care of quotes and escape characters. Almost all user provided completers should use
// this function. The `prefix` passed to `fun` is modified to be unquoted and unescaped, and 
// any results from `rp_add_completion` are automatically quoted and escaped again.
// For example, completing `hello world`, the `fun` always just completes `hel` or `hello w` to `hello world`, 
// but depending on user input, it will complete as:
// ```
// hel        -->  hello\ world
// hello\ w   -->  hello\ world
// hello w    -->                   # no completion, the word is just 'w'>
// "hel       -->  "hello world" 
// "hello w   -->  "hello world"
// ```
// with proper quotes and escapes.
// See `rp_complete_quoted_word` to customize the word boundary, quotes etc.
void rp_complete_word( rp_completion_env_t* cenv, const char* prefix, rp_completer_fun_t* fun );


// Complete a _word_; calls the user provided function `fun` to complete while taking
// care of quotes and escape characters. Almost all user provided completers should use this function. 
// The `non_word_chars` is a set of characters that cannot be a word. Use NULL for the default " \r\t\n".
// The `escape_char` is the escaping character, usually `\` but use 0 to not have escape characters.
// The `quote_chars` define the quotes, use NULL for the default `"\'\""` quotes.
// See `rp_complete_word` which uses the default values for `non_word_chars`, `quote_chars` and `\` for escape characters.
void rp_complete_quoted_word( rp_completion_env_t* cenv, const char* prefix, rp_completer_fun_t fun, const char* non_word_chars, char escape_char, const char* quote_chars );

//--------------------------------------------------------------
// Customization
//--------------------------------------------------------------

// Available ANSI colors.
typedef enum rp_color_e {
  RP_BLACK      = 30,
  RP_MAROON,
  RP_GREEN,
  RP_ORANGE,
  RP_NAVY,
  RP_PURPLE,
  RP_TEAL,
  RP_LIGHTGRAY,
  RP_DARKGRAY   = 90,
  RP_RED,
  RP_LIME,
  RP_YELLOW,
  RP_BLUE,
  RP_MAGENTA,
  RP_CYAN,
  RP_WHITE,
  RP_DEFAULT_COLOR = 39
} rp_color_t;

// Set a prompt marker. Pass NULL for the default marker ("> ").
void rp_set_prompt_marker( rp_env_t* env, const char* prompt_marker );

// Set the color used for the prompt text and marker.
void rp_set_prompt_color( rp_env_t* env, rp_color_t color );

// Disable or enable multi-line input (enabled by default).
void rp_enable_multiline( rp_env_t* env, bool enable );

// Disable or enable sound (enabled by default).
// A beep is used when tab cannot find any completion for example.
void rp_enable_beep( rp_env_t* env, bool enable );

// Disable or enable color output (enabled by default).
void rp_enable_color( rp_env_t* env, bool enable );

// Disable or enable duplicate entries in the history (disabled by default).
void rp_enable_history_duplicates( rp_env_t* env, bool enable );



//--------------------------------------------------------------
// Advanced Completion
//--------------------------------------------------------------

// Get the raw current input (and cursor position if `cursor` != NULL) for the completion.
// Usually completer functions should look at their `prefix` though as transformers
// like `rp_complete_word` may modify the prefix (for example, unescape it).
const char* rp_completion_input( rp_completion_env_t* cenv, long* cursor );

// Get the current repline environment.
rp_env_t* rp_completion_env( rp_completion_env_t* cenv );

// Get the completion argument passed to `rp_set_completer`.
void* rp_completion_arg( rp_completion_env_t* cenv );

// Do we have already some completions?
bool rp_has_completions( rp_completion_env_t* cenv );


// Primitive completion, cannot be used with most transformers (like `rp_complete_word` and `rp_complete_quoted_word`).
// When completed, `delete_before` _bytes_ are deleted before the cursor position,
// `delete_after` _bytes_ are deleted after the cursor, and finally `completion` is inserted.
// The `display` is used to display the completion in the completion menu.
// (both `display` and `completion` are copied by repline and do not need to be preserved or allocated).
//
// Returns `true` if the callback should continue trying to find more possible completions.
// If `false` is returned, the callback should try to return and not add more completions (for improved latency).
bool rp_add_completion_ex( rp_completion_env_t* cenv, const char* display, const char* completion, long delete_before, long delete_after);

// Convenience: return the position of a previous code point in a UTF-8 string `s` from postion `pos`.
// Returns `-1` if `pos <= 0` or `pos > strlen(s)` (or other errors).
long rp_prev_char( const char* s, long pos );

// Convenience: return the position of the next code point in a UTF-8 string `s` from postion `pos`.
// Returns `-1` if `pos < 0` or `pos >= strlen(s)` (or other errors).
long rp_next_char( const char* s, long pos );

// Convenience: does a string `s` starts with a given `prefix` ?
bool rp_starts_with( const char* s, const char* prefix );

// Convenience: does a string `s` starts with a given `prefix` ignoring (ascii) case?
bool rp_istarts_with( const char* s, const char* prefix );


//--------------------------------------------------------------
// Register allocation functions for custom allocators
//--------------------------------------------------------------

typedef void* (rp_malloc_fun_t)( size_t size );
typedef void* (rp_realloc_fun_t)( void* p, size_t newsize );
typedef void  (rp_free_fun_t)( void* p );

// Initialize with custom allocation functions.
rp_env_t* rp_init_custom_alloc( rp_malloc_fun_t* _malloc, rp_realloc_fun_t* _realloc, rp_free_fun_t* _free );


void rp_free( rp_env_t* env, void* p );

#ifdef __cplusplus
}
#endif

#endif // RP_REPLINE_H
