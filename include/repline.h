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

#define RP_VERSION   (100)    // 1.0.0

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
char* rp_readline(const char* prompt_text);   


//--------------------------------------------------------------
// History
//--------------------------------------------------------------

// Enable history. Use a NULL filename to not persist the history. Use -1 for max_entries to get the default (200).
void rp_set_history(const char* fname, long max_entries );

// The last returned input from `rp_readline` is automatically added to the history; this function removes it.
void rp_history_remove_last(void);

// Clear the history.
void rp_history_clear(void);

// Add an entry to the history
void rp_history_add( const char* entry );

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

// Set the default completion handler.
// There can only be one default completion function, setting it again disables the previous one.
// The initial completer use `rp_complete_filename`.
void rp_set_default_completer( rp_completer_fun_t* completer, void* arg);


// Read input from the user using rich editing abilities, 
// using a particular completion funcion for this call only.
// See also: `rp_readline`, `rp_set_prompt_marker`, `rp_set_prompt_color`.
char* rp_readline_with_completer(const char* prompt_text, rp_completer_fun_t* completer, void* completer_arg );   


// In a completion callback, use this function to add completions.
// The `display` is used to display the completion in the completion menu.
// (both `display` and `completion` are copied by repline and do not need to be preserved or allocated).
//
// Returns `true` if the callback should continue trying to find more possible completions.
// If `false` is returned, the callback should try to return and not add more completions (for improved latency).
bool rp_add_completion( rp_completion_env_t* cenv, const char* display, const char* completion );

bool rp_add_completions(rp_completion_env_t* cenv, const char* prefix, const char** completions);

// Complete a filename given a semi-colon separated list of root directories `roots` and 
// semi-colon separated list of possible extensions (excluding directories). 
// If `roots` is NULL, the current directory is the root ("."). 
// If `extensions` is NULL, any extension will match.
// Each root directory should _not_ end with a directory separator.
// If a directory is completed, the `dir_separator` is added at the end if it is not `0`.
// Usually the `dir_separator` is `/` but it can be set to `\\` on Windows systems.
// For example:
// ```
// /ho         --> /home/
// /home/.ba   --> /home/.bashrc
// ```
void rp_complete_filename( rp_completion_env_t* cenv, const char* prefix, char dir_separator, const char* roots, const char* extensions );

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
// Basic syntax highlighting
//--------------------------------------------------------------

// Available ANSI colors.
typedef enum rp_color_e {
  RP_COLOR_NONE = 0,
  RP_BLACK = 30,
  RP_MAROON,
  RP_GREEN,
  RP_ORANGE,
  RP_BROWN = RP_ORANGE,
  RP_NAVY,
  RP_PURPLE,
  RP_TEAL,
  RP_LIGHTGRAY,
  RP_LIGHTGREY = RP_LIGHTGRAY,
  RP_DARKGRAY = 90,
  RP_DARKGREY = RP_DARKGRAY,
  RP_RED,
  RP_LIME,
  RP_YELLOW,
  RP_BLUE,
  RP_MAGENTA,
  RP_CYAN,
  RP_WHITE,
  RP_COLOR_DEFAULT = 39    // default terminal color
} rp_color_t;

// A syntax highlight environment
struct rp_highlight_env_s;
typedef struct rp_highlight_env_s rp_highlight_env_t;

// A syntax highlighter callback that is called by readline to syntax highlight user input.
typedef void (rp_highlight_fun_t)(rp_highlight_env_t* henv, const char* input, void* arg);

// Set a syntax highlighter.
// There can only be one highlight function, setting it again disables the previous one.
void rp_set_highlighter(rp_highlight_fun_t* highlighter, void* arg);

// Set the color of characters starting at position `pos` to `color`.
void rp_highlight_color(rp_highlight_env_t* henv, long pos, rp_color_t color );

// Set the background color of characters starting at position `pos` to `bgcolor`.
void rp_highlight_bgcolor(rp_highlight_env_t* henv, long pos, rp_color_t bgcolor);

// Enable/Disable underlining for characters starting at position `pos`.
void rp_highlight_underline(rp_highlight_env_t* henv, long pos, bool enable );

// Enable/Disable reverse video for characters starting at position `pos`.
void rp_highlight_reverse(rp_highlight_env_t* henv, long pos, bool enable);


// Convenience callback for a function that highlights `s` using ANSI CSI SGR escape sequences (`ESC [ <code> m`)
// The returned string should be allocated and is free'd by the caller.
// See: <https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_(Select_Graphic_Rendition)_parameters>
typedef char* (rp_highlight_esc_fun_t)(const char* s, void* arg);

// Convenience function for highlighting with ANSI CSI SGR escape sequences (`ESC [ <code> m`).
// Can be called in a `rp_highlight_fun_t` callback to colorize the `input` using the 
// the provided `highlight` function that returns the original `input` interspersed with 
// ANSI CSI SGR color sequences. User state is passed through the `arg`. 
// See: <https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_(Select_Graphic_Rendition)_parameters>
void rp_highlight_esc(rp_highlight_env_t* henv, const char* input, rp_highlight_esc_fun_t* highlight, void* arg);


//--------------------------------------------------------------
// Customization
//--------------------------------------------------------------

// Set a prompt marker and a potential marker for extra lines with multiline input. 
// Pass NULL for the `prompt_marker` for the default marker ("> ").
// Pass NULL for continuation prompt marker to make it equal to the `prompt_marker`.
void rp_set_prompt_marker( const char* prompt_marker, const char* continuation_prompt_marker );

// Set the color used for the prompt text and marker.
void rp_set_prompt_color( rp_color_t color );

// Disable or enable multi-line input (enabled by default).
void rp_enable_multiline( bool enable );

// Disable or enable sound (enabled by default).
// A beep is used when tab cannot find any completion for example.
void rp_enable_beep( bool enable );

// Disable or enable color output (enabled by default).
void rp_enable_color( bool enable );

// Disable or enable duplicate entries in the history (disabled by default).
void rp_enable_history_duplicates( bool enable );

// Disable or enable automatic tab completion after a completion 
// to expand as far as possible if the completions are unique. (disabled by default).
void rp_enable_auto_tab( bool enable );

// Disable or enable preview of a completion selection (enabled by default)
void rp_enable_completion_preview( bool enable );

// Disable or enable automatic identation of continuation lines in multiline
// input so it aligns with the initial prompt.
void rp_enable_multiline_indent(bool enable);

// Disable or enable display of short help messages for history search etc.
// (full help is always dispayed when pressing F1 regardless of this setting)
void rp_enable_inline_help(bool enable);

// Disable or enable hinting (enabled by default)
// Shows a hint inline when there is a single possible completion.
void rp_enable_hint(bool enable);

// Disable or enable syntax highlighting (enabled by default).
// This applies regardless whether a syntax highlighter callback was set (`rp_set_highlighter`)
void rp_enable_highlight(bool enable);

// Set the color used for interface elements:
//
// - info: for example, numbers in the completion menu (`RP_DARKGRAY` by default)
// - diminish: for example, non matching parts in a history search (`RP_LIGHTGRAY` by default)
// - emphasis: for example, the matching part in a history search (`RP_WHITE` by default)
// - hint: for hints.
//
// Use `RP_COLOR_NONE` to use the default color. (but `RP_COLOR_DEFAULT` for the default terminal text color!)

void rp_set_iface_colors( rp_color_t color_info, rp_color_t color_diminish, rp_color_t color_emphasis, rp_color_t color_hint );

//--------------------------------------------------------------
// Advanced Completion
//--------------------------------------------------------------

// Get the raw current input (and cursor position if `cursor` != NULL) for the completion.
// Usually completer functions should look at their `prefix` though as transformers
// like `rp_complete_word` may modify the prefix (for example, unescape it).
const char* rp_completion_input( rp_completion_env_t* cenv, long* cursor );

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


// Convenience: function that returns whether a (utf8) character is in a certain class
typedef bool (rp_is_char_class_fun_t)(const char* s, long len);

// Convenience: character class for whitespace `[ \t\r\n]`.
bool rp_char_is_white(const char* s, long len);

// Convenience: character class for letters (`[A-Za-z]` and any unicode > 0x80).
bool rp_char_is_letter(const char* s, long len);

// Convenience: character class for digits (`[0-9]`).
bool rp_char_is_digit(const char* s, long len);

// Convenience: character class for identifier letters (`[A-Za-z0-9_-]` and any unicode > 0x80).
bool rp_char_is_idletter(const char* s, long len);

// Convenience: character class for filename letters (`[^ \t\r\n`@$><=;|&{}()[]`).
bool rp_char_is_filename_letter(const char* s, long len);

// Convenience: If this is a token start, return the length.
long rp_is_token(const char* s, long pos, rp_is_char_class_fun_t* is_token_char);

// Convenience: Does this match the spefied token? 
// Ensures not to match prefixes or suffixes, and returns the length of the match (in bytes).
// E.g. `rp_match_token("function",0,&rp_char_is_letter,"fun")` returns 0.
long rp_match_token(const char* s, long pos, rp_is_char_class_fun_t* is_token_char, const char* token);

// Convenience: Do any of the specified tokens match? 
// Ensures not to match prefixes or suffixes, and returns the length of the match (in bytes).
// E.g. `rp_match_any_token("function",0,&rp_char_is_letter,{"fun","func",NULL})` returns 0.
long rp_match_any_token(const char* s, long pos, rp_is_char_class_fun_t* is_token_char, const char** tokens);


//--------------------------------------------------------------
// Terminal, experimental
//--------------------------------------------------------------

void rp_write(const char* s);
void rp_writeln(const char* s);


//--------------------------------------------------------------
// Register allocation functions for custom allocators
//--------------------------------------------------------------

typedef void* (rp_malloc_fun_t)( size_t size );
typedef void* (rp_realloc_fun_t)( void* p, size_t newsize );
typedef void  (rp_free_fun_t)( void* p );

// Initialize with custom allocation functions.
// This must be called as the first function in a program!
void rp_init_custom_alloc( rp_malloc_fun_t* _malloc, rp_realloc_fun_t* _realloc, rp_free_fun_t* _free );

// Free a potentially custom alloc'd pointer (in particular, the result returned from `rp_readline`)
void rp_free( void* p );

#ifdef __cplusplus
}
#endif

#endif // RP_REPLINE_H
