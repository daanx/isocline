/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#pragma once
#ifndef IC_ISOCLINE_H
#define IC_ISOCLINE_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>    // size_t
#include <stdbool.h>   // bool
#include <stdint.h>    // uint32_t
#include <stdarg.h>    // term_vprintf

//--------------------------------------------------------------
// Main interface
//--------------------------------------------------------------

#define IC_VERSION   (100)    // 1.0.0

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
// See also: `ic_set_prompt_marker`, `ic_set_style_color`.
char* ic_readline(const char* prompt_text);   


//--------------------------------------------------------------
// History
//--------------------------------------------------------------

// Enable history. Use a NULL filename to not persist the history. Use -1 for max_entries to get the default (200).
void ic_set_history(const char* fname, long max_entries );

// The last returned input from `ic_readline` is automatically added to the history; this function removes it.
void ic_history_remove_last(void);

// Clear the history.
void ic_history_clear(void);

// Add an entry to the history
void ic_history_add( const char* entry );

//--------------------------------------------------------------
// Basic Completion
//--------------------------------------------------------------

// A completion environment
struct ic_completion_env_s;
typedef struct ic_completion_env_s ic_completion_env_t;

// A completion callback that is called by isocline when tab is pressed.
// It is passed a completion environment (containing the current input and the current cursor position), 
// the current input up-to the cursor (`prefix`)
// and the user given argument when the callback was set.
// When using completion transformers, like `ic_complete_quoted_word` the `prefix` contains the
// the word to be completed without escape characters or quotes.
typedef void (ic_completer_fun_t)(ic_completion_env_t* cenv, const char* prefix );

// Set the default completion handler.
// There can only be one default completion function, setting it again disables the previous one.
// The initial completer use `ic_complete_filename`.
void ic_set_default_completer( ic_completer_fun_t* completer, void* arg);


// In a completion callback (usually from `ic_complete_word`), use this function to add a completion.
// The `display` is used to display the completion in the completion menu.
// (both `display` and `completion` are copied by isocline and do not need to be preserved or allocated).
//
// Returns `true` if the callback should continue trying to find more possible completions.
// If `false` is returned, the callback should try to return and not add more completions (for improved latency).
bool ic_add_completion( ic_completion_env_t* cenv, const char* display, const char* completion );

// In a completion callback (usually from `ic_complete_word`), use this function to add completions.
// The `completions` array should be terminated with a NULL element, and all elements
// are added as completions if they start with `prefix`.
//
// Returns `true` if the callback should continue trying to find more possible completions.
// If `false` is returned, the callback should try to return and not add more completions (for improved latency).
bool ic_add_completions(ic_completion_env_t* cenv, const char* prefix, const char** completions);

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
// (This already uses `ic_complete_quoted_word` so do not call it from inside a word handler).
void ic_complete_filename( ic_completion_env_t* cenv, const char* prefix, char dir_separator, const char* roots, const char* extensions );

// Complete a _word_; calls the user provided function `fun` to complete while taking
// care of quotes and escape characters. Almost all user provided completers should use
// this function. The `prefix` passed to `fun` is modified to be unquoted and unescaped, and 
// any results from `ic_add_completion` are automatically quoted and escaped again.
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
// See `ic_complete_quoted_word` to customize the word boundary, quotes etc.
void ic_complete_word( ic_completion_env_t* cenv, const char* prefix, ic_completer_fun_t* fun );


// Function that returns whether a (utf8) character (of length `len`) is in a certain character class
// See `ic_char_is_<xxx>` below.
typedef bool (ic_is_char_class_fun_t)(const char* s, long len);

// Complete a _word_; calls the user provided function `fun` to complete while taking
// care of quotes and escape characters. Almost all user provided completers should use this function. 
// The `non_word_chars` is a set of characters that are part of a "word". Use NULL for the default (`&ic_char_is_nonseparator`).
// The `escape_char` is the escaping character, usually `\` but use 0 to not have escape characters.
// The `quote_chars` define the quotes, use NULL for the default `"\'\""` quotes.
// See `ic_complete_word` which uses the default values for `non_word_chars`, `quote_chars` and `\` for escape characters.
void ic_complete_quoted_word( ic_completion_env_t* cenv, const char* prefix, ic_completer_fun_t fun, 
                                ic_is_char_class_fun_t* is_word_char, char escape_char, const char* quote_chars );


//--------------------------------------------------------------
// Basic syntax highlighting
//--------------------------------------------------------------

// A color is either RGB or an ANSI code.
// (RGB colors have bit 24 set to distinguish them from the ANSI color palette colors.)
// (Isocline will automatically convert from RGB on terminals that do not support full colors)
typedef uint32_t ic_color_t;

// Create a color from a 24-bit color value.
ic_color_t ic_rgb(uint32_t hex);

// Create a color from a 8-bit red/green/blue components.
ic_color_t ic_rgbx(uint8_t r, uint8_t g, uint8_t b);


#define IC_COLOR_NONE     (0)
#define IC_RGB(rgb)       ic_rgb(rgb)

// ANSI colors.
// The actual colors used is usually determined by the terminal theme
// See <https://en.wikipedia.org/wiki/ANSI_escape_code#3-bit_and_4-bit>
#define IC_ANSI_BLACK     (30)
#define IC_ANSI_MAROON    (31)
#define IC_ANSI_GREEN     (32)
#define IC_ANSI_ORANGE    (33)
#define IC_ANSI_NAVY      (34)
#define IC_ANSI_PURPLE    (35)
#define IC_ANSI_TEAL      (36)
#define IC_ANSI_LIGHTGRAY (37)
#define IC_ANSI_DEFAULT   (39)

#define IC_ANSI_DARKGRAY  (90)
#define IC_ANSI_RED       (91)
#define IC_ANSI_LIME      (92)
#define IC_ANSI_YELLOW    (93)
#define IC_ANSI_BLUE      (94)
#define IC_ANSI_MAGENTA   (95)
#define IC_ANSI_CYAN      (96)
#define IC_ANSI_WHITE     (97)

// Predefined RGB colors.
#define IC_BLACK          IC_RGB(0x000000)
#define IC_MAROON         IC_RGB(0x800000)
#define IC_GREEN          IC_RGB(0x008000)
#define IC_ORANGE         IC_RGB(0x808000)
#define IC_NAVY           IC_RGB(0x000080)
#define IC_PURPLE         IC_RGB(0x800080)
#define IC_TEAL           IC_RGB(0x008080)
#define IC_LIGHTGRAY      IC_RGB(0xC0C0C0)

#define IC_DARKGRAY       IC_RGB(0x808080)
#define IC_RED            IC_RGB(0xFF0000)
#define IC_LIME           IC_RGB(0x00FF00)
#define IC_YELLOW         IC_RGB(0xFFFF00)
#define IC_BLUE           IC_RGB(0x0000FF)
#define IC_MAGENTA        IC_RGB(0xFF00FF)
#define IC_CYAN           IC_RGB(0x00FFFF)
#define IC_WHITE          IC_RGB(0xFFFFFF)


// A syntax highlight environment
struct ic_highlight_env_s;
typedef struct ic_highlight_env_s ic_highlight_env_t;

// A syntax highlighter callback that is called by readline to syntax highlight user input.
typedef void (ic_highlight_fun_t)(ic_highlight_env_t* henv, const char* input, void* arg);

// Set a syntax highlighter.
// There can only be one highlight function, setting it again disables the previous one.
void ic_set_default_highlighter(ic_highlight_fun_t* highlighter, void* arg);

// Set the color of characters starting at position `pos` to `color`.
// (Use a negative position to indicate a logical unicode character position).
void ic_highlight_color(ic_highlight_env_t* henv, long pos, ic_color_t color );

// Set the background color of characters starting at position `pos` to `bgcolor`.
// (Use a negative position to indicate a logical unicode character position).
void ic_highlight_bgcolor(ic_highlight_env_t* henv, long pos, ic_color_t bgcolor);

// Enable/Disable underlining for characters starting at position `pos`.
// (Use a negative position to indicate a logical unicode character position).
void ic_highlight_underline(ic_highlight_env_t* henv, long pos, bool enable );

// Enable/Disable reverse video for characters starting at position `pos`.
// (Use a negative position to indicate a logical unicode character position).
void ic_highlight_reverse(ic_highlight_env_t* henv, long pos, bool enable);


// Convenience callback for a function that highlights `s` using ANSI CSI SGR escape sequences (`ESC [ <code> m`)
// The returned string should be allocated and is free'd by the caller.
// See: <https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_(Select_Graphic_Rendition)_parameters>
typedef char* (ic_highlight_esc_fun_t)(const char* s, void* arg);

// Convenience function for highlighting with ANSI CSI SGR escape sequences (`ESC [ <code> m`).
// Can be called in a `ic_highlight_fun_t` callback to colorize the `input` using the 
// the provided `highlight` function that returns the original `input` interspersed with 
// ANSI CSI SGR color sequences. User state is passed through the `arg`. 
// See: <https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_(Select_Graphic_Rendition)_parameters>
void ic_highlight_esc(ic_highlight_env_t* henv, const char* input, ic_highlight_esc_fun_t* highlight, void* arg);


//--------------------------------------------------------------
// Readline with a specific completer and highlighter
//--------------------------------------------------------------

// Read input from the user using rich editing abilities, 
// using a particular completion function and highlighter for this call only.
// both can be NULL in which case the defaults are used.
// See also: `ic_readline`, `ic_set_prompt_marker`, `ic_set_style_color`,
// `ic_set_default_compteter`, `ic_set_default_highlighter`.
char* ic_readline_ex(const char* prompt_text, ic_completer_fun_t* completer, void* completer_arg,
                                              ic_highlight_fun_t* highlighter, void* highlighter_arg);


//--------------------------------------------------------------
// Customization
//--------------------------------------------------------------

// Set a prompt marker and a potential marker for extra lines with multiline input. 
// Pass NULL for the `prompt_marker` for the default marker ("> ").
// Pass NULL for continuation prompt marker to make it equal to the `prompt_marker`.
void ic_set_prompt_marker( const char* prompt_marker, const char* continuation_prompt_marker );

// Get the current prompt marker.
const char* ic_get_prompt_marker(void);

// Get the current continuation prompt marker.
const char* ic_get_contiuation_prompt_marker(void);

// Disable or enable multi-line input (enabled by default).
// Returns the previous setting.
bool ic_enable_multiline( bool enable );

// Disable or enable sound (enabled by default).
// A beep is used when tab cannot find any completion for example.
// Returns the previous setting.
bool ic_enable_beep( bool enable );

// Disable or enable color output (enabled by default).
// Returns the previous setting.
bool ic_enable_color( bool enable );

// Disable or enable duplicate entries in the history (disabled by default).
// Returns the previous setting.
bool ic_enable_history_duplicates( bool enable );

// Disable or enable automatic tab completion after a completion 
// to expand as far as possible if the completions are unique. (disabled by default).
// Returns the previous setting.
bool ic_enable_auto_tab( bool enable );

// Disable or enable preview of a completion selection (enabled by default)
// Returns the previous setting.
bool ic_enable_completion_preview( bool enable );

// Disable or enable automatic identation of continuation lines in multiline
// input so it aligns with the initial prompt.
// Returns the previous setting.
bool ic_enable_multiline_indent(bool enable);

// Disable or enable display of short help messages for history search etc.
// (full help is always dispayed when pressing F1 regardless of this setting)
// Returns the previous setting.
bool ic_enable_inline_help(bool enable);

// Disable or enable hinting (enabled by default)
// Shows a hint inline when there is a single possible completion.
// Returns the previous setting.
bool ic_enable_hint(bool enable);

// Disable or enable syntax highlighting (enabled by default).
// This applies regardless whether a syntax highlighter callback was set (`ic_set_highlighter`)
// Returns the previous setting.
bool ic_enable_highlight(bool enable);


// Styles for interface elements.
typedef enum ic_style_e {
  IC_STYLE_PROMPT,   // prompt style
  IC_STYLE_INFO,     // info: for example, numbers in the completion menu(`IC_DARKGRAY` by default)
  IC_STYLE_DIMINISH, // diminish: for example, non matching parts in a history search (`IC_LIGHTGRAY` by default)
  IC_STYLE_EMPHASIS, // emphasis: for example, the matching part in a history search (`IC_WHITE` by default)
  IC_STYLE_HINT,     // hint: for hints.
  IC_STYLE_LAST
} ic_style_t;

// Set the color used for interface elements.
// Use `IC_COLOR_NONE` to use the default color. (but `IC_COLOR_DEFAULT` for the default terminal text color!)
void ic_set_style_color( ic_style_t style, ic_color_t color );

// Get the current interface color for a given style.
ic_color_t ic_get_style_color( ic_style_t style );

//--------------------------------------------------------------
// Advanced Completion
//--------------------------------------------------------------

// Get the raw current input (and cursor position if `cursor` != NULL) for the completion.
// Usually completer functions should look at their `prefix` though as transformers
// like `ic_complete_word` may modify the prefix (for example, unescape it).
const char* ic_completion_input( ic_completion_env_t* cenv, long* cursor );

// Get the completion argument passed to `ic_set_completer`.
void* ic_completion_arg( ic_completion_env_t* cenv );

// Do we have already some completions?
bool ic_has_completions( ic_completion_env_t* cenv );

// Do we already have enough completions and should we return if possible? (for improved latency)
bool ic_stop_completing(ic_completion_env_t* cenv);


// Primitive completion, cannot be used with most transformers (like `ic_complete_word` and `ic_complete_quoted_word`).
// When completed, `delete_before` _bytes_ are deleted before the cursor position,
// `delete_after` _bytes_ are deleted after the cursor, and finally `completion` is inserted.
// The `display` is used to display the completion in the completion menu.
// (both `display` and `completion` are copied by isocline and do not need to be preserved or allocated).
//
// Returns `true` if the callback should continue trying to find more possible completions.
// If `false` is returned, the callback should try to return and not add more completions (for improved latency).
bool ic_add_completion_ex( ic_completion_env_t* cenv, const char* display, const char* completion, long delete_before, long delete_after);


//--------------------------------------------------------------
// Convenience functions for character classes, highlighting and completion.
//--------------------------------------------------------------

// Convenience: return the position of a previous code point in a UTF-8 string `s` from postion `pos`.
// Returns `-1` if `pos <= 0` or `pos > strlen(s)` (or other errors).
long ic_prev_char( const char* s, long pos );

// Convenience: return the position of the next code point in a UTF-8 string `s` from postion `pos`.
// Returns `-1` if `pos < 0` or `pos >= strlen(s)` (or other errors).
long ic_next_char( const char* s, long pos );

// Convenience: does a string `s` starts with a given `prefix` ?
bool ic_starts_with( const char* s, const char* prefix );

// Convenience: does a string `s` starts with a given `prefix` ignoring (ascii) case?
bool ic_istarts_with( const char* s, const char* prefix );


// Convenience: character class for whitespace `[ \t\r\n]`.
bool ic_char_is_white(const char* s, long len);

// Convenience: character class for non-whitespace `[^ \t\r\n]`.
bool ic_char_is_nonwhite(const char* s, long len);

// Convenience: character class for separators `[ \t\r\n,.;:/\\\(\)\{\}\[\]]`.
// This is used for word boundaries in isocline.
bool ic_char_is_separator(const char* s, long len);

// Convenience: character class for non-separators.
bool ic_char_is_nonseparator(const char* s, long len);

// Convenience: character class for letters (`[A-Za-z]` and any unicode > 0x80).
bool ic_char_is_letter(const char* s, long len);

// Convenience: character class for digits (`[0-9]`).
bool ic_char_is_digit(const char* s, long len);

// Convenience: character class for hexadecimal digits (`[A-Fa-f0-9]`).
bool ic_char_is_hexdigit(const char* s, long len);

// Convenience: character class for identifier letters (`[A-Za-z0-9_-]` and any unicode > 0x80).
bool ic_char_is_idletter(const char* s, long len);

// Convenience: character class for filename letters (_not in_ " \t\r\n`@$><=;|&\{\}\(\)\[\]]").
bool ic_char_is_filename_letter(const char* s, long len);


// Convenience: If this is a token start, return the length. Otherwise return 0.
long ic_is_token(const char* s, long pos, ic_is_char_class_fun_t* is_token_char);

// Convenience: Does this match the spefied token? 
// Ensures not to match prefixes or suffixes, and returns the length of the match (in bytes).
// E.g. `ic_match_token("function",0,&ic_char_is_letter,"fun")` returns 0.
// while `ic_match_token("fun x",0,&ic_char_is_letter,"fun"})` returns 3.
long ic_match_token(const char* s, long pos, ic_is_char_class_fun_t* is_token_char, const char* token);


// Convenience: Do any of the specified tokens match? 
// Ensures not to match prefixes or suffixes, and returns the length of the match (in bytes).
// E.g. `ic_match_any_token("function",0,&ic_char_is_letter,{"fun","func",NULL})` returns 0.
// while `ic_match_any_token("func x",0,&ic_char_is_letter,{"fun","func",NULL})` returns 4.
long ic_match_any_token(const char* s, long pos, ic_is_char_class_fun_t* is_token_char, const char** tokens);


//--------------------------------------------------------------
// Terminal output, experimental.
// Ensures basic ANSI CSI escape sequences are processed
// in a portable way (including Windows)
//--------------------------------------------------------------

// Initialize for terminal output; call this before using the terminal functions (`ic_term_xxx`)
// Does nothing on most platforms
// but on Windows it sets the console to UTF8 output and possible enables
// virtual terminal processing.
void ic_term_init(void);

// Call this when done with the terminal functions.
void ic_term_done(void);

// Flush the terminal output. (happens automatically on newline characters ('\n') as well).
void ic_term_flush(void);

// Write a string to the console (and process CSI escape sequences).
void ic_term_write(const char* s);

// Write a string to the console and end with a newline (and process CSI escape sequences).
void ic_term_writeln(const char* s);

// Write a formatted string to the console (and process CSI escape sequences)
void ic_term_printf(const char* fmt, ...);

// Write a formatted string to the console.
void ic_term_vprintf(const char* fmt, va_list args);

// Set the text color in a portable way where colors auto translate on terminals with less color.
void ic_term_color( ic_color_t color );

// Set the text back ground color in a portable way where colors auto translate on terminals with less color.
void ic_term_bgcolor( ic_color_t color );

// Set the text underline mode.
void ic_term_underline( bool enable );

// Set the text reverse video mode.
void ic_term_reverse( bool enable );

// Reset the text attributes.
void ic_term_reset( void );

// Get the palette used by the terminal:
// This is usually initialized from the COLORTERM environment variable. The 
// possible values of COLORTERM for each palette are given in parenthesis.
//  1: monochrome (monochrome)
//  3: old ANSI terminal with 8 colors, using bold for bright (8color/3bit)
//  4: regular ANSI terminal with 16 colors.     (16color/4bit)
//  8: terminal with ANSI 256 color palette.     (256color/8bit)
// 24: true-color terminal with full RGB colors. (truecolor/24bit)
int ic_term_get_color_bits( void );

//--------------------------------------------------------------
// Async support
//--------------------------------------------------------------

// Thread-safe way to asynchronously unblock a readline.
// Behaves as if the user pressed the `ctrl-C` character
// (resulting in returning NULL from `ic_readline`).
// Returns `true` if the event was successfully delivered.
// (This may not be supported on all platforms, but it is
// functional on Linux, macOS and Windows).
bool ic_async_stop(void);

//--------------------------------------------------------------
// Register allocation functions for custom allocators
//--------------------------------------------------------------

typedef void* (ic_malloc_fun_t)( size_t size );
typedef void* (ic_realloc_fun_t)( void* p, size_t newsize );
typedef void  (ic_free_fun_t)( void* p );

// Initialize with custom allocation functions.
// This must be called as the first function in a program!
void ic_init_custom_alloc( ic_malloc_fun_t* _malloc, ic_realloc_fun_t* _realloc, ic_free_fun_t* _free );

// Free a potentially custom alloc'd pointer (in particular, the result returned from `ic_readline`)
void ic_free( void* p );

// Allocate using the current memory allocator.
void* ic_malloc(size_t sz);

// Duplicate a string using the current memory allocator.
const char* ic_strdup( const char* s );

#ifdef __cplusplus
}
#endif

#endif // IC_ISOCLINE_H
