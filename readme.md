<!-- <img align="right" width="350px" src="doc/completion-macos.png"/> -->

<img align="left" src="doc/isocline-inline.svg"/>

# Isocline: a portable readline alternative.
 
Isocline is a pure C library that can be used as an alternative to the GNU readline library.

- Small: less than 8k lines and can be compiled as a single C file without 
  any dependencies or configuration (e.g. `gcc -c src/isocline.c`).
  
- Portable: works on Unix, Windows, and macOS, and uses a minimal
  subset of ANSI escape sequences.
    
- Features: extensive multi-line editing mode (`shift-tab`), (24-bit) color, history, completion, unicode, 
  undo/redo, incremental history search, inline hints, syntax highlighting, brace matching,
  closing brace insertion, auto indentation, graceful fallback, support for custom allocators, etc.
  
- License: MIT. 

- Comes with a [Haskell] binding (`System.Console.Isocline`).

Enjoy,
  Daan
  
<!--  <img align="right" width="350px" src="doc/history-win.png"/> -->
  
# Demo

![recording](doc/record-macos.svg)  

Shows in order: unicode, syntax highlighting, brace matching, jump to matching brace, auto indent, multiline editing, 24-bit colors, inline hinting, filename completion, and incremental history search.  
<sub>(screen capture was made with [termtosvg] by Nicolas Bedos)</sub>

# Usage

Include the isocline header in your C or C++ source:
```C
#include <include/isocline.h>
```

and call `ic_readline` to get user input with rich editing abilities:
```C
char* input;
while( (input = ic_readline("prompt")) != NULL ) { // ctrl+d/c or errors return NULL
  printf("you typed:\n%s\n", input); // use the input
  free(input);  
}
```

See the [example] for a full example with completion, syntax highligting, history, etc.

# Run the Example

You can compile and run the [example] as:
```
$ gcc -o example -Iinclude test/example.c src/isocline.c
$ ./example
```

or, the Haskell [example][HaskellExample]:
```
$ ghc -ihaskell test/Example.hs src/isocline.c
$ ./test/Example
```


# Editing with Isocline

Isocline tries to be as compatible as possible with standard [GNU Readline] key bindings.

### Overview:
```apl
       home/ctrl-a       cursor     end/ctrl-e
         ┌─────────────────┼───────────────┐    (navigate)
         │     ctrl-left   │  ctrl-right   │
         │         ┌───────┼──────┐        │    ctrl-r   : search history
         ▼         ▼       ▼      ▼        ▼    tab      : complete word
  prompt> it is the quintessential language     shift-tab: insert new line
         ▲         ▲              ▲        ▲    esc      : delete input, done
         │         └──────────────┘        │    ctrl-z   : undo
         │    alt-backsp        alt-d      │
         └─────────────────────────────────┘    (delete)
       ctrl-u                          ctrl-k
```

### Key Bindings

These are also shown when pressing `F1` on a Isocline prompt. We use `^` as a shorthand for `ctrl-`:

| Navigation        |                                                 |
|-------------------|-------------------------------------------------|
| `left`,`^b`       | go one character to the left |
| `right`,`^f   `   | go one character to the right |
| `up           `   | go one row up, or back in the history |
| `down         `   | go one row down, or forward in the history |
| `^left        `   | go to the start of the previous word |
| `^right       `   | go to the end the current word |
| `home`,`^a    `   | go to the start of the current line |
| `end`,`^e     `   | go to the end of the current line |
| `pgup`,`^home `   | go to the start of the current input |
| `pgdn`,`^end  `   | go to the end of the current input |
| `alt-m        `   | jump to matching brace |
| `^p           `   | go back in the history |
| `^n           `   | go forward in the history |
| `^r`,`^s      `   | search the history starting with the current word |
  

| Deletion        |                                                 |
|-------------------|-------------------------------------------------|
| `del`,`^d     `   | delete the current character |
| `backsp`,`^h  `   | delete the previous character |
| `^w           `   | delete to preceding white space |
| `alt-backsp   `   | delete to the start of the current word |
| `alt-d        `   | delete to the end of the current word |
| `^u           `   | delete to the start of the current line |
| `^k           `   | delete to the end of the current line |
| `esc          `   | delete the current input, or done with empty input |
  

| Editing           |                                                 |
|-------------------|-------------------------------------------------|
| `enter        `   | accept current input |
| `^enter`,`^j`,`shift-tab` | create a new line for multi-line input |
| `^l           `   | clear screen |
| `^t           `   | swap with previous character (move character backward) |
| `^z`,`^_      `   | undo |
| `^y           `   | redo |
| `tab          `   | try to complete the current input |
  

| Completion menu   |                                                 |
|-------------------|-------------------------------------------------|
| `enter`,`left`    | use the currently selected completion |
| `1` - `9`         | use completion N from the menu |
| `tab, down    `   | select the next completion |
| `shift-tab, up`   | select the previous completion |
| `esc          `   | exit menu without completing |
| `pgdn`,`^enter`,`^j`   | show all further possible completions |
  

| Incremental history search        |                                                 |
|-------------------|-------------------------------------------------|
| `enter        `   | use the currently found history entry |
| `backsp`,`^z  `   | go back to the previous match (undo) |
| `tab`,`^r`,`up`   | find the next match |
| `shift-tab`,`^s`,`down`  | find an earlier match |
| `esc          `   | exit search |


# Build the Library

### Build as a Single Source

Copy the sources (in `include` and `src`) into your project, or add the library as a [submodule]:
```
$ git submodule add https://github.com/daanx/isocline
```
and add `isocline/src/isocline.c` to your build rules -- no configuration is needed. 

### Build with CMake

Clone the repository and run cmake to build a static library (`.a`/`.lib`):
```
$ git clone https://github.com/daanx/isocline
$ cd isocline
$ mkdir -p build/release
$ cd build/release
$ cmake ../..
$ cmake --build .
```
This builds a static library `libisocline.a` (or `isocline.lib` on Windows)
and the example program:
```
$ ./example
```

### Build the Haskell Library

See the Haskell [readme][Haskell] for instructions to build the Haskell library.


# C Interface

See [isocline.h](https://github.com/daanx/isocline/blob/main/include/isocline.h) for the full API,
and the [example] for example usage of history, completion, etc.


# Motivation

Isocline was created for use in the [Koka] interactive compiler. 
This required: pure C (no dependency on a C++ runtime or other libraries), 
portable (across Linux, macOS, and Windows), unicode support, 
a BSD-style license, and good functionality for completion and multi-line editing.

Some other excellent libraries that we considered:
[GNU readline],
[editline](https://github.com/troglobit/editline),
[linenoise](https://github.com/antirez/linenoise),
[replxx](https://github.com/AmokHuginnsson/replxx), and 
[Haskeline](https://github.com/judah/haskeline).

[GNU readline]: https://tiswww.case.edu/php/chet/readline/rltop.html
[koka]: http://www.koka-lang.org
[submodule]: https://git-scm.com/book/en/v2/Git-Tools-Submodules
[Haskell]: https://github.com/daanx/isocline/tree/main/haskell
[HaskellExample]: https://github.com/daanx/isocline/blob/main/test/Example.hs
[example]: https://github.com/daanx/isocline/blob/main/test/example.c
[termtosvg]: https://github.com/nbedos/termtosvg

# Internals

## Environment Variables

- `NO_COLOR`: if present no colors are displayed.
- `CLICOLOR=1`: if set, the `LSCOLORS` or `LS_COLORS` environment variables are used to colorize
  filename completions.
- `COLORTERM=`(`truecolor`|`256color`|`16color`|`8color`|`monochrome`): enable a certain color palette, see the next section.
- `TERM`: used on some systems to determine the color

## Colors

Isocline supports 24-bit colors and any RGB colors are automatically
mapped to a reduced palette on older terminals if these do not
support true color. Detection of full color support
is not always possible to do automatically and you can
set the `COLORTERM` environment variable expicitly to force Isocline to use
a specific palette:
- `COLORTERM=truecolor`: use 24-bit colors.  
  <img width="500px" src="doc/color/ansi-truecolor.png"/>
- `COLORTERM=256color`: use the ANSI 256 color palette.  
  <img width="500px" src="doc/color/ansi-256color.png"/>
- `COLORTERM=16color` : use the regular ANSI 16 color 
   palette (8 normal and 8 bright colors).  
   <img width="500px" src="doc/color/ansi-16color.png"/>
- `COLORTERM=8color`: use bold for bright colors.
- `COLORTERM=monochrome`: use no color.

The above screenshots are made with the 
[`test_colors.c`](https://github.com/daanx/isocline/blob/main/test/test_colors.c) program. You can test your own
terminal as:
```
$ gcc -o test_colors -Iinclude test/test_colors.c src/isocline.c
$ ./test_colors
$ COLORTERM=truecolor ./test_colors
$ COLORTERM=16color ./test_colors
```

## ANSI Escape Sequences

Isocline uses just few ANSI escape sequences that are widely
supported:
- `ESC[`_n_`A`, `ESC[`_n_`B`, `ESC[`_n_`C`, and `ESC[`_n_`D`,
  for moving the cursor _n_ places up, down, right, and left.
- `ESC[K` to clear the line from the cursor.
- `ESC[`_n_`m` for colors, with _n_ one of: 0 (reset), 1,22 (bold), 
   4,24 (underline), 7,27 (reverse), 30-37,40-47,90-97,100-107 (color),
   and 39,49 (select default color).
- `ESC[38;5;`_n_`m`, `ESC[48;5;`_n_`m`: on terminals that support it, select 
  entry _n_ from the
  256 color ANSI palette (used with `XTERM=xterm-256color` for example)
  for the foreground or background color.
- `ESC[38;2;`_r_`;`_g_`;`_b_`m`, `ESC[48;2;`_r_`;`_g_`;`_b_`m`: 
  on terminals that support it, select
  any 24-bit RGB color for foreground or background.  
    
On Windows the above functionality is implemented using the Windows console API
(except if running in the new Windows Terminal which supports these escape
sequences natively).

## Async

The best way to use `ic_readline` asynchronously is
to run it in a (blocking) dedicated thread and deliver
results from there to the async event loop. Isocline has the
```C
bool ic_async_stop(void)
```
function that is thread-safe and can deliver an
asynchronous event to Isocline that unblocks a current
`ic_readline` and makes it behave as if the user pressed
`ctrl-c` (which returns NULL from the read line call).


## Possible Future Extensions

- Vi key bindings
- kill buffer 
- ...

Contact me if you are interested in doing any of these :-)


## Color Mapping

To map full RGB colors to an ANSI 256 or 16-color palette
Isoclines pick a color with a minimal distance. There are various
ways of calculating this: one way is to take the euclidean distance
in the sRGB space (_simple-rgb_), a slightly better way is to 
take a weighted distance that takes the human perception of each
color component into account (_delta-rgb_, this is used by Iscoclines),
and finally, we can first translate into the perceptual color space
CIElab and calculate the distance there using the CIEDE2000 
algorithm (_ciede2000_). Here are these three methods compared on
some colors: 

![color space comparison](doc/color/colorspace-map.png)

Each top row is the true 24-bit RGB color. Surprisingly,
the sophisticated CIEDE2000 seems not so good compared to the 
simpler methods.
This might be because this algorithm was created to find close
perceptual colors in images where lightness differences may be given
less weight. The CIEDE2000 also leads to more "outliers", for example
in column 5. Given these results Isoclines uses _delta-rgb_ for
color mapping. It adds to this a gray correction that makes it less
likely to substitute a color for a gray value (and the other way
around).