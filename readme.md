# Repline: a portable readline alternative.

Repline is a pure C library that can be used as readline alternative. 

- small: less than 3k lines and can be compiled as a single C file without 
  any dependencies or configuration (e.g. `gcc -c src/repline.c`).
- portable: works on Unix, Windows, and macOS, etc, and relies on a minimal
  subset of ANSI escape sequences.
- features: extensive multi-line editing mode, colors, history, completion, unicode, 
  graceful fallback, etc.
- license: MIT.

Enjoy,
  Daan


## Motivation

Repline was created for use in the [Koka] interative compiler. 
This required: pure C (no dependecy on a C++ runtime), 
portable (across Linux, macOS, and Windows), unicode support, 
a BSD-style license, and good functionality for completion and multi-line editing. 

Some other libraries that we considered:

- [GNU readline](https://tiswww.case.edu/php/chet/readline/rltop.html), the classic.
- [editline](https://github.com/troglobit/editline).
- [linenoise](https://github.com/antirez/linenoise).
- [replxx](https://github.com/AmokHuginnsson/replxx).

