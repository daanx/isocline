# -----------------------------------------------------------------------------
# Copyright (c) 2021, Daan Leijen
# -----------------------------------------------------------------------------
cmake_minimum_required(VERSION 3.10)
project(libisocline C CXX ASM)

set(CMAKE_C_STANDARD   99)
set(CMAKE_CXX_STANDARD 11)

option(IC_USE_CXX           "Build with C++ compiler" OFF)
option(IC_DEBUG_UBSAN       "Build with undefined behaviour sanitizer" OFF)
option(IC_DEBUG_ASAN        "Build with address sanitizer" OFF)
option(IC_DEBUG_MSG         "Enable printing debug messages stderr (only if also ISOCLINE_DEBUG=1 is set in the environment)" ON)
option(IC_SEPARATE_OBJS     "Compile with separate object files instead of one (warning: exports internal symbols)" OFF)

set(ic_version "0.1")
set(ic_sources          src/isocline.c)    
set(ic_example_sources  test/example.c test/test_colors.c)

# -----------------------------------------------------------------------------
# Initial definitions
# -----------------------------------------------------------------------------
set(ic_cflags)
set(ic_cdefs)
set(ic_install_dir)

if(IC_SEPARATE_OBJS)
  list(APPEND ic_cdefs IC_SEPARATE_OBJS)
  list(APPEND ic_sources  
              src/attr.c
              src/bbcode.c
              src/common.c
              src/completions.c
              src/completers.c
              src/editline.c
              src/highlight.c
              src/history.c
              src/stringbuf.c
              src/term.c
              src/tty_esc.c
              src/tty.c
              src/undo.c)
endif()

if(IC_USE_CXX)
  set(IC_COMPILER_ID "${CMAKE_CXX_COMPILER_ID}")
  set_source_files_properties(${ic_sources}         PROPERTIES LANGUAGE CXX )
  set_source_files_properties(${ic_example_sources} PROPERTIES LANGUAGE CXX )
else()
  set(IC_COMPILER_ID "${CMAKE_C_COMPILER_ID}")  
endif()

if(NOT IC_DEBUG_MSG)
  message(STATUS "Disable debug messages")
  list(APPEND ic_cdefs IC_NO_DEBUG_MSG)
endif()  


# -----------------------------------------------------------------------------
# Convenience: set default build type depending on the build directory
# -----------------------------------------------------------------------------

if (NOT CMAKE_BUILD_TYPE)
  if ("${CMAKE_BINARY_DIR}" MATCHES ".*(Debug|debug|dbg|ubsan|tsan|asan)$")
    message(STATUS "No build type selected, default to: Debug")
    set(CMAKE_BUILD_TYPE "Debug")
  else()
    message(STATUS "No build type selected, default to: Release")
    set(CMAKE_BUILD_TYPE "Release")
  endif()
endif()


# -----------------------------------------------------------------------------
# Sanitizers
# -----------------------------------------------------------------------------

if(IC_DEBUG_UBSAN OR IC_DEBUG_ASAN)
  if((CMAKE_BUILD_TYPE MATCHES "Debug") AND (IC_COMPILER_ID MATCHES "Clang"))
    set(ic_san)
    if (IC_DEBUG_UBSAN)
      list(APPEND ic_san "undefined")
      message(STATUS "Using the undefined behavior sanitizer.")
    endif()
    if (IC_DEBUG_ASAN)
      list(APPEND ic_san "address")
      message(STATUS "Using the address sanitizer. To detect memory leaks run as:") 
      message(STATUS "> ASAN_OPTIONS=\"detect_leaks=1:verbosity=1\" ./example")
    endif()
    list(JOIN ic_san "," ic_san)
    list(APPEND ic_cflags -fsanitize=${ic_san})
    list(APPEND CMAKE_EXE_LINKER_FLAGS -fsanitize=${ic_san})    
  else()
    message(WARNING "Can only use sanitizer with a clang debug build (currently: ${IC_COMPILER_ID}, CMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}, IC_USE_C=${IC_USE_C})")
  endif()
endif()


# -----------------------------------------------------------------------------
# Flags
# -----------------------------------------------------------------------------

if (IC_COMPILER_ID MATCHES "AppleClang|Clang|GNU|Intel")
  list(APPEND ic_cflags -Wall -Wextra -Wpedantic -Wno-unknown-pragmas -Wno-unused-function -Wno-padded -Wno-missing-field-initializers)
  if (IC_COMPILER_ID MATCHES "AppleClang|Clang")
    list(APPEND ic_cflags -Wimplicit-int-conversion -Wsign-conversion)
  endif()
  if (IC_COMPILER_ID MATCHES "GNU")
    list(APPEND ic_cflags -Wsign-conversion -Wno-missing-braces)
    if (NOT IC_USE_CXX)
      list(APPEND ic_cflags -Wint-conversion)
    endif()
  endif()
endif()

# treat C extension as C++
if (IC_USE_CXX)
  if(CMAKE_CXX_COMPILER_ID MATCHES "AppleClang|Clang")
    list(APPEND ic_cflags -Wno-deprecated)
  endif()
  if(CMAKE_CXX_COMPILER_ID MATCHES "Intel")
    list(APPEND ic_cflags -Kc++)
  endif()
endif()



# -----------------------------------------------------------------------------
# Overview
# -----------------------------------------------------------------------------

message(STATUS "")
message(STATUS "Library   : libisocline")
message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "Compiler  : ${IC_COMPILER_ID}")
message(STATUS "Defines   : ${ic_cdefs}")
# message(STATUS "Flags     : ${ic_cflags}")
message(STATUS "")


# -----------------------------------------------------------------------------
# Static library (libisocline.a) and samples (example)
# -----------------------------------------------------------------------------

add_library(isocline STATIC ${ic_sources})
set_property(TARGET isocline PROPERTY POSITION_INDEPENDENT_CODE ON)
target_compile_options(isocline PRIVATE ${ic_cflags})
target_compile_definitions(isocline PRIVATE ${ic_cdefs})
target_include_directories(isocline PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:${ic_install_dir}/include>
)

add_executable(example test/example.c)
target_compile_options(example PRIVATE ${ic_cflags})
target_include_directories(example PRIVATE include)
target_link_libraries(example PRIVATE isocline)

add_executable(test_colors test/test_colors.c)
target_compile_options(test_colors PRIVATE ${ic_cflags})
target_include_directories(test_colors PRIVATE include)
target_link_libraries(test_colors PRIVATE isocline)
