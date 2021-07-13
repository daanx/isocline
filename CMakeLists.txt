# -----------------------------------------------------------------------------
# Copyright (c) 2021, Daan Leijen
# -----------------------------------------------------------------------------
cmake_minimum_required(VERSION 3.12)
project(libmprompt C CXX ASM)

set(CMAKE_C_STANDARD 99)
set(CMAKE_CXX_STANDARD 11)

option(RL_USE_CXX           "Build with C++ compiler" OFF)
option(RL_DEBUG_UBSAN       "Build with undefined behaviour sanitizer" OFF)
option(RL_DEBUG_ASAN        "Build with address sanitizer" OFF)
option(RL_DEBUG_MSG         "Output debug messages to repline.debug.txt (warning: do not enable in release)" OFF)
option(RL_SEPARATE_OBJS     "Compile with separate object files instead of one (warning: exports internal symbols)" OFF)

set(rl_version "0.6")


set(rl_sources          src/repline.c)    
set(rl_example_sources  test/example.c)
set(rl_cflags)
set(rl_install_dir)

if(RL_SEPARATE_OBJS)
list(APPEND rl_sources  
    src/completions.c
    src/editline.c
    src/history.c
    src/term.c
    src/tty.c
    src/wcwidth.c)
endif()

if(RL_USE_CXX)
  set(RL_COMPILER_ID "${CMAKE_CXX_COMPILER_ID}")
  set_source_files_properties(${rl_sources} PROPERTIES LANGUAGE CXX )
  set_source_files_properties(${rl_example_sources} PROPERTIES LANGUAGE CXX )
else()
  set(RL_COMPILER_ID "${CMAKE_C_COMPILER_ID}")  
endif()

# -----------------------------------------------------------------------------
# Convenience: set default build type depending on the build directory
# -----------------------------------------------------------------------------

if (NOT CMAKE_BUILD_TYPE)
  if ("${CMAKE_BINARY_DIR}" MATCHES ".*(Debug|debug|ubsan|tsan|asan)$")
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

if(RL_DEBUG_UBSAN OR RL_DEBUG_ASAN)
  if((CMAKE_BUILD_TYPE MATCHES "Debug") AND (RL_COMPILER_ID MATCHES "Clang"))
    set(rl_san)
    if (RL_DEBUG_UBSAN)
      list(APPEND rl_san "undefined")
    endif()
    if (RL_DEBUG_ASAN)
      list(APPEND rl_san "address")
    endif()
    list(JOIN rl_san "," rl_san)
    list(APPEND rl_cflags -fsanitize=${rl_san})
    list(APPEND CMAKE_EXE_LINKER_FLAGS -fsanitize=${rl_san})
  else()
    message(WARNING "Can only use sanitizer with a clang debug build (currently: ${RL_COMPILER_ID}, CMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}, RL_USE_C=${RL_USE_C})")
  endif()
endif()


# -----------------------------------------------------------------------------
# Flags
# -----------------------------------------------------------------------------

if (RL_COMPILER_ID MATCHES "AppleClang|Clang|GNU|Intel")
  list(APPEND rl_cflags -Wall -Wextra -Wpedantic -Wno-unknown-pragmas -Wno-unused-function -Wno-padded)
  if (RL_COMPILER_ID MATCHES "AppleClang|Clang")
    list(APPEND rl_cflags -Wimplicit-int-conversion -Wsign-conversion)
  endif()
  if (RL_COMPILER_ID MATCHES "GNU")
    list(APPEND rl_cflags -Wint-conversion -Wsign-conversion)
  endif()
endif()

# treat C extension as C++
if (RL_USE_CXX)
  if(CMAKE_CXX_COMPILER_ID MATCHES "AppleClang|Clang")
    list(APPEND rl_cflags -Wno-deprecated)
  endif()
  if(CMAKE_CXX_COMPILER_ID MATCHES "Intel")
    list(APPEND rl_cflags -Kc++)
  endif()
endif()



# -----------------------------------------------------------------------------
# Overview
# -----------------------------------------------------------------------------

message(STATUS "")
message(STATUS   "Library   : librepline")
message(STATUS   "Build type: ${CMAKE_BUILD_TYPE}")
if(RL_USE_CXX)
  message(STATUS "Compiler  : ${CMAKE_CXX_COMPILER}")
else()
  message(STATUS "Compiler  : ${CMAKE_C_COMPILER}")
endif()
message(STATUS   "            ${rl_cflags}")
message(STATUS "")


# -----------------------------------------------------------------------------
# Library and samples
# -----------------------------------------------------------------------------

add_library(repline STATIC ${rl_sources})
set_property(TARGET repline PROPERTY POSITION_INDEPENDENT_CODE ON)
target_compile_options(repline PRIVATE ${rl_cflags})
target_include_directories(repline PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:${rl_install_dir}/include>
)
if (RL_SEPARATE_OBJS)
  target_compile_definitions(repline PRIVATE RL_SEPARATE_OBJS)
endif()
if (RL_DEBUG_MSG)
  target_compile_definitions(repline PRIVATE RL_DEBUG_MSG)
endif()  

add_executable(example ${rl_example_sources})
target_compile_options(example PRIVATE ${rl_cflags})
target_include_directories(example PRIVATE include)
target_link_libraries(example PRIVATE repline)
