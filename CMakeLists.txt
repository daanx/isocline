# -----------------------------------------------------------------------------
# Copyright (c) 2021, Daan Leijen
# -----------------------------------------------------------------------------
cmake_minimum_required(VERSION 3.10)
project(librepline C CXX ASM)

set(CMAKE_C_STANDARD   99)
set(CMAKE_CXX_STANDARD 11)

option(RP_USE_CXX           "Build with C++ compiler" OFF)
option(RP_DEBUG_UBSAN       "Build with undefined behaviour sanitizer" OFF)
option(RP_DEBUG_ASAN        "Build with address sanitizer" OFF)
option(RP_DEBUG_MSG         "Output debug messages to repline.debug.txt (warning: do not enable in release)" OFF)
option(RP_SEPARATE_OBJS     "Compile with separate object files instead of one (warning: exports internal symbols)" OFF)

set(rp_version "0.1")
set(rp_sources          src/repline.c)    
set(rp_example_sources  test/example.c)

# -----------------------------------------------------------------------------
# Initial definitions
# -----------------------------------------------------------------------------
set(rp_cflags)
set(rp_cdefs)
set(rp_install_dir)

if(RP_SEPARATE_OBJS)
  list(APPEND rp_cdefs RP_SEPARATE_OBJS)
  list(APPEND rp_sources  
              src/completions.c
              src/editline.c
              src/history.c
              src/term.c
              src/tty.c
              src/wcwidth.c)
endif()

if(RP_USE_CXX)
  set(RP_COMPILER_ID "${CMAKE_CXX_COMPILER_ID}")
  set_source_files_properties(${rp_sources}         PROPERTIES LANGUAGE CXX )
  set_source_files_properties(${rp_example_sources} PROPERTIES LANGUAGE CXX )
else()
  set(RP_COMPILER_ID "${CMAKE_C_COMPILER_ID}")  
endif()

if (RP_DEBUG_MSG)
  message(STATUS "Enabling debug messages -- written to 'repline.debug.txt'")
  list(APPEND rp_cdefs RP_DEBUG_MSG)
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

if(RP_DEBUG_UBSAN OR RP_DEBUG_ASAN)
  if((CMAKE_BUILD_TYPE MATCHES "Debug") AND (RP_COMPILER_ID MATCHES "Clang"))
    set(rp_san)
    if (RP_DEBUG_UBSAN)
      list(APPEND rp_san "undefined")
      message(STATUS "Using the undefined behavior sanitizer.")
    endif()
    if (RP_DEBUG_ASAN)
      list(APPEND rp_san "address")
      message(STATUS "Using the address sanitizer. To detect memory leaks run as:") 
      message(STATUS "> ASAN_OPTIONS=\"detect_leaks=1\" ./example")
    endif()
    list(JOIN rp_san "," rp_san)
    list(APPEND rp_cflags -fsanitize=${rp_san})
    list(APPEND CMAKE_EXE_LINKER_FLAGS -fsanitize=${rp_san})    
  else()
    message(WARNING "Can only use sanitizer with a clang debug build (currently: ${RP_COMPILER_ID}, CMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}, RP_USE_C=${RP_USE_C})")
  endif()
endif()


# -----------------------------------------------------------------------------
# Flags
# -----------------------------------------------------------------------------

if (RP_COMPILER_ID MATCHES "AppleClang|Clang|GNU|Intel")
  list(APPEND rp_cflags -Wall -Wextra -Wpedantic -Wno-unknown-pragmas -Wno-unused-function -Wno-padded)
  if (RP_COMPILER_ID MATCHES "AppleClang|Clang")
    list(APPEND rp_cflags -Wimplicit-int-conversion -Wsign-conversion)
  endif()
  if (RP_COMPILER_ID MATCHES "GNU")
    list(APPEND rp_cflags -Wint-conversion -Wsign-conversion)
  endif()
endif()

# treat C extension as C++
if (RP_USE_CXX)
  if(CMAKE_CXX_COMPILER_ID MATCHES "AppleClang|Clang")
    list(APPEND rp_cflags -Wno-deprecated)
  endif()
  if(CMAKE_CXX_COMPILER_ID MATCHES "Intel")
    list(APPEND rp_cflags -Kc++)
  endif()
endif()



# -----------------------------------------------------------------------------
# Overview
# -----------------------------------------------------------------------------

message(STATUS "")
message(STATUS "Library   : librepline")
message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "Compiler  : ${RP_COMPILER_ID}")
message(STATUS "Defines   : ${rp_cdefs}")
# message(STATUS "Flags     : ${rp_cflags}")
message(STATUS "")


# -----------------------------------------------------------------------------
# Static library (librepline.a) and samples (example)
# -----------------------------------------------------------------------------

add_library(repline STATIC ${rp_sources})
set_property(TARGET repline PROPERTY POSITION_INDEPENDENT_CODE ON)
target_compile_options(repline PRIVATE ${rp_cflags})
target_compile_definitions(repline PRIVATE ${rp_cdefs})
target_include_directories(repline PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:${rp_install_dir}/include>
)

add_executable(example ${rp_example_sources})
target_compile_options(example PRIVATE ${rp_cflags})
target_include_directories(example PRIVATE include)
target_link_libraries(example PRIVATE repline)
