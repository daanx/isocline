/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>  

#include "common.h"
#include "term.h"
#include "bbcode.h" 


static const term_attr_t term_attr_none = { IC_COLOR_NONE, IC_COLOR_NONE, 0, 0, 0, 0 };


ic_private void attr_update_with( term_attr_t* attr, term_attr_t newattr ) {
  if (newattr.color != IC_COLOR_NONE) { attr->color = newattr.color; }
  if (newattr.bgcolor != IC_COLOR_NONE) { attr->bgcolor = newattr.bgcolor; }
  if (newattr.bold != IC_NONE) { attr->bold = newattr.bold; }
  if (newattr.italic != IC_NONE) { attr->italic = newattr.italic; }
  if (newattr.reverse != IC_NONE) { attr->reverse = newattr.reverse; }
  if (newattr.underline != IC_NONE) { attr->underline = newattr.underline; }
}

ic_private void attr_to_prev( term_attr_t* attr, term_attr_t prev ) {
  if (attr->color != IC_COLOR_NONE) { attr->color = prev.color; }
  if (attr->bgcolor != IC_COLOR_NONE) { attr->bgcolor = prev.bgcolor; }
  if (attr->bold != IC_NONE) { attr->bold = prev.bold; }
  if (attr->italic != IC_NONE) { attr->italic = prev.italic; }
  if (attr->reverse != IC_NONE) { attr->reverse = prev.reverse; }
  if (attr->underline != IC_NONE) { attr->underline = prev.underline; }
}

ic_private void attr_negate( term_attr_t* attr ) {
  if (attr->color != IC_COLOR_NONE) { attr->color = IC_COLOR_NONE; }
  if (attr->bgcolor != IC_COLOR_NONE) { attr->bgcolor = IC_COLOR_NONE; }
  if (attr->bold != IC_NONE) { attr->bold = -attr->bold; }
  if (attr->italic != IC_NONE) { attr->italic = -attr->italic; }
  if (attr->reverse != IC_NONE) { attr->reverse = -attr->reverse; }
  if (attr->underline != IC_NONE) { attr->underline = -attr->underline; }
}


typedef struct style_s {
  const char*  name;
  term_attr_t  attr;
} style_t;

struct bbcode_s {
  term_attr_t* attrs;
  ssize_t      attrs_capacity;
  ssize_t      attrs_nesting;  
  style_t*     styles;
  ssize_t      styles_capacity;
  ssize_t      styles_count;
  term_t*      term;
  alloc_t*     mem;
};

ic_private bbcode_t* bbcode_new( alloc_t* mem, term_t* term ) {
  bbcode_t* bb = mem_zalloc_tp(mem,bbcode_t);
  if (bb==NULL) return NULL;
  bb->mem = mem;
  bb->term = term;
  return bb;
}

ic_private void bbcode_free( bbcode_t* bb ) {
  for(ssize_t i = 0; i < bb->styles_count; i++) {
    mem_free(bb->mem, bb->styles[i].name);
  }
  mem_free(bb->mem, bb->attrs);
  mem_free(bb->mem, bb->styles);
  mem_free(bb->mem, bb);
}

ic_private void bbcode_add_style( bbcode_t* bb, const char* style_name, term_attr_t attr ) {
  if (bb->styles_count >= bb->styles_capacity) {
    ssize_t newlen = bb->styles_capacity + 32;
    style_t* p = mem_realloc_tp( bb->mem, style_t, bb->styles, newlen );
    if (p == NULL) return;
    bb->styles = p;
    bb->styles_capacity = newlen;
  }
  assert(bb->styles_count < bb->styles_capacity);
  bb->styles[bb->styles_count].name = mem_strdup( bb->mem, style_name );
  bb->styles[bb->styles_count].attr = attr;
  bb->styles_count++;
}

ic_private void bbcode_attr_push( bbcode_t* bb, term_attr_t attr ) {
  if (bb->attrs_nesting >= bb->attrs_capacity) {
    ssize_t newcap = bb->attrs_capacity + 32;
    term_attr_t* p = mem_realloc_tp( bb->mem, term_attr_t, bb->attrs, newcap );
    if (p == NULL) return;
    bb->attrs = p;
    bb->attrs_capacity = newcap;    
  }
  assert(bb->attrs_nesting < bb->attrs_capacity);
  bb->attrs[bb->attrs_nesting] = attr;
  bb->attrs_nesting++;
}

ic_private term_attr_t bbcode_attr_pop( bbcode_t* bb ) {
  if (bb->attrs_nesting <= 0) {
    return term_attr_none;
  }
  else {
    bb->attrs_nesting--;
    return bb->attrs[bb->attrs_nesting];
  }
}

ic_private void bbcode_set( bbcode_t* bb, term_attr_t attr ) {
  term_set_attr(bb->term,attr);
}

ic_private void bbcode_unset( bbcode_t* bb, term_attr_t attr ) {
  attr_negate(&attr);
  term_set_attr(bb->term,attr);
}

ic_private void bbcode_open( bbcode_t* bb, term_attr_t attr ) {  
  bbcode_attr_push(bb,term_get_attr(bb->term));
  bbcode_set(bb,attr);
}

ic_private void bbcode_close( bbcode_t* bb, term_attr_t attr ) {
  term_attr_t prev = bbcode_attr_pop(bb);
  attr_to_prev(&attr,prev);
  bbcode_set(bb,attr);
}

ic_private void attr_update_bool( int8_t* field, const char* value ) {
  if (value == NULL || value[0] == 0 || strcmp(value,"on") || strcmp(value,"true") || strcmp(value,"1")) {
    *field = IC_ON;
  }
  else if (strcmp(value,"off") || strcmp(value,"false") || strcmp(value,"0")) {
    *field = IC_OFF;
  }
  else {
    debug_msg("bbcode: invalid value: %s\n", value );
  }
}

ic_private void attr_update_color( ic_color_t* field, const char* value ) {
  if (value == NULL || value[0] == 0 || strcmp(value,"none")) {
    *field = IC_COLOR_NONE;
  }
  else if (strcmp(value,"default") == 0) {
    *field = IC_ANSI_DEFAULT;
  }
  else if (strcmp(value,"ansi-red") == 0) {
    *field = IC_ANSI_RED;
  }
  else if (strcmp(value,"red") == 0) {
    *field = IC_RGB(0xFF0000);
  }
  else {
    *field = IC_COLOR_NONE;
  }
}

ic_private bool attr_update_property( term_attr_t* attr, const char* attr_name, const char* value ) {
  if (strcmp(attr_name,"bold") == 0) {
    attr_update_bool(&attr->bold, value);
  }
  else if (strcmp(attr_name,"italic") == 0) {
    attr_update_bool(&attr->italic, value);
  }
  else if (strcmp(attr_name,"underline") == 0) {
    attr_update_bool(&attr->underline, value);
  }
  else if (strcmp(attr_name,"reverse") == 0) {
    attr_update_bool(&attr->reverse, value);
  }
  else if (strcmp(attr_name,"color") == 0) {
    attr_update_color(&attr->color, value);
  }
  else if (strcmp(attr_name,"bgcolor") == 0) {
    attr_update_color(&attr->bgcolor, value);
  }
  else {
    return false;
  }
  return true;
}

static const style_t builtin_styles[] = {
  { "b", { IC_COLOR_NONE, IC_COLOR_NONE, 1, 0, 0, 0 } },
  { "r", { IC_COLOR_NONE, IC_COLOR_NONE, 0, 1, 0, 0 } },
  { "u", { IC_COLOR_NONE, IC_COLOR_NONE, 0, 0, 1, 0 } },
  { "i", { IC_COLOR_NONE, IC_COLOR_NONE, 0, 0, 0, 1 } },
  { "ansi-red", { IC_ANSI_RED, IC_COLOR_NONE, 0, 0, 0, 0 } },
  { "red",      { IC_RED,      IC_COLOR_NONE, 0, 0, 0, 0 } },
  { NULL, { IC_COLOR_NONE, IC_COLOR_NONE, 1, 0, 0, 0 } }
};

ic_private void attr_update_with_styles( term_attr_t* attr, const char* attr_name, const char* value, 
                                            const style_t* styles, ssize_t count ) 
{
  // first try if it is a builtin property
  if (attr_update_property(attr,attr_name,value)) return;
  // then check all styles
  while( count-- > 0 ) {
    const style_t* style = styles + count;
    if (strcmp(style->name,attr_name) == 0) {
      attr_update_with(attr,style->attr);
      return;
    }    
  }
  // check builtin styles
  const style_t* style = builtin_styles;
  while( style->name != NULL ) {
    if (strcmp(style->name,attr_name) == 0) {
      attr_update_with(attr,style->attr);
      return;
    }
    style++;
  }
}

ic_private const char* parse_skip_white(const char* s) {
  while( *s != 0 && *s != ']') {
    if (!(*s == ' ' || *s == '\t' || *s == '\n' || *s == '\r')) break;
    s++;
  }
  return s;
}

ic_private const char* parse_skip_to_white(const char* s) {
  while( *s != 0 && *s != ']') {  
    if (*s == ' ' || *s == '\t' || *s == '\n' || *s == '\r') break;
    s++;
  }
  return parse_skip_white(s);
}

ic_private const char* parse_skip_to_end(const char* s) {
  while( *s != 0 && *s != ']' ) { s++; }    
  return s;
}

ic_private const char* parse_attr_name(const char* s) {
  while( *s != 0 && *s != ']') {
    if (!((*s >= 'a' && *s <= 'z') || (*s >= 'A' && *s <= 'Z') || (*s >= '0' && *s <= '9') || *s == '_' || *s == '-')) break;
    s++;
  }
  return s;
}

ic_private const char* parse_value(const char* s, const char** start, const char** end) {
  if (*s == '"') {
    s++;
    *start = s;
    while( *s != 0 ) {
      if (*s == '"') break;
      s++;
    }
    *end = s-1;      
    if (*s == '"') { s++; }
  }
  else if (*s == '#') {
    s++;
    *start = s;
    while( *s != 0 ) {
      if (!((*s >= 'a' && *s <= 'f') || (*s >= 'A' && *s <= 'Z') || (*s >= '0' && *s <= '9'))) break;
      s++;
    }
    *end = s;
  }
  else {
    *start = s;
    while( *s != 0 ) {
      if (!((*s >= 'a' && *s <= 'z') || (*s >= 'A' && *s <= 'F') || (*s >= '0' && *s <= '9') || *s == '-' || *s == '_')) break;
      s++;
    }
    *end = s;
  }  
  return s;  
}

ic_private const char* parse_attr( term_attr_t* attr, const char* s, const style_t* styles, ssize_t scount ) {
  // parse: \s*[\w-]+\s*(=\s*<value>)
  const char* id = s;
  const char* idend = parse_attr_name(id);
  if (id == idend) return parse_skip_to_white(id);
  s = parse_skip_white(idend);
  const char* val = NULL;
  const char* valend = NULL;
  if (*s == '=') {
    s++;
    s = parse_skip_white(s);
    s = parse_value(s, &val, &valend);
  }
  s = parse_skip_to_white(s); // ignore postfix
  
  // limit name and attr to 128 bytes
  char idbuf[128];
  char valbuf[128];
  ic_strncpy( idbuf, 128, id, idend - id);
  ic_strncpy( valbuf, 128, val, valend - val);
  ic_str_tolower(idbuf);
  ic_str_tolower(valbuf);
  attr_update_with_styles( attr, idbuf, valbuf, styles, scount );  
  return s;
}

static const char* parse_attrs( term_attr_t* attr, const char* s, const style_t* styles, ssize_t scount ) {
  s = parse_skip_white(s);
  while( *s != 0 && *s != ']') {
    s = parse_attr(attr, s, styles, scount);
  }
  if (*s == ']') { s++; }
  return s;
}


static const char* parse_bbcode( term_attr_t* attr, bool* open, const char* s, const style_t* styles, ssize_t scount ) {
  *open = true;
  if (*s != '[') return s;
  s++;
  s = parse_skip_white(s);
  if (*s == '/') { *open = false; s++; };
  s = parse_attrs( attr, s, styles, scount);
  return s;
}

ic_private void bbcode_parse_style( bbcode_t* bb, const char* style_name, const char* s ) {
  term_attr_t attr = term_attr_none;
  s = parse_skip_white(s);
  parse_attrs( &attr, s, bb->styles, bb->styles_count);
  bbcode_add_style(bb, style_name, attr);
}

ic_private ssize_t bbcode_parse( bbcode_t* bb, const char* s ) {
  assert(*s == '[');
  term_attr_t attr = term_attr_none;
  bool open = true;
  const char* end = parse_bbcode( &attr, &open, s, bb->styles, bb->styles_count ); // todo: styles
  assert(end > s);
  if (open) {
    bbcode_open( bb, attr );
  }
  else {
    bbcode_close( bb, attr );
  }
  return (end - s);
}

ic_private void bbcode_print( bbcode_t* bb, const char* s ) {
  bbcode_attr_push(bb,term_get_attr(bb->term));
  const ssize_t nesting0 = bb->attrs_nesting;
  ssize_t i = 0;
  while( s[i] != 0 ) {
    ssize_t nobb = 0;
    while( s[i+nobb] != 0 && s[i+nobb] != '[') {
      nobb++;
    }
    if (nobb > 0) { term_write_n(bb->term, s+i, nobb); }
    i += nobb;
    if (s[i] == '[') {
      i += bbcode_parse(bb, s+i);
    }
  }
  // restore unclosed openings
  if (bb->attrs_nesting >= nesting0) {
    bb->attrs_nesting = nesting0;
    term_set_attr(bb->term, bbcode_attr_pop(bb));
  }
}

