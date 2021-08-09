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

#include "bbcode_colors.c"

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

typedef struct tag_s {
  const char*  name;
  term_attr_t  attr;
} tag_t;


static void tag_init(tag_t* tag) {
  memset(tag,0,sizeof(*tag));  
}

struct bbcode_s {
  tag_t*       tags;
  ssize_t      tags_capacity;
  ssize_t      tags_nesting;  
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
  mem_free(bb->mem, bb->tags);
  mem_free(bb->mem, bb->styles);
  mem_free(bb->mem, bb);
}

static void bbcode_add_style( bbcode_t* bb, const char* style_name, term_attr_t attr ) {
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

static ssize_t bbcode_tag_push( bbcode_t* bb, const tag_t* tag ) {
  if (bb->tags_nesting >= bb->tags_capacity) {
    ssize_t newcap = bb->tags_capacity + 32;
    tag_t* p = mem_realloc_tp( bb->mem, tag_t, bb->tags, newcap );
    if (p == NULL) return -1;
    bb->tags = p;
    bb->tags_capacity = newcap;    
  }
  assert(bb->tags_nesting < bb->tags_capacity);
  bb->tags[bb->tags_nesting] = *tag;
  bb->tags_nesting++;
  return (bb->tags_nesting-1);
}

static void bbcode_tag_pop( bbcode_t* bb, tag_t* tag ) {
  if (bb->tags_nesting <= 0) {
    if (tag != NULL) { tag_init(tag); }
  }
  else {
    bb->tags_nesting--;
    if (tag != NULL) {
      *tag = bb->tags[bb->tags_nesting];
    }    
  }
}

ic_private void bbcode_set( bbcode_t* bb, term_attr_t attr ) {
  term_set_attr(bb->term,attr);
}

ic_private void bbcode_unset( bbcode_t* bb, term_attr_t attr ) {
  attr_negate(&attr);
  term_set_attr(bb->term,attr);
}

static void bbcode_open( bbcode_t* bb, const tag_t* tag ) { 
  // save current and set
  tag_t cur;
  tag_init(&cur);
  cur.name = tag->name;
  cur.attr = term_get_attr(bb->term);
  bbcode_tag_push(bb,&cur);
  bbcode_set(bb,tag->attr);
}

static void bbcode_close( bbcode_t* bb, const char* name ) {
  // pop until match
  while (bb->tags_nesting > 0) {
    tag_t prev;
    bbcode_tag_pop(bb,&prev);
    if (name==NULL || prev.name==NULL || ic_stricmp(prev.name,name) == 0) {
      // matched
      bbcode_set(bb,prev.attr);
      break;
    }
    else {
      // unbalanced, continue
      debug_msg("bbcode: unbalanced tags: open [%s], close [/%s]\n", prev.name, name);      
    }
  }  
}

static void attr_update_bool( int8_t* field, const char* value ) {
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

static void attr_update_color( ic_color_t* field, const char* value ) {
  if (value == NULL || value[0] == 0 || strcmp(value,"none") == 0) {
    *field = IC_COLOR_NONE;
    return;
  }
  // search color names
  ssize_t lo = 0;
  ssize_t hi = IC_HTML_COLOR_COUNT-1;
  while( lo <= hi ) {
    ssize_t mid = (lo + hi) / 2;
    style_color_t* info = &html_colors[mid];
    int cmp = strcmp(info->name,value);
    if (cmp < 0) {
      lo = mid+1;
    }
    else if (cmp > 0) {
      hi = mid-1;
    }
    else { 
      *field = info->color;
      return;
    }    
  }
  *field = IC_COLOR_NONE;
}

static const char* attr_update_property( term_attr_t* attr, const char* attr_name, const char* value ) {
  if (strcmp(attr_name,"bold") == 0) {
    attr_update_bool(&attr->bold, value);
    return "bold";
  }
  else if (strcmp(attr_name,"italic") == 0) {
    attr_update_bool(&attr->italic, value);
    return "italic";
  }
  else if (strcmp(attr_name,"underline") == 0) {
    attr_update_bool(&attr->underline, value);
    return "underline";
  }
  else if (strcmp(attr_name,"reverse") == 0) {
    attr_update_bool(&attr->reverse, value);
    return "reverse";
  }
  else if (strcmp(attr_name,"color") == 0) {
    attr_update_color(&attr->color, value);
    return "color";
  }
  else if (strcmp(attr_name,"bgcolor") == 0) {
    attr_update_color(&attr->bgcolor, value);
    return "bgcolor";
  }
  else {
    return NULL;
  }
}

static const style_t builtin_styles[] = {
  { "b", { IC_COLOR_NONE, IC_COLOR_NONE, IC_ON  , IC_NONE, IC_NONE, IC_NONE } },
  { "r", { IC_COLOR_NONE, IC_COLOR_NONE, IC_NONE, IC_ON  , IC_NONE, IC_NONE } },
  { "u", { IC_COLOR_NONE, IC_COLOR_NONE, IC_NONE, IC_NONE, IC_ON  , IC_NONE } },
  { "i", { IC_COLOR_NONE, IC_COLOR_NONE, IC_NONE, IC_NONE, IC_NONE, IC_ON   } },
  { NULL, { IC_COLOR_NONE, IC_COLOR_NONE, IC_NONE, IC_NONE, IC_NONE, IC_NONE } }
};

static const char* attr_update_with_styles( term_attr_t* attr, const char* attr_name, const char* value, 
                                            const style_t* styles, ssize_t count ) 
{
  // first try if it is a builtin property
  const char* name;
  if ((name = attr_update_property(attr,attr_name,value)) != NULL) {
    return name;
  }
  // then check all styles
  while( count-- > 0 ) {
    const style_t* style = styles + count;
    if (strcmp(style->name,attr_name) == 0) {
      attr_update_with(attr,style->attr);
      return style->name;
    }    
  }
  // check builtin styles; todo: binary search?
  for( const style_t* style = builtin_styles; style->name != NULL; style++) {
    if (strcmp(style->name,attr_name) == 0) {
      attr_update_with(attr,style->attr);
      return style->name;
    }
  }
  // check colors as a style
  ssize_t lo = 0;
  ssize_t hi = IC_HTML_COLOR_COUNT-1;
  while( lo <= hi ) {
    ssize_t mid = (lo + hi) / 2;
    style_color_t* info = &html_colors[mid];
    int cmp = strcmp(info->name,attr_name);
    if (strcmp(attr_name,"lavender") == 0) {
      debug_msg("bbcode: lavender: %zd - %zd - %zd, %s\n", lo, mid, hi, info->name);
    }
    if (cmp < 0) {
      lo = mid+1;
    }
    else if (cmp > 0) {
      hi = mid-1;
    }
    else {
      term_attr_t cattr = term_attr_none;
      cattr.color = info->color;
      attr_update_with(attr,cattr);
      return info->name;
    }
  }
  // not found
  debug_msg("bbcode: cannot find style %s\n", attr_name);
  return NULL;
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

ic_private const char* parse_tag_value( tag_t* tag, const char* s, const style_t* styles, ssize_t scount ) {
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
  tag->name = attr_update_with_styles( &tag->attr, idbuf, valbuf, styles, scount );  
  return s;
}

static const char* parse_tag_values( tag_t* tag, const char* s, const style_t* styles, ssize_t scount ) {
  s = parse_skip_white(s);  
  while( *s != 0 && *s != ']') {
    s = parse_tag_value(tag, s, styles, scount);
  }
  if (*s == ']') { s++; }
  return s;
}


static const char* parse_tag( tag_t* tag, bool* open, bool* pre, const char* s, const style_t* styles, ssize_t scount ) {
  *open = true;
  *pre = false;
  if (*s != '[') return s;
  s = parse_skip_white(s+1);
  if (*s == '!') { // pre
    *pre = true;
    s = parse_skip_white(s+1);  
  }  
  else if (*s == '/') { 
    *open = false; 
    s = parse_skip_white(s+1); 
  };
  s = parse_tag_values( tag, s, styles, scount);
  return s;
}

//---------------------------------------------------------
// Styles
//---------------------------------------------------------

static void bbcode_parse_tag_content( bbcode_t* bb, const char* s, tag_t* tag ) {
  tag_init(tag);
  parse_tag_values(tag, s, bb->styles, bb->styles_count);
}


ic_private void bbcode_parse_style( bbcode_t* bb, const char* style_name, const char* s ) {
  tag_t tag;
  bbcode_parse_tag_content( bb, s, &tag);
  bbcode_add_style(bb, style_name, tag.attr);
}

ic_private void bbcode_start_style( bbcode_t* bb, const char* fmt ) {
  tag_t tag;
  bbcode_parse_tag_content(bb, fmt, &tag);
  bbcode_open(bb,&tag);
}

ic_private void bbcode_end_style( bbcode_t* bb, const char* fmt ) {
  tag_t tag;
  bbcode_parse_tag_content(bb, fmt, &tag);  
  bbcode_close(bb, tag.name);
}

//---------------------------------------------------------
// Print
//---------------------------------------------------------

ic_private ssize_t bbcode_process_tag( bbcode_t* bb, const char* s, const ssize_t nesting_base ) {
  assert(*s == '[');
  tag_t tag;
  tag_init(&tag);  
  bool open = true;
  bool ispre = false;
  const char* end = parse_tag( &tag, &open, &ispre, s, bb->styles, bb->styles_count ); // todo: styles
  assert(end > s);
  if (open) {
    bbcode_open( bb, &tag );
    if (ispre) {
      // set end tag
      stringbuf_t* pre = sbuf_new(bb->mem);
      if (pre != NULL) {
        sbuf_replace(pre,"[/");
        if (tag.name != NULL) { sbuf_append(pre,tag.name); }
        sbuf_append(pre,"]");
        const char* etag = strstr(end,sbuf_string(pre));
        if (etag == NULL) {
          const ssize_t len = ic_strlen(end);
          term_write_n(bb->term, end, len);
          end += len;
        }
        else {
          term_write_n(bb->term, end, (etag - end));
          end = etag + sbuf_len(pre);
        }
      }
    }
  }
  else {
    // don't pop beyond the base
    if (bb->tags_nesting > nesting_base) {
      bbcode_close( bb, tag.name );
    }
  }  
  return (end - s);
}

ic_private void bbcode_print( bbcode_t* bb, const char* s ) {
  tag_t tag;
  tag_init(&tag); 
  tag.attr = term_get_attr(bb->term);
  bbcode_tag_push(bb,&tag); 
  const ssize_t base = bb->tags_nesting; // base; will not be popped
  ssize_t i = 0;
  while( s[i] != 0 ) {
    // handler no tags in bulk
    ssize_t nobb = 0;
    while( s[i+nobb] != 0 && s[i+nobb] != '[') {
      nobb++;
    }
    if (nobb > 0) { term_write_n(bb->term, s+i, nobb); }
    i += nobb;
    // tag
    if (s[i] == '[') {
      i += bbcode_process_tag(bb, s+i, base);
    }
  }
  // restore unclosed openings
  assert(bb->tags_nesting >= base);
  while( bb->tags_nesting >= base ) {
    bbcode_tag_pop(bb,NULL);
  };
  bbcode_set(bb,tag.attr);
}

ic_private void bbcode_vprintf( bbcode_t* bb, const char* fmt, va_list args  ) {
  stringbuf_t* sb = sbuf_new(bb->mem);
  if (sb == NULL) return;
  sbuf_append_vprintf(sb,fmt,args);
  bbcode_print(bb, sbuf_string(sb));
  sbuf_free(sb);  
}


