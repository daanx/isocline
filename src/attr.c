/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#include <string.h>

#include "common.h"
#include "stringbuf.h" // str_next_ofs
#include "attr.h"

//-------------------------------------------------------------
// Attributes
//-------------------------------------------------------------

ic_private attr_t attr_none(void) {
  attr_t attr = { 0 };
  return attr;
}

ic_private attr_t attr_default(void) {
  attr_t attr = attr_none();
  attr.x.color = IC_ANSI_DEFAULT;
  attr.x.bgcolor = IC_ANSI_DEFAULT;
  attr.x.bold = IC_OFF;
  attr.x.underline = IC_OFF; 
  attr.x.reverse = IC_OFF;
  attr.x.italic = IC_OFF; 
  return attr;
}

ic_private bool attr_is_none(attr_t attr) {
  return (attr.value == 0);
}

ic_private bool attr_is_eq(attr_t attr1, attr_t attr2) {
  return (attr1.value == attr2.value);
}

ic_private attr_t attr_from_color( ic_color_t color ) {
  attr_t attr = attr_none();
  attr.x.color = color;
  return attr;
}


ic_private attr_t attr_update_with( attr_t oldattr, attr_t newattr ) {
  attr_t attr = oldattr;
  if (newattr.x.color != IC_COLOR_NONE)   { attr.x.color = newattr.x.color; }
  if (newattr.x.bgcolor != IC_COLOR_NONE) { attr.x.bgcolor = newattr.x.bgcolor; }
  if (newattr.x.bold != IC_NONE)          { attr.x.bold = newattr.x.bold; }
  if (newattr.x.italic != IC_NONE)        { attr.x.italic = newattr.x.italic; }
  if (newattr.x.reverse != IC_NONE)       { attr.x.reverse = newattr.x.reverse; }
  if (newattr.x.underline != IC_NONE)     { attr.x.underline = newattr.x.underline; }
  return attr;
}

//-------------------------------------------------------------
// Attribute buffer
//-------------------------------------------------------------
struct attrbuf_s {
  attr_t*  attrs;
  ssize_t  capacity;
  ssize_t  count;
  alloc_t* mem;
};

static bool attrbuf_ensure_capacity( attrbuf_t* ab, ssize_t needed ) {
  if (needed <= ab->capacity) return true;
  ssize_t newcap = (ab->capacity <= 0 ? 64 : (ab->capacity > 1024 ? ab->capacity + 1024 : 2*ab->capacity));
  if (needed > newcap) { newcap = needed; }
  attr_t* newattrs = mem_realloc_tp( ab->mem, attr_t, ab->attrs, newcap );
  if (newattrs == NULL) return false;
  ab->attrs = newattrs;
  ab->capacity = newcap;
  assert(needed <= ab->capacity);
  return true;
}

static bool attrbuf_ensure_extra( attrbuf_t* ab, ssize_t extra ) {
  const ssize_t needed = ab->count + extra;
  return attrbuf_ensure_capacity( ab, needed );
}


ic_private attrbuf_t* attrbuf_new( alloc_t* mem ) {
  attrbuf_t* ab = mem_zalloc_tp(mem,attrbuf_t);
  if (ab == NULL) return NULL;
  ab->mem = mem;
  attrbuf_ensure_extra(ab,1);
  return ab;
}

ic_private void attrbuf_free( attrbuf_t* ab ) {
  if (ab==NULL) return;
  mem_free(ab->mem, ab->attrs);
  mem_free(ab->mem, ab);
}

ic_private ssize_t attrbuf_len( attrbuf_t* ab ) {
  return ab->count;
}

ic_private const attr_t* attrbuf_attrs( attrbuf_t* ab, ssize_t expected_len ) {
  assert(expected_len <= ab->count );
  // expand if needed
  if (ab->count < expected_len) {    
    if (!attrbuf_ensure_capacity(ab,expected_len)) return NULL;
    for(ssize_t i = ab->count; i < expected_len; i++) {
      ab->attrs[i] = attr_none();  
    }
    ab->count = expected_len;
  }
  return ab->attrs;
}



static void attrbuf_update_set_at( attrbuf_t* ab, ssize_t pos, ssize_t count, attr_t attr, bool update ) {
  const ssize_t end = pos + count;
  if (!attrbuf_ensure_capacity(ab, end)) return;
  ssize_t i;
  // initialize if end is beyond the count (todo: avoid duplicate init and set if update==false?)
  if (ab->count < end) {
    for(i = ab->count; i < end; i++) {
      ab->attrs[i] = attr_none();  
    }
    ab->count = end;
  }
  // fill pos to end with attr 
  for(i = pos; i < end; i++) {
    ab->attrs[i] = (update ? attr_update_with(ab->attrs[i],attr) : attr);    
  }  
}

ic_private void attrbuf_set_at( attrbuf_t* ab, ssize_t pos, ssize_t count, attr_t attr ) {
  attrbuf_update_set_at(ab, pos, count, attr, false);
}

ic_private void attrbuf_update_at( attrbuf_t* ab, ssize_t pos, ssize_t count, attr_t attr ) {
  attrbuf_update_set_at(ab, pos, count, attr, true);  
}

ic_private void attrbuf_insert_at( attrbuf_t* ab, ssize_t pos, ssize_t count, attr_t attr ) {
  if (pos < 0 || pos > ab->count || count <= 0) return;
  if (!attrbuf_ensure_extra(ab,count)) return;  
  ic_memmove( ab->attrs + pos + count, ab->attrs + pos, (ab->count - pos)*ssizeof(attr_t) );
  ab->count += count;
  attrbuf_set_at( ab, pos, count, attr );
}



ic_private ssize_t attrbuf_append_n( stringbuf_t* sb, attrbuf_t* ab, const char* s, ssize_t len, attr_t attr ) {
  if (s == NULL || len == 0 || !attrbuf_ensure_extra(ab,len)) return sbuf_len(sb);
  attrbuf_set_at(ab, ab->count, len, attr);
  return sbuf_append_n(sb,s,len);
}
