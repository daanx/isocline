/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#include <stdint.h>
#include <limits.h>

// This file is included in "term.c"

//-------------------------------------------------------------
// Standard ANSI palette for 256 colors
//-------------------------------------------------------------

static uint32_t ansi256[256] = {
  // standard ansi
  0x000000,0x800000,0x008000,0x808000,0x000080,0x800080,0x008080,0xc0c0c0,

  // bright ansi
  0x808080, 0xff0000,0x00ff00,0xffff00,0x0000ff,0xff00ff,0x00ffff,0xffffff,

  // 6x6x6 colors
  // 16
  0x000000,0x00005f,0x000087,0x0000af,0x0000d7,0x0000ff,
  0x005f00,0x005f5f,0x005f87,0x005faf,0x005fd7,0x005fff,
  0x008700,0x00875f,0x008787,0x0087af,0x0087d7,0x0087ff,
  0x00af00,0x00af5f,0x00af87,0x00afaf,0x00afd7,0x00afff,
  0x00d700,0x00d75f,0x00d787,0x00d7af,0x00d7d7,0x00d7ff,
  0x00ff00,0x00ff5f,0x00ff87,0x00ffaf,0x00ffd7,0x00ffff,
  // 52
  0x5f0000,0x5f005f,0x5f0087,0x5f00af,0x5f00d7,0x5f00ff,
  0x5f5f00,0x5f5f5f,0x5f5f87,0x5f5faf,0x5f5fd7,0x5f5fff,
  0x5f8700,0x5f875f,0x5f8787,0x5f87af,0x5f87d7,0x5f87ff,
  0x5faf00,0x5faf5f,0x5faf87,0x5fafaf,0x5fafd7,0x5fafff,
  0x5fd700,0x5fd75f,0x5fd787,0x5fd7af,0x5fd7d7,0x5fd7ff,
  0x5fff00,0x5fff5f,0x5fff87,0x5fffaf,0x5fffd7,0x5fffff,
  // 88
  0x870000,0x87005f,0x870087,0x8700af,0x8700d7,0x8700ff,
  0x875f00,0x875f5f,0x875f87,0x875faf,0x875fd7,0x875fff,
  0x878700,0x87875f,0x878787,0x8787af,0x8787d7,0x8787ff,
  0x87af00,0x87af5f,0x87af87,0x87afaf,0x87afd7,0x87afff,
  0x87d700,0x87d75f,0x87d787,0x87d7af,0x87d7d7,0x87d7ff,
  0x87ff00,0x87ff5f,0x87ff87,0x87ffaf,0x87ffd7,0x87ffff,
  // 124
  0xaf0000,0xaf005f,0xaf0087,0xaf00af,0xaf00d7,0xaf00ff,
  0xaf5f00,0xaf5f5f,0xaf5f87,0xaf5faf,0xaf5fd7,0xaf5fff,
  0xaf8700,0xaf875f,0xaf8787,0xaf87af,0xaf87d7,0xaf87ff,
  0xafaf00,0xafaf5f,0xafaf87,0xafafaf,0xafafd7,0xafafff,
  0xafd700,0xafd75f,0xafd787,0xafd7af,0xafd7d7,0xafd7ff,
  0xafff00,0xafff5f,0xafff87,0xafffaf,0xafffd7,0xafffff,
  // 160
  0xd70000,0xd7005f,0xd70087,0xd700af,0xd700d7,0xd700ff,
  0xd75f00,0xd75f5f,0xd75f87,0xd75faf,0xd75fd7,0xd75fff,
  0xd78700,0xd7875f,0xd78787,0xd787af,0xd787d7,0xd787ff,
  0xd7af00,0xd7af5f,0xd7af87,0xd7afaf,0xd7afd7,0xd7afff,
  0xd7d700,0xd7d75f,0xd7d787,0xd7d7af,0xd7d7d7,0xd7d7ff,
  0xd7ff00,0xd7ff5f,0xd7ff87,0xd7ffaf,0xd7ffd7,0xd7ffff,
  // 196
  0xff0000,0xff005f,0xff0087,0xff00af,0xff00d7,0xff00ff,
  0xff5f00,0xff5f5f,0xff5f87,0xff5faf,0xff5fd7,0xff5fff,
  0xff8700,0xff875f,0xff8787,0xff87af,0xff87d7,0xff87ff,
  0xffaf00,0xffaf5f,0xffaf87,0xffafaf,0xffafd7,0xffafff,
  0xffd700,0xffd75f,0xffd787,0xffd7af,0xffd7d7,0xffd7ff,
  0xffff00,0xffff5f,0xffff87,0xffffaf,0xffffd7,0xffffff,

  // gray scale
  0x080808, 0x121212, 0x1c1c1c, 0x262626, 0x303030, 0x3a3a3a, 0x444444, 0x4e4e4e, 
  0x585858, 0x626262, 0x6c6c6c, 0x767676, 0x808080, 0x8a8a8a, 0x949494, 0x9e9e9e, 
  0xa8a8a8, 0xb2b2b2, 0xbcbcbc, 0xc6c6c6, 0xd0d0d0, 0xdadada, 0xe4e4e4, 0xeeeeee   
};


//-------------------------------------------------------------
// Match an rgb color to a ansi8, ansi16, or ansi256
//-------------------------------------------------------------

static bool color_is_rgb( rp_color_t color ) {
  return (color >= RP_RGB(0));  // bit 24 is set for rgb colors
}

static void color_to_rgb(rp_color_t color, int* r, int* g, int* b) {
  assert(color_is_rgb(color));
  *r = ((color >> 16) & 0xFF);
  *g = ((color >> 8) & 0xFF);
  *b = (color & 0xFF);
}

// Approximation to delta-E CIE color distance using much 
// simpler calculations. See <https://www.compuphase.com/cmetric.htm>
static long rgb_dist( uint32_t color, int r2, int g2, int b2 ) {
  int r1, g1, b1;
  color_to_rgb(RP_RGB(color),&r1,&g1,&b1);
  long rmean = (r1 + r2) / 2;
  long r = r1 - r2;
  long g = g1 - g2;
  long b = b1 - b2;
  long dist = ((512+rmean)*r*r)/256 + 4*g*g + ((767-rmean)*b*b)/256;
  return dist;

}

// Return the index of the closest matching color
static int rgb_match( uint32_t* palette, int start, int len, rp_color_t color ) {
  assert(color_is_rgb(color));
  int r, g, b;
  color_to_rgb(color,&r,&g,&b);
  int min = start;
  long mindist = LONG_MAX;
  for(int i = start; i < len; i++) {
    long dist = rgb_dist(palette[i],r,g,b);
    if (dist < mindist) {
      min = i;
      mindist = dist;
    }
  }
  return min;
}

// Match RGB to an index in the ANSI 256 color table
static int rgb_to_ansi256(rp_color_t color) {
  int c = rgb_match( ansi256, 16, 256, color); // not the first 16 ANSI colors as those may be different 
  debug_msg("term: rgb %x -> ansi 256: %d\n", color, c );
  return c;
}

// Match RGB to an ANSI 16 color code (30-37, 90-97)
static int color_to_ansi16(rp_color_t color) {
  if (!color_is_rgb(color)) {
    return (int)color;
  }
  else {
    int c = rgb_match(ansi256, 0, 16, color);
    debug_msg("term: rgb %x -> ansi 16: %d\n", color, c );
    return (c < 8 ? 30 + c : 90 + c - 8); 
  }
}

// Match RGB to an ANSI 16 color code (30-37, 90-97)
// but assuming the bright colors are simulated using 'bold'.
static int color_to_ansi8(rp_color_t color) {
  if (!color_is_rgb(color)) {
    return (int)color;
  }
  else {
    // match to basic 8 colors first
    int c = 30 + rgb_match(ansi256, 0, 8, color);
    // and then adjust for brightness
    int r, g, b;
    color_to_rgb(color,&r,&g,&b);
    if (r>196 || g>196 || b>196) c += 60;
    debug_msg("term: rgb %x -> ansi 8: %d\n", color, c );
    return c;
  }
}


//-------------------------------------------------------------
// Emit color escape codes based on the terminal capability
//-------------------------------------------------------------

static void fmt_color_ansi8( char* buf, ssize_t len, rp_color_t color, bool bg ) {
  int c = color_to_ansi8(color) + (bg ? 10 : 0);
  if (c >= 90) {
    snprintf(buf, len, RP_CSI "1;%dm", c - 60);    
  }
  else {
    snprintf(buf, len, RP_CSI "22;%dm", c );  
  }
}

static void fmt_color_ansi16( char* buf, ssize_t len, rp_color_t color, bool bg ) {
  snprintf( buf, len, RP_CSI "%dm", color_to_ansi16(color) + (bg ? 10 : 0) );  
}

static void fmt_color_ansi256( char* buf, ssize_t len,  rp_color_t color, bool bg ) {
  if (!color_is_rgb(color)) {
    fmt_color_ansi16(buf,len,color,bg);
  }
  else {
    snprintf( buf, len, RP_CSI "%d;5;%dm", (bg ? 48 : 38), rgb_to_ansi256(color) );  
  }
}

static void fmt_color_rgb( char* buf, ssize_t len, rp_color_t color, bool bg ) {
  if (!color_is_rgb(color)) {
    fmt_color_ansi16(buf,len,color,bg);
  }
  else {
    int r,g,b;
    color_to_rgb(color, &r,&g,&b);
    snprintf( buf, len, RP_CSI "%d;2;%d;%d;%dm", (bg ? 48 : 38), r, g, b );  
  }
}

static void fmt_color_ex(char* buf, ssize_t len, palette_t palette, rp_color_t color, bool bg) {
  if (color == RP_COLOR_NONE || palette == MONOCHROME) return;
  if (palette == ANSI8) {
    fmt_color_ansi8(buf,len,color,bg);
  }
  else if (!color_is_rgb(color) || palette == ANSI16) {
    fmt_color_ansi16(buf,len,color,bg);
  }
  else if (palette == ANSI256) {
    fmt_color_ansi256(buf,len,color,bg);
  }
  else {
    fmt_color_rgb(buf,len,color,bg);
  }
}

static void term_color_ex(term_t* term, rp_color_t color, bool bg) {
  char buf[128+1];
  fmt_color_ex(buf,128,term->palette,color,bg);
  term_write(term,buf);
}

//-------------------------------------------------------------
// Main API functions
//-------------------------------------------------------------

rp_private void term_color(term_t* term, rp_color_t color) {
  term_color_ex(term,color,false);
}

rp_private void term_bgcolor(term_t* term, rp_color_t color) {
  term_color_ex(term,color,true);
}

rp_private void term_append_color(term_t* term, stringbuf_t* sbuf, rp_color_t color) {
  char buf[128+1];
  fmt_color_ex(buf,128,term->palette,color,false);
  sbuf_append(sbuf,buf);
}
