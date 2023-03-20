/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>  
#include <sys/stat.h>
#include <sqlite3.h>

#include "../include/isocline.h"
#include "common.h"
#include "history.h"
#include "stringbuf.h"

#define IC_MAX_HISTORY (200)

struct db_t {
  sqlite3             *dbh;
  sqlite3_stmt       **stmts;
  size_t               stmtcnt;
};

struct db_query_t {
  unsigned int id;
  const char *query;
};

struct history_s {
  ssize_t  count;              // current number of entries in use
  ssize_t  len;                // size of elems 
  const char** elems;         // history items (up to count)
  const char*  fname;         // history file
  struct db_t  db;
  alloc_t* mem;
  bool     allow_duplicates;   // allow duplicate entries?
};

static const char *db_tables[] = {
  "create table if not exists cmd     (cid integer, ts integer, cmd text)",
  "create table if not exists session (sid text, cid integer, ts integer, path text)",
  // "create table if not exists path  (cid integer, ts integer, path text)",
  "create index if not exists cmdididx  on cmd(cid, ts)",
  "create index if not exists sessionididx on session(sid, cid, ts)",
  // "create index pathididx on path(cid, ts)",
  NULL
};

enum db_rc {
  DB_ERROR = 0,
  DB_OK,
  DB_ROW,
};

enum db_stmt {
  DB_INS_CMD,
  DB_MAX_ID_CMD,
  DB_COUNT_CMD,
  DB_GET_CMD,
  DB_GET_PREF_CNT,
  DB_GET_PREF_CMD,
  DB_SEARCH_CMD_FWD,
  DB_SEARCH_CMD_BCK,
  DB_UPD_ID_CMD,
  DB_STMT_CNT,
};

static const struct db_query_t db_queries[] = {
  { DB_INS_CMD,           "insert into cmd values (?,?,?)" },
  { DB_MAX_ID_CMD,        "select max(cid) from cmd" },
  { DB_COUNT_CMD,         "select count(cid) from cmd" },
  { DB_GET_CMD,           "select cmd from cmd where cid = ?" },
  { DB_GET_PREF_CNT,      "select count(cid) from cmd where cmd like ?" },
  { DB_GET_PREF_CMD,      "select cmd from cmd where cmd like ? order by cid desc limit 1 offset ?" },
  { DB_SEARCH_CMD_FWD,
    "select cid from cmd where cmd = ? and cid >= ? order by cid asc limit 1" },
  { DB_SEARCH_CMD_BCK,
    "select cid from cmd where cmd = ? and cid <= ? order by cid desc limit 1" },
  { DB_UPD_ID_CMD,        "update cmd set cid = cid + ? where cid > ?" },
  { DB_STMT_CNT,          "" },
};

int db_rc(int rc)
{
	switch (rc) {
	case SQLITE_ERROR:
		return DB_ERROR;
	case SQLITE_OK:
		return DB_OK;
	case SQLITE_ROW:
		return DB_ROW;
	}
	return DB_ERROR;
}

static int db_open(struct db_t *db, const char *fname)
{
  int rc = db_rc(sqlite3_open(fname, &db->dbh));
  if (rc != DB_OK) {
    debug_msg("Cannot open database %s: %s\n", fname, sqlite3_errmsg(db->dbh));
  }
  return rc;
}

static int db_close(struct db_t *db)
{
  int rc = sqlite3_close(db->dbh);
  if (rc != SQLITE_OK) return DB_ERROR;
  return DB_OK;
}

static bool db_exec_str(struct db_t *db, const char *query)
{
  debug_msg("%s\n", query);
  char *err_msg = 0;
  int rc = sqlite3_exec(db->dbh, query, 0, 0, &err_msg);
  if (rc != SQLITE_OK ) {
    debug_msg("SQL error %s in statement:\n", err_msg);
    sqlite3_free(err_msg);
    return false;
  }
  return true;
}

static bool create_tables(struct db_t *db)
{
  for (int i = 0; db_tables[i]; i++) {
    db_exec_str(db, db_tables[i]);
  }
  return true;
}

static int db_prepare_stmts(struct db_t *db, const struct db_query_t db_queries[], size_t n)
{
  int rc = DB_OK;
  db->stmts = calloc(n, sizeof(sqlite3_stmt *));
  db->stmtcnt = n;
  for (size_t i = 0; i < n; i++) {
    if (db_queries[i].id != i) {
      debug_msg("db query strings and ids are inconsistent\n");
      exit(EXIT_FAILURE);
    }
    db->stmts[i] = calloc(1, sizeof(sqlite3_stmt *));
    rc = db_rc(sqlite3_prepare_v2(db->dbh, db_queries[i].query, -1, &db->stmts[i], 0));
    if (rc != DB_OK) {
      debug_msg("failed to prepare statement with stmt idx %ld:\n%s\n", i, db_queries[i].query);
      debug_msg("%s\n", sqlite3_errmsg(db->dbh));
      break;
    }
  }
  return rc;
}

static int db_free_stmts(struct db_t *db)
{
  int rc = DB_OK;
  for (size_t i = 0; i < db->stmtcnt; i++) {
    rc = db_rc(sqlite3_finalize(db->stmts[i]));
    free(db->stmts[i]);
    if (rc != DB_OK) break;
  }
  free(db->stmts);
  return rc;
}

static int db_exec(const struct db_t *db, int stmt) {
  return db_rc(sqlite3_step(db->stmts[stmt]));
}

static int db_in_int(const struct db_t *db, int stmt, int pos, int val) {
  return db_rc(sqlite3_bind_int(db->stmts[stmt], pos, val));
}

static int db_in_txt(const struct db_t *db, int stmt, int pos, const char *val) {
  return db_rc(sqlite3_bind_text(db->stmts[stmt], pos , val, -1, NULL));
}

static int db_out_int(const struct db_t *db, int stmt, int pos) {
  return sqlite3_column_int(db->stmts[stmt], pos - 1);
}

static const unsigned char * db_out_txt(const struct db_t *db, int stmt, int pos) {
  return sqlite3_column_text(db->stmts[stmt], pos - 1);
}

static int db_reset(const struct db_t *db, int stmt) {
  return db_rc(sqlite3_reset(db->stmts[stmt]));
}

ic_private history_t* history_new(alloc_t* mem) {
  history_t* h = mem_zalloc_tp(mem,history_t);
  h->mem = mem;
  return h;
}

ic_private void history_free(history_t* h) {
  if (h == NULL) return;
  // history_clear(h);
  // if (h->len > 0) {
    // mem_free( h->mem, h->elems );
    // h->elems = NULL;
    // h->len = 0;
  // }
  sqlite3_close(h->db.dbh);
  mem_free(h->mem, h->fname);
  h->fname = NULL;
  mem_free(h->mem, h); // free ourselves
}

ic_private bool history_enable_duplicates( history_t* h, bool enable ) {
  bool prev = h->allow_duplicates;
  h->allow_duplicates = enable;
  return prev;
}

ic_private ssize_t  history_count(const history_t* h) {
  db_exec(&h->db, DB_COUNT_CMD);
  int count = db_out_int(&h->db, DB_COUNT_CMD, 1);
  db_reset(&h->db, DB_COUNT_CMD);
  return count;
  // return h->count;
}

//-------------------------------------------------------------
// push/clear
//-------------------------------------------------------------

ic_private bool history_update( history_t* h, const char* entry ) {
  if (entry==NULL) return false;
  // history_remove_last(h);
  history_push(h,entry);
  //debug_msg("history: update: with %s; now at %s\n", entry, history_get(h,0));
  return true;
}

// static void history_delete_at( history_t* h, ssize_t idx ) {
  // if (idx < 0 || idx >= h->count) return;
  // mem_free(h->mem, h->elems[idx]);
  // for(ssize_t i = idx+1; i < h->count; i++) {
    // h->elems[i-1] = h->elems[i];
  // }
  // h->count--;
// }

/// TODO check if entry is already in cmd table, then update timestamp
/// TODO add timestamp
/// TODO trim entry (add an optional setting)
ic_private bool history_push( history_t* h, const char* entry ) {
  /// There's always an empty statement inserted, I guess that's
  /// in the main editline() function ... not sure why
  /// ... that's most likely also the reason why history_remove_last()
  /// is called in history_update()
  if (strlen(entry) == 0) return true;
  if (h->len <= 0 || entry==NULL)  return false;
  db_exec(&h->db, DB_MAX_ID_CMD);
  int new_cid = db_out_int(&h->db, DB_MAX_ID_CMD, 1) + 1;
  db_reset(&h->db, DB_MAX_ID_CMD);
  db_in_int(&h->db, DB_INS_CMD, 1, new_cid);
  db_in_int(&h->db, DB_INS_CMD, 2, 0);
  db_in_txt(&h->db, DB_INS_CMD, 3, entry);
  db_exec(&h->db, DB_INS_CMD);
  db_reset(&h->db, DB_INS_CMD);
  // exec_stmt(h->db, "insert into cmd values (?,?,?)");
  // // remove any older duplicate
  // if (!h->allow_duplicates) {
    // for( int i = 0; i < h->count; i++) {
      // if (strcmp(h->elems[i],entry) == 0) {
        // history_delete_at(h,i);
      // }
    // }
  // }
  // // insert at front
  // if (h->count == h->len) {
    // // delete oldest entry
    // history_delete_at(h,0);    
  // }
  // assert(h->count < h->len);
  // h->elems[h->count] = mem_strdup(h->mem,entry);
  // h->count++;
  return true;
}


// static void history_remove_last_n( history_t* h, ssize_t n ) {
  // if (n <= 0) return;
  // if (n > h->count) n = h->count;
  // for( ssize_t i = h->count - n; i < h->count; i++) {
    // mem_free( h->mem, h->elems[i] );
  // }
  // h->count -= n;
  // assert(h->count >= 0);
// }

ic_private void history_remove_last(history_t* h) {
  (void)h;
  // history_remove_last_n(h,1);
}

ic_private void history_clear(history_t* h) {
  (void)h;
  // history_remove_last_n( h, h->count );
}

/// Parameter n is the history command index from latest to oldest, starting with 1
/// TODO need to free the returned string
ic_private const char* history_get( const history_t* h, ssize_t n ) {
  db_exec(&h->db, DB_MAX_ID_CMD);
  int max_cid = db_out_int(&h->db, DB_MAX_ID_CMD, 1);
  db_reset(&h->db, DB_MAX_ID_CMD);
  if (n <= 0 || n > max_cid) return NULL;
  db_in_int(&h->db, DB_GET_CMD, 1, max_cid - n + 1);
  /// TODO check if row is returned
  db_exec(&h->db, DB_GET_CMD);
  const char* ret = mem_strdup(h->mem, (const char*)db_out_txt(&h->db, DB_GET_CMD, 1));
  db_reset(&h->db, DB_GET_CMD);
  return ret;
  // if (n < 0 || n >= h->count) return NULL;
  // return h->elems[h->count - n - 1];
}

ic_private const char* history_get_with_prefix( const history_t* h, ssize_t n, const char* prefix ) {
  char prefix_param[64] = {0};
  sprintf(prefix_param, "%s%%", prefix);
  db_in_txt(&h->db, DB_GET_PREF_CNT, 1, prefix_param);
  db_exec(&h->db, DB_GET_PREF_CNT);
  int cnt = db_out_int(&h->db, DB_GET_PREF_CNT, 1);
  db_reset(&h->db, DB_GET_PREF_CNT);
  if (n <= 0 || n > cnt) return NULL;
  // db_in_int(&h->db, DB_GET_PREF_CMD, 1, cnt - n + 1);
  db_in_txt(&h->db, DB_GET_PREF_CMD, 1, prefix_param);
  db_in_int(&h->db, DB_GET_PREF_CMD, 2, n);
  /// TODO check if row is returned
  db_exec(&h->db, DB_GET_PREF_CMD);
  const char* ret = mem_strdup(h->mem, (const char*)db_out_txt(&h->db, DB_GET_PREF_CMD, 1));
  db_reset(&h->db, DB_GET_PREF_CMD);
  return ret;
  // if (n < 0 || n >= h->count) return NULL;
  // return h->elems[h->count - n - 1];
}

ic_private bool history_search( const history_t* h, ssize_t from /*including*/, const char* search, bool backward, ssize_t* hidx, ssize_t* hpos ) {
  debug_msg("searching history %s for '%s' from %d\n", backward ? "back" : "forward", search, from);
  int search_query = backward ? DB_SEARCH_CMD_BCK : DB_SEARCH_CMD_FWD;
  int i = -1;
  db_in_txt(&h->db, search_query, 1, search);
  db_in_int(&h->db, search_query, 2, from);
  int ret = db_exec(&h->db, search_query);
  if (ret == DB_ROW) i = db_out_int(&h->db, search_query, 1);
  db_reset(&h->db, search_query);
  if (i == -1) debug_msg("searching '%s': not found\n", search);
  if (i == -1) return false;
  debug_msg("found '%s' at index: %d\n", search, i);
  if (hidx != NULL) *hidx = i;
  if (hpos != NULL) *hpos = 0;
  return true;
  // const char* p = NULL;
  // ssize_t i;
  // if (backward) {
    // for( i = from; i < h->count; i++ ) {
      // p = strstr( history_get(h,i), search);
      // if (p != NULL) break;
    // }
  // }
  // else {
    // for( i = from; i >= 0; i-- ) {
      // p = strstr( history_get(h,i), search);
      // if (p != NULL) break;
    // }
  // }
  // if (p == NULL) return false;
  // if (hidx != NULL) *hidx = i;
  // if (hpos != NULL) *hpos = (p - history_get(h,i));
  // return true;
}

//-------------------------------------------------------------
// 
//-------------------------------------------------------------

ic_private void history_load_from(history_t* h, const char* fname, long max_entries ) {
  history_clear(h);
  h->fname = mem_strdup(h->mem,fname);
  if (max_entries == 0) {
    assert(h->elems == NULL);
    return;
  }
  if (max_entries < 0 || max_entries > IC_MAX_HISTORY) max_entries = IC_MAX_HISTORY;
  h->elems = (const char**)mem_zalloc_tp_n(h->mem, char*, max_entries );
  if (h->elems == NULL) return;
  h->len = max_entries;
  history_load(h);
}

//-------------------------------------------------------------
// save/load history to file
//-------------------------------------------------------------

// static char from_xdigit( int c ) {
  // if (c >= '0' && c <= '9') return (char)(c - '0');
  // if (c >= 'A' && c <= 'F') return (char)(10 + (c - 'A'));
  // if (c >= 'a' && c <= 'f') return (char)(10 + (c - 'a'));
  // return 0;
// }

// static char to_xdigit( uint8_t c ) {
  // if (c <= 9) return ((char)c + '0');
  // if (c >= 10 && c <= 15) return ((char)c - 10 + 'A');
  // return '0';
// }

// static bool ic_isxdigit( int c ) {
  // return ((c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F') || (c >= '0' && c <= '9'));
// }

// static bool history_read_entry( history_t* h, FILE* f, stringbuf_t* sbuf ) {
  // sbuf_clear(sbuf);
  // while( !feof(f)) {
    // int c = fgetc(f);
    // if (c == EOF || c == '\n') break;
    // if (c == '\\') {
      // c = fgetc(f);
      // if (c == 'n')       { sbuf_append(sbuf,"\n"); }
      // else if (c == 'r')  { /* ignore */ }  // sbuf_append(sbuf,"\r");
      // else if (c == 't')  { sbuf_append(sbuf,"\t"); }
      // else if (c == '\\') { sbuf_append(sbuf,"\\"); }
      // else if (c == 'x') {
        // int c1 = fgetc(f);
        // int c2 = fgetc(f);
        // if (ic_isxdigit(c1) && ic_isxdigit(c2)) {
          // char chr = from_xdigit(c1)*16 + from_xdigit(c2);
          // sbuf_append_char(sbuf,chr);
        // }
        // else return false;
      // }
      // else return false;
    // }
    // else sbuf_append_char(sbuf,(char)c);
  // }
  // if (sbuf_len(sbuf)==0 || sbuf_string(sbuf)[0] == '#') return true;
  // return history_push(h, sbuf_string(sbuf));
// }

// static bool history_write_entry( const char* entry, FILE* f, stringbuf_t* sbuf ) {
  // sbuf_clear(sbuf);
  // //debug_msg("history: write: %s\n", entry);
  // while( entry != NULL && *entry != 0 ) {
    // char c = *entry++;
    // if (c == '\\')      { sbuf_append(sbuf,"\\\\"); }
    // else if (c == '\n') { sbuf_append(sbuf,"\\n"); }
    // else if (c == '\r') { /* ignore */ } // sbuf_append(sbuf,"\\r"); }
    // else if (c == '\t') { sbuf_append(sbuf,"\\t"); }
    // else if (c < ' ' || c > '~' || c == '#') {
      // char c1 = to_xdigit( (uint8_t)c / 16 );
      // char c2 = to_xdigit( (uint8_t)c % 16 );
      // sbuf_append(sbuf,"\\x");
      // sbuf_append_char(sbuf,c1);
      // sbuf_append_char(sbuf,c2);
    // }
    // else sbuf_append_char(sbuf,c);
  // }
  // //debug_msg("history: write buf: %s\n", sbuf_string(sbuf));
  //
  // if (sbuf_len(sbuf) > 0) {
    // sbuf_append(sbuf,"\n");
    // fputs(sbuf_string(sbuf),f);
  // }
  // return true;
// }

ic_private void history_load( history_t* h ) {
  if (db_open(&h->db, h->fname) != DB_OK) return;
  if (!create_tables(&h->db)) return;
  db_prepare_stmts(&h->db, db_queries, DB_STMT_CNT);

  // if (h->fname == NULL) return;
  // FILE* f = fopen(h->fname, "r");
  // if (f == NULL) return;
  // stringbuf_t* sbuf = sbuf_new(h->mem);
  // if (sbuf != NULL) {
    // while (!feof(f)) {
      // if (!history_read_entry(h,f,sbuf)) break; // error
    // }
    // sbuf_free(sbuf);
  // }
  // fclose(f);
}

ic_private void history_save( const history_t* h ) {
  (void)h;
  // if (h->fname == NULL) return;
  // FILE* f = fopen(h->fname, "w");
  // if (f == NULL) return;
  // #ifndef _WIN32
  // chmod(h->fname,S_IRUSR|S_IWUSR);
  // #endif
  // stringbuf_t* sbuf = sbuf_new(h->mem);
  // if (sbuf != NULL) {
    // for( int i = 0; i < h->count; i++ )  {
      // if (!history_write_entry(h->elems[i],f,sbuf)) break;  // error
    // }
    // sbuf_free(sbuf);
  // }
  // fclose(f);  
}
