/* ----------------------------------------------------------------------------
  Copyright (c) 2023, Jörg Bakker
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>  
#include <sys/stat.h>
#include <time.h>
#include <sqlite3.h>

#include "../include/isocline.h"
#include "common.h"
#include "history.h"
#include "stringbuf.h"

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
  const char*  fname;              // history file
  struct db_t  db;
  alloc_t*     mem;
  bool         allow_duplicates;   // allow duplicate entries?
};

static const char *db_tables[] = {
  "create table if not exists cmds (cid integer, ts integer, cmd text)",
  "create index if not exists cmdididx on cmds(cid, ts)",
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
  DB_GET_PREF_CNT,
  DB_GET_PREF_CMD,
  DB_GET_CMD_ID,
  DB_DEL_CMD_ID,
  DB_DEL_ALL,
  DB_UPD_TS,
  DB_STMT_CNT,
};

static const struct db_query_t db_queries[] = {
  { DB_INS_CMD,           "insert into cmds values (?,?,?)" },
  { DB_MAX_ID_CMD,        "select max(cid) from cmds" },
  { DB_COUNT_CMD,         "select count(cid) from cmds" },
  { DB_GET_PREF_CNT,      "select count(cid) from cmds where cmd like ?" },
  { DB_GET_PREF_CMD,      "select cmd from cmds where cmd like ? order by ts desc, cid desc limit 1 offset ?" },
  { DB_GET_CMD_ID,        "select cid from cmds where cmd = ? limit 1" },
  { DB_DEL_CMD_ID,        "delete from cmds where cid = ?" },
  { DB_DEL_ALL,           "delete from cmds" },
  { DB_UPD_TS,            "update cmds set ts = ? where cid = ?" },
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
  sqlite3_close(h->db.dbh);
  mem_free(h->mem, h->fname);
  h->fname = NULL;
  mem_free(h->mem, h); // free ourselves
}

ic_private bool history_enable_duplicates(history_t* h, bool enable) {
  bool prev = h->allow_duplicates;
  h->allow_duplicates = enable;
  return prev;
}

ic_private ssize_t history_count_with_prefix(const history_t* h, const char *prefix) {
  char *prefix_param = mem_malloc(h->mem, ic_strlen(prefix) + 3);
  sprintf(prefix_param, "%s%%", prefix);
  db_in_txt(&h->db, DB_GET_PREF_CNT, 1, prefix_param);
  db_exec(&h->db, DB_GET_PREF_CNT);
  int count = db_out_int(&h->db, DB_GET_PREF_CNT, 1);
  db_reset(&h->db, DB_GET_PREF_CNT);
  mem_free(h->mem, prefix_param);
  return count;
}

//-------------------------------------------------------------
// push/clear
//-------------------------------------------------------------

static time_t get_current_ts(void)
{
  struct timespec curtime;
  clock_gettime(CLOCK_REALTIME, &curtime);
  return curtime.tv_sec;
}

ic_private bool history_push( history_t* h, const char* entry ) {
  if (entry==NULL || ic_strlen(entry) == 0) return false;

  if (!h->allow_duplicates) {
    db_in_txt(&h->db, DB_GET_CMD_ID, 1, entry);
    int cid = -1;
    if (db_exec(&h->db, DB_GET_CMD_ID) == DB_ROW) {;
        cid = db_out_int(&h->db, DB_GET_CMD_ID, 1);
    }
    db_reset(&h->db, DB_GET_CMD_ID);
    if (cid != -1) {
      debug_msg("duplicate entry: %s\n", entry);
      db_in_int(&h->db, DB_UPD_TS, 1, get_current_ts());
      db_in_int(&h->db, DB_UPD_TS, 2, cid);
      db_exec(&h->db, DB_UPD_TS);
      db_reset(&h->db, DB_UPD_TS);
      return false;
    }
  }

  db_exec(&h->db, DB_MAX_ID_CMD);
  int new_cid = db_out_int(&h->db, DB_MAX_ID_CMD, 1) + 1;
  db_reset(&h->db, DB_MAX_ID_CMD);

  db_in_int(&h->db, DB_INS_CMD, 1, new_cid);
  db_in_int(&h->db, DB_INS_CMD, 2, get_current_ts());
  db_in_txt(&h->db, DB_INS_CMD, 3, entry);
  db_exec(&h->db, DB_INS_CMD);
  db_reset(&h->db, DB_INS_CMD);
  return true;
}

ic_private void history_remove_last(history_t* h) {
  db_exec(&h->db, DB_MAX_ID_CMD);
  int last_cid = db_out_int(&h->db, DB_MAX_ID_CMD, 1);
  db_reset(&h->db, DB_MAX_ID_CMD);

  db_in_int(&h->db, DB_DEL_CMD_ID, 1, last_cid);
  db_exec(&h->db, DB_DEL_CMD_ID);
  db_reset(&h->db, DB_DEL_CMD_ID);
}

ic_private void history_clear(history_t* h) {
  db_exec(&h->db, DB_DEL_ALL);
  db_reset(&h->db, DB_DEL_ALL);
}

/// Parameter n is the history command index from latest to oldest, starting with 1
/// NOTE need to free the returned string at the callsite with this backend
ic_private const char* history_get_with_prefix( const history_t* h, ssize_t n, const char* prefix ) {
  if (n <= 0) return NULL;
  char *prefix_param = mem_malloc(h->mem, ic_strlen(prefix) + 3);
  sprintf(prefix_param, "%s%%", prefix);
  db_in_txt(&h->db, DB_GET_PREF_CNT, 1, prefix_param);
  db_exec(&h->db, DB_GET_PREF_CNT);
  int cnt = db_out_int(&h->db, DB_GET_PREF_CNT, 1);
  db_reset(&h->db, DB_GET_PREF_CNT);
  if (n < 0 || n > cnt) return NULL;
  db_in_txt(&h->db, DB_GET_PREF_CMD, 1, prefix_param);
  db_in_int(&h->db, DB_GET_PREF_CMD, 2, n - 1);
  int ret = db_exec(&h->db, DB_GET_PREF_CMD);
  if (ret != DB_ROW) return NULL;
  const char* entry = mem_strdup(h->mem, (const char*)db_out_txt(&h->db, DB_GET_PREF_CMD, 1));
  db_reset(&h->db, DB_GET_PREF_CMD);
  mem_free(h->mem, prefix_param);
  return entry;
}

//-------------------------------------------------------------
// save/load history to file
//-------------------------------------------------------------

ic_private void history_load_from(history_t* h, const char* fname, long max_entries ) {
  ic_unused(max_entries);
  h->fname = mem_strdup(h->mem,fname);
  if (db_open(&h->db, h->fname) != DB_OK) return;
  if (!create_tables(&h->db)) return;
  db_prepare_stmts(&h->db, db_queries, DB_STMT_CNT);
}

/// function history_load() is not needed ...
ic_private void history_load(history_t* h) {
  ic_unused(h);
}

/// function history_save() is not needed with this backend
ic_private void history_save( const history_t* h ) {
  ic_unused(h);
}
