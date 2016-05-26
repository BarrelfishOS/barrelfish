/* BEGIN LICENSE BLOCK
 * Version: CMPL 1.1
 *
 * The contents of this file are subject to the Cisco-style Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file except
 * in compliance with the License.  You may obtain a copy of the License
 * at www.eclipse-clp.org/license.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
 * the License for the specific language governing rights and limitations
 * under the License. 
 * 
 * The Original Code is  The ECLiPSe Constraint Logic Programming System. 
 * The Initial Developer of the Original Code is  Cisco Systems, Inc. 
 * Portions created by the Initial Developer are
 * Copyright (C) 1996 - 2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 *
 * Contributor(s): Joachim Schimpf, Stefano Novello, IC-Parc
 *                 Kish Shen, CrossCore Optimization
 *
 * END LICENSE BLOCK */

/*
 *
 * Contents:	Prolog wrappers around Oracle Call Interface Headers
 *
 * Author:	Stefano Novello
 * Author:      Kish Shen, Generalised and updated from original OCI code,
 *              intially for MySQL, Jan - Feb 2006.
 *
 */


/* ----------------------------------------------------------------------
 *  Definitions
 * ---------------------------------------------------------------------- */

/* ----------------------------------------------------------------------
 *  Types
 * ---------------------------------------------------------------------- */

typedef struct {
    /* filled in from Prolog side */
    char prolog_tag;
    int  ext_type;
    /* filled in from DB */
    int  dbtype;
    word  size; /* e.g. for length of fixed len strings */
    /*
     * give a buffer b, this argument in the ith tuple
     * (starting from 0) is to
     * be found in b[offset + i * increment]
     */
    word  offset;
    word  increment;
#ifdef USE_MYSQL
    my_bool is_null; 
#endif
} map_t;

typedef struct {
	dident did;	/* Used to create structure functor */
	uword arity; 	/* number of slots in type map */
	uword tuples;	/* number of tuples stored in cursor */
	uword from;	/* index of first valid unread tuple */
	uword to;		/* 1 + index of last valid unread tuple */
	map_t * map;	/* Maps prolog types to Oracle external types */
} template_t;

/* ---------------------------------------------------------------------- */


/* ---------------------------------------------------------------------- */


typedef struct
{
    /* User fields */
    int refs;    /* How many live references to this session */
    char closed; /* Was the session closed */
    char in_transaction;
    /* DB specific fields */
#ifdef ORACLE
    Lda_Def lda;
    ub1 hda[256];
#endif
#ifdef USE_MYSQL
    MYSQL * mysql;
#endif
} session_t;


/* ---------------------------------------------------------------------- */

typedef enum
{
	opened,
	executed,
	idle,
	nodata,
	closed,
} cursor_state_t;

/* ---------------------------------------------------------------------- */

typedef enum
{
         direct,
         prepared
} sql_t;
 
/* ---------------------------------------------------------------------- */

typedef struct cursor_handle
{
 /* User fields */
    sql_t sql_type;
    word prolog_processed_count;
    template_t * param_template;
    template_t * tuple_template;
    session_t * session;
    void * param_buffer;
    unsigned long * param_datalengths;
    void * tuple_buffer;
    unsigned long * tuple_datalengths;
    cursor_state_t state;
 /* Oracle specific fields */
#ifdef ORACLE
    Cda_Def cda;
#endif
#ifdef USE_MYSQL
    my_bool * tuple_errors;
    unsigned long sql_length; /* prepared or not */
    char server_cursor;       /* server-side cursor or not */ 
    char cursor_type;         /* type of cursor (prepared only) */
    union 
    {
	MYSQL_STMT * stmt; /* prepared SQL statement */
	char * sql;        /* SQL statement (raw) */
	MYSQL_RES * res;   /* result for SQL statement */
    } s;
#endif
} cursor_t;

#ifdef USE_MYSQL
/* cursor types */
#define CURSOR_NO_CURSOR 0
#define CURSOR_READ_ONLY 1

#endif

#define DEFAULT_BUFFER_SIZE 1000

/* ---------------------------------------------------------------------- */

typedef enum
{
	state,
	rows_processed_count,
	return_code,
	return_code_as_string,
	warning_flags,
	row_ID
} field_t;

#define FIELD_FIRST (state)
#define FIELD_LAST (row_ID)


/* free *p if it is pointing at something */
#define TryFree(p)  {if (p) { free(p); p = NULL; } }

/* dbformat header */
#define DBF_HEADER_LEN		2

static unsigned char dbformat_header[DBF_HEADER_LEN] = {'D','B'};

/* ----------------------------------------------------------------------
 *  Forward declarations
 * ---------------------------------------------------------------------- */

void 
session_init(session_t ** session);

int 
session_start(session_t * session, char * username, char * host, 
              char * password, value v_opts);

void
session_error_value(session_t * session, int * code, char ** msg);

int
session_commit(session_t * session);

int
session_rollback(session_t * session);

int
session_sql_ddl(session_t *, char * SQL);

int
session_sql_delete(session_t * session, char * SQL);

int
session_sql_update(session_t * session, char * SQL);

cursor_t *
session_sql_prepare(session_t * session, char * SQL, word lenght, char use_prepared);

cursor_t *
session_sql_prep(session_t *session, template_t *template, char *SQL, word length, word N);

cursor_t *
ready_session_sql_cursor(session_t *session,
	template_t *params, template_t *query, char *SQL, word length, 
        word N, char use_prepared);

int 
session_tostr(session_t * session, char *buf, int quoted);

void
session_free(session_t * session); /* DB independent part */

void
session_close(session_t * session); /* DB dependent part */

session_t *
session_copy(session_t * session); /* DB independent */

int
cursor_set_options(cursor_t * cursor, value v_opts); /* DB dependent */

int
cursor_sql_execute(cursor_t * cursor, int with_options);

int
cursor_bind_placeholder(cursor_t * cursor, char * placeholder, char * value);

int
cursor_next_tuple(cursor_t * cursor, void ** buffer);

int
cursor_one_tuple(cursor_t *cursor);

int 
cursor_N_tuples(cursor_t* cursor, word* n, pword* tuple_listp, pword** tp);

int
cursor_all_tuples(cursor_t * cursor, template_t * template, void * buffer);

int
cursor_field_value(cursor_t * cursor, field_t field, void ** value);

int 
cursor_tostr(cursor_t * cursor, char *buf, int quoted);

void
cursor_free(cursor_t * cursor);


void
dbi_init();

void
dbi_final();
