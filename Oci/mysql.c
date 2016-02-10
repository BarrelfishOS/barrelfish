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
 * Copyright (C) 1997 - 2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 *
 * Contributor(s): Joachim Schimpf, Stefano Novello, IC-Parc
 *                 Kish Shen, CrossCore Optimization
 *
 * END LICENSE BLOCK */

/*
 * ECLiPSe LIBRARY MODULE
 *
 * $Header: /cvsroot/eclipse-clp/Eclipse/Oci/mysql.c,v 1.9 2015/10/29 01:04:21 kish_shen Exp $
 *
 *
 * IDENTIFICATION:	mysql.c
 *
 * AUTHOR:		Joachim Schimpf
 * AUTHOR:		Stefano Novello
 * AUTHOR:		Kish Shen
 *
 */

/*
 * char *sccsid = "%W%  %E%";
 *
 * Contents:	Prolog wrappers around Oracle Call Interface
 *
 * Author:	Stefano Novello
 * Author:      Kish Shen, Converted to MySQL from original oracle.c
 *              Jan - Feb, 2006.
 *
 *
 * TODO General header for contents of this file
 */

#ifdef _WIN32
#include <windows.h>
#endif
#include <stdio.h>
#include <stdlib.h>
/*#include <malloc.h>*/
#include <string.h>
#include <mysql/mysql.h> 
#include "external.h"	/* ECLiPSe definitions */
#include "dbi.h"	/* Oracle call interface */


/* ----------------------------------------------------------------------
 *  Definitions
 * ---------------------------------------------------------------------- */

/* these must correspond to their named structure positions in the ECLiPSe
   option code
*/
#define SESSION_OPT_DBNAME	1
#define SESSION_OPT_STORAGE     2
#define CURSOR_OPT_BUFFER	3
#define CURSOR_OPT_TYPE		4

#define NoErrors {err_code = 0 ; err_msg = "\0"; }

#define DBI_TYPE_CONV       1
#define DBI_BAD_CURSOR      2
#define DBI_BAD_SESSION     3
#define DBI_BAD_FIELD       4
#define DBI_BAD_TEMPLATE    5
#define DBI_NOT_QUERY       6
#define DBI_CANCELLED       7
#define DBI_NOT_PREPARED    8
#define DBI_NO_PARAM        9
#define DBI_NYI            10
#define DBI_MEMORY         11
#define DBI_BUFFER_OVER    12
#define DBI_DATA_TRUNC     13

#ifdef HAVE_LONG_LONG

#define BUFINT long long int

typedef long long long_long;

#define HAVE_MYSQLBIGINT  

#elif defined(HAVE__INT64)

typedef __int64 long_long;

#define BUFINT __int64

#define HAVE_MYSQLBIGINT

#else

#define BUFINT int

#undef HAVE_MYSQLBIGINT

#endif

/* ----------------------------------------------------------------------
 *  Global data
 * ---------------------------------------------------------------------- */

int  err_code;
const char *err_msg;
char *dbi_error[] =
{
/* DBI_TYPE_CONV */	"DBI-001: type conversion failure" ,
/* DBI_BAD_CURSOR */	"DBI-002: bad cursor state" ,
/* DBI_BAD_SESSION */	"DBI-003: bad session state",
/* DBI_BAD_FIELD */	"DBI-004: bad field name",
/* DBI_BAD_TEMPLATE */  "DBI-005: bad template",
/* DBI_NOT_QUERY */	"DBI-006: not a query",
/* DBI_CANCELLED */	"DBI-007: cursor was cancelled",
/* DBI_NOT_PREPARED */	"DBI-008: cursor was not a prepared SQL",
/* DBI_NO_PARAM */      "DBI-009: input parameters not supplied",
/* DBI_NYI */		"DBI-010: not implemented" ,
/* DBI_MEMORY */        "DBI-011: memory allocation problem",
/* DBI_BUFFER_OVER */   "BBI-012: buffer overflow",
/* DBI_DATA_TRUNC */    "BBI-013: result data truncated"
};

/* ----------------------------------------------------------------------
 *  Internally used procedures
 * ---------------------------------------------------------------------- */

void
raise_dbi_error(int code)
{
	err_code= - code;
	err_msg = dbi_error[code - 1];
#ifdef DEBUG
	fprintf(stderr,"DEBUG DBI: %d %s\n",err_code,err_msg);
#endif
}


void
raise_mysql_error(MYSQL * mysql)
{

	err_code = mysql_errno(mysql);
	err_msg = mysql_error(mysql);
#ifdef DEBUG
	fprintf(stderr,"DEBUG SQL: %d %s\n",err_code,err_msg);
#endif
}

void
raise_mysql_stmt_error(MYSQL_STMT * stmt)
{

	err_code = mysql_stmt_errno(stmt);
	err_msg = mysql_stmt_error(stmt);
#ifdef DEBUG
	fprintf(stderr,"DEBUG OSQL: %d %s\n",err_code,err_msg);
#endif
}
/* ----------------------------------------------------------------------
 *  Auxiliary functions
 * ---------------------------------------------------------------------- */

#define RoundupSize(s) (sizeof(word) * (1 + (s)/sizeof(word)))

/* initialise the data structures  associated with a template from the
   information supplied from Prolog. This should be followed, for prepared
   statements,  by allocation of the data buffers, and the binding of the DB 
   cursor's parameter/result to the data buffers. Finally the actual values
   of parameters are loaded into the data buffers by template_bind, or the
   result values are extracted from the DB's return result with template_put
   Direct statements have no parameters, and the result row do not need 
   user-supplied buffers, so only template_put has to be called.
*/
int
template_get(value v,type t,template_t * * template_out)
{
	dident did;
	char argtag;
	word arity;
	word i;
	word size;
	template_t * template;
	pword * arg;

	if (IsNil(t)) 
	{
	    *template_out = NULL;
	    return 0;
	}
	Check_Structure(t);

	did =  v.ptr->val.did;
	arity = DidArity(did);

	if (!(template = (template_t *) malloc(sizeof(template_t))))
        {
	    raise_dbi_error(DBI_MEMORY);
	    return -1;
	}
	memset(template, 0, sizeof(template_t));

	template->did = did;
	template->arity = arity;
	if (!(template->map = (map_t *) malloc(arity * sizeof(map_t))))
        {
	    raise_dbi_error(DBI_MEMORY);
	    return -1;
	}
	memset(template->map, 0, arity * sizeof(map_t));

	for( i = 0 ; i < arity ; i ++)
	{
	    arg = v.ptr+i+1;
	    Dereference_(arg);
	    argtag = TagType(arg->tag) ;
	    template->map[i].prolog_tag = argtag;
	    switch (argtag) {
	    case TDICT:
		/* LONG VARCHAR */
	    	template->map[i].ext_type = MYSQL_TYPE_VAR_STRING;
		size = atoi( DidName(arg->val.did) );
		if ( size && size > 0)
		{
		    template->map[i].size = RoundupSize(size);
		}
		else
		{
		    template->map[i].size =  DEFAULT_BUFFER_SIZE;
		}
	    	break;
	    case TSTRG:
		/* LONG VARCHAR */
	    	template->map[i].ext_type = MYSQL_TYPE_VAR_STRING;
		size = atoi( StringStart(arg->val) );
		if ( size && size > 0 )
		{
		    template->map[i].size = RoundupSize(size);
		}
		else
		{
		    template->map[i].size = DEFAULT_BUFFER_SIZE;
		}
	    	break;
	    case TINT:
		/* signed integer */
#ifdef HAVE_MYSQLBIGINT /* buffer for 64 bit integers if available */
		template->map[i].ext_type = MYSQL_TYPE_LONGLONG;
#else
		template->map[i].ext_type = MYSQL_TYPE_LONG;
#endif
		template->map[i].size =  sizeof(BUFINT);
	    	break;
	    case TDBL:
		template->map[i].ext_type = MYSQL_TYPE_DOUBLE;
		template->map[i].size =  sizeof(double);
	    	break;
	    default:
		/* disable typechecking */
		template->map[i].prolog_tag = 0;

		template->map[i].ext_type = MYSQL_TYPE_BLOB;
		if (argtag == TCOMP && 
		    TagType((arg->val.ptr + 1)->tag) == TINT
		   )
		    template->map[i].size =
				    RoundupSize((arg->val.ptr + 1)->val.nint);
		else
		    template->map[i].size = DEFAULT_BUFFER_SIZE*2;
	    	break;
	    }
	}
#ifdef DEBUG
	for( i = 0 ; i < arity ; i ++)
	{
		fprintf(stderr,"DEBUG template i %d size %d etype %d\n",
			i, template->map[i].size, template->map[i].ext_type); 
	}
	fprintf(stderr, "DEBUG template_get template=0x%x m=0x%x\n",
		template,template->map);
#endif

	*template_out = template;
	Succeed;
}

/* Construct a Prolog structure for a row of tuple results, copying the data
   from the tuple buffer. For tuple (output) templates only
   tuple_num is for compatibility for the Oracle code only and is not used
   For MySQL, tuple_num must be 0.
*/
int
template_put(int tuple_num, template_t * template,sql_t sql_type,
	     void * buffer, void * lengths, pword * tuple) 
{
    word i;
    word arg;
    map_t * argmap;
    char * argbuf;
    pword *pw;
    char *s;
    double d;
    pword * res;
    extern pword * dbformat_to_term(char *, dident, type);

    pw = TG;
    Make_Struct(tuple , pw);
    Push_Struct_Frame(template->did);
    for (arg = 0 ; arg < template->arity ; arg++)
    {
	/* For prepared statements, buffers with sizes specified by the 
           results tuple template is used to receive the results. A 
           MYSQL_DATA_TRUNCATED result code is returned by
           mysql_stmt_fetch() if the returned results are truncated.
	*/
	argmap = &(template->map[arg]);
	if (sql_type == prepared)
	{
	    if (argmap->is_null)
	    {/* NULL value */
		Make_Var(&pw[arg+1]);
		continue;
	    }
	    argbuf = ((char *)buffer) + argmap->offset;

	} else /* if (sql_type == direct) */
	{
	    /* For direct statements, the buffer sizes specified
               by the results tuple template can be ignored, because the
               results are returned as a byte stream by MySQL, which is then
               converted to ECLiPSe data structure, using the global stack
               as needed
	    */
	    argbuf = (char *) ((MYSQL_ROW)buffer)[arg]; 
	    if (argbuf == NULL)
	    {/* NULL value */
		Make_Var(&pw[arg+1]);
		continue;
	    }
	}
	switch(argmap->prolog_tag)
	{
	case TDICT:
	    /* lengths is a unsigned long * rather than uword *, as defined
               in MySQL. 
	    */
	    argbuf[((unsigned long *)lengths)[arg] ] = '\0';
	    Make_Atom(&pw[arg+1], Did(argbuf, 0));
	    break;
	case TSTRG:
	    pw[arg+1].tag.kernel = TSTRG;
	    Make_Stack_String(((unsigned long *)lengths)[arg] , pw[arg+1].val, s);
	    Copy_Bytes(s, argbuf, ((unsigned long *)lengths)[arg]);
	    s[ ((unsigned long *)lengths)[arg] ] = '\0';
	    break;
	case TINT:
	    /* signed integer */
	    if (sql_type == prepared)
	    {
#if defined(HAVE_MYSQLBIGINT)
		/* may convert to ECLiPSe TBIG if required */
		tag_desc[TBIG].arith_op[ARITH_BOXLONGLONG](*(long_long *)argbuf, &pw[arg+1]);
#else
		Make_Integer( &pw[arg+1], *(word *)argbuf);
#endif
	    } else
	    {
#if defined(HAVE_MYSQLBIGINT) 
		long_long i;
#ifdef _WIN32
		if (sscanf(((MYSQL_ROW)buffer)[arg],"%I64d",&i) == 0) {
#else
		if (sscanf(((MYSQL_ROW)buffer)[arg],"%lld",&i) == 0) {
#endif
		    raise_dbi_error(DBI_TYPE_CONV); /* no integer read */
		    return -1;
		}

		/* may convert to ECLiPSe TBIG if required */
		tag_desc[TBIG].arith_op[ARITH_BOXLONGLONG](i, &pw[arg+1]);
#else
		word i;
		if (sscanf(((MYSQL_ROW)buffer)[arg],"%ld",&i) == 0) {
		    raise_dbi_error(DBI_TYPE_CONV);
		    return -1;
		}

		Make_Integer(&pw[arg+1], i);
#endif
	    }
	    break;
	case TDBL:
	    if (sql_type == prepared)
	    {
		Make_Float( &pw[arg+1], *(double *)argbuf);
	    } else
	    {
		double f; 
		if (sscanf(((MYSQL_ROW)buffer)[arg],"%lf",&f) == 0) {
		    raise_dbi_error(DBI_TYPE_CONV);
		    return -1;
		}

		Make_Float(&pw[arg+1], f);
	    }

	    break;
/* no longer supported
	case TFLOAT:
	    d = *(float *)argbuf; 
	    Make_Float( &pw[arg+1], d);
	    break;
*/
	default:
	    /* RAW -- check for dbformat header, assume rest is
               in dbformat 
	    */
	    for (i=0; i<DBF_HEADER_LEN; i++)
	    {
		if (argbuf[i] != dbformat_header[i])
		{
		    TG = pw;
		    Bip_Error(TYPE_ERROR);
		}
	    }
	    res = dbformat_to_term( argbuf+DBF_HEADER_LEN, 0, tdict);
	    if (NULL == res)
	    {
		/* bad error but probably won't happen since, if the
		 * string is bad we will probably overflow
		 */
		TG = pw;
		Bip_Error(TYPE_ERROR);
	    } 
/*	    if(IsMut(res->tag))
	    {
		Make_Ref( &(pw[arg+1]) , res);
	    }
	    else */
	    {
		pw[arg+1] = *res;
	    }
	    break;
	}
    }

    Succeed;
}
    
#define BindLong(lbuf, buf, max, start, len)\
	{\
	    *(lbuf) = (len);\
	    if ( *(lbuf) > (max) ) {\
	    	raise_dbi_error(DBI_BUFFER_OVER);\
                return -1;\
            }\
	    Copy_Bytes((buf),(start),(len));\
	}

/* based on BindLong, the header for the dbformat is placed at the start
   of the buffer before the dbformatted string is copied
*/
#define BindDbFormat(lbuf, buf, max, start, len) \
	{\
	    *(lbuf) = (len)+DBF_HEADER_LEN;\
	    if ( *(lbuf) > (max) ) {\
	    	raise_dbi_error(DBI_BUFFER_OVER);\
                return -1;\
            }\
	    Copy_Bytes((buf),dbformat_header,DBF_HEADER_LEN);\
	    Copy_Bytes((buf)+DBF_HEADER_LEN,(start),(len));\
	}

/* bind the actual data supplied in Prolog structure tuple to the buffers in 
   template (used for input (param) templates)
*/
int
template_bind(int tuple_num, template_t * template,char * buffer,void * lengths,pword * tuple) 
{
    word i;
    word j;
    pword * arg;
    value argval;
    map_t * m;
    char * argbuf;
    unsigned long * largbuf;
    extern pword * term_to_dbformat(pword *,dident);

#ifdef DEBUG
    fprintf(stderr,"tuple_num=%d template=0x%x buffer=0x%x tuple=0x%x\n",
            tuple_num, template, buffer, tuple);
#endif

    /* if there is no template */
    if (IsNil(tuple->tag) && template == NULL) { Succeed; }

    Check_Structure(tuple->tag)
    if (template->did != tuple->val.ptr->val.did)
	Bip_Error(TYPE_ERROR);

    for (i = 0 ; i < template->arity ; i++)
    {
	m = &(template->map[i]);
	largbuf = &(((unsigned long *)lengths)[i]);
	arg = tuple->val.ptr + i + 1;
#ifdef DEBUG
	fprintf(stderr,"m=0x%x arg=0x%x\n",m,arg);
#endif
	Dereference_(arg);
	if (IsRef(arg->tag))
	{
	    m->is_null = 1;
	    continue;
	}
	m->is_null = 0;

	argbuf  = buffer + m->offset  + tuple_num * m->increment;
	*largbuf = m->size; /* maximum size (buffer size) */

	if (m->prolog_tag && (TagType(arg->tag) != m->prolog_tag))
	{
#ifdef DEBUG
	    fprintf(stderr,"DEBUG m tag %d arg tag %d\n",
	    	m->prolog_tag, TagType(arg->tag));
#endif
#if defined(HAVE_LONG_LONG) && SIZEOF_LONG == 4
	    /* allow TBIG for integers between 32 and 64 bits */
	    if (!(TagType(arg->tag) == TBIG && m->prolog_tag == TINT))
#endif 
		Bip_Error(TYPE_ERROR);
	}
	switch(m->prolog_tag)
	{
	case TDICT:
	    BindLong(largbuf, argbuf, m->size,
	    	DidName(arg->val.did), DidLength(arg->val.did));
#ifdef DEBUG
	    for (j=0 ; j < *largbuf ; j++)
		    fprintf(stderr,"%02x ",argbuf[j]);
	    fprintf(stderr,"\n");
#endif
	    break;
	case TSTRG:
	    /*
	     * Build up a LONG VARCHAR which consists of a 32 bit
	     * string length + chars in non null terminated string
	     */
	    BindLong(largbuf, argbuf, m->size,
	    	StringStart(arg->val), StringLength(arg->val)); 
#ifdef DEBUG
	    for (j=0 ; j < *largbuf ; j++)
		    fprintf(stderr,"%02x ",argbuf[j]);
	    fprintf(stderr,"\n");
#endif
	    break;
	case TINT:
#ifdef DEBUG
	    fprintf(stderr,"DEBUG bind int argbuf 0x%x int %d\n",
	    				argbuf, arg->val.nint);
#endif
#if defined(HAVE_MYSQLLONGLONG) && SIZEOF_LONG == 4
	    if (TagType(arg->tag) == TBIG)
	    {/* convert TBIG integers of between 33 and 64 bits to long long */
		int res;
		res = tag_desc[TBIG].arith_op[ARITH_TOCLONGLONG](arg->val.ptr, argbuf);
		if (res != PSUCCEED) Bip_Error(res);
		break;
	    } else
#endif
	    /* BUFINT sized signed integer */
	    *(BUFINT *) argbuf  =  (BUFINT) arg->val.nint;
	    break;
	case TDBL:
	    *(double *) argbuf  =  Dbl(arg->val);
	    break;
	default:
	    {
	    	pword * old_tg = TG;
		pword * ext;

		ext = term_to_dbformat(arg,NULL);
		if (NULL == ext)
		    Bip_Error(TYPE_ERROR);
		BindDbFormat(largbuf, argbuf, m->size,
		    (char *) BufferStart(ext), BufferSize(ext)); 
	    	TG = old_tg;
	    }
	}
    }
    Succeed;
}

/* ----------------------------------------------------------------------
 *  Stubs
 * ---------------------------------------------------------------------- */
void 
session_init(session_t ** session)
{
	session_t * s;

	*session = NULL; 

	if (!(s = (session_t *) malloc(sizeof(session_t))))
        {
	    raise_dbi_error(DBI_BAD_SESSION);
	    return;
	}
	memset(s, 0, sizeof(session_t));
	s->mysql = mysql_init(NULL);

	if (s->mysql == NULL) /* init failed */
	{/* cannot raise mysql error here -- no mysql handle! */
	    raise_dbi_error(DBI_BAD_SESSION);
	    free(s);
	    return;
	}

	NoErrors;
	*session = s;
	return;

}

int
session_start(session_t * s, char * username, char * host, char * password, value v_opts)
{
	char * dbname = NULL;
	pword * optarg;
	char * engine = NULL;
	dident optdid = v_opts.ptr->val.did;

	if (host[0] == '\0') host = NULL; /* emptry string -> no hostname */

	/* processing options */
	if (strcmp(DidName(optdid), "options") != 0) 
	{
	    raise_dbi_error(DBI_BAD_FIELD);
	    return -1;
	}

	optarg = v_opts.ptr+SESSION_OPT_DBNAME;
	Dereference_(optarg);
	if (!IsRef(optarg->tag)) 
	{
	    switch (TagType(optarg->tag))
	    {
	    case TSTRG:
		dbname = StringStart(optarg->val);
		break;
	    case TDICT:
		dbname = DidName(optarg->val.did);
		break;
	    default:
		raise_dbi_error(DBI_BAD_FIELD);
		return -1; /* incompatible type */
		break;
	    }
	}

	optarg = v_opts.ptr+SESSION_OPT_STORAGE;
	Dereference_(optarg);
	if (!IsRef(optarg->tag)) 
	{
	    char * trans_type;
	    switch (TagType(optarg->tag))
	    {
	    case TDICT:
		trans_type = DidName(optarg->val.did);
		break;
	    case TSTRG:
		trans_type = StringStart(optarg->val);
		break;
	    default:
		raise_dbi_error(DBI_BAD_FIELD);
		return -1;
	    }
	    if (strcmp(trans_type, "transactional") == 0)
	    {
		engine = "innodb";
	    }
	    else if (strcmp(trans_type, "nontransactional") == 0)
	    {
		engine = "myisam";
	    }
	}

	if (!mysql_real_connect(s->mysql, host, username, password, dbname, 0, NULL, CLIENT_MULTI_STATEMENTS))
	{
	    raise_mysql_error(s->mysql);
	    return -1;
	}
	mysql_autocommit(s->mysql, 0);
	/* select @@storage_engine */
	if (engine) 
	{
	    char sql_set[50] = "set @@storage_engine=";

	    strcat(sql_set, engine);
	    if (!mysql_query(s->mysql, sql_set))
	    {
		raise_mysql_error(s->mysql);
		return -1;
	    }
	}

	NoErrors;
	return 0;
}

void
session_error_value( session_t * session, int * code, char ** msg)
{
	*code = err_code;
	*msg =  (char*) err_msg;
	
}

int
session_commit(session_t * session)
{

	if (mysql_commit(session->mysql))
	{
	    raise_mysql_error(session->mysql);
	    return -1;
	}

	return 0;
}

int
session_rollback(session_t * session)
{
	if (mysql_rollback(session->mysql))
	{
	    raise_mysql_error(session->mysql);
	    return -1;
	}

	return 0;
}

/* initialise and prepare a cursor (no bindings) */
cursor_t *
session_sql_prepare(session_t * session, char * SQL, word length, char use_prepared)
{
	cursor_t * cursor;


	if (!(cursor = (cursor_t *) malloc(sizeof(cursor_t))))
        {
	    raise_dbi_error(DBI_MEMORY);
	    return NULL;
	}
	memset(cursor, 0, sizeof(cursor_t));

	cursor->session = session;
	cursor->state = closed;
        cursor->sql_type = (use_prepared ? prepared : direct);  

	if (use_prepared)
	{/* prepared statement */
	    cursor->sql_length = 0;
	    cursor->s.stmt = mysql_stmt_init(session->mysql);
	    if (cursor->s.stmt == NULL)
	    {
		raise_mysql_error(session->mysql);
		free(cursor);
		return NULL;
	    }

	    session->refs++; /* incremented before cursor_free() can be called */
	    if (mysql_stmt_prepare(cursor->s.stmt, SQL, (unsigned long)length) != 0)
	    {
		raise_mysql_stmt_error(cursor->s.stmt);
		cursor_free(cursor);
		return NULL;
	    }
	} else
	{/* interprete SQL as string */
	    cursor->sql_length = length;
	    if (!(cursor->s.sql = (char *)malloc(length)))
	    {
		raise_dbi_error(DBI_MEMORY);
		free(cursor);
		return NULL;
	    }
	    memcpy(cursor->s.sql, SQL, length);
	    session->refs++;
	}

	cursor->state = opened;
	
	NoErrors;
	return cursor;
}

/* ready the cursor for the SQL statement SQL for both prepared and direct
   statements. Does not execute or bind the parameters for the statement
*/
cursor_t *
ready_session_sql_cursor(session_t *session, template_t *params, template_t *query, 
		  char *SQL, word length, word N, char use_prepared)
{
    cursor_t * cursor;
    MYSQL_STMT * c;
    MYSQL_BIND * bind = NULL;
    MYSQL_RES *resdata = NULL;
    MYSQL_FIELD *field;
    char * b;
    map_t * m;
    word free_off;
    word size;
    unsigned long *tlengths;
    my_bool * errors;
#ifdef DEBUG
    int * mytype;
#endif

    /* params != NULL only for prepared statements which have (input) parameters */
    if (params)
    	cursor = session_sql_prep(session, params, SQL, length, 1);
    else
	cursor = session_sql_prepare(session,SQL,length,use_prepared);

    if (NULL == cursor)
	return NULL;

    if (query == NULL)
    {/* query template cannot be NULL for a query */
	raise_dbi_error(DBI_NOT_QUERY);
	return NULL;
    }

    cursor->tuple_template = query;
    query->tuples = N;
    query->from = 0;
    query->to = 0;

    if (!use_prepared) return cursor;

    /* prepared statement only from here on */
    c = cursor->s.stmt;

    free_off = 0;

    resdata = mysql_stmt_result_metadata(c);
    /* for prepared statement, we can catch the mismatch between template
       and actual result columns early here. For direct statements, this
       can only be done after the statement is executed
    */
    if (mysql_num_fields(resdata) != query->arity)
    {
	TryFree(bind);
	TryFree(resdata);
	raise_dbi_error(DBI_BAD_TEMPLATE);
	return NULL;
    }

    if (query->arity == 0) return cursor;

    if (!(bind = (MYSQL_BIND *)malloc(sizeof(MYSQL_BIND)*query->arity)))
    {
	raise_dbi_error(DBI_MEMORY);
	return NULL;
    }
    memset(bind, 0, sizeof(MYSQL_BIND)*query->arity);
    if (!(tlengths = (unsigned long *) malloc(query->arity*sizeof(unsigned long))))
    {
	raise_dbi_error(DBI_MEMORY);
	return NULL;
    }
    if (!(errors = (my_bool *) malloc(query->arity*sizeof(my_bool))))
    {
	raise_dbi_error(DBI_MEMORY);
	return NULL;
    }

    unsigned int i;
    for(i=0 ; i < query->arity ; i++)
    {
	m = &(query->map[i]);
	if (!(field = mysql_fetch_field_direct(resdata, i)))
	{
#ifdef DEBUG
		fprintf(stderr,"DEBUG error fetch field i=%d\n",i);
#endif
		goto mysql_error;
	}
	m->offset = free_off;
	m->dbtype = field->type;

	/*
	 * Make sure sizes are OK.
	 */
	switch(field->type)
	{
	case MYSQL_TYPE_TINY:
	case MYSQL_TYPE_SHORT:
	case MYSQL_TYPE_LONG:
	case MYSQL_TYPE_INT24:
	case MYSQL_TYPE_LONGLONG:
	case MYSQL_TYPE_FLOAT:
	case MYSQL_TYPE_DOUBLE:
	case MYSQL_TYPE_YEAR:
	case MYSQL_TYPE_SET:
	case MYSQL_TYPE_ENUM:
	case MYSQL_TYPE_NULL:
	case MYSQL_TYPE_BIT:
	case MYSQL_TYPE_DECIMAL:
	case MYSQL_TYPE_NEWDECIMAL:
	    /*
	     * These are types whose string representation will never be
	     * very long so we can save a bit of buffer space in 
	     * these cases
	     */
	    if (m->size > 32) m->size = 32;
	    break;
	case MYSQL_TYPE_TIME:
	case MYSQL_TYPE_DATE:
	case MYSQL_TYPE_DATETIME:
	case MYSQL_TYPE_TIMESTAMP:
	    /* these are types which may require slightly more buffer space */
	    if (m->size > 255) m->size = 255;
	    break;
	case MYSQL_TYPE_STRING:
	    /* fixed length string, get the length (+1 for \0) */
	    if (m->prolog_tag != TSTRG && m->prolog_tag != TDICT) 
		goto conversion_error;

	    m->size = field->length+1;
	    break;

	/* last cases: no clue from DB type as to what size  is needed */
	case MYSQL_TYPE_VAR_STRING: /* VARCHAR, BINARY data */
	    if (m->prolog_tag != TSTRG && m->prolog_tag != TDICT) 
		goto conversion_error;
	    break;
	case MYSQL_TYPE_BLOB:
	    if (m->prolog_tag != 0) goto conversion_error;
	    break;
/* unsupported types - these should not occur in prepared statements
	case MYSQL_TYPE_GEOMETRY:
*/
	default:
	    goto conversion_error;
    	}

	m->increment = m->size;
	free_off += N * m->size;
    }

    if (!(b = (char *)malloc(free_off)))
    {
	raise_dbi_error(DBI_MEMORY);
	return NULL;
    }
#ifdef DEBUG
    fprintf(stderr,"DEBUG buffer = 0x%x\n",b);
#endif
    cursor->tuple_buffer = b;
    cursor->tuple_datalengths = tlengths;
    cursor->tuple_errors = errors;

    for(i=0 ; i < query->arity ; i++)
    {
	m = &(query->map[i]);
	bind[i].buffer_type = m->ext_type;
	bind[i].buffer = (char *)b+m->offset;
	bind[i].buffer_length = m->size;
	bind[i].length = (unsigned long *) &(tlengths[i]);
	bind[i].is_null = &(m->is_null);
	m->is_null = 0;
	bind[i].error = &(errors[i]);
#ifdef DEBUG
    for(i=0 ; i < query->arity ; i++)
	fprintf(stderr,"DEBUG bind[%2d]\toff=%8d,size=%8d,ext_type=%8d,dbtype=%8d\n",
		i,
		query->map[i].offset,
		bind[i].length,
	        bind[i].buffer_type,
		query->map[i].dbtype);

    fprintf(stderr,"DEBUG buffer size = %d\n",free_off);
#endif
    }
    if (mysql_stmt_bind_result(cursor->s.stmt, bind)) 
    {
	goto mysql_error;
    }
    TryFree(bind);
    TryFree(resdata);
    return cursor;

mysql_error:
    TryFree(bind);
    TryFree(resdata);
    raise_mysql_error(session->mysql);
    cursor_free(cursor);
    return NULL;

conversion_error:
    TryFree(bind);
    TryFree(resdata);
    raise_dbi_error(DBI_TYPE_CONV);
    cursor_free(cursor);
    return NULL;

}


/* prepare the param template's data buffer, and bind them to the DB */
cursor_t *
session_sql_prep(session_t *session,
		template_t *template, char *SQL, word length, word N)
{
    cursor_t * cursor;
    word i,j;
    map_t * m;
    word free_off, err;
    char * b;
    MYSQL_BIND *bind = NULL;

    cursor = session_sql_prepare(session,SQL,length,1);
    if (NULL == cursor)
	return NULL;

    cursor->param_template = template;
    cursor->tuple_template = NULL;
    if (template == NULL) return cursor;

    template->tuples = N;
    template->from = 0;
    template->to = 0;

    free_off = 0;

    if (template->arity != mysql_stmt_param_count(cursor->s.stmt))
    {
	raise_dbi_error(DBI_BAD_TEMPLATE);
	return NULL;
    }

    for(i=0 ; i < template->arity ; i++)
    {
	unsigned long size = DEFAULT_BUFFER_SIZE;

	m = &(template->map[i]);
	
	m->offset = free_off;


	free_off += m->size * N;
	m->increment = m->size;
/*	m->loffset = free_off;*/
	free_off += sizeof(word) * N;

	m->increment = m->size;
	/*	m->lincrement = sizeof(word);*/
    }

    if (!(cursor->param_buffer = (char *)malloc(free_off)))
    {
	raise_dbi_error(DBI_MEMORY);
	return NULL;
    }
    b = cursor->param_buffer;


    if (!(bind = (MYSQL_BIND *)malloc(sizeof(MYSQL_BIND)*template->arity)))
    {
	raise_dbi_error(DBI_MEMORY);
	return NULL;
    }
    memset(bind, 0, sizeof(MYSQL_BIND)*template->arity);

    if (!(cursor->param_datalengths = (unsigned long *) malloc(template->arity*sizeof(unsigned long))))
    {
	raise_dbi_error(DBI_MEMORY);
	return NULL;
    }
    for(i=0 ; i < template->arity ; i++)
    {
	m = &(template->map[i]);

	bind[i].length = &(cursor->param_datalengths[i]);
	bind[i].buffer_type = m->ext_type;
	bind[i].buffer = (char *) b + m->offset;
	bind[i].buffer_length = m->size;
	bind[i].is_null = &(m->is_null);
    }

    if (err=mysql_stmt_bind_param(cursor->s.stmt, bind))
    {
	TryFree(bind);
	raise_mysql_stmt_error(cursor->s.stmt);
	cursor_free(cursor);
	return NULL;
    }

#ifdef DEBUG
    fprintf(stderr,"DEBUG buffer = 0x%x\n",cursor->param_buffer);
#endif
    return cursor;
}

int 
session_tostr(session_t * session, char *buf, int quoted)
{
    sprintf(buf, "'MySQLS'(16'%x)", (word) session);
    return strlen(buf); /* size of actual string */
}

void
session_close(session_t * session)
{
    mysql_close(session->mysql);
    session->mysql = NULL;
    session->closed = 1;
    return;
}

/* sets MySQL specific options for cursor. opts must be a structure */
int
cursor_set_options(cursor_t * cursor, value v_opts)
{
    pword * optarg;
    char * option;
    dident optdid = v_opts.ptr->val.did; 

    /* processing options */
    if (strcmp(DidName(optdid), "options") != 0) 
    {
	raise_dbi_error(DBI_BAD_FIELD);
	return -1;
    }

    optarg = v_opts.ptr+CURSOR_OPT_BUFFER;
    Dereference_(optarg);
    switch (TagType(optarg->tag))
    {
    case TSTRG:
	option = StringStart(optarg->val);
	break;
    case TDICT:
	option = DidName(optarg->val.did);
	break;
    default:
	raise_dbi_error(DBI_BAD_FIELD);
	return -1;
	break;
    }
    cursor->server_cursor = (strcmp(option, "server") == 0 ? 1 : 0);

    optarg = v_opts.ptr+CURSOR_OPT_TYPE;
    Dereference_(optarg);
    switch (TagType(optarg->tag))
    {
    case TSTRG:
	option = StringStart(optarg->val);
	break;
    case TDICT:
	option = DidName(optarg->val.did);
	break;
    default:
	raise_dbi_error(DBI_BAD_FIELD);
	return -1;
	break;
    }
    if (strcmp(option, "no_cursor") == 0)
    {
	cursor->cursor_type = CURSOR_NO_CURSOR;
    } else if (strcmp(option, "read_only") == 0)
    {
	cursor->cursor_type = CURSOR_READ_ONLY;
    } else
    {
	raise_dbi_error(DBI_BAD_FIELD);
	return -1;
    }
    return 0;
}

/* executes the statement in cursor. For prepared statements, input params
   must already be bound
*/
int
cursor_sql_execute(cursor_t * cursor, int with_defaults)
{
    unsigned int nfield = 0;
#ifdef DEBUG
    fprintf(stderr,"cursor_sql_execute\n");
#endif

    if (cursor->session->closed ||
	cursor->state == closed ||
	cursor->state == idle 
	)
    {
	raise_dbi_error(DBI_BAD_CURSOR);
	return -1;
    }

    if (with_defaults)
    {
	cursor->server_cursor = 0;
	cursor->cursor_type = CURSOR_NO_CURSOR;
    }

    if (cursor->sql_type == prepared)
    {/* prepared statememnt */
	unsigned long type;

	/* free any previous results */
	if (cursor->state == executed) mysql_stmt_free_result(cursor->s.stmt);
	if (cursor->cursor_type == CURSOR_READ_ONLY)
	    type  = CURSOR_TYPE_READ_ONLY;
	else
	    type = CURSOR_TYPE_NO_CURSOR;
	mysql_stmt_attr_set(cursor->s.stmt, STMT_ATTR_CURSOR_TYPE, (void*) &type);
	if (mysql_stmt_execute(cursor->s.stmt))
	{
	    raise_mysql_stmt_error(cursor->s.stmt);
	    return -1;
	}
	cursor->prolog_processed_count = (word) mysql_stmt_affected_rows(cursor->s.stmt);
	if (!cursor->server_cursor && /* store result on client side */
	    mysql_stmt_store_result(cursor->s.stmt))
	{
	    raise_mysql_stmt_error(cursor->s.stmt);
	    return -1;
	}
	nfield = mysql_stmt_field_count(cursor->s.stmt);
    } else /* if cursor->sql_type == direct */
    {
	if (cursor->state != opened)
	{/* direct statements can only be executed once */
	    raise_dbi_error(DBI_NOT_PREPARED);
	    return -1;
	}

	if (mysql_real_query(cursor->session->mysql, cursor->s.sql, cursor->sql_length))
	{
	    raise_mysql_error(cursor->session->mysql);
	    TryFree(cursor->s.sql);
	    return -1;
	}
	TryFree(cursor->s.sql);
	cursor->prolog_processed_count = (word) mysql_affected_rows(cursor->session->mysql);
	if (cursor->server_cursor)
	{/* result on server side */
	    cursor->s.res = mysql_use_result(cursor->session->mysql);	
	} else
	{/* result on client side */
	    cursor->s.res = mysql_store_result(cursor->session->mysql);
	}
	if (cursor->s.res == NULL)
	{
	    nfield = mysql_field_count(cursor->session->mysql);
	    if (nfield != 0)
	    {/* error - should have returned results */
		raise_mysql_error(cursor->session->mysql);
		return -1;
	    }
	} else
	{
	    nfield = mysql_num_fields(cursor->s.res);
	}
    }
    if (cursor->tuple_template == NULL)
    {/* no tuple template, should have no output columns */
	if (nfield != 0) {
	    raise_dbi_error(DBI_BAD_TEMPLATE);
	    return -1;
	}
    } 
    else if (cursor->tuple_template->arity != nfield)
    {
	raise_dbi_error(DBI_BAD_TEMPLATE);
	return -1;
    }
    
    cursor->state = executed;
    return 0;
}


/* MySQL returns only 1 tuple at a time */
int
cursor_one_tuple(cursor_t *cursor)
{
    template_t * t;
    void * b;
    int err;

    if (cursor->session->closed)
    {
	raise_dbi_error(DBI_BAD_CURSOR);
	return -1;
    }

    if (! cursor->tuple_template)
    {
	raise_dbi_error(DBI_NOT_QUERY);
	return -1;
    }

    t = cursor->tuple_template;
    b = cursor->tuple_buffer;

#ifdef DEBUG
/*    fprintf(stderr,"DEBUG: oexfet rpc = %d from = %d to = %d\n",
      cursor->cda.rpc, t->from, t->to);*/
    fprintf(stderr,"DEBUG: state = %d\n",cursor->state);
#endif

    if (cursor->state ==  nodata)
    {
	cursor->state = idle;
	return 0;
    }

    if (cursor->state ==  idle)
    {
	/*
	 * Application is trying to read on after it has already
	 * been told the cursor is not giving any more data
	 * or it has, itself cancelled the cursor
	 */
	raise_dbi_error(DBI_CANCELLED);
	return -1;
    }	
    
    if (cursor->state == opened) 
    {
	if (cursor->param_template != NULL)
	{/* has parameters, but these are not yet bound to actual values */
	    raise_dbi_error(DBI_NO_PARAM);
	    return -1;
	}
	if (err = cursor_sql_execute(cursor, 1)) return err;
    }
    
    /* using from..to is for compatibility with Oracle oci */
    t->to = t->from; 
    if (cursor->state == executed) /* parameters set or */
    {
#ifdef DEBUG
	fprintf(stderr,"DEBUG: tuples = %d\n",t->tuples);
#endif
	if (cursor->sql_type == prepared)
	{
	    switch (mysql_stmt_fetch(cursor->s.stmt))
	    {
	    case MYSQL_NO_DATA:
		mysql_stmt_free_result(cursor->s.stmt);
		cursor->state = nodata;
		return 0;
		break;
	    case 1: /* error */
		raise_mysql_stmt_error(cursor->s.stmt);
		return -1;
		break;
	    case MYSQL_DATA_TRUNCATED: 
		/* truncated  - cinversion problems or buffer(s) too small */
		raise_dbi_error(DBI_DATA_TRUNC);
		return -1;
		break;
	    /* otherwise, there is no problem */
	    }

	} else
	{
	    /* note the results and lengths are stored in the s.res structure.
               this is freed when the result is freed
	    */
	    if (cursor->s.res == NULL)
	    {/* no result to return */
		cursor->state = nodata;
		return 0;
	    }
	    if ((cursor->tuple_buffer  = (void *) mysql_fetch_row(cursor->s.res)) == NULL)
	    {
		mysql_free_result(cursor->s.res);
		cursor->tuple_datalengths = NULL;
		cursor->tuple_errors = NULL;
		cursor->state = nodata;
		return 0;
	    }
	    cursor->tuple_datalengths = mysql_fetch_lengths(cursor->s.res);
	}
    }

    else
    {
	raise_dbi_error(DBI_BAD_CURSOR);
	return -1;
    }
    t->to = t->from + 1;


#ifdef DEBUG
/*    fprintf(stderr,"DEBUG: oexfet rpc = %d from = %d to = %d\n",
      cursor->cda.rpc, t->from, t->to);*/
#endif

    return 0;
}

int
cursor_N_tuples(cursor_t * cursor, word * n, pword * tuple_listp, pword ** tp)
{
    pword * head;
    template_t * template; 
    int res;

    template = cursor->tuple_template;

    *tp = tuple_listp;
    for (*n=0; ; (*n)++)
    {
	/*
	 * At this point *cdr is a pointer to the cell being created
	 * in this loop. I.e the cdr of the previous cell or the whole
	 * list
	 */

	if (cursor->state == nodata) break; /* exhausted data previously, just return */
	if (cursor_one_tuple(cursor) == -1) return -1; /* error has occurred */
	if (template->to == template->from) break; /* reached end */

	/* new cons cell */
	head = TG;
	Push_List_Frame();
	Make_List(*tp, head);
	if (res = template_put(*n, template, cursor->sql_type,
		cursor->tuple_buffer, cursor->tuple_datalengths, head))
	{
	    return res;
	}
	*tp = head + 1;
    }
    Make_Var(*tp);
    template->from = template->to;

    return 0;
}

int 
cursor_N_execute(cursor_t * cursor, word * tuplep, value v_tuples, type t_tuples, pword ** cdrp)
{
    int res;
    pword * car; 


    for (	*tuplep = 0 ;
    		IsList(t_tuples) ;
		(*tuplep)++)
    {
	car = v_tuples.ptr;
	*cdrp = car + 1;
	Dereference_(car);               /* access the data */

	if (res = template_bind(0, cursor->param_template,
		       cursor->param_buffer, cursor->param_datalengths, car))
	{
#ifdef DEBUG
		fprintf(stderr,"DEBUG tuple=%d\n",tuplep);
#endif
		return res;
	}

	if (cursor_sql_execute(cursor, 1))
	{
	    return -1;
	}

	Dereference_((*cdrp));       /* proceed to next element */
	t_tuples = (*cdrp)->tag;
	v_tuples = (*cdrp)->val;
    }


    return 0;
}

int 
cursor_tostr(cursor_t * cursor, char *buf, int quoted)
{
    sprintf(buf, "'MySQLC'(16'%x)", (word) cursor);
    return strlen(buf); /* size of actual string */
}

void
cursor_free(cursor_t * cursor)
{
	session_t * s;

	if (cursor == NULL) return;
	s = cursor->session;

	if (cursor->state != closed && ! s->closed )
	{

	    if (cursor->sql_type == prepared)
	    {
		/* mysql_stmt_free_result() need not be called --
                   results cancelled automatically
		*/
		if (mysql_stmt_close(cursor->s.stmt)!= 0 )
		{
		    raise_mysql_stmt_error(cursor->s.stmt);
		    /* we can't return an error here because the copy method
                       returns void. Just print the error and return
		    */
		    fprintf(stderr, "MySQl error freeing cursor: %d %s\n", err_code,err_msg);
		    NoErrors; 
		    return;
		} 
	    } else 
	    { /* direct SQL */
		if (cursor->state == executed)
		{
		    mysql_free_result(cursor->s.res);
		    /* freeing the results free the tuple + tuple length
                       buffers as well */
		    cursor->tuple_buffer = NULL;
		    cursor->tuple_datalengths = NULL;
		} else if (cursor->state == opened)
		{
		    TryFree(cursor->s.sql);
		}
	    }
	}

	/*
	 * free extended cursor
	 */

#ifdef DEBUG
	fprintf(stderr,"cursor free\n");
#endif
	if (cursor->param_template) {
	    TryFree(cursor->param_template->map);
	}
	TryFree(cursor->param_template);
	if (cursor->tuple_template) {
	    TryFree(cursor->tuple_template->map);
	}
	TryFree(cursor->tuple_template);
	TryFree(cursor->param_buffer);
	TryFree(cursor->tuple_errors);
	TryFree(cursor->param_datalengths);
	if (cursor->sql_type == prepared)
	{/* these are malloc'ed only for prepared statements. 
	    If session has already been freed, then tuple_buffer etc.
            would be freed by MySQL already, even though they still have
            non-zero values
	 */
	    TryFree(cursor->tuple_buffer);
	    TryFree(cursor->tuple_datalengths);
	}

	free(cursor);

	/*
	 * update session cursor count
	 */
	if (--(s->refs) == 0)
	{
	    if (s->closed) free(s);
	    else 
	    {
		/* this should not happen */
		fprintf(stderr, "Dbi error: problem with session handle reference count. Please report problem.\n");
		free(s);
	    }
	}
}

int
cursor_field_value(cursor_t * cursor, field_t field,void ** value)
{
    if (cursor->session->closed)
    {
	raise_dbi_error(DBI_BAD_CURSOR);
	return -1;
    }
    switch(field)
    {
    case state :
	*value = &(cursor->state);
    	break;
    case rows_processed_count :
	/* A coursor has 2 templates now so no 1 processed count.
	if (cursor->template)
	cursor->prolog_processed_count +=
		cursor->template->to - cursor->template->from;
	*/
	*value = &(cursor->prolog_processed_count);
    	break;
    case return_code :
    	*value = &err_code;
    	break;
    case return_code_as_string :
    	*value = &err_msg;
    	break;
    default :
	raise_dbi_error(DBI_BAD_FIELD);
    	return -1;
    }

    return 0;
}

/* these initilization and finalization calls does not seem to be strictly
   needed, and were introduced only in MySQL 5.0.3, but they are recommended
   in the manual
*/
void
dbi_init()
{
    mysql_library_init(-1, NULL, NULL);
}

void
dbi_final()
{
    mysql_library_end();
}
