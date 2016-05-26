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
 *
 * ECLiPSe LIBRARY MODULE
 *
 * $Header: /cvsroot/eclipse-clp/Eclipse/Oci/dbi.c,v 1.4 2007/07/03 20:42:47 kish_shen Exp $
 *
 *
 * IDENTIFICATION:	dbi.c
 *
 * AUTHOR:		Joachim Schimpf
 * AUTHOR:		Stefano Novello
 * AUTHOR:              Kish Shen
 *
 * DESCRIPTION:
 */

/*
 *
 * Contents:	Prolog wrappers around DB Interface
 *              taken from oci.c
 *
 * Author:	Stefano Novello
 * Author:      Kish Shen, Generalised and updated from original OCI code,
 *              intially for MySQL, Jan - Feb 2006.
 *
 *
 */

#include <stdio.h>
#include "external.h"	/* ECLiPSe definitions */
#include "dbi.h"	/* Oracle call interface */

#ifdef _WIN32
#define EXPORT __declspec(dllexport)
#else
#define EXPORT
#endif

static int dbi_errno = 0;

#define CURSOR_HANDLE   1 /* argument index for cursor handle in
                             the cursor handle structure. Must
                             correspond to the ECLiPSe level code
			  */
/* if a `dbi' error occur, the return code is -1; otherwise it is the bip error */
#define Error_Code(E) (E == -1 ? dbi_errno : E)
/* ----------------------------------------------------------------------
 *  Forward declarations
 * ---------------------------------------------------------------------- */
EXPORT int
p_session_init(
		/* - */ value v_session, type t_session
              );

EXPORT int
p_session_start(
                /* + */ value v_session, type t_session,
		/* + */ value v_username, type t_username,
		/* + */ value v_host, type t_host,
		/* + */ value v_password, type t_password,
		/* + */ value v_opts, type t_opts
		);

EXPORT int
p_session_close(
		/* + */ value v_session, type t_session
                );

EXPORT int
p_session_error_value(
		/* + */ value v_session, type t_session,
		/* + */ value v_code, type t_code,
		/* + */ value v_message, type t_message
		);

EXPORT int
p_session_commit(
		/* + */ value v_session, type t_session
		);

EXPORT int
p_session_rollback(
		/* + */ value v_session, type t_session
		);

EXPORT int
p_session_sql_dml(
		/* + */ value v_session, type t_session,
		/* + */ value v_SQL, type t_SQL,
		/* - */ value v_rows, type t_rows
		);

EXPORT int
p_session_sql_query(
		/* + */ value v_session, type t_session,
		/* + */ value v_template, type t_template,
		/* + */ value v_SQL, type t_SQL,
		/* + */ value v_N, type t_N,
		/* + */ value v_opts, type t_opts,
		/* - */ value v_cursor, type t_cursor
		);

EXPORT int
p_session_sql_prepare(
		/* + */ value v_session, type t_session,
		/* + */ value v_template, type t_template,
		/* + */ value v_SQL, type t_SQL,
		/* + */ value v_N, type t_N,
		/* - */ value v_cursor, type t_cursor
		);

EXPORT int
p_session_sql_prepare_query(
		/* + */ value v_session, type t_session,
		/* + */ value v_ptemplate, type t_ptemplate,
		/* + */ value v_qtemplate, type t_qtemplate,
		/* + */ value v_SQL, type t_SQL,
		/* + */ value v_N, type t_N,
		/* - */ value v_cursor, type t_cursor
		);

EXPORT int
p_session_is_in_transaction(
		/* + */ value v_session, type t_session
                );

EXPORT int
p_session_set_in_transaction(
                /* + */ value v_session, type t_session,
		/* + */ value v_in, type t_in
                );

EXPORT int
p_cursor_N_execute(
		/* + */ value v_cursor, type t_cursor,
		/* + */ value v_N, type t_N,
		/* + */ value v_tuples, type t_tuples,
		/* ? */ value v_tail, type t_tail
		);

EXPORT int
p_cursor_next_execute(
		/* + */ value v_cursor, type t_cursor,
		/* + */ value v_tuple, type t_tuple,
		/* + */ value v_opts, type t_opts
		);

EXPORT int
p_cursor_next_tuple(
		/* + */ value v_cursor, type t_cursor,
		/* - */ value v_tuple, type t_tuple
		);

EXPORT int
p_cursor_N_tuples(
		/* + */ value v_cursor, type t_cursor,
		/* + */ value v_N, type t_N,
		/* - */ value v_tuples, type t_tuples,
		/* ? */ value v_tail, type t_tail
		);

EXPORT int
p_cursor_free(
		/* + */ value v_cursorh, type t_cursorh
		);

EXPORT int
p_cursor_field_value(
		/* + */ value v_cursor, type t_cursor,
		/* + */ value v_item, type t_item,
		/* - */ value v_value, type t_value
		);

EXPORT int
p_handle_free_eagerly(value v_handle, type t_handle);


/* max. number of digits for a printed address - 2 digits per byte */
#define MAX_ADDRESS_DIGITS 2*sizeof(void *) 

#define CURSOR_STRSZ MAX_ADDRESS_DIGITS+20

/* define the strsz methods here -- assume at most an extra 20 characters
   will be used for the printed handle in addition to the address
*/
static int
cursor_strsz(cursor_t * cursor, int quoted)
{
    return CURSOR_STRSZ;
}


t_ext_type cursor_handle_tid = {
    (void (*)(t_ext_ptr)) cursor_free,  /* free */
    NULL,  /* copy */
    NULL,  /* mark_dids */
    (int (*)(t_ext_ptr,int)) cursor_strsz,  /* string_size */
    (int (*)(t_ext_ptr,char *,int)) cursor_tostr,  /* to_string */
    NULL,  /* equal */
    NULL,  /* remote_copy */
    NULL,  /* get */
    NULL   /* set */
};


#define SESSION_STRSZ MAX_ADDRESS_DIGITS+20

static int
session_strsz(session_t * session, int quoted)
{
    return CURSOR_STRSZ;
}

t_ext_type session_handle_tid = {
    (void (*)(t_ext_ptr)) session_free,  /* free */
    (t_ext_ptr (*)(t_ext_ptr)) session_copy,  /* copy */
    NULL,  /* mark_dids */
    (int (*)(t_ext_ptr,int)) session_strsz,  /* string_size */
    (int (*)(t_ext_ptr,char *,int)) session_tostr,  /* to_string */
    NULL,  /* equal */
    NULL,  /* remote_copy */
    NULL,  /* get */
    NULL   /* set */
};

/* ----------------------------------------------------------------------
 *  Initialization and finalization
 * ---------------------------------------------------------------------- */

EXPORT int
p_dbi_init(
		/* + */ value v_errno, type t_errno
		)
{
    static int initialized = 0;

    if (initialized) Succeed_;

    Check_Integer(t_errno);
    /*
     * In C error codes are negative while in Prolog they are positive !
     */
    dbi_errno = - v_errno.nint;

    dbi_init();  /* do any DB specific initialisation */
    initialized = 1;
    Succeed_;
}

EXPORT int
p_dbi_final()
{
    dbi_final(); /* do any DB specific finalization */
    Succeed_;
}


/* ----------------------------------------------------------------------
 *  handler support
 * ---------------------------------------------------------------------- */

static void
get_cursor_handlepw(cursor_t *cursor, pword * cursor_handle)
{
    session_t * session = cursor->session;

    *cursor_handle = ec_handle(&cursor_handle_tid, cursor);

}

void
session_free(session_t * session)
{

    if (session == NULL) return;

    if (--(session->refs) == 0)
    {
	if (!session->closed)
	{

#ifdef DEBUG
	    fprintf(stderr,"session close\n");
#endif

	    session_close(session); 
	}
	free(session);
    }
#ifdef DEBUG
    fprintf(stderr,"session free\n");
#endif

    return;
}

session_t *
session_copy(session_t * session)
{

    if (session == NULL) return NULL;

    session->refs++;

    return session;
}

/* ----------------------------------------------------------------------
 *  Prolog Interface
 * ---------------------------------------------------------------------- */

int
p_session_init(
		/* - */ value v_session, type t_session
		)
{
	session_t * session;
	pword p_session;

	session_init( &session);

	if (session == NULL)
	{
	    Bip_Error(dbi_errno);
	}
	session->refs = 1;
	p_session = ec_handle(&session_handle_tid, session);

	Return_Unify_Pw(v_session, t_session, p_session.val, p_session.tag );
}

int
p_session_start(
 	        /* + */ value v_session, type t_session,
		/* + */ value v_username, type t_username,
		/* + */ value v_host, type t_host,
		/* + */ value v_password, type t_password,
		/* + */ value v_opts, type t_opts
		)
{
        session_t * session;

	Get_Typed_Object(v_session,t_session,&session_handle_tid,session);

	Check_String(t_username);
	Check_String(t_host);
	Check_String(t_password);
	Check_Structure(t_opts);
		
	if ( session_start( session,
			    StringStart(v_username),
			    StringStart(v_host),
			    StringStart(v_password),
			    v_opts) )
	    Bip_Error(dbi_errno);

	Succeed;

}

int
p_session_close(value v_session, type t_session)
{
    pword handle;
    handle.val.all = v_session.all;
    handle.tag.all = t_session.all;
    session_t * session;

    Get_Typed_Object(v_session,t_session,&session_handle_tid,session);

    session_close(session);
    return ec_free_handle(handle, &session_handle_tid);
}


int
p_session_error_value(
		/* + */ value v_session, type t_session,
		/* + */ value v_code, type t_code,
		/* + */ value v_message, type t_message
		)
{
	int code;
	char * message;
	pword p;
	session_t * session;
	Prepare_Requests;

	Check_Output_Integer(t_code);
	Check_Output_String(t_message);
	Get_Typed_Object(v_session,t_session,&session_handle_tid,session);

	session_error_value(session, &code, &message);

	Make_String(&p,message);
	Request_Unify_Integer(v_code, t_code, code);
	Request_Unify_Pw(v_message, t_message, p.val, p.tag);
	Succeed;
}

int
p_session_commit(
		/* + */ value v_session, type t_session
		)
{
	session_t * session;

	Get_Typed_Object(v_session,t_session,&session_handle_tid,session);

	if (session_commit(session))
		Bip_Error(dbi_errno);

	Succeed;
}

int
p_session_rollback(
		/* + */ value v_session, type t_session
		)
{
	session_t * session;
	int res;

	Get_Typed_Object(v_session,t_session,&session_handle_tid,session);

	if (res = session_rollback(session))
	    Bip_Error(Error_Code(res));

	Succeed;
}


int
p_session_sql_dml(
		/* + */ value v_session, type t_session,
		/* + */ value v_SQL, type t_SQL,
		/* - */ value v_rows, type t_rows
		)
{
	session_t * session;
	cursor_t * cursor;
	char * SQL;
	word rows, *prows;
	int res;

	Check_String(t_SQL);
	Check_Output_Integer(t_rows);
	Get_Typed_Object(v_session,t_session,&session_handle_tid,session);


	cursor = session_sql_prepare(session, StringStart(v_SQL), StringLength(v_SQL), 0);
	if (NULL == cursor)
	    Bip_Error(dbi_errno);

	if (res = cursor_sql_execute(cursor, 1))
	{
	    cursor_free(cursor);
	    Bip_Error(Error_Code(res));
	}

	cursor_field_value(cursor, rows_processed_count, (void **)&prows);
	rows = *prows;

	cursor_free(cursor);

	Return_Unify_Integer(v_rows, t_rows, rows);
}

int
p_session_sql_query(
		/* + */ value v_session, type t_session,
		/* + */ value v_template, type t_template,
		/* + */ value v_SQL, type t_SQL,
		/* + */ value v_N, type t_N,
		/* + */ value v_opts, type t_opts,
		/* - */ value v_cursor, type t_cursor
		)
{
	session_t * session;
	pword p_cursor;
	char * SQL;
	template_t * template;
	cursor_t * cursor;
	int res;
	word N;

	Check_Integer(t_N);
	N = v_N.nint;
	Check_String(t_SQL);
	Check_Structure(t_opts);
	Get_Typed_Object(v_session,t_session,&session_handle_tid,session);

	if (res = template_get(v_template, t_template, &template))
	{
	    Bip_Error(Error_Code(res));
	}

	cursor = ready_session_sql_cursor(session, NULL, template, StringStart(v_SQL), StringLength(v_SQL),  N, 0);
	if (NULL == cursor ) {  Bip_Error(dbi_errno); }

	if (res = cursor_set_options(cursor, v_opts))
	{
	    Bip_Error(Error_Code(res));
	}
	
	if (res = cursor_sql_execute(cursor, 0)) 
	{
	    cursor_free(cursor);
	    Bip_Error(Error_Code(res));
	}


	get_cursor_handlepw(cursor, &p_cursor);
	Return_Unify_Pw(v_cursor, t_cursor, p_cursor.val, p_cursor.tag);
}

int
p_session_sql_prepare(
		/* + */ value v_session, type t_session,
		/* + */ value v_template, type t_template,
		/* + */ value v_SQL, type t_SQL,
		/* + */ value v_N, type t_N,
		/* - */ value v_cursor, type t_cursor
		)
{
	session_t * session;
	pword p_cursor;
	char * SQL;
	template_t * template;
	cursor_t * cursor;
	int res;
	word N;

	Check_Integer(t_N);
	N = v_N.nint;
	Check_String(t_SQL);
        Get_Typed_Object(v_session,t_session,&session_handle_tid,session);

	if (res = template_get(v_template, t_template, &template))
	{
	    Bip_Error(Error_Code(res));
	}
	cursor = session_sql_prep(session,
				template, StringStart(v_SQL), StringLength(v_SQL), N);
	if (NULL == cursor)
	{
	    Bip_Error(dbi_errno);
	}


	get_cursor_handlepw(cursor, &p_cursor);
	Return_Unify_Pw(v_cursor, t_cursor, p_cursor.val, p_cursor.tag);
}

int
p_session_sql_prepare_query(
		/* + */ value v_session, type t_session,
		/* + */ value v_ptemplate, type t_ptemplate,
		/* + */ value v_qtemplate, type t_qtemplate,
		/* + */ value v_SQL, type t_SQL,
		/* + */ value v_N, type t_N,
		/* - */ value v_cursor, type t_cursor
		)
{
	session_t * session;
	pword p_cursor;
	char * SQL;
	template_t *ptemplate, *qtemplate;
	cursor_t * cursor;
	int res;
	word N;

	Check_Integer(t_N);
	N = v_N.nint;
	Check_String(t_SQL);
	Get_Typed_Object(v_session,t_session,&session_handle_tid,session);

	if (res = template_get(v_ptemplate, t_ptemplate, &ptemplate))
	{
	    Bip_Error(Error_Code(res));
	}
	if (res = template_get(v_qtemplate, t_qtemplate, &qtemplate))
	{
	    Bip_Error(Error_Code(res));
	}

	cursor = ready_session_sql_cursor(session, ptemplate, qtemplate, 
		     StringStart(v_SQL), StringLength(v_SQL),N,1);
	if (NULL == cursor)
	{
	    Bip_Error(dbi_errno);
	}

	get_cursor_handlepw(cursor, &p_cursor);
	Return_Unify_Pw(v_cursor, t_cursor, p_cursor.val, p_cursor.tag);
}


int
p_session_is_in_transaction(
		/* + */ value v_session, type t_session
                )
{
	session_t * session;

	Get_Typed_Object(v_session,t_session,&session_handle_tid,session);
	if (session->in_transaction == 0) Fail;

	Succeed;
}


static void _dbi_reset_in_transaction ARGS((pword*,word*,int,int));

static void _dbi_reset_in_transaction(pword * pw, word * pdata, int size, int flags)
{
    session_t * session = ExternalData(pw->val.ptr);     

    if (session == NULL) return; /* stale handle */
    session->in_transaction = 0;
}


p_session_set_in_transaction(
                /* + */ value v_session, type t_session,
		/* + */ value v_in, type t_in
                )
{
    session_t * session;
    pword * pw;
    
    Check_Integer(t_in);
    Get_Typed_Object(v_session,t_session,&session_handle_tid,session);

    switch (v_in.nint)
    {
    case 1:
	if (session->in_transaction == 1) { Fail; }

	session->in_transaction = 1;

	ec_trail_undo(_dbi_reset_in_transaction, v_session.ptr, NULL, NULL, 0, 0);
	break;
    case 0:
	session->in_transaction = 0;
	break;
    default:
	Bip_Error(RANGE_ERROR);
	break;
    }
    Succeed;
}


int
p_cursor_next_execute(
		/* + */ value v_cursor, type t_cursor,
		/* + */ value v_tuple, type t_tuple,
		/* + */ value v_opts, type t_opts
		)
{
    cursor_t * cursor;
    int res;
    pword tuple, * argp;

    tuple.val = v_tuple;
    tuple.tag = t_tuple;
    Check_Structure(t_opts);
    Check_Structure(t_cursor);
    argp = &v_cursor.ptr[CURSOR_HANDLE];
    Dereference_(argp);
    Get_Typed_Object(argp->val, argp->tag,&cursor_handle_tid,cursor);

    if (res = cursor_set_options(cursor, v_opts))
    {
	Bip_Error(Error_Code(res));
    }

    if (res = template_bind(0, cursor->param_template,
		  cursor->param_buffer, cursor->param_datalengths,&tuple))
    {
	Bip_Error(Error_Code(res));
    }

    if (res = cursor_sql_execute(cursor, 0)) 
    {
	Bip_Error(Error_Code(res));
    }


    Succeed;
}

int
p_cursor_N_execute(
		/* + */ value v_cursor, type t_cursor,
		/* - */ value v_N, type t_N,
		/* + */ value v_tuples, type t_tuples,
		/* ? */ value v_tail, type t_tail
		)
{
    cursor_t * cursor;
    pword * car; pword * cdr, * argp;
    int res;
    word tuple;
    Prepare_Requests;

    Check_Output_Integer(t_N);
    Check_Structure(t_cursor);
    Check_Pair(t_tuples);
    argp = &v_cursor.ptr[CURSOR_HANDLE];
    Dereference_(argp);
    Get_Typed_Object(argp->val, argp->tag,&cursor_handle_tid,cursor);

    if (! cursor->param_template)
	    Bip_Error(TYPE_ERROR);

    if (res = cursor_N_execute(cursor, &tuple, v_tuples, t_tuples, &cdr))
    {
	Bip_Error(Error_Code(res));
    }

    Request_Unify_Integer(v_N, t_N, tuple );
    Request_Unify_Pw(v_tail, t_tail, cdr->val, cdr->tag);
    Return_Unify;
}

int
p_cursor_next_tuple(
		/* + */ value v_cursor, type t_cursor,
		/* - */ value v_tuple, type t_tuple
		)
{
    cursor_t * cursor;
    int res;
    template_t * t;
    pword p_tuple, * argp;

    Check_Structure(t_cursor);
    argp = &v_cursor.ptr[CURSOR_HANDLE];
    Dereference_(argp);
    Get_Typed_Object(argp->val, argp->tag,&cursor_handle_tid,cursor);

    if (res = cursor_one_tuple(cursor))
    {
	Bip_Error(Error_Code(res));
    }

    t = cursor->tuple_template;
    if (t->to == t->from)
	Fail;

    if (res = template_put(t->from , t, cursor->sql_type, cursor->tuple_buffer, cursor->tuple_datalengths, &p_tuple))
    {
	Bip_Error(Error_Code(res));
    }

    t->from++;
    Return_Unify_Pw(v_tuple, t_tuple, p_tuple.val, p_tuple.tag);

}


int
p_cursor_N_tuples(
		/* + */ value v_cursor, type t_cursor,
		/* - */ value v_N, type t_N,
		/* - */ value v_tuples, type t_tuples,
		/* ? */ value v_tail, type t_tail
		)
{
    cursor_t * cursor;
    word n;
    int res;
    pword tuple_list, * argp;
    pword * tail;
    Prepare_Requests;

    Check_Structure(t_cursor);
    argp = &v_cursor.ptr[CURSOR_HANDLE];
    Dereference_(argp);
    Get_Typed_Object(argp->val, argp->tag,&cursor_handle_tid,cursor);

    Check_Output_Integer(t_N);
    Check_Output_List(t_tuples);
    
    if (res = cursor_N_tuples(cursor, &n, &tuple_list, &tail))
    {
	Bip_Error(Error_Code(res));
    }
    
    Request_Unify_Integer(v_N, t_N, n);
    if (tail == &tuple_list)
    {
	Request_Unify_Pw(v_tuples, t_tuples, v_tail, t_tail);
    }
    else
    {
	Request_Unify_Pw(v_tuples, t_tuples, tuple_list.val, tuple_list.tag);
	Request_Unify_Pw(v_tail, t_tail, tail->val, tail->tag);
    }
    Return_Unify;
}

int
p_cursor_free(value v_cursorh, type t_cursorh)
{
    pword handle;

    handle.val.all = v_cursorh.all;
    handle.tag.all = t_cursorh.all;

    return ec_free_handle(handle, &cursor_handle_tid);


}

int
p_cursor_field_value(
		/* + */ value v_cursor, type t_cursor,
		/* + */ value v_item, type t_item,
		/* - */ value v_value, type t_value
		)
{
    cursor_t * cursor;
    field_t item;
    void * value;
    int res;
    pword * argp;

    Check_Integer(t_item);
    item = (field_t) v_item.nint;
    if (item < FIELD_FIRST || item > FIELD_LAST)
    {
	Bip_Error(TYPE_ERROR);
    }
    if (item == return_code_as_string)
    {
	Check_Output_String(t_value);
    }
    else
    {
	Check_Output_Integer(t_value);
    }
    Check_Structure(t_cursor);
    argp = &v_cursor.ptr[CURSOR_HANDLE];
    Dereference_(argp);
    Get_Typed_Object(argp->val, argp->tag,&cursor_handle_tid,cursor);

    if (res = cursor_field_value(cursor, item, &value))
    {
	Bip_Error(Error_Code(res));
    }

    if (item == return_code_as_string)
    {
	pword pw;

	Make_String(&pw,*(char **)value);
	Return_Unify_Pw(v_value, t_value, pw.val, pw.tag);  
    }
    else
    {
	Return_Unify_Integer(v_value, t_value, * (word *)value);
    }
	

}


int
p_handle_free_eagerly(value v_handle, type t_handle)
{
	Check_Type(t_handle, THANDLE);
	Check_Type(v_handle.ptr->tag, TPTR);

	schedule_cut_fail_action(p_handle_free,v_handle,t_handle);
	Succeed;
}

