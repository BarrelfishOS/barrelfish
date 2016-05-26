% BEGIN LICENSE BLOCK
% Version: CMPL 1.1
%
% The contents of this file are subject to the Cisco-style Mozilla Public
% License Version 1.1 (the "License"); you may not use this file except
% in compliance with the License.  You may obtain a copy of the License
% at www.eclipse-clp.org/license.
% 
% Software distributed under the License is distributed on an "AS IS"
% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
% the License for the specific language governing rights and limitations
% under the License. 
% 
% The Original Code is  The ECLiPSe Constraint Logic Programming System. 
% The Initial Developer of the Original Code is  Cisco Systems, Inc. 
% Portions created by the Initial Developer are
% Copyright (C) 1997 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf, Stefano Novello, IC-Parc
%                 Kish Shen, CrossCore Optimization
% 
% END LICENSE BLOCK
%
% ECLiPSe PROLOG LIBRARY MODULE
%
% $Header: /cvsroot/eclipse-clp/Eclipse/Oci/dbi.ecl,v 1.8 2013/06/20 20:59:09 kish_shen Exp $
%
%
% IDENTIFICATION:	dbi.ecl
%
% AUTHOR:		Joachim Schimpf
% AUTHOR:		Stefano Novello
%


/*
 * char *sccsid = "@(#)oci.pl	1.2  96/11/21";
 *
 * Contents:	Prolog library for DB interface
 *
 * Author:	Stefano Novello
 * Author:      Kish Shen, Generalised and updated from original OCI code,
 *              initially for MySQL, Jan - Feb 2006.
 *
 *
 */
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(dbi).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- comment(categories, ["Interfacing"]).
:- comment(summary, "Interface to MySQL databases").
:- comment(author, "Kish Shen, based on Oracle interface by Stefano Novello").
:- comment(date, "$Date: 2013/06/20 20:59:09 $").
:- comment(copyright, "Cisco Systems, 2006").

:- lib(lists).
:- lib(suspend).

:- export
        session_start/4,
        session_start/3,
	session_close/1,
	session_error_value/3,

	session_commit/1,
        session_rollback/1,
	session_transaction/2,
	session_transaction/3,

        session_sql/3,

	session_sql_prepare/4,
	session_N_sql_prepare/5,
	cursor_next_execute/2,
        cursor_next_execute/3,
	cursor_N_execute/4,
        cursor_all_execute/2,

        cursor_close/1,
	session_sql_query/4,
        session_sql_query/5,
	session_sql_query_N/6,
   
	cursor_next_tuple/2,
	cursor_N_tuples/4,
        cursor_all_tuples/2,
	session_sql_prepare_query/5,
        session_sql_prepare_query_N/6,

        session_retrieve_tuple/4,
        session_retrieve_N_tuples/5,
        session_retrieve_lazy_tuples/5,

	cursor_field_value/3.


:- export struct(cursor(
                        handle,            % ec_handle. C code assumes this
                                           % is in argument position 1.
                        session            % parent session
                 ) 
          ).


:- pragma(expand).
:- import symbol_address/2,get_cut/1,cut_to/1 from sepia_kernel.

% load the dynamic MySQL library for Windows explicitly
% to avoid path problems
load_dynamic_mysql("i386_nt", SUF) :- !,
        concat_string(["i386_nt/libmysql.", SUF], F),
        load(F).
load_dynamic_mysql("x86_64_nt", SUF) :- !,
        concat_string(["x86_64_nt/libmysql.", SUF], F),
        load(F).
load_dynamic_mysql(_, _).

:- symbol_address(p_dbi_init, _ ) -> true ;
	get_flag(object_suffix,SUF),
        get_flag(hostarch, Arch),
        catch(load_dynamic_mysql(Arch, SUF), abort,
              (printf(error, "Cannot find/load the dyanmic libmysql.%s"
                             " library.%n", [SUF]),
               writeln(error, "You can obtain this file as part of MySQL."),
               printf(error, "Place the file into lib/%s directory of ECLiPSe"
                            " to use Dbi.%n", [Arch]),
               fail
              )
        ),     
        concat_string([Arch,/,"dbi_mysql.",SUF],F),
	load(F).


:- external(	s_init/1,	p_session_init),
   external(	s_start/5,	p_session_start),
   external(	s_error_value/3,	p_session_error_value),
   external(	s_commit/1,	p_session_commit),
   external(	s_rollback/1,	p_session_rollback),
   external(	s_close/1,	p_session_close),

%   external(	s_sql_ddl/2,	p_session_sql_ddl),
   external(	s_sql_dml/3,	p_session_sql_dml),
   external(	s_sql_query/6,	p_session_sql_query),
   external(	s_sql_prepare/5,	p_session_sql_prepare),
   external(	s_sql_prepare_query/6,	p_session_sql_prepare_query),
   external(    s_set_in_transaction/2, p_session_set_in_transaction),
   external(    s_is_in_transaction/1,  p_session_is_in_transaction),
   
   external(	cursor_next_exec/3,	p_cursor_next_execute),
   external(	cursor_N_execute/4,	p_cursor_N_execute),
   external(	cursor_next_tuple/2,	p_cursor_next_tuple),
   external(	cursor_N_tuples/4,	p_cursor_N_tuples),
   
   external(	c_field_value/3,	p_cursor_field_value),
   external(    cursor_free/1,          p_cursor_free),
   
   external(	handle_free_eagerly/1,	p_handle_free_eagerly),

   external(	dbi_init/1,		p_dbi_init),
   external(    dbi_final/0,		p_dbi_final).


:- include(mysqlopts).


:- mode extract_session(+,-).
extract_session(s_sql_ddl(S,_),S).
extract_session(s_sql_dml(S,_,_),S).
extract_session(s_sql_query(S,_,_,_,_,_),S).
extract_session(s_sql_prepare(S,_,_,_,_),S).
extract_session(s_sql_prepare_query(S,_,_,_,_,_),S).
extract_session(s_close(S),S).
extract_session(s_commit(S),S).
extract_session(s_rollback(S),S).
extract_session(s_init(S),S).
extract_session(s_start(S,_,_,_,_),S).

:- mode extract_cursor(+,-).
extract_cursor(cursor_next_exec(C,_,_),C).
extract_cursor(cursor_next_execute(C,_),C).
extract_cursor(cursor_next_execute(C,_,_),C).
extract_cursor(cursor_N_execute(C,_,_,_),C).
extract_cursor(cursor_next_tuple(C,_),C).
extract_cursor(cursor_N_tuples(C,_,_,_),C).
extract_cursor(cursor_close(C),C).

session_abort(S) :-
        (nonvar(S), s_is_in_transaction(S) ->
            exit_block(dbi_fail)
        ;
            abort
        ).

cursor_session(cursor{session:S0},S) ?- S0 = S.



dbi_error_handler(Error, Goal) :-
	extract_cursor(Goal,Cursor),
	!,
	error_id(Error,Id),
	cursor_field_value(Cursor,return_code_as_string,Message),
	cursor_field_value(Cursor,return_code,Code),
	printf(stderr,"%s Code:%d in %PQw%n%s%n",[Id,Code,Goal,Message]),
	flush(stderr),
	cursor_session(Cursor,Session),
	session_abort(Session).

dbi_error_handler(Error,Goal) :-
	extract_session(Goal,S),
        nonvar(S),
	s_error_value(S,Code,Message),
	!,
	error_id(Error,Id),
	printf(stderr,"%s Code:%d in %PQw%n%s%n",[Id,Code,Goal,Message]),
	flush(stderr),
	session_abort(S).

dbi_error_handler(Error,Goal) :-
	error_id(Error,Id),
	printf(stderr,"%s in %PQw%n",[Id,Goal]),
	flush(stderr),
	abort.


:- tool(session_transaction/2,session_transaction/3).
session_transaction(Session,Goal,Module) :-
        (s_is_in_transaction(Session) ->
            once(call(Goal)@Module)
        ;
            s_set_in_transaction(Session, 1),
            ( block( once(call(Goal)@Module) , Tag,
                     transaction_recovery(Session,Tag) ) ->
                s_set_in_transaction(Session, 0),
                s_commit(Session)
            ;
                transaction_recovery(Session,dbi_fail)
            )
        ).

	transaction_recovery(Session,Tag) :-
                s_set_in_transaction(Session, 0),
		s_rollback(Session),
		(Tag = dbi_fail ->
		    fail
		;
		    exit_block(Tag)
		).

get_user_and_host(User0@Host0, User, Host) :- !,
        concat_string([User0], User),
        concat_string([Host0], Host).
get_user_and_host(User0, User, Host) :-
        concat_string([User0], User),
        Host = "".

session_start(Login,Passwd,Session) :-
        session_start(Login,Passwd,[],Session).

session_start(Login,Passwd0,OptsList,Session) :-
        get_user_and_host(Login, User, Host),
        get_opts(session, OptsList,Opts), !,
        concat_string([Passwd0], Passwd),
        s_init(Session),
        s_start(Session,User,Host,Passwd,Opts).
session_start(Login,Passwd,OptsList,Session) :-
        error(6,session_start(Login,Passwd,OptsList,Session)).


session_error_value(Session,Code,Message) :-
        ( is_handle(Session) ->
            s_error_value(Session,Code,Message)
        ;
            error(5, session_error_value(Session,Code,Message))
        ).


session_commit(Session) :-
    (s_is_in_transaction(Session) -> true ; s_commit(Session)).

session_rollback(Session) :-
    (s_is_in_transaction(Session) -> 
        exit_block(dbi_fail) 
    ; 
        s_rollback(Session)
    ).


session_sql(Session,SQL,Tuples) :-
    ( is_handle(Session) -> 
        s_sql_dml(Session,SQL,Tuples) 
    ;
        error(5, session_sql(Session,SQL,Tuples))
    ).

session_sql_query(Session,Template,SQL,OptsList,Cursor) :-
    Cursor = cursor{handle:CursorH,session:Session},
    ( get_opts(cursor, OptsList, Opts) ->
        s_sql_query(Session, Template,SQL,1,Opts,CursorH)
    ;
        printf(error, "Invalid option list in %w%n", [OptsList]),
        error(6, session_sql_query(Session, Template,SQL,OptsList,Cursor))
    ).
    
session_sql_query(Session,Template,SQL,Cursor) :-
    session_sql_query_N(Session,Template,SQL,1,[],Cursor).


session_sql_query_N(Session,Template,SQL,N,OptsList,Cursor) :-
    Cursor = cursor{handle:CursorH,session:Session},
    ( get_opts(cursor, OptsList, Opts) ->
        s_sql_query(Session,Template,SQL,N,Opts,CursorH)
    ;
        printf(error, "Invalid option list in %w%n", [OptsList]),
        error(6, session_sql_query_N(Session, Template,SQL,N,OptsList,Cursor))
    ).


session_sql_prepare(Session,Template,SQL,Cursor) :-
    Cursor = cursor{handle:CursorH,session:Session},
    s_sql_prepare(Session,Template,SQL,1,CursorH).

session_N_sql_prepare(Session,Template,SQL,N,Cursor) :-
    Cursor = cursor{handle:CursorH,session:Session},
    s_sql_prepare(Session,Template,SQL,N,CursorH).


session_sql_prepare_query(Session,ParamT,QueryT,SQL,Cursor) :-
    Cursor = cursor{handle:CursorH,session:Session},
    s_sql_prepare_query(Session,ParamT,QueryT,SQL,1,CursorH).

session_sql_prepare_query_N(Session,ParamT,QueryT,SQL,N,Cursor) :-
    Cursor = cursor{handle:CursorH,session:Session},
    s_sql_prepare_query(Session,ParamT,QueryT,SQL,N,CursorH).


session_close(Session) :-
    ( is_handle(Session) ->
        s_close(Session)
    ;
        error(5, session_close(Session))
    ).        


%
% Mapping between atoms and field_t C enumerated type in dbi.h
:- mode field_t(++,-).

field_t( state,			0).
field_t( rows_processed_count,	1).
field_t( return_code,		2).
field_t( return_code_as_string,	3).
field_t( warning_flags,		4).
field_t( row_ID,		5).

cursor_close(cursor{handle:Handle}) ?- !,
        cursor_free(Handle).
cursor_close(Cursor) :-
        error(5, cursor_close(Cursor)).

cursor_field_value(Cursor,Field,Value) :-
	atom(Field),
        Cursor = cursor{},
	field_t(Field,F),
	!,
	c_field_value(Cursor,F,Value).
cursor_field_value(Cursor,Field,Value) :-
    	error(5,cursor_field_value(Cursor,Field,Value)).


session_retrieve_tuple(Session,SQL,Template,Tuple) :-
	session_sql_query(Session,Template,SQL,10,Cursor),
	handle_free_eagerly(Cursor),
	repeat,
	( cursor_next_tuple(Cursor,Tuple0) ->
		Tuple0 = Tuple
	;
		!,fail
	).


session_retrieve_N_tuples(Session,Template,SQL,N,Tuple) :-
	session_sql_query(Session,Template,SQL,N,Cursor),
	handle_free_eagerly(Cursor),
	repeat,
	cursor_N_tuples(Cursor,R,Tuples,[]),
	(R < N ->
		!
	;
		true
	),
	member(Tuple, Tuples).

cursor_all_tuples(Cursor,Tuples) :-
	cursor_N_tuples(Cursor,N,Tuples,Rest),
	( N > 0 ->
	    cursor_all_tuples(Cursor,Rest)
	;
	    Rest = []
	).


session_retrieve_lazy_tuples(Session,Template,SQL,N,Tuples) :-
	get_cut(Cut),
	session_sql_query(Session,Template,SQL,N,Cursor),
	cursor_lazy_tuples(Cursor,Cut,Tuples).

cursor_lazy_tuples(Cursor,Cut,Tuples) :-
	var(Tuples),
	!,
	suspend(cursor_lazy_tuples(Cursor,Cut,Tuples),3,Tuples->inst).

cursor_lazy_tuples(Cursor,Cut,Tuples) :-
	get_cut(CutNow),
	(CutNow \= Cut ->
		cut_to(Cut),
		printf(stderr,
		    "warning: cutting in  cursor_lazy_tuples/3\n")
	;
		true
	),
	cursor_N_tuples(Cursor,N,Tuples,Rest),
	(N > 0 ->
	    cursor_lazy_tuples(Cursor,CutNow,Rest)
	;
	    Rest = []
	).

cursor_next_execute(Cursor, Tuple)  :-
        cursor_next_execute(Cursor, Tuple, []).

cursor_next_execute(Cursor, Tuple, OptsList) :-
        ( get_opts(cursor, OptsList, Opts) ->
            cursor_next_exec(Cursor, Tuple, Opts)
        ;
            printf(error, "Invalid option list in %w%n", [OptsList]),
            error(6, cursor_next_execute(Cursor, Tuple, OptsList))
        ).


cursor_all_execute(_Cursor,[]) :- !.
cursor_all_execute(Cursor,Tuples) :-
	cursor_N_execute(Cursor,_,Tuples,Rest),
	cursor_all_execute(Cursor,Rest).

:- pragma(deprecated_warnings(off)).

dbi_initialize :- 
        define_error("DBI General exception",E),
	set_event_handler(E,dbi_error_handler/2),
	dbi_init(E).
 
dbi_finalize :-
        dbi_final.

:- local initialization(dbi_initialize), finalization(dbi_finalize).


/******************************************************************************/

:- comment(desc, html("\
<P>
 This library provides an interface to database management systems (DBMS)
 that supports SQL (Structure Query Language), allowing SQL commands,
 written in an ECLiPSe program, to be passed to the DBMS for processing,
 and their results (if any) to be passed back to ECLiPSe.
</P><P>
 The exact SQL supported by the library is determined by the underlying
 DBMS, as the SQL commands are passed directly to the DBMS via its C API. 
 If supported by the C API, the library also allows the use of prepared SQL
 statements, where a SQL statement, possibly parameterised to accept
 different input data, is parsed once by the DBMS, and efficiently executed
 multiple times. Support for transactions (with commit and rollback) is also
 provided.
</P><P>
 The library provides a relatively low-level interface to the DBMS, but
 higher level interfaces (e.g. where a SQL query is viewed as a predicate,
 yielding different results tuples on backtracking) can be constructed on
 top of this.
</P><P>
 Currently, MySQL (version 5.0 or later) is supported by the library.
 Note that the MySQL client dynamic library (libmysqlclient.so on Unix
 systems, mysql.dll on Windows) is not included with the distribution, and
 if this file is not found by ECLiPSe on loading lib(dbi), there will be 
 an error. This file can be obtained by downloading MySQL, and placing the
 .so or .dll file into a place where ECLiPSe can find it, for example a 
 standard system library location, or in the ECLiPSe library location 
 (<eclipsedir>/lib/<arch>).
 
</P><P>
 Data is exchanged with the DBMS via:
<DL>
<P>
<DT>SQL statements (input)
  <DD>Directly in the SQL statements passed to the DBMS;
<P>
<DT>SQL query results (output)
  <DD>The results returned by SQL queries are returned to ECLiPSe via
      result tuples. Each tuple represents a single row of result, and is
      returned to ECLiPSe as a Prolog structure, with each argument
      represents one column in the result tuple. The ordering of the
      arguments corresponds to the ordering defined by the SQL query.
<DT>Parameters for prepared SQL statements (input)
  <DD>If prepared statements are supported by the DBMS, data can also be
      passed to the DBMS via parameters (also known as placeholders). Each
      parameter functions like a variable, whose value is instantiated when
      the statement is executed. The library passes parameter values to the
      DBMS via Prolog structures, with each argument represent one parameter. 
      The ordering of the arguments is defined by the ordering of the
      parameters in the prepared SQL statement.
 </DL>
<P>
 Input and output data in the structures need to be converted from/to
 ECLiPSe datatypes. The ECLiPSe datatype is specified by a template, which
 specifies the datatype by example, i.e. an example result tuple structure
 (for output) or parameter structure (for input) where each argument
 specify the type for that argument, e.g.
    <TT>emp(123,\"some name\",100.0,'some job',extra(data))</TT>,
 specifies that the first argument should be an integer, the second a 
 string, the third a real, the fourth, an atom, and the last, a general
 Prolog term. Note that general Prolog terms are stored in the database in
 a binary representation, and the conversion is done by the library. 
</P><P>
 The data is exchanged with the DBMS via buffers. Some DBMS require the
 sizes of the buffers to be specified. This is done automatically by the
 library, but if the default sizes need to overridden (e.g. if the argument
 is large), the template can be used to specify the size of buffers for
 variable length data, e.g.
    <TT>emp(123,\"1000\",100.0,'2000',extra(5000))</TT>,
 where the second argument <TT>\"1000\"</TT> specifies a buffer size of 1000
 bytes, the third argument <TT>'2000'</TT> a buffer size of 2000 bytes,
 and the last argument, <TT>extra(5000)</TT>, a buffer size of 5000 bytes.
 Note that any size specifications are simply ignored if not required.
")).

:- comment(session_start/4,  [
        amode: session_start(++,++,++,-),
        args: ["Login": "DBMS login: ID or ID@Host. (ID,Host: string or"
                        " atom)",
               "Password": "Password (string or atom)",
               "Options": "Options (list or nil)",
               "Session": "Session handle"
              ],
        summary: "starts a new session for the DBMS",
        see_also:[session_close/1],
        eg:"
     % connecting to a database server running on the local machine, as 
     % database user sqluser with password password
     session_start(\"sqluser\", \"password\", [], S).

     % connecting to a database server running on machine my.sql.host,
     % as database user sqluser with password password, and selecting
     % to use the database mydb
     session_start(sqluser@my.sql.host, password, [dbame:\"mydb\"], S).
",
        desc: html("\
<P>
 Starts a new session with the DBMS. A new connection with the DBMS server is
 established, with the supplied DBMS user ID. Optionally, a host could be
 supplied, if the server is located on a remote machine. The host could be
 either the machine name, or an IP address. A session handle for this new
 connection is returned by the predicate.
</P><P>
 The session is closed automatically if the program backtracks or aborts back
 beyond this call. 
</P><P>
 Options is a (possibly empty) list of <TT>Option:Value</TT> pairs, specifying any
 options for this session. The following options are supported:
<DL>
<P>
<DT><STRONG><TT>dbname</TT></STRONG>
 <DD>Specifies the name of the database to use. Value can be a
     string or an atom. The same effect can also be achieved by issuing the
     SQL statement <TT>USE ...</TT> (where ... is the dbname).
<P>
<DT><STRONG><TT>storage</TT></STRONG>
 <DD>(MySQL specific) specifies the default storage model to use for
     the tables. Value can be either the string/atom <TT>transactional</TT>
     or <TT>non-transactional</TT>. For transactional tables, changes are
     local until they are committed. Uncommitted changes can be rollback.
     For non-transactional tables, changes are global. InnoDB (with autocommit
     turned off) is the storage  engine used for transactional, and MyISAM is
     the storage engine used for non-transactional. See the MySQL manual for
     details on storage engines and their effect on tables.
</DL>"),
        exceptions:[
            5: "Login, Password or Options not of the correct type.",
            6: "Invalid option specification in Options.",
            dbi_bad_field: "Problems with Option's argument.",
            dbi_error: "Problems connecting to DBMS server."]
                   
]).

:- comment(session_close/1,  [
        amode: session_close(++),
        args: ["Session": "A session handle"],
        summary: "close a session for the DBMS",
        see_also:[session_start/4,cursor_close/1],
        exceptions: [
            5: "Session is not a session handle"],
        desc: html("\
</P><P>
 This closes a session explicitly, disconnecting from the DBMS server. It 
 takes  effect immediately. Any uncommitted transactional updates are rolled 
 back. Some resources associated with the session are also freed.
</P><P>
 Cursors for this session are not closed by the closing of the session,
 but they will no longer be able to access the database, but will still use
 some resources. It is recommended that cursors be closed before closing
 the session. All resources associated with a session are only freed when
 both the session and all the cursors are closed. 
")]).

:- comment(session_rollback/1, [
        amode: session_rollback(++),
        args: ["Session": "A session handle"],
        summary: "rollback transactional changes made to the database.",
        see_also:[session_commit/1, session_transaction/2],
        exceptions: [
            5: "Session is not a valid session handle",
            dbi_error: "Problems from DBMS during rollback."],
        eg:
"
    session_sql(Session, \"insert into mytable values (\'a\')\", _),
    session_commit(Session), % committing the insert of a to table mytable 
    session_sql(Session, \"insert into mytable values (\'b\')\", _),
    session_sql(Session, \"insert into mytable values (\'c\')\", _),
    % undo the inserting of b and c into table mytable (if tranactional)
    session_rollback(Session), 
",
        desc: html("\
<P>
  This undoes all <STRONG>transactional</STRONG> changes made to the database since the last
  commit for this session. Note some DBMS can operate non-transactionally
  (for example, non-transactional tables in MySQL), such operations cannot
  be undone. Also, even for transactions, not all changes can be undone.
  See DBMS's manual for details.
</P><P>
  Outside a transaction this predicate will either succeed if rollback was
  successful, or raise an error otherwise.
<P></P>
  When executed within the scope of a session_transaction/2 goal, this 
  predicate will simply abort the complete outer transaction.
</P><P>
 NOTE: This predicate behaves very differently, depending on
 whether it is in a transaction or not. It is always advisable not
 to rely on it succeeding, but rather to fail or abort immediately
 after it adopting a coding style that causes a Prolog failure
 whenever a transaction cannot be completed.")
]).

:- comment(session_commit/1, [
        amode: session_commit(++),
        args: ["Session": "A session handle"],
        summary: "commits transactional changes made to the database.",
        see_also:[session_rollback/1, session_transaction/2],
        eg:
"
    session_sql(Session, \"insert into mytable values (\'a\')\", _),
    session_commit(Session), % committing the insert of a to table mytable 
",
        exceptions: [
            5: "Session is not a valid session handle",
            dbi_error: "Problems from DBMS during commit."],
         desc: html("\
<P>
 This commits any transactional updates to the database made within Session.
 By default, transactional updates are local to the session, and the changes
 only become global when a commit occurs, e.g. when session_commit/1 is
 executed. Note that non-transactional updates become globally visible 
 immediately.
</P><P>
 When executed within the scope of a session_transaction/2 goal, this
 predicate simply succeeds without doing anything.")
]).

:- comment(session_transaction/2, [
        amode: session_transaction(++,+),
        args: ["Session": "A session handle",
               "Goal": "Prolog goal that implements a database update."],
        summary: "executes Goal as a database transaction.",
        see_also:[session_rollback/1, session_commit/1],
        exceptions: [
            5: "Session is not a valid session handle",
            abort: "session_rollback/1 called within Goal"],
        eg: "
transfer(Session, Amount,FromAccount,ToAccount) :-
     session_transaction(Session, 
         transfer_(Session,Amount,FromAccount,ToAccount)
     ).

% note \'?\' in SQL in the syntax MySQL uses for placeholders. This may be
% different in other DBMS
transfer_(Session, Amount, FromAccount, ToAccount) :-
    SQL = \"update accounts set balance = balance + ? \\
                                             where id = ?\",
    Deduct is - Amount,
    session_sql_prepare(Session,incbal(1.0,12),SQL,1,Update),
    cursor_next_execute(Update,incbal(Deduct,FromAccount)),
    cursor_next_execute(Update,incbal(Amount,ToAccount)).
",
        desc: html("\
<P>
 This executes Goal as a database transaction. This predicate is only useful
 if the database supports transactions (i.e. changes are local until committed).
 If Goal succeeds session_transaction/2  commits the update, cuts any
 alternative solutions to Goal and succeeds itself.
</P><P>
 If Goal fails or causes a database error, session_transaction/2 fails and
 rolls back any changes done to the database.
</P><P>
 If Goal aborts, the update is rolled back, and session_transaction/2 aborts.
</P><P>
 MySQL specific note: transactions are supported for transactional tables only
 (i.e. stored with a storage engine with transaction-safe capabilities).
</P><P?
 NOTE: It is a programming error to have some previous uncommitted
 database updates at the time of calling session_transaction/2. 
</P><P>
 Calls of this predicate can be nested, but only the outermost transaction 
 is real. All the inner transactions are simply equivalent to call(Goal).
 This way it is possible to write a call to session_transaction/2, into 
 some code that implements a simple update, but then to include that simple 
 update into a larger transaction.
</P><P>
 Similarly session_commit/1 and session_rollback/1 alter their behaviour so
 that they can be used within a transaction.
</P><P>
 Transactions are local to one session so there is no way to safely make an 
 update relating to several sessions.")
]).


:- comment(session_sql/3, [ 
        amode: session_sql(++,++,-),
        args: ["Session": "A session handle",
               "SQL":"A SQL statement (string)",
               "RowProcessedCount": "Number of rows affected by this"
                                    " statement"
              ],
        summary: "Executes a SQL statement on the database server.",
        exceptions: [
            5: "Session is not a valid session handle or SQL not a string",
            dbi_error: "database returned an error when executing SQL."
                    ],
        eg:"
make_accounts(Session) :-
    session_sql(Session,
        \"create table accounts \\
         (id           decimal(4)      not null,\\
          balance      decimal(9,2)    default 0.0 not null, \\
          overdraft    decimal(9,2)    default 0.0 not null \\
         )\" ,_),
    session_sql(Session,
        \"insert into accounts (id,balance) values (1001,1200.0)\",1),
    session_sql(Session,
        \"insert into accounts (id,balance) values (1002,4300.0)\",1).
",
        desc: html("\
<P>
 Executes a SQL statement (without parameters) on the database server. The
 SQL statement should not have a terminating semicolon (;). If supported by
 the DBMS, SQL can consist of multiple statements separated by semicolons.
 RowProcessedCount will be unified with the number of rows affected by this
 statement.
</P><P>
 In the case of data definition language statements the RowProcessedCount
 parameter is undefined and should be passed as a free variable.
</P><P>
 In the case of data manipulation language statements, the
 RowProcessedCount is the number of rows, deleted, modified or inserted
 by the SQL statement.
</P><P>
 The SQL statement must be valid for the DBMS to execute. It can contain
 NULL characters, i.e. it can contain binary data.
</P><P>
 Any results generated by the statement is discarded.")
]).


:- comment(session_sql_query/4, [
        amode: session_sql_query(++,+,++, -),
        args: ["Session": "A session handle",
               "ResultTemplate": "Template defining the types of results"
                                " tuple (structure)",
               "SQLQuery": "A SQL statement query (string)",
               "Cursor": "Returned cursor handle"
              ],
        summary: "Executes a SQL query on the database server.",
        see_also: [session_sql_query/5, cursor_next_tuple/2, 
                   cursor_all_tuples/2,
                   cursor_N_tuples/4, session_sql_prepare_query/5,
                   cursor_close/1
                  ],
        eg:"
  check_overdraft_limit(Session, Account) :-
      L = [\"select count(id) from accounts \\
          where     id = \",Account,\" and balance < overdraft\"],
      concat_string(L,SQL),
      session_sql_query(Session,c(0),SQL,OverdraftCheck),
      cursor_next_tuple(OverdraftCheck,c(Count)),
      Count = 0.
",
        exceptions: [5: "Session is not a valid session handle, or SQLQuery"
                        " not a string, or ResultTemplate not a structure",
                     dbi_error: "Error from DBMS while executing SQLQuery.",
                     dbi_bad_template: "ResultTemplate has the wrong arity"
                    ],
        desc: html("\
<P>
 Executes a SQL query on the database server. The predicate returns in
 Cursor the cursor handle for this SQL query, and the results can then be
 retrieved using cursor_*_tuple family of predicates. 
</P><P>
 The SQL query returns result in tuples of N elements each. Each tuple is
 mapped to a Prolog structure of arity N. ResultTemplate is a structure of
 arity N specifying the types of the return data for ECLiPSe. See the
 general description of this library or the manual for a description of 
 the template specification.
 </P><P>
 The SQL query must be valid for the DBMS to execute. It can contain
 NULL characters, i.e. it can contain binary data.
 </P><P>
 This predicate is called with default options for the cursor, i.e. it is
 equivalent to calling session_sql_query/5 with an empty Options list.
")
]).

:- comment(session_sql_query/5, [
        amode: session_sql_query(++,+,++, ++,-),
        args: ["Session": "A session handle",
               "ResultTemplate": "Template defining the types of results"
                                " tuple (structure)",
               "SQLQuery": "A SQL statement query (string)",
               "Options": "Options (list of Option:Value pairs or nil)",
               "Cursor": "Returned cursor handle"
              ],
        summary: "Executes a SQL query on the database server with options"
                 " specified by Options.",
        see_also: [session_sql_query/4, cursor_next_tuple/2, cursor_all_tuples/2,
                   cursor_N_tuples/4, session_sql_prepare_query/5,
                   cursor_close/1
                  ],
        eg:"
  check_overdraft_limit(Session, Account) :-
      L = [\"select count(id) from accounts \\
          where     id = \",Account,\" and balance < overdraft\"],
      concat_string(L,SQL),
      % the buffering:server option is MySQL specific
      session_sql_query(Session,c(0),SQL,[buffering:server],OverdraftCheck),
      cursor_next_tuple(OverdraftCheck,c(Count)),
      Count = 0.
",
        exceptions: [5: "Session is not a valid session handle, or SQLQuery"
                        " not a string, or ResultTemplate not a structure",
                     6: "Invalid option specification in Options",
                     dbi_error: "Error from DBMS while executing SQLQuery.",
                     dbi_bad_template: "ResultTemplate has the wrong arity"
                    ],
        desc: html("\
<P>
 Executes a SQL query on the database server. The predicate returns in
 Cursor the cursor handle for this SQL query, and the results can then be
 retrieved using cursor_*_tuple family of predicates. Options is 
 a (possibly empty) list of <TT>Option:Value</TT> pairs, specifying 
 DBMS-specific options for the cursor.
</P><P>
 The SQL query returns result in tuples of N elements each. Each tuple is
 mapped to a Prolog structure of arity N. ResultTemplate is a structure of
 arity N specifying the types of the return data for ECLiPSe. See the
 general description of this library or the manual for a description of 
 the template specification.
 </P><P>
 The SQL query must be valid for the DBMS to execute. It can contain
 NULL characters, i.e. it can contain binary data.
</P><P>
 MySQL specific:
</P><P>
 Options is used to specify the type of cursor used. Currently this only
 applies to cursors for SQL queries. The options are:
<DL>
<P>
<DT><STRONG><TT>buffering</TT></STRONG>
<DD>Specifies where the result set of a SQL query is buffered. Value can be
 either <TT>client</TT> (the default) or <TT>server</TT>. By default, the
 whole of the result set for a query is copied to the client (i.e. the
 ECLiPSe process running lib(dbi)) after the SQL query is executed. The 
 alternative is to leave the result set on the DBMS server, and only get
 the result tuples from the server one by one (with e.g. cursor_next_tuple/2).
 </P><P>
 The default buffering is on the client side, because this is the default
 of the MySQL C API, and in addition, it imposes certain restrictions on
 how the result tuples can be retrieved. However, as the whole result set
 is retreived, this can impose significant memory overheads if there are
 many tuples in the result set. On the other hand, there is no restrictions
 on how many active client buffered cursor is allowed per session at the
 same time, but only one active server buffered cursor is allowed at any
 one time -- a query result must be exhausted or the cursor explicitly
 closed before another query can be issued for that session.
 </P><P>
<DT><STRONG><TT>type</TT></STRONG>
<DD>This option is not relevant for the direct SQL queries of
 session_sql_query/4. It is only relevant for prepared queries, and has no
 effect here.
</DL>
")
]).

:- comment(session_sql_prepare/4, [
        amode: session_sql_prepare(++,+,++,-),
        args: ["Session": "A session handle",
               "ParamTemplate": "Template defining the types of the"
                                " parameters (structure or [])",
               "SQL": "A SQL statement in prepared syntax (string)",
               "Cursor": "Returned cursor handle"
              ],
        summary: "Prepares a SQL statement for execution by the DBMS.",
        see_also: [cursor_next_execute/2, cursor_all_execute/2,
                   cursor_N_execute/4, cursor_close/1,
                   session_sql/3, session_sql_prepare_query/5
                  ],
        exceptions: [5: "Session is not a valid session handle, or SQL"
                        " not a string, or ParamTemplate not a structure",
                     dbi_error: "Error from DBMS while preparing SQL",
                     dbi_bad_template: "ParamTemplate has the wrong arity"
                    ],
        eg:"
  % note \'?\' in SQL in the syntax MySQL uses for placeholders. This may be
  % different in other DBMS
  transfer_(Session, Amount, FromAccount, ToAccount) :-
      SQL = \"update accounts set balance = balance + ? \\
                                               where id = ?\",
      Deduct is - Amount,
      % incbal(1.0,12) is the parameter template
      session_sql_prepare(Session,incbal(1.0,12),SQL,1,Update),
      cursor_next_execute(Update,incbal(Deduct,FromAccount)),
      cursor_next_execute(Update,incbal(Amount,ToAccount)).",

        desc: html("\
<P>
 Prepares a SQL statement for execution. The statement is not actually
 executed, and a cursor_*_execute family of predicate is required to
 execute the predicate. This facility is only available if the DBMS
 supports prepared statements, and the SQL statement has to be written in
 the prepared statement syntax of the DBMS. The predicate returns the
 cursor handle representing this prepared statement in Cursor, which can
 then be used in subsequent library predicates.
</P><P>
 A prepared SQL statement is parsed by the DBMS, so that it could be
 executed more efficiently. It can also be parameterised, where the 
 parameters represent values to be filled in when the statement is
 executed. The statement can be executed multiple times with different
 parameters. The types of the parameters is specified by ParamTemplate,
 which is a Prolog structure of arity M (where M is the number of
 parameters for the statement), or the nil atom [] if there are no parameters.
 See the general description of this library or the manual for a
 description of the template specification.
 </P><P>
 The SQL statement must be valid for the DBMS to execute. It can contain
 NULL characters, i.e. it can contain binary data. The SQL statement cannot
 return any results. If it does, then an error would be raised when the SQL
 statement is actually executed. 
</P><P>
 Note that some DBMS restricts which SQL statements can be prepared. If an
 SQL statement cannot be prepared, it can still be executed using
 session_sql/3. ")
]).
 
:- comment(session_sql_prepare_query/5, [
        amode: session_sql_prepare_query(++,+,+,++,-),
        args: ["Session": "A session handle",
               "ParamTemplate": "Template defining the types of the"
                                " parameters (structure or [])",
               "ResultTemplate": "Template defining the types of results"
                                " tuple (structure)",
               "SQLQuery": "A SQL query in prepared syntax (string)",
               "Cursor": "Returned cursor handle"
              ],
        summary: "Prepares a SQL query for execution by the DBMS.",
        exceptions: [5: "Session is not a valid session handle, or SQLQuery"
                        " not a string, or ResultTemplate or ParamTemplate"
                        " not a structure",
                     dbi_error: "Error from DBMS while preparing SQLQuery.",
                     dbi_bad_template: "ResultTemplate or ParamTemplate has the wrong arity"
                    ],
        see_also: [cursor_next_execute/2, 
                   cursor_next_tuple/2, cursor_all_tuples/2,
                   cursor_N_tuples/4,
                   session_sql/3, session_sql_query/4
                  ],
        eg:"
 make_check_overdraft_limit(Session, Cursor) :-
      % note \'?\' in SQL in the syntax MySQL uses for placeholders. This may be
      % different in other DBMS
      SQL = \"select count(id) from accounts where ID = ? \\
                 and balance < overdraft\",
      session_sql_prepare_query(Session,a(0),c(0),SQL,1,Cursor).",

        desc: html("\
<P>
 Prepares a SQL query for execution. The query is not actually
 executed, and a cursor_next_execute/2 needs to be called to execute the 
 SQL query, giving values to any parameters. Then the cursor_*_tuple family
 of predicates can be used to obtain the results. This facility is only
 available if the DBMS supports prepared statements, and the SQL query has
 to be written in the prepared statement syntax of the DBMS. The predicate
 returns the cursor handle representing this prepared query in Cursor,
 which can then be used in subsequent library predicates.
</P><P>
 A prepared SQL statement is parsed by the DBMS, so that it could be
 executed more efficiently. It can also be parameterised, where the 
 parameters represent values to be filled in when the statement is
 executed. The statement can be executed multiple times with different
 parameters. The types of the parameters is specified by ParamTemplate,
 which is a Prolog structure of arity M (where M is the number of
 parameters for the statement), or the nil atom [] if there are no parameters.
 See the general description of this library or the manual for a
 description of the template specification.
</P><P>
 The SQL query returns result in tuples of N elements each. Each tuple is
 mapped to a Prolog structure of arity N. ResultTemplate is a structure of
 arity N specifying the types of the return data for ECLiPSe. See the
 general description of this library or the manual for a description of 
 the template specification.
</P><P>
 The SQL query must be valid for the DBMS to execute. It can contain
 NULL characters, i.e. it can contain binary data.
</P><P>
 Note that some DBMS restricts which SQL statements can be prepared. If an
 SQL statement cannot be prepared, it can still be executed using
 session_sql/3.
</P><P>
 MySQL specific note: not all SQL statements can be prepared by MySQL.
 Refer to the MySQL manual for details.
")
]).
 
:- comment(cursor_close/1, [
        amode: cursor_close(++),
        args: ["Cursor": "A cursor handle"],
        summary: "Close the cursor associated with Cursor",
        exceptions: [
              5: "Cursor is not a cursor handle."
        ],
        see_also: [session_close/1,
                   session_sql_prepare/4,session_sql_query/4,
                   session_sql_prepare_query/5],
        desc: html("\
<P>
 Closes a cursor explicitly, and frees the resources associated with the
 cursor. It takes effect immediately.
</P><P>
 It is recommended that the user close all cursors before explicitly
 closing a session. Cursors that remain open after a session is closed
 cannot be used, but may still be using resources.
</P><P>
 Cursors are automatically closed if the program backtracks or aborts back
 beyond where they were created.
")]).

:- comment(cursor_next_execute/2, [
        amode: cursor_next_execute(++,+),
        args: ["Cursor": "A cursor handle",
               "Tuple": "A tuple of parameter values matching the template"
                        " for this cursor (structure)"
              ],
        see_also: [cursor_next_execute/3, cursor_all_execute/2, 
                   cursor_N_execute/4, 
                   session_sql_prepare/4, session_sql_prepare_query/5
                  ],
        summary: "Executes the parametrised prepared SQL statement"
                 " represented by Cursor.",
        exceptions: [5: "Cursor is not a valid cursor handle", 
                     5: "Type mismatch between parameter template"
                        " specification for Cursor and actual tuple data",
                     dbi_buffer_over: "Parameter value(s) too big for the"
                                      " buffer",
                     dbi_error: "Error from DBMS while executing SQL"
                                " associated with Cursor.",
                     dbi_bad_template: "ParamTemplate not specified when"
                                       " Cursor was created",
                     dbi_bad_cursor: "The Cursor is not in a state to"
                                     " execute a query (e.g. it was cancelled)"
                    ],
        eg:"
  % note \'?\' in SQL in the syntax MySQL uses for placeholders. This may be
  % different in other DBMS
  transfer_(Session, Amount, FromAccount, ToAccount) :-
      SQL = \"update accounts set balance = balance + ? \\
                                               where id = ?\",
      Deduct is - Amount,
      % incbal(1.0,12) is the parameter template
      session_sql_prepare(Session,incbal(1.0,12),SQL,Update),
      cursor_next_execute(Update,incbal(Deduct,FromAccount)),
      cursor_next_execute(Update,incbal(Amount,ToAccount)).",

        desc: html("\
<P>
 Executes the parameterised prepared SQL statement represented by Cursor,
 previously prepared by session_sql_prepare/4 or session_sql_prepare_query/5.
 The parameter values for this execution is supplied by Tuple.
</P><P>
 Tuple is a structure whose name and arity match the parameter template
 when Cursor was prepared, and the arguments give the values for the
 parameters for this execution, and must be compatible with the type
 specified by the template, except that an argument can be an
 uninstantiated variable, to denote a NULL value for the corresponding
 parameter.
</P><P>
 If the SQL statement is a query, and was prepared as a query using
 session_sql_prepare_query/5, results can be obtained from the query by
 the cursor_*_tuple family of predicates.
</P><P>
 This predicate is called with default options for the cursor, i.e. it is
 equivalent to calling cursor_next_execute/3 with an empty Options list.
 
")
]).

:- comment(cursor_next_execute/3, [
        amode: cursor_next_execute(++,+,++),
        args: ["Cursor": "A cursor handle",
               "Tuple": "A tuple of parameter values matching the template"
                        " for this cursor (structure)",
               "Options": "Options (list of Option:Value pairs or nil)"
              ],
        see_also: [cursor_next_execute/2, cursor_all_execute/2, cursor_N_execute/4, 
                   session_sql_prepare/4, session_sql_prepare_query/5
                  ],
        summary: "Executes the parametrised prepared SQL statement"
                 " represented by Cursor, with options Options.",
        exceptions: [5: "Cursor is not a valid cursor handle", 
                     5: "Type mismatch between parameter template"
                        " specification for Cursor and actual tuple data",
                     6: "Invalid option specification in Options",
                     dbi_buffer_over: "Parameter value(s) too big for the"
                                      " buffer",
                     dbi_error: "Error from DBMS while executing SQL"
                                " associated with Cursor.",
                     dbi_bad_template: "ParamTemplate not specified when"
                                       " Cursor was created",
                     dbi_bad_cursor: "The Cursor is not in a state to"
                                     " execute a query (e.g. it was cancelled)"
                    ],

        desc: html("\
<P>
 Executes the parameterised prepared SQL statement represented by Cursor,
 previously prepared by session_sql_prepare/4 or session_sql_prepare_query/5.
 The parameter values for this execution is supplied by Tuple. Options is 
 a (possibly empty) list of <TT>Option:Value</TT> pairs, specifying 
 DBMS-specific options for the cursor.
</P><P>
 Tuple is a structure whose name and arity match the parameter template
 when Cursor was prepared, and the arguments give the values for the
 parameters for this execution, and must be compatible with the type
 specified by the template, except that an argument can be an
 uninstantiated variable, to denote a NULL value for the corresponding
 parameter.
</P><P>
 If the SQL statement is a query, and was prepared as a query using
 session_sql_prepare_query/5, results can be obtained from the query by
 the cursor_*_tuple family of predicates.
</P><P>
 MySQL specific:
</P><P>
 Options is used to specify the type of cursor used. Currently this only
 applies to cursors for SQL queries. The options are:
<DL>
<P>
<DT><STRONG><TT>buffering</TT></STRONG>
<DD>Specifies where the result set of a SQL query is buffered. Value can be
 either <TT>client</TT> (the default) or <TT>server</TT>. By default, the
 whole of the result set for a query is copied to the client (i.e. the
 ECLiPSe process running lib(dbi)) after the SQL query is executed. The 
 alternative is to leave the result set on the DBMS server, and only get
 the result tuples from the server one by one (with e.g. cursor_next_tuple/2).
 </P><P>
 The default buffering is on the client side, because this is the default
 of the MySQL C API, and in addition, it imposes certain restrictions on
 how the result tuples can be retrieved. However, as the whole result set
 is retreived, this can impose significant memory overheads if there are
 many tuples in the result set.
 </P><P>
<DT><STRONG><TT>type</TT></STRONG>
<DD>Specifies the type of cursor, and is only meaningful if the buffering
 option is set to server. Value can be either <TT>no_cursor</TT> (the
 default) or <TT>read_only</TT>.  These correspond to the MySQL statement 
 attribute STMT_ATTR_CURSOR_TYPE of CURSOR_TYPE_NO_CURSOR and 
 CURSOR_TYPE_READ_ONLY respectively (See the MySQL manual for details).
</P><P>
 Only one active cursor of type no_cursor is allowed per session, and this
 active cursor must be closed before another query can be issued to the
 DBMS server. read_only cursor does not have this restriction, and several
 such cursors can be active at the same time. 
</DL>
")
]).

:- comment(cursor_all_execute/2, [
        amode: cursor_all_execute(++,+),
        args: ["Cursor": "A cursor handle",
               "TupleList": "A list of tuples of parameter values matching the template"
                        " for this cursor"
              ],
        summary: "Executes the parametrised prepared SQL statement"
                 " represented by Cursor, once for each tuple in TupleList.",
        see_also: [cursor_next_execute/2, cursor_N_execute/4, 
                   session_sql_prepare/4, session_sql_prepare_query/5
                  ],
        exceptions: [5: "Cursor is not a valid cursor handle", 
                     5: "Type mismatch between parameter template"
                        " specification for Cursor and actual tuple data",
                     dbi_error: "Error from DBMS while executing SQL"
                                " associated with Cursor.",
                     dbi_bad_template: "ParamTemplate not specified when"
                                       " Cursor was created"
                    ],
        desc: html("\
<P>
 Executes the parameterised prepared SQL statement represented by Cursor,
 previously prepared by session_sql_prepare/4 or session_sql_prepare_query/5.
 The statement is executed once for each tuple in TupleList, with the 
 parameter values supplied by that tuple. The tuples are executed in the
 order they are in TupleList, and any results produced are discarded when
 the next tuple is executed.
</P><P>
 Each tuple is a structure whose name and arity match the parameter
 template when Cursor was prepared, and the arguments give the values for
 the parameters for this execution, and must be compatible with the type
 specified by the template, except that an argument can be an
 uninstantiated variable, to denote a NULL value for the corresponding
 parameter.")
]).

:- comment(cursor_N_execute/4, [
        amode: cursor_N_execute(++,-,+,-),
        args: ["Cursor": "A cursor handle",
               "Executed": "Number of times the SQL statement was executed.",
               "TupleList": "A difference list (together with"
                            " RestTupleList) of tuples of parameter values matching the template"
                        " for this cursor",
               "RestTupleList": "Tail of tuple difference list"
              ],
        summary: "Executes the parametrised prepared SQL statement"
                 " represented by Cursor, once for each tuple in TupleList.",
        see_also: [cursor_next_execute/2, cursor_N_execute/4, 
                   session_sql_prepare/4, session_sql_prepare_query/5
                  ],
        exceptions: [5: "Cursor is not a valid cursor handle", 
                     5: "Type mismatch between parameter template"
                        " specification for Cursor and actual tuple data",
                     dbi_error: "Error from DBMS while executing SQL"
                                " associated with Cursor.",
                     dbi_bad_template: "ParamTemplate not specified when"
                                       " Cursor was created"
                    ],
        eg:"
  transfer_(Session, Amount, FromAccount, ToAccount) :-
      SQL = \"update accounts set balance = balance + ? \
                                               where id = ?\",
      Deduct is - Amount,
      session_sql_prepare(Session,incbal(1.0,12),SQL,2,Update),
      Updates = [incbal(Deduct,FromAccount),incbal(Amount,ToAccount)],
      % execute both updates with one call to cursor_N_execute/4
      cursor_N_execute(Update,2,Updates,[]).
",
        desc: html("\
<P>
 Executes the parameterised prepared SQL statement represented by Cursor,
 previously prepared by session_sql_prepare/4 or session_sql_prepare_query/5.
 The predicate executes a number of tuples from TupleList, taking the
 tuples in order from the start of the list, leaving the unprocessed tuples
 in RestTupleList. The number of tuple processed is unified with Executed.
 Any results produced when executing a tuple is discarded when the next
 tuple is processed.
</P><P>
 The number of tuples processed per call is determined by the library. 
 This freedom allows the  predicate to process a multiple number of tuples
 in the most efficient way for the DBMS.
</P><P>
 Each tuple is a structure whose name and arity match the parameter
 template when Cursor was prepared, and the arguments give the values for
 the parameters for this execution, and must be compatible with the type
 specified by the template, except that an argument can be an
 uninstantiated variable, to denote a NULL value for the corresponding
 parameter.
</P><P>
 MySQL specific note: MySQL does not support the execution of multiple
 tuples in one go, so for MySQL, this predicate is essentially equivalent
 to executing cursor_next_execute/1 multiple times.
")
]).

:- comment(cursor_next_tuple/2, [
        amode: cursor_next_tuple(++,-),
        args: ["Cursor": "A cursor handle",
               "ResultTuple": "Returned result tuple"
              ],
        summary: "Retrieve the next result tuple from the SQL query in"
                 " ResultTuple",
        fail_if: "No more results are available for the SQL query",
        resat: no,
        exceptions: [5: "Cursor is not a valid cursor handle", 
                     5: "Unable to convert tuple result to ECLiPSe type",
                     dbi_error: "Error from DBMS while executing SQL"
                                " associated with Cursor.",
                     dbi_error: "Error from DBMS while fetching result",
                     dbi_not_query: "The SQL associated with Cursor is not"
                                    " a query and so cannot return results.",
                     dbi_buffer_over: "Result value(s) too big for the"
                                      " buffer",
                     dbi_cancelled: "The cursor have been cancelled."
                    ],
        eg:"
  check_overdraft_limit(Session, Account) :-
      L = [\"select count(id) from accounts \\
          where     id = \",Account,\" and balance < overdraft\"],
      concat_string(L,SQL),
      session_sql_query(Session,c(0),SQL,OverdraftCheck),
      cursor_next_tuple(OverdraftCheck,c(Count)),
      Count = 0.
",
        desc: html("\
<P>
 Retrieve the next result tuple from the SQL query represented by Cursor,
 and unify it with ResultTuple. Cursor is a cursor previously created with
 session_sql_query/4 or session_sql_prepare_query/5. ResultTuple is a
 structure with the same name and arity as defined by the tuple template
 when the cursor was created. The predicate converts the result to the type
 specified in the template, except that NULL values are returned as
 variables.
</P><P>
 If the SQL query have not yet been executed, and it contains no
 parameters, then the SQL query will first be executed before retrieving
 the result.
</P><P>
 cursor_next_tuple/2 will fail when all the results tuples for the query
 have been returned. If it is then called again for the same SQL query,
 this cancels the cursor, and raise the cursor cancelled error.
</P><P>
 cursor_next_tuple/2 is not resatisfiable, so to return successive tuples
 on backtracking, use repeat/0 to re-execute cursor_next_tuple/2:
<TT><PRE>
  match_tuple(Cursor, Tuple) :-
        repeat,
        ( cursor_next_tuple(Cursor, Tuple0) ->
             Tuple0 = Tuple
        ;
             !, fail
        ).
</PRE></TT>
")
]).

:- comment(cursor_all_tuples/2, [
        amode: cursor_all_tuples(++,-),
        args: ["Cursor": "A cursor handle",
               "ResultTuples": "Returned list of result tuples"
              ],
        summary: "Retrieve all remaining result tuples from the SQL query in"
                 " ResultTuples",
        see_also: [session_sql_query/4, session_sql_prepare_query/5,
                   cursor_next_tuple/2, cursor_N_tuples/4
                  ],
        exceptions: [5: "Cursor is not a valid cursor handle", 
                     5: "Unable to convert tuple result to ECLiPSe type",
                     dbi_error: "Error from DBMS while executing SQL"
                                " associated with Cursor.",
                     dbi_error: "Error from DBMS while fetching result",
                     dbi_not_query: "The SQL associated with Cursor is not"
                                    " a query and so cannot return results."
                    ],
        desc: html("\
<P>
 Retrieve the all remaining result tuples from the SQL query represented by
 Cursor, and unify them with ResultTuples list. Each element of the list is
 a single result tuple. Cursor is a cursor previously created with
 session_sql_query/4 or session_sql_prepare_query/5. Each tuple in
 ResultTuples is a structure with the same name and arity as defined by the
 tuple template when the cursor was created. The predicate converts the
 result to the type specified in the template, except that NULL values are
 returned as variables.
</P><P>
 If the SQL query have not yet been executed, and it contains no
 parameters, then the SQL query will first be executed before retrieving
 the results.")
]).

:- comment(cursor_N_tuples/4, [
        amode: cursor_N_tuples(++,-,-,-),
        args: ["Cursor": "A cursor handle",
               "Retrieved": "Number of result tuples retrieved",
               "ResultTuples": "Returned difference list (with"
                               " RestResultTuples) of result tuples",
               "RestResultTuples": "Tail of returned result tuples"
              ],
        summary: "Retrieve  result tuples from the SQL query in"
                 " the difference list ResultTuples and RestResultTuples.",
        see_also: [session_sql_query/4, session_sql_prepare_query/5,
                   cursor_next_tuple/2, cursor_all_tuples/2
                  ],
        exceptions: [5: "Cursor is not a valid cursor handle", 
                     5: "Unable to convert tuple result to ECLiPSe type",
                     dbi_error: "Error from DBMS while executing SQL"
                                " associated with Cursor.",
                     dbi_error: "Error from DBMS while fetching result",
                     dbi_not_query: "The SQL associated with Cursor is not"
                                    " a query and so cannot return results."
                    ],
        desc: html("\
<P>
 This is similar to cursor_all_tuples/4 except it works on difference list.
 It is designed to efficiently retrieve a buffer full of tuples for DBMS
 that support the retrieval of multiple tuples in its API. Otherwise, all
 the remaining tuples are retrieved one by one and the ResultTuples list
 constructed. Retrieved is unified with the number of retrieved result
 tuples. If no tuples match the query, Retrieved will be 0. Each element
 of the list is a single result tuple. Cursor is a cursor previously
 created with session_sql_query/4 or session_sql_prepare_query/5. Each
 tuple in ResultTuples is a structure with the same name and arity as
 defined by the tuple template when the cursor was created. The predicate
 converts the result to the type specified in the template, except that
 NULL values are returned as variables.
</P><P>
 If the SQL query have not yet been executed, and it contains no
 parameters, then the SQL query will first be executed before retrieving
 the results.
</P><P>
 MySQL specific note: MySQL does not support the retrieval of multiple
 tuples in one go, so for MySQL, this predicate is essentially equivalent
 to retrieving the results one by one.
")
]).

:- comment(cursor_field_value/3,hidden).
:- comment(dbi_error_handler/2, hidden).
:- comment(session_error_value/3, hidden).
:- comment(session_retrieve_N_tuples/5, hidden).
:- comment(session_retrieve_lazy_tuples/5, hidden).
:- comment(session_retrieve_tuple/4, hidden).
:- comment(session_N_sql_prepare/5, hidden).
:- comment(session_sql_prepare_query_N/6, hidden).
:- comment(session_sql_query_N/6, hidden).
:- comment(session_start/3, hidden).

