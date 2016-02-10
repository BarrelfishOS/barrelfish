% ----------------------------------------------------------------------
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
% Copyright (C) 1989-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: cio.pl,v 1.3 2010/04/22 14:12:49 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 */

/*
 * IDENTIFICATION:	cio.pl
 *
 * DESCRIPTION: 	Contains built-in predicates to handle the I/O
 *			in the C-prolog way.
 *
 *
 * CONTENTS:     
 *
 * REVISION HISTORY:
 * AUTHOR	VERSION	DATE	REASON
 * Micha Meier	2.4	14.2.90	
 */

:- module(cio).

:- comment(categories, ["Compatibility"]).
:- comment(summary, "Predicates for C-Prolog style I/O (see/tell family)").
:- comment(author, "Micha Meier, ECRC Munich").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(date, "$Date: 2010/04/22 14:12:49 $").
:- comment(desc, html("
    This library provides C-Prolog compatible I/O predicates. It is included
    in the C-Prolog compatibility package, but can be used independently.
    The predicates change ECLiPSe's input or output stream, respectively.")).

:- export
	see/1,
	seeing/1,
	seen/0,
	skip/1,
	skip/2,
	tab/1,
	tab/2,
	tell/1,
	telling/1,
	told/0.

% remember input/output the first time we redirect via see/tell
:- local
	variable(old_input, unknown),
	variable(old_output, unknown).

:- pragma(nodebug).


% Files opened by see/tell are characterised by having a stream
% alias that is identical to the name of the opened file.

% We actually allow a bit of a mixture between old style I/O and
% new style I/O, because that is used in some tests and seems to
% be done in Sicstus as well. E.g. see/1 and tell/1 can be called
% with any stream number or stream name, even one that has not been
% created with a see/tell. Also, seeing/1 and telling/1 return
% stream handles if there is no corresponding see/tell file.

stream_to_cprolog_name(Stream, File) :-
	get_stream_info(Stream, name, File0),
	( File0 = user ->
	    File = File0
	; atom(File0), current_stream(File0), get_stream(File0, Stream) ->
	    File = File0
	;
	    File = Stream	% nothing better to return...
	).

cprolog_name_to_stream(File, Stream) :-
	current_stream(File),
	( get_stream(File, Stream), get_stream_info(Stream, name, File) ->
	    true
	;
	    Stream = File	% nothing better to return...
	).


see(user) ?- !,
	getval(old_input, OldInput),
	( OldInput == unknown ->
	    true
	;
	    setval(old_input, unknown),
	    set_stream(input, OldInput)
	).
see(File) :-
	( getval(old_input, unknown) ->
	    get_stream(input, OldInput),
	    setval(old_input, OldInput)
	;
	    true
	),
	( cprolog_name_to_stream(File, Stream) ->
	    true
	;
	    open(File, read, Stream),
	    set_stream(File, Stream)
	),
	set_stream(input, Stream).


seeing(File) :-
	get_stream(input, Stream),
	stream_to_cprolog_name(Stream, File).


seen :-
	seeing(File),
	getval(old_input, OldInput),
	( OldInput == unknown ->
	    true
	;
	    close(File),
	    setval(old_input, unknown),
	    set_stream(input, OldInput)
	).


tell(user) ?- !,
	getval(old_output, OldOutput),
	( OldOutput == unknown ->
	    true
	;
	    setval(old_output, unknown),
	    set_stream(output, OldOutput)
	).
tell(File) :-
	( getval(old_output, unknown) ->
	    get_stream(output, OldOutput),
	    setval(old_output, OldOutput)
	;
	    true
	),
	( cprolog_name_to_stream(File, Stream) ->
	    true
	;
	    open(File, write, Stream),
	    set_stream(File, Stream)
	),
	set_stream(output, Stream).


telling(File) :-
	get_stream(output, Stream),
	stream_to_cprolog_name(Stream, File).


told :-
	telling(File),
	getval(old_output, OldOutput),
	( OldOutput == unknown ->
	    true
	;
	    close(File),
	    setval(old_output, unknown),
	    set_stream(output, OldOutput)
	).


skip(S) :-
	skip(input, S).

skip(S, X) :-
	N is X,
	skip_to(S, N).

skip_to(S, N) :-
	(get(S, N) ->
		true
	;
		skip_to(S, N)
	).

tab(X) :-
	tab(output, X).

tab(S, X) :-
	Tab is X,
	put_spaces(S, Tab).

put_spaces(_, 0) :- !.
put_spaces(S, N) :-
	put(S, 32),
	N1 is N-1,
	put_spaces(S, N1).

