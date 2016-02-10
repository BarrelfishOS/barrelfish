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
% Version:	$Id: quintus.pl,v 1.16 2013/06/16 02:21:27 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 */

/*
 * IDENTIFICATION:	quintus.pl
 *
 * DESCRIPTION: 	Quintus prolog compatibility package
 *
 *
 * CONTENTS:     
 *
 */

:- module(quintus).

:- comment(categories, ["Compatibility"]).
:- comment(summary, 'Quintus prolog compatibility package').
:- comment(author, 'Micha Meier, ECRC Munich').
:- comment(copyright, 'Cisco Systems, Inc').
:- comment(date, '$Date: 2013/06/16 02:21:27 $').
:- comment(desc, html('
    ECLiPSe includes a Quintus Prolog compatibility package to ease the
    task of porting Quintus Prolog applications to ECLiPSe Prolog.  This
    package does not provide the user with a system completely compatible
    to Quintus Prolog, however it provides most of the Quintus built-in
    predicates, moreover some of the Quintus library predicates are
    available in the ECLiPSe library.  This package includes the C-Prolog
    compatibility package (see Appendix A.6). 
    <P>
    Please note that this appendix does not detail the functionality of
    Quintus Prolog, refer to the Quintus Prolog documentation for this
    information. 
    <P>
    The effect of the compatibility library is local to the module where
    it is loaded. For maximum compatibility, a Quintus program should
    be wrapped in a separate module starting with a directive like
    <PRE>
    :- module(mymodule, [], quintus).
    </PRE>
    In this case, Eclipse-specific language constructs will not be available.
    <P>
    If the compatibility package is loaded into a standard module, e.g. like
    <PRE>
    :- module(mymixedmdule).
    :- use_module(library(quintus)).
    </PRE>
    then Quintus and Eclipse language features can be used together. 
    However, ambiguities must be resolved explicitly and confusion may
    arise from the different meaning of quotes in Eclipse vs Quintus-Prolog.
    <P>
    The following differences remain even with the compatibility package: 
    <DL>
    <DT>expand_term/2 
	<DD>This predicate is dummy, since the ECLiPSe macro facility
	works on every input term, provided that the flag
	macro_expansion is set to on. 
    <DT>get0/2 
	<DD>This predicate is identical to get/2 in ECLiPSe. 
    <DT>help/1 
	<DD>This is the normal ECLiPSe help/1 predicate. 
    <DT>meta_predicate/1 
	<DD>This declaration does not cause passing of module information
	in Quintus-style, as ECLiPSe\'s concept of meta predicates
	differs substantially.  The meta-predicates very likely have
	to be modified manually to use ECLiPSe tools (see User Manual).
    <DT>multifile/1 
	<DD>This is implemented by declaring the predicates as dynamic, so
	to obtain more efficient programs it is better to put all
	clauses of the same procedure into one file (or to concatenate
	all files where multifile predicates occur). 
    <DT>predicate_property/2 
	<DD>The property interpreted is not provided.  The property
	exported is returned if the predicate is exported or global. 
	Use of get_flag/3 should be preferred. 
    <DT>prolog_flag/2, 3 
	<DD>There are some differences in the flags, as they are mostly
	simulated with the ECLiPSe flags:
	<UL>
	    <LI>not all the character escapes used in the Quintus Prolog
	    are available.
	    <LI>gc_margin is taken as the ECLiPSe flag gc_interval
	    (see Section 19.2)
	    <LI>setting gc_trace to on sets also gc to on
	</UL>
    <DT>public/1
	<DD>synonym for export/1 
    <DT>statistics/0, 2 
	<DD>these predicates are slightly different than in Quintus, in
	particular the meaning of the memory statistics is approximate,
	and the output format is different.
    <DT>ttyflush/0, ttyget/1, ttyget0/1, ttynl/0, ttyput/1, ttyskip/1, ttytab/1 
	<DD>these predicates work with the stdout stream 
    <DT>line_position/2
	<DD>Not implemented.  To perform sophisticated output formatting,
	printf/2,3 or string streams can be used.
    </DL>
    The list below describes the syntax differences between
    ECLiPSe and Quintus Prolog.  The following properties of Quintus
    Prolog are simulated by the compatibility package: 
    <UL>
	<LI>single (resp.  double) quote must be doubled between
	    single (resp.  double) quote. 
	<LI>The symbol | (bar) is recognised as an alternative sign
	    for a disjunction and it acts as an infix operator. 
	<LI>the symbol | is not an atom
    </UL>
    The following Quintus properties are not simulated: 
    <UL>
	<LI>a clause can not be ended by end of file. 
	<LI>signed numbers: explicitly positive numbers are structures. 
	<LI>a real with an exponent must have a floating point. 
	<LI>a space is allowed before the decimal point and the exponent sign. 
	<LI>the definition of the escape sequence is more extended
	    than in ECLiPSe. 
	<LI>spy/1 and nospy/1 accept as arguments lists, rather than
	    comma-separated terms like in ECLiPSe. 
    </UL>
    ')).
:- comment(see_also, [library(cio),library(cprolog),library(sicstus),library(swi),
	library(multifile)]).

:- reexport cio.
:- reexport foreign.
:- reexport multifile.

% suppress deprecation warnings for reexported builtins
:- pragma(deprecated_warnings(not_reexports)).

:- reexport eclipse_language except

	(\=)/2,                         % hide (e.g. for Press)
%	append/3, % in QP 3.6
	delete/3,
	gcd/3,
%	ground/1, % in QP 3.6
	select/3,
        memberchk/2,
	maplist/3,
	member/2,
	(not)/1,
	union/3,
	eval/2,
	pathname/2,

	get/1,				% redefined predicates
	put/1,
	put/2,
	instance/2,
	(abolish)/1,
	arg/3,
	(dynamic)/1,
	display/1,
	ensure_loaded/1,
	erase/1,
	name/2,
	op/3,
	recorda/3,
	recordz/3,
	recorded/3,
	statistics/0,
	statistics/2,
	use_module/1,
	use_module_body/2.

	/*
	op(_,   xfx, (of)),		% don't provide these
	op(_,   xfx, (with)),
	op(_,   xfy, (do)),
	op(_,   xfx, (@)),
	op(_,   fx, (-?->)),
	op(_,	fy, (not)),
	op(_,	fy, (spied)),
	op(_,	fx, (delay)),
	macro((with)/2, _, _),
	macro((of)/2, _, _).
	*/

:- export			% temporary, while op/macros still global
	op(0,   xfx, (of)),
	op(0,   xfx, (with)),
	op(0,   xfy, (do)),
	op(0,   xfx, (@)),
	op(0,   fx, (-?->)),
	op(0,	fy, (not)),
	op(0,	fy, (spied)),
	op(0,	fx, (delay)),
	macro((with)/2, (=)/2, []),
	macro((of)/2, (=)/2, []).

:- local
	op(650, xfx, (@)).

:- export
	syntax_option(nl_in_quotes),
        syntax_option(no_array_subscripts),
	syntax_option(limit_arg_precedence),
	syntax_option(doubled_quote_is_quote),
	syntax_option(bar_is_no_atom),
	syntax_option(bar_is_semicolon),
	syntax_option(no_attributes),
	syntax_option(no_curly_arguments),
	syntax_option(blanks_after_sign),
	syntax_option(float_needs_point),

	chtab(0'\, symbol),		% disable escape sequences
	chtab(128, string_quote),	% there must be some string_quote
	chtab(0'", list_quote),

	op(1150, fx, [(meta_predicate), (multifile), (discontiguous), (public),
			(mode),
			(dynamic), (initialization), (volatile)]).

:- set_flag(macro_expansion, on).

:- reexport
	(.)/3,		% to evaluate lists in arithmetic expressions
	consult/1,
	current_functor/2,
	current_predicate/2,
	db_reference/1,
	erased/1,
	fileerrors/0,
	get/1,
	get0/1,
	heapused/1,
	leash/1,
	log10/2,
	log/2,
	nofileerrors/0,
	primitive/1,
	prompt/2,
	put/1,
	reconsult/1,
	sh/0
    from cprolog.

:- reexport
    op(_,_,_)
    from cprolog.

:- reexport numbervars.
:- reexport format.

:- export
	(abolish)/1,
	(abolish)/2,
	absolute_file_name/2,
	arg/3,
	atom_chars/2,
        break/0,
	character_count/2,
	current_input/1,
	current_output/1,
	current_key/2,
	current_module/2,
	display/1,
	(dynamic)/1,
	ensure_loaded/1,
	erase/1,
	expand_term/2,
	flush_output/1,
	gc/0,
	get0/2,
	incore/1,
	instance/2,
	is_digit/1,
	is_lower/1,
	is_upper/1,
	line_count/2,
	manual/0,
	name/2,
	no_style_check/1,
	nogc/0,
	nospyall/0,
	number_chars/2,
	op/3,
	open_null_stream/1,
	otherwise/0,
	portray_clause/1,
	predicate_property/2,
	prolog_flag/2,
	prolog_flag/3,
	(public)/1,
	put/2,
	put_line/1,
	recorda/3,
	recorded/3,
	recordz/3,
	save/1,
	set_input/1,
	set_output/1,
	source_file/1,
	source_file/2,
	statistics/0,
	statistics/2,
	stream_code/2,
	stream_position/2,
	stream_position/3,
	style_check/1,
	term_expansion/2,
        trace/0,
	ttyflush/0,
	ttyget/1,
	ttyget0/1,
	ttynl/0,
	ttyput/1,
	ttyskip/1,
	ttytab/1,
	unix/1,
	unknown/2,
	use_module/1,
	use_module/2,
	version/0,
	version/1.


:- export
	tr_lib/2.

:- export
	macro(library_directory/1, tr_lib/2, [clause]).

tr_lib(L, L) :-
	L = no_macro_expansion(library_directory(Path)),
	atom_string(Path, PathS),
	get_flag(library_path, P),
	(member(PathS, P) ->
	    true
	;
	    append(P, [PathS], New),
	    set_flag(library_path, New)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- system.		% compiler directive to add the SYSTEM flag

:- import
	abolish_body/2,
	abolish_op_body/3,
	current_built_in_body/2,
	current_op_body/4,
	current_predicate_body/2,
	dynamic_body/2,
	ensure_loaded/2,
	export_body/2,
	get_bip_error/1,
	get_flag_body/4,
	get_pager/1,
	global_body/2,
	global_op_body/4,
	import_body/2,
	nospy_body/2,
	printf_/8,
	retract_all_body/2,
	set_flag_body/4,
	tool_/2,
	untraced_call/2
   from sepia_kernel.

:-
	tool((abolish)/1, q_abolish_body/2),
	tool(incore/1, untraced_call/2),
	tool(nospyall/0, nospyall_body/1),
	tool(predicate_property/2, predicate_property_body/3),
	tool(prolog_flag/2, prolog_flag_body/3),
	tool(prolog_flag/3, prolog_flag_body/4),
	tool((public)/1, public_body/2),
	tool(op/3, op_body/4).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% *** Loading Programs ***

no_style_check(X) :- var(X), !, fail.
no_style_check(single_var) :-
	get_flag(variable_names, check_singletons) ->
	    set_flag(variable_names, on)
	;
	    true.
no_style_check(discontiguous).
no_style_check(multiple).
no_style_check(all) :-
	no_style_check(single_var).

style_check(X) :- var(X), !, fail.
style_check(single_var) :- set_flag(variable_names, check_singletons).
style_check(discontiguous).
style_check(multiple).
style_check(all) :- 
	style_check(single_var).

save(File) :-
	printf(error, 'Saved states not supported: %w%n', [save(File)]).

trace :-
	printf(error, 'trace/0 only allowed as a toplevel command%n', []).

break :-
        toplevel:break.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% *** Online help ***

manual :-
	get_flag(installation_directory, Sepiadir),
	printf('To read the documentation, please open%n%s/doc/index.html%n',
		[Sepiadir]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% *** Control ***

otherwise.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% *** I/O ***

display(Term) :-
	eclipse_language:display(stdout, Term).

open_null_stream(null).
flush_output(X) :- flush(X).
set_input(Stream) :- set_stream(input, Stream).
set_output(Stream) :- set_stream(output, Stream).
current_input(Stream) :- get_stream(input, Stream).
current_output(Stream) :- get_stream(output, Stream).

source_file(File) :-
	setof(X, P^(current_predicate(P), get_flag(P, source_file, X)), L),
	member(File, L).

source_file(Pred, File) :-
	(var(Pred) ->
	    current_predicate(F/A)
	;
	    true
	),
	functor(Pred, F, A),
	get_flag(F/A, source_file, File).

get0(S, T) :- get(S, T).

put(S, T) :-
	X is T,
	eclipse_language:put(S, X).

character_count(Stream, N) :-
	at(Stream, N).

stream_position(S, N) :-
	at(S, N).

stream_position(Stream, Old, New) :-
	at(Stream, Old),
	seek(Stream, New).

ttyget0(X) :- get0(stdin, X).
ttyget(X) :- get(stdin, X).
ttyskip(X) :- skip(stdin, X).
ttyput(X) :- put(stdout, X).
ttynl :- nl(stdout).
ttytab(N) :- tab(stdout, N).
ttyflush :-
	current_stream(S),
	get_stream_info(S, device, tty),
	get_stream_info(S, mode, write),
	flush(S),
	fail;true.

portray_clause(Clause) :-
	writeclause(Clause).

line_count(Stream, Line1) :-
	get_stream_info(Stream, line, Line),
	Line1 is Line + 1.

op_body(_, _, [], _) :- !.
op_body(P, A, [O|L], M) :-
	!,
	op_body(P, A, O, M),
	op_body(P, A, L, M).
op_body(P, A, O, M) :-
	global_op_body(P, A, O, M),
	( current_op_body(P, A, O, M) ->
	    true
	;
	    abolish_op_body(O, A, M)	% remove the hiding local definition
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% *** Arithmetic ***

arith_exception_handler(_, integer(X,Y), _) :- !,
	Y is integer(truncate(X)).
arith_exception_handler(N, Culprit, Module) :-
	error(default(N), Culprit, Module).

:- set_event_handler(20, arith_exception_handler/3).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% *** Term Conversion ***

atom_chars(Atom, List) :-
	var(Atom),
	!,
	string_list(String, List),
	atom_string(Atom, String).
atom_chars(Atom, List) :-
	atom(Atom),
	atom_string(Atom, String),
	string_list(String, List).

number_chars(Number, List) :-
	var(Number),
	!,
	remove_leading_whtspaces(List, List0),
	string_list(String, List0),
	number_string(Number, String).
number_chars(Number, List) :-
	number_string(Number, String),
	(var(List) ->
	    List0 = List
	;   remove_leading_whtspaces(List, List0)
        ),
	string_list(String, List0).

remove_leading_whtspaces([C|Cs], List) ?- 
	((nonvar(C), is_white_space(C)) ->
	    remove_leading_whtspaces(Cs, List)
	;   List = [C|Cs]
        ).

is_white_space(C) :- 
	% concat_string(['\t','\n','\r',' '], SWhtSpaces),
	% string_list(SWhtSpaces, WhtSpaces),
	% this assumes ASCII as character escapes are off by default
	WhtSpaces = [9,10,13,32], 
	memberchk(C, WhtSpaces).


name(Name, Codes) :-
	var(Codes),
	( number(Name) ->
	    number_string(Name, String),
	    string_list(String, Codes)
	; atom(Name) ->
	    atom_string(Name, String),
	    string_list(String, Codes)
	; string(Name) ->       % convenience extension
	    string_list(Name, Codes)
	;
	    error(5, name(Name, Codes))
	).
name(Name, Codes) :-
	nonvar(Codes),
	string_list(String, Codes),
	( number_string(Number, String) ->
	    Name = Number
	;
	    atom_string(Name, String)
	).


arg(N, S, X) :-
	integer(N),
	1 =< N, N =< arity(S),
	eclipse_language:arg(N, S, X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% *** Term Comparison ***

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% *** Environment ***

predicate_property_body(Pred, Property, M) :-
	var(Pred),
	!,
	(current_predicate_body(F/A, M); current_built_in_body(F/A, M)),
	functor(Pred, F, A),
	sepia_property(F/A, M, Property).
predicate_property_body(Pred, Property, M) :-
	functor(Pred, F, A),
	A < 256,
	sepia_property(F/A, M, Property).

sepia_property(P, M, compiled) :-
	get_flag_body(P, stability, static, M).
sepia_property(P, M, dynamic) :-
	get_flag_body(P, stability, dynamic, M).
sepia_property(P, M, built_in) :-
	get_flag_body(P, type, built_in, M).
sepia_property(P, M, exported) :-
	get_flag_body(P, visibility, V, M),
	(V == exported; V == reexported) -> true.
sepia_property(P, M, foreign) :-
	get_flag_body(P, call_type, external, M).
sepia_property(P, M, imported_from(Mi)) :-
	get_flag_body(P, visibility, imported, M),
	get_flag_body(P, definition_module, Mi, M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% *** Debugging ***

nospyall_body(M) :-
	nospy_body(_, M),
	writeln('All spypoints removed').

unknown(Old, New) :-
	(nonvar(Old) ->
	    error(5, unknown(Old, New))
	;
	    (get_event_handler(68, fail/0, _) ->
		Old = fail
	    ;
		Old = trace
	    ),
	    (New == trace ->
		reset_event_handler(68)
	    ;
	    New == fail ->
		set_event_handler(68, fail/0)
	    ;
		error(5, unknown(Old, New))
	    )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% *** Modules ***

public_body([], _) :- !.
public_body([Proc|Procs], M) :-
	!,
	globalize([Proc|Procs], M).
public_body(Procs, M) :-
	export_body(Procs, M).

current_module(Module, File) :-
	setof((M,F), P^(current_predicate(P),
	    get_flag(P, source_file, F),
	    get_flag(P, definition_module, M)), L),
	member((Module,File), L).

:- system_debug.
%:- system.
globalize([], _).
globalize([Pred|Rest], M) :-
	export_body(Pred, M),
	globalize(Rest, M).

:- tool(use_module/2, use_module_body/3).
:- local use_module_body/2.
:- tool(use_module/1, use_module_body/2).
:- tool(ensure_loaded/1, use_module_body/2).

use_module_body(F, L, M) :-
	ensure_loaded(F, M),
	(F = library(FM) ->
		true
	;
		FM = F
	),
	import_list(L, FM, M).

use_module_body(F, M) :-
	ensure_loaded(F, M),
	import_list(F, M).

import_list([], _, _).
import_list([Pred|L], F, M) :-
	import_body((Pred from F), M),
	import_list(L, F, M).

import_list([], _).
import_list([File|L], M) :-
	!,
	import1(File, M),
	import_list(L, M).
import_list(File, M) :-
	import1(File, M).

import1(library(File), M) :-
	!,
	import1(File, M).
import1(File, M) :-
	pathname(File, _, ModS),
	atom_string(Module, ModS),
	current_module(Module),
	!,
	import_body(Module, M).
import1(_, _).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% *** Dynamic Database ***

q_abolish_body(Name/Arity, Module) :-
	!,
	abolish_body(Name/Arity, Module).
q_abolish_body(:(Module, Spec), _) :-
	!,
	q_abolish_body(Spec, Module).
q_abolish_body([], _) :- !.		% wrong, but compatible
q_abolish_body([Spec|T], Module) :-
	!,
	q_abolish_body(Spec, Module),
	q_abolish_body(T, Module).
q_abolish_body(Atom, Module) :-
	atom(Atom),			% this is wrong in Q2.0!
	!,
	(
	    @(current_predicate(Atom/N),Module),
	    @(get_flag(Atom/N, definition_module, Module), Module),
	    abolish_body(Atom/N, Module),
	    fail
	;
	    true
	).
q_abolish_body(Term, _) :-
	error(5, abolish(Term)).

:- tool((abolish)/2, abolish_body/3).
abolish_body(Name, Arity, Module):-
	q_abolish_body(Name/Arity, Module).

:- tool((dynamic)/1, qdynamic_body/2).

qdynamic_body(M:P, _) :-
    dynamic_body(P, M).
qdynamic_body(F/A, M) :-
    dynamic_body(F/A, M).
qdynamic_body((P1, P2), M) :-
    qdynamic_body(P1, M),
    qdynamic_body(P2, M).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% *** Internal Database ***

:- tool(current_key/2, current_key_body/3).
current_key_body(KeyName, KeyTerm, Module) :-
	@(current_record(IKey), Module),
	external_key(IKey, KeyTerm),
	functor(KeyTerm, KeyName, _).

external_key(Key, KeyN) :-
	atom(Key),
	atom_string(Key, KeyS),
	string_length(KeyS, N),
	string_code(KeyS, N, 0),		% last key char is \000
	!,
	term_string(KeyN, KeyS).
external_key(Key, Key).


internal_key(Key, IKey) :-
	var(Key), !,
	current_record(IKey),
	external_key(IKey, Key).
internal_key(Key, IKey) :-
	number(Key), !,
	concat_atom([Key, '\000'], IKey).	% append \000 char
internal_key(Key, Key).



% Possible modes: recorded(+,-,-) recorded(-,-,+) recorded(-,-,-)
:- tool(recorded/3, recorded_body/4).
recorded_body(Key, Term, QRef, Module) :-
	var(QRef), !,
	internal_key(Key, IKey),
	@(sepia_kernel:recorded(IKey, Term, DbRef), Module),
	QRef = '$ref'(DbRef, 0).
recorded_body(Key, Term, '$ref'(DbRef, 0), Module) :-
	internal_key(Key, IKey),
	@(sepia_kernel:recorded(IKey, Term, DbRef), Module),
	!.

:- tool(recorda/3, recorda_body/4).
recorda_body(Key, Term, '$ref'(DbRef, 0), Module) :-
	nonvar(Key),
	internal_key(Key, NewKey),
	@(sepia_kernel:recorda(NewKey, Term, DbRef), Module).

:- tool(recordz/3, recordz_body/4).
recordz_body(Key, Term, '$ref'(DbRef, 0), Module) :-
	nonvar(Key),
	internal_key(Key, NewKey),
	@(sepia_kernel:recordz(NewKey, Term, DbRef), Module).

:- tool(erase/1, erase_body/2).
erase_body('$ref'(DbRef, 0), Module) :-
	@(sepia_kernel:erase(DbRef), Module).

instance('$ref'(DbRef, 0), Term) :-
	referenced_record(DbRef, Term).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% *** Grammar Rules ***

% only temporary
expand_term(A, B) :-
	term_expansion(A, B),
	!.
expand_term(A, A).

% To avoid calling an undefined procedure.
term_expansion(_, _) :- fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% *** Miscellaneous ***

prolog_flag_body(Flag, Old, New, _Module) :-
	var(Flag), 
	!,
	error(4, prolog_flag(Flag, Old, New)).
prolog_flag_body(Flag, Old, New, _Module) :-
	var(New), 
	Old \== New,
	!,
	error(4, prolog_flag(Flag, Old, New)).
prolog_flag_body(Flag, Old, New, Module) :-
	prolog_flag_body(Flag, Old, Module),
	set_quintus_flag(Flag, New, Module).

set_quintus_flag(character_escapes, New, Module) :-
% effect localised to Module!
	(New == on ->
	    @(set_chtab(0'\, escape), Module)
	;
	New == off ->
	    @(set_chtab(0'\, symbol), Module)
	).
set_quintus_flag(debugging, New, _) :-
	(New == off ->
		set_flag(debugging, nodebug)
	;
		set_flag(debugging, New)
	).
set_quintus_flag(fileerrors, New, _) :-
	(New == on ->
		fileerrors
	;
	New == off ->
		nofileerrors
	).
set_quintus_flag(single_var_warnings, New, _) :-
	(New == on ->
	    set_flag(variable_names, check_singletons)
	;
	get_flag(variable_names, off) ->
	    true
	;
	    set_flag(variable_names, on)
	).
set_quintus_flag(unknown, New, _) :-
	unknown(_, New).
set_quintus_flag(gc, New, _) :-
	set_flag(gc, New).
set_quintus_flag(gc_margin, New, _) :-
	set_flag(gc_interval, New).
set_quintus_flag(gc_trace, New, _) :-
	(New == on ->
		set_flag(gc, verbose)
	;
	New == off ->
		set_flag(gc, on)
	).

prolog_flag_body(character_escapes, Old, Module) :-
	(
		@(get_chtab(0'\, escape), Module) -> Old = on ; Old = off
	).
prolog_flag_body(debugging, Old, _) :-
	get_flag(debugging, Mode),
	(Mode == nodebug ->
		Old = off
	;
	Mode == leap ->
		Old = debug
	;
		Old = trace
	).
prolog_flag_body(fileerrors, Old, _) :-
	(get_event_handler(170, nofileerrors_handler/2, _) ->
		Old = off
	;
		Old = on
	).
prolog_flag_body(single_var_warnings, Old, _) :-
	(get_flag(variable_names, check_singletons) ->
	    Old = on
	;
	    Old = off
	).
prolog_flag_body(unknown, Old, _) :-
	unknown(Old, Old).
prolog_flag_body(gc, Old, _) :-
	get_flag(gc, OldF),
	(OldF == off -> Old = off; Old = on).
prolog_flag_body(gc_margin, Old, _) :-
	get_flag(gc_interval, Old).
prolog_flag_body(gc_trace, Old, _) :-
	(get_flag(gc, verbose) ->
		Old = on
	;
		Old = off

	).
prolog_flag_body(typein_module, M, _) :-
% not in quintus, but in SICStus
	get_flag(toplevel_module, M).


version.

version(Message) :-
	write(Message), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% *** System Dependent ***

is_lower(X):- X < 128, X > 96.
is_upper(X):- X > 64, X < 91.
is_digit(X):- X > 47, X < 58.

unix(cd(Path)) :-
	cd(Path).
unix(cd) :-
	getenv('HOME', Home),
	cd(Home).
unix(shell(X)) :-
	getenv('SHELL', Shell),
	concat_string([Shell, ' -c "', X, '"'], Command),
	sh(Command).
unix(system(X)) :-
	sh(X).
unix(system(X, Status)) :-
	getenv('SHELL', Shell),
	concat_string([Shell, ' -c "', X, '"'], Command),
	exec(sh, [S], Pid),
	printf(S, '%w\nexit\n%b', Command),
	close(S),
	wait(Pid, Status).
unix(shell) :-
	getenv('SHELL', Shell),
	sh(Shell).
unix(argv(L)) :-
	argc(N),
	args_list(N, [], L).
unix(exit(N)) :-
	exit(N).

:- mode args_list(++, +, -).
args_list(1, L, L) :- !.
args_list(N, L, M) :-
	N1 is N - 1,
	argv(N1, S),
	(number_string(A, S) -> true ; atom_string(A, S)),
	args_list(N1, [A|L], M).

% put_line(list of charcters)
% writes the list of characters and a newline to the current output 

put_line([]) :-
	!,
	nl.
put_line([H|T]) :-
	put(H),
	put_line(T).

gc :-	set_flag(gc, on).
nogc :-	set_flag(gc, off).

stream_code(S, C) :-
	get_stream(S, C).

absolute_file_name(Rel, Abs) :-
	(Rel == user ->
	    Abs == user
	; get_flag(prolog_suffix, Sufs),
	  (existing_file(Rel, Sufs, [], ExtRel) -> true ; ExtRel = Rel),
	  canonical_path_name(ExtRel, Abs)
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% *** Statistics ***

/*
memory (total)          377000 bytes:     350636 in use,       26364 free
   program space        219572 bytes
      atom space         (2804 atoms)      61024 in use,  43104 free
   global space          65532 bytes:       9088 in use,      56444 free
      global stack                          6984 bytes
      trail                                   16 bytes
      system                                2088 bytes
   local stack           65532 bytes:        356 in use,      65176 free
      local stack                            332 bytes
      system                                  24 bytes
 0.000 sec.  for 0 global and 0 local space shifts
 0.000 sec.  for 0 garbage collections which collected 0 bytes
 0.000 sec.  for 0 atom garbage collections which collected 0 bytes
 0.233 sec.  runtime
*/

:- local variable(systime, 0),
	variable(realtime, 0),
	variable(walltime, 0).

statistics(runtime, TotalLast) :-
	eclipse_language:statistics(runtime, TotalLast).
statistics(system_time, [Total,Last]) :-
	eclipse_language:statistics(times, [_User,System,_Real]),
	Total is fix(System*1000),
	getval(systime, Prev),
	Last is Total - Prev,
	setval(systime, Total).
statistics(real_time, [Total,Last]) :-
	eclipse_language:statistics(times, [_User,_System,Real]),
	Total is fix(Real*1000),
	getval(realtime, Prev),
	Last is Total - Prev,
	setval(realtime, Total).
statistics(walltime, [Total,Last]) :-		% Sicstus
	eclipse_language:statistics(times, [_User,_System,Real]),
	Total is fix(Real*1000),
	getval(walltime, Prev),
	Last is Total - Prev,
	setval(walltime, Total).
statistics(memory, [Total, 0]) :-
	Total is  (eclipse_language:statistics(shared_heap_allocated))
		+ (eclipse_language:statistics(private_heap_allocated))
		+ (eclipse_language:statistics(global_stack_allocated))
		+ (eclipse_language:statistics(control_stack_allocated))
		+ (eclipse_language:statistics(trail_stack_allocated))
		+ (eclipse_language:statistics(local_stack_allocated)).
statistics(program, [Used, Free]) :-
	eclipse_language:statistics(shared_heap_used, Used),
	Free is (eclipse_language:statistics(shared_heap_allocated)) - Used.
statistics(global_stack, [Used, Free]) :-
	eclipse_language:statistics(global_stack_used, Used),
	Free is (eclipse_language:statistics(global_stack_allocated)) - Used.
statistics(local_stack, [Used, Free]) :-
	Used is (eclipse_language:statistics(local_stack_used))
		+ (eclipse_language:statistics(control_stack_used)),
	Free is (eclipse_language:statistics(local_stack_allocated))
		+ (eclipse_language:statistics(control_stack_allocated)) - Used.
statistics(trail, [Used, Free]) :-
	eclipse_language:statistics(trail_stack_used, Used),
	Free is (eclipse_language:statistics(trail_stack_allocated)) - Used.
statistics(choice, [Used, Free]) :-		% Sicstus
	eclipse_language:statistics(control_stack_used, Used),
	Free is (eclipse_language:statistics(control_stack_allocated)) - Used.
statistics(stacks, [Global, Local]) :-		% Sicstus
	eclipse_language:statistics(global_stack_used, Global),
	Local is (eclipse_language:statistics(local_stack_used))
		+ (eclipse_language:statistics(control_stack_used)).
statistics(garbage_collection, [Number, Freed, Time]) :-
	eclipse_language:statistics(gc_number, Number),
	eclipse_language:statistics(gc_collected, Freed),
	eclipse_language:statistics(gc_time, Time).
statistics(stack_shifts, [0, 0, 0.0]).
statistics(atoms, [Number,Used,Free]) :-
	eclipse_language:statistics(dictionary_entries, Number),
	eclipse_language:statistics(dict_hash_usage, Used/Total),
	Free is Total-Used.
statistics(atom_garbage_collection, [Number,-1,Time]) :-
	eclipse_language:statistics(dict_gc_number, Number),
	eclipse_language:statistics(dict_gc_time, Time).
statistics(core, List) :-			% DEC-10
	statistics(memory, List).
statistics(heap, List) :-			% DEC-10
	statistics(program, List).

statistics :-
	nl(log_output),
	(
	    statistics(What, Value),
	    Fill is 24 - atom_length(What),
	    printf(log_output, '%w:%*c%w%n', [What, Fill, 0' , Value]),
	    fail
	;
	    true
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- (current_module(user) ->
	true
    ;
	create_module(user),
	@(call(import(quintus)), user)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	MODULE INITIALIZATION
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
