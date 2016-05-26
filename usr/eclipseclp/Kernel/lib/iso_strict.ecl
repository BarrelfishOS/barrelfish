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
% The Original Code is  The ECLiPSe Library iso_strict.
% The Initial Developer of the Original Code is  Coninfer Ltd.
% Portions created by the Initial Developer are
% Copyright (C) 2010-2013 Coninfer Ltd
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: iso_strict.ecl,v 1.10 2015/05/19 22:16:32 jschimpf Exp $
% ----------------------------------------------------------------------

%
% ECLiPSe PROLOG LIBRARY MODULE
%
% IDENTIFICATION:	iso_strict.pl
%
% AUTHOR:		Joachim Schimpf
%
% CONTENTS:		see export directive
%
% DESCRIPTION:		Strict ISO 13211-1 standard language module
%

:- module(iso_strict).

:- pragma(system).

% Workaround: make tools (which don't obey the system directive) into built-ins
:- local initialization((
    current_module_predicate(exported,P), set_flag(P, type, built_in), fail ; true
)).

% Tell the compiler that we are strict
:- export initialization(eclipse_language:error(148,pragma(iso(strict)))).

:- export
	chtab(0'`, string_quote),
	chtab(0'", list_quote).

:- comment(categories, [`Compatibility`]).
:- comment(summary, `Strict ISO Prolog compatibility library`).
:- comment(author, `Joachim Schimpf, Coninfer Ltd`).
:- comment(copyright, 'Joachim Schimpf, Coninfer Ltd').
:- comment(date, `$Date: 2015/05/19 22:16:32 $`).
:- comment(see_also, [library(multifile),library(iso),library(iso_light)]).
:- comment(desc, html(`
<h3>Overview</h3>
    This library provides an implementation of Standard Prolog as
    defined in ISO/IEC 13211-1 (Information Technology, Programming
    Languages, Prolog, Part 1, General Core, 1995) and the technical
    corrigenda ISO/IEC 13211-1 TC1 (2007) and TC2 (2012).
    The library is provided in source form.
    <P>
    This library aims at providing a "strict mode" as required by ISO 13211-1,
    paragraph 5.1.e.  For non-strict versions, see library(iso) and
    library(iso_light).
    <P>
<h3>Usage</h3>
    The effect of this compatibility library is (with minor exceptions)
    local to the module where it is loaded.  An ISO-program should always
    be contained in a separate module, starting with a directive like
    <PRE>
    :- module(myisomodule, [], iso_strict).
    </PRE>
    Here, the last argument of the module/3 directive indicates the language.
    It is not advisable to use ":-lib(iso_strict)" or
    ":-ensure_loaded(library(iso_strict))" within an eclipse_language module,
    because this would lead to import conflicts between the different
    versions of built-in predicates.
    <P>
    Alternatively, in order to use ISO-Prolog without having different
    modules, one can invoke eclipse with a "-L iso_strict" command line option,
    or set the ECLIPSEDEFFAULTLANGUAGE environment variable to 'iso_strict'.
    This will launch eclipse with a default module accepting 'iso_strict'
    language instead of the usual 'eclipse_language'.
    <P>
<h3>Specification of implementation defined features</h3>
    <DL>
    <DT>6.5 Processor characted set</DT>
	<DD>The PCS is the ISO 8859-1 character set.  Classification of
	extended characters: 7f-a0 layout; a1-bf, d7, f7 graphic;
	c0-d6, d8-f6, f8-ff alphanumeric.</DD>
    <DT>6.6 Collating sequence</DT>
	<DD>The collating sequence is that of the ISO 8859-1 character set</DD>
    <DT>7.1.2.2 Character codes</DT>
	<DD>Each character maps to a corresponding byte</DD>
    <DT>7.1.4.1 Characters</DT>
	<DD>As in the ISO 8859-1 character set</DD>
    <DT>7.2.1 Variable term order</DT>
	<DD>Older variables precede newer variables</DD>
    <DT>7.4.2.4,5 op/3 and char_conversion/2</DT>
	<DD>An operator or character-conversion defined in a directive is
	effective at runtime, and only in the module in which it occurs</DD>
    <DT>7.4.2.6 initialization/1</DT>
	<DD>Initialization goals are executed in the order in which they
	occur in the Prolog text</DD>
    <DT>7.4.2.7 include/1</DT>
	<DD>The argument is a file name atom according to ECLiPSe\'s canonical
	file name syntax, or a term of the form library(atom)</DD>
    <DT>7.4.2.8 ensure_loaded/1</DT>
	<DD>The argument is a file name atom according to ECLiPSe\'s canonical
	file name syntax, or a term of the form library(atom).  A file will
	be loaded on the first occurrence of ensure_loaded/1 in a prolog text,
	and if the file has been modified since the time it was first loaded.
	</DD>
    <DT>7.4.2.7 set_prolog_flag/2</DT>
	<DD>Flag setting are effective at runtime and globally (except for
	a few module-local flags in non-strict mode, see set_flag/2)</DD>
    <DT>7.5.1 Preparing for execution</DT>
	<DD>See the eclipse_language built-ins, menu items and command line
	options for compiling, loading and module handling, and also the
	instructions for using library(iso) or library(iso_strict)</DD>
    <DT>7.7.1,3 Execution and Initialization</DT>
	<DD>See the general ECLiPSe facilities, i.e. toplevel, graphical
	user interface and command line options</DD>
    <DT>7.10.1 Sources and sinks</DT>
	<DD>See open/3,4</DD>
    <DT>7.10.2.6 Text streams</DT>
	<DD>Text streams are very similar to binary streams, no characters
	are implicitly inserted or removed.  The nl/0,1 predicates emit
	an operating system and device dependent newline sequence.</DD>
    <DT>7.10.2.8,11 Stream positions</DT>
	<DD>File, string, and null streams can be (re)positioned</DD>
    <DT>7.10.2.9 End position of a stream</DT>
	<DD>The end position of a stream is the same as the position that
	a character appended to the stream would have</DD>
    <DT>7.10.2.11 Stream options</DT>
	<DD>The default eof_action is error</DD>
    <DT>7.10.2.13 Stream properties</DT>
	<DD>File names are atoms according to ECLiPSe\'s canonical file name
	syntax.</DD>
    <DT>7.11 Flags</DT>
	<DD>Fixed values: bounded=false, min_integer and max_integer fail,
	integer_rounding_function=toward_zero, max_arity=unbounded,
	char_conversion=off.
	Default values: double_quotes=chars, debug=off.
	If debug=on, the ECLiPSe tracer is active.  In non-strict mode,
	there is an additional flag max_predicate_arity, which indicates
	the limit on predicate arity (there is no limit on term arity).
	</DD>
    <DT>7.12.1 Effect of an error</DT>
	<DD>The implementation defined error term argument is normally the
	predicate indicator of the culprit goal.  For syntax errors, it is
	a term describing the error location.</DD>
    <DT>8.15.4 call/N</DT>
	<DD>The maximum N is given by the flag max_predicate_arity (255)</DD>
    <DT>8.17.1 set_prolog_flag/2</DT>
	<DD>The admissible flag values are the ones defined by ISO-Prolog (for
	iso_strict), plus those accepted by ECLiPSe\'s set_flag/2 (for iso)</DD>
    <DT>8.17.3,4 halt/0,1</DT>
	<DD>Exits the OS process with the given return code (or 0)</DD>
    <DT>9 Evaluable functors</DT>
        The 'exceptional values' are realized as follows: 'float_overflow'
	leads to a floating point infinity result; 'underflow' leads to
	a floating point denormalized value result; 'zero_divisor' leads
	to a floating point infinity result in the case of floats,
	or an evaluation_error(zero_divisor) in the case of integers;
	'int_overflow' does not occur and might lead to running out of
	memory instead.
    <DT>9.3 Other arithmetic operations</DT>
	<DD>When min/2 or max/2 are used with mixed integer and float
	arguments, the integer is coerced to float, and the result
	computed by comparing two floats</DD>
    <DT>9.4 Bitwise arithmetic operations</DT>
	<DD>The bitwise arithmetic operations behave as if operating on
	an unlimited length two\'s complement representation</DD>
    </DL>

<h3>Implementation specific features</h3>
    These are only available with library(iso), not with library(iso_strict)!
    <DL>
    <DT>7.10.2.11 Stream options</DT>
	<DD>For additional stream options, see open/4</DD>
    <DT>7.10.2.12 Options on stream closure</DT>
	<DD>None</DD>
    <DT>7.10.2.13 Stream properties</DT>
	<DD>For additional stream properties, see get_stream_info/3</DD>
    <DT>7.10.3 Read options</DT>
	<DD>For additional read options, see read_term/3</DD>
    <DT>7.10.4 Write options</DT>
	<DD>For additional stream options, see write_term/3</DD>
    <DT>7.11 Flags</DT>
	<DD>For additional Prolog flags, see get_flag/2</DD>
    </DL>

<h3>Remaining deviations from Standard</h3>
    <OL>
    <LI>The write predicates output extra spaces</LI>
    <LI>The char_conversion flag is always off, meaning that character
    conversion is not applied to prolog texts or on term input.  However,
    char_conversion/2 and current_char_conversion/2 predicates are operational.
    </OL>
    `)).

:- reexport
	true/0,				% built-ins
	fail/0,
	!/0,
	(',')/2,
	(;)/2,
	(->)/2,
	catch/3,
	(=)/2,				% 8.2
	(\=)/2,
	var/1,				% 8.3
	atom/1,
	integer/1,
	float/1,
	atomic/1,
	compound/1,
	nonvar/1,
	number/1,
	callable/1,
	ground/1,
	acyclic_term/1,
	(@=<)/2,			% 8.4
	(==)/2,
	(\==)/2,
	(@<)/2,
	(@>)/2,
	(@>=)/2,
	functor/3,			% 8.5
	arg/3,
	(=..)/2,
	copy_term/2,
	current_predicate/1,
	close/1,			% 8.11
	close/2,
	nl/0,				% 8.12
	nl/1,
	throw/1,
	write/1,
	write/2,
	writeq/1,
	writeq/2,
	write_canonical/1,
	write_canonical/2,
	(\+)/1,				% 8.15
	once/1,
	false/0,
	repeat/0,
	char_code/2,
	halt/0,				% 8.17

	(:)/2				% for modules

   from eclipse_language.


:- export
	syntax_option(not(nl_in_quotes)),
	syntax_option(iso_escapes),
	syntax_option(iso_base_prefix),
	syntax_option(iso_restrictions),
	syntax_option(plus_is_no_sign),
	syntax_option(doubled_quote_is_quote),
	syntax_option(no_array_subscripts),
	syntax_option(bar_is_no_atom),
	syntax_option(no_attributes),
	syntax_option(no_curly_arguments),
	syntax_option(blanks_after_sign),
	syntax_option(float_needs_point),
	syntax_option(limit_arg_precedence).


:- reexport
	(abolish)/1,
	asserta/1,			% 8.9
	assertz/1,
	at_end_of_stream/0,
	at_end_of_stream/1,
	atom_concat/3,
	atom_codes/2,
	atom_chars/2,
	atom_length/2,			% 8.16
	char_conversion/2,
	clause/2,			% 8.8
	current_char_conversion/2,
	current_input/1,
	current_op/3,
	current_output/1,
	flush_output/0,
	flush_output/1,
	get_byte/1,
	get_byte/2,
	get_char/1,
	get_char/2,
	get_code/1,
	get_code/2,
	halt/1,
	number_chars/2,
	number_codes/2,
	op/3,
	peek_byte/1,
	peek_byte/2,
	peek_char/1,
	peek_char/2,
	peek_code/1,
	peek_code/2,
	put_byte/1,
	put_byte/2,
	put_code/1,
	put_code/2,
	put_char/1,
	put_char/2,
	retract/1,
	retractall/1,
	set_input/1,
	set_output/1,
	set_stream_position/2,
	sub_atom/5,
	subsumes_term/2,
	unify_with_occurs_check/2
    from iso.

:- ensure_loaded(library(multifile)).	% for directive
:- ensure_loaded(library(iso_error)).
:- use_module(library(iso_aux)).

t_syntax(_IllegalType, _) :-
	throw(114).	% UNEXPECTED token

?- export initialization((
					% hide (global,non-portray) macros
	eclipse_language:current_macro(F,_,Opt,sepia_kernel),
	eclipse_language:nonmember(write, Opt),
	eclipse_language:delete(global, Opt, Opt1),
	eclipse_language:local(macro(F, (=)/2, Opt1)),
	fail
    ;
					% disallow ECLiPSe types
	eclipse_language:local(macro(type(rational), t_syntax/2, [])),
	eclipse_language:local(macro(type(breal), t_syntax/2, [])),
	eclipse_language:local(macro(type(string), t_syntax/2, [])),
	fail
    ;
	eclipse_language:current_op(P, A, Op),		% hide all (global) operators
	Op \== (','),
	op(0, A, Op),
	fail
    ;
	op(1200,  fx, :-),		% define ISO ones only
	op(1200, xfx, -->),
	op(1200,  fx, ?-),
	op(1200, xfx, :-),
	op(1100, xfy, ;),
	op(1050, xfy, ->),
	%op(1000, xfy, (',')),
	op(900,  fy, \+),
	op(700, xfx, =..),
	op(700, xfx, =),
	op(700, xfx, \=),
	op(700, xfx, ==),
	op(700, xfx, \==),
	op(700, xfx, @<),
	op(700, xfx, @=<),
	op(700, xfx, @>),
	op(700, xfx, @>=),
	op(700, xfx, is),
	op(700, xfx, =:=),
	op(700, xfx, =\=),
	op(700, xfx, <),
	op(700, xfx, =<),
	op(700, xfx, >),
	op(700, xfx, >=),
	op(600, xfy, :),
	op(500, yfx, +),
	op(500, yfx, -),
	op(500, yfx, /\),
	op(500, yfx, \/),
	op(400, yfx, /),
	op(400, yfx, *),
	op(400, yfx, //),
	op(400, yfx, >>),
	op(400, yfx, <<),
	op(400, yfx, rem),
	op(400, yfx, div),
	op(400, yfx, mod),
	op(200, xfx, **),
	op(200, xfy, ^),
	op(200,  fy, +),
	op(200,  fy, -),
	op(200,  fy, \)
    )).

:- import
	bip_error/1
    from sepia_kernel.


%-----------------------------------------------------------------------
% 7.8 Control constructs
%-----------------------------------------------------------------------

:- export call/1, call/2, call/3.

:- tool(call/1,call_/2).
call_(G, M) :-
	( normalize_call(G, G1) ->
	    eclipse_language:call(G1)@M
	;
	    throw(error(type_error(callable,G),call/1))
	).

    :- mode normalize_call(?,-).
    normalize_call(G, G) :- var(G), !.
    normalize_call(G, G1) :-
	normalize_body(G, G1).

:- tool(call/2,call_/3).
call_(P, A, M) :-
	( critical_goal(P, A, G) ->
	    ( normalize_body(G, G1) ->
		eclipse_language:call(G1)@M
	    ;
		throw(error(type_error(callable,G),call/1))
	    )
	;
	    eclipse_language:call(P, A)@M
	).

    critical_goal(','(G1), G2, ','(G1,G2)).
    critical_goal(';'(G1), G2, ';'(G1,G2)).
    critical_goal('->'(G1), G2, '->'(G1,G2)).

:- tool(call/3,call_/4).
call_(P, A1, A2, M) :-
	( critical_goal(P, A1, A2, G) ->
	    ( normalize_body(G, G1) ->
		eclipse_language:call(G1)@M
	    ;
		throw(error(type_error(callable,G),call/1))
	    )
	;
	    eclipse_language:call(P, A1, A2)@M
	).

    critical_goal(',', G1, G2, ','(G1,G2)).
    critical_goal(';', G1, G2, ';'(G1,G2)).
    critical_goal('->', G1, G2, '->'(G1,G2)).


%-----------------------------------------------------------------------
% 8.4 Term comparison
% compare/3:	extra type checks
%-----------------------------------------------------------------------

:- export compare/3.
compare(R, X, Y) :- var(R), !,
	eclipse_language:compare(R, X, Y).
compare(R, X, Y) :- atom(R), !,
	( (R==(=) ; R==(<) ; (R)==(>)) ->
	    eclipse_language:compare(R, X, Y)
	;
	    throw(error(domain_error(order,R),compare/3))
	).
compare(R, _, _) :-
	throw(error(type_error(atom,R),compare/3)).


:- export sort/2.
sort(Xs, Ss) :-
	( is_output_list(Ss) -> true
	; throw(error(type_error(list,Ss),sort/2)) ),
	eclipse_language:sort(Xs, Ss).


:- export keysort/2.
keysort(Xs, Ss) :-
	check_pair_list(Xs, Xs),
	check_pair_list_out(Ss, Ss),
	eclipse_language:keysort(Xs, Ss).

    check_pair_list(Ps, _) :- var(Ps), !,
	throw(error(instantiation_error,keysort/2)).
    check_pair_list([], _) :- !.
    check_pair_list([P|Ps], All) :- !,
	( var(P) -> throw(error(instantiation_error,keysort/2))
	; P = _-_ -> true
	; throw(error(type_error(pair,P),keysort/2))
	),
	check_pair_list(Ps, All).
    check_pair_list(_, All) :-
	throw(error(type_error(list,All),keysort/2)).


    check_pair_list_out(Ps, _) :- var(Ps), !.
    check_pair_list_out([], _) :- !.
    check_pair_list_out([P|Ps], All) :- !,
	( var(P) -> true
	; P = _-_ -> true
	; throw(error(type_error(pair,P),keysort/2))
	),
	check_pair_list_out(Ps, All).
    check_pair_list_out(_, All) :-
	throw(error(type_error(list,All),keysort/2)).


%-----------------------------------------------------------------------
% 8.5.5 Term decomposition
%-----------------------------------------------------------------------

:- export term_variables/2.
term_variables(Term, Vs) :-
	( is_output_list(Vs) -> true
	; throw(error(type_error(list,Vs),term_variables/2)) ),
	sepia_kernel:term_variables_lr(Term, Vs).
	

%-----------------------------------------------------------------------
% 8.10 All Solutions
% findall, bagof, setof: extra error checking
%-----------------------------------------------------------------------

:- export findall/3.
:- tool(findall/3,findall_/4).
findall_(Template, Goal, Instances, Module) :-
	( is_output_list(Instances) -> true
	; throw(error(type_error(list,Instances),findall/3)) ),
	( normalize_call(Goal, NormGoal) ->
	    eclipse_language:findall(Template, NormGoal, Instances)@Module
	;
	    throw(error(type_error(callable,Goal),findall/3))
	).

:- export bagof/3.
:- tool(bagof/3,bagof_/4).
bagof_(Template, QGoal, Instances, Module) :-
	( is_output_list(Instances) -> true
	; throw(error(type_error(list,Instances),bagof/3)) ),
	dequant(QGoal, Goal, QNormGoal, NormGoal),
	( normalize_call(Goal, NormGoal) ->
	    eclipse_language:bagof(Template, QNormGoal, Instances)@Module
	;
	    throw(error(type_error(callable,Goal),bagof/3))
	).

:- export setof/3.
:- tool(setof/3,setof_/4).
setof_(Template, QGoal, Instances, Module) :-
	( is_output_list(Instances) -> true
	; throw(error(type_error(list,Instances),setof/3)) ),
	dequant(QGoal, Goal, QNormGoal, NormGoal),
	( normalize_call(Goal, NormGoal) ->
	    eclipse_language:setof(Template, QNormGoal, Instances)@Module
	;
	    throw(error(type_error(callable,Goal),setof/3))
	).

    :- mode dequant(+,-,-,-).
    dequant(G, G, NG, NG) :- var(G), !.
    dequant(V^VG, G, V^VNG, NG) :- !,
	dequant(VG, G, VNG, NG).
    dequant(G, G, NG, NG).


%-----------------------------------------------------------------------
% 8.11 Stream selection and control
% open/3,4:	Restrictions on mode and options
%-----------------------------------------------------------------------

:- export open/3, open/4.
open(SourceSink, Mode, Stream) :-
	open(SourceSink, Mode, Stream, []).

open(SourceSink, Mode, Stream, Options) :-
	( Mode\==update -> true
	; throw(error(domain_error(io_mode,Mode),open/4))),
	( var(Stream) -> true
	; throw(error(uninstantiation_error(Stream),open/4))),
	check_stream_options(Options, Options),
	eclipse_language:open(SourceSink, Mode, Stream, Options).

    check_stream_options(Options, _) :- var(Options), !,
	throw(error(instantiation_error,open/4)).
    check_stream_options([], _) :- !.
    check_stream_options([Option|Options], All) :- !,
	check_stream_option(Option),
	check_stream_options(Options, All).
    check_stream_options(_Junk, All) :-
	throw(error(type_error(list,All),open/4)).

    check_stream_option(Option) :- var(Option), !,
	throw(error(instantiation_error,open/4)).
    check_stream_option(type(text)) ?- !.
    check_stream_option(type(binary)) ?- !.
    check_stream_option(reposition(true)) ?- !.
    check_stream_option(reposition(false)) ?- !.
    check_stream_option(alias(A)) ?- atom(A), !.
    check_stream_option(eof_action(error)) ?- !.
    check_stream_option(eof_action(eof_code)) ?- !.
    check_stream_option(eof_action(reset)) ?- !.
    check_stream_option(Junk) :-
	throw(error(domain_error(stream_option,Junk),open/4)).


:- export stream_property/2.
stream_property(Stream, Property) :-
	( var(Stream) -> true
	; is_handle(Stream) -> true
	; throw(error(domain_error(stream,Stream), stream_property/2))),
	current_stream(Stream),
	( var(Property) ->
	    (
		iso_only_stream_property(Stream, Property)
	    ;
		iso_ecl_stream_property(Property),
		Property =.. [Name,Value],
		get_stream_info(Stream, Name, Value)
	    )
	; iso_only_stream_property(Property) ->
	    iso_only_stream_property(Stream, Property)
	; iso_ecl_stream_property(Property) ->
	    Property =.. [Name, Value],
	    get_stream_info(Stream, Name, Value)
	;
	    error(6, stream_property(Stream, Property))
	).


%-----------------------------------------------------------------------
% 8.14 Term input/output
% read,read_term:	binary restriction and option error handling
% write_term etc:	binary restriction, options, option error handling
%-----------------------------------------------------------------------

:- export read/1.
:- tool(read/1, read_/2).
read_(Term, Module) :-
	read_term_(input, Term, [], Module).

:- export read/2.
:- tool(read/2, read_/3).
read_(Stream, Term, Module) :-
	read_term_(Stream, Term, [], Module).

:- export read_term/2.
:- tool(read_term/2, read_term_/3).
read_term_(Term, Options, Module) :-
	read_term_(input, Term, Options, Module).

:- export read_term/3.
:- tool(read_term/3, read_term_/4).
read_term_(Stream, Term, Options, Module) :-		% 8.14.1
	check_stream_or_alias_io_type(Stream, input, text),
	!,
	check_read_options(Options, Options),
	eclipse_language:read_term(Stream, Term, Options)@Module.
read_term_(Stream, Term, Options, Module) :-		% 8.14.1
	bip_error(read_term(Stream, Term, Options))@Module.

    check_read_options(Options, _) :- var(Options), !,
	throw(error(instantiation_error, read_term/3)).
    check_read_options([], _) :- !.
    check_read_options([O|Os], All) :- !,
	check_read_option(O),
	check_read_options(Os, All).
    check_read_options(_, All) :-
	throw(error(type_error(list,All), read_term/3)).

    check_read_option(Option) :- var(Option), !,
	throw(error(instantiation_error, read_term/3)).
    check_read_option(variables(_)) :- !.
    check_read_option(variable_names(_)) :- !.
    check_read_option(singletons(_)) :- !.
    check_read_option(Option) :-
	throw(error(domain_error(read_option,Option), read_term/3)).


% Restrictions on options
:- export write_term/2.
:- tool(write_term/2, write_term_/3).
write_term_(Term, Options, Module) :-
	write_term_(output, Term, Options, Module).

:- export write_term/3.
:- tool(write_term/3, write_term_/4).
write_term_(Stream, Term, Options, Module) :-		% 8.14.2
	check_stream_or_alias_io_type(Stream, output, text),
	!,
	check_write_options(Options, Options),
	eclipse_language:write_term(Stream, Term, Options)@Module.
write_term_(Stream, Term, Options, Module) :-		% 8.14.2
	bip_error(write_term(Stream, Term, Options))@Module.

    check_write_options(Options, _) :- var(Options), !,
	throw(error(instantiation_error, write_term/3)).
    check_write_options([], _) :- !.
    check_write_options([O|Os], All) :- !,
	check_write_option(O),
	check_write_options(Os, All).
    check_write_options(_, All) :-
	throw(error(type_error(list,All), write_term/3)).

    check_write_option(Option) :- nonground(Option), !,
	throw(error(instantiation_error, write_term/3)).
    check_write_option(quoted(false)) :- !.
    check_write_option(quoted(true)) :- !.
    check_write_option(ignore_ops(false)) :- !.
    check_write_option(ignore_ops(true)) :- !.
    check_write_option(numbervars(false)) :- !.
    check_write_option(numbervars(true)) :- !.
    check_write_option(Option) :-
	throw(error(domain_error(write_option,Option), write_term/3)).


%-----------------------------------------------------------------------
% 8.17 Implementation defined hooks (strict versions)
%-----------------------------------------------------------------------

:- export set_prolog_flag/2.
:- tool(set_prolog_flag/2, set_prolog_flag_/3).
set_prolog_flag_(Flag, Value, M) :-
	( (var(Flag);var(Value)) ->
	    error(4, set_prolog_flag(Flag, Value), M)
	; iso_only_flag(Flag) ->
	    set_iso_only_flag_(Flag, Value, M)
	; iso_ecl_flag(Flag) ->
	    set_flag(Flag, Value)@M
	;
	    error(6, set_prolog_flag(Flag, Value))@M
	).


:- export current_prolog_flag/2.
:- tool(current_prolog_flag/2, current_prolog_flag_/3).
current_prolog_flag_(Flag, Value, M) :-
	( var(Flag) ->
	    (
		iso_only_flag_(Flag, Value, M)
	    ;
		iso_ecl_flag(Flag),
		get_flag(Flag, Value)@M
	    )
	; atom(Flag) ->
	    ( iso_only_flag(Flag) ->
		iso_only_flag_(Flag, Value, M)
	    ; iso_ecl_flag(Flag) ->
		get_flag(Flag, Value)@M
	    ;
		error(6, current_prolog_flag(Flag, Value))@M
	    )
	;
	    error(5, current_prolog_flag(Flag, Value))@M
	).
	

%-----------------------------------------------------------------------
% 9. Evaluable functors
% - restricted set of operations
% - no user-defined operations
% - ISO errors
% The bulk of this is in iso_aux!
%-----------------------------------------------------------------------

:- export (is)/2.
:- tool((is)/2, is_/3).
is_(R, X, M) :-
	is_(R, X, M, iso_strict).

trans_is(Goal, Expanded) :-
	trans_is(Goal, Expanded, iso_strict).

:- inline((is)/2, trans_is/2).


:- export (>=)/2, (>)/2, (=<)/2, (<)/2, (=:=)/2, (=\=)/2.
:- tool((<)/2, (<)/3),
   tool((>)/2, (>)/3),
   tool((=<)/2, (=<)/3),
   tool((>=)/2, (>=)/3),
   tool((=:=)/2, (=:=)/3),
   tool((=\=)/2, (=\=)/3).

% redefine the comparisons, using the visible is/2
<(X,Y,M)   :- (X1 is X)@M, (Y1 is Y)@M, eclipse_language:(X1<Y1)@M.
>(X,Y,M)   :- (X1 is X)@M, (Y1 is Y)@M, eclipse_language:(X1>Y1)@M.
=<(X,Y,M)  :- (X1 is X)@M, (Y1 is Y)@M, eclipse_language:(X1=<Y1)@M.
>=(X,Y,M)  :- (X1 is X)@M, (Y1 is Y)@M, eclipse_language:(X1>=Y1)@M.
=:=(X,Y,M) :- (X1 is X)@M, (Y1 is Y)@M, eclipse_language:(X1=:=Y1)@M.
=\=(X,Y,M) :- (X1 is X)@M, (Y1 is Y)@M, eclipse_language:(X1=\=Y1)@M.

trans_compare(Goal, Expanded) :-
	trans_compare(Goal, Expanded, iso_strict).

:- inline((>=)/2, trans_compare/2).
:- inline((>)/2, trans_compare/2).
:- inline((=<)/2, trans_compare/2).
:- inline((<)/2, trans_compare/2).
:- inline((=:=)/2, trans_compare/2).
:- inline((=\=)/2, trans_compare/2).

