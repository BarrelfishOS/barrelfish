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
% Version:	$Id: iso_aux.ecl,v 1.3 2015/01/14 01:31:08 jschimpf Exp $
%
% IDENTIFICATION:	iso_aux.ecl
%
% AUTHOR:		Joachim Schimpf, Coninfer Ltd
%
% CONTENTS:		Auxiliaries for lib(iso) and lib(iso_strict)
%
% ----------------------------------------------------------------------

:- module(iso_aux).

:- pragma(system).

%-----------------------------------------------------------------------
% Arithmetic
% Works in 'iso' and 'iso_strict' mode
%-----------------------------------------------------------------------

:- export
	(**)/3,
	(^)/3,
	atan2/3,
	sign/2,
	log/2,
	floor/2,
	ceiling/2,
	round/2,
	float_integer_part/2,
	float_fractional_part/2.

**(X,Y,Z) :-
	( X >= 0 ->
	    eclipse_language:(Z is float(X^Y))
	; integer(Y) ->
	    eclipse_language:(Z is float(X^Y))
	;
	    throw(error(evaluation_error(undefined), **(X,Y,Z)))
	).

^(X,Y,Z) :-
	% error cases a,b,c,f,g handled by kernel's ^/2
	eclipse_language:(R is X^Y),
	( float(R), integer(X), integer(Y) ->
	    RAbs is abs(R),
	    ( RAbs > 1.0 ->
		% case (d) (R=inf, which should really be zero_divisor)
		throw(error(evaluation_error(undefined), ^(X,Y,Z)))
	    ; RAbs < 1.0 ->
		% case (e) extended to Y= -1
		throw(error(type_error(float,X), ^(X,Y,Z)))
	    ;
		Z is integer(R)
	    )
	;
	    Z=R
	).

sign(X,Y) :- eclipse_language:(Yi is sgn(X)),
	( float(X) -> eclipse_language:(Y is float(Yi))	% gives sign(-0.0,0.0)
	; rational(X) -> eclipse_language:(Y is rational(Yi))
	; breal(X) -> eclipse_language:(Y is breal(Yi))
	; Y = Yi
	).

log(X,Y) :- eclipse_language:(Y is ln(X)).

floor(X,Y) :- eclipse_language:(Y is integer(floor(X))).

ceiling(X,Y) :- eclipse_language:(Y is integer(ceiling(X))).

% ECLiPSe's round uses even/odd tie break!
round(X,Y) :- eclipse_language:(Y is integer(floor(X+0.5))).

float_integer_part(X,Y) :- eclipse_language:(Y is truncate(X)).

float_fractional_part(X,Y) :- eclipse_language:(Y is X-truncate(X)).

atan2(X,Y,Z) :-
	( X=:=0, Y=:=0 ->
	    throw(error(evaluation_error(undefined), atan2(X,Y,Z)))
	;
	    eclipse_language:(Z is atan(X,Y))
	).


% Non-expanded evaluation
% is_(Expression, Result, ContextModule, [iso|iso_strict])

:- export (is_)/4.

is_(_, X, _, _) :- var(X), !, throw(error(instantiation_error,(is)/2)).
is_(R, X, _, _) :- number(X), !, R=X.
is_(R, +X, M, V) :-		!, is_(X1,X,M,V), +(X1,R).
is_(R, -X, M, V) :-		!, is_(X1,X,M,V), -(X1,R).
is_(R, abs(X), M, V) :-		!, is_(X1,X,M,V), abs(X1,R).
is_(R, float(X), M, V) :- 	!, is_(X1,X,M,V), float(X1,R).
is_(R, \X, M, V) :-		!, is_(X1,X,M,V), \(X1,R).
is_(R, X + Y, M, V) :-		!, is_(X1,X,M,V), is_(Y1,Y,M,V), +(X1,Y1,R).
is_(R, X - Y, M, V) :-		!, is_(X1,X,M,V), is_(Y1,Y,M,V), -(X1,Y1,R).
is_(R, X * Y, M, V) :-		!, is_(X1,X,M,V), is_(Y1,Y,M,V), *(X1,Y1,R).
is_(R, X / Y, M, V) :-		!, is_(X1,X,M,V), is_(Y1,Y,M,V), /(X1,Y1,R).
is_(R, X // Y, M, V) :-		!, is_(X1,X,M,V), is_(Y1,Y,M,V), //(X1,Y1,R).
is_(R, X div Y, M, V) :-	!, is_(X1,X,M,V), is_(Y1,Y,M,V), div(X1,Y1,R).
is_(R, X mod Y, M, V) :-	!, is_(X1,X,M,V), is_(Y1,Y,M,V), mod(X1,Y1,R).
is_(R, X ^ Y, M, V) :-		!, is_(X1,X,M,V), is_(Y1,Y,M,V), iso_aux: ^(X1,Y1,R).
is_(R, min(X,Y), M, V) :- 	!, is_(X1,X,M,V), is_(Y1,Y,M,V), min(X1,Y1,R).
is_(R, max(X,Y), M, V) :- 	!, is_(X1,X,M,V), is_(Y1,Y,M,V), max(X1,Y1,R).
is_(R, X /\ Y, M, V) :-		!, is_(X1,X,M,V), is_(Y1,Y,M,V), /\(X1,Y1,R).
is_(R, X \/ Y, M, V) :-		!, is_(X1,X,M,V), is_(Y1,Y,M,V), \/(X1,Y1,R).
is_(R, xor(X,Y), M, V) :- 	!, is_(X1,X,M,V), is_(Y1,Y,M,V), xor(X1,Y1,R).
is_(R, X >> Y, M, V) :-		!, is_(X1,X,M,V), is_(Y1,Y,M,V), >>(X1,Y1,R).
is_(R, X << Y, M, V) :-		!, is_(X1,X,M,V), is_(Y1,Y,M,V), <<(X1,Y1,R).
is_(R, sin(X), M, V) :-		!, is_(X1,X,M,V), sin(X1,R).
is_(R, cos(X), M, V) :-		!, is_(X1,X,M,V), cos(X1,R).
is_(R, tan(X), M, V) :-		!, is_(X1,X,M,V), tan(X1,R).
is_(R, asin(X), M, V) :-	!, is_(X1,X,M,V), asin(X1,R).
is_(R, acos(X), M, V) :-	!, is_(X1,X,M,V), acos(X1,R).
is_(R, atan(X), M, V) :-	!, is_(X1,X,M,V), atan(X1,R).
is_(R, exp(X), M, V) :-		!, is_(X1,X,M,V), exp(X1,R).
is_(R, sqrt(X), M, V) :-	!, is_(X1,X,M,V), sqrt(X1,R).
is_(R, pi, _, _) :-		!, R is pi.
is_(R, atan2(X,Y), M, V) :-   	!, is_(X1,X,M,V), is_(Y1,Y,M,V), iso_aux:atan2(X1,Y1,R).
is_(R, **(X,Y), M, V) :-	!, is_(X1,X,M,V), is_(Y1,Y,M,V), iso_aux: **(X1,Y1,R).
is_(R, sign(X), M, V) :-	!, is_(X1,X,M,V), iso_aux:sign(X1,R).
is_(R, log(X), M, V) :-		!, is_(X1,X,M,V), iso_aux:log(X1,R).
is_(R, floor(X), M, V) :-	!, is_(X1,X,M,V), iso_aux:floor(X1,R).
is_(R, ceiling(X), M, V) :-	!, is_(X1,X,M,V), iso_aux:ceiling(X1,R).
is_(R, round(X), M, V) :-	!, is_(X1,X,M,V), iso_aux:round(X1,R).
is_(R, truncate(X), M, V) :-	!, is_(X1,X,M,V), sepia_kernel:fix(X1,R).
is_(R, float_integer_part(X), M, V) :- !, is_(X1,X,M,V), iso_aux:float_integer_part(X1,R).
is_(R, float_fractional_part(X), M, V) :- !, is_(X1,X,M,V), iso_aux:float_fractional_part(X1,R).
is_(R, X, M, V) :-
	functor(X, F, A), A1 is A+1,
	( V\==iso_strict, is_predicate(F/A1)@M ->
	    eclipse_language:(R is X)@M
	;
	    throw(error(type_error(evaluable, F/A), (is)/2))
	).


% Inlining of arithmetic

:- export arith_map/4.
arith_map(+_, R, [X], sepia_kernel: +(X,R)).
arith_map(-_, R, [X], sepia_kernel: -(X,R)).
arith_map(abs(_), R, [X], sepia_kernel: abs(X,R)).
arith_map(float(_), R, [X], sepia_kernel: float(X,R)).
arith_map(\_, R, [X], sepia_kernel: \(X,R)).
arith_map(_ + _, R, [X,Y], sepia_kernel: +(X, Y,R)).
arith_map(_ - _, R, [X,Y], sepia_kernel: -(X, Y,R)).
arith_map(_ * _, R, [X,Y], sepia_kernel: *(X, Y,R)).
arith_map(_ / _, R, [X,Y], sepia_kernel: /(X, Y,R)).
arith_map(_ // _, R, [X,Y], sepia_kernel: //(X, Y,R)).
arith_map(_ div _, R, [X,Y], sepia_kernel: div(X, Y,R)).
arith_map(_ mod _, R, [X,Y], sepia_kernel: mod(X, Y,R)).
arith_map(_ ^ _, R, [X,Y], iso_aux: ^(X, Y,R)).
arith_map(min(_,_), R, [X,Y], sepia_kernel: min(X, Y,R)).
arith_map(max(_,_), R, [X,Y], sepia_kernel: max(X, Y,R)).
arith_map(_ /\ _, R, [X,Y], sepia_kernel: /\(X, Y,R)).
arith_map(_ \/ _, R, [X,Y], sepia_kernel: \/(X, Y,R)).
arith_map(xor(_,_), R, [X,Y], sepia_kernel: xor(X, Y,R)).
arith_map(_ >> _, R, [X,Y], sepia_kernel: >>(X, Y,R)).
arith_map(_ << _, R, [X,Y], sepia_kernel: <<(X, Y,R)).
arith_map(sin(_), R, [X], sepia_kernel: sin(X,R)).
arith_map(cos(_), R, [X], sepia_kernel: cos(X,R)).
arith_map(tan(_), R, [X], sepia_kernel: tan(X,R)).
arith_map(atan2(_,_), R, [X,Y], iso_aux: atan2(X,Y,R)).
arith_map(asin(_), R, [X], sepia_kernel: asin(X,R)).
arith_map(acos(_), R, [X], sepia_kernel: acos(X,R)).
arith_map(atan(_), R, [X], sepia_kernel: atan(X,R)).
arith_map(exp(_), R, [X], sepia_kernel: exp(X,R)).
arith_map(sqrt(_), R, [X], sepia_kernel: sqrt(X,R)).
arith_map(pi, R, [], sepia_kernel:(R is pi)).
arith_map(**(_,_), R, [X,Y], iso_aux: **(X,Y,R)).
arith_map(sign(_), R, [X], iso_aux:sign(X,R)).
arith_map(log(_), R, [X], iso_aux:log(X,R)).
arith_map(floor(_), R, [X], iso_aux:floor(X,R)).
arith_map(ceiling(_), R, [X], iso_aux:ceiling(X,R)).
arith_map(round(_), R, [X], iso_aux:round(X,R)).
arith_map(truncate(_), R, [X], sepia_kernel:fix(X,R)).
arith_map(float_integer_part(_), R, [X], iso_aux:float_integer_part(X,R)).
arith_map(float_fractional_part(_), R, [X], iso_aux:float_fractional_part(X,R)).


% trans_is(Goal, Expanded, [iso|iso_strict])
:- export trans_is/3.
trans_is(Res is Expr, Code, Version) :-
	trans_is(Expr, Res, Code, Version).

    trans_is(Expr, Res, Code, _) :-
	number(Expr),
	Code = (Res = Expr).
    trans_is(Expr, Res, Code, Version) :-
	callable(Expr),
	trans_function(Expr, Res, Call, Code, Call, Version).


:- export trans_compare/3.
trans_compare(In, Code, Version) :-
	functor(In, F, N),
	arg(1, In, X),
	arg(2, In, Y),
	functor(Out, F, N),
	arg(1, Out, RX),
	arg(2, Out, RY),
	trans_expr(X, RX, Code, Code1, Version),
	trans_expr(Y, RY, Code1, eclipse_language:Out, Version),
	Out \== In.		% fail when nothing changed


    % transform a sub-expression:
    % The result variable Res is assumed to be "fresh" and may be unified!
    trans_expr(Expr, Res, Code, NextCode, Version) :-
	callable(Expr),
	!,
	trans_function(Expr, Res, Call, Code, (Call,NextCode), Version).
    trans_expr(Expr, Res, Code, NextCode, _) :-
	%  var(Expr) ; number(Expr) ; and error cases
	Res = Expr,			% bind at transformation time
	Code = NextCode.		% no code


    trans_function(Expr, Res, Call, Code0, Code, Version) :-
	( arith_map(Expr, Res, CallArgs, Call) ->
	    (
		foreacharg(E,Expr),
		foreach(A,CallArgs),
		fromto(Code0,Code1,Code2,Code),
		param(Version)
	    do
		trans_expr(E, A, Code1, Code2, Version)
	    )
	;
	    % allow calling generalised functions if not iso_strict
	    Version \== iso_strict,
	    Code0 = Code,
	    expand_goal(Res is Expr, CallSub),	% eclipse_language expansion!
	    % make the expected error at least for the top level expression
	    functor(Expr, F, A), A1 is A+1,
	    Call = catch(CallSub,
	    	error(existence_error(procedure, F/A1), _Module),
		throw(error(type_error(evaluable, F/A), (is)/2))
	    )
	).


%-----------------------------------------------------------------------
% Support code for both iso_light and iso_strict
%-----------------------------------------------------------------------

:-export encoding_type/2.
encoding_type(octet, Type) :- !, Type=binary.
encoding_type(bytes, Type) :- !, Type=binary.
encoding_type(_, text).

% ISO stream properties that map directly to ECLiPSe ones
:- export iso_ecl_stream_property/1.
iso_ecl_stream_property(mode(_)).
iso_ecl_stream_property(end_of_stream(_)).
iso_ecl_stream_property(eof_action(_)).
iso_ecl_stream_property(reposition(_)).

% ISO only stream properties
:- export iso_only_stream_property/1.
iso_only_stream_property(file_name(_)).
iso_only_stream_property(input).
iso_only_stream_property(output).
iso_only_stream_property(alias(_)).
iso_only_stream_property(position(_)).
iso_only_stream_property(type(_)).

:- export iso_only_stream_property/2.
:- mode iso_only_stream_property(+,?).
iso_only_stream_property(Stream, file_name(F)) :-	% 8.11.8 and 7.10.2.13
	get_stream_info(Stream, device, file),
	get_stream_info(Stream, name, F).
iso_only_stream_property(Stream, input) :-
	get_stream_info(Stream, input, true).
iso_only_stream_property(Stream, output) :-
	get_stream_info(Stream, output, true).
iso_only_stream_property(Stream, alias(Alias)) :-
	% VERY inefficient for var(Alias)!
	( var(Alias) -> current_atom(Alias) ; atom(Alias) ),
	current_stream(Alias),
	get_stream(Alias, Stream).
iso_only_stream_property(Stream, position(P)) :-
	at(Stream, P).
iso_only_stream_property(Stream, type(Type)) :-
	get_stream_info(Stream, encoding, Encoding),
	encoding_type(Encoding, Type).


:- export is_output_list/1.
is_output_list(Xs) :- var(Xs), !.
is_output_list([]).
is_output_list([_|Xs]) :- is_output_list(Xs).


:- import set_bip_error/1 from sepia_kernel.

:- export check_stream_or_alias/1.
check_stream_or_alias(Stream) :-
	( is_handle(Stream) -> true
	; atom(Stream) -> true
	; var(Stream) -> set_bip_error(4)
	; set_bip_error(6)
	).

:- export check_stream_or_alias_io_type/3.
check_stream_or_alias_io_type(Stream, IO, Type) :-
	check_stream_or_alias(Stream),
	( current_stream(Stream) -> true ; set_bip_error(193) ),
	( get_stream_info(Stream, IO, true) -> true ; set_bip_error(192) ),
	get_stream_info(Stream, encoding, E),
	encoding_type(E, Actual),
	( Actual=Type -> true ; set_bip_error(192) ).


:- export normalize_body/2.
:- mode normalize_body(?,-).
normalize_body(Goal, call(Goal)) :- var(Goal), !.
normalize_body((G1,G2), (NG1,NG2)) :- !,
	normalize_body(G1, NG1),
	normalize_body(G2, NG2).
normalize_body((G1;G2), (NG1;NG2)) :- !,
	normalize_body(G1, NG1),
	normalize_body(G2, NG2).
normalize_body((G1->G2), (NG1->NG2)) :- !,
	normalize_body(G1, NG1),
	normalize_body(G2, NG2).
normalize_body(Goal, Goal) :- callable(Goal).


%-----------------------------------------------------------------------
% Prolog flags
%-----------------------------------------------------------------------

:- local variable(unknown, error).

:- set_event_handler(68, undefined_procedure_handler/4).
undefined_procedure_handler(E, Goal, CM, LM) :-
	getval(unknown, Unknown),
	( Unknown==error ->
	    error(default(E), Goal, CM)@LM
	;
	    Unknown==warning,	% else fail
	    ( CM == LM -> QGoal = Goal ; QGoal = LM:Goal ),
	    printf(warning_output,
		'WARNING: calling an undefined procedure %w in module %w%n',
		[QGoal,CM]),
	    fail
	).

:- export iso_ecl_flag/1.
iso_ecl_flag(bounded).
iso_ecl_flag(min_integer).
iso_ecl_flag(max_integer).

:- export iso_only_flag/1.
iso_only_flag(char_conversion).
iso_only_flag(debug).
iso_only_flag(double_quotes).
iso_only_flag(integer_rounding_function).
iso_only_flag(max_arity).
iso_only_flag(unknown).

:- export iso_only_flag_/3.
iso_only_flag_(char_conversion, off, _M).
iso_only_flag_(debug, Value, _M) :-
	get_flag(debugging, D),
	( D = creep -> Value = on
	; D = leap -> Value = on
	; Value = off ).
iso_only_flag_(double_quotes, Value, M) :-
	( get_chtab(0'", Quote)@M, quote(Value, Quote) -> true
	; Value = unknown ).
iso_only_flag_(integer_rounding_function, toward_zero, _M).
iso_only_flag_(max_arity, unbounded, _M).
iso_only_flag_(unknown, Value, _M) :-
	getval(unknown, Value).

:- export set_iso_only_flag_/3.
set_iso_only_flag_(char_conversion, Value, M) :- !,
	error(30, set_prolog_flag(char_conversion, Value))@M.
set_iso_only_flag_(debug, Value, M) :- !,
	( Value == on -> set_flag(debugging, creep)
	; Value == off -> set_flag(debugging, nodebug)
	; error(6, set_prolog_flag(debug, Value))@M ).
set_iso_only_flag_(double_quotes, Value, M) :- !,
        ( quote(Value, Quote) -> local(chtab(0'", Quote))@M
	; error(6, set_prolog_flag(double_quotes, Value))@M).
set_iso_only_flag_(integer_rounding_function, Value, M) :- !,
	error(30, set_prolog_flag(integer_rounding_function, Value))@M.
set_iso_only_flag_(max_arity, Value, M) :- !,
	error(30, set_prolog_flag(max_arity, Value))@M.
set_iso_only_flag_(unknown, Value, M) :- !,
	( unknown_value(Value) -> setval(unknown, Value)
	; error(6, set_prolog_flag(unknown, Value))@M).

quote(atom,   atom_quote).
quote(string, string_quote).
quote(codes,  list_quote).
quote(chars,  chars_quote).

unknown_value(error).
unknown_value(fail).
unknown_value(warning).

