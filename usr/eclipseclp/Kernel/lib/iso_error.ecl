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
% The Original Code is  The ECLiPSe Library iso_error.
% The Initial Developer of the Original Code is  Coninfer Ltd.
% Portions created by the Initial Developer are
% Copyright (C) 2010-2013 Coninfer Ltd
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: iso_error.ecl,v 1.6 2015/01/14 01:31:08 jschimpf Exp $
%
% IDENTIFICATION:	iso_error.ecl
%
% AUTHOR:		Joachim Schimpf, Coninfer Ltd
%
% CONTENTS:		Map ECLiPSe error to ISO errors
%
% ----------------------------------------------------------------------

%
% ISO errors
% 
% 
% instantiation_error
%     instantiation_error.
% 
% type_error(ValidType, Culprit)
%     type_error(list, [foo|bar]).
% 
% domain_error(ValidDomain, Culprit)
%     domain_error(non_empty_list, []).
% 
% existence_error(ObjectType, Culprit)
%     ObjectType in { procedure, source_sink, stream } 
%     existence_error(procedure, ex_nihilo/0).
% 
% permission_error(Operation, PermissionType, Culprit)
%     Operation in { access, create, input, modify, open, output, reposition },
%     PermissionType in { binary_stream, flag, operator, past_end_of_stream, private_procedure, static_procedure, source_sink, stream, text_stream } 
%     permission_error(open, source_sink, '/etc/shadow')
% 
% representation_error(Flag)
%     Flag in { character, character_code, in_character_code, max_arity, max_integer, min_integer } 
% 
% evaluation_error(Error)
%     Error in { float_overflow, int_overflow, undefined, underflow, zero_divisor } 
% 
% resource_error(Resource)
%     resource_error(stack).
% 
% syntax_error(Imp_dep_atom)
%     syntax_error(operator_expected)>
% 
% system_error 
% 
% uninstantiation_error(Culprit)
% 
% 
% NOTES ON STANDARD:
% - different errors required from abolish/1 and current_predicate/1
%   even though they both accept predicate_indicator
% - inconsistent errors required for closed stream handle (domain/existence)
%

:- module(iso_error, [], [iso_light]).

:- use_module(iso_aux).


%----------------------------------------------------------------------
% instantiation_error
%----------------------------------------------------------------------

:- set_event_handler(4, iso_instantiation_error_handler/2).
iso_instantiation_error_handler(4, Culprit_) :-
	real_culprit(Culprit_, Culprit),
	throw_error(instantiation_error, Culprit).

throw_error(Exception, Culprit) :-
	functor(Culprit, F, N),
	ImpDefTerm = F/N,
	throw(error(Exception, ImpDefTerm)).


%----------------------------------------------------------------------
% type_error
% Predicate table see below
%----------------------------------------------------------------------

:- set_event_handler(5, iso_type_error_handler/2).
%:- set_event_handler(24, iso_type_error_handler/2).
iso_type_error_handler(E, Culprit_) :-
	real_culprit(Culprit_, Culprit),
	( type_check(Culprit, Exception) ->
	    throw_error(Exception, Culprit)
	; domain_check(Culprit, Exception) ->
	    throw_error(Exception, Culprit)
	;
	    error(default(E), Culprit)
	).

type_check(Culprit, Exception) :-
	Culprit = functor(_,F,C), atomic(F), integer(C), C>0, !,
	type_check_arg(1, 3, functor(nonvar,atom,integer), Culprit, Exception).
type_check(_ =.. List, Exception) :- !,
	( List == [] -> Exception = domain_error(non_empty_list,List)
	; is_list(List) ->
	    ( List = [H] -> Exception = type_error(atomic,H)
	    ; List = [H|_] -> Exception = type_error(atom,H)
	    )
	; Exception = type_error(list,List)
	).
type_check(current_predicate(P), Exception) ?- !,
	% different from same type in abolish/1, but required...
	Exception = type_error(predicate_indicator,P).
type_check(Culprit, Exception) :-
	compound(Culprit), functor(Culprit, call, _), !,
	arg(1, Culprit, Closure),
	( callable(Closure) -> true
	; Exception = type_error(callable,Closure)
	).
type_check(Culprit, Exception) :-
	functor(Culprit, F, N),
	functor(Type, F, N),
	type(Type),	% fail if we have no type spec
	catch(type_check_arg(1, N, Type, Culprit, Exception),
		error(Exception,_), true).

    % Fails if everything is ok
    % Succeeds with ExceptionTerm if whole term is wrong
    % Throws error/2 term if subterm error detected
    type_check_arg(I, N, Type, Culprit, ExceptionTerm) :-
    	I =< N,
	arg(I, Type, ArgType),
	arg(I, Culprit, ArgVal),
	( (var(ArgVal) ; is_of_type(ArgVal, ArgType)) ->
	    I1 is I+1,
	    type_check_arg(I1, N, Type, Culprit, ExceptionTerm)
	;
	    ExceptionTerm = type_error(ArgType,ArgVal)
	).


:- mode is_of_type(+,++).

is_of_type(_X, term)	:- !.
is_of_type(X, nonvar) :- !, nonvar(X).
is_of_type(X, list)	:- !, is_list(X).
is_of_type(X, atomic)	:- !, atomic(X).
is_of_type(X, number)	:- !, number(X).
is_of_type(X, evaluable) :- !, once (number(X);callable(X)).
is_of_type(F/N, predicate_indicator) ?- !,
	( atom(F) ->
	    ( integer(N) ->
	    	( N>=0 -> true ; throw(error(domain_error(not_less_than_zero,N),_)) )
	    ; throw(error(type_error(integer,N),_)))
	;
	    throw(error(type_error(atom,F),_))
	).
is_of_type(X, callable) :- !, callable(X).
is_of_type(X, byte) :- !, 0=<X, X=<255.
is_of_type(X, character) :- !, char(X).
is_of_type(X, character_code) :- !,
	( integer(X) ->
	    ( char_code(X) -> true
	    ; throw(error(representation_error(character_code),_))
	    )
	;
	    throw(error(type_error(integer,X),_))
	).
is_of_type(X, in_byte) :- !, -1=<X, X=<255.
is_of_type(X, in_character) :- !, once (char(X);X==end_of_file).
is_of_type(X, in_character_code) :- !, once (char_code(X); -1==X).
is_of_type(Xs, character_list) :- !,
	( is_list(Xs) ->
	    ( foreach(X,Xs) do
	    	( char(X) -> true
		; throw(error(type_error(character,X),_)))
	    )
	;
	    throw(error(type_error(list,Xs),_))
	).
is_of_type(Xs, character_code_list) ?- !,
	( is_list(Xs) ->
	    ( foreach(X,Xs) do
		( char_code(X) -> true
		; throw(error(representation_error(character_code),_))
		)
	    )
	;
	    throw(error(type_error(list,Xs),_))
	).
is_of_type(C, clause) :- !,
	( callable(C) -> true ; throw(error(type_error(callable,C),_)) ),
	( C = (H:-B) ->
	    ( callable(H) -> true ; throw(error(type_error(callable,H),_)) ),
	    % we assume we definitely had a type error, it must be in the body
	    % (otherwise we'd need recursive check here)
	    throw(error(type_error(callable,B),_))
	;
	    true
	).
%is_of_type(_DUBIOUS, flag) :- !.
is_of_type(X, head) :- !, callable(X).
is_of_type(Xs, atom_or_atom_list) :- !,
	( atom(Xs) ->
	    true
	; is_list(Xs) ->
	    ( foreach(X,Xs) do
	    	( atom(X) -> true
		; throw(error(type_error(atom,X),_)))
	    )
	;
	    % standard strangely requires 'list' here, not atom (8.14.3)
	    throw(error(type_error(list,Xs),_))
	).
is_of_type(X, Type)	:- type_of(X, Type).


%----------------------------------------------------------------------
% domain_error
% Predicate table see below
%----------------------------------------------------------------------

:- set_event_handler(6, iso_domain_error_handler/2).
iso_domain_error_handler(_, arg(I,_,_)) :- integer(I), I>=0, !, fail.
iso_domain_error_handler(E, Culprit) :-
	iso_type_error_handler(E, Culprit).


:- mode domain_check(+,-).
domain_check(get_flag(F,V), Exception) :- !,
	domain_check(current_prolog_flag(F,V), Exception).
domain_check(current_prolog_flag(F,_), domain_error(prolog_flag, F)) :- !.
domain_check(set_flag(F,V), Exception) :- !,
	% error must be from iso_light or eclipse_language
	( get_flag(F, _) ->
	    Exception = domain_error(flag_value, F+V)
	;
	    Exception = domain_error(prolog_flag, F)
	).
domain_check(set_prolog_flag(F,V), Exception) :- !,
	( iso_only_flag(F) ->
	    Exception = domain_error(flag_value, F+V)
	;
	    Exception = domain_error(prolog_flag, F)
	).
domain_check(functor(_,_,C), representation_error(max_arity)) :-
	C >= 0, !.	% if error 6 is raised, must be max-arity
domain_check(Culprit, Exception) :-
	functor(Culprit, F, N),
	functor(Type, F, N),
	domain(Type),	% fail if we have no domain spec
	catch(domain_check_arg(1, N, Type, Culprit, Exception),
		error(Exception,_), true).

    % Fails if everything is ok
    % Succeeds with Exception if whole term is wrong
    % Throws error/2 term if subterm error detected
    domain_check_arg(I, N, Type, Culprit, ExceptionTerm) :-
    	I =< N,
	arg(I, Type, ArgType),
	arg(I, Culprit, ArgVal),
	( (var(ArgVal) ; is_of_domain(ArgVal, ArgType)) ->
	    I1 is I+1,
	    domain_check_arg(I1, N, Type, Culprit, ExceptionTerm)
	;
	    ExceptionTerm = domain_error(ArgType,ArgVal)
	).


% domains -- can assume the type is checked already
:- mode is_of_domain(+,++).

is_of_domain(Xs, close_options) ?-
	( foreach(X,Xs) do
	    ( is_of_domain(X, close_option) -> true
	    ; throw(error(domain_error(close_option,X),_))
	    )
	).
is_of_domain(X, close_option) ?- close_option(X), !.

%is_of_domain(X, flag_value) ?- !. % done in domain_check/2

is_of_domain(read, io_mode) ?- !.
is_of_domain(write, io_mode) ?- !.
is_of_domain(append, io_mode) ?- !.
is_of_domain(append, io_mode) ?- !.	% implementation specific

is_of_domain(X, non_empty_list) ?- !, X\==[].

is_of_domain(X, not_less_than_zero) ?- !, X >= 0.

is_of_domain(X, operator_priority) ?- !, integer(X), 0 =< X, X =< 1200.

is_of_domain(fx, operator_specifier) ?- !.
is_of_domain(fy, operator_specifier) ?- !.
is_of_domain(xfx, operator_specifier) ?- !.
is_of_domain(xfy, operator_specifier) ?- !.
is_of_domain(yfx, operator_specifier) ?- !.
is_of_domain(xf, operator_specifier) ?- !.
is_of_domain(yf, operator_specifier) ?- !.

is_of_domain(_/N, predicate_indicator) ?- !,
	% range error should only be thrown in this case
	throw(error(domain_error(not_less_than_zero),N)).

%is_of_domain(X, prolog_flag) ?- !. % done in domain_check/2

is_of_domain(Xs, read_options_list) ?- !,
	( foreach(X,Xs) do 
	    ( is_of_domain(X, read_option) -> true
	    ; throw(error(domain_error(read_option,X),_))
	    )
	).

is_of_domain(variables(_), read_option) ?- !.
is_of_domain(variable_names(_), read_option) ?- !.
is_of_domain(singletons(_), read_option) ?- !.

is_of_domain(X, source_sink) ?- source_sink(X), !.

is_of_domain(X, stream) ?- is_handle(X), !.

is_of_domain(type(text), stream_option) ?- !.
is_of_domain(type(binary), stream_option) ?- !.
is_of_domain(reposition(B), stream_option) ?- bool(B), !.
is_of_domain(alias(A), stream_option) ?- is_of_domain(A, stream_or_alias), !.
is_of_domain(eof_action(error), stream_option) ?- !.
is_of_domain(eof_action(eof_code), stream_option) ?- !.
is_of_domain(eof_action(reset), stream_option) ?- !.
is_of_domain(eof_action(reset), stream_option) ?- !.

is_of_domain(X, stream_or_alias) ?- atom(X), !.
is_of_domain(X, stream_or_alias) ?- integer(X), !.
is_of_domain(X, stream_or_alias) ?-
	is_handle(X).	% should test for stream-handle
%	catch(current_stream(S), _, fail), !.

is_of_domain(X, stream_position) ?- integer(X), !.

is_of_domain(X, stream_property) ?- is_of_domain(X, stream_option), !.
is_of_domain(file_name(_), stream_property) ?- !.
is_of_domain(mode(M), stream_property) ?- is_of_domain(M, io_mode), !.
is_of_domain(input, stream_property) ?- !.
is_of_domain(output, stream_property) ?- !.
is_of_domain(position(P), stream_property) ?- is_of_domain(P, stream_position), !.

is_of_domain(Xs, write_options_list) ?- !,
	( foreach(X,Xs) do 
	    ( is_of_domain(X, write_option) -> true
	    ; throw(error(domain_error(write_option,X),_))
	    )
	).

is_of_domain(quoted(B), write_option) ?- bool(B), !.
is_of_domain(ignore_ops(B), write_option) ?- bool(B), !.
is_of_domain(numbervars(B), write_option) ?- bool(B), !.

is_of_domain(_, any).


bool(true) ?- true.
bool(false) ?- true.

char(X) :- atom(X), atom_length(X, 1).

char_code(X) :- integer(X), 0=<X, X=<255.	% 2147483647.

close_option(force(B)) ?- bool(B).

source_sink(X) ?- atom(X).
source_sink(X) ?- string(X).
source_sink(string(S)) ?- string(S).
source_sink(queue(S)) ?- string(S).
source_sink(fd(I)) ?- integer(I).


%----------------------------------------------------------------------
% existence error
%----------------------------------------------------------------------

:- set_event_handler(193, iso_illegal_stream_handler/2).
iso_illegal_stream_handler(_, Culprit) :-
	bip_stream(Culprit, Stream),
	!,
	( atom(Stream),
	  real_culprit(Culprit,Culprit1),
	  domain_check(Culprit1, Exception) ->
	    % ISO prefers domain errors here
	    throw_error(Exception, Culprit1)
	;
	    throw_error(existence_error(stream,Stream), Culprit)
	).
iso_illegal_stream_handler(E, Culprit) :-
	error(default(E), Culprit).

    bip_stream(set_stream(_,S), S).	% for set_{in,out}put/1
    bip_stream(get_stream(_,S), S).	% for get_{in,out}put/1
    bip_stream(Culprit, S) :-
    	compound(Culprit),
    	arg(1, Culprit, S).		% Stream usually 1st argument!


:- set_event_handler(68, iso_undefined_procedure_handler/4).
iso_undefined_procedure_handler(_E, Goal, CM, LM) :-
	functor(Goal, N, A),
	( CM == LM -> Pred = N/A ; Pred = LM:N/A ),
	current_prolog_flag(unknown, Unknown),
	( Unknown==error ->
	    throw(error(existence_error(procedure,Pred), CM))
	;
	    Unknown==warning,	% else fail
	    printf(warning_output,
		'WARNING: procedure does not exist: %w in module %w%n',
		[Pred,CM]),
	    fail
	).


%----------------------------------------------------------------------
% representation_error
%----------------------------------------------------------------------

%----------------------------------------------------------------------
% permission_error
%----------------------------------------------------------------------

:- set_event_handler(30, iso_read_only_flag_handler/2).
iso_read_only_flag_handler(_, set_flag(Flag,_)) ?- !,
	throw(error(permission_error(modify, flag, Flag), set_flag/2)).
iso_read_only_flag_handler(_, set_prolog_flag(Flag,_)) ?- !,
	throw(error(permission_error(modify, flag, Flag), set_prolog_flag/2)).
iso_read_only_flag_handler(E, Culprit) :-
	error(default(E), Culprit).


:- set_event_handler(100, iso_other_module_handler/2).
iso_other_module_handler(_, Culprit) :-
	( dynamic_op(Culprit, Op, Why, Clause) ->
	    clause_pred(Clause, Pred),
	    functor(Culprit, F, N),
	    throw(error(permission_error(Op, Why, Pred), F/N))
	;
	    % should not happen
	    throw(error(permission_error(unknown, private_procedure, unknown), Culprit))
	).


%----------------------------------------------------------------------

:- set_event_handler(70, iso_undefined_dynamic_handler/3).
iso_undefined_dynamic_handler(_, clause(_,_), _) ?- !,
	fail.
iso_undefined_dynamic_handler(_, retract(_), _) ?- !,
	fail.
iso_undefined_dynamic_handler(_, retractall(_), _) ?- !.
iso_undefined_dynamic_handler(_, retract_all(_), _) ?- !.
iso_undefined_dynamic_handler(E, Culprit, Module) :-
	error(default(E), Culprit, Module).


%----------------------------------------------------------------------

:- set_event_handler(63, iso_not_dynamic_handler/2).
iso_not_dynamic_handler(_, Culprit) ?- !,
	( dynamic_op(Culprit, Op, Why, Clause) ->
	    clause_pred(Clause, Pred),
	    functor(Culprit, F, N),
	    throw(error(permission_error(Op, Why, Pred), F/N))
	;
	    % should not happen
	    throw(error(permission_error(unknown, static_procedure, unknown), Culprit))
	).

    dynamic_op(clause(Head, _), access, private_procedure, Head).	% ISO
    dynamic_op(asserta(Clause), modify, static_procedure, Clause).	% ISO
    dynamic_op(assertz(Clause), modify, static_procedure, Clause).	% ISO
    dynamic_op(retract(Clause), modify, static_procedure, Clause).	% ISO
    dynamic_op(retractall(Head), modify, static_procedure, Head).	% ISO
    dynamic_op(abolish(F/N), modify, static_procedure, Head) :-		% ISO
    	functor(Head,F,N).
    dynamic_op(clause(Clause), access, private_procedure, Clause).	% ECLiPSe
    dynamic_op(assert(Clause), modify, static_procedure, Clause).	% ECLiPSe
    dynamic_op(retract_all(Head), modify, static_procedure, Head).	% ECLiPSe

    clause_pred((H:-_), Pred) ?- !, functor(H, F, N), Pred=F/N.
    clause_pred(H, F/N) :- functor(H, F, N).


%----------------------------------------------------------------------

:- set_event_handler(170, iso_system_interface_error_handler/4).
iso_system_interface_error_handler(_, open(File,_Mode,_Stream), _CM, _LM) ?-
	errno_id(Msg),
	( Msg == `No such file or directory`, !,
	    throw(error(existence_error(source_sink,File),open/4))
	; Msg == `Permission denied`, !,
	    throw(error(permission_error(open,source_sink,File),open/4))
	).
iso_system_interface_error_handler(E, Culprit, Module, LM) :-
	error(default(E), Culprit, Module)@LM.


%----------------------------------------------------------------------

% "illegal stream mode" -> domain_error
% "illegal stream mode" -> permission_error
:- set_event_handler(192, iso_stream_mode_handler/2).
iso_stream_mode_handler(E, open(SS,Mode,S)) ?- !,
	iso_stream_mode_handler(E, open(SS,Mode,S,[])).
iso_stream_mode_handler(_, open(_,Mode,_,Options)) ?- !,
	( is_of_domain(Mode,io_mode) -> true
	; throw(error(domain_error(io_mode,Mode),open/4)) ),
	( foreach(Option,Options) do
	    ( Option=alias(A), current_stream(A) ->
		throw(error(permission_error(open,source_sink,Option),open/4))
	    ; Option=reposition(true) ->
		throw(error(permission_error(open,source_sink,Option),open/4))
	    ;
		true
	    )
	).
%iso_stream_mode_handler(_, flush(S)) ?- !,
%	throw(error(permission_error(output,stream,S),flush/1)).
iso_stream_mode_handler(_, readvar(S,_,_,_))	?- !, throw_pe(S, input, binary, readvar/3).
iso_stream_mode_handler(_, write_(_,_))		?- !, get_stream(input,S), throw_pe(S, output, binary, write/1).
iso_stream_mode_handler(_, write_(S,_,_))	?- !, throw_pe(S, output, binary, write/2).
iso_stream_mode_handler(_, writeq_(_,_))	?- !, get_stream(input,S), throw_pe(S, output, binary, writeq/2).
iso_stream_mode_handler(_, writeq_(S,_,_))	?- !, throw_pe(S, output, binary, writeq/2).
iso_stream_mode_handler(_, write_canonical_(_,_)) ?- !, get_stream(input,S), throw_pe(S, output, binary, write_canonical/2).
iso_stream_mode_handler(_, write_canonical_(S,_,_)) ?- !, throw_pe(S, output, binary, write_canonical/2).
iso_stream_mode_handler(_, nl(S))		?- !, throw_pe(S, output, binary, nl/1).
iso_stream_mode_handler(_, set_input(S))	?- !, throw_pe(S, input, binary, set_input/1).
iso_stream_mode_handler(_, set_output(S))	?- !, throw_pe(S, output, binary, set_output/1).
iso_stream_mode_handler(_, flush_output(S))	?- !, throw_pe(S, output, binary, flush_output/1).
iso_stream_mode_handler(_, get_byte(S,_))	?- !, throw_pe(S, input, text, get_byte/2).
iso_stream_mode_handler(_, get_char(S,_))	?- !, throw_pe(S, input, binary, get_char/2).
iso_stream_mode_handler(_, get_code(S,_))	?- !, throw_pe(S, input, binary, get_code/2).
iso_stream_mode_handler(_, peek_byte(S,_))	?- !, throw_pe(S, input, text, peek_byte/2).
iso_stream_mode_handler(_, peek_char(S,_))	?- !, throw_pe(S, input, binary, peek_char/2).
iso_stream_mode_handler(_, peek_code(S,_))	?- !, throw_pe(S, input, binary, peek_code/2).
iso_stream_mode_handler(_, put_byte(S,_))	?- !, throw_pe(S, output, text, put_byte/2).
iso_stream_mode_handler(_, put_char(S,_))	?- !, throw_pe(S, output, binary, put_char/2).
iso_stream_mode_handler(_, put_code(S,_))	?- !, throw_pe(S, output, binary, put_code/2).
iso_stream_mode_handler(_, read_term(S,_,_))	?- !, throw_pe(S, input, binary, read_term/3).
iso_stream_mode_handler(_, write_term(S,_,_))	?- !, throw_pe(S, output, binary, write_term/3).
iso_stream_mode_handler(_, set_stream_position(S,_P)) ?- !,
	throw(error(permission_error(reposition,stream,S),set_stream_position/2)).
iso_stream_mode_handler(E, Culprit) :-
	error(default(E), Culprit).

    throw_pe(SorA, IO, BadType, Culprit) :-
    	(
	    get_stream(SorA, S),
	    stream_property(S, IO),
	    get_stream_info(S, encoding, Enc),
	    encoding_type(Enc, BadType)
	->
	    atom_concat(BadType, '_stream', TypeStream),
	    throw(error(permission_error(IO,TypeStream,S),Culprit))
	;
	    throw(error(permission_error(IO,stream,SorA),Culprit))
	).


%----------------------------------------------------------------------

% "reading past the file end" -> permission error
:- set_event_handler(198, iso_past_eof_handler/2).
iso_past_eof_handler(_, Culprit) :-
	% assume Culprit is an input-builtin, either with 1st argument
	% being the stream, or with input from current 'input'
	(
	    arg(1, Culprit, S),
	    ( is_handle(S) -> SH=S
	    ; atom(S), stream_property(SH, alias(S))
	    )
	->
	    true
	;
	    current_input(SH)
	),
	stream_property(SH, eof_action(error)),
	!,
	close(S, [force(true)]),
	functor(Culprit, Name, Arity),
	throw(error(permission_error(input,past_end_of_stream,S),Name/Arity)).
iso_past_eof_handler(E, Culprit) :-
	error(default(E), Culprit).


%----------------------------------------------------------------------

:- set_event_handler(43, iso_illegal_op_handler/3).
iso_illegal_op_handler(_, op(_P,_A,Op), _Module) ?- !,
	( Op==(',') -> Action=modify ; Action=create ),
	throw(error(permission_error(Action,operator,Op),op/3)).
iso_illegal_op_handler(E, Culprit, Module) :-
	error(default(E), Culprit, Module).


%----------------------------------------------------------------------
% evaluation_error - only 'undefined' exists in ECLiPSe
%----------------------------------------------------------------------

:- set_event_handler(20, iso_evaluation_error_handler/2).
iso_evaluation_error_handler(_, Culprit) :-
	% pass the full Culprit here for more detail
	exceptional_value(Culprit, Exception),
	throw(error(evaluation_error(Exception), Culprit)).

    :- mode exceptional_value(+,-).
    exceptional_value(//(0,0,_), undefined) :- !.
    exceptional_value(//(_,_,_), zero_divisor) :- !.
    exceptional_value(div(_,_,_), zero_divisor) :- !.
    exceptional_value(mod(_,_,_), zero_divisor) :- !.
    exceptional_value(_, undefined).


%----------------------------------------------------------------------
% Type table
% Should have entries for everything where we want to throw a type_error
% ECLiPSe may signal this with error 5 or 6
%----------------------------------------------------------------------

type(functor(nonvar, atomic, integer)).
type(arg(integer, compound, term)).
type(nonvar =.. list).
type(call(callable)).
type(copy_term(term,term)).
type(term is evaluable).
type(clause(callable,callable)).
type(current_predicate(predicate_indicator)).
type(asserta(clause)).
type(assertz(clause)).
type(retract(clause)).
type(retractall(callable)).
type(abolish(predicate_indicator)).
type(findall(term,callable,list)).
type(bagof(term,callable,list)).
type(setof(term,callable,list)).
type(open(nonvar,atom,variable,list)).
type(open(nonvar,atom,variable)).
type(close(nonvar)).
type(close(nonvar,list)).
type(get_byte(in_byte)).
type(get_byte(nonvar,in_byte)).
type(get_char(in_character)).
type(get_char(nonvar,in_character)).
type(get_code(in_character_code)).
type(get_code(nonvar,in_character_code)).
type(peek_byte(in_byte)).
type(peek_byte(nonvar,in_byte)).
type(peek_char(in_character)).
type(peek_char(nonvar,in_character)).
type(peek_code(in_character_code)).
type(peek_code(nonvar,in_character_code)).
type(put_byte(byte)).
type(put_byte(nonvar,byte)).
type(put_char(character)).
type(put_char(nonvar,character)).
type(put_code(character_code)).
type(put_code(nonvar,character_code)).
type(nl(nonvar)).
type(read_term(term,list)).
type(read_term(nonvar,term,list)).
type(read(term)).
type(read(nonvar,term)).
type(write_term(term,list)).
type(write_term(nonvar,term,list)).
type(write(term)).
type(write(nonvar,term)).
type(writeq(term)).
type(writeq(nonvar,term)).
type(write_canonical(nonvar,term)).
type(write_canonical(term)).
type(op(integer,atom,atom_or_atom_list)).
%type(current_op(term,term,atom)).	% literally required by 8.14.4.3
type(current_op(term,atom,atom)).	% according to Szeredi test (and sensible)
type(char_code(character,character_code)).
type(char_conversion(character,character)).
type(current_char_conversion(character,character)).
type(\+(callable)).
type(once(callable)).
type(atom_length(atom,integer)).
type(atom_concat(atom,atom,atom)).
type(sub_atom(atom,integer,integer,integer,atom)).
type(atom_chars(atom,character_list)).
type(atom_codes(atom,character_code_list)).
type(char_code(character,integer)).
type(number_chars(number,character_list)).
type(number_codes(number,character_code_list)).
type(set_prolog_flag(atom,nonvar)).
type(set_flag(atom,nonvar)).
type(current_prolog_flag(atom,term)).
type(halt(integer)).
type(/\(integer,integer,term)).
type(\/(integer,integer,term)).
type(xor(integer,integer,term)).
type(\(integer,term)).
type(>>(integer,integer,term)).
type(<<(integer,integer,term)).
type(//(integer,integer,term)).
type(mod(integer,integer,term)).
type(rem(integer,integer,term)).
type(div(integer,integer,term)).

% These should be done differently to avoid pure ECLiPSe predicates
% to acquire ISO behaviour
type(concat_atoms(atom,atom,atom)).			% for atom_concat/3
type(readvar(nonvar,term,term,atom)).			% for read_term/3
type(atom_string(atom,string)).				% for sub_atom/5
type(substring(string,integer,integer,integer,string)).	% for sub_atom/5
type(retract_all(callable)).				% for retractall/1
type(concat_atom(list,atom)).				% for atom_chars/2
type(sort(integer,atom,list,list)).
type(keysort(list,list)).


%----------------------------------------------------------------------
% Domain table
% Should have entries for everything where we want to throw a domain_error
% ECLiPSe may signal this with error 5 or 6
%----------------------------------------------------------------------

domain(abolish(predicate_indicator)).
domain(atom_length(any,not_less_than_zero)).
domain(functor(any,any,not_less_than_zero)).
domain(arg(not_less_than_zero,any,any)).
domain(current_op(operator_priority,operator_specifier,any)).
domain(op(operator_priority,operator_specifier,any)).
domain(current_prolog_flag(prolog_flag,any)).
domain(set_prolog_flag(prolog_flag,any)).
domain(current_input(stream)).
domain(current_output(stream)).
domain(set_input(stream_or_alias)).
domain(set_output(stream_or_alias)).
domain(get_byte(stream_or_alias,any)).
domain(get_char(stream_or_alias,any)).
domain(get_code(stream_or_alias,any)).
domain(peek_byte(stream_or_alias,any)).
domain(peek_char(stream_or_alias,any)).
domain(peek_code(stream_or_alias,any)).
domain(put_byte(stream_or_alias,any)).
domain(put_char(stream_or_alias,any)).
domain(put_code(stream_or_alias,any)).
domain(open(source_sink,io_mode,stream,stream_options)).
domain(open(source_sink,io_mode,stream)).
domain(close(stream_or_alias)).
domain(close(stream_or_alias,close_options)).
domain(flush_output(stream_or_alias)).
domain(set_stream_position(stream_or_alias,stream_position)).
domain(stream_property(stream_or_alias,stream_property)).
domain(at_end_of_stream(stream_or_alias)).
domain(read_term(any,read_options_list)).
domain(read_term(stream_or_alias,any,read_options_list)).
domain(write_term(any,write_options_list)).
domain(write_term(stream_or_alias,any,write_options_list)).
domain(write(any)).
domain(write(stream_or_alias,any)).
domain(writeq(any)).
domain(writeq(stream_or_alias,any)).
domain(write_canonical(stream_or_alias,any)).
domain(write_canonical(any)).

% cases where the error is raised in a different ECLiPSe builtin
domain(readvar(stream_or_alias,any,any,any)).
domain(substring(any,not_less_than_zero,not_less_than_zero,not_less_than_zero,any)).	% for sub_atom/5
domain(flush(stream_or_alias)).
domain(at_eof(stream_or_alias)).
domain(get_stream_info(stream_or_alias,any,any)).


%----------------------------------------------------------------------
% Cases where the ECLiPSe culprit doesn't match the expected one
% This is not ideal since it affects reporting of errors in
% ECLiPSe's own builtins
%----------------------------------------------------------------------

:- mode real_culprit(+,-).
real_culprit(write_(S,T,_), write(S,T)) :- !.
real_culprit(writeq_(S,T,_), writeq(S,T)) :- !.
real_culprit(write_canonical_(S,T,_), write_canonical(S,T)) :- !.
real_culprit(In, In).

