
%   File   : METUTL.PL
%   Author : R.A.O'Keefe
%   Updated: 15 September 1984
%   Purpose: meta-logical operations as described in my note

:- module(metutl).			% SEPIA header
:- local unify/3.
:- export
%	compound/1,			% is a Sepia builtin
	copy/2,				% use copy_term/2 instead
	ground/1,			% better use \+ nonground
	occurs_check/2,			% better use \+ occurs
	occurs_in/2,			% use occurs/2 instead
	simple/1,
	subsumes/2,
	subsumes_chk/2,
	subterm/2,
	unify/2,
	variables_of/2,
	variant/2,			% less restricted than the builtin
	var_member_chk/2.

:- lib(numbervars).
 
/* the declarations are partially wrong for SEPIA
:- mode
	copy(+, ?),
	ground(+),
	    ground(+, +),
	occurs_check(+, ?),
	    occurs_check(+, +, ?),
	occurs_in(+, +),
	    occurs_in(+, +, +),
	subterm(+, ?),
	    subterm(+, +, ?),
	subsumes(+, +),
	    subsumes(+, +, +),
		subsumes(+, +, +, +),
	subsumes_chk(+, +),
	unify(+, +),
	    unify(+, +, +),
	var_member_chk(+, +),
	variables_of(+, -),
	    variables_of(+, +, -),
		variables_of(+, +, +, -),
	variant(+, +).
*/


% compound(Term) :-
% 	nonvar(Term),		%  not a variable
% 	functor(Term, _, Arity),
% 	Arity > 0.		%  not atomic


% simple(?T)
% True if T is an uninstantiated variable or an atom.

simple(Term) :-
	var(Term), !.			%  is a variable
simple(Term) :-				%  -or-
	atomic(Term).			%  is a number or an atom.

 
% ground(+Term)
% True if Term is ground, that is, it contains no
% uninstantiated variables.

ground(Term) :-
	nonvar(Term),
	functor(Term, _, N),
	ground(N, Term).
 
ground(0, _) :-
	!.

ground(N, Term) :-
	arg(N, Term, Arg),
	ground(Arg),
	M is N-1, !,
	ground(M, Term).
 

% occurs_in(-Var,+Term)
% True if the variable Var occurs somewhere in Term
% For instance
%	occurs_in(X,foo(X)) is true.

occurs_in(Var, Term) :- occurs(Var, Term).	% SEPIA

% occurs_in(Var, Term) :-
% 	var(Term),
% 	!,
% 	Var == Term.
% 
% occurs_in(Var, Term) :-
% 	functor(Term, _, N),
% 	occurs_in(N, Var, Term).
%  
% occurs_in(N, Var, Term) :-
% 	arg(N, Term, Arg),
% 	occurs_in(Var, Arg),
% 	!.
% 
% occurs_in(N, Var, Term) :-
% 	N > 1,
% 	M is N-1,
% 	occurs_in(M, Var, Term).
 

% subterm(+S,+T)
% true if S is a subterm of T (including if S = T)

subterm(Term, Term).

subterm(SubTerm, Term) :-
	nonvar(Term),
	functor(Term, _, N),
	subterm(N, SubTerm, Term).
 
subterm(N, SubTerm, Term) :-
	arg(N, Term, Arg),
	subterm(SubTerm, Arg).

subterm(N, SubTerm, Term) :-
	N > 1,
	M is N-1,
	subterm(M, SubTerm, Term).


% copy(+Old,-New)
% Makes a fresh copy of the term Old. A bit hacky...

copy(Old, New) :- copy_term(Old, New).	% SEPIA

% copy(Old, New) :-			% Altered to use data base
% 	record(copy,Old,Key),		% KJ  21-5-87
% 	recorded(copy,Mid,Key),
% 	erase(Key),
% 	!,
% 	New = Mid.


% unify(?X, ?Y)
% Try to unify X and Y, wih occurs check.
% Further down in this file is the Occurs Check.

unify(X, Y) :-
	var(X),
	var(Y),
	!,
	X = Y.				%  want unify(X,X)

unify(X, Y) :-
	var(X),
	!,
	occurs_check(Y, X),		%  X is not in Y
	X = Y.

unify(X, Y) :-
	var(Y),
	!,
	occurs_check(X, Y),		%  Y is not in X
	X = Y.

unify(X, Y) :-
	atomic(X),
	!,
	X = Y.

unify(X, Y) :-
	functor(X, F, N),
	functor(Y, F, N),
	unify(N, X, Y).
	
unify(0, _, _) :- !.

unify(N, X, Y) :-
	arg(N, X, Xn),
	arg(N, Y, Yn),
	unify(Xn, Yn),
	M is N-1,
	!,
	unify(M, X, Y).

occurs_check(Term, Var) :- \+ occurs(Var, Term).	% SEPIA

% occurs_check(Term, Var) :-
% 	var(Term),
% 	!,
% 	Term \== Var.
% 
% occurs_check(Term, Var) :-
% 	functor(Term, _, Arity),
% 	occurs_check(Arity, Term, Var).
% 
% occurs_check(0, _, _) :- !.
% 
% occurs_check(N, Term, Var) :-
% 	arg(N, Term, Arg),
% 	occurs_check(Arg, Var),
% 	M is N-1,
% 	!,
% 	occurs_check(M, Term, Var).

var_member_chk(Var, [Head|_]) :-
	Head == Var,
	!.
var_member_chk(Var, [_|Tail]) :-
	var_member_chk(Var, Tail).


variables_of(Term, Vars) :-
	variables_of(Term, [], Vars).

variables_of(Term, Sofar, Sofar) :-
	var(Term),
	var_member_chk(Term, Sofar),
	!.
variables_of(Term, Sofar, [Term|Sofar]) :-
	var(Term),
	!.
variables_of(Term, Sofar, Vars) :-
	functor(Term, _, N),
	variables_of(N, Term, Sofar, Vars).

variables_of(0, _, Vars, Vars) :- !.
variables_of(N, Term, Sofar, Vars) :-
	arg(N, Term, Arg),
	variables_of(Arg, Sofar, Mid),
	M is N-1, !,
	variables_of(M, Term, Mid, Vars).


% subsumes(+General, +Specific)
% True if Specific can be found by instantiating variables in General
% for instance, subsumes(foo(X), foo(bar)) is true.

subsumes(General, Specific) :-
	variables_of(Specific, Vars),
	subsumes(General, Specific, Vars).

subsumes(General, Specific, Vars) :-
	var(General),
	var_member_chk(General, Vars),
	!,
	General == Specific.

subsumes(General, Specific, _) :-
	var(General),
	!,
	General = Specific.			%  binds

subsumes(General, Specific, Vars) :-
	nonvar(Specific),			%  mustn't bind it
	functor(General,  FunctionSymbol, Arity),
	functor(Specific, FunctionSymbol, Arity),
	subsumes(Arity, General, Specific, Vars).

subsumes(0, _, _, _) :- !.

subsumes(N, General, Specific, Vars) :-
	arg(N, General,  GenArg),
	arg(N, Specific, SpeArg),
	subsumes(GenArg, SpeArg, Vars),
	M is N-1, !,
	subsumes(M, General, Specific, Vars).


variant(A, B) :-
	subsumes_chk(A, B),
	subsumes_chk(B, A).

subsumes_chk(General, Specific) :-
	\+  (	numbervars(Specific, 0, _),
		\+ General = Specific
	    ).

