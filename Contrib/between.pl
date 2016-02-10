
:- module(between).			% SEPIA header
:- export
	between/3,
	gen_arg/3,
	gen_int/1,
	gen_nat/1,
	gen_nat/2.


%   File   : BETWEEN.PL
%   Author : R.A.O'Keefe
%   Updated: 4 October 1984, 11-8-87
%   Purpose: Generate integers.

between(L, U, N) :-
	nonvar(N),
	!,
	integer(L),
	integer(U),
	integer(N),
	L =< N,
	N =< U.

between(L, U, N) :-
	integer(L),
	integer(U),
	L =< U,
	between1(L, U, N).

between1(L, _, L).

between1(L, U, N) :-
	L < U,
	M is L+1,
	between1(M, U, N).

%   gen_arg(N, Term, Arg)
%   is exactly like arg(N, Term, Arg), except that it will generate
%   solutions for N by backtracking (will work when N is a variable).

gen_arg(N, Term, Arg) :-
	functor(Term, _, Arity),
	between(1, Arity, N),
	arg(N, Term, Arg).

% Original by R O'Keefe
%
% gen_nat(+X)	 True if X is a natural number, false otherwise
% gen_nat(-X)	 Instantiates X to 0, then 1, 2, 3, 4...
%
% gen_nat(+L,-N) Instantiates N to L, then L+1, L+2...
% gen_nat(+L,+N) True if N >= L
% gen_nat(-L,+N) ** Succeeds with L = N , then LOOPS **
%
% gen_int(-X)	 Instantiates X to 0, then 1, -1, 2, -2, 3, -3...

gen_nat(N) :-			% gen-erate nat-ural
	nonvar(N),		% if we aren't to generate it
	!,			% demand that it
	integer(N),		% be an integer
	N >= 0.			% and non-negative.

gen_nat(N) :-			% otherwise, generate an
	gen_nat(0, N).		% integer >= 0
 
 
gen_nat(L, L).

gen_nat(L,N) :-			% Bug fix  Ken Johnson 6-5-87
	nonvar(L),		% Where both args are instantiated
	nonvar(N),		% just test N >= L.
	integer(L),		% Avoids an infinite loop
	integer(N),		% where N < L
	!,
	N >= L.

gen_nat(L, N) :-		% generate natural > L
	M is L+1,
	gen_nat(M, N).		% generate natural >= M
 
 

gen_int(I) :-			% gen-erate int-eger
	nonvar(I),		% if we aren't to generate it
	!,			% demand that it is an integer.
	integer(I).
gen_int(0).			% generate 0
gen_int(I) :-			% generate +/- N for N > 0
	gen_nat(1, N),
	(   I = N
	;   I is -N
	).



