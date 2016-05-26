%   File   : OCCUR.PL
%   Author : R.A.O'Keefe
%   Updated: 22 May 1983
%   Purpose: routines for checking number/place of occurrence 

%   Some of the things in METUTL.PL may also be relevant, particularly
%   subterm/2.  Maybe that should go here?  occ/3 in STRUCT.PL too.

:- module(occur).			% SEPIA header
:- export
	contains/2,			%   Term x Term ->
	freeof/2,			%   Term x Term ->
	patharg/3,			%   Path x Term -> Term
	position/3,			%   Term x Term -> Path
	replace/4.			%   Path x Term x Term -> Term

:- mode
	contains(+, +),
	copy_all_but_one_arg(+, +, +, +),
	freeof(+, +),
	    freeof(+, +, -),
	patharg(+, +, ?),
	position(?, +, ?),
	position(+, ?, +, ?),
	replace(+, +, +, -).



%   contains(Kernel, Expression)
%   is true when the given Kernel occurs somewhere in the Expression.
%   It be only be used as a test; to generate subterms use subterm/2.

contains(Kernel, Expression) :-
	\+ freeof(Kernel, Expression).


%   freeof(Kernel, Expression)
%   is true when the given Kernel does not occur anywhere in the
%   Expression.  NB: if the Expression contains an unbound variable,
%   this must fail, as the Kernel might occur there.  Since there are
%   infinitely many Kernels not contained in any Expression, and als
%   infinitely many Expressions not containing any Kernel, it doesn't
%   make sense to use this except as a test.

freeof(Kernel, Kernel) :- !,
	fail.
freeof(Kernel, Expression) :-
	functor(Expression, _, Arity),		%  can't be a variable!
	freeof(Arity, Kernel, Expression).

freeof(0, _Kernel, _Expression) :- !.
freeof(N, Kernel, Expression) :-
	arg(N, Expression, Argument),
	freeof(Kernel, Argument),
	M is N-1, !,
	freeof(M, Kernel, Expression).



%   patharg(Path, Exp, Term)
%   unifies Term with the subterm of Exp found by following Path.
%   It may be viewed as a generalisation of arg/3.  It cannot be
%   used to discover a path to a known Term; use position/3 for that.

patharg([Head|Tail], Exp, Term) :-
	arg(Head, Exp, Arg),
	patharg(Tail, Arg, Term).
patharg([], Term, Term).



%   position(Term, Exp, Path)
%   is true when Term occurs in Exp at the position defined by Path.
%   It may be at other places too, so the predicate is prepared to
%   generate them all.  The path is a generalised Dewey number, as usual.
%   position(x, 2*x^2+2*x+1=0, [1, 1, 2, 2]) {2*x} and
%   position(x, 2*x^2+2*x+1=0, [1, 1, 1, 2, 1]) {x^2} are both examples.

position(Term, Term, []).
position(Term, Exp, Path) :-
	nonvar(Exp),
	functor(Exp, _, N),
	position(N, Term, Exp, Path).

position(0, _Term, _Exp, _Path) :- !, fail.
position(N, Term, Exp, [N|Path]) :-
	arg(N, Exp, Arg),
	position(Term, Arg, Path).
position(N, Term, Exp, Path) :-
	M is N-1, !,
	position(M, Term, Exp, Path).



%   replace(Path, OldExpr, SubTerm, NewExpr)
%   is true when OldExpr and NewExpr are identical except at the position
%   identified by Path, where NewExpr has SubTerm.  There is a bug in the
%   Dec-10 compiler, which is why the second 'arg' call follows the replace
%   recursion.  If it weren't for that bug, replace would be tail recursive.
%   replace([1,1,2,2], 2*x^2+2*x+1=0, y, 2*x^2+2*y+1=0) is an example.
 
replace([M|Path], OldExpr, SubTerm, NewExpr) :- !,
	arg(M, OldExpr, OldArg),
	functor(OldExpr, F, N),
	functor(NewExpr, F, N),
	copy_all_but_one_arg(N, M, OldExpr, NewExpr),
	replace(Path, OldArg, SubTerm, NewArg),
	arg(M, NewExpr, NewArg).
replace([], _, SubTerm, SubTerm).


copy_all_but_one_arg(0, _, _, _) :- !.
copy_all_but_one_arg(M, M, OldExpr, NewExpr) :- !,
	L is M-1,
	copy_all_but_one_arg(L, M, OldExpr, NewExpr).
copy_all_but_one_arg(N, M, OldExpr, NewExpr) :-
	arg(N, OldExpr, Arg),
	arg(N, NewExpr, Arg),
	L is N-1,
	copy_all_but_one_arg(L, M, OldExpr, NewExpr).


/*  Suppose you have a set of rewrite rules Lhs -> Rhs which you
    want exhaustively applied to a term.  You would write

	waterfall(Expr, Final) :-
		Lhs -> Rhs,
		position(Expr, Lhs, Path),
		replace(Path, Expr, Rhs, Modified),
		!,
		waterfall(Modified, Final).
	waterfall(Expr, Expr).

*/
