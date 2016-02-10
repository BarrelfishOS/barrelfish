:- module(tidy).			% SEPIA header

:- lib(struct).
:- import copy_ground/3, subst/3 from struct.

eval(X) :- call(X).
add(A,B,C) :- +(A,B,C).
multiply(A,B,C) :- *(A,B,C).
power(A,B,C) :- ^(A,B,C).

:- dynamic simplify_axiom/2.

simple(X) :- var(X), !.
simple(X) :- atomic(X).


/*  File   : TIDY.PL
    Author : R.A.O'Keefe
    Updated: 2 June 1984
    Purpose: Limited algebraic expression simplifier.

    This is a new implementation of tidy, written in an attempt to remedy
    some of the deficiencies of the old one.  Unfortunately, it has a few
    of its own.  The only completely satisfactory approach seems to be to
    keep all expressions in bag form all the time.

    Tidy has now been split into two parts: tidy_stmt and tidy_expr.
	<stmt> ::= <stmt> # <stmt>	%   disjunction
		|  <stmt> & <stmt>	%   conjunction
		|  <expr> R <expr>	%   equation/inequality
	where R is one of = < > \= >= =<
    An <expr> is an ordinary algebraic expression.  Statements are scanned
    top-down, and no great effort is expended on them beyond a limited bit
    of evaluation.  Expressions are scanned bottom-up, and are worked hard.

    Tidy_stmt works from top down.  It doesn't bother putting statements in
    bag form, although since & (and) and # (or) are both commutative and
    associative it could well do so.  It does however do some flattening of
    statements: (E1 & E2) & E3 -> E1 & (E2 & E3).  This can do no harm.  As
    an experiment, tidy_stmt tries to put constants on the right-hand-sides
    of equations.  E.g. "x+y-3 = 0" -> "x+y = 3".  Just how useful this may
    be remains to be seen.  The code for combine_and and combine_or comes
    almost directly from the original tidy.

    The intermediate form makes use of a different representation of bags.
    A plus (times) bag is stored as +(Tree, Hole, Num) {*(Tree, Hole, Num)}.
    For example, a+b+c+3 would be stored as
	+(	+    ,  X,  3)
	       / \
	      +   c
	     / \
	    +   b
	   / \
	  X   a

    <expr> ::= <expr> + <expr> | <expr> - <expr> | - <expr>
	    |  <expr> * <expr> | <expr> / <expr>
	    |  <expr> ^ <expr> | sqrt(<expr>)
	    |  <special function>(<expr>,...)
	    |  <atom>		-- algebraic variable
	    |  <variable>	-- treated like an atom
	    |  <number>		-- including rational numbers

    <tidy expr> ::= {like <expr>, but only the first column.  Also,
	    numeric fragments are combined where possible, and sums
	    and products are flattened.}

    <baggy expr> ::= +(Tree, Hole, Num)
		  |  *(Tree, Hole, Num)
		  |  <tidy expr> ^ <baggy expr>
		  |  <tidy expr>

    BUG: if the exponent of a term eventually simplifies to 1, the base
    emerges as a <tidy expr>, rather than a <baggy expr>.  Hence some
    simplifications will be missed.  E.g. "(1+x)^(-1)^(-1) + -1" will end
    up as "(1+x) + -1" rather than as "x".  There appears to be no easy 
    way around this problem, though keeping the base as a <baggy expr> 
    may yet prove to be feasible.  In any case, the new tidy only has this
    problem with exponents, which are generally fairly simple.

    Tidy requires simple/1 and copy_ground/3 from STRUCT.PL.

    [2 June 1984] Bug fix: the user has always been able to add its
    own rewrite rules in the form simplify_axiom(Lhs, Rhs), e.g.
    simplify_axiom(X^log(X,Y), Y).  The result of this rewrite was
    assumed to be tidy, which was not always true.  The result is
    now retidied.  This could lead to looping, where the user's rules
    undo something that tidy does.  Look at your intended use of this
    hook, and decided whether to do retidying or not.  A related bug
    was that simplify_axiom was not called for powers.
*/

 :- export
 	tidy/2,				%  general interface
 	tidy_withvars/2,		%  same as tidy_expr
 	tidy_expr/2,			%  tidy an expression
 	tidy_stmt/2.			%  tidy a statement.
 
 
 :- mode
 	bag_to_tidy(+,-),		%  F(T,H,N) -> T'
 	bag_to_tidy(+,+,+),
 	combine_and(+,+,-),		%  X,Y -> X&Y
 	combine_bags(+,-),		%  apply op to baggy arguments
 	combine_or(+,+,-),		%  X,Y -> X#Y
 	combine_power(+,+,-),		%  X,Y -> X^Y
 	combine_plus(+,+,-),		%  X,Y -> X+Y
 	combine_rel(+,+,-,-),		%  X(R)Y -> X'(R)Y'
 	combine_times(+,+,-),		%  X,Y -> X*Y
 	expr_to_bag(+,-),		%  <expr> -> <baggy expr>
 	expr_to_bag(+,+,+,+,-),		%  map down args of <expr>
 	multiply_exp(+,+,-),		%  X,N -> N*X
 	multiply_out(+,+,-),		%  N,X -> N*X
 	multiply_out(+,-,+,-),		%  +(T,H),N -> +(T*N,H)
 	number_check(+,+,-),		%  maintain number-p accum
 	power_out(+,+,-),		
 	power_out(+,-,+,-),		%  *(T,H),N -> *(T^N,H)
 	relop_tidy(-,+,+,+),		%  R,X,Y -> X(R)Y or true/false
 	tidy_expr(+,-),			%  tidy expression
 	tidy_stmt(+,-),			%  tidy statement
 	user_tidy(+,-).			%  invoke user's simplify_axioms.

% Operator declarations from util.ops

:- op(950,xfy,#).			% Used for disjunction
:- op(920,xfy,&).
:- op(700,xfx,\=).


tidy(Old, New) :-
	tidy_stmt(Old, Mid), !, New = Mid.	%  which now tries tidy_expr
tidy(Old, Old) :-
	write('** failed: '), write(tidy(Old, '_')), nl.


tidy_withvars(Old, New) :-
	copy_ground(Old, Ground, Subst),
	tidy(Ground, Tidier),
	subst(Subst, Tidier, Mid), !,
	New = Mid.


tidy_stmt(Var, _) :-			%  don't do anything with variables
	var(Var), !, fail.
tidy_stmt(OldOne # OldTwo, New) :- !,
	tidy_stmt(OldOne, MidOne),
	tidy_stmt(OldTwo, MidTwo), !,
	combine_or(MidOne, MidTwo, New).
tidy_stmt(OldOne & OldTwo, New) :- !,
	tidy_stmt(OldOne, MidOne),
	tidy_stmt(OldTwo, MidTwo), !,
	combine_and(MidOne, MidTwo, New).
tidy_stmt(Equation, New) :-
	tidy_relop(Equation, Relation, OldLhs, OldRhs),
	!,
	expr_to_bag(OldLhs, MidLhs),
	expr_to_bag(OldRhs, MidRhs),
	combine_rel(MidLhs, MidRhs, NewLhs, NewRhs), !,
	relop_tidy(New, Relation, NewLhs, NewRhs).
tidy_stmt(Old, New) :-
	tidy_expr(Old, New).


combine_or(true, _, true) :- !.		%  zero element
combine_or(false, Y, Y) :- !.		%  unit element
combine_or(_, true, true) :- !.		%  zero element
combine_or(X, false, X) :- !.		%  unit element
combine_or(X, X, X) :- !.		%  merging identical elements
combine_or(W#X, Y, W#(X#Y)) :- !.	%  change association
combine_or(X, Y, X # Y).		%  general case

combine_and(false, _, false) :- !.	%  zero element
combine_and(true, Y, Y) :- !.		%  unit element
combine_and(_, false, false) :- !.	%  zero element
combine_and(X, true, X) :- !.		%  unit element
combine_and(X, X, X) :- !.		%  merging identical elements
combine_and(W&X, Y, W&(X&Y)) :- !.	%  change association
combine_and(X, Y, X & Y).		%  general case


tidy_relop(X = Y,   =, X, Y).
tidy_relop(X < Y,   <, X, Y).
tidy_relop(X > Y,   >, X, Y).
tidy_relop(X =< Y, =<, X, Y).
tidy_relop(X >= Y, >=, X, Y).
tidy_relop(X \= Y, \=, X, Y).


relop_tidy(Value, Relation, Lhs, Rhs) :-
	number(Lhs), number(Rhs),
	tidy_relop(Goal, Relation, Lhs, Rhs), !,
	eval(Goal, Value).
relop_tidy(Goal, Relation, Lhs, Rhs) :-
	tidy_relop(Goal, Relation, Lhs, Rhs).


combine_rel(+(T1, H1, N1), +(T2, H2, N2), Lhs, Rhs) :- !,
	bag_to_tidy(+(T1, H1, 0), Lhs),
	eval(N2-N1, N3),
	bag_to_tidy(+(T2, H2, N3), Rhs).
combine_rel(+(T1, H1, N1), N2, Lhs, N3) :-
	number(N2), !,
	eval(N2-N1, N3),
	bag_to_tidy(+(T1, H1, 0), Lhs).
combine_rel(*(T1, H1, N1), *(T2, H2, N2), Lhs,  Rhs) :-
	eval(N1 > 0), !,
	bag_to_tidy(*(T1, H1, 1), Lhs),
	eval(N2/N1, N3),
	bag_to_tidy(*(T2, H2, N3), Rhs).
combine_rel(*(T1, H1, N1), N2, Lhs, N3) :-
	number(N2),
	eval(N1 > 0), !,
	eval(N2/N1, N3),
	bag_to_tidy(*(T1, H1, 1), Lhs).
combine_rel(E1, E2, Lhs, Rhs) :-
	bag_to_tidy(E1, Lhs),
	bag_to_tidy(E2, Rhs).



tidy_expr(Old, New) :-
	expr_to_bag(Old, Mid), !,
	bag_to_tidy(Mid, New).

expr_to_bag(Var, _) :-			%  do nothing with variables
	var(Var), !, fail.
expr_to_bag(Old, Old) :-
	simple(Old), !.
expr_to_bag(Old, New) :-
	functor(Old, F, N),
	functor(Mid, F, N),
	expr_to_bag(N, Old, Mid, yes, New).

	expr_to_bag(0, _, Mid, yes, New) :- !,
		eval(Mid, New).
	expr_to_bag(0, _, Mid, no,  New) :-
		combine_bags(Mid, New).
	expr_to_bag(N, Old, Mid, EvalP, New) :-
		arg(N, Old, OldN),
		expr_to_bag(OldN, MidN),
		arg(N, Mid, MidN),
		number_check(MidN, EvalP, EvalQ),
		M is N-1, !,
		expr_to_bag(M, Old, Mid, EvalQ, New).

		number_check(N, EvalP, EvalP) :-
			number(N), !.
		number_check(_, _, no).	%  not a number


combine_bags(X+Y, New) :- !,
	combine_plus(X, Y, New).
combine_bags(X-Y, New) :-
	multiply_out(-1, Y, Z), !,
	combine_plus(X, Z, New).
combine_bags(-Y, New) :- !,
	multiply_out(-1, Y, New).
combine_bags(X*Y, New) :- !,
	combine_times(X, Y, New).
combine_bags(X/Y, New) :- !,
	power_out(Y, -1, Z),
	combine_times(X, Z, New).
combine_bags(X^Y, New) :- !,
	combine_power(X, Y, Mid),
	user_tidy(Mid, New).
combine_bags(Old, New) :-
	functor(Old, F, N),
	functor(Mid, F, N),
	bag_to_tidy(N, Old, Mid),
	user_tidy(Mid, New).


user_tidy(Expr, Bag) :-			%  apply user's rules
	simplify_axiom(Expr, Rewritten),
	!,				%  omit expr_to_bag call if the
	expr_to_bag(Rewritten, Bag).	%  Rewritten form is always tidy.
user_tidy(Expr, Expr).


bag_to_tidy(0, _, _) :- !.
bag_to_tidy(N, Old, Mid) :-
	arg(N, Old, OldN),
	bag_to_tidy(OldN, MidN),
	arg(N, Mid, MidN),
	M is N-1, !,
	bag_to_tidy(M, Old, Mid).

bag_to_tidy(+(T+R, R, 0), T) :- !.
bag_to_tidy(+( T , N, N), T) :- !.
bag_to_tidy(*( _,  _, 0), 0) :- !.
bag_to_tidy(*(T*R, R, 1), T) :- !.
bag_to_tidy(*( T , N, N), T) :- !.
bag_to_tidy(_^0,	  1) :- !.		%  B^0 = 1
bag_to_tidy(0^_,	  0) :- !.		%  0^X = 0
bag_to_tidy(1^_,	  1) :- !.		%  1^X = 1
bag_to_tidy(B^1,	  B) :- !.		%  B^1 = B (B already <tidy>)
bag_to_tidy(M^ *(T*R,R,N), B^T) :-		%  M^(N*X) = (M^N)^X
	number(M),
	power(M, N, B), !.
bag_to_tidy(B^X,	   B^T) :- !,		%  B^X, where X is <baggy>
	bag_to_tidy(X, T).

	bag_to_tidy(*(_, _, _), _).

bag_to_tidy(Old, Old).


combine_plus(+(T1, H1, N1), +(T2, T1, N2), +(T2, H1, N3)) :- !,
	add(N1, N2, N3).
combine_plus(+(T1, H1, N1), N2, +(T1, H1, N3)) :-
	number(N2), !,
	add(N1, N2, N3).
combine_plus(+(T1, H1, N1), E2, +(T1+E4, H1, N1)) :- !,
	bag_to_tidy(E2, E4).
combine_plus(0, E2, E2) :- !.
combine_plus(N1, +(T2, H2, N2), +(T2, H2, N3)) :-
	number(N1), !,
	add(N1, N2, N3).
combine_plus(E1, +(T2, H2, N2), +(T2+E3, H2, N2)) :- !,
	bag_to_tidy(E1, E3).
combine_plus(E1, 0, E1) :- !.
combine_plus(E1, N2, +(H+E3, H, N2)) :-
	number(N2), !,
	bag_to_tidy(E1, E3).
combine_plus(N1, E2, +(H+E4, H, N1)) :-
	number(N1), !,
	bag_to_tidy(E2, E4).
combine_plus(E1, E2, +((H+E3)+E4, H, 0)) :-
	bag_to_tidy(E1, E3),
	bag_to_tidy(E2, E4).


combine_times(*(T1, H1, N1), *(T2, T1, N2), *(T2, H1, N3)) :- !,
	multiply(N1, N2, N3).
combine_times(N1, E2, Ans) :-
	number(N1), !,
	multiply_out(N1, E2, Ans).
combine_times(E1, N2, Ans) :-
	number(N2), !,
	multiply_out(N2, E1, Ans).
combine_times(*(T1, H1, N1), E2, *(T1*E4, H1, N1)) :- !,
	bag_to_tidy(E2, E4).
combine_times(E1, *(T2, H2, N2), *(T2*E3, H2, N2)) :- !,
	bag_to_tidy(E1, E3).
combine_times(E1, E2, *((H*E3)*E4, H, 1)) :-
	bag_to_tidy(E1, E3),
	bag_to_tidy(E2, E4).


multiply_out(0, _, 0) :- !.
multiply_out(1, Old, Old) :- !.
/*  The next clause has been replaced by the two following clauses for the
    sake of Press and attraction.  This clause is correct, but alas, when
    attraction moves a number out (N*X+N*X)->N*(X+X) tidy moves it back in.

multiply_out(N, +(OldTree, Hole, OldNum), +(NewTree, Hole, NewNum)) :-
	multiply(N, OldNum, NewNum), !,
	multiply_out(OldTree, Hole, N, NewTree).
*/
multiply_out(-1, +(OldTree, Hole, OldNum), +(NewTree, Hole, NewNum)) :-
	multiply(-1, OldNum, NewNum), !,
	multiply_out(OldTree, Hole, -1, NewTree).
multiply_out(N, +(OldTree, Hole, OldNum), +(NewHole+N*Exp, NewHole, NewNum)) :-
	multiply(N, OldNum, NewNum), !,
	bag_to_tidy(+(OldTree, Hole, 0), Exp).
multiply_out(N, *(OldTree, Hole, OldNum), *(OldTree, Hole, NewNum)) :- !,
	multiply(N, OldNum, NewNum).
multiply_out(N, M, P) :-
	number(M), !,
	multiply(N, M, P).
multiply_out(N, Old, *(Hole*Exp, Hole, N)) :- !,
	bag_to_tidy(Old, Exp).

multiply_out(Bottom, Hole, _, Bottom) :-
	Bottom == Hole.
multiply_out(OldX + OldY, Hole, N, NewX + NewY) :-
	multiply_exp(OldY, N, NewY), !,
	multiply_out(OldX, Hole, N, NewX).

multiply_exp(OldX * OldY, N, NewX * OldY) :- !,
	multiply_exp(OldX, N, NewX).
multiply_exp(OldX + OldY, N, NewX + NewY) :-
	multiply_exp(OldY, N, NewY), !,
	multiply_exp(OldX, N, NewX).
multiply_exp(OldNum, N, NewNum) :-
	number(OldNum), !,
	multiply(N, OldNum, NewNum).
multiply_exp(Old, N, N*Old).


combine_power(B^E1, E2, B^E3) :- !,
	combine_times(E1, E2, E3).
combine_power(B, N2, Ans) :-
	number(N2), !,
	power_out(B, N2, Ans).
combine_power(E1, E2, E3^E4) :-
	bag_to_tidy(E1, E3), !,
	bag_to_tidy(E2, E4).


power_out(_, 0, 1) :- !.
power_out(B, 1, B) :- !.
power_out(B^E1, P, B^E2) :- !,
	multiply_out(P, E1, E2).
power_out(*(H1*T1, H1, 1), P, Ans) :-
	var(H1), !,
	power_out(T1, P, Ans).
power_out(*(T1, H1, N1), P, *(T2, H1, N2)) :-
	power(N1, P, N2), !,
	power_out(T1, H1, P, T2).
power_out(*(T1, H2*N1, N1), P, *(T2, H2, 1)) :- !,
	power_out(T1, H2, P, T2).
power_out(+(H0+T1, H1, 0), P, Ans) :-
	H0 == H1 /*DRAT*/, !,
	power_out(T1, P, Ans).
power_out(N, P, M) :-
	number(N),
	power(N, P, M), !.
power_out(B, P, E^P) :-
	bag_to_tidy(B, E).


power_out(Bottom, Hole, _, Bottom) :-
	Bottom == Hole, !.
power_out(OldX * (OldB^OldP), Hole, Num, NewX * NewB) :-
	multiply_exp(OldP, Num, NewP),
	(   NewP = 1, NewB = OldB
	;   NewB = OldB^NewP
	), !,
	power_out(OldX, Hole, Num, NewX).
power_out(OldX * OldY, Hole, Num, NewX * (OldY^Num)) :- !,
	power_out(OldX, Hole, Num, NewX).

