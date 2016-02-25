%   File   : STRUCT.PL
%   Author : Richard A. O'Keefe.
%   Updated: 15 September 1984
%   Purpose: General term hacking.  See also OCCUR.PL, METUTL.PL.

:- module(struct).			% SEPIA header

:- export
	subst/3,
	occ/3,
	variables/2,
	copy_ground/3.

:- op(950,xfy,#).                     % Used for disjunction
:- op(920,xfy,&).                     % Used for conjunction

/*
    These routines view a term as a data-structure.  In particular,
they handle Prolog variables in the terms as objects.  This is not
entirely satisfactory.  A proper separations of levels is needed.
*/


%   subst(Substitution, Term, Result) applies a substitution, where
%   <substitution> ::= <OldTerm> = <NewTerm>
%		    |  <Substitution> & <Substitution>
%		    |  <Substitution> # <Substitution>
%   The last two possibilities only make sense when the input Term is
%   an equation, and the substitution is a set of solutions.  The
%   "conjunction" of substitutions really refers to back-substitution,
%   and the order in which the substitutions are done may be crucial.
%   If the substitution is ill-formed, and only then, subst will fail.


subst(Subst1 & Subst2, Old, New) :-
	subst(Subst1, Old, Mid), !,
	subst(Subst2, Mid, New).
subst(Subst1 # Subst2, Old, New1 # New2) :-
	subst(Subst1, Old, New1), !,
	subst(Subst2, Old, New2).
subst(Lhs = Rhs, Old, New) :- !,
	subst(Lhs, Rhs, Old, New).
subst(true, Old, Old).


	subst(Lhs, Rhs, Old, Rhs) :-		%   apply substitution
		Old == Lhs, !.
	subst(_, _, Old, Old) :-		%   copy unchanged
		var(Old), !.
	subst(Lhs, Rhs, Old, New) :-		%   apply to arguments
		functor(Old, Functor, Arity),
		functor(New, Functor, Arity),
		subst(Arity, Lhs, Rhs, Old, New).

	
		subst(0, _, _, _, _) :- !.
		subst(N, Lhs, Rhs, Old, New) :-
			arg(N, Old, OldArg),
			subst(Lhs, Rhs, OldArg, NewArg),
			arg(N, New, NewArg),
			M is N-1, !,
			subst(M, Lhs, Rhs, Old, New).
		

%   occ(Subterm, Term, Times) counts the number of times that the subterm
%   occurs in the term.  It requires the subterm to be ground.  We have to
%   introduce occ/4, because occ's last argument may already be instantiated.
%   It is useful to do so, because we can use accumulator arguments to make
%   occ/4 and occ/5 tail-recursive.  NB if you merely want to check whether
%   SubTerm occurs in Term or not, it is possible to do better than this.
%   See Util:Occur.Pl .


occ(SubTerm, Term, Occurrences) :-
	occ(SubTerm, Term, 0, Times), !,
	Occurrences = Times.

	occ(SubTerm, Term, SoFar, Total) :-
		Term == SubTerm, !,
		Total is SoFar+1.
	occ(_, Term, Total, Total) :-
		var(Term), !.
	occ(SubTerm, Term, SoFar, Total) :-
		functor(Term, _, Arity), !,
		occ(Arity, SubTerm, Term, SoFar, Total).

		occ(0, _, _, Total, Total) :- !.
		occ(N, SubTerm, Term, SoFar, Total) :-
			arg(N, Term, Arg),
			occ(SubTerm, Arg, SoFar, Accum),
			M is N-1, !,
			occ(M, SubTerm, Term, Accum, Total).


%   The previous two predicates operate on ground arguments, and have some
%   pretence of being logical (though at the next level up).  The next one
%   is thoroughly non-logical.  Given a Term,
%	variables(Term, VarList)
%   returns a list whose elements are the variables occuring in Term, each
%   appearing exactly once in the list.  var_member_check(L, V) checks
%   that the variable V is *not* a member of the list L.  The original
%   version of variables/2 had its second argument flagged as "?", but this
%   is actually no use, because the order of elements in the list is not
%   specified, and may change from implementation to implementation.
%   The only application of this routine I have seen is in Lawrence's code
%   for tidy_withvars.  The new version of tidy uses copy_ground (next page).
%   If that is the only use, this routine could be dropped.


variables(Term, VarList) :-
	variables(Term, [], VarList).

	variables(Term, VarList, [Term|VarList]) :-
		var(Term),
		var_member_check(VarList, Term), !.
	variables(Term, VarList, VarList) :-
		var(Term), !.
	variables(Term, SoFar, VarList) :-
		functor(Term, _, Arity), !,
		variables(Arity, Term, SoFar, VarList).

		variables(0, _, VarList, VarList) :- !.
		variables(N, Term, SoFar, VarList) :-
			arg(N, Term, Arg),
			variables(Arg, SoFar, Accum),
			M is N-1, !,
			variables(M, Term, Accum, VarList).

		var_member_check([], _).
		var_member_check([Head|Tail], Var) :-
			Var \== Head, !,
			var_member_check(Tail, Var).

/*  In order to handle statements and expressions which contain variables,
    we have to create a copy of the given data-structure with variables 
    replaced by ground terms of some sort, do an ordinary tidy, then put
    the variables back.  Since we can use subst/3 to do this last step, a
    natural choice of working structure in the first step is a substitution
	$VAR(k) = Vk & ... & $VAR(0) = V0 & 9 = 9.
    The rest is straight-forward.  The cost of building the copy is o(E*V)
    where E is the size of the original expression and V is the number of
    variables it contains.  The final substitution is the same order of cost.
    For what it's worth, copy_ground(X,Y,_) & numbervars(X,0,_) => X == Y.
*/


copy_ground(Term, Copy, Substitution) :-
	copy_ground(Term, Copy, 9=9, Substitution).

	copy_ground(Term, Copy, SubstIn, SubstOut) :-
		var(Term), !,
		subst_member(SubstIn, Term, Copy, SubstOut).
	copy_ground(Term, Copy, SubstIn, SubstOut) :-
		functor(Term, Functor, Arity),
		functor(Copy, Functor, Arity), !,
		copy_ground(Arity, Term, Copy, SubstIn, SubstOut).
	
		copy_ground(0, _, _, SubstIn, SubstIn) :- !.
		copy_ground(N, Term, Copy, SubstIn, SubstOut) :-
			arg(N, Term, TermN),
			copy_ground(TermN, CopyN, SubstIn, SubstMid),
			arg(N, Copy, CopyN),
			M is N-1, !,
			copy_ground(M, Term, Copy, SubstMid, SubstOut).
	
		subst_member(SubstIn, Term, Copy, SubstIn) :-
			subst_member(SubstIn, Term, Copy), !.
		subst_member(SubstIn, Term, Copy, (Copy = Term) & SubstIn) :-
			(   SubstIn = (('$VAR'(M) = _) & _),
				N is M+1		%  M+1 variables seen
			;   N = 0			%  SubstIn = 9=9
			), !,
			Copy = '$VAR'(N).
		
			subst_member((Copy = Vrbl) & _, Term, Copy) :-
				Vrbl == Term, !.
			subst_member(_ & Rest, Term, Copy) :-
				subst_member(Rest, Term, Copy).
		

