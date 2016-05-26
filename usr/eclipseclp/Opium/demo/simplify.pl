% MISSING7

:- module(simplify).

getbug :-	
	writeln('\nCheck that you are in "module(simplify).", then'),
	writeln('to start the program type "simplify(1 + a + b + a, S).".\n').

test(S) :-
	simplify(1 + a + b + a, S).


bug :- 
	nl,
	explanation.

explanation :-	
writeln(' \n \
There is a clause missing which matches 2nd parameter = []. \n \
 \n \
GOAL:	 simplify(1 + a + b + a, S). \n \
CORRECT: S = 2 * a + b + 1 \n \
BUGGY:   no (more) solutions. \n \
').


% ============================================================================
% procedure for the simplification of summations.

simplify(Init,Sexpr) :- 
	simplify2(Init,Int,Vars),
	construct(Int,Vars,Sexpr).

simplify2(L+A,I,Var) :-
	!,
	simplify2(L,I1,V1),
	treat(A,I1,V1,I,Var).
simplify2(A,I,V) :- treat(A,0,[],I,V).

treat(A,I,V,I1,V) :- 					% A is an integer
	integer(A),!,
	I1 is I+A.
treat(A,I,V,I,V1) :-					% A is a symbol
	incr(A,V,V1).

					% fix: add incr(A,[],[A/1]) :-!.
incr(A,[A/N|L],[A/N1|L]) :-
	!,N1 is N+1.
incr(A,[S|L],[S|L1]) :-
	incr(A,L,L1).

construct(I,[],I) :-!.					
construct(0,L,P) :-!,construct1(L,P).			
construct(I,L,P+I) :- !,construct1(L,P).
construct1([S],P) :-!,pri(S,P).
construct1([S|L],L1+P) :- pri(S,P),construct1(L,L1).

pri(A/1,A) :-!.						
pri(A/N,N*A).
