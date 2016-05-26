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
% Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): 
% 
% END LICENSE BLOCK

% FINITE and INFINITE DOMAINS		
% created 910527 ECRC thom fruehwirth
% inspired by CHIP
% 910913 modified 
% 920409 element/3 added
% 920616 more CHIP predicates added
% 930726 started porting to CHR release
% 931014 mult/3 added for CHIC user meeting (Andre Chamard)
% 931201 ported to CHR release
% 931208 removed special case of integer domain
% 940304 element/3 constraint loop due to finite domains with repeated
%        elements fixed by replacing findall by setof and adding length where necessary
% 980701 converted to new chr format, Kish Shen
% 990316 fixed problem with imposing constraint with same domain
% 990914 removed singleton warnings
		
% X::Dom - X must be element of the finite or infinite domain Dom

% Domains can be either numbers (including arithemtic expressions)
% or arbitrary ground terms (!), the domain is set with setval(domain,Kind),
% where Kind is either number or term. Default for Kind is term.

:- lib(ech).

:- setval(domain,term). 	% set default

% Simplifies domains together with inequalities and some more CHIP predicates:
% 	element/3, atmost/3, alldistinct/1, circuit/1 and mult/3
% It also includes paired (!) domains (see element constraint)

:- handler domain.

%option(already_in_store, on). 
%option(already_in_heads, on).
:- option(check_guard_bindings, off).

% for domain constraints
:- op(700,xfx,'::').
:- op(600,xfx,'..').
:- op(600,xfx,':').

% for inequality constraints
:-op(700,xfx,lt).
:-op(700,xfx,le).
:-op(700,xfx,gt).
:-op(700,xfx,ge).
:-op(700,xfx,ne).



% INEQUALITIES ===============================================================
% inequalities over numbers (including arithmetic expressions) or terms

:- constraints lt/2,le/2,ne/2.

A gt B :- B lt A.
A ge B :- B le A.
% some basic simplifications
A lt A <=> fail.
A le A <=> true.
A ne A <=> fail.
A lt B,B lt A <=> fail.
A le B,B le A <=> A=B.
% for number domain, allow arithmetic expressions in the arguments
A  lt  B <=> domain(number),ground(A),not number(A) | A1 is A, A1 lt B.
B  lt  A <=> domain(number),ground(A),not number(A) | A1 is A, B lt A1.
A  le  B <=> domain(number),ground(A),not number(A) | A1 is A, A1 le B.
B  le  A <=> domain(number),ground(A),not number(A) | A1 is A, B le A1.
A  ne  B <=> domain(number),ground(A),not number(A) | A1 is A, A1 ne B.
B  ne  A <=> domain(number),ground(A),not number(A) | A1 is A, B ne A1.
% use built-ins to solve the predicates if arguments are known
A  lt  B <=> ground(A),ground(B) | (domain(number) -> A < B ; A @< B).
A  le  B <=> ground(A),ground(B) | (domain(number) -> A =< B ; A @=< B).
A  ne  B <=> ground(A),ground(B) | (domain(number) -> A =\= B ; A \== B).
A ne B \ B ne A <=> true.



% FINITE and INFINITE DOMAINS ================================================

% Domains must be ground and enumeration domains sorted (ascending) !

:- constraints (::)/2, chr_labeling/0:at_lower(1).

% enforce groundness
 X::Dom <=> nonground(Dom) | 
	write('ERROR: Nonground Domain in '),writeln(X::Dom),abort.

% binary search by splitting domain in halves
 'label::2'(X,Min:Max) :- 
	Mid is (Min+Max)/2,
	(X::Min:Mid ; X ne Mid, X::Mid:Max).

%	ground(X):- not nonground(X).

	domain(Kind):- getval(domain,Kind).

% CHIP list shorthand for domain variables
% list must be known (end in the empty list)

 [X|L]::Dom <=> makedom([X|L],Dom).

	makedom([],_D) :- true.
	makedom([X|L],D) :-
		X::D,
		makedom(L,D).


% Consecutive integer domain ---------------------------------------------
% X::Min..Max - X is an integer between the numbers Min and Max (included)
% constraint is mapped to enumeration domain constraint
 X::Min..Max <=> 
	Min0 is Min, 
	(Min0=:=round(Min0) -> Min1 is fix(Min0) ; Min1 is fix(Min0+1)),
	Max1 is fix(Max),
	interval(Min1,Max1,L), 
	X::L.

 	interval(M,N,[M|Ns]):- 
		M<N, 
		!, 
		M1 is M+1, 
		interval(M1,N,Ns).
	interval(N,N,[N]).


% Enumeration domain -----------------------------------------------------
% X::Dom - X must be a ground term in the ascending sorted ground list Dom
% for number domain, allow arithmetic expressions in domain
% uses already_in_heads option
 X::[A|L] <=> domain(number), member(X,[A|L]), not number(X) |
		eval_list([A|L],L1),sort(L1,L2), X::L2.

	eval_list([],[]).
	eval_list([X|L1],[Y|L2]):-
		Y is X,
		eval_list(L1,L2).

% special cases
 X::[] <=> fail.				
 X::[Y] <=> X=Y.
 X::[A|L] <=> ground(X) | (member(X,[A|L]) -> true).



% special cases
 I-I::L <=> setof(X,member(X-X,L),L1), I::L1.
 I-V::L <=> ground(I) | setof(X,member(I-X,L),L1), V::L1.
 I-V::L <=> ground(V) | setof(X,member(X-V,L),L1), I::L1.

% Interval domain ---------------------------------------------------------
% X::Min:Max - X must be a ground term between Min and Max (included)
% for number domain, allow for arithmetic expressions ind omain
% for integer domains, X::Min..Max should be used
% uses already_in_heads option
 X::Min:Max <=> domain(number), not (number(Min),number(Max)) |
		Min1 is Min, Max1 is Max, X::Min1:Max1.
% special cases
 X::Min:Min <=> X=Min.
 X::Min:Max <=> (domain(number) -> Min>Max ; Min@>Max) | fail.
 X::Min:Max <=> ground(X) | 
		(domain(number) -> Min=<X,X=<Max ; Min@=<X,X@=<Max).

% intersection of domains for the same variable
 X::[A1|L1], X::[A2|L2] <=> intersection([A1|L1],[A2|L2],L) | X::L.
% interaction with inequalities
 X ne Y, X::[A|L] <=> ground(Y) , remove(Y,[A|L],L1), L1\==[A|L] |  X::L1.
 Y ne X, X::[A|L] <=> ground(Y) , remove(Y,[A|L],L1), L1\==[A|L] |  X::L1.
 Y le X, X::[A|L] <=> ground(Y) , remove_lower(Y,[A|L],L1), L1\==[A|L] | X::L1.
 X le Y, X::[A|L] <=> ground(Y) , remove_higher(Y,[A|L],L1),L1\==[A|L] | X::L1.
 Y lt X, X::[A|L] <=> ground(Y) , remove_lower(Y,[A|L],L1),remove(Y,L1,L2),
	L2\==[A|L] |  X::L2.
 X lt Y, X::[A|L] <=> ground(Y) , remove_higher(Y,[A|L],L1),remove(Y,L1,L2),
	L2\==[A|L] |  X::L2.

% interaction with interval domain
 X::Min:Max, X::[A|L] <=> remove_lower(Min,[A|L],L1),remove_higher(Max,L1,L2),
	L2\==[A|L] |  X::L2.


% propagation of bounds
 X le Y, Y::[A|L]   ==> var(X) | last([A|L],Max), X le Max.
 X le Y, X::[Min|_] ==> var(Y) | Min le Y.
 X lt Y, Y::[A|L]   ==> var(X) | last([A|L],Max), X lt Max.
 X lt Y, X::[Min|_] ==> var(Y) | Min lt Y.

	last(L,X):- append(_,[X],L),!.

% intersection of domains for the same variable
 X::Min1:Max1, X::Min2:Max2 <=> maximum(Min1,Min2,Min),minimum(Max1,Max2,Max) |
		X::Min:Max.

	minimum(A,B,C):- (domain(number) -> A<B ; A@<B) -> A=C ; B=C.
	maximum(A,B,C):- (domain(number) -> A<B ; A@<B) -> B=C ; A=C.

% interaction with inequalities
 X::Min:Max \ X ne Y <=> ground(Y),
	(domain(number) -> (Y<Min;Y>Max) ; (Y@<Min;Y@>Max)) | true.
 X::Min:Max \ Y ne X <=> ground(Y),
	(domain(number) -> (Y<Min;Y>Max) ; (Y@<Min;Y@>Max)) | true.
 Min2 le X, X::Min1:Max <=> ground(Min2) , maximum(Min1,Min2,Min) | X::Min:Max.
 X le Max2, X::Min:Max1 <=> ground(Max2) , minimum(Max1,Max2,Max) | X::Min:Max.
 Min2 lt X, X::Min1:Max <=> ground(Min2) , maximum(Min1,Min2,Min) |
		X::Min:Max, X ne Min.
 X lt Max2, X::Min:Max1 <=> ground(Max2) , minimum(Max1,Max2,Max) |
		X::Min:Max, X ne Max.
% propagation of bounds
 X le Y, Y::Min:Max ==> var(X) | X le Max.
 X le Y, X::Min:Max ==> var(Y) | Min le Y.
 X lt Y, Y::Min:Max ==> var(X) | X lt Max.
 X lt Y, X::Min:Max ==> var(Y) | Min lt Y.



% MULT/3 EXAMPLE EXTENSION ==================================================
% mult(X,Y,C) - integer X multiplied by integer Y gives the integer constant C.

:- constraints mult/3.

mult(X,Y,C) <=> ground(X) | (X=:=0 -> C=:=0 ; 0=:=C mod X, Y is C//X).
mult(Y,X,C) <=> ground(X) | (X=:=0 -> C=:=0 ; 0=:=C mod X, Y is C//X).
mult(X,Y,C), X::MinX:MaxX ==> 
	%(Dom=MinX:MaxX -> true ; Dom=[MinX|L],last(L,MaxX)),
	MinY is (C-1)//MaxX+1,
        MaxY is C//MinX,
	Y::MinY:MaxY.
mult(Y,X,C), X::MinX:MaxX ==>
	%(Dom=MinX:MaxX -> true ; Dom=[MinX|L],last(L,MaxX)),
	MinY is (C-1)//MaxX+1,
        MaxY is C//MinX,
	Y::MinY:MaxY.

/*
[eclipse 46]: mult(X,Y,156),[X,Y]::2:156,X le Y.

X = X_g307
Y = Y_g331
 
Constraints:
(1) mult(X_g307, Y_g331, 156)
(7) Y_g331 :: 2 : 78
(8) X_g307 :: 2 : 78
(10) X_g307 le Y_g331

yes.
[eclipse 47]: mult(X,Y,156),[X,Y]::2:156,X le Y,labeling.

X = 12
Y = 13     More? (;) 

X = 6
Y = 26     More? (;) 

X = 4
Y = 39     More? (;) 

X = 2
Y = 78     More? (;) 

X = 3
Y = 52     More? (;) 

no (more) solution.
*/




% CHIP ELEMENT/3 ============================================================
% translated to "pair domains", a very powerful extension of usual domains
% this version does not work with arithemtic expressions!
% uses already_in_heads option

element(I,VL,V):- length(VL,N),interval(1,N,IL),gen_pair(IL,VL,BL), I-V::BL.

	gen_pair([],[],[]).
	gen_pair([A|L1],[B|L2],[A-B|L3]):-
		gen_pair(L1,L2,L3).

% intersections
 X::[A|L1], X-Y::L2 <=> intersect(I::[A|L1],I-V::L2,I-V::L3),
			length(L2,N2),length(L3,N3),N2>N3 | X-Y::L3.
 Y::[A|L1], X-Y::L2 <=> intersect(V::[A|L1],I-V::L2,I-V::L3),
			length(L2,N2),length(L3,N3),N2>N3 | X-Y::L3.
 X-Y::L1, Y-X::L2 <=> intersect(I-V::L1,V-I::L2,I-V::L3) | X-Y::L3.
 X-Y::L1, X-Y::L2 <=> intersect(I-V::L1,I-V::L2,I-V::L3) | X-Y::L3.

     intersect(A::L1,B::L2,C::L3):- findall(C,(member(A,L1),member(B,L2)),L3).

% inequalties with two common variables
 Y lt X, X-Y::L <=> A=R-S,findall(A,(member(A,L),R@< S),L1) | X-Y::L1.
 X lt Y, X-Y::L <=> A=R-S,findall(A,(member(A,L),S@< R),L1) | X-Y::L1.
 Y le X, X-Y::L <=> A=R-S,findall(A,(member(A,L),R@=<S),L1) | X-Y::L1.
 X le Y, X-Y::L <=> A=R-S,findall(A,(member(A,L),S@=<R),L1) | X-Y::L1.
 Y ne X, X-Y::L <=> A=R-S,findall(A,(member(A,L),R\==S),L1) | X-Y::L1.
 X ne Y, X-Y::L <=> A=R-S,findall(A,(member(A,L),S\==R),L1) | X-Y::L1.
% propagation between paired domains
% X-Y::L1, Y-Z::L2 ==> intersect(A-B::L1,B-C::L2,A-C::L), X-Z::L.
% X-Y::L1, Z-Y::L2 ==> intersect(A-B::L1,C-B::L2,A-C::L), X-Z::L.
% X-Y::L1, X-Z::L2 ==> intersect(I-V::L1,I-W::L2,V-W::L), Y-Z::L.
% propagation to usual unary domains
 X-Y::L ==> A=R-S,setof(R,A^member(A,L),L1), X::L1,
	          setof(S,A^member(A,L),L2), Y::L2.



% ATMOST/3 ===================================================================

atmost(N,List,V):-length(List,K),atmost(N,List,V,K).

:- constraints atmost/4.

atmost(N,List,V,K) <=> K=<N | true.
atmost(0,List,V,K) <=> (ground(V);ground(List)) | outof(V,List).
atmost(N,List,V,K) <=> K>N,ground(V),delete_ground(X,List,L1) |
		(X==V -> N1 is N-1 ; N1=N),K1 is K-1, atmost(N1,L1,V,K1).

	delete_ground(X,List,L1):- delete(X,List,L1),ground(X),!.



% ALLDISTINCT/1 ===============================================================
% uses ne/2 constraint

:- constraints alldistinct/1.

alldistinct([]) <=> true.
alldistinct([X]) <=> true.
alldistinct([X,Y]) <=> X ne Y.
alldistinct([A|L]) <=> delete_ground(X,[A|L],L1) | outof(X,L1),alldistinct(L1).

alldistinct([]).
alldistinct([X|L]):-
	outof(X,L),
	alldistinct(L).

outof(_X,[]).
outof(X,[Y|L]):-
	X ne Y,
	outof(X,L).

:- constraints alldistinct1/2.
		
alldistinct1(R,[]) <=> true.
alldistinct1(R,[X]), X::[A|L] <=> ground(R) | 
			remove_list(R,[A|L],T), X::T.
alldistinct1(R,[X]) <=> (ground(R);ground(X)) | outof(X,R).	
alldistinct1(R,[A|L]) <=> ground(R),delete_ground(X,[A|L],L1) | 
			(member(X,R) -> fail ; alldistinct1([X|R],L1)).



% CIRCUIT/1 =================================================================

% constraints circuit1/1, circuit/1.
% uses list domains and ne/2


% lazy version

circuit1(L):-length(L,N),N>1,circuit1(N,L).

circuit1(2,[2,1]).
circuit1(N,L):- N>2,
		interval(1,N,D),
		T=..[f|L],
		domains1(1,D,L),
		alldistinct1([],L),
		no_subtours(N,1,T,[]).	

domains1(_N,_D,[]).
domains1(N,D,[X|L]):- 
		remove(N,D,DX),
		X::DX,
		N1 is N+1,
		domains1(N1,D,L).

no_subtours(0,_N,_L,_R):- !.
no_subtours(K,N,L,R):- 
	outof(N,R),
	(var(N) -> delay(N,no_subtours1(K,N,L,R)) ; no_subtours1(K,N,L,R)).
% no_subtours(K,N,T,R) \ no_subtours(K1,N,T,_) <=> K<K1 | true.

	no_subtours1(K,N,L,R):- 
		K>0,K1 is K-1,arg(N,L,A),no_subtours(K1,A,L,[N|R]).


% eager version

circuit(L):- length(L,N),N>1,circuit(N,L).

circuit(2,[2,1]).
%circuit(3,[2,3,1]).
%circuit(3,[3,1,2]).
circuit(N,L):- 	N>2,
		interval(1,N,D),
		T=..[f|L],
		N1 is N-1,
		domains(1,D,L,T,N1),
		alldistinct(L).		

domains(_N,_D,[],_T,_K).
domains(N,D,[X|L],T,K):- 
		remove(N,D,DX),
		X::DX,
		N1 is N+1,
		%no_subtours(K,N,T,[]),		% unfolded
		no_subtours1(K,X,T,[N]),		
		domains(N1,D,L,T,K).




% remove*/3 auxiliary predicates =============================================

remove(A,B,C):- 
	delete(A,B,C) -> true ; B=C.

remove_list(_,[],T):- !, T=[].
remove_list([],S,T):- S=T.
remove_list([X|R],[Y|S],T):- remove(X,[Y|S],S1),remove_list(R,S1,T).

remove_lower(_,[],L1):- !, L1=[].
remove_lower(Min,[X|L],L1):-
	X@<Min,
	!,
	remove_lower(Min,L,L1).
remove_lower(Min,[X|L],[X|L1]):-
	remove_lower(Min,L,L1).

remove_higher(_,[],L1):- !, L1=[].
remove_higher(Max,[X|L],L1):-
	X@>Max,
	!,
	remove_higher(Max,L,L1).
remove_higher(Max,[X|L],[X|L1]):-
	remove_higher(Max,L,L1).


chr_labeling, X::[Y|L] <=> 'label::'(X,[Y|L]), wake, chr_labeling.
'label::'(X,[Y|L]) :- member(X,[Y|L]).

chr_labeling, X::Min:Max <=> domain(number) |  'label::2'(X,Min:Max), chr_labeling.	

% end of handler domain




