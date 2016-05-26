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
% The Original Code is  The ECLiPSe Constraint Logic Programming System. 
% The Initial Developer of the Original Code is  Cisco Systems, Inc. 
% Portions created by the Initial Developer are
% Copyright (C) 1995-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Thierry Le Provost, ECRC.
% Contributor(s): Mark Wallace, IC-Parc and ICL.
% 
% END LICENSE BLOCK

:- use_module(propia).
%:- lib(propia).
:- lib(ria).
:- lib(fd).


tests(File) :-
	open(File,write,Filecomm),
        set_stream(output,Filecomm),
	(  tt(_), fail ;
           set_stream(output,stdout),
           close(Filecomm)   ).

tt(N) :- 
         suspend( (write('Test No. '), writeln(N)), 2, N->constrained),
         t(N).

btest(Goal) :-
	term_variables(Goal,List),
	write('Goal = '), write(List), writeln(':'),
	write('    '),  writeln(Goal),
	subcall((Goal,wake),D),
	write('Answer = '), mprintf(List), nl,
	writeln('Delayed goals = '), write('    '), writelist(D).

%mprintf([]) :- !,write('[]').
%mprintf(Term) :- printf('%QPmw',Term).


mprintf(Var) :- var(Var), !, printf('%QPmw',Var).
mprintf([] ) :- !.
mprintf(Atom) :- atomic(Atom), !, printf('%QPmw',Atom).
mprintf([H|T]) :- !, write('['), mpl([H|T]),  write(']').
mprintf(Term) :- Term=..[F|Args], write(F),
	mprintargs(Args).
mprintargs(Args) :- write('('),mpl(Args),write(')').

mpl([H|T]) :- var(T), !, mprintf(H), write('|'), mprintf(T).
mpl([H]) :- !, mprintf(H).
mpl([H|T]) :- mprintf(H), write(', '), mpl(T).


writelist([]) :- writeln(none), nl.
writelist([H|T]) :- writeln(H), wl(T).
wl([]) :- nl.
wl([H|T]) :- writeln(H), wl(T).

%:- use_module('/home/lp/mark/propia/propia').


mymember(H,[H|_T]).
mymember(X,[_Y|T]) :- mymember(X,T).

 notin3to6(X) :- X#<3.
 notin3to6(X) :- X#>6.

noclash(ST1,_D1,ST2,D2) :-
    ST1 #>= ST2+D2.
noclash(ST1,D1,ST2,_D2) :-
    ST2 #>= ST1+D1.

 and(true,true,true).
 and(true,false,false).
 and(false,true,false).
 and(false,false,false).

product(p1,1,19,1).
product(p2,2,17,2).
product(p3,3,15,3).
product(p4,4,13,4).
product(p5,10,8,5).
product(p6,16,4,4).
product(p7,17,3,3).
product(p8,18,2,2).
product(p9,19,1,1).

sum([],0,0,0,_).
sum([Name|Products],Count1,Count2,Profit,Language) :- 
    [Count1,Count2,Profit]::0..100,
    product(Name,Ct1a,Ct2a,Profita) infers Language,
    Count1 #= Ct1a+Ct1b,
    Count2 #= Ct2a+Ct2b,
    Profit #= Profita+Profitb,
    sum(Products,Ct1b,Ct2b,Profitb,Language).

solve(Products,Batch,Max1,Max2,MinProfit,Language) :-
    length(Products,Batch),
    Comp1 #<= Max1,
    Comp2 #<= Max2,
    Profit #>= MinProfit,
    sum(Products,Comp1,Comp2,Profit,Language),
    labeling(Products).

three(Goal) :-
	setval(count,3),
	nans(Goal).

nans(Goal) :- call(Goal), decval(count), ( getval(count,0), ! ; true).

p(1,a).
p(2,f(_Z)).
p(3,3).

delay element(_,List,_) if var(List).
element(1,[H|_Tail],H).
element(N,[_H|Tail],Val) :-
    plus(M,1,N),
    element(M,Tail,Val).

gp_element(I,L,V) :- element(I,L,V) infers most.


q(1,f(a)).
q(2,f(b)).
q(3,c).

t(1) :- btest(mymember(_X,[a,b,c]) infers most).

t(2) :- btest(mymember(_X,_Y) infers most).

t(3) :- btest((call(X::1..10,fd_domain), notin3to6(X) infers most)).

t(4) :- 
	btest((call([ST1,ST2] :: 1..10,fd_domain), noclash(ST1,5,ST2,7) infers most)).

t(5) :- btest((and(X,Y,_Z) infers most, X=Y)).

t(6) :- btest( three(solve(_P, 9, 85, 85, 40,ac)) ).

t(7) :- btest( three(solve(_P, 9, 85, 85, 40,most)) ).

/* A harder one:
solve(_P, 9, 82, 81, 40,most)
*/

t(8) :- btest(( product(_P,Ct1,Ct2,Prof) infers range, Ct1*>=3, Ct2*>=3 )).

t(9) :- btest(p(_X,_Y) infers most).

t(10) :- btest((X::[1, 3], p(X, _Y) infers most)).

t(11) :- btest(p(2,_Y) infers most).

t(12) :- btest(p(_X,_Y) infers unique).

t(13) :- btest(p(X,X) infers unique).

t(14) :- btest(p(2,_X) infers unique).

t(15) :- btest(p(1,_Y) infers consistent).

t(16) :- btest(p(1,a) infers consistent).

t(18) :- btest(( call(V::3..5,fd_domain), gp_element(I,[2,V,8],Val), Val#<=4, I#>1)).

t(19) :- btest(q(_X,_Y) infers most).

t(22) :- btest(( p(_X,f(_Z)) infers most )).

t(24) :- btest(( p(_X,f(Y)) infers unique, 
                         member(Y,[b,c,d]) infers most
              )).


