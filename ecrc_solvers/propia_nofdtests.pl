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

tests(File) :-
	open(File,write,Filecomm),
        set_stream(output,Filecomm),
	(  t(_), fail ;
           set_stream(output,stdout),
           close(Filecomm)   ).

wt(N) :- write('Test No. '), writeln(N).

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


 and(true,true,true).
 and(true,false,false).
 and(false,true,false).
 and(false,false,false).


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

t(1) :- wt(1), btest(mymember(_X,[a,b,c]) infers most).

t(2) :- wt(2), btest(mymember(_X,_Y) infers most).


t(5) :- wt(5), btest((and(X,Y,_Z) infers most, X=Y)).


t(7) :- wt(7), btest(p(_X,_Y) infers most).

t(9) :- wt(9), btest(p(2,_Y) infers most).

t(10) :- wt(10), btest(p(_X,_Y) infers unique).

t(11) :- wt(11), btest(p(X,X) infers unique).

t(12) :- wt(12), btest(p(2,_X) infers unique).

t(13) :- wt(13), btest(p(1,_Y) infers consistent).

t(14) :- wt(14), btest(p(1,a) infers consistent).

t(19) :- wt(19), btest(q(_X,_Y) infers most).

t(20) :- wt(20), btest(( gp_element(_I,_L,_V) )).


t(22) :- wt(22), btest(( p(_X,f(_Z)) infers most )).


t(24) :- wt(24), btest(( q(X,f(Y)) infers unique, 
                         member(Y,[b,c,d]) infers most
	              )).


