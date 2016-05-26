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
:- use_module(fd).

t :-
  
  ilog_test.

/* Instanciation */
t1(V) :-
  
  V ::  1..10,
  ( V = 1 ; V = 2 ; V = 3 ; true).

/* Unification */
t2(V1, V2) :-
  
  V1 :: 1..10,
  V2 :: 5..15,
  V1 = V2.

/* Backtrack */
t3(V1, V2) :-
  
  V1 :: 1..10,
  V2 :: 5..15,
  V1 = 5, ( V2 = 10 ; true).

t4(V) :-
  
  V :: 1..10,
  V = 15.

t5(X) :-
  suspend(writeln(hello), 2, X->inst),
  
  X:: 1..10,
  writeln(X),
  X = 5.

t6 :-   2 #= 3.
t7 :-  2 #= 2.

t8(X) :-  X :: 1..10, X #= 2.

t9(X) :-  X :: 1..10, suspend(writeln(hello), 2, X->inst), X #= 2.

t10(X) :-  X :: 1..10, X #< 6.

t11(X, Y) :-
   X :: 1..10, Y :: 1..10,
  (true ; X + Y #= 7, (true ; X #> 3, ( true ; Y #> 2 ) ) ).

t12(X, Y) :-
	 X :: 1..10, Y :: 1..10, element(X, [2,9,7], Y).

t13(L) :-
  length(L, 12), L :: 1..10, L=[O|_], O = 3.

t14(L) :-
  length(L, 12), L :: 1 .. 10, alldistinct(L), L=[X1, X2, X3 | _], X1 = 1, X2 #< 3, X3 #> 9.

t15(X1, X2) :-
   [X1,X2] :: 1..4, X1 - X2 ## 1, X2 = 3.

t16(X1, X2) :- 
   [X1,X2] :: 1..4, X1 ## 1 + X2, X2 = 3.

t17(L) :-  length(L, 4), L :: 0..1, sum(L) #= 1, L = [_X1,X2,X3,_X4], X3 = 0, X2 = 1.

t18(X) :-  X :: 1..10, min_max(indomain(X), 10-X).

t19(X) :-  X::1..10, #\+ (X##1).

t20([X1,X2,X3]) :-
   [X1, X2, X3] :: 1 .. 10, atmost(2, [X1, X2, X3], 1), X1=1, X3=1.

t21(X, V) :-  X :: 1..10, X ## 2, element(X, [1,10, 5], V).


t22 :-  X::1..10, suspend(writeln(hello), 3, X->ilog_range), X #< 5.

t23 :-  [X,Y]::1..10, suspend(writeln(hello), 3, X->ilog_range), X #< Y.


t24 :-
  [X,Y,Z,T] :: 1..10, element(X, [Y,3, Z, 12], T), T = 5, Z #> 7.

t25 :-
   [X,Y,Z] :: 1..10,
  suspend(writeln(hello), 3, X->ilog_range),
  element(I, [X, 3], Y), Y #<= Z, Z #< 8, I = 1.

t26 :-
   X::2..10, element(X, [1], Y).


puzzle1(Board) :- 
        Board = [NW,N,NE,W,E,SW,S,SE],

        Board :: 0..12,
        sum(Board) #= 12,
        NW + N + NE #= 5,
        NE + E + SE #= 5,
        NW + W + SW #= 5,
        SW + S + SE #= 5,

        labeling(Board),

        printf("%3d%3d%3d\n", [NW,N,NE]),
        printf("%3d   %3d\n", [ W,   E]),
        printf("%3d%3d%3d\n", [SW,S,SE]).


puzzle2(Coins) :- 
        Coins = [NRound,NSquare,NTriang],
        Coins :: 0..11,
        NRound*15 + NSquare*16 + NTriang*17 #= 121,
        NCoins #= sum(Coins),

        min_max(labeling(Coins), NCoins),

        printf("%d round, %d square, %d triangular\n", Coins).

transit(2,_, 1) :- !.
transit(0,_, 2) :- !.
transit(_, _, 0) :- !.

path :-
   path(t(N0-C0, N1-C1, N2-0), transit, 1, value).

bug1 :- X :: 1..10, findall(X, true, L), L=[Y], Y #= 1.
