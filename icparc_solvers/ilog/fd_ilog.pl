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
% Contributor(s): 
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: fd_ilog.pl,v 1.1 2006/09/23 01:54:04 snovello Exp $
% ----------------------------------------------------------------------

% $Id: fd_ilog.pl,v 1.1 2006/09/23 01:54:04 snovello Exp $

:- module_interface(fd_ilog).

:- lib(forall).
:- pragma(expand).

:- op(600, xfx, [..]).
:- op(700, xfx, [::]).

:- op(700, xfx, [#=, ##, #\=, #>, #<, #>=, #<=]).
:- op(750, fy, #\+).
:- op(760, yfx, #/\).
:- op(770, yfx, #\/).
:- op(780, yfx, #=>).
:- op(790, yfx, #<=>).
:- op(800, xfx, isd).

:- use_module(ilog).


:- begin_module(fd_ilog).

:- setval(max_size_enumerated_domain, 1000).

% Domain constraint
:- export :: /2, :: /3.

% Basic contraints
:- export
     element/3,
     alldistinct/1,
     alldifferent/1,
     atmost/3,
     outof/2,
     # /3.


% Search
:- export
     indomain/1,
     labeling/1,
     deleteff/3,
     deletemin/3.

% Misc operations on domains
:- export
     dom/2.

% Solver specific contraints
:- export
     distribute/3,
     path/4.

% Arithmetic and boolean constraints
:- export #= /2,
          ## /2,
          #\= /2,
          #> /2,
          #< /2,
          #>= /2,
          #<= /2,
          (#\+) /1,
	  #/\ /2,
	  #\/ /2,
	  #=> /2,
	  #<=> /2,
	  isd /2.


Vars :: Domain :-
  (integer(Vars) ; var(Vars) ), !, % A single declaration
  [Vars] :: Domain.
Vars :: Domain :-
  ( foreach(_, Vars) do true ),
  block(parse_domain(Domain, Dom), parse_values, parse_domain_exception(Vars :: Domain)),
  !,
  ( Dom = interval(Min, Max) ->
      ( foreach(V, Vars), param(Min), param(Max) do ilog_intvar(V, Min, Max) )
  ; Dom = values(Values) ->
      ( foreach(V, Vars), param(Values) do ilog_intvar(V, Values) )
  ; error(4, (Vars :: Domain)-"Bad domain")
  ).
Vars :: Domain :-
  error(4, Vars :: Domain).



parse_domain(Min..Max, interval(Min, Max)).
parse_domain([], values([])).
parse_domain([X | Xs], values(Values)) :-
  parse_values([X|Xs], Values).

parse_values([], []).
parse_values([X | Xs], Values) :-
  ( integer(X) ->
      Values = [X | OtherValues]
  ; X = Min..Max,
    ( Max - Min > getval(max_size_enumerated_domain) ->
       exit_block(parse_values)
    ; ( for(I, Min, Max), fromto(Values, [I|R], R, OtherValues) do true )
    )
  ),
  parse_values(Xs, OtherValues).


parse_domain_exception(Goal) :-
  error(4, Goal-"Sub-interval is too large ").


::(V, Min..Max, B) :-
  !,
  B isd (V #>= Min #/\ V#<= Max).
::(V, D, B) :-
  error(4, ::(V, D, B)).


element(Index, List, Value) :-
  nonvar(List), 
  checklist(integer_or_var, List), !,
  ilog_add(element(Index, List, Value)).
element(Index, List, Value) :-
  error(4, element(Index, List, Value)-"2nd arg: Ground integer list expected").


% whenValue propagation
alldistinct(List) :-
  nonvar(List),
  checklist(integer_or_var, List), !,
  Array =.. [alldistint | List],
  ilog_add(alldistinct(Array)).
alldistint(List) :-
  error(4, alldistint(List)).

% whenDomain propagation
alldifferent(List) :-
  nonvar(List),
  checklist(integer_or_var, List), !,
  Array =.. [alldifferent | List],
  ilog_add(alldifferent(Array)).
alldifferent(List) :-
  error(4, alldifferent(List)).

    integer_or_var(X) :- var(X), !.
    integer_or_var(X) :- integer(X).



distribute(CardsValues, Vars, Level) :-
  nonvar(CardsValues),
  ( foreach(Card-Value, CardsValues), foreach(Card, Cards),
    foreach(Value, Values)
  do
    integer(Value), integer_or_var(Card)
  ),
  nonvar(Vars),
  checklist(integer_or_var, Vars),
  ( Level = basic -> CodeLevel=0 ; Level = extended -> CodeLevel=1),
  !,
  ArrayCards =.. [distribute | Cards],
  ArrayValues =.. [distribute | Values],
  ArrayVars =.. [distribute | Vars],
  ilog_add(distribute(ArrayCards, ArrayValues, ArrayVars, CodeLevel)).
distribute(CardsValues, Vars, Level) :-
  error(4, distribute(CardsValues, Vars, Level)).


atmost(N, List, V) :-
  integer(N), integer(V),
  nonvar(List), checklist(integer_or_var, List),
  !,
  C :: 0..N,
  distribute([C-V], List, extended).

/*
indomain(X) :-
  dvar_domain(X, D), dom_range(D, Min, _Max),
  ( X = Min ; X #> Min, indomain(X) ).
*/

indomain(X) :- integer(X), !.
indomain(X) :-
  setmin(X) ; removemin(X), indomain(X).

% debugging ... labeling(L) :- checklist(indomain, L).
labeling([]).
labeling([X|Xs]) :-
  indomain(X),
  labeling(Xs).

X #= Y :- ilog_add(X #= Y).
X ## Y :- ilog_add(X ## Y).
X #\= Y :- ilog_add(X #\= Y).
X #> Y :- ilog_add(X #> Y).
X #< Y :- ilog_add(X #< Y).
X #>= Y :- ilog_add(X #>= Y).
X #<= Y :- ilog_add(X #<= Y).
#\+ Y :- ilog_add(#\+ Y).
X #/\ Y :- ilog_add(X #/\ Y).
X #\/ Y :- ilog_add(X #\/ Y).
X #=> Y :- ilog_add(X #=> Y).
X #<=> Y :- ilog_add(X #<=> Y).
1 isd C :- -?-> ilog_add(C).
0 isd C :- -?-> ilog_add(#\+ C).
B isd Y :- B :: 0..1, ilog_add(B isd Y).



deletemin(Var, List, Rest) :-
    List = [H|T],
    dvar_domain(H, Dom), dom_range(Dom, Min, _),
    find_min_domain(T, List, Min, Chosen, Rest),
    Var = Chosen.

%:- mode find_min_domain(+,+,+,-,-).
find_min_domain([], SoFar, _OldMin, BestVar, Rest) :- !,
    SoFar = [BestVar|Rest].
find_min_domain(List, SoFar, OldMin, BestVar, Rest) :-
    List = [Var|Vars],
    dvar_domain(Var, Dom), dom_range(Dom, NewMin, _),
    ( NewMin >= OldMin ->			% keep old one
	find_min_domain(Vars, SoFar, OldMin, BestVar, Rest)
    ;						% new optimum
    	copy_until(SoFar, List, Rest, Rest0),
	find_min_domain(Vars, List, NewMin, BestVar, Rest0)
    ).


deleteff(Var, List, Rest) :-
    List = [H|T],
    dvar_domain(H, Dom), dom_size(Dom, Size),
    find_min_size(T, List, Size, Chosen, Rest),
    Var = Chosen.

%:- mode find_min_size(+,+,+,-,-).
find_min_size([], SoFar, _OldSize, BestVar, Rest) :- !,
    SoFar = [BestVar|Rest].
find_min_size(List, SoFar, OldSize, BestVar, Rest) :-
    List = [Var|Vars],
    dvar_domain(Var, Dom), dom_size(Dom, NewSize),
    ( NewSize >= OldSize ->			% keep old one
	find_min_size(Vars, SoFar, OldSize, BestVar, Rest)
    ;						% new optimum
    	copy_until(SoFar, List, Rest, Rest0),
	find_min_size(Vars, List, NewSize, BestVar, Rest0)
    ).


    % Copy list In until a tail matching Until is reached.
    % Output in difference list Out-Out0
    copy_until(In, Until, Out, Out0) :-
    	( In == Until ->
	    Out = Out0
	;
	    In = [X|In1],
	    Out = [X|Out1],
	    copy_until(In1, Until, Out1, Out0)
	).

outof(X, List) :-
  ilog_add(outof(X, List)).

#(Min, Cstrs, Max) :-
  length(Cstrs, N),
  [Min, Max] :: 0..N,
  ( foreach(C, Cstrs), foreach(B, Bs) do B isd C ),
  Sum #= sum(Bs), Min #<= Sum, Max #>= Sum, Min #<= Max.



path(NextCumulArray, TransitFunction, MaxNbPaths, WhenEvent) :-
  integer(MaxNbPaths),
  ( WhenEvent = value, WhenEventCode=0 ; WhenEvent = domain, WhenEventCode=1 ),
  functor(NextCumulArray, _, NbNodes),
  NbNodes >= 2*MaxNbPaths,
  ( foreacharg(Next-_Cumul, NextCumulArray),
    param(NbNodes)
  do
    Next :: 0..NbNodes % NbNodes+1 for end nodes when MaxNbPaths=0
  ),
  !,
  functor(ArrayDistances, distances, NbNodes),
  ( foreacharg(ArrayDists, ArrayDistances), fromupto(0, I, _),
    param(NbNodes), param(TransitFunction)
  do
    functor(ArrayDists, dists, NbNodes),
    ( foreacharg(D, ArrayDists), fromupto(0, J, _),
      param(I), param(TransitFunction)
    do
      TransitFunction =.. [Predicate | Params],
      append(Params, [I, J, D], Args),
      Goal =.. [Predicate | Args],
      call(once(Goal), eclipse)
    )
  ), % Distances computed
  ilog_add(path(NextCumulArray, ArrayDistances, MaxNbPaths, WhenEventCode)).

path(N, T, M, W) :-
  error(4, path(N, T, M, W)).
