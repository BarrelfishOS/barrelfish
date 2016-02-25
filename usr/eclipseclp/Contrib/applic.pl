
%   File   : APPLIC.PL
%   Author : Lawrence Byrd + Richard A. O'Keefe
%   Updated: 4 August 1984 and Ken Johnson 11-8-87
%   Purpose: Various "function" application routines based on apply/2.
%   Needs  : append/3 from listut.pl

%   Updated for SEPIA by Joachim Schimpf, ECRC, 14.2.91
%   - introduced tools
%   - made &-operator local

:- module(applic).			% SEPIA header
:- export
    apply/2,
    callable/1,
    checkand/2,
    checklist/2,
    convlist/3,
    exclude/3,
    mapand/3,
    maplist/3,
    some/2,
    somechk/2,
    sublist/3.

:- mode
    apply(+, +),
    callable(?),
    checkand(+, +),
    checklist(+, +),
    convlist(+, +, ?),
    exclude(+, +, ?),
    mapand(+, ?, ?),
    maplist(+, ?, ?),
    some(+, ?),
    somechk(+, +),
    sublist(+, +, ?).

% Operator declaration used below

:-  op(920,xfy,&).      % Used for conjunction


%   apply(Pred, Args)
%   is the key to this whole module.  It is basically a variant of call/1
%   (see the Dec-10 Prolog V3.43 manual) where some of the arguments may
%   be already in the Pred, and the rest are passed in the list of Args.
%   Thus apply(foo, [X,Y]) is the same as call(foo(X,Y)),
%   and apply(foo(X), [Y]) is also the same as call(foo(X,Y)).
%   BEWARE: any goal given to apply is handed off to call/1, which is the
%   Prolog *interpreter*, so if you want to apply compiled predicates you
%   MUST have :- public declarations for them.  The ability to pass goals
%   around with some of their (initial) arguments already filled in is
%   what makes apply/2 so useful.  Don't bother compiling anything that
%   uses apply heavily, the compiler won't be able to help much.  LISP
%   has the same problem.  Later Prolog systems may have a simpler link
%   between compiled and interpreted code, or may fuse compilation and
%   interpretation, so apply/2 may come back into its own.  At the moment,
%   apply and the routines based on it are not well thought of.

:- tool(apply/2, apply/3).

apply(Pred, Args, Module) :-
    (   atom(Pred),
        Goal =.. [Pred|Args]
    ;                   %   compound(Pred)
        Pred =.. OldList,
        append(OldList, Args, NewList),
        Goal =.. NewList
    ),  !,
    call(Goal)@Module.



%   callable(Term)
%   succeeds when Term is something that it would make sense to give to
%   call/1 or apply/2.  That is, Term must be an atom or a compound term;
%   variables and integers are out.

callable(Term) :-
    nonvar(Term),
    functor(Term, FunctionSymbol, _),
    atom(FunctionSymbol).



%   checkand(Pred, Conjunction)
%   succeeds when Pred(Conjunct) succeeds for every Conjunct in the
%   Conjunction.  All the *and predicates in this module assume that
%   a&b&c&d is parsed as a&(b&(c&d)), and that the "null" conjunction
%   is 'true'.  It is possible for this predicate, and most of the
%   others, to backtrack and try alternative solutions.  If you do not
%   want that to happen, copying one of these predicates and putting a
%   cut in the suggested place will produce a tail-recursive version.
%   The cuts in the *and predicates are there because "non-and" is
%   defined by exclusion; they cannot be assigned types.

:- tool(checkand/2, checkand/3).

checkand(_, true, _) :- !.
checkand(Pred, A&B, Module)  :- !,
    apply(Pred, [A], Module),
    checkand(Pred, B, Module).
checkand(Pred, A, Module) :-
    apply(Pred, [A], Module).



%   checklist(Pred, List)
%   suceeds when Pred(Elem) succeeds for each Elem in the List.
%   In InterLisp, this is EVERY.  It is also MAPC.

:- tool(checklist/2, checklist/3).

checklist(_, [], _).
checklist(Pred, [Head|Tail], Module) :-
    apply(Pred, [Head], Module),
    checklist(Pred, Tail, Module).



%   mapand(Rewrite, OldConj, NewConj)
%   succeeds when Rewrite is able to rewrite each conjunct of OldConj,
%   and combines the results into NewConj.

:- tool(mapand/3, mapand/4).

mapand(_, true, true, _) :- !.
mapand(Pred, Old&Olds, New&News, Module) :- !,
    apply(Pred, [Old,New], Module),
    mapand(Pred, Olds, News, Module).
mapand(Pred, Old, New, Module) :-
    apply(Pred, [Old,New], Module).



%   maplist(Pred, OldList, NewList)
%   succeeds when Pred(Old,New) succeeds for each corresponding
%   Old in OldList, New in NewList.  In InterLisp, this is MAPCAR. 
%   It is also MAP2C.  Isn't bidirectionality wonderful?

:- tool(maplist/3, maplist/4).

maplist(_, [], [], _).
maplist(Pred, [Old|Olds], [New|News], Module) :-
    apply(Pred, [Old,New], Module),
    maplist(Pred, Olds, News, Module).



%   convlist(Rewrite, OldList, NewList)
%   is a sort of hybrid of maplist/3 and sublist/3.
%   Each element of NewList is the image under Rewrite of some
%   element of OldList, and order is preserved, but elements of
%   OldList on which Rewrite is undefined (fails) are not represented.
%   Thus if foo(X,Y) :- integer(X), Y is X+1.
%   then convlist(foo, [1,a,0,joe(99),101], [2,1,102]).

:- tool(convlist/3, convlist/4).

convlist(_, [], [], _).
convlist(Pred, [Old|Olds], NewList, Module) :-
    apply(Pred, [Old,New], Module),
    !,
    NewList = [New|News],
    convlist(Pred, Olds, News, Module).
convlist(Pred, [_|Olds], News, Module) :-
    convlist(Pred, Olds, News, Module).



%   exclude(Pred, List, SubList)
%   succeeds when SubList is the SubList of List containing all the
%   elements for which Pred(Elem) is *false*.  That is, it removes
%   all the elements satisfying Pred.  Efficiency would be somewhat
%   improved if the List argument came first, but this argument order
%   was copied from the older sublist/3 predicate, and let's face it,
%   apply/2 isn't stupendously efficient itself.  

:- tool(exclude/3, exclude/4).

exclude(_, [], [], _).
exclude(Pred, [Head|List], SubList, Module) :-
    apply(Pred, [Head], Module),
    !,
    exclude(Pred, List, SubList, Module).
exclude(Pred, [Head|List], [Head|SubList], Module) :-
    exclude(Pred, List, SubList, Module).



%   some(Pred, List)
%   succeeds when Pred(Elem) succeeds for some Elem in List.  It will
%   try all ways of proving Pred for each Elem, and will try each Elem
%   in the List.  somechk/2 is to some/2 as memberchk/2 is to member/2;
%   you are more likely to want somechk with its single solution.
%   In InterLisp this is SOME.

:- tool(some/2, some/3).

some(Pred, [Head|_], Module) :-
    apply(Pred, [Head], Module).
some(Pred, [_|Tail], Module) :-
    some(Pred, Tail, Module).


:- tool(somechk/2, somechk/3).

somechk(Pred, [Head|_], Module) :-
    apply(Pred, [Head], Module),
    !.
somechk(Pred, [_|Tail], Module) :-
    somechk(Pred, Tail, Module).



%   sublist(Pred, List, SubList)
%   succeeds when SubList is the sub-sequence of the List containing all
%   the Elems of List for which Pred(Elem) succeeds.

:- tool(sublist/3, sublist/4).

sublist(_, [], [], _).
sublist(Pred, [Head|List], SubList, Module) :-
    apply(Pred, [Head], Module),
    !,
    SubList = [Head|Rest],
    sublist(Pred, List, Rest, Module).
sublist(Pred, [_|List], SubList, Module) :-
    sublist(Pred, List, SubList, Module).



