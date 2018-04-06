:- module(allocator).

:- lib(ic).

:- export alloc_one/4.
:- export free_one/4.

% The ranges in use
% range/3 -> Tag, Start, End. Both (Start and End) are inclusive.

% Just find the highest possible
is_free_range(S, Tag, _, Start) :-
   findall(End, state_query(S, range(Tag, _, End)), UsedEndLi),
   Start #>= 0,
   (foreach(End, UsedEndLi), param(Start) do
       Start #>= End
   ).

assert_range(S, Tag, Start, Size, NewS) :-
    End is Start + Size,
    state_add(S, range(Tag,Start,End), NewS).

free_range(S, Tag, Start, NewS) :-
    state_remove(S, range(Tag, Start, _), NewS).

alloc_range(S, Tag, Size, Start, NewS) :- 
   is_free_range(S, Tag, Size, Start),
   labeling([Start]),
   !,
   End is Start + Size - 1,
   assert_range(S, Tag, Start, Size, NewS).

alloc_one(S, Tag, X, NewS) :- alloc_range(S, Tag, 1, X, NewS).
free_one(S, Tag, X, NewS) :- free_range(S, Tag, X, NewS).
