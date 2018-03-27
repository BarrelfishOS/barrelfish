:- module(allocator).

:- lib(ic).

:- export alloc_range/3.
:- export free_range/2.
:- export alloc_one/2.
:- export free_one/2.

% The ranges in use
:- dynamic range/3. % Tag, Start, End. Both (Start and End) are inclusive.

% Just find the highest possible
is_free_range(Tag, _, Start) :-
   findall(End, range(Tag, _, End), UsedEndLi),
   Start #>= 0,
   (foreach(End, UsedEndLi), param(Start) do
       Start #>= End
   ).

assert_range(Tag, Start, Size) :-
    End is Start + Size,
    assert(range(Tag,Start,End)).

free_range(Tag, Start) :-
    retract(range(Tag, Start, _)).

alloc_range(Tag, Size, Start) :- 
   is_free_range(Tag, Size, Start),
   labeling([Start]),
   !,
   End is Start + Size - 1,
   assert_range(Tag, Start, Size).

alloc_one(Tag, X) :- alloc_range(Tag, 1, X).
free_one(Tag, X) :- free_range(Tag, X).
