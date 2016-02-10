:- module(assoc).			% SEPIA header
:- export
	assoc_to_list/2,
	put_assoc/4,
	get_assoc/3,
	map_assoc/3,
	list_to_assoc/2,
	list_to_assoc/4.		% needed in map.pl
:- lib(apply).


%   File   : ASSOC.PL
%   Author : R.A.O'Keefe
%   Updated: 9 November 1983
%   Rewritten: K Johnson 25-5-87
%   Ported, fixed and somewhat optimised for SEPIA by J.Schimpf, ECRC 1991
%   Purpose: Binary tree implementation of "association lists".

%   Note   : the keys should be ground, the associated values need not be.


%   This stores association pairs in a tree structure; the empty tree
%   is just "t". For example, to store the pair "foo-moo"  in a hitherto
%   empty tree
%   put_assoc(foo,t,moo,T)
%   To add to tree T the pair "bee-flea" giving the tree U
%   put_assoc(bee,T,flea,U)
%   Test data are at the end of the file to help


%
% put_assoc(+Key, +Old_tree, ?Value, ?New_Tree).
% Create a new tree by putting a new pair into an existing one.
%

put_assoc(Key, t, Val, New) :- !,		% Empty tree
	New = t(Key,Val,t,t).
put_assoc(Key, Old, Val, New) :-
	Old = t(K,_,_,_),
	compare(Rel, Key, K),
	put_assoc(Key, Old, Val, New, Rel).

% Note that SEPIA will properly index on the last argument:

put_assoc(Key, t(_,_,L,R), Val, t(Key,Val,L,R), =).	% replace 
put_assoc(Key, t(K,V,L,R), Val, t(K,V,NewL,R), <) :-
	put_assoc(Key, L, Val, NewL).
put_assoc(Key, t(K,V,L,R), Val, t(K,V,L,NewR), >) :-
	put_assoc(Key, R, Val, NewR).



%
% get_assoc gets the Val associated with Key in a tree.
%   get_assoc(foo,+Tree,-V)
% will find the value associated with "foo" in "Tree".
% If Key is uninstantiated then get_assoc will work sensibly for the calling
% patterns
%   get_assoc(-Key,+Tree,-V)
% which will find every K-V pair on back tracking and
%   get_assoc(-Key,+Tree,+V)
% although the pattern get_assoc(-K,+T,+V) is *time consuming*.
%

get_assoc(Key, Tree, Val) :-
	var(Key), !,
	gen_assoc(Key, Tree, Val).
get_assoc(Key, Tree, Val) :-
	Tree = t(K,_,_,_),
	compare(Rel, Key, K),
	get_assoc(Key, Tree, Val, Rel).

get_assoc(_Key, t(_,Val,_,_), Val, =).
get_assoc(Key, t(_,_,L,_), Val, <) :-
	get_assoc(Key, L, Val).
get_assoc(Key, t(_,_,_,R), Val, >) :-
	get_assoc(Key, R, Val).


gen_assoc(Key, t(_,Val,L,_), Val) :-
	gen_assoc(Key, L, Val).
gen_assoc(Key, t(Key,Val,_,_), Val).
gen_assoc(Key, t(_,Val,_,R), Val) :-
	gen_assoc(Key, R, Val).


%
% assoc_to_list(+Assoc,-List)
% Converts the tree to a list of the form [Key1-Val1, Key2-Val2...]
%

assoc_to_list(Assoc, List) :-
    assoc_to_list(Assoc, List, []).

assoc_to_list(t, Left, Right) :- !,
	Left = Right.
assoc_to_list(t(Key,Val,L,R), Left, Right) :-
    assoc_to_list(L, Left, [Key-Val|Mid]),
    assoc_to_list(R, Mid, Right).


%
% list_to_assoc(+List, -Assoc)
% produces the shortest possible Assoc tree
%

list_to_assoc(List, Assoc) :-
	keysort(List, Keys),
	length(Keys, N),
	list_to_assoc(N, Keys, Assoc, []).


list_to_assoc(0, List, t, List) :- !.
list_to_assoc(N, List, t(Key,Val,L,R), Rest) :-
        A is (N-1) // 2,
        Z is (N-1)-A,
        list_to_assoc(A, List, L, [Key-Val|More]),
        list_to_assoc(Z, More, R, Rest).


%
% map_assoc(+Pred,+In_tree,-Out_tree)
% Calls Pred(X,Y) for every X on the tree.
% Constructs a tree of Ys.
%

:- tool(map_assoc/3, map_assoc/4).

map_assoc(_, t, t, _) :- !.
map_assoc(Pred, t(Key,Val,L0,R0), t(Key,Ans,L1,R1), Module) :-
	apply(Pred, [Val, Ans])@Module,
	map_assoc(Pred, L0, L1, Module),
	map_assoc(Pred, R0, R1, Module).


/* Test

insert(K,V) :-            % Insert pair K,V into the recorded
  recorded(tree,T,Ref),       % tree. Note, the code above does not
  put_assoc(K,T,V,T1),        % record the tree anywhere. You have to
  erase(Ref),         % do it yourself.
  record(tree,T1,_).

test(_) :-                % Test(T) will build up a small tree
  recorded(tree,_,Ref),       % Remove any existing tree(s)
  erase(Ref),
  fail.

test(T) :-
  record(tree,t,_),       % Create an empty tree
  insert(mean, bean),     % Hang some rhyming pairs off it
  insert(hoe,go),
  insert(foo,you),
  insert(bee,flea),
  insert(jack,stack),
  insert(nick,quick),
  insert(why,sky),
  insert(word,bird),
  insert(funny,money),
  insert(ping,sing),
  recorded(tree,T,_).
                  % Usage of assoc_to_list
balance_tree(T,B) :-          % This balances the tree +T giving -B
  assoc_to_list(T,L),     % If you need balanced trees, of course,
  assoc_to_list(B,L).     % there are better ways than this

% a test for map_assoc: the "abbrev" predicate deletes all letters
% in a name after the third

test_map(T,U) :-      % Call test_map(-T,-U)
  recorded(tree,T,_),
  map_assoc(abbrev,T,U).

abbrev(Long,Cut) :-
  name(Long,Letters),
  abbrev_list(3,Letters,Fewer_letters),
  name(Cut,Fewer_letters).

abbrev_list(_,[],[]) :- !.
abbrev_list(0,_,[]) :- !.
abbrev_list(N,[H|T],[H|U]) :-
  M is N - 1,
  abbrev_list(M,T,U).
*/
