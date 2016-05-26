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
% Contributor(s): Andrew Eremin, IC-Parc
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
%
% Description:	ECLiPSe n-ary search tree library
%
% System:	ECLiPSe Constraint Logic Programming System
% Author/s:	Andrew Eremin, IC-Parc
%
% ----------------------------------------------------------------------


% ----------------------------------------------------------------------
:- module(n_trees).

:- ensure_loaded(b_trees).

% ----------------------------------------------------------------------
%
% user level predicates
%
% ----------------------------------------------------------------------

% ----------------------------------------------------------------------
% tree creation
:- export
   n_tree/2.

:- mode n_tree(++, -).
n_tree(Order, Tree) :-
        b_trees:b_tree(Order, Tree).

% ----------------------------------------------------------------------
% node expansion and fathoming
:- export
   n_tree_expand/3,
   n_tree_fathom/1.

:- mode n_tree_expand(+, ++, -).
n_tree_expand(Tree, Rank, Child) :-
        n_tree_get(status, Tree, PStatus),
        n_tree_get(rank, Tree, PRank),
        ( PStatus == open ->
            % has no children yet, create a lchild under the node in
            % the b_tree with rank PRank to be the head of the child
            % b_tree, and a lchild under that with rank Rank to be the
            % actual child node
            b_trees:b_tree_expand(Tree, PRank, CTreeHead),
            b_trees:b_tree_expand(CTreeHead, Rank, Child)
        ; PStatus == expanded ->
            % has chidren, create a rchild under the last rchild in
            % the child b_tree with rank PRank to be the head of the
            % rest of the child b_tree, and a lchild under that with
            % rank Rank to be the actual child node
            b_trees:lchild(Tree, LCTreeHead),
            last_child_head(LCTreeHead, RCTreeHead),
            b_trees:b_tree_expand(RCTreeHead, PRank, CTreeHead),
            b_trees:b_tree_expand(CTreeHead, Rank, Child)
        ; % PStatus == fathomed
            fail
        ).

last_child_head(CTreeHead, RCTreeHead) :-
        ( b_trees:rchild(CTreeHead, RTreeHead) ->
            last_child_head(RTreeHead, RCTreeHead)
        ;
            RCTreeHead = CTreeHead
        ).

:- mode n_tree_fathom(+).
n_tree_fathom(Leaf) :-
        b_trees:b_tree_fathom(Leaf).

% ----------------------------------------------------------------------
% tree access/update
:- export
   n_tree_get/3,
   n_tree_set/3.

:- mode n_tree_get(++, +, ?).
n_tree_get(depth, Tree, Val) ?- !,
        ( parent(Tree, Parent) ->
            b_trees:b_tree_get(depth, Parent, PDepth),
            Val is PDepth + 1
        ;
            Val = 1
        ).
n_tree_get(What, Tree, Val) :-
        b_trees:b_tree_get(What, Tree, Val).

:- mode n_tree_set(++, +, ?).
n_tree_set(What, Tree, Val) ?- !,
        b_trees:b_tree_set(What, Tree, Val).
           
% ----------------------------------------------------------------------
% testing
:- export
   is_n_tree/1.

:- mode is_n_tree(+).
is_n_tree(Tree) :-
        b_trees:is_b_tree(Tree).

% ----------------------------------------------------------------------
% tree navigation
:- export
   parent/2,
   ancestor/2,
   lsib/2,
   rsib/2,
   sib/2,
   child/2,
   descendant/2,
   next/3.

:- mode parent(+, ?).
parent(Node, Parent) :-
        b_trees:parent(Node, BParent),
        ( n_tree_get(id, BParent, Id),
          0 is getbit(Id, 0) ->
            b_trees:parent(BParent, Parent)
        ;
            parent(BParent, Parent)
        ).

:- mode ancestor(+, ?).
ancestor(Node, Ancestor) :-
        parent(Node, Parent),
        ( Ancestor = Parent ; ancestor(Parent, Ancestor) ).

:- mode lsib(+, ?).
lsib(Node, Sib) :-
        b_trees:parent(Node, BParent),
        n_tree_get(id, BParent, Id),
        1 is getbit(Id, 0),
        b_trees:parent(BParent, LSibBParent),
        lsib_body(LSibBParent, Sib).

:- mode lsib_body(+, ?).
lsib_body(BParent, Sib) :-
        n_tree_get(id, BParent, Id),
        1 is getbit(Id, 0),
        b_trees:parent(BParent, LBParent),
        lsib_body(LBParent, Sib).
lsib_body(BParent, Sib) :-
        b_trees:lchild(BParent, Sib).

:- mode rsib(+, ?).
rsib(Node, Sib) :-
        b_trees:parent(Node, BParent),
        b_trees:rchild(BParent, RSibBParent),
        rsib_body(RSibBParent, Sib).

:- mode rsib_body(+, ?).
rsib_body(BParent, Sib) :-
        b_trees:lchild(BParent, Sib).
rsib_body(BParent, Sib) :-
        b_trees:rchild(BParent, RBParent),
        rsib_body(RBParent, Sib).

:- mode sib(+, ?).
sib(Node, Sib) :-
        ( lsib(Node, Sib) ; rsib(Node, Sib) ).

:- mode child(+, ?).
child(Node, Child) :-
        b_trees:lchild(Node, CBParent),
        rsib_body(CBParent, Child).

:- mode descendant(+, ?).
descendant(Node, Descendant) :-
        child(Node, Child),
        ( Descendant = Child ; descendant(Child, Descendant) ).

:- mode next(++, +, ?).
next(bfs, Node, Next) :-
        n_tree_get(status, Node, Status),
        ( Status == open ->
            % Node has not been expanded yet
            Next = Node
        ; Status == fathomed ->
            % subtree rooted at Node is fathomed
            fail
        ;
            b_trees:next(bfs, Node, Next)
        ).
next(bfs(Id), Node, Next) ?-
        b_trees:tree_get_node(Id, Node, SubTree),
        next(bfs, SubTree, Next).
next(dfs, Node, Next) :-
        n_tree_get(status, Node, Status),
        ( Status == open ->
            % Node has not been expanded yet
            Next = Node
        ; Status == fathomed ->
            % subtree rooted at Node is fathomed
            fail
        ;
            b_trees:next(dfs, Node, Next)
        ).
next(dfs(Id), Node, Next) ?-
        b_trees:tree_get_node(Id, Node, SubTree),
        next(dfs, SubTree, Next).

% ----------------------------------------------------------------------

:- comment(categories, ["Constraints"]).
:- comment(summary, "n-ary search tree library").
:- comment(author, "Andrew Eremin").
:- comment(copyright, "Cisco Systems, INc.").
:- comment(status, prototype).

:- comment(include, n_trees_comments).
