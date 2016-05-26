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
% Copyright (C) 1995 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Andrew Eremin, IC-Parc
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
%
% Description:	ECLiPSe binary search tree library
%
% System:	ECLiPSe Constraint Logic Programming System
% Author/s:	Andrew Eremin, IC-Parc
%
% ----------------------------------------------------------------------


% ----------------------------------------------------------------------
%
% this module implements binary search trees, which have certain
% properties:
%
%      o Every tree is Tree either a leaf node with no children or an
%        internal node with either a lchild LChild or both a lchild
%        LChild and a rchild RChild,
%        i.e. children are created left to right
%      o Every Tree has an associated integer id such that for every
%        node Parent with id I, the ids of its lchild LChild and
%        rchild RChild (if any) are 2I and 2I+1
%      o Every tree has an associated numeric rank Rank and order
%        relation Rel such that for every node Parent with child Child
%        \+ (rank(Child) Rel rank(Parent)) is true,
%        i.e. the search is non-improving
%      o Every Tree has an associated status as follows:
%        a Tree that is a leaf may be open or fathomed
%        a Tree that has one or more children is fathomed if all its
%        children are fathomed and expanded otherwise
%
% ----------------------------------------------------------------------
:- module(b_trees).

% ----------------------------------------------------------------------
%
% (unbalanced, ordered) binary search tree structure
%
% ----------------------------------------------------------------------

:- local struct(
                b_tree(
                       id,          % integer: node number
                       depth,       % integer: depth in bfs tree
                                    %    (for bit-twiddling hack to
                                    %     determine nodes under a
                                    %     sub-tree)
                       order,       % atom (</>): the order for bfs
                                    %         rank comparison
                       rank,        % number: node rank for bfs node
                                    %         selection
                       status,      % atom: current status of the node
                                    %        open
                                    %        expanded
                                    %        fathomed
                       parent,      % b_tree struct: parent node
                                    %    in search tree
                       lchild,      % b_tree struct: left child
                                    %    in search tree
                       rchild,      % b_tree struct: right child
                                    %    in search tree
                       bfs_next,    % b_tree struct: best descendant
                                    %    in bfs order
                       data         % prolog term: arbitrary data
                                    %    associated with node
                      )
               ).

% ----------------------------------------------------------------------
%
% creating a new b_tree structure
%
% ----------------------------------------------------------------------

:- mode new_b_tree_struct(-).
new_b_tree_struct(Tree) :-
        Tree = b_tree{
                      status:open,
                      parent:null,
                      lchild:null,
                      rchild:null,
                      bfs_next:null
                     }.

% ----------------------------------------------------------------------
%
% user level predicates
%
% ----------------------------------------------------------------------

% ----------------------------------------------------------------------
% tree creation
:- export
   b_tree/2.

:- mode b_tree(++, -).
b_tree(Order, Tree) :-
        new_b_tree_struct(Tree),
        ( Order == (<) ->
            Rank = -1.0Inf
        ; Order == (>) ->
            Rank = 1.0Inf
        ),
        Tree = b_tree{
                      id:1,
                      depth:1,
                      order:Order,
                      rank:Rank
                     }.

% ----------------------------------------------------------------------
% node expansion and fathoming
:- export
   b_tree_expand/3,
   b_tree_fathom/1.

:- mode b_tree_expand(+, ++, -).
b_tree_expand(Tree, Rank, Child) :-
        Tree = b_tree{
                      id:PId,
                      depth:PDepth,
                      order:POrder,
                      status:PStatus
                     },
        Depth is PDepth+1,
        new_b_tree_struct(Child),
        Child = b_tree{
                       id:Id,
                       depth:Depth,
                       order:POrder,
                       rank:Rank
                      },
        % children in an ordered search tree must not be strictly
        % better than their parent
        \+ better_rank(Child, Tree),
        % add the child
        ( arg(lchild of b_tree, Tree, null) ->
            Id is PId << 1,
            setarg(lchild of b_tree, Tree, Child)
        ;
            arg(rchild of b_tree, Tree, null),
            Id is setbit(PId << 1, 0),
            setarg(rchild of b_tree, Tree, Child)
        ),
        setarg(parent of b_tree, Child, Tree),
        % update the status of tree
        ( PStatus == fathomed ->
            fail
        ; PStatus == expanded ->
            % we have an unfathomed subtree rooted at the left child:
            % check whether bfs_next needs updating
            next(bfs, Tree, OldNext),
            ( better_rank(Child, OldNext) ->
                % Child is better than the best in the left subtree:
                % update and recurse 
                set_bfs_next(Tree, Child),
                expand_ancestors(Tree, OldNext)
            ;
                % Child is no better than the best in the left subtree:
                % we are done
                true
            )
        ; PStatus == open ->
            % we have a new bfs_next:
            % set it, update the status to expanded, then check for
            % updating ancestors' bfs_next
            setarg(status of b_tree, Tree, expanded),
            set_bfs_next(Tree, Child),
            expand_ancestors(Tree, Tree)
        ).

:- mode set_bfs_next(+, +).
set_bfs_next(Node, BfsNext) :-
        ( arg(bfs_next of b_tree, Node, BfsNext) -> true
        ; setarg(bfs_next of b_tree, Node, BfsNext) ).

:- mode bfs_descendant(+, ?).
bfs_descendant(Node, Descendant) :-
        lchild(Node, LChild),
        ( next(bfs, LChild, LDescendant) ->
            % LChild has a bfs descendant:
            % compare with RChild's, if any
            ( rchild(Node, RChild),
              next(bfs, RChild, RDescendant),
              better_rank(RDescendant, LDescendant) ->
                % RChild's bfs descendant is Node's bfs descendant
                Descendant = RDescendant
            ;
                % LChild's bfs descendant is Node's bfs descendant
                Descendant = LDescendant
            )
        ;
            % LChild has no bfs descendant:
            % if RChild has a bfs descendant this is also the bfs
            % descendant of Node, otherwise it has none
            rchild(Node, RChild),
            next(bfs, RChild, Descendant)
        ).

better_rank(b_tree{order:Order, rank:Rank1}, b_tree{order:Order, rank:Rank2}) :-
        ( Order == (<) ->
            Rank1 @< Rank2
        ; % Order == (>)
            Rank1 @> Rank2
        ).

% When expand_ancestors(+Node, +Expanded) is called,
% Expanded is a b_tree node which has just been expanded, and
% the bfs_next field of Node has already been updated;
% we are recursively updating the bfs_next field of its ancestors if
% necessary.
:- mode expand_ancestors(+, +).
expand_ancestors(Node, Expanded) :-
        ( parent(Node, Parent), next(bfs, Parent, Expanded) ->
            % Node has a Parent whose bfs_next is Expanded:
            % update the field and recurse
            bfs_descendant(Parent, BfsNext),
            set_bfs_next(Parent, BfsNext),
            expand_ancestors(Parent, Expanded)
        ;
            % Node is either the root node and has no ancestors to
            % update, or has a Parent with better bfs_next and whose
            % ancestors must all also have better bfs_next:
            % we are done
            true
        ).

:- mode b_tree_fathom(+).
b_tree_fathom(Tree) :-
        Tree = b_tree{
                      status:Status
                     },
        ( Status == open ->
            % mark the leaf node fathomed
            setarg(status of b_tree, Tree, fathomed),
            % and recursively fathom its ancestors or update their
            % bfs_next fields
            fathom_ancestors(Tree, Tree)
        ; Status == expanded ->
            % cannot explicitly fathom an internal node
            fail
        ; % Status == fathomed
            % already fathomed, silently succeed
            true
        ).

% When fathom_ancestors(+Node, +Fathomed) is called,
% Fathomed is a b_tree leaf which has just been fathomed, and
% Node has already been updated; we are recursively updating the
% status and bfs_next fields of its ancestors if necessary.
:- mode fathom_ancestors(+, +).
fathom_ancestors(Node, Fathomed) :-
        %( parent(Node, Parent), next(bfs, Parent, Fathomed) ->
        ( parent(Node, Parent), next(bfs, Parent, Fathomed0),
          Fathomed0 = Fathomed ->
            % Node has a Parent whose bfs_next was Fathomed:
            % update the fields and recurse
            ( bfs_descendant(Parent, BfsNext) ->
                % Parent still has an open descendant:
                % leave status as expanded and update bfs_next
                set_bfs_next(Parent, BfsNext)
            ;
                % Parent has no more open descendants:
                % update status to fathomed
                setarg(status of b_tree, Parent, fathomed)
            ),
            fathom_ancestors(Parent, Fathomed)
        ;
            % Node is either the root node and has no ancestors to
            % update, or has a Parent with better bfs_next and whose
            % ancestors must all also have better bfs_next:
            % we are done
            true
        ).

% ----------------------------------------------------------------------
% tree access/update
:- export
   b_tree_get/3,
   b_tree_set/3.

:- mode b_tree_get(++, +, ?).
b_tree_get(id, b_tree{id:Id}, Val) ?- !,
        Val = Id.
b_tree_get(depth, b_tree{depth:Depth}, Val) ?- !,
        Val = Depth.
b_tree_get(order, b_tree{order:Order}, Val) ?- !,
        Val = Order.
b_tree_get(rank, b_tree{rank:Rank}, Val) ?- !,
        Val = Rank.
b_tree_get(status, b_tree{status:Status}, Val) ?- !,
        Val = Status.
b_tree_get(data, b_tree{data:Data}, Val) ?- !,
        Val = Data.

:- mode b_tree_set(++, +, ?).
b_tree_set(data, Tree, Val) ?- !,
        arg(data of b_tree, Tree, Val0),
        ( Val0 = Val -> true ; setarg(id of b_tree, Tree, Val) ).
           
% ----------------------------------------------------------------------
% testing
:- export
   is_b_tree/1,
   is_b_tree_root/1,
   is_b_tree_leaf/1.

:- mode is_b_tree(+).
is_b_tree(b_tree{}).

:- mode is_b_tree_root(+).
is_b_tree_root(b_tree{parent:null}).

:- mode is_b_tree_leaf(+).
is_b_tree_leaf(b_tree{lchild:null,rchild:null}).

% ----------------------------------------------------------------------
% tree navigation
:- export
   parent/2,
   ancestor/2,
   lchild/2,
   rchild/2,
   child/2,
   descendant/2,
   sib/2,
   next/3.

:- mode parent(+, ?).
parent(b_tree{parent:Node}, Node) :-
        ( Node == null -> fail ; true ).

:- mode ancestor(+, ?).
ancestor(Node, Ancestor) :-
        parent(Node, Parent),
        ( Ancestor = Parent ; ancestor(Parent, Ancestor) ).

:- mode lchild(+, ?).
lchild(b_tree{lchild:Node}, Node) :-
        ( Node == null -> fail ; true ).

:- mode rchild(+, ?).
rchild(b_tree{rchild:Node}, Node) :-
        ( Node == null -> fail ; true ).

:- mode child(+, ?).
child(Node, Child) :-
        lchild(Node, Child).
child(Node, Child) :-
        rchild(Node, Child).

:- mode descendant(+, ?).
descendant(Node, Descendant) :-
        child(Node, Child),
        ( Descendant = Child ; descendant(Child, Descendant) ).

:- mode sib(+, ?).
sib(Node, Sib) :-
        parent(Node, Parent),
        child(Parent, Sib),
        Sib \== Node, !.

:- mode get_bfs_next(+, ?).
get_bfs_next(b_tree{bfs_next:Node}, Node) :-
        ( Node == null -> fail ; true ).

:- mode tree_get_node(++, +, ?).
tree_get_node(1, Root, Root) :- !.
tree_get_node(Id, Root, Node) :-
        Rel is getbit(Id, 1),
        Id0 is Id >> 1,
        tree_get_node(Id0, Root, Node0),
        ( Rel == 0 ->
            lchild(Node0, Node)
        ;
            rchild(Node0, Node)
        ).

:- mode next(++, +, ?).
next(bfs, Node, Next) :-
        b_tree_get(status, Node, Status),
        ( Status == open ->
            % Node has not been expanded yet
            Next = Node
        ; Status == fathomed ->
            % subtree rooted at Node is fathomed
            fail
        ; get_bfs_next(Node, Next) ->
            % next should be the best ranked open b_tree struct in
            % the subtree rooted at Node; note that this can fail if
            % we are currently in the process of expanding a node: we
            % will have updated Status fomr open to expanded but not
            % changed the bfs_next from null when we call next/3
            % within b_tee_expand
            true
        ).
next(bfs(Id), Node, Next) ?-
        tree_get_node(Id, Node, SubTree),
        next(bfs, SubTree, Next).
next(dfs, Node, Next) :-
        b_tree_get(status, Node, Status),
        ( Status == open ->
            % Node has not been expanded yet
            Next = Node
        ; Status == fathomed ->
            % subtree rooted at Node is fathomed
            fail
        ; child(Node, Child), next(dfs, Child, Next) ->
            % next should be the first open b_tree struct in
            % the subtree rooted at Node
            true
        ).
next(dfs(Id), Node, Next) ?-
        tree_get_node(Id, Node, SubTree),
        next(dfs, SubTree, Next).

% ----------------------------------------------------------------------

:- comment(categories, ["Algorithms","Constraints"]).
:- comment(summary, "binary search tree library").
:- comment(author, "Andrew Eremin").
:- comment(copyright, "Cisco Systems, INc.").
:- comment(status, prototype).

:- comment(include, b_trees_comments).
