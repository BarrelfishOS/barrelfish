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
% Copyright (C) 1994-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH.
% 
% END LICENSE BLOCK

%
% Utility predicates to create and manipulate data
%

:- module_interface(matrix_util).

:- op(900, yfx, apply_to).

:- export
	apply/3,
	(apply_to)/2,
	apply_with_accs/4,
	apprest/2,
	boolean_list_elem/2,
	diagonal/2,
	int_list/3,
	int_list/4,
	list/3,
	list_elem/3,
	list_product/3,
	list_range/4,
	list_sum/2,
	matrix/4,
	matrix_elem/4,
	matrix_list/2,
	matrix_product/3,
	matrix_range/6,
	matrix_sorted_list/4,
	matrix_scalar_product/3,
	sumlist/5,
	transpose/2,
	upper/2.

:- begin_module(matrix_util).
:- call(lib(fd)).

:- tool(apprest/2, apprest_body/3).
:- tool((apply_to)/2, apply_to_body/3).
:- tool(apply_with_accs/4, apply_with_accs_body/5).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  List Predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% Create a list of domain variables
%
list(Size, Domain, List) :-
    N is Size,
    length(List, N),
    make_domain(List, Domain).

%
% Scalar product of two lists
%
list_product([], [], 0).
list_product([X1|L1], [X2|L2], X1*X2 + Sum) :-
    list_product(L1, L2, Sum).

%
% Get the rest of the list from the I-th element
%
list_rem(1, L, L) :- !.
list_rem(I, [_|L], Rest) :-
    I1 is I - 1,
    list_rem(I1, L, Rest).

copy_list([], L, L).
copy_list([X|T], [X|L], C) :-
    copy_list(T, L, C).

%
% Get the I-th list element
%
list_elem(1, [X|_], X) :- !.
list_elem(I, [_|List], Elem) :-
    I1 is I - 1,
    list_elem(I1, List, Elem).

%
% Get a list range
%
list_range(From, To, List, SubList) :-
    list_rem(From, List, List1),
    Length is To - From + 1,
    length(SubList, Length),
    append(SubList, _, List1).

%
% Instantiate the I-th list element to 1 and others to 0
%
boolean_list_elem(_, []).
boolean_list_elem(1, [1|L]) :-
    !,
    boolean_list_elem(0, L).
boolean_list_elem(I, [0|L]) :-
    I1 is I - 1,
    boolean_list_elem(I1, L).

%
% produce the sum of a list of domain variables
%
list_sum([], 0).
list_sum([X|L], X+S) :-
    list_sum(L, S).

%
% Produce integer lists
%
int_list(From, To, Step, []) :-
    (From > To, Step > 0
    ;
    From < To, Step < 0),
    !.
int_list(From, To, Step, [From|L]) :-
    From1 is From + Step,
    int_list(From1, To, Step, L).

int_list(From, To, List) :-
    (From < To ->
	Step = 1
    ;
	Step = -1
    ),
    int_list(From, To, Step, List).

delete_var(Var, [Var|L], R) :-
    -?->
    !,
    R = L.
delete_var(Var, [H|L], [H|T]) :-
    delete_var(Var, L, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Matrix Predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% Create a matrix of domain variables as a list of row lists
%
matrix(Height, Width, Domain, Matrix) :-
    W is Width,
    H is Height,
    length(Matrix, H),
    rows(Matrix, W, Domain).

rows([], _, _).
rows([Row|Rows], Width, Domain) :-
    list(Width, Domain, Row),
    rows(Rows, Width, Domain).

make_domain(Row, Domain) :-
    Domain = random(_, _),
    !,
    fill_row(Row, Domain).
make_domain(Row, Domain) :-
    Row :: Domain.

fill_row([], _).
fill_row([X|L], Domain) :-
    (Domain = random(Min, Max) ->
	X is Min + (random mod (Max - Min + 1))
    ;
	fail
    ),
    fill_row(L, Domain).

%
% Convert the matrix into a list
%
matrix_list(Matrix, List) :-
    matrix_list(Matrix, List, []).

matrix_list([], L, L).
matrix_list([Row|Rows], List, CL) :-
    add_row(Row, List, RC),
    matrix_list(Rows, RC, CL).

add_row([], L, L).
add_row([H|T], [H|L], CL) :-
    add_row(T, L, CL).

%
% Convert the matrix into a list sorted by the values
% of another matrix
%

matrix_sorted_list(Matrix, RefMatrix, Cmp, List) :-
    matrix_list(Matrix, MList),
    matrix_list(RefMatrix, RList),
    list_key(MList, RList, KeyList),
    sort(2, Cmp, KeyList, SortedKeyList),
    list_key(List, _, SortedKeyList).

list_key([], [], []) :- !.
list_key([H1|T1], [H2|T2], [k(H1, H2)|L]) :-
    list_key(T1, T2, L).

%
% Transpose the matrix
%
transpose([[]|_],[]) :- !.
transpose(Rows, [Col|Cols]) :-
    make_column(Rows, Col, Rests),
    transpose(Rests, Cols).

make_column([], [], []).
make_column([[H|T]|L], [H|Col], [T|LR]) :-
    make_column(L, Col, LR).

%
% Extract the diagonal
%
diagonal(Square, Diag) :-
    diagonal(Square, 1, Diag).

diagonal([], _, []).
diagonal([Row|Rows], I, [D|Diag]) :-
    list_elem(I, Row, D),
    I1 is I + 1,
    diagonal(Rows, I1, Diag).

%
% Upper part of the matrix without the diagonal
%
upper(Square, Upper) :-
    upper(Square, 2, Upper).

upper([], _, []).
upper([Row|Rows], I, U) :-
     list_rem(I, Row, Rest),
     copy_list(Rest, U, UC),
     I1 is I + 1,
     upper(Rows, I1, UC).

%
% Extract a submatrix in a given range
%
matrix_range(Row1, Row2, Col1, Col2, Matrix, SubMatrix) :-
    list_range(Row1, Row2, Matrix, Matrix1),
    apply_to(list_range(Col1, Col2), [Matrix1, SubMatrix]).

%
% Scalar multiplication of two matrices
%
matrix_scalar_product([], [], 0).
matrix_scalar_product([Row1|L1], [Row2|L2], Product + Sum) :-
    list_product(Row1, Row2, Product),
    matrix_scalar_product(L1, L2, Sum).

%
% Multiplication of two matrices
%
matrix_product(Matrix1, Matrix2, Product) :-
    transpose(Matrix2, Matrix2T),
    multiply_rows(Matrix1, Matrix2T, Product).

multiply_rows([], _, []).
multiply_rows([Row1|M1], M2T, [P1|P]) :-
    multiply_row(Row1, M2T, P1),
    multiply_rows(M1, M2T, P).

multiply_row(_, [], []).
multiply_row(Row1, [Row2|M2], [Sum|P]) :-
    list_product(Row1, Row2, Sum),
    multiply_row(Row1, M2, P).

%
% Get the (I,J)-th matrix element
%
matrix_elem(R, C, Matrix, Elem) :-
    list_elem(R, Matrix, Row),
    list_elem(C, Row, Elem).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Higher-Order Predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% Apply the predicate on each element and corresponding rest of the list
%

apprest_body(Pred, List, Module) :-
    apprest_each(Pred, List, List, Module).

apprest_each(_, [], _, _).
apprest_each(Pred, [H|T], List, Module) :-
    delete_var(H, List, RestList),
    apply(Pred, [H, RestList], Module),
    apprest_each(Pred, T, List, Module).

apply(Term, AddArgs, Module) :-
    append_args(Term, AddArgs, Goal),
    call(Goal, Module).

append_args(Term, Args, NewTerm) :-
	atom(Term),
	NewTerm =.. [Term|Args].
append_args(Term, Args, NewTerm) :-
	compound(Term),
	Term =.. OldList,
	append(OldList, Args, NewList),
	NewTerm =.. NewList.

%
% Apply the predicate on each element of the list recursively
%

apply_to_body(_, [[]|L], _) :-
    !,
    nil_list(L).
apply_to_body(Pred, Lists, Module) :-
    lists_heads(Lists, Args, Rests),
    append_args(Pred, Args, Goal),
    call(Goal, Module),
    apply_to_body(Pred, Rests, Module).

lists_heads([], [], []) :- !.
lists_heads([[H|T]|L], [H|LH], [T|LT]) :-
    lists_heads(L, LH, LT).

nil_list([]).
nil_list([[]|L]) :-
    nil_list(L).

%
% Iterate over N lists with M acumulators
%

apply_with_accs_body(_, [[]|L], Accs, Accs, _) :-
    nil_list(L),
    !.
apply_with_accs_body(Pred, Lists, AccsIn, AccsOut, Module) :-
    lists_heads(Lists, Args, RestArgs),
    length(AccsIn, N),
    length(Accs1, N),
    append_args(Pred, Args, Pred1),
    append_args(Pred1, AccsIn, Pred2),
    append_args(Pred2, Accs1, Goal),
    call(Goal, Module),
    apply_with_accs_body(Pred, RestArgs, Accs1, AccsOut, Module).
