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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Matrix handling
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- begin_module(grace).

:- export
	grace_matrix/2.

:- call(lib(fd)).

:- pragma(expand).

:- tool(grace_start/1).

:- tool(grace_add_matrix/1, grace_add_matrix_body/2).
:- tool(grace_matrix/2, grace_matrix_body/3).

grace_add_matrix_body([Matrix, Name|_], Module) :-
    grace_matrix_body(Matrix, Name, Module).

grace_matrix_body(Matrix, Name, Module) :-
/*
    my_copy_matrix(Matrix,NewMatrix),
    gmb(NewMatrix,Name,Module).

my_copy_matrix(Atom,Atom) :- atomic(Atom), !.
my_copy_matrix(Var,Var) :-
    is_integer_domain(Var), !.
my_copy_matrix(Var,NewVar) :-
    is_domain(Var), !,
    dom(Var,Dom),
    length(Dom,Length),
    NewVar::1..Length,
    element(NewVar,Dom,Var).
my_copy_matrix(Var,Var) :-
    var(Var), !.
my_copy_matrix(Term,NewTerm) :-
    Term=..[F|Args],
    my_copy_matrix(Args,NewArgs),
    NewTerm=..[F|NewArgs].

gmb(Matrix,Name,Module) :-
*/
    get_data(Stored),
    (compound(Stored) ->
    	true
    ;
    	grace_start_body(Name, Module)
    ),
    check_matrix(Matrix, Name, NewMatrix, NewName),
    (nonground(Matrix) ->
	true
    ;
	grace_option(NewName, label, 0)
    ),
    (getval(grace, on) ->
	(make_matrix(NewMatrix, NewName) ->
	    true
	;
	    error(1, grace_matrix(Matrix, Name))
	),
	apply_matrix(NewMatrix, NewName, attributes),
	apply_matrix(NewMatrix, NewName, display_delay)
    ;
	option(NewName, show, _)	% to enforce the global array
    ),
    store_matrix([NewMatrix, NewName]).

% Make sure that the matrices are in a correct format. If a matrix
% is just a list of variables, convert it to rows of 10 elements.
% If it is a structure with variable arguments, consider it one row.
check_matrix(Matrix, Name, NewMatrix, NewName) :-
    (var(Name) ->
	error(4, grace_matrix(Matrix, Name))
    ;
    atom(Name) ->
	Name1 = Name
    ;
    string(Name) ->
	atom_string(Name1, Name)
    ;
	error(5, grace_matrix(Matrix, Name))
    ),
    (Name1 = matrix ->
	NewName = ' matrix'
    ;
	NewName = Name1
    ),
    (compound(Matrix) ->
	(Matrix = [Row|_] ->
	    ((var(Row); atomic(Row)) ->
		% A simple list; split if X names allow it
		option(Name, label_x, XL),
		length(XL, Split),
		split_list(Matrix, Split, NewMatrix)
	    ;
		compound(Row),
		NewMatrix = Matrix
	    )
	;
	    arg(1, Matrix, Row),
	    (compound(Row) ->
		NewMatrix = Matrix
	    ;
		NewMatrix = f(Matrix)
	    )
	)
    ;
    var(Matrix) ->
	(is_domain(Matrix) ->
	    check_matrix([Matrix], Name, NewMatrix, NewName)
	;
	    error(4, grace_matrix(Matrix, Name))
	)
    ;
    integer(Matrix) ->
	check_matrix([Matrix], Name, NewMatrix, NewName)
    ;
	error(5, grace_matrix(Matrix, Name))
    ).

make_matrix(Matrix, Name) :-
    matrix_sizes(Matrix, SX, SY),
    matrix_labels(Name, [], SX, SY, LabX, LabY),
    option(Name, show, Vis),
    option(Name, font_size, FS),
    option(Name, diagonal_color, MC),
    option(Name, element_width, MW),
    option(Name, font_sizes, ML),
    option(Name, matrix_geometry, MG),
    tcl_string(ML, MS),
    option(Name, label, MLab),
    tcl('create_matrix ########## ## ## ## ## ## ## ##',
	[Name, LabX, SX, LabY, SY, Vis, FS, MC, MW, MS, MLab, MG]).

matrix_labels(Name, [], SX, SY, LabX, LabY) :-
    matrix_label(Name, label_x, SX, LabX),
    matrix_label(Name, label_y, SY, LabY).

/*
matrix_label_x(Name, [], SX, Lab) :-
    !,
    option(Name, label_x, List),
    (length(List, SX) ->
	list_to_tcl(List, Lab)
    ;
	To is SX - 1,
	make_list(0, To, ListX),
	list_to_tcl(ListX, Lab)
    ).
matrix_label_x(_, LabList, _, Lab) :-
    matrix_labels(LabList, L),
    list_to_tcl(L, Lab).
*/

matrix_label(Name, Pos, S, Lab) :-
    option(Name, Pos, List1),
    (length(List1, S) ->
	list_to_tcl(List1, Lab)
    ;
	% get the prefix of right length
	length(List, S),
	(append(List, _, List1) ->
	    true
	;
	    List1 = [From|_],
	    To is S + From - 1,
	    int_list(From, To, 1, List)
	),
	grace_option(Name, Pos, List),
	list_to_tcl(List, Lab)
    ).

matrix_label_y(Name, [], S, Lab) :-
    option(Name, label_y, List1),
    % get the prefix of right length
    length(List, S),
    (append(List, _, List1) ->
	true
    ;
	List1 = [From|_],
	To is S - From + 1,
	int_list(From, To, 1, List)
    ),
    list_to_tcl(List, Lab).

matrix_columns([Row|_], Col) :-
    !,
    length(Row, Col).
matrix_columns(M, Col) :-
    arg(1, M, Row),
    functor(Row, _, Col).

display_all_matrices :-
    get_matrices(M),
    display_matrices(M, display_direct).

print_all_matrices :-
    get_matrices(M),
    open('/tmp/grace', write, grace),
    printf(grace, ".pl 17cm\n.po -2cm\n.vs 8\n\n", []),
    print_matrices(M),
    nl(grace),
    close(grace),
    concat_strings("groff -t -P-l -l ", "/tmp/grace", Exec),
    exec(Exec, []),
    delete('/tmp/grace'),
    message("Matrices printed").

print_matrices([]).
print_matrices([Matrix|L]) :-
    Matrix = [M, Name|_],
    printf(grace, "\n\n%w:\n.TS\ntab (/) ;\n", [Name]),
    matrix_columns(M, Size),
    getval(print_size, FontSize),
    print_columns(Size, FontSize),
    printf(grace, ".\n", []),
    apply_matrix(M, Name, print),
    printf(grace, "\n.TE\n", []),
    print_matrices(L).

print_columns(0, _).
print_columns(I, FontSize) :-
    integer(I),
    I > 0,
    printf(grace, "lp%df(HNR) ", [FontSize]),
    I1 is I - 1,
    print_columns(I1, FontSize).

display_matrices([], _).
display_matrices([[M,Name]|L], T) :-
    apply_matrix(M, Name, T),
    display_matrices(L, T).

apply_matrix(L, Name, T) :-
    compound(L),
    L = [_|_],
    !,
    apply_matrix_list(L, Name, 0, T).
apply_matrix(L, Name, T) :-
    compound(L),
    functor(L, _, M),
    apply_matrix_compnd(L, Name, 0, M, T).

apply_row(Row, Name, I, T) :-
    (var(Row); atomic(Row)),
    !,
    apply_row_list([Row], Name, I, 0, T).
apply_row(Row, Name, I, T) :-
    Row = [_|_],
    !,
    apply_row_list(Row, Name, I, 0, T).
apply_row(Row, Name, I, T) :-
    functor(Row, _, N),
    apply_row_compnd(Row, Name, I, N, 0, T).

matrix_element(N, I, J, El) :-
    matrix(N, Sq),
    matrix_element_cont(Sq, I, J, El).

matrix_element_cont(Sq, I, J, El) :-
    (Sq = [_|_] ->
	matrix_element_list(Sq, I, J, El)
    ;
	matrix_element_compnd(Sq, I, J, El)
    ).

matrix_sizes(Matrix, SX, SY) :-
    compound(Matrix),
    Matrix = [Row|_],
    !,
    row_size(Row, SX),
    length(Matrix, SY).
matrix_sizes(Matrix, SX, SY) :-
    compound(Matrix),
    functor(Matrix, _, SY),
    arg(1, Matrix, Row),
    row_size(Row, SX).

row_size(Row, SX) :-
    compound(Row),
    (length(Row, SX) -> true; functor(Row, _, SX)).

var_position(Var, L, I, J) :-
    compound(L),
    L = [_|_],
    !,
    var_position_list(Var, L, I, J, 0).
var_position(Var, Sq, I, J) :-
    compound(Sq),
    functor(Sq, _, M),
    var_position_compnd(Var, Sq, I, J, 0, M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Matrix is a list of row lists
%
apply_matrix_list([], _, _, _).
apply_matrix_list([H|L], N, I, T) :-
    -?->
    apply_row(H, N, I, T),
    I1 is I + 1,
    apply_matrix_list(L, N, I1, T).

apply_row_list([], _, _, _, _).
apply_row_list([H|L], N, I, J, T) :-
    -?->
    apply_element(H, N, I, J, T),
    J1 is J + 1,
    apply_row_list(L, N, I, J1, T).

matrix_element_list(Sq, I, J, El) :-
    list_arg(I, Sq, Row),
    list_arg(J, Row, El).

list_arg(0, [H|_], H).
list_arg(N, [_|T], El) :-
    integer(N),
    N>0,
    N1 is N - 1,
    list_arg(N1, T, El).

var_position_list(Var, [Row|R], IR, JR, I) :-
    (var_position_row_list(Var, Row, 0, JR) ->
	IR = I
    ;
	I1 is I + 1,
	var_position_list(Var, R, IR, JR, I1)
    ).

var_position_row_list(Var, [El|L], J, JR) :-
    (El == Var ->
	J = JR
    ;
	J1 is J + 1,
	var_position_row_list(Var, L, J1, JR)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Matrix is a structure
%
apply_matrix_compnd(Sq, N, I, M, T) :-
    I < M,
    !,
    I1 is I + 1,
    arg(I1, Sq, Row),
    apply_row(Row, N, I1, T),
    apply_matrix_compnd(Sq, N, I1, M, T).
apply_matrix_compnd(_, _, _, _, _) :- !.

apply_row_compnd(Row, N, I, M, J, T) :-
    J < M,
    !,
    J1 is J + 1,
    arg(J1, Row, El),
    apply_element(El, N, I, J1, T),
    apply_row_compnd(Row, N, I, M, J1, T).
apply_row_compnd(_, _, _, _, _, _) :- !.

matrix_element_compnd(Sq, I, J, El) :-
    I1 is I + 1,
    J1 is J + 1,
    arg(I1, Sq, Row),
    arg(J1, Row, El).

var_position_compnd(Var, Sq, IR, JR, I, M) :-
    I1 is I + 1,
    arg(I1, Sq, Row),
    (var_position_row_compnd(Var, Row, 0, M, JR) ->
	IR = I
    ;
	I1 < M,
	var_position_compnd(Var, Sq, IR, JR, I1, M)
    ).

var_position_row_compnd(Var, Row, J, M, JR) :-
    J1 is J + 1,
    arg(J1, Row, El),
    (El == Var ->
	J = JR
    ;
	J1 < M,
	var_position_row_compnd(Var, Row, J1, M, JR)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

apply_element(H, N, _, _, _) :-
    compound(H),
    !,
    error(5, grace_add_matrix(N)).
apply_element(H, N, I, J, display_direct) :-
    el_to_const(H, S, _),
    display_element(N, I, J, S).
apply_element(El, N, I, J, display_delay) :-
    el_window(N, I, J, W),
    print_handler(El, W, matrix_prio).
apply_element(El, N, I, J, stepd) :-
    el_window(N, I, J, W),
    stepd_handler(El, W).
apply_element(El, _, _, J, print) :-
    (J = 0 ->
	nl(grace)
    ;
	true
    ),
    el_to_const(El, S, _),
    name(S, L),
    selectlist(\==(0' ), L, LS),
    string_list(SS, LS),
    printf(grace, "%s/", SS).
apply_element(Var, N, I, J, attributes) :-
    el_window(N, I, J, Id),
    add_attributes(Var, N, Id).

display_element(N, I, J, S) :-
    tcl_eval(['.', N, '.', I, '.', J, ' configure -text "', S, '"']).

compare_elems(El1, El2, I, J) :-
    const_to_el(El1, L1),
    const_to_el(El2, L2),
    (getval(selected, [123, I, J]) ->
	true
    ;
    (subset(L1, L2); L1 = [_], L2 = [_,_|_]) ->		% this smaller
	background(123, I, J, '#fff8d6')	
    ;
    (subset(L2, L1); L2 = [_], L1 = [_,_|_]) ->		% this larger
	background(123, I, J, '#e0cdad')
    ;							% different
	background(123, I, J, '#ffff80')
    ).

matrix(N, Sq) :-
    (string(N) -> atom_string(Type, N); Type = N),
    get_matrices(Matrices),
    memberchk([Sq, Type|_], Matrices).

print_handler(Var, W, Prio) :-
    el_to_const(Var, D, _),
    print_delay(Var, D, W, Prio).

print_delay(Var, _, W, Prio) :-
    el_to_const(Var, D, _),
    tcl_eval(['el_display_domain ', W, ' {', D, '}']),
    (var(Var) ->
	make_suspension(print_delay(Var, D, W, Prio), Prio, Susp),
	insert_suspension(Var, Susp, of(constrained, suspend), suspend)
    ;
	true
    ).
print_delay(_, Old, W, _) :-
    tcl_eval(['el_display_domain ', W, ' {', Old, '}']),
    fail.

active_matrices(List) :-
    tcl_eval(active_matrices, AL),
    (AL = "" -> L = []; L = AL),
    tcl_matrices(L, List).

tcl_matrices([], []).
tcl_matrices([S|L], [Sq|T]) :-
    !,
    matrix(S, Sq),
    tcl_matrices(L, T).
tcl_matrices(S, [Sq]) :-
    matrix(S, Sq).

% Return the position of the variable in a matrix
%find_variable(Var{grace with id:Id}, N, I, J) :-
find_variable(Var, N, I, J) :-
    get_matrices(Matrices),
    member([Sq, N|_], Matrices),
    var_position(Var, Sq, I, J),
    !.

reset_matrices :-
    get_matrices(Matrices),
    reset_matrices(Matrices).

reset_matrices([]).
reset_matrices([[M, Name|_]|Matrices]) :-
    reset_matrix(M, Name, 1),
    reset_matrices(Matrices).

reset_matrix([], _, _).
reset_matrix([Row|Rows], N, I) :-
    reset_row(Row, N, I, 1),
    I1 is I + 1,
    reset_matrix(Rows, N, I1).

reset_row([], _, _, _).
reset_row([V|L], N, I, J) :-
    reset_var(V, N, I, J),
    J1 is J + 1,
    reset_row(L, N, I, J1).

el_window(N, I, J, W) :-
    concat_string([., N, ., I, ., J], W).

el_label(N, I, J, Lab) :-
    tcl('m_el_label ## ## ##', [N, I, J], string(Lab)).

matrix_labels(between(From, To, Step), List) :-
    make_list(From, To, Step, List).
matrix_labels(between(From, To), List) :-
    (From < To ->
	Step = 1
    ;
	Step = -1
    ),
    make_list(From, To, Step, List).
matrix_labels(Value, List) :-
    Value = [_|_],
    List = Value.

labeled_matrices(Matrices) :-
    get_matrices(All),
    labeled_matrices(All, Matrices).

labeled_matrices([], []).
labeled_matrices([[M, Name|_]|Matrices], LM) :-
    (option(Name, label, 1) ->
	LM = [[M, Name]|L]
    ;
	LM = L
    ),
    labeled_matrices(Matrices, L).

m_make_font(Matrix, Size, Font) :-
    option(Matrix, font(Size), Font).


