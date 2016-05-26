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
% Options handling
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_module(grace).

:- import
	export_body/2,
	get_flag_body/4,
	is_predicate_/2
    from sepia_kernel.

% Initialization, done on first Grace invocation
init_options(Module) :-
    (exists('.gracerc') ->
    	compile('.gracerc', Module)
    ;
    exists('~/.gracerc') ->
    	compile('~/.gracerc', Module)
    ;
    	true
    ).

erase_old_arrays :-
    m_option_number(N),
    current_array(Array, L),
    functor(Array, Name, Arity),
    (Arity = 1,
    arg(1, Array, N) ->
    	true
    ;
    grace_window(Name) ->
    	true
    ;
    	valid_option(_, Name, I),
    	var(I),
    	not(matrix_option(Name, _))
    ),
    memberchk(local, L),
    erase_array(Name/Arity),
    fail.
erase_old_arrays.

% Options setting, called each time Grace is invoked
process_options :-
    option(varstack, font, F),
    tcl('set vs_font ##', F),
    option(control, font, CF),
    tcl('set ct_font ##', CF),
    option(menu, font, MF),
    tcl('set menu_font ##', MF),
    (option(elements, font, EF) ->
	tcl('set elc_font ##', EF)
    ;
	tcl('set elc_font [m_make_font 18]')
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Getting the options
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Short form, if we know that the array already exists (not the first call)
matrix_option(W, N, V) :-
    matrix_option(N, I),
    AI =.. [W, I],
    getval(AI, V).

% General form, may be slow
option(W, N, V) :-
    matrix_option(N, I),
    !,
    m_option_number(Arity),
    AD =.. [W, Arity],
    (current_array(AD, _) ->
	true
    ;
	make_local_array(AD),
	copy_matrix_defaults(W)
    ),
    AI =.. [W, I],
    getval(AI, Term),
    option_value(Term, W, N, V).
option(W, N, V) :-
    valid_option(W, N, I),
    (integer(I) ->
	AI =.. [W, I],
	getval(AI, Term)
    ;
	getval(N, Term)
    ),
    option_value(Term, W, N, V).

option_value(Term, W, N, V) :-
    (Term = (W, N, G) ->
	(G = Module:Goal ->
	    true
	;
	    getval(module, Module),
	    Goal = G
	),
	apply(Goal, [V], Module)
    ;
	Term = V
    ).

% A fast one which works only for local properties
single_option(_W, N, V) :-
    getval(N, V).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Setting the options
%
%
% matrix: 	MatrixName(Index)
% font etc.:	Window(Index)
% else:		OptionName
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
grace_option(W, N, V) :-
    var(V),
    !,
    option(W, N, V).
grace_option(W, N, V) :-
    nonvar(V),
    matrix_option(N, I),
    !,
    (var(W) ->
	AI = '.default'(I)
    ;
	m_option_number(Arity),
	AD =.. [W, Ar],
	(current_array(AD, _) ->
	    (Ar = Arity ->
		AI =.. [W, I]
	    ;
		error(6, grace_option(W, N, V))
	    )
	;
	atom(W) ->
	    Ar = Arity,
	    make_local_array(AD),
	    copy_matrix_defaults(W),
	    AI =.. [W, I]
	;
	    error(5, grace_option(W, N, V))
	)
    ),
    type_check(W, N, V, V1),
    action(W, N, V1, NewV),
    setval(AI, NewV).
grace_option(W, N, V) :-
    valid_option(W, N, I),
    not(read_only(W, N)),
    !,
    type_check(W, N, V, V1),
    action(W, N, V1, NewV),
    (integer(I) ->
	% multi-window options
	AI =.. [W, I],
	setval(AI, NewV)
    ;
	setval(N, NewV),
	tcl_string(NewV, S),
	(tcl('set cv_## ##', [N, S]) -> true; true)
    ).
grace_option(W, N, V) :-
    error(6, grace_option(W, N, V)).

copy_matrix_defaults(M) :-
    m_option_number(Max),
    copy_matrix_defaults(M, 0, Max).

copy_matrix_defaults(_, Max, Max) :- !.
copy_matrix_defaults(W, I, Max) :-
    AI =.. [W, I],
    getval('.default'(I), V),
    setval(AI, V),
    I1 is I + 1,
    copy_matrix_defaults(W, I1, Max).

:- mode matrix_option(+, ?).
matrix_option(label_x, 0).
matrix_option(label_y, 1).
matrix_option(font_size, 2).
matrix_option(show, 3).
matrix_option(diagonal_color, 4).
matrix_option(lookahead, 5).
matrix_option(lookahead_var, 6).
matrix_option(element_width, 7).
matrix_option(matrix_geometry, 8).
matrix_option(font_sizes, 9).
matrix_option(label, 10).
matrix_option(font(_), 11).
m_option_number(12).

% Rudimentary type test, should be improved
type_check(W, N, V, _) :-
    var(V),
    !,
    error(4, grace_option(W, N, V)).
type_check(W, N, V, (W, N, V)) :-
    compound(V),
    V \= [_|_],
    V \= +[_|_],
    !.			% callable terms are executed on demand
type_check(W, N, V, V) :-
    list_option(W, N),
    !,
    (V = [_|_] ->
	true
    ;
	V = +[_|_]
    ).
type_check(W, N, V, NewV) :-
    boolean_option(W, N),
    !,
    (true(V) ->
	NewV = 1
    ;
    false(V) ->
	NewV = 0
    ;
	error(6, grace_option(W, N, V))
    ).
type_check(W, N, V, V) :-
    integer_option(W, N),
    !,
    (integer(V) ->
	true
    ;
	error(5, grace_option(W, N, V))
    ).
type_check(W, N, V, NewV) :-
    string_arg_option(W, N),
    !,
    (V == "" ->
	NewV = "{}"
    ;
    string(V) ->
	NewV = V
    ;
    atom(V) ->
	atom_string(V, NewV)
    ;
	error(5, grace_option(W, N, V))
    ).
type_check(W, N, V, NewV) :-
    string_option(W, N),
    !,
    (string(V) ->
	NewV = V
    ;
    atom(V) ->
	atom_string(V, NewV)
    ;
	error(5, grace_option(W, N, V))
    ).
type_check(W, N, V, NewV) :-
    enumerated_option(W, N, L),
    !,
    (string(V) ->
    	NewV = V
    ;
    atom(V) ->
    	atom_string(V, NewV)
    ;
	error(5, grace_option(W, N, V))
    ),
    (member(NewV, L) ->
    	true
    ;
	error(6, grace_option(W, N, V))
    ).
type_check(W, N, V, _) :-
    error(6, grace_option(W, N, V)).

valid_option(W, N, Index) :-
    common_option(W, N, Index),
    !.
valid_option(W, N, _) :-
    compound_option(W, N),
    !.
valid_option(W, N, _) :-
    list_option(W, N),
    !.
valid_option(W, N, _) :-
    boolean_option(W, N),
    !.
valid_option(W, N, _) :-
    string_arg_option(W, N),
    !.
valid_option(W, N, _) :-
    string_option(W, N),
    !.
valid_option(W, N, _) :-
    integer_option(W, N),
    !.
valid_option(W, N, _) :-
    enumerated_option(W, N, _),
    !.

common_option(W, font, 0) :-
    grace_window(W),
    W \== matrix.
common_option(W, geometry, 1) :-
    grace_window(W),
    W \== elements,
    W \== menu.

grace_window(control).
grace_window(varstack).
grace_window(elements).
grace_window(matrix).
grace_window(menu).
grace_window(constraints).

boolean_option(_, show).
boolean_option(_, label).
boolean_option(_, lookahead).
boolean_option(_, lookahead_var).
boolean_option(control, all_solutions).
boolean_option(control, display_solutions).
boolean_option(control, print_trace).
boolean_option(varstack, flush).

string_arg_option(control, title).
string_arg_option(control, version).
string_arg_option(control, display).
string_arg_option(control, var_selection).
string_arg_option(control, value_selection).
string_arg_option(varstack, empty_color).
string_arg_option(varstack, rest_color).
string_arg_option(varstack, tried_color).
string_arg_option(varstack, current_color).
string_arg_option(varstack, partly_color).
string_arg_option(matrix, selected_forward).
string_arg_option(matrix, selected_backward).
string_arg_option(matrix, top).
string_arg_option(_, geometry).
string_arg_option(_, matrix_geometry).
string_arg_option(_, diagonal_color).
string_arg_option(W, font) :-
    grace_window(W),
    W \== matrix.
string_arg_option(W, geometry) :-
    grace_window(W),
    W \== elements,
    W \== menu.

string_option(tk, init).

integer_option(varstack, rows).
integer_option(varstack, box_width).
integer_option(varstack, text_width).
integer_option(control, percent).
integer_option(_, font_size).
integer_option(_, element_width).

compound_option(_, font(_)).

list_option(control, var_selections).
list_option(control, value_selections).
list_option(_, label_x).
list_option(_, label_y).
list_option(_, font_sizes).

enumerated_option(control, branch_and_bound, ["restart", "continue"]).
enumerated_option(control, display, ["all", "stack", "none"]).
enumerated_option(control, restart, ["ask", "restart"]).

read_only(control, title) :- getval(startup, 0).
read_only(control, version) :- getval(startup, 0).

% Options that require some other action than just setting a variable
action(control, var_selection, S, S) :-
    !,
    (set_var_selection(S) ->
	true
    ;
	error(6, grace_option(control, var_selection, S))
    ).
action(control, value_selection, S, S) :-
    !,
    (set_value_selection(S) ->
	true
    ;
	error(6, grace_option(control, value_selection, S))
    ).
action(control, value_selections, Name, Value) :-
    !,
    (Name = +List,
    convert_pred_list(List, NewList) ->
	option(control, value_selections, OldList),
	subtract(NewList, OldList, ReallyNew),
	add_value_selections(ReallyNew),
	append(OldList, ReallyNew, Value)
    ;
    Name = [_|_],
    convert_pred_list(Name, Value) ->
	(tcl('catch {.valsel.menu delete 0 last}') -> true; true),
	add_value_selections(Value)
    ;
	error(5, grace_option(control, value_selections, Name))
    ).
action(control, var_selections, Name, Value) :-
    !,
    (Name = +List,
    convert_pred_list(List, NewList) ->
	option(control, var_selections, OldList),
	subtract(NewList, OldList, ReallyNew),
	add_var_selections(ReallyNew),
	append(OldList, ReallyNew, Value)
    ;
    Name = [_|_],
    convert_pred_list(Name, Value) ->
	(tcl('catch {.varsel.menu delete 0 last}') -> true; true),
	add_var_selections(Value)
    ;
	error(5, grace_option(control, var_selections, Name))
    ).
action(Matrix, label, Bool, Bool) :-
    !,
    (var(Matrix) ->
	true		% could also iterate on all existing matrices
    ;
	tcl('catch {m_set_label .## ## ##}', [Matrix, Matrix, Bool])
    ).
action(_, _, V, V).


convert_pred_list([], []).
convert_pred_list([[P, N]|L], [[P, S]|L1]) :-
    (string(N) ->
	S = N
    ;
    atom(N) ->
	atom_string(N, S)
    ),
    convert_pred_list(L, L1).

true(1).
true(yes).
true(on).
true(true).

false(0).
false(no).
false(off).
false(false).

default_font(Size, Font) :-
    concat_string(['-*-times-bold-r-normal-*-', Size, '-*-*-*-*-*-*-*'], Font).

init_arrays :-
    % Initialize the matrix default array
    m_option_number(N), make_local_array('.default'(N)),
    % Initialize the multiple-window arrays
    (grace_window(W), AI =.. [W, 2], make_local_array(AI), fail; true),
    % Initialize the single-window arrays
    (valid_option(_, O, I), var(I), make_local_array(O), fail; true).
%
% Default option values
%
default_options(Title) :-
    erase_old_arrays,
    init_arrays,
    setval(startup, 1),
    grace_option(_, label_x, int_list(0, 9)),
    grace_option(_, label_y, int_list(0, 30)),
    grace_option(_, label, yes),
    grace_option(_, font_size, 12),
    grace_option(_, show, 1),
    grace_option(_, diagonal_color, "#ffc0c0"),
    grace_option(_, element_width, 9),
    grace_option(_, lookahead, 0),
    grace_option(_, lookahead_var, 0),
    grace_option(_, font_sizes, [6, 8, 10, 12, 14, 18, 22, 24]),
    grace_option(_, matrix_geometry, ""),
    grace_option(control, geometry, ""),
    grace_option(control, var_selections,
	[[first_in_list/2, "List Order"],
	[smallest_domain/2, "Smallest Domain"],
	[largest_domain/2, "Largest Domain"],
	[smallest_minimum/2, "Smallest Minimum"],
	[largest_minimum/2, "Largest Minimum"],
	[smallest_maximum/2, "Smallest Maximum"],
	[largest_maximum/2, "Largest Maximum"],
	[smallest_difference/2, "Smallest Difference"],
	[largest_difference/2, "Largest Difference"],
	[least_regret/2, "Least Regret"],
	[most_constrained/2, "Most Constrained"]]),
    grace_option(control, value_selections,
	[[smallest_element/3, "Smallest Element"],
	[largest_element/3, "Largest Element"],
	[random_element/3, "Random Element"],
	[halve_range_bottom/3, "Halve Range Bottom"],
	[halve_range_top/3, "Halve Range Top"],
	[halve_elements_bottom/3, "Halve Elements Bottom"],
	[halve_elements_top/3, "Halve Elements Top"]]),
    grace_option(control, title, Title),
    grace_option(control, version, "1.0"),
    grace_option(control, var_selection, "Smallest Domain"),
    grace_option(control, value_selection, "Smallest Element"),
    grace_option(control, font, "-*-helvetica-bold-r-normal-*-12-*"),
    grace_option(control, display, "Stack"),
    grace_option(control, print_trace, 0),
    grace_option(control, all_solutions, 0),
    grace_option(control, display_solutions, 1),
    grace_option(control, percent, 0),
    grace_option(control, branch_and_bound, "restart"),
    grace_option(control, restart, restart),
    grace_option(varstack, geometry, "-5+0"),
    grace_option(varstack, flush, 1),
    grace_option(tk, init, ""),
    grace_option(varstack, box_width, 80),
    grace_option(varstack, font, grace:default_font(14)),
    grace_option(varstack, rows, 30),
    grace_option(varstack, text_width, 110),
    grace_option(varstack, empty_color, white),
    grace_option(varstack, rest_color, steelblue2),
    grace_option(varstack, tried_color, gray80),
    grace_option(varstack, current_color, red),
    grace_option(varstack, partly_color, "#ff8500"),
    grace_option(menu, font, grace:default_font(14)),
    grace_option(_, font(Size), grace:default_font(Size)),
    grace_option(matrix, selected_forward, red),
    grace_option(matrix, selected_backward, red),
    grace_option(matrix, top, "+0+0"),
    grace_option(constraints, geometry, "+300+300"),
    grace_option(elements, font, grace:default_font(14)),
    setval(startup, 0).

:- init_arrays.
