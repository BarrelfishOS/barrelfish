%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Lightweight Dynamic Symmetry Breaking
%
% Implemented by Chris Mears, 2009.
%
%
% Copyright 2010 Chris Mears. All rights reserved.
%
% Redistribution and use in source and binary forms, with or without modification, are
% permitted provided that the following conditions are met:
%
%    1. Redistributions of source code must retain the above copyright notice, this list of
%       conditions and the following disclaimer.
%
%    2. Redistributions in binary form must reproduce the above copyright notice, this list
%       of conditions and the following disclaimer in the documentation and/or other materials
%       provided with the distribution.
%
% THIS SOFTWARE IS PROVIDED BY CHRIS MEARS ``AS IS'' AND ANY EXPRESS OR IMPLIED
% WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
% FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL CHRIS MEARS OR
% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%
% The views and conclusions contained in the software and documentation are those of the
% authors and should not be interpreted as representing official policies, either expressed
% or implied, of Chris Mears.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(ldsb).

:- comment(categories, ["Constraints","Techniques"]).
:- comment(summary, "Lightweight dynamic symmetry breaking for finite domains.").
:- comment(author, "Chris Mears").

:- comment(desc, html("</p>

   The LDSB (lightweight dynamic symmetry breaking) library adds
   symmetry breaking to search.  Its aim is to provide easy-to-use
   symmetry breaking to their finite domain constraint models.  The
   method is described in \"<i>Lightweight Dynamic Symmetry
   Breaking</i>. C. Mears, M. Garcia de la Banda, B. Demoen,
   M. Wallace.  <i>SymCon'08</i>\".
                     
   <p/>

   To use LDSB, first call ldsb_initialise/2 with the search variables
   for which you want to use symmetry breaking and a specification of
   the symmetries.  LDSB supports binary search branching of the form
   <tt>Var = Val</tt> and <tt>Var /= Val</tt>, or <tt>Val in Var</tt>
   and <tt>Val notin Var</tt> for sets.  Use ldsb_indomain/1 and
   ldsb_indomain_set/1 to instantiate variables during search, or
   ldsb_try/3 and ldsb_try_set/3 for a custom branching.")).

:- comment(status, "evolving").

:- lib(ic).
:- lib(ic_sets).

:- lib(listut).
:- lib(ordset).

:- lib(test_util).

:- import append/3 from eclipse_language.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Export list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- export
        ldsb_initialise/2,
        ldsb_try/3,
        ldsb_try_set/3,

        ldsb_indomain/1,
        ldsb_indomain_set/1,

        run_tests/0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Attributes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- meta_attribute(ldsb, [print:print_ldsb/2]).

:- local struct(ldsb_shared(matrix,
                            global_symmetries)).

:- local struct(ldsb_local(idx,
                           local_symmetries,
                           shared)).

print_ldsb(_{ldsb:Attr}, String) :- -?->
        Attr = ldsb_local{idx:Idx},
        String = ldsb(Idx).

get_index(_{ldsb:(ldsb_local{idx:Idx0})}, Idx) :- -?->
	Idx = Idx0.

get_symmetries(_{ldsb:(ldsb_local{local_symmetries:Syms})}, S) :- -?->
        S = Syms.

get_shared(_{ldsb:(ldsb_local{shared:Shared})}, S) :- -?->
        S = Shared.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Symmetry processing.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Primitive symmetries.
process_sym(_, variable_interchange(X), variable_interchange(X)).
process_sym(_, parallel_variable_interchange(X), parallel_variable_interchange(X)).
process_sym(_, value_interchange(X), value_interchange(X)).
process_sym(_, parallel_value_interchange(X), parallel_value_interchange(X)).
% All variables interchangeable.
process_sym(Xs, variables_interchange, variable_interchange(L)) :-
        collection_to_list(Xs, L).
% All values interchangeable.  Determines the minimum and maximum
% value by looking at the first variable.  Works for set variables
% too.
process_sym(Xs, values_interchange, value_interchange(L)) :-
        term_variables(Xs, [X|_]),
        ( ic_sets : is_solver_type(X) ->
            potential_members(X, PM),
            eclipse_language:min(PM, Min), eclipse_language:max(PM, Max)
        ; get_min(X, Min), get_max(X, Max) ),
        ( for(I,Min,Max), foreach(I, L) do true ).
% Rows interchangeable.
process_sym(Xs, rows_interchange, parallel_variable_interchange(L)) :-
        seqs_to_lists(Xs, L).
% Columns interchangeable.
process_sym(Xs, columns_interchange, parallel_variable_interchange(L)) :-
        m_transpose(Xs, Transpose),
        seqs_to_lists(Transpose, L).
% Reflection around main diagonal.
process_sym(Xs, diagonal_reflection, parallel_variable_interchange(L)) :-
        dim(Xs, [N,N]),
        ( ((for(_I,2,N)) >>
           (for(J,1,_I-1), param(_I))),
          foreach(Pair, Pairs) do
            Pair = [ [_I,J], [J,_I] ]
        ),
        length(Pairs, SeqLength),
        dim(Seqs, [2,SeqLength]),
        ( foreach([I1,I2], Pairs),
          param(Xs,Seqs),
          count(I,1,_) do
            arg(I1,Xs,V1),
            arg(I2,Xs,V2),
            arg([1,I], Seqs, V1),
            arg([2,I], Seqs, V2)
        ),
        seqs_to_lists(Seqs, L).
% Reflection around horizontal axis.
process_sym(Xs, row_reflection, parallel_variable_interchange([G1,G2])) :-
        dim(Xs, [M,N]),
        div(M, 2, Lim),
        ( ((for(_I,1,Lim), param(N)) >>
           (for(J,1,N), param(_I))),
          foreach(V1, G1),
          foreach(V2, G2),
          param(Xs,M) do
            E1 = [_I,J],
            I2 is M - _I + 1,
            E2 = [I2,J],
            arg(E1, Xs, V1),
            arg(E2, Xs, V2)
        ).
% Reflection around vertical axis.
process_sym(Xs, column_reflection, S) :-
        m_transpose(Xs, Transpose),
        process_sym(Transpose, row_reflection, S).
% Value reflection.
process_sym(Xs, value_reflection, S) :-
        term_variables(Xs, [X|_]),
        get_min(X, Min), get_max(X, Max),
        process_sym(Xs, value_reflection(Min,Max), S).
% Value reflection with explicit bounds.
process_sym(_Xs, value_reflection(Lower, Upper), parallel_value_interchange(Ls)) :-
        N is Upper - Lower + 1,
        N2 is N // 2,
        dim(Seqs, [2, N2]),
        ( for(I,1,N2),
          param(Seqs,Lower,Upper) do
            Val1 is Lower+(I-1),
            Val2 is Upper-(I-1),
            arg([1,I], Seqs, Val1),
            arg([2,I], Seqs, Val2)
        ),
        seqs_to_lists(Seqs, Ls).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Variable initialisation.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- comment(ldsb_initialise/2,
           [ amode: (ldsb_initialise(+, +) is det),
             args: [ "Xs": "Array of search variables",
                     "Syms": "List of symmetries" ],
             summary: "Initialise LDSB search variables.",
             resat: no,
             see_also: [ldsb_try/3, ldsb_indomain/1, ldsb_indomain_set/1],

             desc: html(" <p/> Initialise an array of search variables
to use LDSB with the given symmetries.  A variables must be
initialised with ldsb_initialise before it can be used with ldsb_try
or any predicate that relies on it such as ldsb_indomain.

</p> Each element of Syms must be a symmetry specifier from the
following set:

<ul>

<li> variable_interchange(L).  This specifies that the variables in
the list L are interchangeable.

<li> value_interchange(L).  This specifies that the values in the list
L are interchangeable.

<li> parallel_variable_interchange(Ls).  This specifies that the lists
of variables in the list L are interchangeable.  Each list in Ls must
have the same length.  For example,
parallel_variable_interchange([A,B,C],[D,E,F],[G,H,I]) says that the
sequence A-B-C can be interchanged with the sequence D-E-F and the
sequence G-H-I.

<li> parallel_value_interchange(Ls).  This specifies that the lists of
values in the list L are interchangeable.  It is the same as
parallel_variable_interchange, but for values.

<li> variables_interchange.  This specifies that all variables in Xs
are interchangeable.

<li> values_interchange.  This specifies that all values in the
domains of the variables in Xs are interchangeable.  Note that for
this specifier it is assumed that all variables in Xs have the same
domain.

<li> rows_interchange.  This specifies that the rows of variables in
Xs are interchangeable.  Assumes that Xs is a 2D matrix of variables.

<li> columns_interchange.  This specifies that the columns of
variables in Xs are interchangeable.  Assumes that Xs is a 2D matrix
of variables.

<li> diagonal_reflection.  This specifies that the variables of Xs can
be reflected around the main diagonal.  Assumes that Xs is a 2D matrix
of variables.

<li> row_reflection.  This specifies that the rows of Xs can be
reflected around their centre.  Assumes that Xs is a 2D matrix of
variables.

<li> column_reflection.  This specifies that the columns of Xs can be
reflected around their centre.  Assumes that Xs is a 2D matrix of
variables.

<li> value_reflection(L, U).  This specifies that the values in the
sequence L..U can be reflected; i.e. value L+i maps to U-i.

<li> value_reflection.  This is the same as value_reflection(L,U),
where L and U are taken from the minimum and maximum values in the
domain of the first variable in Xs.

</ul> " ),

        eg: "
% A vector of interchangeable variables.
dim(Xs, [N]),
[...]
ldsb_initialise(Xs, [variables_interchange])

% Vector of piecewise interchangeable variables.
Xs = [](A,B,C,D,E,F),
[...]
% A,B,C are interchangeable; D,E,F are interchangeable.
ldsb_initialise(Xs, [variable_interchange([A,B,C]),
                     variable_interchange([D,E,F])])

% Variables with interchangeable values.
dim(Xs, [N]),
Xs #:: 1..M,
ldsb_initialise(Xs, [values_interchange])

% N-queens, with one boolean variable per square.
dim(A, [N,N]),
A #:: 0..1,
[...]
ldsb_initialise(A, [row_reflection, column_reflection, diagonal_reflection])

% N-queens with one integer variable per queen.
% Note that only half of the symmetries are represented.
dim(Xs, [1,N]),    % make Xs a 1xN matrix.
Xs #:: 1..N,
[...]
ldsb_initialise(Xs, [column_reflection, value_reflection])

% Latin square of order N.
dim(Xs, [N,N]),
Xs #:: 1..N,
[...]
ldsb_initialise(Xs, [rows_interchange, columns_interchange, values_interchange, diagonal_reflection])

% Social Golfers problem with one set variable per group.
dim(Xs, [W,G]),
[...]
% Within each week, the groups are interchangeable.
( for(I, 1, W), foreach(Subsym, Subsyms), param(Xs,G) do
    subscript(Xs, [I, 1..G], Submatrix),
    variables_interchange(Submatrix, Subsym) ),
% rows_interchange: weeks are interchangeable
% values_interchange: golfers are interchangeable
ldsb_initialise(Xs, [rows_interchange, values_interchange | Subsyms])
"]).


ldsb_initialise(Matrix, Symmetries) :-
        ( foreach(S, Symmetries), foreach(S2, Syms2), param(Matrix) do
            process_sym(Matrix, S, S2) ),
        index_syms(Matrix, Syms2).
        
index_syms(Matrix, Syms) :-
        % Extract parallel_variable_interchange symmetries from other
        % symmetries.
        ( foreach(Sym,Syms),
          fromto([], PVIIn, PVIOut, PVISyms),
          fromto([], OtherIn, OtherOut, OtherSyms) do
            ( Sym = parallel_variable_interchange(_) ->
                PVIOut = [Sym|PVIIn], OtherOut = OtherIn
            ; OtherOut = [Sym|OtherIn], PVIOut = PVIIn ) ),

        % Create shared LDSB structure.
        Shared = ldsb_shared{matrix : Matrix,
                             global_symmetries : OtherSyms},

        (foreachelem(Var,Matrix,Idx), param(PVISyms,Shared) do
          index_syms2(Var, PVISyms, LocalSyms),
          % Create local LDSB structure.
          Local = ldsb_local{idx : Idx,
                             local_symmetries : LocalSyms,
                             shared : Shared},
          add_attribute(Var, Local, ldsb) ).

index_syms2(Var, Syms, Out) :-
        ( foreach(S, Syms),
          foreach(O, Out2),
          param(Var) do
            ( S = parallel_variable_interchange(VarLists),
              find_equality(Var, VarLists, Set, Pos) ->
              O = [sym(S, Set, Pos)]
            ;
              O = []
            )
        ),
        flatten(Out2, Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Updating of symmetry state.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

copy_value_symmetries(X, ValueSymmetries) :-
        get_shared(X, Shared),
        Shared = ldsb_shared{global_symmetries:Syms},
        ( foreach(Sym,Syms),
          fromto([], VSIn, VSOut, ValueSymmetries) do
            ( Sym = value_interchange(_) ->
                VSOut = [Sym|VSIn]
            ; VSOut = VSIn ) ).

% Alter the symmetry as if X=Val had just happened.
alter_symmetry(value_interchange(Values), _Var, Val, ReducedSym) :-
        delete_first(Val, Values, Rest),
        ReducedSym = value_interchange(Rest),
        !.
alter_symmetry(variable_interchange(Vars), Var, _Val, ReducedSym) :-
        delete_equality(Var, Vars, Rest),
        ReducedSym = variable_interchange(Rest),
        !.
alter_symmetry(parallel_value_interchange(ValueLists), _Var, Val, ReducedSym) :-
        ( find_equality(Val, ValueLists, WhichSet, _WhichPosition) ->
          delete_nth1(WhichSet, ValueLists, Rest),
          ReducedSym = parallel_value_interchange(Rest)
        ;
          ReducedSym = parallel_value_interchange(ValueLists)
        ),
        !.
alter_symmetry(S, _, _, S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

member_equality(X,[Y|Ys]) :-
        ( X == Y -> true
        ; member_equality(X, Ys) ).

delete_equality(X, [Y|Ys], Rest) :-
        ( X == Y -> Rest = Ys
        ; delete_equality(X, Ys, Rest2),
          Rest = [ Y | Rest2 ] ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%                          in             in   in       out          out
apply_symmetry(value_interchange(Values), Var, Val, SymLiterals, ReducedSyms) :-
        ( memberchk(Val, Values) ->
          delete_first(Val, Values, Rest),
          ( foreach(R, Rest),
            foreach(L, SymLiterals),
            param(Var) do
              L = Var - R
          ),
          ( Rest = [_] -> ReducedSyms = []
          ; ReducedSyms = [value_interchange(Rest)] )
        ;
          % The value isn't in the set of interchangeable values, so nothing happens.
          SymLiterals = [],
          ReducedSyms = [value_interchange(Values)]
        ).
apply_symmetry(parallel_value_interchange(ValueLists), Var, Val, SymLiterals, ReducedSyms) :-
        ( find_equality(Val, ValueLists, WhichSet, WhichPosition) ->
          delete_nth1(WhichSet, ValueLists, Rest),
          ( foreach(R, Rest),
            foreach(L, SymLiterals),
            param(Var, WhichPosition) do
              nth1(WhichPosition, R, SymValue),
              L = Var - SymValue
          ),
          ( Rest = [_] -> ReducedSyms = []
          ; ReducedSyms = [parallel_value_interchange(Rest)] )
        ;
          SymLiterals = [],
          ReducedSyms = [parallel_value_interchange(ValueLists)]
        ).
apply_symmetry(variable_interchange(Vars), Var, Val, SymLiterals, ReducedSyms) :-
        ( member_equality(Var, Vars) ->
          delete_equality(Var, Vars, Rest),
          ( foreach(R, Rest),
            foreach(L, SymLiterals),
            param(Val) do
              L = R - Val
          ),
          ( Rest = [_] -> ReducedSyms = []
          ; ReducedSyms = [variable_interchange(Rest)] )
        ;
          % The index isn't in the set of interchangeable indices, so nothing happens.
          SymLiterals = [],
          ReducedSyms = [variable_interchange(Vars)]
        ).
apply_symmetry(parallel_variable_interchange(_VarLists), _Var, _Val, _SymLiterals, _ReducedSyms) :-
        writeln(error, "error: apply_symmetry called on parallel_variable_interchange term."),
        abort.

apply_symmetry2(parallel_variable_interchange(VarLists), _Var, Val, WhichSet, WhichPosition, SymLiterals) :-
          delete_nth1(WhichSet, VarLists, OtherLists),
          % The list containing the chosen variable.
          nth1(WhichSet, VarLists, ThisList),
          
          ( foreach(OtherList, OtherLists),
            fromto([], PruningsIn, PruningsOut, SymLiterals),
            param(WhichPosition, ThisList, Val) do
              check_pruning_ok(ThisList, OtherList, PruningOk),

              ( PruningOk = yes, nth1(WhichPosition, OtherList, PruneVar) ->
                PruningsOut = [ ( PruneVar - Val ) | PruningsIn ]
              ; PruningsOut = PruningsIn )
          ).

check_pruning_ok([], [], yes).
check_pruning_ok([T|Ts], [O|Os], PruningOk) :-
          % See if both corresponding variables are ground and equal, or
          % both non-ground.
          OtherVariable = O,
          ThisVariable = T,
          ( 
            ground_and_equal(ThisVariable, OtherVariable) ->
              check_pruning_ok(Ts, Os, PruningOk)
          ; PruningOk = no
          ).

% Succeeds if two variables are ground and equal, or both non-ground.
% Works for sets too, but in the case of set variables, checks that
% their domains are equal.
ground_and_equal(X, Y) :-
        % are they both set variables?
        ( ic_sets : is_solver_var(X), ic_sets : is_solver_var(Y) ->
          % if both set variables, are their domains equal?
          set_range(X, L, U), set_range(Y, L, U),
          #(X, XC), #(Y, YC), get_domain(XC, D), get_domain(YC, D)
          % are they both integer variables?
        ; ( ic : is_solver_var(X), ic : is_solver_var(Y) -> true
            % are they both ground and equal?
          ; ( nonvar(X), nonvar(Y), X = Y -> true
            ; fail ))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utilities for list processing.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Delete the first occurrence of X in a list.
delete_first(X, [Y|Ys], Rest) :-
        ( X == Y -> Rest = Ys
        ; delete_first(X, Ys, Rest2),
          Rest = [X|Rest2] ).

% Delete the nth element from a list.  The first element of the list
% has index 1.
delete_nth1(1, [_|Xs], Rest) :-
        Rest = Xs, !.
delete_nth1(Index, [X|Xs], Rest) :-
	Index \= 1,
        succ(IndexMinus1, Index),
        delete_nth1(IndexMinus1, Xs, RestTail),
        Rest = [ X | RestTail ].

% Retrieve the nth element of a list.  The first element of the list
% has index 1.
nth1_equality(I, [Y|Ys], X) :- nth1_equality(I, [Y|Ys], X, 1).
nth1_equality(I, [Y|Ys], X, N) :-
        ( X == Y -> I = N
        ; N1 is N + 1,
          nth1_equality(I, Ys, X, N1) ).

% find_equality(+X, +L, -N, -P) finds the first occurrence of X in L,
% where L is a list of lists.  Fails if X is not found.  On success,
% instantiates N and P such that X is the Pth element of the Nth list
% in L, where N and P both are indexed from 1.
find_equality(Element, ListOfLists, WhichList, WhichPosition) :-
        find_equality(Element, ListOfLists, 1, WhichList, WhichPosition).
find_equality(Element, [L|Ls], N, WhichList, WhichPosition) :-
        ( nth1_equality(Position, L, Element) ->
          WhichList = N,
          WhichPosition = Position
        ;
          N1 is N+1,
          find_equality(Element, Ls, N1, WhichList, WhichPosition)
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Miscellaneous utilities.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Transpose a matrix.
m_transpose(A, T) :-
        dim(A, [R,C]),
        dim(T, [C,R]),
        ( foreachelem(X,A,[I,J]),
          param(T) do
            subscript(T, [J,I], X)
        ).

% Convert a 2D array into a list of lists.
seqs_to_lists(Seqs, Lists) :-
        ( foreacharg(Seq, Seqs),
          foreach(List, Lists) do
            Seq =.. [[]|List] ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Determine symmetries.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

determine_symmetries(Symmetries, Var, Value, KeptSymmetries) :-
        ( foreach(S, Symmetries),
          foreach(R, KeptSymmetries),
          param(Var, Value) do
          alter_symmetry(S, Var, Value, R)
        ).

determine_symmetries_value(ValueInterchanges, Symmetries, Value, KeptSymmetries) :-
        ( foreach(S, ValueInterchanges),
          foreach(R, KeptValues2),
          param(Value) do
            S = value_interchange(SymVals),
            eclipse_language:intersection(SymVals, Value, Intersection),
            eclipse_language:subtract(SymVals, Value, RestSymVals),
            ( Intersection = [] -> Intersection2 = []
            ; Intersection2 = [value_interchange(Intersection)]),
            ( RestSymVals = [] -> RestSymVals2 = []
            ; RestSymVals2 = [value_interchange(RestSymVals)]),
            R = [Intersection2, RestSymVals2]
        ),
        flatten(KeptValues2, KeptValueSyms),

        % Separate the value_interchange symmetries from the rest of the symmetries.
        ( foreach(S, Symmetries),
          foreach(VI, ValueInterchange2),
          foreach(Other, OtherSymmetries2) do
            ( S = value_interchange(_) -> VI = [S], Other = []
            ; VI = [], Other = [S] )
        ),
        flatten(ValueInterchange2, _ValueInterchanges),
        flatten(OtherSymmetries2, OtherSymmetries),

        append(OtherSymmetries, KeptValueSyms, KeptSymmetries).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% determine_prunings collects all the variable-value pairs that are
% symmetric to the given Var-Value (including Var-Value itself).  It
% does so by finding all pairs directly symmetric to Var-Value, then
% the pairs symmetric to those pairs, etc. until no more pairs can be
% added.

% This case (where Var is ground) is needed for branch-and-bound
% searches (because the cost imposition may make the search variable
% ground).
determine_prunings(_Symmetries, Var, _Value, Prunings) :-
        nonvar(Var), Prunings = [].
determine_prunings(Symmetries, Var, Value, Prunings) :-
        var(Var),
        ( fromto([Var - Value], [(X - V)|PRest], POut, []),
          fromto([Var - Value], FinalPruningsIn, FinalPruningsOut, Prunings2),
          param(Symmetries) do
          determine_prunings2(Symmetries, X, V, PsList),
          list_to_ord_set(PsList, Ps),
          ord_union(FinalPruningsIn, Ps, FinalPruningsOut, New),
          ord_union(PRest, New, POut)
        ),
        ord_subtract(Prunings2, [Var - Value], Prunings).

% determine_prunings2 finds the variable-value pairs *directly*
% symmetric to Var-Value.  (Here "directly" means not via
% composition).
%                      in       in    in      out
determine_prunings2(Symmetries, Var, Value, Prunings) :-
        ( foreach(S, Symmetries),
          foreach(P, PruningsList),
          param(Value, Var) do

          % Make sure Var is an LDSB variable.
          ( get_index(Var, _Idx) -> true ; writeln(error, 'VERY BAD; should not happen'), abort ),
          
          ( S = parallel_variable_interchange(_) -> P = []
          ; apply_symmetry(S, Var, Value, P, _NewSyms) )
        ),
        get_symmetries(Var, Syms),
        (foreach(sym(S,Set,Pos), Syms),
         foreach(P, PruningsList2),
         param(Var, Value) do
           apply_symmetry2(S, Var, Value, Set, Pos, P)
        ),
        flatten(PruningsList, Prunings1),
        flatten(PruningsList2, Prunings2),
        append(Prunings1, Prunings2, PruningsL),
        Prunings = PruningsL.

prune(Prunings) :-
        ( foreach(P, Prunings) do
          Var - Val = P,
          Var #\= Val
        ).

prune_set(Prunings) :-
        ( foreach(P, Prunings) do
          Var - Val = P,
          Val notin Var
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- comment(ldsb_try/3,
           [ amode: (ldsb_try(+, ++, ?)),
             args: [ "X": "Variable to try",
                     "Value": "Value to try",
                     "Success": "Whether the assignment succeeded or not"],
             resat: yes,
             see_also: [ldsb_indomain/1, ldsb_initialise/2, ldsb_try_set/3],
             
             summary: "Try assigning a value to an LDSB variable.",
             
             desc: html("<p/> Tries to assign Value to X.  Upon
             backtracking, excludes Value from the domain of X.  The
             value of Success tells whether the assignment succeeded;
             Success is 1 if X #= Value and 0 if X #\\= Value."),

             eg: "
ldsb_indomain(X) :- nonvar(X), !.
ldsb_indomain(X) :-
        ic:is_solver_var(X), !,
        get_min(X,V),
        ldsb_try(X, V, _),
        ldsb_indomain(X)."]).
           
% LDSB equivalent of ic_gap_sbds:sbds_try/3.
ldsb_try(Var{ldsb:(ldsb_local{shared:Shared})}, Value, Success) :- -?->
        Shared = ldsb_shared{global_symmetries:GlobalSymmetries},
        (
          % Positive branch.

          % Update global symmetries.
          determine_symmetries(GlobalSymmetries, Var, Value, KeptSymmetries),
          setarg(global_symmetries of ldsb_shared, Shared, KeptSymmetries),

          % Make assignment.
          Var = Value,
          Success = 1
        ;
          % Negative branch.
          determine_prunings(GlobalSymmetries, Var, Value, Prunings),
          Var #\= Value,
          prune(Prunings),
          Success = 0
        ).

:- comment(ldsb_try_set/3,
           [ amode: (ldsb_try_set(+, ++, ?)),
             args: [ "X": "Variable to try",
                     "Value": "Value to try",
                     "Success": "Whether the inclusion succeeded or not"],
             resat: yes,
             see_also: [ldsb_indomain_set/1, ldsb_initialise/2, ldsb_try/3],
             
             summary: "Try including a value in an LDSB set variable.",
             
             desc: html("<p/> Tries to include Value in X.  Upon
             backtracking, excludes Value from X.  The value of
             Success tells whether the inclusion succeeded; Success is
             1 if (Value in X) and 0 if Value has been excluded.

             <p/> Note that due to the interaction between set
             variable searching and value symmetries, using this
             predicate in discouraged.  Use ldsb_indomain_set/1
             instead.")]).
           
ldsb_try_set(Var{ldsb:(ldsb_local{shared:Shared})}, Value, Success) :- -?->
        Shared = ldsb_shared{global_symmetries:GlobalSymmetries},
        (
          % Positive branch.

          % Update global symmetries.
          determine_symmetries(GlobalSymmetries, Var, Value, KeptSymmetries),
          setarg(global_symmetries of ldsb_shared, Shared, KeptSymmetries),

          % Make assignment.
          Value in Var,
          Success = 1
        ;
          % Negative branch.
          determine_prunings(GlobalSymmetries, Var, Value, Prunings),
          Value notin Var,
          prune_set(Prunings),
          Success = 0
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicates to instantiate variables.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- comment(ldsb_indomain/1,
           [ amode: (ldsb_indomain(?)),
             args: [ "X": "Variable or integer" ],
             resat: yes,
             see_also: [ldsb_indomain_set/1, ldsb_initialise/2],
             
             summary: "Instantiates an LDSB integer variable to an
             element of its domain.",
             
             desc: html("<p/> Simple predicate for instantiating an
             integer LDSB variable to an element of its domain. It
             starts with the smallest element, and upon backtracking
             tries successive elements until the entire domain has
             been explored, at which point the predicate fails.

             <p/>If X is already a ground integer, then this predicate
             simply succeeds exactly once without leaving a
             choicepoint.

             <p/>This predicate can be used with the search/6
             predicate (see example)."),

             eg:"
go :-
        dim(Xs, [3]),
        Xs #:: 1..5,
        collection_to_list(Xs, L), sum(L) #= 10,
        ldsb_initialise(Xs, [variables_interchange]),
        ( search(Xs, 0, input_order, ldsb_indomain, complete, []),
          writeln(Xs),
          fail
        ; true ).
" ]).
           
% Tries values for X in ascending order.
ldsb_indomain(X) :- nonvar(X), !.
ldsb_indomain(X) :-
        ic:is_solver_var(X), !,
        get_min(X,V),
        ldsb_try(X, V, _),
        ldsb_indomain(X).

:- comment(ldsb_indomain_set/1,
           [ amode: (ldsb_indomain_set(?)),
             args: [ "X": "Set variable or set" ],
             resat: yes,
             see_also: [ldsb_indomain/1, ldsb_initialise/2],
             
             summary: "Instantiates an LDSB set variable to an element
             of its domain.",
             
             desc: html("<p/> Simple predicate for instantiating a set
             LDSB variable to an element of its domain.  If a set
             value is considered a binary number, where 1 is inclusion
             and 0 is exclusion, the value ordering is descending.
             For example:

             <pre>
go :-
    intset(S, 1, 3),
    Xs = [](S),
    ldsb_initialise(Xs, []),
    ( ldsb_indomain_set(S), writeln(S), fail
    ; true).
             </pre>

             would produce the following output:

             <pre>
[1, 2, 3]
[1, 2]
[1, 3]
[1]
[2, 3]
[2]
[3]
[]
             </pre>

             <p/>If X is already a ground set, then this predicate
             simply succeeds exactly once without leaving a
             choicepoint.

             <p/>This predicate can be used with the search/6
             predicate (see example)."),

             eg:"
go :-
        intsets(L, 3, 1, 10),
        ( foreach(S, L) do #(S, 3) ),
        ( fromto(L, [X|Xs], Xs, []) do
          ( foreach(Y, Xs), param(X) do
              #(X /\\ Y, 0) ) ),
        Xs =.. [[]|L],
        ldsb_initialise(Xs, [values_interchange]),
        ( search(Xs, 0, input_order, ldsb_indomain_set, complete, []),
          writeln(Xs),
          fail
        ; true ).
" ]).

% Tries values for a set variable in ascending order, inclusion before
% exclusion.
ldsb_indomain_set(X) :- nonvar(X), !.
ldsb_indomain_set(X) :- var(X), !,
        % Remember state of value symmetries.
        copy_value_symmetries(X, ValueSymmetries),
        get_shared(X, Shared),
        % Assign a value.
        ldsb_indomain_set2(X),
        % Replace the current value symmetries with the result of the
        % old ones applied to the given value.
        Shared = ldsb_shared{global_symmetries : Symmetries},
        determine_symmetries_value(ValueSymmetries, Symmetries, X, KeptSymmetries),
        setarg(global_symmetries of ldsb_shared, Shared, KeptSymmetries).
ldsb_indomain_set2(X) :- nonvar(X), !.
ldsb_indomain_set2(X) :- var(X), !,
        potential_members(X, [V|_]),
        ldsb_try_set(X, V, _),
        ldsb_indomain_set2(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Testing.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

example_case1 :-
        dim(Xs, [3]),
        Xs #:: 1..5,
        collection_to_list(Xs, L), sum(L) #= 10,
        ldsb_initialise(Xs, [variables_interchange]),
        ( search(Xs, 0, input_order, ldsb_indomain, complete, []),
          writeln(Xs),
          fail
        ; true ).

example_case2 :-
        intsets(L, 3, 1, 10),
        ( foreach(S, L) do #(S, 3) ),
        ( fromto(L, [X|Xs], Xs, []) do
          ( foreach(Y, Xs), param(X) do
              #(X /\ Y, 0) ) ),
        Xs =.. [[]|L],
        ldsb_initialise(Xs, [values_interchange]),
        ( search(Xs, 0, input_order, ldsb_indomain_set, complete, []),
          writeln(Xs),
          fail
        ; true ).

example_case3 :-
        intset(S, 1, 3),
        Xs = [](S),
        ldsb_initialise(Xs, []),
        ( ldsb_indomain_set(S), writeln(S), fail
        ; true).

example_case4 :-
        intset(S, 1, 3),
        Xs = [](S),
        ldsb_initialise(Xs, [values_interchange]),
        ( ldsb_indomain_set(S), writeln(S), fail
        ; true).

:- comment(run_tests/0, hidden).

% Initialising variables should set attributes.
test_case(init1) :- dim(Xs, [4]), Xs #:: 1..10, ldsb_initialise(Xs, []), Xs = [](X,_,_,_), get_index(X, _) should_give true.
test_case(init2) :- dim(Xs, [4]), Xs #:: 1..10, Xs = [](X,_,_,_), get_index(X, _) should_fail.
test_case(init3) :- dim(Xs, [4]), Xs #:: 1..10, ldsb_initialise(Xs, []), Xs = [](X,_,_,_), get_symmetries(X, _) should_give true.
test_case(init4) :- dim(Xs, [4]), Xs #:: 1..10, Xs = [](X,_,_,_), get_symmetries(X, _) should_fail.
test_case(init5) :- dim(Xs, [4]), Xs #:: 1..10, ldsb_initialise(Xs, []), Xs = [](X,_,_,_), get_shared(X, _) should_give true.
test_case(init6) :- dim(Xs, [4]), Xs #:: 1..10, Xs = [](X,_,_,_), get_shared(X, _) should_fail.

% Test composition of symmetry.
test_case(compos1) :-
        Xs = [](A,_,_,_),
        Xs #:: 1..6,
        ic_global:alldifferent(Xs),
        ldsb_initialise(Xs, [variables_interchange, value_interchange([1,2,3])]),
        ldsb_try(A, 1, 0) should_fail.

% Test variable and value interchange.
test_case(var1) :- Xs = [](A,_,_,_), Xs #:: 1..10, ldsb_initialise(Xs, [variables_interchange]), ldsb_try(A, 1, 0) should_give true.
test_case(value1) :- Xs = [](A,_,_,_), Xs #:: 1..10, ldsb_initialise(Xs, [values_interchange]), ldsb_try(A, 1, 0) should_fail.

% Test process_sym.
test_case(process_sym1) :- Xs = [](A,B,C,D), Xs #:: 1..10, process_sym(Xs, variables_interchange, variable_interchange([A,B,C,D])) should_give true.
test_case(process_sym2) :- Xs = [](_,_,_,_), Xs #:: 1..3, process_sym(Xs, values_interchange, value_interchange([1,2,3])) should_give true.
test_case(process_sym3) :- Xs = []([](A,B,C),
                                   [](D,E,F),
                                   [](G,H,I)), Xs #:: 1..3, process_sym(Xs, rows_interchange, parallel_variable_interchange([[A1,B1,C1],
                                                                                                                             [D1,E1,F1],
                                                                                                                             [G1,H1,I1]])),
                                   [A,B,C,D,E,F,G,H,I] == [A1,B1,C1,D1,E1,F1,G1,H1,I1] should_give true.
test_case(process_sym4) :- Xs = []([](A,B,C),
                                   [](D,E,F),
                                   [](G,H,I)), Xs #:: 1..3, process_sym(Xs, columns_interchange, parallel_variable_interchange([[A1,D1,G1],
                                                                                                                                [B1,E1,H1],
                                                                                                                                [C1,F1,I1]])),
                                   [A,B,C,D,E,F,G,H,I] == [A1,B1,C1,D1,E1,F1,G1,H1,I1] should_give true.
test_case(process_sym5) :- Xs = []([](_,B,C),
                                   [](D,_,F),
                                   [](G,H,_)), Xs #:: 1..3, process_sym(Xs, diagonal_reflection, parallel_variable_interchange([[D1,G1,H1],[B1,C1,F1]])),
                                   [B,C,D,F,G,H] == [B1,C1,D1,F1,G1,H1] should_give true.
test_case(process_sym6) :- Xs = []([](A,B,C),
                                   [](_,_,_),
                                   [](G,H,I)), Xs #:: 1..3, process_sym(Xs, row_reflection, parallel_variable_interchange([[A1,B1,C1],[G1,H1,I1]])),
                                   [A,B,C,G,H,I] == [A1,B1,C1,G1,H1,I1] should_give true.
test_case(process_sym7) :- Xs = []([](A,_,C),
                                   [](D,_,F),
                                   [](G,_,I)), Xs #:: 1..3, process_sym(Xs, column_reflection, parallel_variable_interchange([[A1,D1,G1],[C1,F1,I1]])),
                                   [A,C,D,F,G,I] == [A1,C1,D1,F1,G1,I1] should_give true.
test_case(process_sym8) :- Xs = [](_,_,_,_), Xs #:: 1..5, process_sym(Xs, value_reflection, parallel_value_interchange([[1,2],[5,4]])) should_give true.
test_case(process_sym9) :- Xs = [](_,_,_,_), Xs #:: 1..6, process_sym(Xs, value_reflection, parallel_value_interchange([[1,2,3],[6,5,4]])) should_give true.
        
run_tests :-
        findall(X, (test_case(X), writeln(X)), _).
