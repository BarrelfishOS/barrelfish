%
% Library for expressing the symmetries of a CSP.
%
% $Id: sym_expr.ecl,v 1.2 2009/07/16 09:11:27 jschimpf Exp $
%

%
% Copyright (C) 2003-2004  The SBDS Group
%
% This library is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
% Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this library; if not, write to the Free Software
% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
%

%
% TODO
%
% - Allow arithmetic expressions where a number is expected.
% - Support multiple value dimensions?
% - Support multiple matrices
% - Allow the user to define their own symmetry specifiers?
% - More error checking...  :)
%

:- module(sym_expr).

:- comment(categories, ["Constraints"]).
:- comment(summary, "Library for expressing the symmetries of a CSP").
:- comment(desc, html("\
   This library provides a convenient way to express the symmetries of a
   CSP, as described in:<P>

   Warwick Harvey, Tom Kelsey and Karen Petrie.  \"Symmetry Group Expression
   for CSPs.\"  In Barbara Smith et al., editors, <EM>Proceedings of
   SymCon'03: Third International Workshop on Symmetry in Constraint
   Satisfaction Problems, a workshop of CP 2003</EM>, pages 86-96.
   September, 2003.<P>

   This is particularly useful for GAP-based symmetry-breaking libraries, as
   it will construct an appropriate group in GAP and provide predicates for
   mapping between CSP assignments and the points acted on by the GAP group.<P>

   Please note that this library currently does not work on Windows machines
   due to its dependence on the GAP interface library.<P>
")).
:- comment(see_also, [library(gap)]).
:- comment(author, "Warwick Harvey").
:- comment(status, evolving).


:- lib(ic).
:- lib(ic_global).
:- lib(hash).
:- use_module(gap).

:- export
	construct_group/8,
	generic_to_point/5,
	generic_from_point/6,
	value_spec_to_range/3.


:- comment(construct_group/8, [
    summary: "Constructs a GAP group based on the provided symmetry specification.",
    args: [ "Array": "The array of search variables",
	    "VarDimNames": "A list of symbolic names for each dimension in Array",
	    "ValueSpec": "The symbolic name for values and their range",
	    "SymSpecs": "A list of symmetry specifiers",
	    "GroupName": "The GAP name to give the constructed group (string)",
	    "MaxPoints": "The number of GAP points operated on by group",
	    "ToPointPred": "A predicate for converting a variable/value assignment to a GAP point",
	    "FromPointPred": "A predicate for converting a GAP point to a variable/value assignment"
	],
    amode: construct_group(+, ++, ++, ++, ++, -, -, -),
    desc: html("\
    This predicate is intended for use in conjunction with a
    GAP/ECLiPSe-based symmetry breaking method.  Given an array of search
    variables (Array) and a description of the symmetries, this predicate
    constructs the corresponding group in GAP (GroupName) and returns a pair
    of `to point' and `from point' predicates suitable for use in
    conjunction with it.<P>

    In order to facilitate the expression of the symmetries, a list of
    symbolic names (VarDimNames) is required, to specify a symbolic name for
    each of the dimensions of Array.  Similarly, some way is needed to
    refer to the value `dimension'; this is provided by ValueSpec, which is
    of the form ValueName:ValueRange.  ValueRange specifies the range of
    values the variables may take (this is just used for constructing the
    group; it is not imposed on the variables) and must be of the form
    Lo..Hi.<P>

    The symmetries (SymSpecs) are then specified by a list with entries of
    the form symmetry(Symmetry, SymmetryDimensions, OtherDimensions).
    Roughly speaking, Symmetry specifies the base symmetry,
    SymmetryDimensions specifies which dimensions it applies to, and
    OtherDimensions specifies which other dimensions should be treated
    specially with respect to this symmetry (if any).  The options for
    Symmetry are:
<dl>
    <dt>s_n</dt>
	<dd>(1 dimension)</dd>
	<dd>Full permutation of the indices of the specified dimension.</dd>

    <dt>cycle</dt>
	<dd>(1 dimension)</dd>
	<dd>Indices may be cycled (i.e. all shifted k positions modulo the
	size of the dimension).</dd>

    <dt>r_4</dt>
	<dd>(2 dimensions)</dd>
	<dd>Rotational symmetry of the square.  The two dimensions specified
	must be of equal size.</dd>

    <dt>d_4</dt>
	<dd>(2 dimensions)</dd>
	<dd>Full symmetry of the square (i.e. including reflections).  The
	two dimensions specified must be of equal size.</dd>

    <dt>reverse</dt>
	<dd>(1 dimension)</dd>
	<dd>Indices may be reversed.</dd>

    <dt>gap_group(GroupFunc)</dt>
	<dd>(no fixed number of dimensions)</dd>
	<dd>The symmetries described by the group returned by the GAP
	function GroupFunc.  GroupFunc should accept a single argument,
	which is a list of the sizes of the dimensions that the symmetry is
	expected to act upon.  The mapping between GAP points and the
	elements of the subarray that this symmetry acts upon is obtained
	by numbering the indices of the elements in lexicographic order
	(first dimension most significant).  For example, if the symmetry
	subarray is of size MxN, then the element with index [I,J] is
	mapped to point (I-1)*N + J, so that [1,1]->1, [1,N]->N, [2,1]->N+1,
	etc.</dd>

    <dt>function(Func)</dt>
	<dd>(no fixed number of dimensions)</dd>
	<dd>Allows a generator to be specified via an ECLiPSe function Func,
	a la classic SBDS symmetry functions.  Func should accept three
	(extra) arguments: DimSizes, SrcIdx and DestIdx.  The first,
	DimSizes, is a list of the sizes of the dimensions that the symmetry
	is expected to act upon.  The second, SrcIdx, provides the index of
	an element of the subarray that the symmetry acts upon.  The third,
	DestIdx, should then be unified with the index of the element that
	SrcIdx is mapped to by this function.  For example, if the generator
	is intended to exchange the dimensions of a square array (i.e.
	reflect along the leading diagonal), if `swap_dim' is passed as
	Func, then the definition of `swap_dim' might be
	<pre>swap_dim([N, N], [I, J], [J, I]).</pre></dd>
	<dd>Note that for 1-dimensional arrays, DestIdx may be returned as
	just an integer rather than a length-1 list containing the integer,
	if desired.</dd>

    <dt>table(Generator)</dt>
	<dd>(no fixed number of dimensions)</dd>
	<dd>Allows a generator to be specified via a lookup table.
	Generator must be an array of same size as the subarray the
	symmetry acts upon, with each element containing the index of the
	element that that element should be mapped to.</dd>
	<dd>Note that for 1-dimensional arrays, an index may be specified
	using just an integer rather than a length-1 list containing the
	integer, if desired.</dd>
</dl>
    SymmetryDimensions must be a list with one entry for each dimension of
    the base symmetry.  Each entry must either be the name of a dimension,
    or of the form Dimension:IdxSpec, where Dimension is the name of a
    dimension and IdxSpec specifies a subset of the indices of that
    dimension.  IdxSpec may be an integer, an integer range L..H, or a list
    of these.  Providing such a qualification means that the symmetry is
    only applied to the specified indices of the dimension, not the
    dimension as a whole.<P>

    By default, a symmetry affects the array as a whole; that is, for
    example, a symmetry applied to columns will affect all rows, and in
    particular all rows in the same way (it won't, say, swap columns 1 and 3
    in row 1, but swap 5 and 6 in row 2; the columns will be rearranged in
    the same way in every row).  If one or more indices for a dimension
    should be affected independently of the rest of the indices for that
    dimension, then this can be specified in the OtherDimensions field,
    which is a list of terms of the form Dimension:IdxSpec.  This specifies
    that the symmetry should only affect the given indices of the given
    dimension, independent of the other indices (the specified indices are
    still affected synchronously).  This allows one to express, for example,
    that the elements of a given row can be permuted, independent of the
    other rows.  If OtherDimensions is just the empty list, then it may be
    omitted.<P>

<dl>
    <dt>To do:</dt>
	<dd>Support multiple value dimensions</dd>
	<dd>Support multiple matrices</dd>
	<dd>More error checking...  :)</dd>
</dl>
"),
    eg: html("\
    <dl><dt>Balanced Incomplete Block Design (BIBD)</dt>
	<dd>Standard model, with a 2-D array of booleans.</dd>
	<dd>Full row and column permutation.</dd>
<p>
	<dl><dt>dim(Array, [NRows, NCols]),</dt>
	<dt>...</dt>
	<dt>construct_group(Array, [rows, cols], values:0..1, [</dt>
		    <dl><dd>symmetry(s_n, [rows], []),</dd>
		    <dd>symmetry(s_n, [cols], [])</dd></dl>
		<dd>], \"bibd_group\", MaxPoints, ToPointPred, FromPointPred)</dd></dl>
<p>
    <dt>Social golfer problem</dt>
	<dd>Boolean model, with one boolean variable for each round/group/player
	combination.</dd>
	<dd>Full permutation of the rounds, full permutation of the groups in
	any round, and full permutation of the players.</dd>
<p>
	<dl><dt>dim(Array, [NRounds, NGroups, NPlayers]),</dt>
	<dt>...</dt>
	<dt>construct_group(Array, [rounds, groups, players], values:0..1, [</dt>
		    <dl><dd>symmetry(s_n, [rounds], []),</dd>
		    <dd>symmetry(s_n, [groups], [rounds:1]),</dd>
		    <dd>symmetry(s_n, [players], [])</dd></dl>
		<dd>], \"golf_group\", MaxPoints, ToPointPred, FromPointPred)</dd></dl>
<p>
	<dd>Note that the permutation of the groups within a round does not
	need to be specified for every round; the fact that the rounds are
	interchangeable means that it need only be specified for one round.</dd>
<p>
    <dt>N Queens problem</dt>
	<dd>Standard row model, with one integer for each row specifying which
	column the queen appears in.</dd>
	<dd>The symmetries of the square, including reflection, applied to the
	row/column combination.</dd>
<p>
	<dl><dt>dim(Array, [NQueens]),</dt>
	<dt>...</dt>
	<dt>construct_group(Array, [rows], cols:1..NQueens, [</dt>
		    <dl><dd>symmetry(d_4, [rows, cols], [])</dd></dl>
		<dd>], \"queens_group\", MaxPoints, ToPointPred, FromPointPred)</dd></dl>
<p>
    <dt>N Queens problem</dt>
	<dd>Boolean model, with one boolean for each location on the board,
	specifying whether a queen appears there or not.</dd>
	<dd>The symmetries of the square, including reflection.</dd>
<p>
	<dl><dt>dim(Array, [NQueens, NQueens]),</dt>
	<dt>...</dt>
	<dt>construct_group(Array, [rows, cols], values:0..1, [</dt>
		    <dl><dd>symmetry(d_4, [rows, cols], [])</dd></dl>
		<dd>], \"queens_group\", MaxPoints, ToPointPred, FromPointPred)</dd></dl>
<p>
	<dd>Note that the symmetry expression for this model is exactly the
	same as for the standard row model; whether the columns are values
	or a dimension of the variable array is irrelevant.</dd>
")]).


cycle_generators_as_lookup_tables(N, [Lookup]) :-
	dim(Lookup, [N]),
	( for(I, 2, N), param(Lookup) do [I] is Lookup[I-1] ),
	[1] is Lookup[N].

swap_generators_as_lookup_tables(I, J, N, [Lookup]) :-
	dim(Lookup, [N]),
	(
	    for(K, 1, N),
	    param(I, J, Lookup)
	do
	    [X] is Lookup[K],
	    ( K == I ->
		X = J
	    ; K == J ->
		X = I
	    ;
		X = K
	    )
	).

s_n_generators_as_lookup_tables(N, Generators) :-
	( N > 2 ->
	    swap_generators_as_lookup_tables(1, 2, N, SwapGenerators),
	    append(SwapGenerators, CycleGenerators, Generators)
	;
	    Generators = CycleGenerators
	),
	cycle_generators_as_lookup_tables(N, CycleGenerators).

reverse_generators_as_lookup_tables(N, [Lookup]) :-
	dim(Lookup, [N]),
	( for(I, 1, N), param(N, Lookup) do [I] is Lookup[N - I + 1] ).

square_rotate_generators_as_lookup_tables(N, [Lookup]) :-
	dim(Lookup, [N, N]),
	(
	    for(I, 1, N),
	    param(N, Lookup)
	do
	    (
		for(J, 1, N),
		param(N, Lookup, I)
	    do
		[I, J] is Lookup[N-J+1,I]
	    )
	).


d_4_generators_as_lookup_tables(N, Generators) :-
	reverse_generators_as_lookup_tables(N, ReverseGenerators0),
	extend_generators([1], [], [N, N], ReverseGenerators0, ReverseGenerators),
	square_rotate_generators_as_lookup_tables(N, RotateGenerators),
	append(ReverseGenerators, RotateGenerators, Generators).


gap_generators_as_lookup_tables(GroupFunc, DimSizes, Generators) :-
	compute_dim_factors(DimSizes, DimFactors, MaxPoints),
	get_generators_from_gap(GroupFunc, DimSizes, GapGenerators),
	unflatten_generators(DimSizes, DimFactors, MaxPoints, GapGenerators, Generators).

ecl_func_as_lookup_table(Func, DimSizes, Generator, Module) :-
	Func =.. FuncAsList0,
	append(FuncAsList0, [DimSizes], FuncAsList),
	dim(Generator, DimSizes),
	ecl_func_as_lookup_table1(FuncAsList, DimSizes, [], Generator,
		Module).

ecl_func_as_lookup_table1(FuncAsList, [], SrcIdx, Generator, Module) :-
	append(FuncAsList, [SrcIdx, DestIdx], CallAsList),
	Call =.. CallAsList,
	call(Call)@Module,
	subscript(Generator, SrcIdx, DestIdx).
ecl_func_as_lookup_table1(FuncAsList, [DimSize | DimSizes], Prefix,
		Generator, Module) :-
	(
	    for(I, 1, DimSize),
	    param(FuncAsList, DimSizes, Prefix, Generator, Module)
	do
	    append(Prefix, [I], Prefix1),
	    ecl_func_as_lookup_table1(FuncAsList, DimSizes, Prefix1,
		    Generator, Module)
	).

	




    % We'll want something more general/flexible than this eventually...
%symmetry(swap, [2], Generators) :-
%	swap_generators_as_lookup_tables(1, 2, 2, Generators).
symmetry(cycle, [N], Generators, _Module) :-
	!,
	cycle_generators_as_lookup_tables(N, Generators).
symmetry(s_n, [N], Generators, _Module) :-
	!,
	s_n_generators_as_lookup_tables(N, Generators).
symmetry(r_4, [N, N], Generators, _Module) :-
	!,
	square_rotate_generators_as_lookup_tables(N, Generators).
symmetry(d_4, [N, N], Generators, _Module) :-
	!,
	d_4_generators_as_lookup_tables(N, Generators).
symmetry(reverse, [N], Generators, _Module) :-
	!,
	reverse_generators_as_lookup_tables(N, Generators).
symmetry(gap_group(GroupFunc), DimSizes, Generators, _Module) :-
	!,
	gap_generators_as_lookup_tables(GroupFunc, DimSizes, Generators).
symmetry(function(Func), DimSizes, [Generator], Module) :-
	!,
	ecl_func_as_lookup_table(Func, DimSizes, Generator, Module).
symmetry(table(Generator0), DimSizes, [Generator], _Module) :-
	!,
	check_generator(Generator0, DimSizes, Generator).
symmetry(Sym, _, _, _) :-
	printf(error, "Unrecognised symmetry specifier: %w.%n", [Sym]),
	abort.



check_generator(Generator0, DimSizes, Generator) :-
	dim(Generator0, DimSizes0),
	( DimSizes0 == DimSizes ->
	    true
	;
	    printf(error, "Generator of wrong size (expected %Dw, got %Dw).%n",
		    [DimSizes, DimSizes0]),
	    abort
	),
	% Check structure of entries, bounds on entries, uniqueness of
	% entries.
	dim(Generator, DimSizes),
	collect_generator_elements(Generator0, DimSizes, Elements0, [], Generator),
	sort(Elements0, Elements),
	length(Elements0, N0),
	length(Elements, N),
	( N =:= N0 ->
	    true
	;
	    printf(error, "Generator contains duplicate elements:%n%Dw.%n",
		    [Generator0]),
	    abort
	),
	check_generator_elements(Elements, DimSizes).

collect_generator_elements(Element0, [], Elements, ElementsTail, Element) :-
	( integer(Element0) ->
	    Element = [Element0]
	;
	    Element = Element0
	),
	Elements = [Element | ElementsTail].
collect_generator_elements(Generator0, [Dim | Dims], Elements, ElementsTail, Generator) :-
	(
	    for(Idx, 1, Dim),
	    fromto(Elements, Elements, ElementsTail, ElementsTail),
	    param(Generator0, Dims, Generator)
	do
	    subscript(Generator0, [Idx], SubGen0),
	    subscript(Generator, [Idx], SubGen),
	    collect_generator_elements(SubGen0, Dims, Elements, ElementsTail, SubGen)
	).

check_generator_elements(Elements, DimSizes) :-
	(
	    foreach(Element, Elements),
	    param(DimSizes)
	do
	    (
		(
		    foreach(I, Element),
		    foreach(Dim, DimSizes)
		do
		    I >= 1,
		    I =< Dim
		)
	    ->
		true
	    ;
		( foreach(_, DimSizes), foreach(1, Ones) do true ),
		printf(error, "Generator contains out-of-range element %Dw.%n",
			[Element]),
		printf(error, "(Should be between %Dw and %Dw.)%n",
			[Ones, DimSizes]),
		abort
	    )
	).


:- tool(construct_group/8, construct_group/9).

construct_group(Array, VarDimNames, ValueSpecs, SymSpecs,
		GroupName, MaxPoints, ToPointPred, FromPointPred, Module) :-
	construct_generators(Array, VarDimNames, ValueSpecs,
		SymSpecs, Generators, DimFactors, MaxPoints, ToPointPred,
		FromPointPred, Module),
	flatten_generators(DimFactors, MaxPoints, Generators, Flattened),
	send_generators_to_gap(Flattened, GroupName).

construct_generators(Array, VarDimNames, ValueDimName:ValueSpec, SymSpecs,
		Generators, DimFactors, MaxPoints, ToPointPred,
		FromPointPred, Module) :-
	dim(Array, VarDimSizes),
	length(VarDimSizes, NVarDims),
	length(VarDimNames, NVarDims1),
	( NVarDims == NVarDims1 ->
	    true
	;
	    printf(error, "Number of variable dimension names (%d) does not match number of dimensions%nin variable array (%d).%n",
		    [NVarDims1, NVarDims]),
	    abort
	),
	value_spec_to_range(ValueSpec, LoVal, HiVal),
	ValDimSize is HiVal - LoVal + 1,
	DimSizes = [ValDimSize | VarDimSizes],
	DimSizeVec =.. [[] | DimSizes],
	hash_create(NameMap),
	(
	    foreach(Name, [ValueDimName | VarDimNames]),
	    count(I, 1, _),
	    param(NameMap)
	do
	    hash_add(NameMap, Name, I)
	),

	(
	    foreach(SymSpec, SymSpecs),
	    fromto(Generators, GeneratorsOut, GeneratorsIn, []),
	    param(NameMap, DimSizes, DimSizeVec, Module)
	do
	    extract_symmetry_data(SymSpec, Sym, SymDimSpecs0, OtherDimSpecs0),
	    translate_dim_specs(NameMap, SymDimSpecs0, SymDimSpecs),
	    translate_dim_specs(NameMap, OtherDimSpecs0, OtherDimSpecs),
	    (
		foreach(SymDimSpec, SymDimSpecs),
		foreach(BaseDimSize, BaseDimSizes),
		param(DimSizeVec)
	    do
		extract_dim_data(SymDimSpec, DimSizeVec,
			BaseDimSize)
	    ),
	    ( symmetry(Sym, BaseDimSizes, BaseGenerators, Module) ->
		true
	    ;
		printf(error, "Conversion to generators failed for specifier:%n%Dw.%n", [Sym]),
		abort
	    ),
	    (
		foreach(BaseGenerator, BaseGenerators),
		fromto(GeneratorsOut, [FinalGenerator | GeneratorsTail],
			GeneratorsTail, GeneratorsIn),
		param(SymDimSpecs, OtherDimSpecs, DimSizes)
	    do
		extend_generator(SymDimSpecs, OtherDimSpecs, DimSizes,
			BaseGenerator, FinalGenerator)
	    )
	),

	compute_dim_factors(DimSizes, DimFactors, MaxPoints),
	ToPointPred = generic_to_point(DimFactors, LoVal),
	FromPointPred = generic_from_point(DimFactors, LoVal).


extract_symmetry_data(symmetry(Sym, SymDimSpecs, OtherDimSpecs),
		Sym, SymDimSpecs, OtherDimSpecs).
extract_symmetry_data(symmetry(Sym, SymDimSpecs),
		Sym, SymDimSpecs, []).



value_spec_to_range(N, 1, N) :-
	integer(N).
value_spec_to_range(L..H, L, H) :-
	integer(L), integer(H).

looks_like_a_list([]) ?- true.
looks_like_a_list([_|_]) ?- true.

translate_dim_specs(NameMap, DimSpecs0, DimSpecs) :-
	% Allow the user to drop the list notation if there's only one.
	( looks_like_a_list(DimSpecs0) ->
	    DimSpecs1 = DimSpecs0
	;
	    DimSpecs1 = [DimSpecs0]
	),
	(
	    foreach(DimSpec1, DimSpecs1),
	    foreach(DimSpec, DimSpecs),
	    param(NameMap)
	do
	    ( DimSpec1 = Dim1:IdxSpec ->
/*
		( integer(Dim1) ->
		    DimSpec = DimSpec1
		;
*/
		    hash_get(NameMap, Dim1, Dim),
		    idx_spec_to_idx_list(IdxSpec, IdxList),
		    DimSpec = Dim:IdxList
%		)
	    ;
		hash_get(NameMap, DimSpec1, DimSpec)
	    )
	).

extract_dim_data(SymDimSpec, DimSizeVec, BaseDimSize) :-
	( SymDimSpec = _ : IdxList ->
	    length(IdxList, BaseDimSize)
	;
	    BaseDimSize is DimSizeVec[SymDimSpec]
	).


    % Dim factors should be largest first, so that generic_from_point/6
    % works.
compute_dim_factors(DimSizes, DimFactors, MaxPoints) :-
	reverse(DimSizes, RevDimSizes),
	(
	    foreach(DimSize, RevDimSizes),
	    fromto([], FactorsTail, [FactorIn | FactorsTail], DimFactors),
	    fromto(1, FactorIn, FactorOut, MaxPoints)
	do
	    FactorOut is FactorIn * DimSize
	).


% Don't use dim sizes, use precomputed dim factors...

generic_to_point(DimFactors, ValOffset, VarIdx, Value0, Point) :-
	Value is Value0 - ValOffset + 1,
	idx_to_point(DimFactors, [Value | VarIdx], Point).

idx_to_point(DimFactors, Idx, Point) :-
	(
	    foreach(Factor, DimFactors),
	    foreach(X, Idx),
	    fromto(1, PointIn, PointOut, Point)
	do
	    PointOut is PointIn + Factor * (X - 1)
	).

generic_from_point(DimFactors, ValOffset, Array, Point, Var, Value) :-
	point_to_idx(DimFactors, Point, [Value0 | VarIdx]),
	Value is Value0 + ValOffset - 1,
	subscript(Array, VarIdx, Var).

point_to_idx(DimFactors, Point0, Idx) :-
	Point is Point0 - 1,
	(
	    foreach(Factor, DimFactors),
	    foreach(X, Idx),
	    fromto(Point, PointIn, PointOut, _)
	do
	    X0 is PointIn // Factor,
	    PointOut is PointIn - X0 * Factor,
	    X is X0 + 1
	).


flatten_generators(DimFactors, MaxPoints, Generators, Flatteneds) :-
	(
	    foreach(Generator, Generators),
	    foreach(Flattened, Flatteneds),
	    param(DimFactors, MaxPoints)
	do
	    flatten_generator(DimFactors, MaxPoints, Generator, Flattened)
	).

flatten_generator(DimFactors, MaxPoints, Generator, Flattened) :-
	(
	    for(SrcPoint, 1, MaxPoints),
	    foreach(DestPoint, Flattened),
	    param(Generator, DimFactors)
	do
	    point_to_idx(DimFactors, SrcPoint, SrcIdx),
	    subscript(Generator, SrcIdx, DestIdx),
	    idx_to_point(DimFactors, DestIdx, DestPoint)
	).

unflatten_generators(DimSizes, DimFactors, MaxPoints, FlatGenerators, Generators) :-
	(
	    foreach(FlatGenerator, FlatGenerators),
	    foreach(Generator, Generators),
	    param(DimSizes, DimFactors, MaxPoints)
	do
	    dim(Generator, DimSizes),
	    unflatten_generator(DimFactors, MaxPoints, FlatGenerator, Generator)
	).

unflatten_generator(DimFactors, MaxPoints, FlatGenerator, Generator) :-
	(
	    foreach(DestPoint, FlatGenerator),
	    count(SrcPoint, 1, FlatPoints),
	    param(Generator, DimFactors)
	do
	    point_to_idx(DimFactors, SrcPoint, SrcIdx),
	    point_to_idx(DimFactors, DestPoint, DestIdx),
	    subscript(Generator, SrcIdx, DestIdx)
	),
	( FlatPoints > MaxPoints ->
	    printf(error, "Generator has too many points (%d; should be %d).%n",
		    [FlatPoints, MaxPoints]),
	    abort
	;
	    % If the generator doesn't have enough points, fill in the rest.
	    (
		for(Point, FlatPoints + 1, MaxPoints),
		param(Generator, DimFactors)
	    do
		point_to_idx(DimFactors, Point, Idx),
		subscript(Generator, Idx, Idx)
	    )
	).
		



extend_generators(OldSpecs, NewSpecs, NewDimSizeList, OldGenerators,
		NewGenerators) :-
	(
	    foreach(OldGenerator, OldGenerators),
	    foreach(NewGenerator, NewGenerators),
	    param(OldSpecs, NewSpecs, NewDimSizeList)
	do
	    extend_generator(OldSpecs, NewSpecs, NewDimSizeList,
		    OldGenerator, NewGenerator)
	).


/*
    initialise_symmetry(Array, [rounds, groups, players], [values:0..1], [
		symmetry(s_n, [rounds], []),
		symmetry(s_n, [groups], [rows:1]),
		symmetry(s_n, [players], [])
	    ], #=)
*/
/*
    Old dimension list:
	<index of new dimension>
	<index of new dimension>:<index set specifier>

    New dimension list:
	<index of new dimension>
	<index of new dimension>:<index set specifier>
*/
extend_generator(OldSpecs, NewSpecs, NewDimSizeList,
		OldGenerator, NewGenerator) :-
	dim(OldGenerator, OldDimSizeList),
	OldDimSizeVec =.. [[] | OldDimSizeList],
	functor(OldDimSizeVec, _, NOldDims),
	length(OldSpecs, NOldSpecs),
	( NOldSpecs =:= NOldDims ->
	    true
	;
	    printf(error, "extend_generator/5: number of specifiers for old generator (%d)%ndoes not match dimensions of old generator (%d).%n",
		    [NOldSpecs, NOldDims]),
	    abort
	),
	dim(NewGenerator, NewDimSizeList),
	NewDimSizeVec =.. [[] | NewDimSizeList],
	extend_generator2(OldSpecs, NewSpecs, OldDimSizeVec, NewDimSizeVec,
		OldGenerator, NewGenerator),
	fill_in_blanks(NewGenerator).


    % How about:
    % Set it up as a constraint problem, and use findall to fill in the
    % relevant fields in the array.
    % "key" -> "value" defines generator's mapping.
    % New dimension:
    %	same variable gives "key" and "value" index for this dimension.
    % Old dimension:
    %	Have 4 variables:
    %	    1.  Old generator "key" variable.
    %	    2.  Old generator "value" variable.
    %	    3.  New generator "key" variable.
    %	    4.  New generator "value" variable.
    %	The "old" variables are mapped to the corresponding "new" ones
    %	either directly or via an element/3 constraint, as appropriate.
    %	When all the old key variables (for all dimensions) are
    %	instantianted, they can be used to look up the appropriate old
    %	values to assign to the old value variables, which will in turn
    %	assign the new value variables.
extend_generator2(OldSpecs, NewSpecs, OldDimSizeVec, NewDimSizeVec,
		OldGenerator, NewGenerator) :-
	functor(OldDimSizeVec, _, NOldDims),
	dim(OldKeyVec, [NOldDims]),
	dim(OldValueVec, [NOldDims]),
	functor(NewDimSizeVec, _, NNewDims),
	dim(NewKeyVec, [NNewDims]),
	dim(NewValueVec, [NNewDims]),
	% Process specs for old dims.
	(
	    foreach(OldSpec, OldSpecs),
	    count(OldDim, 1, _),
	    param(OldDimSizeVec, OldKeyVec, OldValueVec, NewKeyVec, NewValueVec)
	do
	    old_spec_get_idx_var(OldSpec, OldDim, OldDimSizeVec, NewDim, OldKeyVar, NewKeyVar),
	    old_spec_get_idx_var(OldSpec, OldDim, OldDimSizeVec, NewDim, OldValueVar, NewValueVar),
	    arg(OldDim, OldKeyVec, OldKeyVar),
	    arg(OldDim, OldValueVec, OldValueVar),
	    arg(NewDim, NewKeyVec, NewKeyVar),
	    arg(NewDim, NewValueVec, NewValueVar)
	),
	% Process specs for new dims.
	(
	    foreach(NewSpec, NewSpecs),
	    param(NewDimSizeVec, NewKeyVec, NewValueVec)
	do
	    new_spec_get_idx_var(NewSpec, NewDimSizeVec, NewDim, NewKeyValueVar),
	    arg(NewDim, NewKeyVec, NewKeyValueVar),
	    arg(NewDim, NewValueVec, NewKeyValueVar)
	),
	% Fill in any blanks.
	(
	    foreacharg(NewKeyVar, NewKeyVec),
	    foreacharg(NewValueVar, NewValueVec),
	    foreacharg(NewDimSize, NewDimSizeVec)
	do
	    ( free(NewKeyVar) ->
		NewValueVar = NewKeyVar,
		NewKeyVar :: 1..NewDimSize
	    ;
		% Already set up.
		true
	    )
	),
	NewKeyVec =.. [_ | Key],
	NewValueVec =.. [_ | Value],
	OldKeyVec =.. [_ | OldKey],
	OldValueVec =.. [_ | OldValue],
	findall(Key - Value, (
		    labeling(Key),
		    subscript(OldGenerator, OldKey, OldValue)
		), KVList),
	(
	    foreach(K - V, KVList),
	    param(NewGenerator)
	do
	    subscript(NewGenerator, K, V)
	).




old_spec_get_idx_var(NewDim, OldDim, OldDimSizeVec, NewDim, IdxVar, IdxVar) :-
	number(NewDim),
	IdxVar :: 1..OldDimSizeVec[OldDim].
old_spec_get_idx_var(NewDim:IdxList, _OldDim, _OldDimSizeVec, NewDim, SrcIdxVar, DestIdxVar) :-
	element(SrcIdxVar, IdxList, DestIdxVar).

new_spec_get_idx_var(NewDim, NewDimSizeVec, NewDim, IdxVar) :-
	number(NewDim),
	IdxVar :: 1..NewDimSizeVec[NewDim].
new_spec_get_idx_var(NewDim:IdxList, _NewDimSizeVec, NewDim, IdxVar) :-
	IdxVar :: IdxList.


fill_in_blanks(Generator) :-
	dim(Generator, DimSizes),
	reverse(DimSizes, RevDimSizes),
	fill_in_blanks2(RevDimSizes, [], Generator).

fill_in_blanks2([], Idx, Generator) :-
	subscript(Generator, Idx, Entry),
	( var(Entry) ->
	    Entry = Idx
	;
	    true
	).
fill_in_blanks2([DimSize | RevDimSizes], IdxTail, Generator) :-
	(
	    for(I, 1, DimSize),
	    param(RevDimSizes, IdxTail, Generator)
	do
	    fill_in_blanks2(RevDimSizes, [I | IdxTail], Generator)
	).

idx_spec_to_idx_list(IdxSpec0, IdxList) :-
	( looks_like_a_list(IdxSpec0) ->
	    IdxSpec = IdxSpec0
	;
	    IdxSpec = [IdxSpec0]
	),
	(
	    foreach(S, IdxSpec),
	    fromto(IdxList, IdxListOut, IdxListIn, [])
	do
	    ( integer(S) ->
		IdxListOut = [S | IdxListIn]
	    ; S = L .. U, integer(L), integer(U) ->
		(
		    for(I, L, U),
		    fromto(IdxListOut, [I | Tail], Tail, IdxListIn)
		do
		    true
		)
	    ;
		printf(error, "Unrecognised index specifier: %w.%n", [S]),
		abort
	    )
	).


send_generators_to_gap(Generators, GroupName) :-
	gap_command("%Dw := GroupWithGenerators(List(%Dw, i -> PermList(i)))",
		[GroupName, Generators]).

get_generators_from_gap(GroupFunc, DimSizes, Generators) :-
	gap_query("List(GeneratorsOfGroup(%Dw(%Dw)), i -> ListPerm(i))",
		[GroupFunc, DimSizes], Generators).

/*
Test case:

dim(M, [10, 8, 32]),
construct_generators(M, [rounds, groups, players], bools:0..1, [
	symmetry(s_n, [rounds], []),
	symmetry(s_n, [groups], [rounds:[1]]),
	symmetry(s_n, [players], [])
    ], Generators, DimFactors, MaxPoints, ToPointPred, FromPointPred),
flatten_generators(DimFactors, MaxPoints, Generators, Flattened)
*/

