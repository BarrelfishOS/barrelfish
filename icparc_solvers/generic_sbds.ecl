
%TERMS OF USE
%This Library is public domain software; you are entitled to copy, modify and
%redistribute this Library subject to the following:
%1)  If you fix any bugs or make any enhancements to the Library which would
%    be of use to others, we request that you send these modifications to the
%   authors for possible inclusion in future versions of the Library.
%
%2)  If the Library or any derivative is used in the preparation of any
%    publications then the authors should be acknowledged.
%
%3)  This notice is kept intact in any modified or redistributed versions of
%    the Library.
%
%The authors would also like to hear any comments on or suggestions for
%improvement to the Library.
%
%Authors:
%	Warwick Harvey wh@icparc.ic.ac.uk
%	Karen Petrie k.e.petrie@dcs.st-and.ac.uk
%
%The authors acknowledge the support of IC-Parc and Imperial College (first
%author) and The University of Huddersfield (second author) during the
%development of this library.

% $Id: generic_sbds.ecl,v 1.3 2013/02/13 00:58:47 jschimpf Exp $

:- comment(categories, ["Constraints"]).
:- comment(summary, "Symmetry Breaking During Search (SBDS)").
:- comment(desc, "Symmetry Breaking During Search (SBDS) library, provides predicates to initalise symmetry breaking during search search and to perform the search, as well as utility functions for prining and unification.").
:- comment(author, "Warwick Harvey & Karen Petrie").
:- comment(date, "$Date: 2013/02/13 00:58:47 $").
    
%The predicates that the user may need to call, 
% see definitions with predicate
:- export
	sbds_initialise/4, 
	sbds_initialise/5, 	
	sbds_try/2,	   
	is_sbds_var/1,
	sbds_get_index/2,
	sbds_get_matrix/2.

	
:- comment(sbds_initialise/4, [
	summary: "Initialises the data needed for sbds",
	args: ["VarMatrix": "Matrix of Search Variables",
		"SymPreds": "List of symmetry predicates",
		"FixPred": "Predicate to assign a variable to a value", 
		"Options": "List of Options to use during search"],
	amode: sbds_initialise(+,+,++,+),
	see_also: [sbds_initialise/5, sbds_try/2],
	desc: html("<H4><I> Symmetry Predicates </I></H4>
	VarMatrix, is the matrix of variables, which are searched over to allocate 		values to, in this case a 1-dimensonal matrix, for the n-dimensonal case see 		sbds_initalise/5.
	<br>
	<br>
	The symmetry predicates should transform a variable and value to their 
	symmetrical equivalent. The last four arguments of these predicates should 
	therefore be the original variable, the original value (which are input) then 		the symmetrical variable and the symmetrical value (which are output). Before 		these parameters you can give any other parameters which are useful in your 		implementation
	i.e. the matrix of variables. So my symmetry predicate might be:
	symmetry_predicate(Matrix, Var, Val, SymVar, SymVal).
	<br>
	<br>
	When creating the list of symmetry predicates (the parameter given to 			sbds_initalise), 
	you only need to specify the parameters that you have added. So for the above 		predicate,
	the entry to the list would be: symmetry_predicate(Matrix)
	This is shown below in the N-Queens model.
	<br>
	<br>
	The FixPred is the predicate which will fix and exclude a variable to a value 		at decision points in the search tree, it must have three parameters the first 		two will be the variable and the value, and the third will be a boolean which 		specifies whether the variable is being fixed or excluded i.e. is this 			constraint true or false. #= / 3, is usually used for thse purposes.
	<br>
	<br>
	The Options list, will be a list of options which can be used during search 		i.e. whether SBDS should be used at every node of the search tree. None of 		these options are implemented as yet, so it should always be an empty list.
	<br><H4><I> What SBDS initialise does: </I></H4>
	Called before search commences. Sets up the symmetries to indicate that 		
	they are all unbroken initally and initalises all the variables etc. that will 		be utilised during search."),
	exceptions:[abort: "Options is not an empty list"],
	eg: html("<H3>Nqueens using a 1 dimensonal array</H3>
	<H4><I> The Symmetry Predicates for Nqueens Symmetries:</I></H4>
	<dl>
	<dt>r90(Matrix, N, Index, Value, SymVar, SymValue) :-</dt>
		<dd>SymVar is Matrix[Value],</dd>
		<dd>SymValue is N + 1 - Index.</dd>
	</dl>
	
	<dl>
	<dt>r180(Matrix, N, Index, Value, SymVar, SymValue) :-</dt>
		<dd>SymVar is Matrix[N + 1 - Index],</dd>
		<dd>SymValue is N + 1 - Value.</dd>
	</dl>
	
	<dl>
	<dt>r270(Matrix, N, Index, Value, SymVar, SymValue) :-</dt>
		<dd>SymVar is Matrix[N + 1 - Value],</dd>
		<dd>SymValue is Index.</dd>
	</dl>

	<dl>
	<dt>rx(Matrix, N, Index, Value, SymVar, SymValue) :-</dt>
		<dd>SymVar is Matrix[N + 1 - Index],</dd>
		<dd>SymValue is Value.</dd>
	</dl>

	<dl>
	<dt>ry(Matrix, N, Index, Value, SymVar, SymValue) :-</dt>
		<dd>SymVar is Matrix[Index],</dd>
		<dd>SymValue is N + 1 - Value.</dd>
	</dl>

	<dl>
	<dt>rd1(Matrix, _N, Index, Value, SymVar, SymValue) :-</dt>
		<dd>SymVar is Matrix[Value],</dd>
		<dd>SymValue is Index.</dd>
	</dl>

	<dl>
	<dt>rd2(Matrix, N, Index, Value, SymVar, SymValue) :-</dt>
		<dd>SymVar is Matrix[N + 1 - Value],</dd>
		<dd>SymValue is N + 1 - Index.</dd>
	</dl>
		
	<H4><I> Then to initialise SBDS</I></H4>
	<dl>%If the Board is a list of variables then we change it to a matrix<br>	
	<dt>Matrix =.. [[] | Board],</dt>
	<dt>%The list of symmetry predicates,</dt>
	<dt>Syms = [</dt>
		<dd>r90(Matrix, N),</dd>
		<dd>r180(Matrix, N),</dd>
		<dd>r270(Matrix, N),</dd>
		<dd>rx(Matrix, N),</dd>
		<dd>ry(Matrix, N),</dd>
		<dd>rd1(Matrix, N),</dd>
		<dd>rd2(Matrix, N)</dd>
	<dt>],</dt>
	<dt>%the call to sbds_initalise,</dt>
	<dt>sbds_initialise(Matrix, Syms, #=, []).</dt></dl> ")
	]).
	
	
:- comment(sbds_initialise/5, [
	summary: "Initialises the data needed for sbds",
	args: ["VarMatrix": "Matrix of Search Variables",
		"NDims": "The Dimenson of VarMatrix",
		"SymPreds": "List of symmetry predicates",
		"FixPred": "Predicate to assign a variable to a value", 
		"Options": "List of Options to use during search"],
	amode: sbds_initialise(+,++,+,++,+),
	see_also: [sbds_initialise/4, sbds_try/2],
	desc: html("<H4><I> Symmetry Predicates </I></H4>
	VarMatrix, is the matrix of variables, which are searched over to allocate 		values to.
	<br>
	<br>
	NDims is the dimenson of VarMatrix
	<br>
	<br>
	The symmetry predicates should transform a variable and value to their 
	symmetrical equivalent. The last four arguments of these predicates should 
	therefore be the original variable, the original value (which are input) then 		the symmetrical variable and the symmetrical value (which are output). Before 		these parameters you can give any other parameters which are useful in your 		implementation i.e. the matrix of variables. So my symmetry predicate might be:
	symmetry_predicate(Matrix, Var, Val, SymVar, SymVal).
	<br>
	<br>
	When creating the list of symmetry predicates (the parameter given to 			sbds_initalise), you only need to specify the parameters that you have added. 		So for the above predicate, the entry to the list would 				be: symmetry_predicate(Matrix).
	This is shown below in the N-Queens model.
	<br>
	<br>
	The FixPred is the predicate which will fix and exclude a variable to a value 		at decision points in the search tree, it must have three parameters the first 		two will be the variable and the value, and the third will be a boolean which 		specifies whether the variable is being fixed or excluded i.e. is this 			constraint true or false. #= / 3, is usually used for thse purposes.
	<br>
	<br>
	The Options list, will be a list of options which can be used during search 		i.e. whether SBDS should be used at every node of the search tree. None of 		these options are implemented as yet, so it should always be an empty list.
	<br><H4><I> What SBDS initialise does: </I></H4>
	Called before search commences. Sets up the symmetries to indicate that 		
	they are all unbroken initally and initalises all the variables etc. that will 		be utilised during search."),
	exceptions:[abort: "Options is not an empty list"],
	eg: html("<H3>Nqueens using a boolean encoding on a 2 dimensonal array</H3>
	<H4><I> The Symmetry Predicates for Nqueens Symmetries:</I></H4>
	<dl>
	<dt>r90(Matrix, N, [X,Y], Value, SymVar, SymValue) :-</dt>
    		<dd>SymVar is Matrix[Y, N + 1 - X],</dd>
    		<dd>SymValue is Value.</dd>
	</dl>
	<dl>
	<dt>r180(Matrix, N, [X,Y], Value, SymVar, SymValue) :-</dt>
    		<dd>SymVar is Matrix[N + 1 - X, N + 1 - Y],</dd>
    		<dd>SymValue is Value.</dd>
	</dl>
	<dl>
	<dt>r270(Matrix, N, [X,Y], Value, SymVar, SymValue) :-</dt>
    		<dd>SymVar is Matrix[N + 1 - Y, X],</dd>
    		<dd>SymValue is Value.</dd>
	</dl>	
	<dl>
	<dt>rx(Matrix, N, [X,Y], Value, SymVar, SymValue) :-</dt>
    		<dd>SymVar is Matrix[N + 1 - X, Y],</dd>
    		<dd>SymValue is Value.</dd>
	</dl>
	<dl>
	<dt>ry(Matrix, N, [X,Y], Value, SymVar, SymValue) :-</dt>
    		<dd>SymVar is Matrix[X, N + 1 - Y],</dd>
    		<dd>SymValue is Value.</dd>
	</dl>
	<dl>
	<dt>rd1(Matrix, _N, [X,Y], Value, SymVar, SymValue) :-</dt>
    		<dd>SymVar is Matrix[Y, X],</dd>
    		<dd>SymValue is Value.</dd>
	</dl>
	<dl>
	<dt>rd2(Matrix, N, [X,Y], Value, SymVar, SymValue) :-</dt>
    		<dd>SymVar is Matrix[N + 1 - Y, N + 1 - X],</dd>
    		<dd>SymValue is Value.</dd>
	</dl>
	<H4><I> Then to initialise SBDS</I></H4>
	<dt>%The list of symmetry predicates,</dt>
	<dt>Syms = [</dt>
		<dd>r90(Matrix, N),</dd>
		<dd>r180(Matrix, N),</dd>
		<dd>r270(Matrix, N),</dd>
		<dd>rx(Matrix, N),</dd>
		<dd>ry(Matrix, N),</dd>
		<dd>rd1(Matrix, N),</dd>
		<dd>rd2(Matrix, N)</dd>
	<dt>],</dt>
	<dt>%the call to sbds_initalise,</dt>
	<dt>sbds_initialise(Matrix, 2, Syms, #=, []).</dt></dl> ")
	]).
	
	
%attributes defined for the SBDS attributed variable
%see definitions with predicate
:- meta_attribute(sbds, [
	print:print_sbds/2,
	unify:unify_sbds/2
    ]).


%Shared SBDS info (shared between a collection of variables)
%held as part of the variable attribute    
:- local struct(sbds_shared(
	    matrix,	% array of SBDS variables
	    symmetries,	% list of current symmetry info
	    fix,	% pred to call when fixing a value
	    module	% module to call fix, exclude & symmetry preds from
	)).


%"Local" SBDS info (specific to this variable).
%idx is the variable index in data structure
:- local struct(sbds_local(
	    idx,	% integer index (or list of) into matrix
	    shared	% shared SBDS info
	)).
	
	
:- tool(sbds_initialise/4, sbds_initialise_body/5).
:- tool(sbds_initialise/5, sbds_initialise_body/6).


%Called by user as sbds_initalise,
%changed by tool to sbds_initalise_body where module is appended to parameters
%Set up a matrix of SBDS variables, with the given symmetry, fix
%predicates, setting the given options.
sbds_initialise_body(Matrix, SymPreds, FixPred, Options, Module) :-
	sbds_initialise_body(Matrix, 1, SymPreds, FixPred, Options, Module).
	
sbds_initialise_body(Matrix, NDims, SymPreds, FixPred, Options, Module) :-
	(
	    foreach(SymPred, SymPreds),
	    foreach(1 - SymPred, Symmetries)
	do
	    true
	),
	Shared = sbds_shared with [
		matrix:Matrix,
		symmetries:Symmetries,
		fix:FixPred,
		module:Module
	    ],
	process_options(Options, Shared),
	( NDims == 1 ->
	    (
		foreacharg(Var, Matrix),
		count(Idx, 1, _),
		param(Shared)
	    do
		init_var(Var, Idx, Shared)
	    )
	; 
	    init_multi_dim_matrix(Matrix, NDims, [], Shared)
	).


%Called by sbds_initalise_body 
%For multi dimensonal case
init_multi_dim_matrix(MatrixVar, NDims, Prefix, Shared) :-
	( NDims > 0 ->
	    NDims1 is NDims - 1,
	    (
		foreacharg(X, MatrixVar),
		count(Idx, 1, _),
		param(Prefix, Shared, NDims1)
	    do
		append(Prefix, [Idx], Prefix1),
		init_multi_dim_matrix(X, NDims1, Prefix1, Shared)
	    )
	;
	    init_var(MatrixVar, Prefix, Shared)
	).

	
%Called by sbds_initalise_body
%No options implemented yet.
%Eventually options will indicate how to do symmetry breaking 
%i.e. sbds, sbdd, depth of sbdd etc.
%The process options functions are left so it is easy to see 
%where they may be used later
process_options(Options, Shared) :-
	(
	    foreach(Option, Options),
	    param(Shared)
	do
	    process_option(Option, Shared)
	).

process_option(Option,_):-
	printf("Error: unrecognised SBDS option %q\n", [Option]),
	abort.	


%Called by SBDS initalise
%Initialise an SBDS variable with the given index and shared SBDS info.
%Note that a variable can be initialised more than once, as long as it
%belongs to the same SBDS collection each time.
init_var(X{sbds:Attr}, Idx, Shared) :- -?->
	!,
	init_var1(X, Attr, Idx, Shared).
init_var(X, Idx, Shared) :-
	new_sbds_var(X, Idx, Shared).
	

%Called by init_var 
init_var1(X, Attr, Idx, Shared) :-
	var(Attr),
	new_sbds_var(X, Idx, Shared).
init_var1(X, Attr, _Idx, Shared) :-
	nonvar(Attr),
	% Variable already has an SBDS attribute.
	Attr = sbds_local with [shared:Shared1],
	( Shared == Shared1 ->
	    true
	;
	    printf("Error: %q cannot belong to more than one SBDS collection\n", [X]),
	    abort
	).


%Called by init_var1
%Initialise a new SBDS variable with the given index and shared SBDS info
new_sbds_var(X, Idx, Shared) :-
	Attr = sbds_local with [idx:Idx, shared:Shared],
	add_attribute(X, Attr, sbds),
	notify_constrained(X).


:- comment(sbds_try/2, [
	summary: "tries to assign a variable to a value",
	args: ["Var": "SBDS Variable",
		"Val": "Value to try and assign to Value"],
	amode: sbds_try(?,++),
	see_also: [sbds_initialise/4, sbds_initialise/5, ic:search/6],
	desc: html("Called for each variable, value pair tried at a decision point 		during search.
	<br>
	<br>
	Initally tries to assign a variable to a value, if this is succesful then it 		updates the symmetry functions to take account of any symmetries which have 		been broken by this assignment.
	<br>
	<br>
	If this assignment is unsuccesful then it stipulates that the variable cannot 		be assigned to this value, before placing constraints to eliminate the 			symmetrical equivalent of this assgnment. If these symmetries are not already 		broken"),
	exceptions:[abort: "Var is not an sbds attributed variable"],
	eg: html("<H4><I> Replacement for labeling/1 which takes SBDS into account 		</I></H4>
	<dl>
	<dt>sbds_labeling(AllVars) :-</dt>
        <dd>( foreach(Var, AllVars) do</dd>
	  <dl><dd>count_backtracks,</dd>
            <dd>sbds_indomain(Var)</dd></dl>                       
        <dd>).</dd>
        </dl>
	<H4><I>Replacement for indomain/1 which takes SBDS into account.</I></H4>
	<dl>
	<dt>% value ordering is input order</dt>
	<dt>sbds_indomain(X) :-</dt>
		<dd>nonvar(X).</dd>
	<dt>sbds_indomain(X) :-</dt>
		<dd>var(X),</dd>
		<dd>mindomain(X, LWB),</dd>
		<dd>%sbds_try called here</dd>
		<dd>sbds_try(X, LWB),</dd>
		<dd>sbds_indomain(X).</dd>
	</dl>")
	]).


%called by the user to assign var to val using SBDS
%prdeicate extrapolates data from variable attirbute
sbds_try(Var{sbds:(sbds_local with [idx:Idx, shared:Shared])}, Value):- -?->
	!,
	Shared = sbds_shared with [
		symmetries:Symmetries0,
		fix:FixPred,
		module:Module
	    ],
	sbds_try_assign(Var, Value, Idx, Shared, Symmetries0, FixPred, Module).
sbds_try(Var, _Value):-
	nonvar(Var),
	!.
sbds_try(Var, _Value):-
	printf("%q is not an SBDS variable\n", [Var]),
	abort.
	

%called by sbds_try to assign var to val and update symmetries accordingly
%1st clause assigns var to value
%2nd clause assigns var NOT equal to var
sbds_try_assign(Var, Value, Idx, Shared,Symmetries0, FixPred, Module):-	
	sbds_update(Symmetries0, Symmetries, Idx, Value, FixPred, Module),
	setarg(symmetries of sbds_shared, Shared, Symmetries),
	call(FixPred, Var, Value, 1)@Module.
sbds_try_assign(Var, Value, Idx, _Shared, Symmetries0, FixPred, Module):-
	sbds_exclude(Var, Value, Idx, Symmetries0, FixPred, Module),
	call(FixPred, Var, Value, 0)@Module.

%called by 1st clause sbds_try_assign (i.e. var = val)
%updates the symmetry list to record which are broken
sbds_update([], [], _, _, _, _).
sbds_update([SymIn | InTail], SymsOut, Idx, Value, FixPred, Module) :-
	SymIn = SymStatusIn - SymPred,
	apply_symm_get_bool(SymPred, Module, Idx, Value, FixPred, FixBool),
	SymStatusOut #= SymStatusIn + FixBool - 1,
	( check_in(1, SymStatusOut) ->
	    % Symmetry still holds
	    SymsOut = [SymStatusOut - SymPred | OutTail]
	;
	    OutTail = SymsOut
	),
	sbds_update(InTail, OutTail, Idx, Value, FixPred, Module).
    

%called by sbds_exclude
%applies symmetry breaking constraints for unbroken symmetries
sbds_exclude(_Var, Value, Idx, Symmetries, FixPred, Module) :- 
	(
	        foreach(SymStatus - SymPred, Symmetries),
	        param(Idx, Value, FixPred, Module)
	    do
	    	apply_symm_get_bool(SymPred, Module, Idx, Value, FixPred, FixBool),
	        SymStatus + FixBool #=< 1
	).
	

%called by sbds_update and sbds_exclude
%applies the symmetry function to get symm var/val
%then gets bool which indcates whether symm is broken
apply_symm_get_bool(SymPred, Module, Idx, Value, FixPred, FixBool):-
	call(SymPred, Idx, Value, SymVar, SymValue)@Module,
	call(FixPred, SymVar, SymValue, FixBool)@Module.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- comment(is_sbds_var/1, [
	summary: "checks whether a variable is an SBDS variable",
	args: ["Var": "Variable to check"],
	amode: is_sbds_var(?),
	see_also: [sbds_initialise/4, sbds_initialise/5],
	desc: html("\
	Succeeds if and only if Var is a variable and has been initialised
	as an SBDS variable (through a call to sbds_initialise/4 or
	sbds_initialise/5)."),
	fail_if: "Var is not a variable or not an SBDS variable."
	]).

%called by the user to check for an SBDS var
is_sbds_var(_{sbds:(sbds_local with [])}):- -?->
	true.


:- comment(sbds_get_index/2, [
	summary: "get the index of an SBDS attributed variable",
	args: ["Var": "SBDS variable",
		"Idx": "The index of Var"],
	amode: sbds_get_index(?,-),
	desc: html("\
	Unifies Idx with the index of the SBDS variable Var.  The index of
	the variable is its position in the matrix provided in the
	corresponding call to sbds_initialise/4 or sbds_initialise/5"),
	fail_if: "Var is not an SBDS variable."
	]).

%get index from sbds variable
sbds_get_index(_{sbds:(sbds_local with [idx:Idx0])}, Idx) :- -?->
	Idx = Idx0.


:- comment(sbds_get_matrix/2, [
	summary: "get the matrix (array) of SBDS attributed variables",
	args: ["Var": "SBDS variable",
		"Matrix": "The matrix that Var appears in"],
	amode: sbds_get_matrix(?,-),
	desc: html("\
	Unifies Matrix with the matrix (array) provided to a call to
	sbds_initialise/4 or sbds_initialise/5 which contains Var."),
	fail_if: "Var is not an SBDS variable."
	]).

%get matrix from sbds variable
sbds_get_matrix(_{sbds:(sbds_local with [shared:Shared])}, Matrix) :- -?->
	Shared = sbds_shared with [matrix:Matrix].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%Print SBDS variables
print_sbds(_{sbds:Attr}, Print) :- -?->
	nonvar(Attr),
	Attr = sbds_local with [idx:Idx],
	% Probably don't want to print the whole matrix for each variable...
	Print = sbds(Idx).

%Unify SBDS variables
unify_sbds(_, YAttr) :-
	var(YAttr).
unify_sbds(X, YAttr) :-
	nonvar(YAttr),
	unify_any(X, YAttr).

unify_any(_{sbds:XAttr}, YAttr) :- -?->
	unify_meta(XAttr, YAttr).
unify_any(X, _) :-
	nonvar(X).

unify_meta(XAttr, YAttr) :-
	var(XAttr),
	XAttr = YAttr.
unify_meta(XAttr, YAttr) :-
	nonvar(XAttr),
	XAttr = sbds_local with [shared:XShared],
	YAttr = sbds_local with [shared:YShared],
	( XShared == YShared ->
	    true
	;
	    printf("Error: cannot unify variables from different SBDS collections\n", []),
	    abort
	).

%test_unify_sbds(_, _).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
