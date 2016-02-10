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
% Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): 
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% 
% Comments for the generic search routine and search utilities for fd/ic
% problems.
%
% ----------------------------------------------------------------------

:-comment(search/6,[
summary:"A generic search routine for finite domains or IC which implements 
different partial search methods (complete, credit, lds, bbs, dbs, sbds,
gap_sbds, gap_sbdd)",
amode:search(+,++,++,+,++,+),

args:[
      "L" : "a collection (a la collection_to_list/2) of domain
	    variables (Arg = 0) or a collection of terms (Arg > 0)",

      "Arg" :"an integer, which is 0 if the list is a list of domain
	    variables, or greater than 0 if the list consists of terms
	    with arity at least Arg (the value Arg indicating the argument
	    that contains the domain variables to be labeled)",

      "Select" :  "the name of a predefined selection method (input_order,
	    first_fail, smallest, largest, occurrence, most_constrained,
	    max_regret, anti_first_fail), or an atom or compound term
	    specifying a user-defined selection method",

      "Choice" :  "the name of a predefined choice method (indomain,
	    indomain_min, indomain_max, indomain_middle, indomain_reverse_min,
	    indomain_reverse_max, indomain_median, indomain_split,
	    indomain_reverse_split, indomain_random, indomain_interval),
	    or an atom or compound term specifying a user-defined method",

      "Method" :  "one of the following search method specifications:
	    complete, bbs(Steps:integer), lds(Disc:integer),
	    credit(Credit:integer, Extra:integer or bbs(Steps:integer)
	    or lds(Disc:integer)), dbs(Level:integer, Extra:integer or
	    bbs(Steps:integer) or lds(Disc:integer)), sbds, gap_sbds,
	    gap_sbdd",

       "Option" :  "a list of option terms.  Currently recognized
	   are backtrack(-N), node(++Call), nodes(++N)"
],
desc:html("<b>Search/6</b> provides a generic interface to a set of different search
methods.  It can currently be used with either the finite domains (if loaded
via lib(fd_search)), integer IC finite domains, and GFD integer finite 
domains (if loaded via lib(gfd_search)).  By changing the <b>Method</b> 
argument, different partial search algorithms (and their parameters) can be
selected and controlled. The search predicate also provides a number of pre-defined
variable selection methods (to choose which variable will be assigned next)
and some pre-defined value assignment methods (to try out the possible
values for the selected variable in some heuristic order), but user-defined
methods can be used in their place as well.  In order to allow more
structure in the application program, it is possible to pass a collection
of terms rather than only a collection of domain variables. In this way all
information about some entity can be easily grouped together. It also
allows more complex labeling methods which combine the assignment of
multiple variables (like a preference value and a decision variable).

<p>
All search methods use a <i>stable</i> selection method. If several entries
have the same heuristic value, then the first one is selected.  The rest of
the collection (treated as a list) is equal to the original list with the
selected entry removed, the order of the non-selected entries does not
change.
<p>
The pre-defined <b>selection methods</b> are the following:
<ul>
<li><b>input_order</b>
	the first entry in the list is selected</li>
<li><b>first_fail</b>
	the entry with the smallest domain size is selected</li>
<li><b>anti_first_fail</b>
	the entry with the largest domain size is selected</li>
<li><b>smallest</b>
	the entry with the smallest value in the domain is selected</li>
<li><b>largest</b>
	the entry with the largest value in the domain is selected</li>
<li><b>occurrence</b>
	the entry with the largest number of attached constraints is selected</li>
<li><b>most_constrained</b>
	the entry with the smallest domain size is selected.  If several
	entries have the same domain size, the entry with the largest number
	of attached constraints is selected.</li>
<li><b>max_regret</b>
	the entry with the largest difference between the smallest and second
	smallest value in the domain is selected. This method is typically
	used if the variable represents a cost, and we are interested in
	the choice which could increase overall cost the most if the best
	possibility is not taken. Unfortunately, the implementation does
	not always work: If two decision variables incur the same minimal
	cost, the regret is not calculated as zero, but as the difference
	from this minimal value to the next greater value.</li>
</ul><p>
Any other atom will be taken as the specification of a user-defined
selection predicate.  This will be invoked with 2 arguments (X,Criterion)
added and is expected to compute a selection criterion (typically a number)
from a variable or value X.  E.g. if Select is 'my_select', a predicate
definition like the following has to be provided:
<pre>
    my_select(X,Criterion) :-
	...	% compute Criterion from variable X
</pre>
The variable-selection will then select the variable with the lowest value
(in standard term order) of Criterion.  If several variables have the same
value, the first one is selected.  
<p>
The above selection methods use the predefined delete/5 predicate.
If this is not general enough, you can replace it with your own: if Select
is given as select(my_delete), then my_delete(-SelectedVar,+List,-Rest,+Arg)
will be invoked for selecting a variable from List.

<p>
The pre-defined <b>choice methods</b> have the following meaning:
<ul>
<li><b>indomain</b>
    uses the built-in indomain/1.  Values are tried in increasing order. 
    On failure, the previously tested value is not removed.</li>

<li><b>indomain_min</b>
    Values are tried in increasing order.  On failure, the previously
    tested value is removed.  The values are tested in the same order as
    for <b>indomain</b>, but backtracking may occur earlier.</li>

<li><b>indomain_max</b>
    Values are tried in decreasing order.  On failure, the previously
    tested value is removed.</li>

<li><b>indomain_reverse_min</b>
    Like indomain_min, but the alternatives are tried in reverse order.
    I.e. the smallest value is first removed from the domain, and only
    if that fails, the value is assigned.

<li><b>indomain_reverse_max</b>
    Like indomain_max, but the alternatives are tried in reverse order.
    I.e. the largest value is first removed from the domain, and only
    if that fails, the value is assigned.

<li><b>indomain_middle</b>
    Values are tried beginning from the middle of the domain.  On failure,
    the previously tested value is removed.</li>

<li><b>indomain_median</b>
    Values are tried beginning from the median value of the domain.  On
    failure, the previously tested value is removed.</li>

<li><b>indomain_split</b>
    Values are tried by succesive domain splitting, trying the lower half
    of the domain first.  On failure, the tried interval is removed.  This
    enumerates values in the same order as indomain or indomain_min, but
    may fail earlier.</li>

<li><b>indomain_reverse_split</b>
    Values are tried by succesive domain splitting, trying the upper half
    of the domain first.  On failure, the tried interval is removed.  This
    enumerates values in the same order as indomain or indomain_max, but
    may fail earlier.</li>

<li><b>indomain_random</b>
    Values are tried in a random order.  On backtracking, the previously
    tried value is removed.  Using this rutine may lead to unreproducable
    results, as another call wil create random numbers in a different
    sequence.  This method uses the built-in <b>random/1</b> to create
    random numbers, <b>seed/1</b> can be used to force the same number
    generation sequence in another run.</li>

<li><b>indomain_interval</b>
    If the domain consists of several intervals, we first branch on the
    choice of the interval.  For one interval, we use domain
    splitting.</li>

</ul><p>
Any other name is taken as the name of a user-defined predicate of
arity 1, to which the variable to be labeled (or a whole element of
list L, in the Arg&gt;0 case) is passed, e.g.
<pre>
    my_choice(X) :-
	...	% make a choice on variable X
</pre>
Alternatively, a term with 2 arguments can be given as the choice-method,
e.g. my_choice(FirstIn,LastOut). this will lead to the invocation of a
choice predicate with arity 3, e.g.
<pre>
    my_choice(X,In,Out) :-
	...	% make a choice on variable X, using In-Out
</pre>
This allows user-defined state to be transferred between the subsequent
invocations of the choice-predicate (the Out argument of a call to
my_choice/3 for one variable is unified with the In argument of the call to
my_choice/3 for the next variable, and so on).
<p>
In addition, a fixed argument can be passed: my_choice(Param) leads to
invocation of my_choice(X,Param), and my_choice(Param,FirstIn,LastOut)
leads to invocation of my_choice(X,Param,In,Out).

<p>
The different <b>search methods</b> are
<ul>
<li><b>complete</b>
    a complete search routine which explores all alternative choices.</li>

<li><b>bbs(Steps)</b>
    The <i>bounded backtracking search</i> allows <b>Steps</b>
    backtracking steps.</li>

<li><b>lds(Disc)</b>
    A form of the <i> limited discrepancy search </i>.  This method
    iteratively tries 0, 1, 2 ..  <b>Disc</b> changes against the
    heuristic (first) value.  Typical values are between 1 and 3 (which
    already may create too many alternatives for large problems).  The
    original LDS paper stated that the discrepancy to be tested first
    should be at the top of the tree.  Our implementation tries the first
    discrepancy at the bottom of the tree.  This means that solutions may
    be found in a different order compared to the original algorithm. 
    This change is imposed by the evaluation strategy used and can not be
    easily modified.</li>

<li><b>credit(Credit, bbs(Steps))</b>
    The credit based search explores the top of the search tree
    completely.  Initially, a given number of credits (<b>Credit</b>) are
    given.  At each choice point, the first alternative gets half of the
    available credit, the second alternative half of the remaining credit,
    and so on.  When the credit run out, the system switches to another
    search routine, here bbs.  In each of these bounded backtracking
    searches <b>Steps</b> backtracking steps are allowed before returning
    to the top most part of the tree and choosing the next remaining
    candidate.  A good value for <b>Steps</b> is 5, a value of 0 forces a
    deterministic search using the heuristic.  Typical values for
    <b>Credit</b> are either N or N*N, where N is the number of entries in
    the collection.</li>

<li><b>credit(Credit, lds(Disc))</b>
    like the one above, but using <i>lds</i> when the credit runs out. 
    Typically, only one (perhaps 2) discrepancies should be allowed.</li>

<li><b>dbs(Level, bbs(Steps))</b>
    The <i>depth bounded search</i> explores the first <b>Level</b>
    choices in the search tree completely, i.e.  it tries all values for
    the first <b>Level</b> selected variables.  After that, it switches to
    another search method, here bbs.  In each of these searches,
    <b>Steps</b> backtracking steps are allowed.  </li>

<li><b>dbs(Level, lds(Disc))</b>
    like the method above, but switches to lds after the first
    <b>Level</b> variables.</li>

<li><b>sbds</b>
    A complete search routine which uses the SBDS symmetry breaking library
    (lib(ic_sbds) or lib(fd_sbds)) to exclude symmetric parts of the search tree
    from consideration.  The symmetry breaking must be initialised through a
    call to sbds_initialise/4,5 before calling search/6.  Currently the only
    pre-defined choice methods supported by this search method are
    <b>indomain_min</b>, <b>indomain_max</b>, <b>indomain_middle</b>,
    <b>indomain_median</b> and <b>indomain_random</b>.  Any user-defined choice
    method used in conjunction with this search method must use sbds_try/2 to
    assign/exclude values or the symmetry breaking will not work correctly.</li>

<li><b>gap_sbds</b> (Not available for FD.)
    A complete search routine which uses the GAP-based SBDS symmetry breaking
    library lib(ic_gap_sbds) to exclude symmetric parts of the search tree from
    consideration.  The symmetry breaking must be initialised through a call to
    sbds_initialise/5 before calling search/6.  Currently the only pre-defined
    choice methods supported by this search method are <b>indomain_min</b>,
    <b>indomain_max</b>, <b>indomain_middle</b>, <b>indomain_median</b> and
    <b>indomain_random</b>.  Any user-defined choice method used in conjunction
    with this search method must use sbds_try/2 to assign/exclude values or the
    symmetry breaking will not work correctly.</li>

<li><b>gap_sbdd</b> (Not available for FD.)
    A complete search routine which uses the GAP-based SBDD symmetry breaking
    library lib(ic_gap_sbdd) to exclude symmetric parts of the search tree from
    consideration.  The symmetry breaking must be initialised through a call to
    sbdd_initialise/5 before calling search/6.  Currently the only pre-defined
    choice methods supported by this search method are <b>indomain_min</b>,
    <b>indomain_max</b>, <b>indomain_middle</b>, <b>indomain_median</b> and
    <b>indomain_random</b>.  Any user-defined choice method used in conjunction
    with this search method must use sbdd_try/2 to assign/exclude values or the
    symmetry breaking will not work correctly.</li>
</ul>
<p>

The option list is used to pass additional parameters to and from the
procedure.  The currently recognized options are:
<ul>
<li><b>backtrack(-N)</b>
    returns the number of backtracking steps used in the search routine</li>

<li><b>nodes(++N)</b>
    sets an upper limit on the number of nodes explored in the search.  If
    the given limit is exceeded, the search routine stops the exploration
    of the search tree.</li>

<li><b>node(daVinci)</b>
    create a drawing of the search tree using the daVinci graph drawing
    tool.  Each node of the search tree is shown as a node in the tree. 
    The label of the node is the selected term of the collection.</li>

<li><b>node(daVinci(++Call))</b>
    as the previous option, it creates a drawing of the search tree using
    the daVinci graph drawing tool.  But instead of using the complete
    selected term as the label, it call the predicate <b>Call/2</b> to
    choose which part of the selected term to display.</li>
</ul>
"),
fail_if:"Fails if the search tree generated does not contain any solution. 
For partial search methods, this does not mean that the problem does not 
have a solution, but only that the part of the tree generated did not 
contain one.",
resat:yes,
eg:"
top:-
	length(L,8),
	L :: 1..8,
	search(L,0,input_order,indomain,complete,[]).

top:-
	length(L,8),
	L :: 1..8,
	search(L,0,input_order,indomain,bbs(15),[]).

top:-
	length(L,8),
	L :: 1..8,
	search(L,0,input_order,indomain,lds(2),[]).

top:-
	length(L,8),
	L :: 1..8,
	search(L,0,input_order,indomain,credit(64,bbs(5)),[]).

top:-
	length(L,8),
	L :: 1..8,
	search(L,0,input_order,indomain,dbs(2,lds(1)),[]).

% a more complex example with different methods and heuristics
% the list to be assigned is a list of terms queen/2

:- local struct(queen(place,var)).

top:-
	member(Method,[complete,lds(2),credit(64,5),bbs(1000),dbs(5,10)]),
	member(Select,[first_fail,most_constrained,input_order]),
	member(Choice,[indomain,
	               indomain_min,
		       indomain_max,
		       indomain_middle,
		       indomain_median,
		       indomain_split,
		       indomain_random]),
	writeln(queen(Method,Select,Choice)),
	once(queen_credit(64,Select,Choice,Method,L,Back)),
	writeln(L),
	writeln(backtrack(Back)),
	fail.
top:-
	nl.

queen_credit(N,Select,Choice,Method,L,Back):-
	create_queens(1,N,Queens,L),
	setup(L),
	rearrange(Queens,Queens,[],[],Queens1),
	search(Queens1, var of queen, Select, Choice, Method, [backtrack(Back)]).

rearrange([],Last,Last,Res,Res).
rearrange([_],[Mid|Last],Last,Res,[Mid|Res]).
rearrange([_,_|S],[H|T],A1,In,Res):-
	rearrange(S,T,[A|A1],[H,A|In],Res).

create_queens(N,M,[],[]):-
	N > M,
	!.
create_queens(N,M,[queen{place:N,var:X}|T],[X|L]):-
	X :: 1..M,
	N1 is N+1,
	create_queens(N1,M,T,L).

setup([]).
setup([H|T]):-
	setup1(H,T,1),
	setup(T).

setup1(_,[],_).
setup1(X,[Y|R],N):-
	X #\\= Y,
	X #\\= Y + N,
	Y #\\= X + N,
	N1 is N+1,
	setup1(X,R,N1).


% this example shows how to pass information from one assignment step 
% to the next
% this uses a term of two arguments as the choice argument
% The example also shows the use of the option argument:
% the search tree generated is drawn with the daVinci graph drawing tool
% and the search is limited to 1000 nodes.
% The number of backtracking steps is returned in the variables Back.
:-local struct(country(name,color)).

top:-
	countries(C),
	create_countries(C,Countries,Vars),
	findall(n(A,B),n(A,B),L),
	setup(L,Countries),
	search(Countries,
	       color of country, % select based on this variable
	       most_constrained,
	       assign([1,2,3,4],Out), % this calls assign/3
	       complete,
	       [backtrack(Back),node(daVinci),nodes(1000)]),
	writeln(Vars),
	writeln(Back),
	writeln(Out).

create_countries([],[],[]).
create_countries([C|C1],[country{name:C, color:V}|R1],[V|V1]):-
	V :: 1..4,
	create_countries(C1,R1,V1).

setup([],_L).
setup([n(A,B)|N1],L):-
	member(country{name:A, color:Av},L),
	member(country{name:B, color:Bv},L),
	Av #\\= Bv,
	setup(N1,L).

% this is the choice predicate
% the first argument is the complete selected term
% the second is the input argument
% the third is the output argument
% here we pass a list of values and rotate this list from one step to the next
assign(country{color:X},L,L1):-
	rotate(L,L1),
	member(X,L).

rotate([A,B,C,D],[B,C,D,A]).

% another example of argument passing 
% here each entry gets the same information
% it is passed unchanged from one level to the next

top:-
	...
	length(L,N),
	L :: 1..10,
	...
        search(L,
	       0,
	       most_constrained,
	       % pass two lists as the In argument of assign
	       % try the odd numbers before the even numbers
	       assign([1,3,5,7,9]-[2,4,6,8,10],_), 
	       complete,[]),
	...

% this is the assignment routine
% the first argument is a 
% Pass the In argument as the Out argument
% try values from list L1 before values from list L2
assign(X,L1-L2,L1-L2):-
	member(X,L1);member(X,L2).

% and another example from square placement
% alternatively try minimal and maximal values first

:-local struct(square(x,y,size)).

top:-
	data(L),
	create_squares(L,Squares),
	...
        search(Squares,
	       0, % this value does not matter if input_order is chosen
	       input_order,
	       assign(min,_),
	       complete,
	       []),
	...

% the assignment routine
% alternate between min and max for consecutive levels in the search
assign(square{x:X,y:Y},Type,Type1):-
	swap(Type,Type1),
	indomain(X,Type),
	indomain(Y,Type).

swap(max,min).
swap(min,max).

% this example shows that the choice routine may consist of several clauses
% the idea comes from a graph coloring heuristic

top:-
	length(L,N),
	L :: 1..100,
	...
        search(L,
	       0,
	       most_constrained,
	       assign(0,K), The In argument is the highest color used so far
	       complete,[]),
	...


% assign variable X either to one of the colors 1..K 
% which have already been used, or to the new color K+1
% we do not need to try other values K+2 etc, as this is a symmetry that
% we can avoid
assign(X,K,K):-
	X #=< K,
	indomain(X).
assign(K1,K,K1):-
	K1 is K+1.


% example showing use of the SBDS library with a user-defined choice method
% which calls sbds_try/2.

top:-
	dim(M, [8]),
	M[1..8] :: 1..8,
	...
	sbds_initialise(M,SymPreds,#=,[]),
	M =.. [_|L],	% get list of variables for search routine
	search(L,0,first_fail,sbds_indomain_max,sbds,[]).

sbds_indomain_max(X):-
	nonvar(X).
sbds_indomain_max(X):-
	var(X),
	get_max(X,Max),
	sbds_try(X,Max),
	sbds_indomain_max(X).


% Example showing use of the GAP-based SBDS library with a user-defined
% choice method which calls sbds_try/2.  (For the GAP-based SBDD library,
% just substitute \"sbdd\" for each occurrence of \"sbds\" below.)

top:-
	dim(M, [8]),
	M[1..8] :: 1..8,
	sbds_initialise(M,[vars],values:1..8,[symmetry(s_n,[vars])],[]),
	M =.. [_|L],	% get list of variables for search routine
	search(L,0,first_fail,gap_sbds_indomain_max,gap_sbds,[]).

gap_sbds_indomain_max(X):-
	nonvar(X).
gap_sbds_indomain_max(X):-
	var(X),
	get_max(X,Max),
	sbds_try(X,Max),
	gap_sbds_indomain_max(X).
",

see_also:[indomain/1,indomain/2,labeling/1,deleteff/3,deleteffc/3,
	sbds_initialise/4, sbds_initialise/5, sbds_try/2, sbdd_initialise/5,
	sbdd_try/2, library(ic_sbds), library(fd_sbds),
	library(ic_gap_sbds), library(ic_gap_sbdd)]

]).


:-comment(delete/5,[
summary:"Choose a domain variable from a list according to selection criterion.",
amode:delete(-,+,-,++,++),

args:[
      "X" : " a free variable",
      "List" : " a list of variables or terms ",
      "R" : " a free variable ",
      "Arg" : " an integer",
      "Select" : " the name of the selection criterion"
],
desc:html("
This predicate chooses one entry in a list of variables or terms based
on some selection criterion.  The criteria are explained in detail in
the <b>search/6</b> predicate. The selected entry is returned in X, with
the rest of the list without X returned in R.<p>
"),
fail_if:"fails if the list is empty",
resat:no,
eg:"
",

see_also:[search/6]

]).


:-comment(indomain/2,[
summary:"a flexible way to assign values to finite domain variables",
amode:indomain(?,++),

args:[
"Var":"a domain variable or an integer",
"Method":"one of the atoms min, max, middle, median, split, interval, random or an integer"],
desc:html("This predicate provides a flexible way to assign values to finite 
domain variables.<p>
The available methods are:
<ul>
<li><b>enum</b> Identical to indomain/1. Start enumeration from the smallest
    value upwards, without first removing previously tried values.</li>

<li><b>min</b> Start the enumeration from the smallest value upwards. 
    This behaves like the built-in <b>indomain/1</b>, except that it
    removes previously tested values on backtracking.</li>

<li><b>max</b> Start the enumeration from the largest value
    downwards.</li>

<li><b>reverse_min</b> Like min, but tries the alternatives in opposite
    order, i.e. values are excluded first, then assigned.</li>

<li><b>reverse_max</b> Like max, but tries the alternatives in opposite
    order, i.e. values are excluded first, then assigned.</li>

<li><b>middle</b> Try the enumeration starting from the middle of the
    domain.  On backtracking, this chooses alternatively values above and
    below the middle value, until all alternatives have been tested.</li>

<li><b>median</b> Try the enumeration starting from the median value
    of the domain.  On backtracking, this chooses alternatively values
    above and below the median value, until all alternatives have been
    tested.</li>

<li><b>split</b> Try the enumeration by splitting the domain
    successively into halves until a ground value is reached.  This
    sometimes can detect failure earlier than the normal enumeration
    methods, but enumerates the values in the same order as min.</li>

<li><b>reverse_split</b> Like split, but tries the upper half of the
    domain first.</li>

<li><b>interval</b> If the domain consists of several intervals, then
    we branch first on the choice of the interval.  For one interval, we
    use domain splitting.</li>

<li><b>random</b> Try the enumeration in a random order.  On
    backtracking, the previously tested value is removed.  This method
    uses <b>random/1</b> to create random numbers, use <b>seed/1</b>
    before to make results reproducible.</li>

<li><b>Value:integer</b> Like middle, but start with the given integer
    <b>Value</b></li>

<li><b>sbds_min</b> Like min, but use <b>sbds_try/2</b> to make choices (for
    use in conjunction with the SBDS symmetry breaking library).</li>

<li><b>sbds_max</b> Like max, but use <b>sbds_try/2</b> to make choices (for
    use in conjunction with the SBDS symmetry breaking library).</li>

<li><b>sbds_middle</b> Like middle, but use <b>sbds_try/2</b> to make choices
    (for use in conjunction with the SBDS symmetry breaking library).</li>

<li><b>sbds_median</b> Like median, but use <b>sbds_try/2</b> to make choices
    (for use in conjunction with the SBDS symmetry breaking library).</li>

<li><b>sbds_random</b> Like random, but use <b>sbds_try/2</b> to make choices
    (for use in conjunction with the SBDS symmetry breaking library).</li>

<li><b>sbds(Value:integer)</b> Like Value:integer, but use <b>sbds_try/2</b>
    to make choices (for use in conjunction with the SBDS symmetry breaking
    library).</li>

<li><b>gap_sbds_min</b> Like min, but use <b>sbds_try/2</b> to make choices (for
    use in conjunction with the GAP-based SBDS symmetry breaking library,
    lib(ic_gap_sbds)).</li>

<li><b>gap_sbds_max</b> Like max, but use <b>sbds_try/2</b> to make choices (for
    use in conjunction with the GAP-based SBDS symmetry breaking library,
    lib(ic_gap_sbds)).</li>

<li><b>gap_sbds_middle</b> Like middle, but use <b>sbds_try/2</b> to make choices
    (for use in conjunction with the GAP-based SBDS symmetry breaking
    library, lib(ic_gap_sbds)).</li>

<li><b>gap_sbds_median</b> Like median, but use <b>sbds_try/2</b> to make choices
    (for use in conjunction with the GAP-based SBDS symmetry breaking
    library, lib(ic_gap_sbds)).</li>

<li><b>gap_sbds_random</b> Like random, but use <b>sbds_try/2</b> to make choices
    (for use in conjunction with the GAP-based SBDS symmetry breaking
    library, lib(ic_gap_sbds)).</li>

<li><b>gap_sbds(Value:integer)</b> Like Value:integer, but use <b>sbds_try/2</b>
    to make choices (for use in conjunction with the GAP-based SBDS symmetry
    breaking library, lib(ic_gap_sbds)).</li>

<li><b>gap_sbdd_min</b> Like min, but use <b>sbdd_try/2</b> to make choices (for
    use in conjunction with the GAP-based SBDD symmetry breaking library,
    lib(ic_gap_sbdd)).</li>

<li><b>gap_sbdd_max</b> Like max, but use <b>sbdd_try/2</b> to make choices (for
    use in conjunction with the GAP-based SBDD symmetry breaking library,
    lib(ic_gap_sbdd)).</li>

<li><b>gap_sbdd_middle</b> Like middle, but use <b>sbdd_try/2</b> to make choices
    (for use in conjunction with the GAP-based SBDD symmetry breaking
    library, lib(ic_gap_sbdd)).</li>

<li><b>gap_sbdd_median</b> Like median, but use <b>sbdd_try/2</b> to make choices
    (for use in conjunction with the GAP-based SBDD symmetry breaking
    library, lib(ic_gap_sbdd)).</li>

<li><b>gap_sbdd_random</b> Like random, but use <b>sbdd_try/2</b> to make choices
    (for use in conjunction with the GAP-based SBDD symmetry breaking
    library, lib(ic_gap_sbdd)).</li>

<li><b>gap_sbdd(Value:integer)</b> Like Value:integer, but use <b>sbdd_try/2</b>
    to make choices (for use in conjunction with the GAP-based SBDD symmetry
    breaking library, lib(ic_gap_sbdd)).</li>
</ul>
On backtracking, all methods except enum first remove the previously tested
value before choosing a new one.  This sometimes can have a huge impact on the
constraint propagation, and normally does not cause much overhead, even if no
additional propagation occurs.
"),
fail_if:"No",
resat:yes,
eg:"
top:-
	X :: 1..10,
	indomain(X,min),
	write(X),put(32),
	fail.
top.

% writes 1 2 3 4 5 6 7 8 9 10

top:-
	X :: 1..10,
	indomain(X,max),
	write(X),put(32),
	fail.
top.

% writes 10 9 8 7 6 5 4 3 2 1

top:-
	X :: 1..10,
	indomain(X,middle),
	write(X),put(32),
	fail.
top.

% writes 5 6 4 7 3 8 2 9 1 10

top:-
	X :: 1..10,
	indomain(X,median),
	write(X),put(32),
	fail.
top.

% writes 5 6 4 7 3 8 2 9 1 10

top:-
	X :: 1..10,
	indomain(X,3),
	write(X),put(32),
	fail.
top.

% writes 3 4 2 5 1 6 7 8 9 10

top:-
	X :: 1..10,
	indomain(X,split),
	write(X),put(32),
	fail.
top.

% writes 1 2 3 4 5 6 7 8 9 10

top:-
	X :: 1..10,
	indomain(X,random),
	write(X),put(32),
	fail.
top.

% writes for example 5 3 7 6 8 1 2 10 9 4

",
see_also:[search/6,indomain/1,random/1,seed/1,sbds_try/2,sbdd_try/2]

]).


:-comment(nth_value/3,[
summary:"return the nth value in a domain",
amode:nth_value(+,++,-),

args:[
    "Domain":	"The domain list to select a value from",
    "N":	"The position of the value to select",
    "Value":	"The selected value"
],
desc:html("Find the nth value in a domain given by an interval or a list of
integers and intervals, as it is returned for example by ic's
<b>get_domain/2</b> or fd's <b>::/2</b>.
"),
fail_if:"N < 1 or there are less than N values in the domain.",
resat:no,
eg:"
% a typical use

	....
	get_domain(X, L),
	nth_value(L, 2, Value),
	....
",

see_also:[fd:(::)/2, ic:get_domain/2]

]).

