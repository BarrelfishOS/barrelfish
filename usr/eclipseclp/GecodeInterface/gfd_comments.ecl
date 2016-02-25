:- comment(categories, ["Constraints"]).
:- comment(summary, "Interface to gecode solver for integer finite domains").

:- comment(author, "Kish Shen").

:- comment(desc, html("<P>
   The GFD library is an interface to the gecode finite domain constraint
   solver. Gecode (www.gecode.org) is an open-source toolkit for developing
   constraint-based systems in C++, and includes a high-performance constraint 
   solver.</P><P>

   This interface provides a high degree of compatibility with the finite 
   domain portion of the IC library, and to a lesser extent, with the FD
   library as well. This means that programs originally written for the
   IC library should run with GFD with little modifications, beyond 
   renaming any explicit calls to the ic family of modules.</P><P>
 
   The main differences from the IC library are:
<UL>
       <LI>Real interval arithmetic and variables are not supported.

       <LI>Domain variables have finite bounds, and the maximum bounds are
       determined by gecode. Like FD, default finite bounds are given to 
       domain variables that are not explicitly given bounds.

       <LI>Constraint propagation is performed in gecode, and each propagation
       phase is atomic at the ECLiPSe level. Posting of constraints and 
       propagation of their consequences are separate in gecode. GFD uses a
       demon suspended goal to perform the propagation: after the posting
       of any constraint (and other changes to the problem that needs 
       propagation), the suspended goal is scheduled and woken. When the
       woken goal is executed, propagation is performed. The goal is suspended
       at priority 9, so if the posting of the constraint is executed at
       normal priority (12), the propagation will happen immediately. However,
       if the posting is done at a priority 10 or higher, then the propagation
       is delayed, allowing multiple constraints to be posted without
       propagation. 

       <LI>GFD supports constraints that are supported by Gecode,
       so the exact set of constraints supported is different from IC. 
       However, the same basic arithmetic operators and relations are 
       supported, allowing for very similar arithmetic expressions. Also,
       many, if not most, of the constraints in IC are supported, along with
       many constraints not supported by IC. See the detailed documentation
       for more information.

       <LI>All constraints can be called from the gfd module, and in
       addition, some constraints can be called from modules that specify
       the consistency level: gfd_gac (generalised arc consistency, aka
       domain consistency), gfd_bc (bounds consistency), gfd_vc (value
       consistency (naive)). The calls to gfd uses the default consistency 
       defined for the constraint by gecode. These consistency levels maps 
       directly to those defined for the constraints, so if gecode supports 
       different consistency levels for a constraint, GFD supports it as 
       well. In  particular (and unlike IC), most arithmetic operations can 
       be bounds (the default) as well as domain consistent.

       <LI>gfd:search/6 interfaces to gecode's search-engines, where the
       entire search is performed in gecode, and the whole search appears
       atomic at the ECLiPSe level. The syntax for search/6 is designed to
       be as compatible with IC's search/6, although there are some 
       differences. The exact equivalent to IC's search/6, where the search
       is performed at the ECLiPSe level, is available via the gfd_search
       module (in fact it shares the same code with IC's search/6). This
       provides more flexibility, but is likely to be less efficient,
       because the search is done in ECLiPSe, and also because 
       it is not optimised for use with gecode. In addition,
       gfd also provide predicates for both variable selection and
       value choice that are optimised for gecode, which should be more
       efficient than those provided by gfd_search.

       <LI>The suspension lists supported by GFD are different from IC.
       Currently, only the 'any' suspension list (for any changes to the
       variable's domain) found in FD but not IC, is supported. Not that
       the GFD constraints are implemented in gecode directly, and therefore
       do not use GFD's suspension lists. 

</UL><P>
      <P>
   The following can be used inside arithmetic integer expressions:
   <DL>
   <DT><STRONG>X</STRONG><DD>
	    Variables.  If X is not yet a domain variable, it is turned 
	    into one.

   <DT><STRONG>123</STRONG><DD>
	    Integer constants.

   <DT><STRONG>-Expr</STRONG><DD>
	    Sign change.

   <DT><STRONG>abs(Expr)</STRONG><DD>
	    The absolute value of Expr.

   <DT><STRONG>E1+E2</STRONG><DD>
	    Addition.

   <DT><STRONG>E1-E2</STRONG><DD>
	    Subtraction.

   <DT><STRONG>E1*E2</STRONG><DD>
	    Multiplication.

   <DT><STRONG>E1//E2</STRONG><DD>
	    Integer division. Truncate towards zero.

   <DT><STRONG>E1/E2</STRONG><DD>
	    Division, defined only where E2 evenly divides E1 (non-inlined),

   <DT><STRONG>E1 rem E2</STRONG><DD>
	    Integer remainder (modulus), same sign as E1.

   <DT><STRONG>Expr^N</STRONG><DD>
	    Power, Expr to the power N. N is a non-negative integer. 
            Mapped to sqr(Expr) if N = 2.

   <DT><STRONG>min(E1,E2)</STRONG><DD>
	    Minimum.

   <DT><STRONG>max(E1,E2)</STRONG><DD>
	    Maximum.

   <DT><STRONG>sqr(Expr)</STRONG><DD>
	    Square.  Logically equivalent to Expr*Expr.

   <DT><STRONG>isqrt(Expr)</STRONG><DD>
	    Integer square root. Truncated to nearest smaller integer.
            Always non-negative

   <DT><STRONG>sqrt(Expr)</STRONG><DD>
	    Square root, defined only where Expr is the square of an integer.
            Always non-negative (non-inlined).

   <DT><STRONG>inroot(Expr,N)</STRONG><DD>
	    Integer Nth root. N is a positive integer. Truncated to nearest smaller integer.
            For even N, result is the non-negative root. 

   <DT><STRONG>rsqr(Expr)</STRONG><DD>
	    Reverse of the sqr function.  Negative root is not excluded (non-inlined).

   <DT><STRONG>rpow(E1,N)</STRONG><DD>
	    Reverse of exponentiation. i.e. finds X in E1 = X^N. N is a
            positive integer (non-inlined).

   <DT><STRONG>sum(ExprCol)</STRONG><DD>
	    Sum of a collection of expressions.

   <DT><STRONG>sum(IntCol*ExprCol)</STRONG><DD>
	    Scalar product of a collection of integers and expressions.
            IntCol and ExprCol must be the same size.

   <DT><STRONG>min(ExprCol)</STRONG><DD>
	    Minimum of a collection of expressions.

   <DT><STRONG>max(ExprCol)</STRONG><DD>
	    Maximum of a collection of expressions.

   <DT><STRONG>element(ExprIdx, Col)</STRONG><DD>
            Element constraint, Evaluate to the ExprIdx'th element of Col.
	    ExprIdx can be an integer expression. 

   <DT><STRONG>Functional/reified constraints</STRONG><DD>
            Written without last argument, which is taken as the value of
            the expression. Only reified constraints (whose last argument
            is the 0/1 boolean) and constraints that can be written as 
            functions (last argument is a domain variable) are allowed.
            Expressions in relational constraints are restricted to 
            inlined expressions only.
            (non-inlined).

   <DT><STRONG>eval(Expr)</STRONG><DD>
	    Equivalent to Expr.
  
 <DT><STRONG>ConLev: Expr</STRONG><DD>
	    Expr is passed to Gecode at constraint level ConLev.
            ConLev can be gfd_gac, gfd_bc, gfd_vc, gfd. 
   </DL>
<p>
The following can be used inside logical constraint expressions:
   <DL>
   <DT><STRONG>X</STRONG><DD>
	    Boolean variables with 0..1 domain.  If X is not yet a
            domain variable, it is turned 
	    into one.

   <DT><STRONG>1/STRONG><DD>
	    boolean constants. 0 for false, 1 for true.

   <DT><STRONG>E1 and E2</STRONG><DD>
	    Reified constraint conjunction.  
`           E1 and E2 are logical constraint expressions.

   <DT><STRONG>E1 or E2</STRONG><DD>
	    Reified constraint disjunction.
`           E1 and E2 are logical constraint expressions.

   <DT><STRONG>E1 xor E2</STRONG><DD>
	    Reified constraint exclusive disjunction/non-equivalence. 
`           E1 and E2 are logical constraint expressions.

   <DT><STRONG>E1 =&gt; E2</STRONG><DD>
	    Reified constraint implication.
`           E1 and E2 are logical constraint expressions.

   <DT><STRONG>E1 &lt;=&gt; E2</STRONG><DD>
	    Reified constraint equivalence.
`           E1 and E2 are logical constraint expressions.

   <DT><STRONG>neg E</STRONG><DD>
	    Reified constraint negation. 
`           E is a logical constraint expression.

   <DT><STRONG>element(ExprIdx, BoolCol)</STRONG><DD>
            Element constraint, Evaluate to the ExprIdx'th element of BoolCol.
	    ExprIdx can be an inlined integer expression. BoolCol is a
	    collection of boolean values or domain variable.

   <DT><STRONG>Reified constraints</STRONG><DD>
            Written without last argument, which is taken as the truth
            value of the expression. Reified relational constraints
            are supported inlined and only inlined integer expressions
            are allowed.
            (non-inlined except reified relational constraints).

   <DT><STRONG>eval(Expr)</STRONG><DD>
	    Equivalent to Expr.
  
 <DT><STRONG>ConLev: Expr</STRONG><DD>
	    Expr is passed to Gecode at constraint level ConLev.
            ConLev can be gfd_gac, gfd_bc, gfd_vc, gfd.
   </DL>
")).


%---------------------------------------------------------------------

:- comment(integers/1, [
    amode: (integers(-) is det),
    amode: (integers(+) is semidet),
%    template: "integers(?Vars)",
    args: [
	"Vars": "Variable or integer, or a collection of variables"
    ],
    summary: "Vars' domain is the integer numbers (within default bounds).",
    see_also: [_:integers/1],
    kind: [constraint],
%    fail_if: "variables already a non-integer.",
    desc: html("<P>
   Constrain the variables to integer values.  If any variable is a non-domain
   variable, a default domain will be created for it.
")
]).

%---------------------------------------------------------------------

:- comment(is_solver_var/1, [
    amode: (is_solver_var(?) is semidet),
    args: [
	"Term": "A Term"
    ],
    summary: "Succeeds iff Term is an GFD domain variable.",
    fail_if: "Var is not an GFD domain variable.",
    kind:[varq],
    desc: html("<P>
   Test if the term Term is an GFD domain variable.  Succeed if it is, fail
   otherwise.</P>
")
]).

%---------------------------------------------------------------------

:- comment(is_exact_solver_var/1, [
    amode: (is_exact_solver_var(?) is semidet),
    args: [
	"Term": "A Term"
    ],
    summary: "Succeeds iff Term is an GFD domain variable.",
    fail_if: "Var is not an GFD domain variable.",
    kind:[varq],
    desc: html("<P>
   Test if the term Term is an GFD domain variable. This is an alias for
   is_solver_var/1 in GFD.
")
]).

%---------------------------------------------------------------------

:- comment(is_solver_type/1, [
    amode: (is_solver_type(?) is semidet),
    args: [
	"Term": "A Term"
    ],
    summary: "Succeeds iff Term is a GFD domain variable or an integer.",
    fail_if: "Var is not a GFD domain variable or an integer.",
    kind:[varq],
    desc: html("<P>
   Test if the term Term is a GFD domain variable or an integer.
   Succeed if it is, fail otherwise.</P>
")
]).

%---------------------------------------------------------------------

:- comment(get_bounds/3, [
    amode: (get_bounds(?, -, -) is det),
    args: [
	"Var": "A (domain) variable or an integer",
	"Lo":  "Lower bound",
	"Hi":  "Upper bound"
    ],
    summary: "Retrieve the current bounds of Var.",
%    fail_if: "Var is not a variable or an integer.",
    see_also: [get_min/2, get_max/2, 
		get_integer_bounds/3, get_finite_integer_bounds/3,
		get_delta/2, get_median/2],
    kind:[varq],
    desc: html("<P>
   Primitive for retrieving the upper and lower bounds of Var.  Lo and Hi
   return the minimum and maximum (respectively) of the variable's interval.
   If Var has not been declared before, it will be turned into a domain
   variable with default interval.  If Var is an integer, Lo and Hi will
   be set to Var.</P>
")
]).

%---------------------------------------------------------------------

:- comment(get_min/2, [
    amode: (get_min(?, -) is det),
    args: [
	"Var": "A (domain) variable or an integer",
	"Lo":  "Lower bound"
    ],
    summary: "Retrieve the current lower bound of Var.",
%    fail_if: "Var is not a variable or an integer.",
    see_also: [get_bounds/3, 
		get_integer_bounds/3, get_finite_integer_bounds/3],
    kind:[varq],
    desc: html("<P>
   Primitive for retrieving the lower bound of Var.  Lo returns the minimum
   of the variable's interval. If Var has not been declared before, it
   will be turned into a domain variable with default interval.</P>
")
]).

%---------------------------------------------------------------------

:- comment(get_max/2, [
    amode: (get_max(?, -) is det),
    args: [
	"Var": "A (domain) variable or an integer",
	"Hi":  "Upper bound"
    ],
    summary: "Retrieve the current upper bound of Var.",
%    fail_if: "Var is not a variable or an integer.",
    see_also: [get_bounds/3, 
		get_integer_bounds/3, get_finite_integer_bounds/3],
    kind:[varq],
    desc: html("<P>
   Primitive for retrieving the upper bound of Var.  Hi returns the maximum
   of the variable's interval. If Var has not been declared before, it
   will be turned into a domain variable with default interval.</P>
")
]).

%---------------------------------------------------------------------

:- comment(get_integer_bounds/3, [
    amode: (get_integer_bounds(?, -, -) is det),
    args: [
	"Var": "A (domain) variable or an integer (array notation recognised)",
	"Lo":  "Lower bound",
	"Hi":  "Upper bound"
    ],
    summary: "Retrieve the current bounds of Var.",
%    fail_if: "Var is not a variable or an integer.",
    see_also: [get_bounds/3, get_finite_integer_bounds/3],
    kind:[varq],
    desc: html("<P>
   This is provided for compatibility with IC, and is an alias for 
   get_bounds/3.</P>
")
]).

%---------------------------------------------------------------------

:- comment(get_finite_integer_bounds/3, [
    amode: (get_finite_integer_bounds(?, -, -) is det),
    args: [
	"Var": "A (domain) variable or an integer",
	"Lo":  "Lower bound",
	"Hi":  "Upper bound"
    ],
    summary: "Retrieve the current (finite, integral) bounds of Var.",
%    fail_if: "Var is not a variable or a number.",
    see_also: [get_bounds/3, get_integer_bounds/3],
    kind:[varq],
    desc: html("<P>
   This is provided for compatibility with IC, and is an alias for 
   get_bounds/3.</P>
")
]).

%---------------------------------------------------------------------

:- comment(get_domain_size/2, [
    amode: (get_domain_size(?, -) is det),
    args: [
	"Var":   "A domain variable or an integer",
        "Size":  "A variable (or integer)"
    ],
    summary: "Size is the number of integer elements in the GFD domain for Var",
    see_also: [get_delta/2],
    kind:[varq],
    desc: html("<P>
   If Var is an GFD domain variable, Size will be set to the number of 
   integer values in the domain of Var.  If Var is a number, then Size 
   will be set to 1.</P><P>
</P>
"),
   exceptions: [
        5: "Var is neither a GFD variable or integer."
   ],
   fail_if: "The initial value of Size fails to unify with the returned value."
]).

%---------------------------------------------------------------------

:- comment(get_domain/2, [
    amode: (get_domain(?, -) is det),
    args: [
	"Var":    "A domain variable or an integer.",
        "Domain": "A ground representation of the domain of Var."
    ],
    summary: "Returns a ground representation of the current GFD domain of a variable.",
    see_also: [get_domain_as_list/2, get_bounds/3],
    eg:"
[eclipse 8]: X :: [1..5,10], get_domain(X, D).

X = X{[1 .. 5, 10]}
D = [1 .. 5, 10]

",
    kind:[varq],
    desc: html("<P>
   If Var is an integer, Domain will be unified with a singleton list
   with that integer.</P><P>

   If Var is an GFD domain variable with no holes in its domain, Domain will
   be unified with the term Lo..Hi where Lo and Hi are integers
   corresponding to the current lower and upper bounds of Var, respectively.</P><P>
   If Var is an GFD domain variable with holes in its domain, Domain will
   be unified with an ordered list of integers and/or terms Lo..Hi where Lo
   and Hi are integers; in this case the elements of the domain of Var are
   exactly those integers appearing directly in the list or falling within
   any of the intervals Lo..Hi.</P>
"),
   exceptions: [
        5: "Var is neither an IC variable or number."
   ],
   fail_if: "The initial value of Domain fails to unify with the returned value."
]).

%---------------------------------------------------------------------
:- comment(get_domain_as_list/2, [
    amode: (get_domain_as_list(?, -) is det),
    args: [
	"Var":   "A domain variable or a number ",
        "DomainList":  "The domain of Var as a list of elements."
    ],
    summary: "List of all the elements in the GFD domain of Var",
    see_also: [get_domain/2, get_bounds/3],
    eg:"
 [eclipse 9]: X :: [1..5,10], get_domain_as_list(X, D).

X = X{[1 .. 5, 10]}
D = [1, 2, 3, 4, 5, 10]

",      
    kind:[varq],
    desc: html("<P>
   If Var is a GFD domain variable, DomainList will be set to an ordered
   list containing each element in the domain of Var.  If Var is a number,
   then DomainList will be set to a singleton list containing the number.</P>
"),
   exceptions: [
        5: "Var is neither a GFD variable or integer."
   ],
   fail_if: "The initial value of DomainList fails to unify with the returned value."
]).

%---------------------------------------------------------------------

:- comment(get_median/2, [
    amode: (get_median(?, -) is det),
    args: [
	"Var":    "A (domain) variable or an integer",
	"Median": "The median of the domain"
    ],
    summary: "Returns the median of the domain of the GFD domain variable Var.",
    see_also: [get_delta/2, get_bounds/3],
    kind:[varq],
    desc: html("<P>
   Returns the median of the domain of Var, i.e. if there are N values in
   the domain, the N/2'th (rounded down if N is odd) value is returned.
   If Var is not a GFD domain variable, it will be turned into one.
   Note that this is different from the definition used in IC, where the
   median (a float) of the interval is returned. If Var is an integer, the 
   median is unified with that number.
"),
    eg: "\
[eclipse 2]: X :: 10..1000, get_median(X, M).

X = X{10 .. 1000}
M = 500

[eclipse 2]: A :: [2,3,100,1000], get_median(A,M).

A = A{[2, 3, 100, 1000]}
M = 3

[eclipse 2]: get_median(3, M).

M = 3
"
]).

%---------------------------------------------------------------------

:- comment(get_delta/2, [
    amode: (get_delta(?, -) is det),
    args: [
	"Var":   "A domain variable or an integer ",
	"Width": "Width of the interval"
    ],
    summary: "Returns the width of the interval of Var.",
    see_also: [get_median/2, get_bounds/3],
    kind:[varq],
    desc: html("<P>
   Returns the width (Hi - Lo) of the interval of Var. If Var is an integer,
   0 will be returned as the width. If Var is free, then 1.0Inf is
   returned as the width, but Var is not turned into a domain variable.</P>
")
]).

%---------------------------------------------------------------------

:- comment(get_constraints_number/2, [
    amode: (get_constraints_number(?, -) is det),
    args: [
	"Var":   "A domain variable or a term",
	"Number": "Variable (instantiates to a non-negative integer)"
    ],
    summary: "Returns the number of propagators attached to the gecode"
             " variable representing Var.",
    kind:[varq],
    desc: html("<P>
   Returns the number of propagators attached to the gecode variable
   representing Var, This approximates the number of constraints attach
   to the variables, and is known as the degree of the variable in the
   literature. 
</P><P>
   If Var is not a variable, a very large number (1.0Inf) is returned. If
   Var is a variable but not a domain variable, 0 will be returned.
</P><P>
   Note that unlike a native ECLiPSe solver like IC, this is not the number 
   of suspensions on the variable, but is the number of propagators
   attached to the Gecode variable, obtained via the degree() method
   for IntVar. Thus, any constraints implemented at the ECLiPSe level
   will not be included in this count.
</P>
")
]).

%---------------------------------------------------------------------

:- comment(get_weighted_degree/2, [
    amode: (get_weighted_degree(?, -) is det),
    args: [
	"Var":   "A domain variable",
	"WD": "Current wighted degree for variable"
    ],
    summary: "Returns the weighted degree of domain variable Var.",
    kind:[varq],
    see_also:[set_weighted_degree_decay/1, init_weighted_degree/1],
    desc: html("<P>
   Returns the weighted degree for a domain variable. Weighted degree
   is the original name used in the literature for this measure, ans is 
   known as AFC (accumulated failure count) in Gecode, and is usually
   used as a criterion for selecting a variable for labelling.
</p><p>
   The weighted degree is a floating point, 
   due to Gecode's extension of the basic weighted degree measure: weighted
   degree for a variable is defined as the count of the failures so 
   far of the constraints (propagators) associated with the variable, plus
   an initial value that is proportional to the variable's degree (number 
   of associated constraints) -- to give reasonable initial values.
   The degree proportionality can be non-integral but is by default one 
   (i.e. the default initial value for the weighted degree of a
   variable is its degree), but can be changed by init_weighted_degree/1. 
   In addition, the failure count for a constraint can decay if the 
   constraint was not involved in the failure. The rate of decay can be
   set by set_weighted_degree_decay/1 (the default is 1 -- no decay).
</P>
")
]).

%---------------------------------------------------------------------

:- comment(get_regret_lwb/2, [
    amode: (get_regret_lwb(?, -) is det),
    args: [
	"Var":   "A domain variable",
	"Regret": "Regret value"
    ],
    summary: "Returns the regret value for the lower bound of Var.",
    kind:[varq],
    desc: html("<P>
   Returns the regret value for the lower bound of the variable, that is, 
   the magnitude of the difference between the lowest and second lowest value 
   in the variable's domain. This can be used in selecting a variable for 
   labelling.
</P>
")
]).

%---------------------------------------------------------------------

:- comment(get_regret_upb/2, [
    amode: (get_regret_upb(?, -) is det),
    args: [
	"Var":   "A domain variable",
	"Regret": "Regret value"
    ],
    summary: "Returns the regret value for the upper bound of Var.",
    kind:[varq],
    desc: html("<P>
   Returns the regret value for the upper bound of the variable, that is, 
   the magnitude of the difference between the largest and second largest 
   value in the variable's domain. This can be used in selecting a variable 
   for labelling.
</P>
")
]).

%---------------------------------------------------------------------

:- comment(is_in_domain/2, [
    amode: (is_in_domain(++,?) is semidet),
    args: [
        "Val": "An integer",
        "Var": "A domain variable or an integer"
    ],
    summary: "Succeeds iff Val is in the domain of Var",
    exceptions:[5: "Val is not an integer"],
    see_also:[is_in_domain/3],
    kind:[varq],
    desc: html("<P>
   Low level predicate which succeeds when Val is in the domain of Var.
</P>
")
]).

%---------------------------------------------------------------------

:- comment(is_in_domain/3, [
    amode: (is_in_domain(++,?,-) is det),
    args: [
        "Val": "A number",
    	"Var": "A domain variable or an integer",
        "Result": "A variable"
    ],
    summary: "Binds Result to indicate presence of Val in domain of Var",
    exceptions:[5: "Val is not an integer"],
    see_also:[is_in_domain/2],
    kind:[varq],
    desc: html("<P>
   Low level predicate which succeeds when Val is in the domain of Var with
   Result bound to the atom 'yes'.  When Val is not in the domain of Var,
   the predicate succeeds binding Result to the atom 'no'. 
</P>
")
]).

%---------------------------------------------------------------------

:- comment((#::)/2, [
    amode: #::(?, ++),
    template: "?Vars #:: ++Domain",
    args: [
	"Vars":   "Variable (array notation accepted) or collection (a la collection_to_list/2) of variables",
	"Domain": "Domain specification"
    ],
    summary: "Constrain Vars to have the domain Domain.",
    see_also: [(::)/2],
    kind: [constraint],
    desc: html("<P>
   Alias of ::/2. See ::/2 for more details.
</P>
")]).

:- comment((::)/2, [
    amode: ::(?, ++),
    template: "?Vars :: ++Domain",
    args: [
	"Vars":   "Variable (array notation accepted) or collection (a la collection_to_list/2) of variables",
	"Domain": "Domain specification"
    ],
    summary: "Constrain Vars to have the domain Domain.",
    see_also: [integers/1, _:(::)/2, (::)/3, (#::)/2],
    kind: [constraint],
    desc: html("<P>
   Constrains Vars to take only values from the domain specified by Domain.  
   Vars may be a variable or a collection of variables (as accepted by 
   collection_to_list/2).  Domain can be specified as a simple range Lo .. Hi, 
   or as a list of sub-ranges and/or individual elements. Each element
   of the specification is an integer, or is a ground expression that evaluates
   to an integer. All domain elements must be integers within the range allowed 
   by gecode. 
</P>
"),
    eg: "\
[eclipse 2]: X :: 0..1.

X = X{[0, 1]}

[eclipse 2]: X :: -1..5.

X = X{[-1 .. 5]}

[eclipse 2]: X :: 0.0..1.0.
type error in X :: 0.0 .. 1.0
Abort

[eclipse 2]: [X,Y] :: [1..10, -1, 0, 7, 21].

X = X{[-1 .. 10, 21]}
Y = Y{[-1 .. 10, 21]}
"
]).

:- comment((::)/3, [
    amode: ::(?, ++, ?),
    template: "::(?Var, ++Domain, ?Bool)",
    args: [
	"Var":    "(Domain) variable (array notation accepted)",
	"Domain": "Domain specification",
        "Bool":   "Reified truth value  (array notation accepted)"
    ],
    summary: "Reflect into Bool the truth of Var having the domain Domain.",
    see_also: [_:(::)/3, (::)/2],
    kind: [constraint],
    desc: html("<P>
   Provides a reified form of the ::/2 domain assignment predicate.  This
   reified ::/3 is defined only to work for one variable (unlike ::/2).
<P>
   For a single variable, V, the Bool will be instantiated to 0 if the
   current domain of V does not intersect with Domain.  It will be
   instantiated to 1 iff the domain of V is wholly contained within Domain.
   Finally the Boolean will remain a domain variable in the range 0..1, if
   neither of the above two conditions hold.
<P>
   Instantiating Bool to 1, will cause the constraint to behave exactly like
   ::/2.  Instantiating Bool to 0 will cause Domain to be excluded from the
   domain of the variable.
<P>
   Note that calling the reified form of :: will result in the Variable
   becoming a domain variable, even if Bool is uninstantiated.
<P>
   Further note that, like other reified predicates, :: can be used infix in
   a GFD  expression, e.g. B #= (X :: [1..10]) is equivalent to
   ::(X, [1..10], B).
</P>
"),
    eg: "\
[eclipse 2]: ::(X, [1..10, 12..30], 1).

X = X{[1 .. 10, 12 .. 30]}


[eclipse 2]: ::(X, [1..10, 12..30], 0).

X = X{[-1000000 .. 0, 11, 31 .. 1000000]}

[eclipse 2]: ::(X, [1..10, 12..30], B).

X = X{[-1000000 .. 1000000]}
B = B{[0, 1]}

[eclipse 2]: gfd:( B #= (X :: [1..10, 12..30])).

B = B{[0, 1]}
X = X{[-1000000 .. 1000000]}

"
]).

:- comment((#::)/3, [
    amode: #::(?, ++, ?),
    template: "#::(?Var, ++Domain, ?Bool)",
    args: [
	"Var":    "(Domain) variable (array notation accepted)",
	"Domain": "Domain specification",
        "Bool":   "Reified truth value (array notation accepted)"
    ],
    summary: "Reflect into Bool the truth of Var having the domain Domain.",
    see_also: [_:(#::)/3, (#::)/2],
    kind: [constraint],
    desc: html("<P>
  An alias for ::/3. See ::/3 for more details.</P>
</P>
")
]).

%---------------------------------------------------------------------

:- comment((#=)/2, [
    amode: #=(?, ?),
    template: "<ConsistencyModule:> ?ExprX #= ?ExprY",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression"
    ],
    summary: "ExprX is equal to ExprY.",
    see_also: [(#<)/2, (#=<)/2, (#>=)/2, (#>)/2, (#\=)/2,
               (#=)/3, _:(#=)/2],
    kind: [constraint:[extra:[gccat:eq]]],
    eg: "
[eclipse 28]: A :: [1,3,5,7], A #= B.

A = A{[1, 3, 5, 7]}
B = B{[1 .. 7]}

[eclipse 29]: A :: [1,3,5,7], gfd_gac: (A #= B).

A = A{[1, 3, 5, 7]}
B = B{[1, 3, 5, 7]}

[eclipse 30]:  A :: [1,3,5,7], gfd_gac: ( A #= B + 1).

A = A{[1, 3, 5, 7]}
B = B{[0, 2, 4, 6]}


",
    desc: html("<P>
   Constrains ExprX and ExprY to be equal.  Also constrains all variables
   appearing in ExprX and ExprY to be domain variables and checks that all 
   constants and ground sub-expressions are integers.</P><P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: gfd_bc
   for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. Note that some operators (integer division, modulus)
   only support bounds consistency and will be propagated with bounds
   consistency even when posted with gfd_gac.
</P><P>
   This constraint is implemented using Gecode's MiniModel's rel() for
   both integer and boolean expressions, with sub-expressions/constraints
   not supported by MiniModel factored out and posted as auxiliary 
   constraints.
</P><P>
   A more restricted version of this constraint is defined in the 
   global constraint catalog as 'eq'. Here the constraint is defined
   between two domain variables rather than two expressions.
</P>
")
]).

:- comment((#=)/3, [
    amode: #=(?, ?, ?),
    template: "<ConsistencyModule:> #=(?ExprX, ?ExprY, ?Bool)",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression",
	"Bool": "Reified truth value of the constraint"
    ],
    summary: "Reified ExprX is equal to ExprY.",
    see_also: [(#<)/3, (#=<)/3, (#>=)/3, (#>)/3, (#\=)/3,
               (#=)/2, _:(#=)/3],
    kind: [constraint],
    desc: html("<P>
   This predicate is a reified constraint: it succeeds if and only if the
   truth value of its associated constraint (the constraint with arity one
   less and the same arguments except for B) is equal to B, where the value
   0 means false and 1 true.  This constraint can be used both to test the
   validity of the associated constraint (entailment test) and to impose
   this constraint or its negation.  For the former, B will be instantiated
   as soon as either the associated constraint or its negation is subsumed
   by the current state of its domain variables.  For the latter, when B is
   instantiated, then depending on its value, either the associated
   constraint or its negation will be imposed on its arguments.</P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: gfd_bc
   for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. Note that some operators (integer division, modulus)
   only support bounds consistency and will be propagated with bounds
   consistency even when posted with gfd_gac.
</P><P>
   This constraint is implemented using Gecode's MiniModel's rel() for
   both integer and boolean expressions, with sub-expressions/constraints
   not supported by MiniModel factored out and posted as auxiliary 
   constraints.
</P>
")
]).

%---------------------------------------------------------------------

:- comment((#>=)/2, [
    amode: #>=(?, ?),
    template: "<ConsistencyModule:> ?ExprX #>= ?ExprY",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression"
    ],
    summary: "ExprX is greater than or equal to ExprY.",
    see_also: [(#<)/2, (#=<)/2, (#=)/2, (#>)/2, (#\=)/2,
               (#>=)/3, _:(#>=)/2],
    kind: [constraint:[extra:[gccat:geq]]],
    desc: html("<P>
   Constrains ExprX to be greater than or equal to ExprY.  Also constrains
   all variables appearing in ExprX and ExprY to be domain variables and 
   checks that all constants and ground sub-expressions are integers.</P><P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: gfd_bc
   for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. Note that some operators (integer division, modulus)
   only support bounds consistency and will be propagated with bounds
   consistency even when posted with gfd_gac.
</P><P>
   This constraint is implemented using Gecode's MiniModel's rel() for
   both integer and boolean expressions, with sub-expressions/constraints
   not supported by MiniModel factored out and posted as auxiliary 
   constraints.
</P><P>
   A more restricted version of this constraint is defined in the 
   global constraint catalog as 'geq'. Here the constraint is defined
   between two domain variables rather than two expressions.
</P>
")
]).

:- comment((#>=)/3, [
    amode: #>=(?, ?, ?),
    template: "<ConsistencyModule:> #>=(?ExprX, ?ExprY, ?Bool)",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression",
	"Bool": "Reified truth value of the constraint"
    ],
    summary: "Reified ExprX is greater than or equal to ExprY.",
    see_also: [(#<)/3, (#=<)/3, (#=)/3, (#>)/3, (#\=)/3,
               (>=)/3, (#>=)/2, _:(#>=)/3],
    kind: [constraint],
    desc: html("<P>
   This predicate is a reified constraint: it succeeds if and only if the
   truth value of its associated constraint (the constraint with arity one
   less and the same arguments except for B) is equal to B, where the value
   0 means false and 1 true.  This constraint can be used both to test the
   validity of the associated constraint (entailment test) and to impose
   this constraint or its negation.  For the former, B will be instantiated
   as soon as either the associated constraint or its negation is subsumed
   by the current state of its domain variables.  For the latter, when B is
   instantiated, then depending on its value, either the associated
   constraint or its negation will be imposed on its arguments.</P><P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: gfd_bc
   for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. Note that some operators (integer division, modulus)
   only support bounds consistency and will be propagated with bounds
   consistency even when posted with gfd_gac.
</P><P>
   This constraint is implemented using Gecode's MiniModel's rel() for
   both integer and boolean expressions, with sub-expressions/constraints
   not supported by MiniModel factored out and posted as auxiliary 
   constraints.
</P>
")
]).

%---------------------------------------------------------------------

:- comment((#=<)/2, [
    amode: #=<(?, ?),
    template: "<ConsistencyModule:> ?ExprX #=< ?ExprY",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression"
    ],
    summary: "ExprX is less than or equal to ExprY.",
    see_also: [(#<)/2, (#=)/2, (#>=)/2, (#>)/2, (#\=)/2,
               (#=<)/3, _:(#=<)/2],
    kind: [constraint:[extra:[gccat:leq]]],
    desc: html("<P>
   Constrains ExprX to be less than or equal to ExprY.  Also constrains all
   variables appearing in ExprX and ExprY to be integral and checks that all
   constants are integers.</P><P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: gfd_bc
   for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. Note that some operators (integer division, modulus)
   only support bounds consistency and will be propagated with bounds
   consistency even when posted with gfd_gac.
</P><P>
   This constraint is implemented using Gecode's MiniModel's rel() for
   both integer and boolean expressions, with sub-expressions/constraints
   not supported by MiniModel factored out and posted as auxiliary 
   constraints.
</P><P>
   A more restricted version of this constraint is defined in the 
   global constraint catalog as 'leq'. Here the constraint is defined
   between two domain variables rather than two expressions.
</P>
")
]).
:- comment((#=<)/3, [
    amode: #=<(?, ?, ?),
    template: "<ConsistencyModule:> #=<(?ExprX, ?ExprY, ?Bool)",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression",
	"Bool": "Reified truth value of the constraint"
    ],
    summary: "Reified ExprX is less than or equal to ExprY.",
    see_also: [(#<)/3, (#=)/3, (#>=)/3, (#>)/3, (#\=)/3,
               (#=<)/2, _:(#=<)/3],
    kind: [constraint],
    desc: html("<P>
   This predicate is a reified constraint: it succeeds if and only if the
   truth value of its associated constraint (the constraint with arity one
   less and the same arguments except for B) is equal to B, where the value
   0 means false and 1 true.  This constraint can be used both to test the
   validity of the associated constraint (entailment test) and to impose
   this constraint or its negation.  For the former, B will be instantiated
   as soon as either the associated constraint or its negation is subsumed
   by the current state of its domain variables.  For the latter, when B is
   instantiated, then depending on its value, either the associated
   constraint or its negation will be imposed on its arguments.</P><P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: gfd_bc
   for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. Note that some operators (integer division, modulus)
   only support bounds consistency and will be propagated with bounds
   consistency even when posted with gfd_gac.
</P><P>
   This constraint is implemented using Gecode's MiniModel's rel() for
   both integer and boolean expressions, with sub-expressions/constraints
   not supported by MiniModel factored out and posted as auxiliary 
   constraints.
</P>
")
]).

%---------------------------------------------------------------------

:- comment((#>)/2, [
    amode: #>(?, ?),
    template: "<ConsistencyModule:> ?ExprX #> ?ExprY",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression"
    ],
    summary: "ExprX is strictly greater than ExprY.",
    see_also: [(#<)/2, (#=<)/2, (#=)/2, (#>=)/2, (#\=)/2,
               (#>)/3, _:(#>)/2],
    kind: [constraint:[extra:[gccat:gt]]],
    desc: html("<P>
   Constrains ExprX to be greater than ExprY.  Also constrains all variables
   appearing in ExprX and ExprY to be domain variables and checks that all 
   constants and ground sub-expressions are integers.</P><P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: gfd_bc
   for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. Note that some operators (integer division, modulus)
   only support bounds consistency and will be propagated with bounds
   consistency even when posted with gfd_gac.
</P><P>
   This constraint is implemented using Gecode's MiniModel's rel() for
   both integer and boolean expressions, with sub-expressions/constraints
   not supported by MiniModel factored out and posted as auxiliary 
   constraints.
</P><P>
   A more restricted version of this constraint is defined in the 
   global constraint catalog as 'gt'. Here the constraint is defined
   between two domain variables rather than two expressions.
</P>
")
]).
:- comment((#>)/3, [
    amode: #>(?, ?, ?),
    template: "<ConsistencyModule:> #>(?ExprX, ?ExprY, ?Bool)",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression",
	"Bool": "Reified truth value of the constraint"
    ],
    summary: "Reified ExprX is strictly greater than ExprY.",
    see_also: [(#<)/3, (#=<)/3, (#=)/3, (#>=)/3, (#\=)/3,
               (#>)/2, _:(#>)/3],
    kind: [constraint],
    desc: html("<P>
   This predicate is a reified constraint: it succeeds if and only if the
   truth value of its associated constraint (the constraint with arity one
   less and the same arguments except for B) is equal to B, where the value
   0 means false and 1 true.  This constraint can be used both to test the
   validity of the associated constraint (entailment test) and to impose
   this constraint or its negation.  For the former, B will be instantiated
   as soon as either the associated constraint or its negation is subsumed
   by the current state of its domain variables.  For the latter, when B is
   instantiated, then depending on its value, either the associated
   constraint or its negation will be imposed on its arguments.</P><P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: gfd_bc
   for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. Note that some operators (integer division, modulus)
   only support bounds consistency and will be propagated with bounds
   consistency even when posted with gfd_gac.
</P><P>
   This constraint is implemented using Gecode's MiniModel's rel() for
   both integer and boolean expressions, with sub-expressions/constraints
   not supported by MiniModel factored out and posted as auxiliary 
   constraints.
</P>
")
]).

%---------------------------------------------------------------------

:- comment((#<)/2, [
    amode: #<(?, ?),
    template: "<ConsistencyModule:> ?ExprX #< ?ExprY",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression"
    ],
    summary: "ExprX is less than ExprY.",
    see_also: [(#=<)/2, (#=)/2, (#>=)/2, (#>)/2, (#\=)/2,
               (#<)/3, _:(#<)/2],
    kind: [constraint:[extra:[gccat:lt]]],
    desc: html("<P>
   Constrains ExprX to be less than ExprY.  Also constrains all variables
   appearing in ExprX and ExprY to be domain variables and checks that all 
   constants and ground sub-expressions are integers.</P><P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: gfd_bc
   for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. Note that some operators (integer division, modulus)
   only support bounds consistency and will be propagated with bounds
   consistency even when posted with gfd_gac.
</P><P>
   This constraint is implemented using Gecode's MiniModel's rel() for
   both integer and boolean expressions, with sub-expressions/constraints
   not supported by MiniModel factored out and posted as auxiliary 
   constraints.
</P><P>
   A more restricted version of this constraint is defined in the 
   global constraint catalog as 'lt'. Here the constraint is defined
   between two domain variables rather than two expressions.
</P>
")
]).

:- comment((#<)/3, [
    amode: #<(?, ?, ?),
    template: "<ConsistencyModule:> #<(?ExprX, ?ExprY, ?Bool)",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression",
	"Bool": "Reified truth value of the constraint"
    ],
    summary: "Reified ExprX is less than ExprY.",
    see_also: [(#=<)/3, (#=)/3, (#>=)/3, (#>)/3, (#\=)/3,
               (<)/3, (#<)/2, _:(#<)/3],
    kind: [constraint],
    desc: html("<P>
   This predicate is a reified constraint: it succeeds if and only if the
   truth value of its associated constraint (the constraint with arity one
   less and the same arguments except for B) is equal to B, where the value
   0 means false and 1 true.  This constraint can be used both to test the
   validity of the associated constraint (entailment test) and to impose
   this constraint or its negation.  For the former, B will be instantiated
   as soon as either the associated constraint or its negation is subsumed
   by the current state of its domain variables.  For the latter, when B is
   instantiated, then depending on its value, either the associated
   constraint or its negation will be imposed on its arguments.</P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: gfd_bc
   for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. Note that some operators (integer division, modulus)
   only support bounds consistency and will be propagated with bounds
   consistency even when posted with gfd_gac.
</P><P>
   This constraint is implemented using Gecode's MiniModel's rel() for
   both integer and boolean expressions, with sub-expressions/constraints
   not supported by MiniModel factored out and posted as auxiliary 
   constraints.
</P>
")
]).

%---------------------------------------------------------------------

:- comment((#\=)/2, [
    amode: #\=(?, ?),
    template: "<ConsistencyModule:> ?ExprX #\\= ?ExprY",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression"
    ],
    summary: "ExprX is not equal to ExprY.",
    see_also: [(#<)/2, (#=<)/2, (#=)/2, (#>=)/2, (#>)/2,
               (#\=)/3, _:(#\=)/2],
    kind: [constraint:[extra:[gccat:neq]]],
    desc: html("<P>
   Constrains ExprX to be not equal to ExprY.  Also constrains all variables
   appearing in ExprX and ExprY to be domain variable and checks that all 
   constants and ground sub-expressions are integers.</P><P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: gfd_bc
   for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. Note that some operators (integer division, modulus)
   only support bounds consistency and will be propagated with bounds
   consistency even when posted with gfd_gac.
</P><P>
   This constraint is implemented using Gecode's MiniModel's rel() for
   both integer and boolean expressions, with sub-expressions/constraints
   not supported by MiniModel factored out and posted as auxiliary 
   constraints.
</P><P>
   A more restricted version of this constraint is defined in the 
   global constraint catalog as 'neq'. Here the constraint is defined
   between two domain variables rather than two expressions.
</P>
")
]).

:- comment((#\=)/3, [
    amode: #\=(?, ?, ?),
    template: "<ConsistencyModule:> #\\=(?ExprX, ?ExprY, ?Bool)",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression",
	"Bool": "Reified truth value of the constraint"
    ],
    summary: "Reified ExprX is not equal to ExprY.",
    see_also: [(#<)/3, (#=<)/3, (#=)/3, (#>=)/3, (#>)/3,
               (=\=)/3, (#\=)/2, _:(#\=)/3],
    kind: [constraint],
    desc: html("<P>
   This predicate is a reified constraint: it succeeds if and only if the
   truth value of its associated constraint (the constraint with arity one
   less and the same arguments except for B) is equal to B, where the value
   0 means false and 1 true.  This constraint can be used both to test the
   validity of the associated constraint (entailment test) and to impose
   this constraint or its negation.  For the former, B will be instantiated
   as soon as either the associated constraint or its negation is subsumed
   by the current state of its domain variables.  For the latter, when B is
   instantiated, then depending on its value, either the associated
   constraint or its negation will be imposed on its arguments.</P><P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: gfd_bc
   for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. Note that some operators (integer division, modulus)
   only support bounds consistency and will be propagated with bounds
   consistency even when posted with gfd_gac.
</P><P>
   This constraint is implemented using Gecode's MiniModel's rel() for
   both integer and boolean expressions, with sub-expressions/constraints
   not supported by MiniModel factored out and posted as auxiliary 
   constraints.
</P>
")
]).


%---------------------------------------------------------------------

:- comment(indomain/1, [
    amode: (indomain(?) is nondet),
    args: [
    	"Var": "A domain variable or an integer"
    ],
    kind: [search],
    summary: "Instantiates a domain GFD variable to an element of its domain.",
    see_also: [try_value/2,gfd_search:indomain/2, labeling/1, (::)/2, _:indomain/1],
    desc: html("<P>
   Simple predicate for instantiating a GFD domain variable to an element
   of its domain.  It starts with the smallest element, and upon
   backtracking tries successive elements until the entire domain has been
   explored, at which point the predicate fails.</P><P>

   If Var is already a ground integer, then this predicate simply succeeds
   exactly once without leaving a choicepoint.</P><P>

   Note that this predicate is an alias for using the indomain_min method
   of try_value/2.
</P>
")
]).

%---------------------------------------------------------------------

:- comment(labeling/1, [
    amode: (labeling(+) is nondet),
    args: [
    	"Vars": "A collection (a la collection_to_list/2) of integer IC variables or integers"
    ],
    summary: "Instantiates all variables in a collection to elements of their domains.",
    see_also: [gfd_search:indomain/2, _:labeling/1, collection_to_list/2],
    kind: [search],
    desc: html("<P>
   Simple predicate for instantiating a collection of GFD domain variables
   to elements of their domains.  (Integers are also allowed in the
   collection; they are effectively ignored.)  The variables are
   instantiated in the order in which they appear in the collection; the
   implementation is essentially:
<PRE>
	labeling(Vars) :-
		collection_to_list(Vars, List),
                gfd_update,
		( foreach(Var,List) do
		    indomain(Var,min)
		).
</PRE></P>
   Note that labeling performs the search in ECLiPSe, but it uses
   indomain/2 with min, which is optimised for use with Gecode, and a 
   gfd_update before the labeling starts to ensure that no recomputation
   will not be done for events before the labeling starts.
</P>
")
]).

%---------------------------------------------------------------------
/*
:- comment(labeling/3, [
    amode: (labeling(+,+,+) is nondet),
    args: [
    	"Vars": "A collection (a la collection_to_list/2) of integer IC variables or integers",
      "Select" :  "a predefined variable selection method.",
      "Choice" :  "a predefine value choice method "
          ],
    summary: "Instantiates all variables in a collection to elements"
             " of their domains according to Select and Choice.",
    see_also: [indomain/2, select_var/5,labeling/1, collection_to_list/2],
    kind: [search],
    desc: html("<P>
   Simple predicate for instantiating a collection of GFD domain variables
   to elements of their domains.  (Integers are also allowed in the
   collection; they are effectively ignored.)  The order the variables are
   instantiated is according to Select, and the value a variable is 
   instantiated to is according to Choice.
</P><P>
   This predicate is a combination of select_var/5 and indomain/2, along with
   a gfd_update before the start of labeling to ensure that only
   changes during the labeling will be recomputed. 
</P><P>
   This predicate is provided to allow a more flexible GFD-specific 
   labeling procedure where the search is performed in ECLiPSe -- 
   GFD's search/6 performs the search in Gecode, and search/6 in
   lib(gfd_search) uses the generic search code and does not use
   GFD-specific features (and will thus be less efficient). 
</P>
")
]).
*/
%---------------------------------------------------------------------

:- comment(gfd_update/0, [
        summary: "Update the parent Gecode space to the current"
                 " state.",
        desc: html("<P>
   This is a low-level primitive that updates (if needed) the parent cloned
   Gecode space to the current state. The parent clone is from where
   recomputation is performed, so updating the parent will reduce the
   amount of recomputation done during subsequent backtracking. Note 
   that the old parent is discarded and replaced by the updated parent
   if possible.
</P><P>
   GFD handles the cloning of Gecode spaces automatically, and the user
   does not normally have to deal with cloning explicitly. However, if
   the cloning distance is set very high, so that the system does
   mostly recomputation instead of copying from clones, it is possible
   for GFD performance to suffer significantly if the changes to set-up
   a problem before the search has to be recomputed. This problem does
   not occur in Gecode when using Gecode's search engine, because the space
   is always cloned before the start of the search. As the search in
   ECLiPSe is not normally distinguished from the model setup, GFD
   cannot automatically ensure that the space is cloned before the
   start of search (if the search is performed in ECLiPSe). 
</P><P>
   The update is only done if the current state is different from the 
   parent's, and the current Gecode state is stable (i.e. fully propagated).
   If the computation since the parent is deterministic, the old
   parent will be discarded and replaced by the new parent.
</P><P>
   The intended use for this predicate is just before the search
   starts in the user's program, if the search is performed in ECLiPSe. 
   Note that gfd_update is included in GFD's labeling/1 and labeling/3, 
   and so if you use these, you do not need to use gfd_update. Also, this 
   predicate will only be useful if the cloning distance is set very high,
   and with default settings it should not be needed.
")
]).

%---------------------------------------------------------------------

:- comment(alldifferent/1, [
    amode: alldifferent(+),
    template: "<ConsistencyModule:> alldifferent(+Vars)",
    args: [
    	"Vars": "A collection (a la collection_to_list/2) of variables or integers"
    ],
    summary: "All elements of Vars are different.",
    see_also: [alldifferent_cst/2,_:alldifferent/1, collection_to_list/2],
    kind: [constraint:[extra:[gccat:alldifferent]]],
    desc: html("<P>
   Constrains all elements of a collection to be different from each other.
   Semantically, all elements of the collection are pairwise different.
</p><p>
   This constraint is also known as alldifferent_cst in the global constraints 
   catalog, and is implemented using Gecode's distinct() constraint.</P><P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_vc for value consistency (naive), gfd_bc for bounds consistency, 
   and gfd_gac for domain (generalised arc) consistency. 
</P>
")
]).

%---------------------------------------------------------------------

:- comment(alldifferent_cst/2, [
    amode: alldifferent_cst(+,++),
    template: "<ConsistencyModule:> alldifferent_cst(+Vars,++Offsets)",
    args: [
    	"Vars": "A collection (a la collection_to_list/2) of variables or integers",
        "Offsets": "A collection (a la collection_to_list/2) of integers, with"
                   " the same cardinality as Vars."
    ],
    summary: "The values of each element plus corresponding offset are pair-wised different.",
    see_also: [alldifferent/1, collection_to_list/2],
    kind: [constraint:[extra:[gccat:alldifferent_cst]]],
    desc: html("<P>
   Constrains all elements of Vars plus its corresponding offset value in
   Offset to be different. That is, 
<PRE>
        Vari + Offseti #\\= Varj + Offsetj, i #\\= j
</PRE>
   where Vari, Offseti are the i'th element of Vars and Offsets, and
   Varj, Offsetj are the j'th element.</P><P>

   This constraint is also known as alldifferent_cst in the global constraints 
   catalog, and is implemented using Gecode's distinct() constraint.</P><P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_vc for value consistency (naive), gfd_bc for bounds consistency, 
   and gfd_gac for domain (generalised arc) consistency.</P><P> 
")
]).

%---------------------------------------------------------------------

:- comment(nvalues/3, [
    summary:"Constrains N, the number of distinct values assigned to "
            "Collection to satisfy the relation N Rel Limit.",
    amode:nvalues(+,+,?),
    args:[
	"Collection": "Collection of integers or (domain) variables.",
        "RelOp": "One of the atom: #>, #>=, #<, #=<, #=, #\\=",
	"Limit":"Variable or integer"
    ],
    kind: [constraint:[extra:[gccat:nvalues]]],
    eg:"\
[eclipse 21]: nvalues([4,5,5,4,1,5], (#=), N).

N = 3

[eclipse 22]: nvalues([A,B,C,D], (#>), N).

A = A{[-1000000 .. 1000000]}
B = B{[-1000000 .. 1000000]}
C = C{[-1000000 .. 1000000]}
D = D{[-1000000 .. 1000000]}
N = N{[-1000000 .. 3]}

[eclipse 23]: nvalues([A,B,C,D], (#=), N).

A = A{[-1000000 .. 1000000]}
B = B{[-1000000 .. 1000000]}
C = C{[-1000000 .. 1000000]}
D = D{[-1000000 .. 1000000]}
N = N{[1 .. 4]}

",
    
    desc:html("<P>\
  Constrains N, the number of distinct values assigned to Collection
  to satisfy the relation N Rel Limit.
</P><P>
  Rel can be one of #&gt;, #&gt;=, #&lt;, #=&lt;, #=, #\\= (or equivalently,
  &gt;, &gt;=, &lt;, =&lt;, =, \\=).
</P><P>
  Any input variables which are not already domain variable will be turned 
  into domain variables with default bounds.
</P><P>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</P><P>
  This constraint is also known as nvalues in the global constraint catalog. 
  It is implemented by Gecode's nvalue() constraint.
") 
]).

%---------------------------------------------------------------------

:- comment(all_le/2, [
    summary:"Constrains all in Collection to be less than or equal to Y.",
    template: "<ConsistencyModule:> all_le(?Collection,?Y)",
    amode:all_le(+,?),
    args:[
        "Collection":"Collection of integers
 or (domain) variables",
        "Y":"An integer or (domain) variable (array notation accepted)"
    ],
    kind: [constraint:[extra:[gccat:[arith] ]]],
    eg: "\
[eclipse 2]: all_le([X,Y,Z],3).

X = X{[-1000000 .. 3]}
Y = Y{[-1000000 .. 3]}
Z = Z{[-1000000 .. 3]}

[eclipse 3]: [X,Y] :: 1..10, Z :: 2..5, A :: [1,3..5], all_le([X,Y,Z], A).


X = X{[1 .. 5]}
Y = Y{[1 .. 5]}
Z = Z{[2 .. 5]}
A = A{[3 .. 5]}

[eclipse 4]: all_le([2,3,4], 3).   % fail

",
    desc:html("\
   Constrain every element in Collection to be less than or equal to  Y.
   </P><P> 
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised 
   arc) consistency. 
</P><P>
   The constraint is known as arith (with the less than or equal
   relation) in the Global Constraint Catalog.This constraint
   is implemented using Gecode's rel() constraint.
"),
    see_also: [(#=<)/2,all_lt/2,all_gt/2,all_ge/2,all_ne/2,all_eq/2]
]).

%---------------------------------------------------------------------

:- comment(all_lt/2, [
    summary:"Constrains Collection to be less than Y.",
    template: "<ConsistencyModule:> all_lt(?Collection,?Y)",
    amode:all_lt(+,?),
    args:[
        "Collection":"Collection of integers or (domain) variables",
        "Y":"An integer or (domain) variable (array notation accepted)"
    ],
    kind: [constraint:[extra:[gccat:[arith] ]]],
    eg: "\
[eclipse 16]:   all_lt([A,B,C,D], 3).

A = A{[-1000000 .. 2]}
B = B{[-1000000 .. 2]}
C = C{[-1000000 .. 2]}
D = D{[-1000000 .. 2]}

[eclipse 17]: all_lt([0,1,2], 3).     % succeeds

[eclipse 18]: all_lt([0,1,2,3], 3).   % fails

",
    desc:html("\
   Constrains every element in Collection to be less than Y.
   </P><P> 
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised 
   arc) consistency. 
</P><P>
   This constraint is known as arith (with the less than relation) in 
   the Global Constraint Catalog. This constraint is
   implemented using Gecode's rel() constraint.
"),
    see_also: [(#=<)/2,all_le/2,all_gt/2,all_ge/2,all_ne/2,all_eq/2]
]).

%---------------------------------------------------------------------

:- comment(all_ge/2, [
    summary:"Constrains Collection to be greater than or equal to Y.",
    template: "<ConsistencyModule:> all_ge(?Collection,?Y)",
    args:[
        "Collection":"Collection of integers or (domain) variables",
        "Y":"An integer or (domain) variable (array notation accepted)"
    ],
    amode:all_ge(+,?),
    kind: [constraint:[extra:[gccat:[arith] ]]],
    eg:"\
[eclipse 34]: [X,Y,Z] :: 1..10, all_ge([X,Y,Z], 5).

X = X{[5 .. 10]}
Y = Y{[5 .. 10]}
Z = Z{[5 .. 10]}

[eclipse 35]: [X,Y,Z] :: 1..10, all_ge([X,Y,Z], A).

X = X{[1 .. 10]}
Y = Y{[1 .. 10]}
Z = Z{[1 .. 10]}
A = A{[-1000000 .. 10]}

[eclipse 36]: all_ge([3,4,5],3).            % succeed

[eclipse 37]: all_ge([2,3,4,5],3).          % fail

",
    desc:html("\
   Constrains every element in Collection to be greater than or equal
   to Y.
   </P><P> 
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised 
   arc) consistency. 
</P><P>
   This constraint is known as arith (with the greater than or equal to 
   relation) in the Global Constraint Catalog, and is implemented using
   Gecode's rel() constraint.
"),
    see_also: [(#>=)/2,all_lt/2,all_gt/2,all_le/2,all_ne/2,all_eq/2]
]).

%---------------------------------------------------------------------

:- comment(all_gt/2, [
    summary:"Constrains Collection to be greater than Y.",
    template: "<ConsistencyModule:> all_gt(?Collection,?Y)",
    amode:all_gt(+,?),
    args:[
        "Collection":"Collection of integers or (domain) variables",
        "Y":"An integer or (domain) variable (array notation accepted)"
    ],
    kind: [constraint:[extra:[gccat:[arith] ]]],
    eg:"\
[eclipse 27]: all_gt([4,5,6,7], 4).      % succeed

[eclipse 28]: all_gt([5,6,7], 4).        % fail

",
    desc:html("\
   Constrains every element in Collection to be greater than Y.
   </P><P> 
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised 
</P><P>
   This constraint is known as arith (with the greater than relation)
   in the Global Constraint Catalog, and is implemented using
   Gecode's rel() constraint.
"),
    see_also: [(#<)/2,all_le/2,all_lt/2,all_ge/2,all_ne/2,all_eq/2]
]).

%---------------------------------------------------------------------

:- comment(all_ne/2, [
    summary:"Constrains Collection to be not equal to Y.",
    template: "<ConsistencyModule:> all_ne(?Collection,?Y)",
    amode:all_ne(+,?),
    args:[
        "Collection":"Collection of integers or (domain) variables",
        "Y":"An integer or (domain) variable (array notation accepted)"
    ],
    kind: [constraint:[extra:[gccat:[arith] ]]],
    eg:"\
[eclipse 45]:  X :: 1..10, all_ne([1,3,4,5], X).

X = X{[2, 6 .. 10]}

[eclipse 46]: all_ne([1,3,4,5], 2).       % succeed

[eclipse 47]: all_ne([1,3,4,5], 4).       % fail

",
    desc:html("\
   Constrains every element in Collection to be greater than or equal
   to Y.
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised 
   arc) consistency. 
</P><P>
   This constraint is known as arith (with the not equal to relation)
   in the Global Constraint Catalog, and is implemented using
   Gecode's rel() constraint.
"),
    see_also: [(#=<)/2,all_le/2,all_gt/2,all_ge/2,all_lt/2,all_eq/2]
]).

%---------------------------------------------------------------------

:- comment(all_eq/2, [
    summary:"Constrains Collection to be equal to Y.",
    template: "<ConsistencyModule:> all_eq(?Collection,?Y)",
    amode:all_eq(+,?),
    args:[
        "Collection":"Collection of integers or (domain) variables",
        "Y":"An integer or (domain) variable (array notation accepted)"
    ],
    kind: [constraint:[extra:[gccat:[arith] ]]],
    eg:"\
[eclipse 51]: all_eq([1,2,3,5], X).     % fail

[eclipse 52]: all_eq([1,1,1,1], X).

X = 1

[eclipse 53]: [X,Y] :: 0..10, Z :: 9..15, all_eq([X,Y,Z], A).

X = X{[9, 10]}
Y = Y{[9, 10]}
Z = Z{[9, 10]}
A = A{[9, 10]}

",
    desc:html("\
   Constrains every element in Collection to be equal to Y.
   </P><P> 
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised 
   arc) consistency. 
</P><P>
   This constraint is known as arith (with the equal to relation)
   in the Global Constraint Catalog, and is implemented using
   Gecode's rel() constraint.
"),
    see_also: [(#=<)/2,all_le/2,all_gt/2,all_ge/2,all_lt/2,all_ne/2]
]).

%---------------------------------------------------------------------

:- comment(divmod/4, [
    summary:"Constrains Q to X // Y, and M to X mod Y.",
    template: "<ConsistencyModule:> divmod(?X,?Y,?Q,?M)",
    amode:divmod(?,?,?,?),
    args:[
        "X":"An integer or (domain) variable (array notation accepted)",
        "Y":"An integer or (domain) variable (array notation accepted)",
        "Q":"An integer or (domain) variable (array notation accepted)",
        "M":"An integer or (domain) variable (array notation accepted)"
    ],
    kind: [constraint],
    desc:html("\
   Constrains Q to be the integer quotient of X and Y (X // Y), and M to
   be the modulus of X and Y (X mod Y). Q is rounded towards 0.
   </P><P> 
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency.
</P>
")
]).


%---------------------------------------------------------------------

:- comment(min_index/2, [
    summary:"Index is constrained to the index(es) of the variable(s) with the minimum value in Collection",
    amode:min_index(+,?),
    template: "<ConsistencyModule:> min_index(+Collection,?Index)",
    args:[
	"Collection":"A collection (a la collection_to_list/2) of integers or (domain) variables",
	"Index":"(Domain) variable or integer (array notation accepted)"
    ],
    kind: [constraint:[extra:[gccat:min_index]]],
    eg:"\
[eclipse 1]: min_index([1,2,3], I).

I = 1


[eclipse 2]: min_index([1,2,3,1,10,9,10], I).

I = I{[1, 4]}

[eclipse 4]: L = [A,B,C,D,E], L :: 1..10, min_index(L, 3), C #> 4.

L = [A{[5 .. 10]}, B{[5 .. 10]}, C{[5 .. 10]}, D{[5 .. 10]}, E{[5 .. 10]}]
A = A{[5 .. 10]}
B = B{[5 .. 10]}
C = C{[5 .. 10]}
D = D{[5 .. 10]}
E = E{[5 .. 10]}

[eclipse 5]: L = [A,B,C,D,E], L :: 1..10, min_index(L, 3), B #> 4.

L = [A{[1 .. 10]}, B{[5 .. 10]}, C{[1 .. 10]}, D{[1 .. 10]}, E{[1 .. 10]}]
A = A{[1 .. 10]}
B = B{[5 .. 10]}
C = C{[1 .. 10]}
D = D{[1 .. 10]}
E = E{[1 .. 10]}

[eclipse 6]:  L = [A,B,C,D,E], L :: 1..10, min_index(L, 3), B #< 4.

L = [A{[1 .. 10]}, B{[1 .. 3]}, C{[1 .. 3]}, D{[1 .. 10]}, E{[1 .. 10]}]
A = A{[1 .. 10]}
B = B{[1 .. 3]}
C = C{[1 .. 3]}
D = D{[1 .. 10]}
E = E{[1 .. 10]}
[eclipse 7]: [A,B,D] :: 1..10, C :: 20..30, min_index([A,B,C,D], I).

A = A{[1 .. 10]}
B = B{[1 .. 10]}
D = D{[1 .. 10]}
C = C{[20 .. 30]}
I = I{[1, 2, 4]}

",
    desc:html("\
    	Index is constrained to the index(es) of the variable(s) with
        the minimum  value in Collection. If Index is a variable, it
        must not occur in  Collection..</P><P>  

        You may find it more convenient to embed <TT>min_index(Vars)</TT> in a
        constraint expression.
        </P><P>
        As with all constraints that involve indexes, the index starts
        from 1, unlike Gecode's native indexes that starts from 0 - a
        dummy first element is added to Collection in the constraint posted
        to Gecode if Collection is not empty. 
        </P><P> 
        ConsistencyModule is the optional module specification to give the 
        consistency level for the propagation for this constraint: 
        gfd_gac for domain (generalised arc) consistency. 
</P><P>
        This constraint is known as min_index in the global constraint catalog,
        and is implemented using Gecode's minarg() constraint with tie-break
        set to false.
</P>
"),
    see_also:[min_first_index/2, max_index/2, max_first_index/2, min/2, max/2, collection_to_list/2]
    ]).

:- comment(min_index_g/2, [
    summary:"Index is constrained to the index(es) of the variable(s) with the"
" minimum value in Collection, with native gecode indexing",
    amode:min_index_g(+,?),
    template: "<ConsistencyModule:> min_index_g(+Collection,?Index)",
    args:[
	"Collection":"A collection (a la collection_to_list/2) of integers or (domain) variables",
	"Index":"(Domain) variable or integer (array notation accepted)"
    ],
    kind: [constraint:[extra:[gccat:min_index]]],
    see_also: [min_index/2],
    desc:html("\
  This version of min_index/2 uses the native Gecode indexing, which starts 
  from 0, i.e. the first element of Collection has index 0. This is different 
  from normal ECLiPSe's indexing, which starts from 1.
</p><p>
  This predicate maps directly to Gecode's native implementation of the
  constraint, and may therefore be more efficient, but could also be
  incompatible with existing ECLiPSe code. 
</P><P>
   This constraint can be embedded in a constraint expression in its
   functional form (without the last argument).
</p><p>
  See min_index/2 for a more detailed description of this predicate.")
]).   

%---------------------------------------------------------------------

:- comment(min_first_index/2, [
    summary:"Index is constrained to the index of the first variable with the minimum value in Collection",
    amode:min_first_index(+,?),
    template: "min_first_index(+Collection,?Index)",
    args:[
	"Collection":"A collection (a la collection_to_list/2) of integers or d(omain) variables",
	"Index":"(Domain) variable or integer (array notation accepted)"
    ],
    kind: [constraint:[extra:[gccat:min_index]]],
    desc:html("\
    	Index is constrained to the index of the first (smallest index)
        variable(s) with the minimum value in Collection.  If Index is a
        variable, it must not occur in  Collection..</P><P>  
  
        You may find it more convenient to embed <TT>min_first_index(Vars)</TT> 
        in a constraint expression.
        </P><P>
        As with all constraints that involve indexes, the index starts
        from 1, unlike Gecode's native indexes that starts from 0 - a
        dummy first element is added to Collection in the constraint posted
        to Gecode if Collection is not empty. 
        </P><P> 
        This constraint is a variation of min_index in the global
        constraint catalog, and is implemented using
        Gecode's minarg() constraint with tie-break set to true..
        
</P>
"),
    see_also:[min_index/2, max_first_index/2, max_index/2, min/2, max/2, collection_to_list/2]
    ]).

:- comment(min_first_index_g/2, [
    summary:"Index is constrained to the index of the first variable with the"
" minimum value in Collection, with native gecode indexing",
    amode:min_first_index_g(+,?),
    template: "min_first_index_g(+Collection,?Index)",
    args:[
	"Collection":"A collection (a la collection_to_list/2) of integers or (domain) variables",
	"Index":"(Domain) variable or integer (array notation accepted)"
    ],
    kind: [constraint:[extra:[gccat:min_index]]],
    see_also: [min_first_index/2],
    eg:"\
[eclipse 11]: min_first_index([1,2,3], I).

I = 1

[eclipse 12]: min_first_index([1,2,3,1,10,9,10], I).

I = 1

[eclipse 13]: L = [A,B,C,D,E], L :: 1..10, min_first_index(L, 3), C #> 4.

L = [A{[6 .. 10]}, B{[6 .. 10]}, C{[5 .. 9]}, D{[5 .. 10]}, E{[5 .. 10]}]
A = A{[6 .. 10]}
B = B{[6 .. 10]}
C = C{[5 .. 9]}
D = D{[5 .. 10]}
E = E{[5 .. 10]}
[eclipse 14]: L = [A,B,C,D,E], L :: 1..10, min_first_index(L, 3), B #> 4.

L = [A{[2 .. 10]}, B{[5 .. 10]}, C{[1 .. 9]}, D{[1 .. 10]}, E{[1 .. 10]}]
A = A{[2 .. 10]}
B = B{[5 .. 10]}
C = C{[1 .. 9]}
D = D{[1 .. 10]}
E = E{[1 .. 10]}

[eclipse 15]: L = [A,B,C,D,E], L :: 1..10, min_first_index(L, 3), B #< 4.

L = [A{[2 .. 10]}, B{[2, 3]}, C{[1, 2]}, D{[1 .. 10]}, E{[1 .. 10]}]
A = A{[2 .. 10]}
B = B{[2, 3]}
C = C{[1, 2]}
D = D{[1 .. 10]}
E = E{[1 .. 10]}

[eclipse 16]: [A,B,D] :: 1..10, C :: 20..30, min_first_index([A,B,C,D], I).

A = A{[1 .. 10]}
B = B{[1 .. 10]}
D = D{[1 .. 10]}
C = C{[20 .. 30]}
I = I{[1, 2, 4]}

",
    desc:html("\
  This version of min_first_index/2 uses the native Gecode indexing,
  which starts from 0, i.e. the first element of Collection has index 0.
  This is different from normal ECLiPSe's indexing, which starts from 1.
</p><p>
  This predicate maps directly to Gecode's native implementation of the
  constraint, and may therefore be more efficient, but could also be
  incompatible with existing ECLiPSe code. 
</P><P>
   This constraint can be embedded in a constraint expression in its
   functional form (without the last argument).
</p><p>
  See min_first_index/2 for a more detailed description of this predicate.")
]).   

%---------------------------------------------------------------------

:- comment(max_index/2, [
    summary:"Index is constrained to the index(es) of the variable(s) with the maximum value in Collection",
    amode:max_index(+,?),
    template: "<ConsistencyModule:> max_index(+Collection,?Index)",
    args:[
	"Collection":"A collection (a la collection_to_list/2) of integers or (domain) variables",
	"Index":"(Domain) variable or integer (array notation accepted)"
    ],
    kind: [constraint:[extra:[gccat:max_index]]],
    eg:"\
[eclipse 17]: max_index([1,2,3], I).

I = 3

[eclipse 18]: max_index([1,2,3,1,10,9,10], I).

I = I{[5, 7]}

[eclipse 19]: L = [A,B,C,D,E], L :: 1..10, max_index(L, 3), C #> 4.

L = [A{[1 .. 10]}, B{[1 .. 10]}, C{[5 .. 10]}, D{[1 .. 10]}, E{[1 .. 10]}]
A = A{[1 .. 10]}
B = B{[1 .. 10]}
C = C{[5 .. 10]}
D = D{[1 .. 10]}
E = E{[1 .. 10]}

[eclipse 20]:  L = [A,B,C,D,E], L :: 1..10, max_index(L, 3), C #< 4.

L = [A{[1 .. 3]}, B{[1 .. 3]}, C{[1 .. 3]}, D{[1 .. 3]}, E{[1 .. 3]}]
A = A{[1 .. 3]}
B = B{[1 .. 3]}
C = C{[1 .. 3]}
D = D{[1 .. 3]}
E = E{[1 .. 3]}

[eclipse 21]:  L = [A,B,C,D,E], L :: 1..10, max_index(L, 3), B #> 4.

L = [A{[1 .. 10]}, B{[5 .. 10]}, C{[5 .. 10]}, D{[1 .. 10]}, E{[1 .. 10]}]
A = A{[1 .. 10]}
B = B{[5 .. 10]}
C = C{[5 .. 10]}
D = D{[1 .. 10]}
E = E{[1 .. 10]}

[eclipse 23]:  L = [A,B,C,D,E], L :: 1..10, max_index(L, 3), B #< 4.

L = [A{[1 .. 10]}, B{[1 .. 3]}, C{[1 .. 10]}, D{[1 .. 10]}, E{[1 .. 10]}]
A = A{[1 .. 10]}
B = B{[1 .. 3]}
C = C{[1 .. 10]}
D = D{[1 .. 10]}
E = E{[1 .. 10]}

[eclipse 24]: [A,B,D] :: 1..10, C :: 20..30, max_index([A,B,C,D], I).

A = A{[1 .. 10]}
B = B{[1 .. 10]}
D = D{[1 .. 10]}
C = C{[20 .. 30]}
I = 3

",
    desc:html("\
    	Index is constrained to the index(es) of the variable(s) with
        the maximum  value in Collection. If Index is a variable, it
        must not occur in  Collection..</P><P>  

        You may find it more convenient to embed <TT>max_index(Vars)</TT> in a
        constraint expression.
        </P><P>
        As with all constraints that involve indexes, the index starts
        from 1, unlike Gecode's native indexes that starts from 0 - a
        dummy first element is added to Collection in the constraint posted
        to Gecode if Collection is not empty. 
        </P><P> 
        ConsistencyModule is the optional module specification to give the 
        consistency level for the propagation for this constraint: 
        gfd_gac for domain (generalised arc) consistency. 
</P><P>
        This constraint is known as max_index in the global constraint catalog,
        and is implemented using Gecode's maxarg() constraint with tie-break
        set to false.
</P>
"),
    see_also:[max_first_index/2, min_index/2, min_first_index/2, min/2, max/2, collection_to_list/2]
    ]).

:- comment(max_index_g/2, [
    summary:"Index is constrained to the index(es) of the variable(s) with the"
" maximum value in Collection, with native gecode indexing",
    amode:max_index_g(+,?),
    template: "<ConsistencyModule:> max_index_g(+Collection,?Index)",
    args:[
	"Collection":"A collection (a la collection_to_list/2) of integers or (domain) variables",
	"Index":"(Domain) variable or integer (array notation accepted)"
    ],
    kind: [constraint:[extra:[gccat:max_index]]],
    see_also: [max_index/2],
    eg:"\
[eclipse 25]: max_first_index([1,2,3], I).

I = 3

[eclipse 26]: max_first_index([1,2,3,1,10,9,10], I).

I = 5

[eclipse 27]: L = [A,B,C,D,E], L :: 1..10, max_first_index(L, 3), C #> 4.

L = [A{[1 .. 9]}, B{[1 .. 9]}, C{[5 .. 10]}, D{[1 .. 10]}, E{[1 .. 10]}]
A = A{[1 .. 9]}
B = B{[1 .. 9]}
C = C{[5 .. 10]}
D = D{[1 .. 10]}
E = E{[1 .. 10]}
[eclipse 28]: L = [A,B,C,D,E], L :: 1..10, max_first_index(L, 3), C #< 4.

L = [A{[1, 2]}, B{[1, 2]}, C{[2, 3]}, D{[1 .. 3]}, E{[1 .. 3]}]
A = A{[1, 2]}
B = B{[1, 2]}
C = C{[2, 3]}
D = D{[1 .. 3]}
E = E{[1 .. 3]}

[eclipse 29]:  L = [A,B,C,D,E], L :: 1..10, max_first_index(L, 3), B #> 4.

L = [A{[1 .. 9]}, B{[5 .. 9]}, C{[6 .. 10]}, D{[1 .. 10]}, E{[1 .. 10]}]
A = A{[1 .. 9]}
B = B{[5 .. 9]}
C = C{[6 .. 10]}
D = D{[1 .. 10]}
E = E{[1 .. 10]}

[eclipse 30]:  L = [A,B,C,D,E], L :: 1..10, max_first_index(L, 3), B #< 4.

L = [A{[1 .. 9]}, B{[1 .. 3]}, C{[2 .. 10]}, D{[1 .. 10]}, E{[1 .. 10]}]
A = A{[1 .. 9]}
B = B{[1 .. 3]}
C = C{[2 .. 10]}
D = D{[1 .. 10]}
E = E{[1 .. 10]}

[eclipse 31]: [A,B,D] :: 1..10, C :: 20..30, max_first_index([A,B,C,D], I).

A = A{[1 .. 10]}
B = B{[1 .. 10]}
D = D{[1 .. 10]}
C = C{[20 .. 30]}
I = 3

",
    desc:html("\
  This version of max_index/2 uses the native Gecode indexing, which starts 
  from 0, i.e. the first element of Collection has index 0. This is different 
  from normal ECLiPSe's indexing, which starts from 1.
</p><p>
  This predicate maps directly to Gecode's native implementation of the
  constraint, and may therefore be more efficient, but could also be
  incompatible with existing ECLiPSe code. 
</P><P>
   This constraint can be embedded in a constraint expression in its
   functional form (without the last argument).
</p><p>
  See max_index/2 for a more detailed description of this predicate.")
]).   

%---------------------------------------------------------------------

:- comment(max_first_index/2, [
    summary:"Index is constrained to the index of the first variable with the maximum value in Collection",
    amode:max_first_index(+,?),
    template: "max_first_index(+Collection,?Index)",
    args:[
	"Collection":"A collection (a la collection_to_list/2) of integers or (domain) variables",
	"Index":"(Domain) variable or integer (array notation accepted)"
    ],
    kind: [constraint:[extra:[gccat:max_index]]],
    desc:html("\
    	Index is constrained to the index of the first (smallest index)
        variable(s) with the maximum value in Collection. If Index is a 
        variable, it must not occur in  Collection..</P><P>  

        You may find it more convenient to embed <TT>max_first_index(Vars)</TT> 
        in a constraint expression.
        </P><P>
        As with all constraints that involve indexes, the index starts
        from 1, unlike Gecode's native indexes that starts from 0 - a
        dummy first element is added to Collection in the constraint posted
        to Gecode if Collection is not empty. 
        </P><P> 
        This constraint is a variation of max_index in the global
        constraint catalog, and is implemented using
        Gecode's maxarg() constraint with tie-break set to true..
        
</P>
"),
    see_also:[max_index/2, max_first_index/2, min_index/2, max/2, min/2, collection_to_list/2]
    ]).

:- comment(max_first_index_g/2, [
    summary:"Index is constrained to the index of the first variable with the"
" maximum value in Collection, with native gecode indexing",
    amode:max_first_index_g(+,?),
    template: "max_first_index_g(+Collection,?Index)",
    args:[
	"Collection":"A collection (a la collection_to_list/2) of integers or (domain_ variables",
	"Index":"(Domain) variable or integer (array notation accepted)"
    ],
    kind: [constraint:[extra:[gccat:max_index]]],
    see_also: [max_first_index/2],
    desc:html("\
  This version of max_first_index/2 uses the native Gecode indexing,
  which starts from 0, i.e. the first element of Collection has index 0.
  This is different from normal ECLiPSe's indexing, which starts from 1.
</p><p>
  This predicate maps directly to Gecode's native implementation of the
  constraint, and may therefore be more efficient, but could also be
  incompatible with existing ECLiPSe code. 
</P><P>
   This constraint can be embedded in a constraint expression in its
   functional form (without the last argument).
</p><p>
  See max_first_index/2 for a more detailed description of this predicate.")
]).   

%---------------------------------------------------------------------

:- comment(max/2, [
    summary:"Max is the maximum of the values in Collection",
    template: "<ConsistencyModule:> max(+Collection,?Max)",
    amode:max(+,?),
    args:[
	"Collection":"A collection (a la collection_to_list/2) of integers or (domain )variables",
	"Max":"(Domain) variable or integer (array notation accepted)"
    ],
    kind: [constraint:[extra:[gccat:maximum]]],
    desc:html("\
	Max is the maximum of the values in Collection.</P><P>

        You may find it more convenient to embed <TT>max(Vars)</TT> in a
        constraint expression.
        </P><P> 
        ConsistencyModule is the optional module specification to give the 
        consistency level for the propagation for this constraint: 
        gfd_bc for bounds consistency, and gfd_gac for domain (generalised 
        arc) consistency. 
</P><P>
        This constraint is known as maximum in the global constraint catalog,
        and is implemented using Gecode's max() constraint.
</P>
"),
    see_also:[min/2,sum/2,_:max/2,collection_to_list/2]
    ]).


%---------------------------------------------------------------------

:- comment(min/2, [
    summary:"Min is the minimum of the values in Collection",
    amode:min(+,?),
    template: "<ConsistencyModule:> min(+Collection,?Min)",
    args:[
	"Collection":"A collection (a la collection_to_list/2) of integers or (domain) variables",
	"Min":"(Domain) variable or integer (array notation accepted)"
    ],
    kind: [constraint:[extra:[gccat:minimum]]],
    desc:html("\
    	Min is the minimum of the values in Collection.</P><P>  

        You may find it more convenient to embed <TT>min(Vars)</TT> in a
        constraint expression.
        </P><P> 
        ConsistencyModule is the optional module specification to give the 
        consistency level for the propagation for this constraint: 
        gfd_bc for bounds consistency, and gfd_gac for domain (generalised 
        arc) consistency. 
</P><P>
        This constraint is known as minimum in the global constraint catalog,
        and is implemented using Gecode's min() constraint.
</P>
"),
    see_also:[max/2,sum/2,_:min/2,collection_to_list/2]
    ]).


%----------------------------------------------------------------------

:- comment(sum/2, [
    summary:"The sum (Collection) or scalar product (IntCollection*Collection) of the Collection elements is Sum",
    template: "<ConsistencyModule:> sum(+Collection,?Sum)",
    amode:sum(+,?),
    args:[
	"Collection or Coeffs*Collection":
        "Collection: collection of N integers or (domain) variables. Coeffs: collection of N integers.",
	"Sum":"(Domain) variable or integer (array notation accepted)"
    ],
    see_also: [sumlist/2, _:sum/2],
    kind: [constraint:[extra:[gccat:sum_ctr]]],
    desc:html("<P>\
          Constrains Sum to be the sum of the elements in Collection if
          the first argument is a collection of integers or domain variables.
	  </P><P>
          Constrains Sum to be the scalar product of a collection of integers 
          and a collection of integers or domain variables if the first
          argument is Coeffs*Collection. Coeffs and Collection
          must have the same number of elements, and the scalar product 
          is the sum of the coefficients in Coeffs with the corresponding
          element in Collection.
	  </P><P>
	  Any input variables which are not already domain variable will be
          turn into domain variables with default bounds.</P><P>
	  </P><P>
          You may find it more convenient to embed <TT>sum(Vars)</TT> in a
          constraint expression.
	  </P><P>
          ConsistencyModule is the optional module specification to give the 
          consistency level for the propagation for this constraint: 
          gfd_bc for bounds consistency, and 
          gfd_gac for domain (generalised arc) consistency.
          </P><P>
          This constraint is known as sum_ctr (with the = relation) in the
          global constraint catalog, and is implemented using Gecode's
          linear() constraint (with IRT_EQ relation).
          </P>
") 
    ]).

:- comment(sumlist/2, [
    summary:"The sum (Collection) or scalar product (IntCollection*Collection) of the Collection elements is Sum",
    template: "<ConsistencyModule:> sumlist(+Collection,?Sum)",
    amode:sumlist(+,?),
    args:[
	"Collection or Coeffs*Collection":
        "Collection: collection of N integers or (domain) variables. Coeffs: collection of N integers.",
	"Sum":"(Domain) variable or integer (array notation accepted)"
    ],
    see_also: [_:sumlist/2],
    kind: [constraint:[extra:[gccat:sum_ctr]]],
    desc:html("<P>\
   An alias for sum/2. provided for consistency.</P>

   You may find it more convenient to embed <TT>sumlist(Vars)</TT> in a
   constraint expression.
")
    ]).

%----------------------------------------------------------------------

:- comment(sum/3, [
    summary:"Constrains the sum of the elements of Collection to"
            " satisfy the relation sum(Collection) Rel Sum.",
    template: "<ConsistencyModule:> sum(+Collection,+Rel,?Sum)",
    amode:sum(+,+,?),
    args:[
	"Collection": "Collection of integers or (domain) variables.",
        "RelOp": "One of the atom: #>, #>=, #<, #=<, #=, #\\=",
	"Sum":"(Domain) variable or integer (array notation accepted)"
    ],
    kind: [constraint:[extra:[gccat:sum_ctr]]],
    see_also: [sum/4],
    desc:html("<P>\
          Constrains the sum of the elements in Collection to satisfy
          the relation sum(Collection) Rel Sum.
	  </P><P>
          Rel can be one of #&gt;, #&gt;=, #&lt;, #=&lt;, #=, #\\= (or equivalently,
          &gt;, &gt;=, &lt;, =&lt;, =, \\=).
	  </P><P>
	  Any input variables which are not already domain variable will be
          turn into domain variables with default bounds.</P><P>
	  </P><P>
          You may find it more convenient to embed <TT>sum(Collections,RelOp)</TT> 
          in a constraint expression.
	  </P><P>
          ConsistencyModule is the optional module specification to give the 
          consistency level for the propagation for this constraint: 
          gfd_bc for bounds consistency, and 
          gfd_gac for domain (generalised arc) consistency.
          </P><P>
          Domain consistency is different from bounds consistency only if
          Rel is #=.
          </P><P>
          This constraint is known as sum_ctr in the global constraint 
          catalog, and is implemented using Gecode's linear() constraint.
          </P>
    ") 
]).

%----------------------------------------------------------------------

:- comment(sum/4, [
    summary:"Reflect into Bool the truth of the sum of the elements of Collection"
            " satisfying the relation sum(Collection) Rel Sum.",
    template: "<ConsistencyModule:> sum(+Collection,+Rel,?Sum,?Bool)",
    amode:sum(+,+,?,?),
    args:[
	"Collection": "Collection of integers or (domain) variables.",
        "RelOp": "One of the atom: #>, #>=, #<, #=<, #=, #\\=",
	"Sum":"(Domain) variable or integer (array notation accepted)",
        "Bool":"(Domain) variable or the integer 0 or 1 (array notation accepted)"
    ],
    kind: [constraint],
    see_also: [sum/3],
    desc:html("<P>\
          This is the reified form of sum/3, which constrains the sum
          of  the elements in Collection to satisfy the relation 
          sum(Collection) Rel Sum.
	  </P><P>
          Rel can be one of #&gt;, #&gt;=, #&lt;, #=&lt;, #=, #\\= (or equivalently,
          &gt;, &gt;=, &lt;, =&lt;, =, \\=).
	  </P><P>
	  Any input variables which are not already domain variable will be
          turn into domain variables with default bounds.</P><P>
	  </P><P>
          ConsistencyModule is the optional module specification to give the 
          consistency level for the propagation for this constraint: 
          gfd_bc for bounds consistency 
          </P><P>
          This constraint is implemented using Gecode's linear() constraint
          (reified version).
          </P>
    ")
]).

%----------------------------------------------------------------------

:- comment(mem/2, [
    amode: mem(+, ?),
    template: "<ConsistencyModule:> mem(+Vars,?Member)",
    args:[
	"Vars": "Collection (a la collection_to_list/2) of (domain) variables or integers (NOT arbitrary expressions)",
	"Member":  "Member element of Vars (domain variable or integer, array notation accepted)"
    ],
    summary: "Constrains Member to be the a member element in Vars.",
    see_also: [mem/3, collection_to_list/2],
    kind: [constraint],
    eg: "\
[eclipse 7]: A :: 1..10, B :: 2..20, mem([A,B], M).

A = A{[1 .. 10]}
B = B{[2 .. 20]}
M = M{[1 .. 20]}


[eclipse 8]: A :: 1..10, B :: 2..20, mem([A,B], M), M #< 5.

A = A{[1 .. 10]}
B = B{[2 .. 20]}
M = M{[1 .. 4]}


[eclipse 9]: A :: 1..10, B :: 2..20, mem([A,B], M), M #< 2.

A = A{[1 .. 10]}
B = B{[2 .. 20]}
M = 1

[eclipse 10]: mem([4,5,5,4,1,5], C).

C = C{[1, 4, 5]}


",
    desc: html("<P>
   Constrains Member to be a one of the elements in Vars.
</P><P>
   Note that this constraint has the same declarative semantics as the
   standard member/2 predicate, but the order of the arguments are
   reversed to allow the constraint to be used in constraint
   expressions.
 </P><P>
   You may find it more convenient to embed <TT>mem(Vars)</TT> in a
   constraint expression.
</P><P>
   This constraint is implemented by Gecode's member() constraint.
</P><P>
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
       gfd_gac for domain (generalised arc) consistency.
</P>
")
]).

:- comment(mem/3, [
    amode: mem(+, ?, ?),
    template: "<ConsistencyModule:> mem(+Vars,?Member,?Bool)",
    args:[
	"Vars": "Collection (a la collection_to_list/2) of (domain) variables or integers (NOT arbitrary expressions)",
	"Member":  "Member element of Vars (domain variable or
 integer, array notation accepted)",
        "Bool": "Reified truth value (0/1 integer or (domain) variable, array notation accepted)"
    
    ],
    summary: "Reflect into Bool the truth of Member being a member element of Vars.",
    see_also: [mem/2, collection_to_list/2],
    kind: [constraint],
    eg: "\
[eclipse 11]: A :: 1..10, B :: 2..20, mem([A,B], M, Bool), M #< 2.

A = A{[1 .. 10]}
B = B{[2 .. 20]}
M = M{[-1000000 .. 1]}
Bool = Bool{[0, 1]}


Delayed goals:
        gfd : gfd_do_propagate(gfd_prob(nvars(4)))
Yes (0.00s cpu)
[eclipse 12]: A :: 1..10, B :: 2..20, mem([A,B], M, Bool), M #< 2, Bool = 1.

A = A{[1 .. 10]}
B = B{[2 .. 20]}
M = 1
Bool = 1

[eclipse 13]: A :: 1..10, B :: 2..20, mem([A,B], M, Bool), M #< 2, Bool = 0.

A = A{[1 .. 10]}
B = B{[2 .. 20]}
M = M{[-1000000 .. 1]}
Bool = 0

",
    desc: html("<P>
   Reified form of the mem/2 constraint, which constrains Member to be
   one of the elements in Vars.
</P><P>
   This constraint is implemented by Gecode's member() constraint 
   (reified version).
</P><P>
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
       gfd_gac for domain (generalised arc) consistency.
</P>
")
]).

%----------------------------------------------------------------------

:- comment(scalar_product/4, [
    summary:"Constrains the scalar product of the elements of Coeffs"
            " and Collection to satisfy the relation sum(Coeffs*Collection) Rel P.",
    template: "<ConsistencyModule:> scalar_product(++Coeffs,+Collection,+Rel,?Sum)",
    amode:scalar_product(++,+,+,?),
    args:[
	"Coeffs": "Collection of N integers.",
	"Collection": "Collection of N integers or (domain) variables.",
        "RelOp": "One of the atom: #>, #>=, #<, #=<, #=, #\\=",
	"P":"(Domain) variable or integer (array notation accepted)"
    ],
    kind: [constraint:[extra:[gccat:scalar_product]]],
    desc:html("<P>\
          Constrains the scalar product of the elements in Collection to satisfy
          the relation sum(Coeffs*Collection) Rel P.
	  </P><P>
          Rel can be one of #&gt;, #&gt;=, #&lt;, #=&lt;, #=, #\\= (or equivalently,
          &gt;, &gt;=, &lt;, =&lt;, =, \\=).
	  </P><P>
          The Scalar Product of the collection of N integers in Coeffs and
          the collection of N domain variables or integers in Collection 
          is the sum of all Ci*Vi, where Ci is a element in Coeffs and
          Vi the corresponding element in Collection.
          </P><P>
	  Any input variables which are not already domain variable will be
          turn into domain variables with default bounds.
	  </P><P>
          You may find it more convenient to embed <TT>scalar_product</TT> in a
          constraint expression.
	  </P><P>
          ConsistencyModule is the optional module specification to give the 
          consistency level for the propagation for this constraint: 
          gfd_bc for bounds consistency, and 
          gfd_gac for domain (generalised arc) consistency.
          </P><P>
          Domain consistency is different from bounds consistency only if
          Rel is #=.
          </P><P>
          This constraint is known as scalar_product in the global constraint 
          catalog, and is implemented using Gecode's linear() constraint.
          </P>
    ") 
]).

%----------------------------------------------------------------------

:- comment(scalar_product/5, [
    summary:"Reflect into Bool the truth of the scalar product of the"
            " elements of Coeffs and Collection satisfying the relation "
            " sum(Coeffs*Collection) Rel Sum.",
    template: "<ConsistencyModule:> scalar_product(++Coeffs,+Collection,+Rel,?Sum,?Bool)",
    amode:scalar_product(++,+,+,?,?),
    args:[
	"Coeffs": "Collection of N integers.",
	"Collection": "Collection of N integers or (domain) variables.",
        "RelOp": "One of the atom: #>, #>=, #<, #=<, #=, #\\=",
	"P":"(Domain) variable or integer (array notation accepted)",
        "Bool":"(Domain) variable or the integer 0 or 1 (array notation accepted)"
    ],
    kind: [constraint],
    desc:html("<P>\
          This is the reified form of scalar_product/4, which constrains the
          scalar product of  the elements in Coeffs and Collection to satisfy
          the relation 
          sum(Coeffs*Collection) Rel P.
	  </P><P>
          Rel can be one of #&gt;, #&gt;=, #&lt;, #=&lt;, #=, #\\= (or equivalently,
          &gt;, &gt;=, &lt;, =&lt;, =, \\=).
          </P><P>
	  Any input variables which are not already domain variable will be
          turn into domain variables with default bounds.</P><P>
	  </P><P>
          ConsistencyModule is the optional module specification to give the 
          consistency level for the propagation for this constraint: 
          gfd_bc for bounds consistency 
          </P><P>
          This constraint is implemented using Gecode's linear() constraint
          (reified version).
          </P>
    ")
]).

%---------------------------------------------------------------------
:- comment((and)/2, [
    amode: and(+, +),
    template: "<ConsistencyModule:> +ConX and +ConY",
    args: [
	"ConX": "Constraint",
	"ConY": "Constraint"
    ],
    summary: "Constraints ConX and ConY must both be true.",
    see_also: [(and)/3, (neg)/1, (neg)/2, (or)/2, (xor)/2, (=>)/2,
               (<=>)/2,_:(and)/2],
    kind: [constraint],
    desc: html("<P>
   Equivalent to BX #= (ConX), BY #= (ConY), BX + BY #= 2</P>
   <P>
   The two constraints are reified in such a way that both must be true.
   ConX and ConY must be a constraints that have a corresponding reified
   form.</P><P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. 
</P><P>
   This constraint is implemented using Gecode's MiniModel's rel() for
   both integer and boolean expressions, with sub-expressions/constraints
   not supported by MiniModel factored out and posted as auxiliary 
   constraints.
</P>
")
]).

:- comment((and)/3, [
    amode: and(+, +, ?),
    template: "<ConsistencyModule:> and(+ConX,+ConY,Bool)",
    args: [
	"ConX": "Constraint",
	"ConY": "Constraint",
        "Bool": "Reified truth value of the constraint (array notation accepted)"
    ],
    summary: "Bool is the reified truth of both constraints ConX and ConY being true.",
    see_also: [(and)/2, (neg)/2, (or)/3, (xor)/3, (=>)/3,
               (<=>)/3, _:(and)/3],
    kind: [constraint:[extra:[gccat:(and)]]],
    desc: html("<P>
   Equivalent to BX #= (ConX), BY #= (ConY), Bool #= (BX + BY #= 2)</P>
   <P>
   The two constraints are reified in such a way that Bool reflects the
   truth of both being true.  ConX and ConY must be constraints that have a
   corresponding reified form.<P></P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. 
</P><P>
   This constraint is implemented using Gecode's MiniModel's rel() for
   both integer and boolean expressions, with sub-expressions/constraints
   not supported by MiniModel factored out and posted as auxiliary 
   constraints.
</P><P>
   A more restricted version of this constraint is defined in the 
   global constraint catalog as 'and', in that the reified
   truth value is the logical conjunctions of 0/1 variables rather than 
   constraints.
</P>
")
]).

%---------------------------------------------------------------------
:- comment((or)/2, [
    amode: or(+, +),
    template: "<ConsistencyModule:> +ConX or +ConY",
    args: [
	"ConX": "Constraint",
	"ConY": "Constraint"
    ],
    summary: "At least one of the constraints ConX or ConY must be true.",
    see_also: [(or)/3, (neg)/1, (xor)/2, (and)/2, (=>)/2,
               (<=>)/2, _:(or)/2],
    kind: [constraint],
    desc: html("<P>
   Equivalent to BX #= (ConX), BY #= (ConY), BX + BY #&gt;= 1</P>
   <P>
   The two constraints are reified in such a way that at least one must be
   true.  ConX and ConY must be constraints that have a corresponding
   reified form.<P></P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. 
</P><P>
   This constraint is implemented using Gecode's MiniModel's rel() for
   both integer and boolean expressions, with sub-expressions/constraints
   not supported by MiniModel factored out and posted as auxiliary 
   constraints.
</P>
")
]).

:- comment((or)/3, [
    amode: or(+, +, ?),
    template: "<ConsistencyModule:> or(+ConX,+ConY,Bool)",
    args: [
	"ConX": "Constraint",
	"ConY": "Constraint",
        "Bool": "Reified truth value of the constraint (array notation accepted)"
    ],
    summary: "Bool is the reified truth of at least one of the constraints ConX or ConY being true.",
    see_also: [(or)/2, (neg)/2, (xor)/3, (and)/3, (=>)/3,
               (<=>)/3, _:(or)/3],
    kind: [constraint:[extra:[gccat:(or)]]],
    desc: html("<P>
   Equivalent to BX #= (ConX), BY #= (ConY), Bool #= (BX + BY #&gt;= 1)</P>
   <P>
   The two constraints are reified in such a way that Bool reflects the
   truth of at least one being true.  ConX and ConY must be constraints that
   have a corresponding reified form.<P></P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. 
</P><P>
   This constraint is implemented using Gecode's MiniModel's rel() for
   both integer and boolean expressions, with sub-expressions/constraints
   not supported by MiniModel factored out and posted as auxiliary 
   constraints.
</P><P>
   A more restricted version of this constraint is defined in the 
   global constraint catalog as 'or', in that the reified truth value
   is the logical disjunction of 0/1 variables rather than constraints.
</P>
")
]).

%---------------------------------------------------------------------
:- comment((xor)/2, [
    amode: xor(+, +),
    template: "<ConsistencyModule:> +ConX xor +ConY",
    args: [
	"ConX": "Constraint",
	"ConY": "Constraint"
    ],
    summary: "One of the constraints ConX or ConY must be true.",
    see_also: [(xor)/3, (neg)/1, (or)/2, (and)/2, (=>)/2,
               (<=>)/2, _:(#\=)/2],
    kind: [constraint],
    desc: html("<P>
   Equivalent to BX #= (ConX), BY #= (ConY), BX + BY #= 1</P>
   <P>
   The two constraints are reified in such a way that one and only one must be
   true.  ConX and ConY must be constraints that have a corresponding
   reified form.<P></P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. 
</P><P>
   This constraint is implemented using Gecode's MiniModel's rel() for
   both integer and boolean expressions, with sub-expressions/constraints
   not supported by MiniModel factored out and posted as auxiliary 
   constraints.
</P>
")
]).

:- comment((xor)/3, [
    amode: xor(+, +, ?),
    template: "<ConsistencyModule:> xor(+ConX,+ConY,Bool)",
    args: [
	"ConX": "Constraint",
	"ConY": "Constraint",
        "Bool": "Reified truth value of the constraint (array notation accepted)"
    ],
    summary: "Bool is the reified truth of one of the constraints ConX or ConY being true.",
    see_also: [(or)/2, (neg)/2, (and)/3, (xor)/3, (=>)/3,
               (<=>)/3, (=<)/3, _: (#\=)/3],
    kind: [constraint:[extra:[gccat:(xor)]]],
    desc: html("<P>
   Equivalent to BX #= (ConX), BY #= (ConY), Bool #= (BX + BY #= 1)</P>
   <P>
   The two constraints are reified in such a way that Bool reflects the
   truth of one (and only one) being true.  ConX and ConY must be 
   constraints that have a corresponding reified form.<P></P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. 
</P><P>
   This constraint is implemented using Gecode's MiniModel's rel() for
   both integer and boolean expressions, with sub-expressions/constraints
   not supported by MiniModel factored out and posted as auxiliary 
   constraints.
</P><P>
   A more restricted version of this constraint is defined in the 
   global constraint catalog as 'xor', in that the reified truth value
   is the logical exclusive disjunction of 0/1 variables rather than 
   constraints.
</P>
")
]).

%---------------------------------------------------------------------
:- comment((=>)/2, [
    amode: =>(+, +),
    template: "<ConsistencyModule:> +ConX => +ConY",
    args: [
	"ConX": "Constraint",
	"ConY": "Constraint"
    ],
    summary: "Constraint ConX  implies ConY.",
    see_also: [(=>)/3, (neg)/1, (and)/2, (or)/2, (xor)/2, (<=>)/2, _:(=>)/2],
    kind: [constraint],
    desc: html("<P>
   Equivalent to BX #= (ConX), BY #= (ConY), BX #=&lt; BY</P>
   <P>
   The two constraints are reified in such a way that ConX being true
   implies that ConY must also be true.  ConX and ConY must be constraints
   that have a corresponding reified form.<P></P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. 
</P><P>
   This constraint is implemented using Gecode's MiniModel's rel() for
   both integer and boolean expressions, with sub-expressions/constraints
   not supported by MiniModel factored out and posted as auxiliary 
   constraints.
</P>
")
]).

:- comment((=>)/3, [
    amode: =>(+, +, ?),
    template: "<ConsistencyModule:> =>(+ConX,+ConY,Bool)",
    args: [
	"ConX": "Constraint",
	"ConY": "Constraint",
        "Bool": "Reified truth value of the constraint (array notation accepted)"
    ],
    summary: "Bool is the reified truth of constraint ConX implying the truth of ConY.",
    see_also: [(=>)/2, (neg)/2, (or)/3, (xor)/3, (and)/3, (<=>)/3,
               _:(=>)/3],
    kind: [constraint:[extra:[gccat:imply]]],
    desc: html("<P>
   Equivalent to BX #= (ConX), BY #= (ConY), Bool #= (BX #=&lt; BY)</P>
   <P>
   The two constraints are reified in such a way that Bool is true if ConX
   being true implies that ConY must also be true.  ConX and ConY must be
   constraints that have a corresponding reified form.<P></P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. 
</P><P>
   This constraint is implemented using Gecode's MiniModel's rel() for
   both integer and boolean expressions, with sub-expressions/constraints
   not supported by MiniModel factored out and posted as auxiliary 
   constraints.
</P><P>
   A more restricted version of this constraint is defined in the 
   global constraint catalog as 'imply', in that the reified truth value
   is the logical implication of 0/1 variables rather than constraints.
</P>
")
]).

%---------------------------------------------------------------------

:- comment((<=>)/2, [
    amode: <=>(+, +),
    template: "<ConsistencyModule:> +ConX <=> +ConY",
    args: [
	"ConX": "Constraint",
	"ConY": "Constraint"
    ],
    summary: "Constraint ConX has the equivalent truth value as ConY.",
    see_also: [(<=>)/3, (neg)/1, (or)/2, (xor)/2, (and)/2,
               (=>)/2, _:(#=)/2],
    kind: [constraint],
    desc: html("<P>
   Equivalent to BX #= (ConX), BY #= (ConY), BX #= BY</P>
   <P>
   The two constraints are reified in such a way that ConX and ConY constrained
   to the same truth value. ConX and ConY must be constraints that have a 
   corresponding reified form.</P><P>

   This connective is not available in IC because #=/2 can be used instead.
   It is provided in GFD as it maps directly to gecode's equivalence 
   connective.</P><P> 

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. 
</P><P>
   This constraint is implemented using Gecode's MiniModel's rel() for
   both integer and boolean expressions, with sub-expressions/constraints
   not supported by MiniModel factored out and posted as auxiliary 
   constraints.
</P>
")
]).

:- comment((<=>)/3, [
    amode: <=>(+, +, ?),
    template: "<ConsistencyModule:> <=>(+ConX,+ConY,Bool)",
    args: [
	"ConX": "Constraint",
	"ConY": "Constraint",
        "Bool": "Reified truth value of the constraint (array notation accepted)"
    ],
    summary: "Bool is the reified truth of constraint ConX is equivalent to the truth of ConY.",
    see_also: [(<=>)/2, (neg)/2, (=>)/3, (or)/3, (xor)/3, (and)/3, _:(#\=)/3],
    kind: [constraint:[extra:[gccat:equivalent]]],
    desc: html("<P>
   Equivalent to BX #= (ConX), BY #= (ConY), Bool #= (BX #= BY)</P>
   <P>
   The two constraints are reified in such a way that Bool is true if ConX
   and ConY have the same truth value.  ConX and ConY must be
   constraints that have a corresponding reified form.<P></P>

   This reified connective is not available in IC because #=/3 can be 
   used instead. It is provided in GFD as <=> maps to gecode's 
   equivalence connective.</P><P> 

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. 
</P><P>
   This constraint is implemented using Gecode's MiniModel's rel() for
   both integer and boolean expressions, with sub-expressions/constraints
   not supported by MiniModel factored out and posted as auxiliary 
   constraints.
</P><P>
   A more restricted version of this constraint is defined in the 
   global constraint catalog as 'equivalent', in that the reified truth value
   is the logical equivalence of 0/1 variables rather than constraints.
</P>
")
]).

%---------------------------------------------------------------------

:- comment((neg)/1, [
    amode: neg(+),
    template: "<ConsistencyModule:> neg(+Con)",
    args: [
	"Con": "Constraint"
    ],
    summary: "Constraints Con is negated.",
    see_also: [(neg)/2, (or)/2, (xor)/2, (=>)/2,
               (<=>)/2, (and)/2, _:(neg)/1],
    kind: [constraint],
    desc: html("<P>
   Equivalent to 0 #= (Con)</P>
   <P>
   The reified constraint Con is negated.  Con must be a constraint that has
   a corresponding reified form.<P></P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. 
</P><P>
   This constraint is implemented using Gecode's MiniModel's rel() for
   both integer and boolean expressions, with sub-expressions/constraints
   not supported by MiniModel factored out and posted as auxiliary 
   constraints.
</P>
")
]).

:- comment((neg)/2, [
    amode: neg(+, ?),
    template: "<ConsistencyModule:> neg(+Con,Bool)",
    args: [
	"Con": "Constraint",
        "Bool": "Reified truth value of the constraint (array notation accepted)"
    ],
    summary: "Bool is the logical negation of the reified truth constraints Con.",
    see_also: [(and)/3, (neg)/1, (xor)/3, (or)/3, (=>)/3,
               (<=>)/3, _:(neg)/2],
    kind: [constraint],
    desc: html("<P>
   Equivalent to B #= (Con), Bool #= 1-B</P>
   <P>
   Bool is the logical negation of the reified truth constraint Con.  Con
   must be a constraint that has a corresponding reified form.<P></P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. 
</P><P>
   This constraint is implemented using Gecode's MiniModel's rel() for
   both integer and boolean expressions, with sub-expressions/constraints
   not supported by MiniModel factored out and posted as auxiliary 
   constraints.
</P>
")
]).

%---------------------------------------------------------------------

:- comment(element/3, [
	summary:"Value is the Index'th element of the integer collection Collection.",
	template:"<ConsistencyModule:> element(?Index, +Collection, ?Value)",
	args:[
	    "?Index" : "A (domain) variable or an integer (array notation accepted).",
	    "+Collection" : "A non-empty collection of integers or (domain) variable.",
	    "?Value" : "A (domain) variable or an integer (array notation accepted)."
	],
	fail_if:"Fails if Value is not the Index'th element of Collection.",
        kind: [constraint:[extra:[gccat:element]]],
        see_also: [element_g/3, _:element/3],
        desc:html("This constraint can be used in a variety of programs to state a
   relation between two domain variables.  Collection is a collection of 
   integers and the constraint states that its Index'th element is equal to 
   Value, i.e.
<P>
<PRE>
			     Collection_Index = Value
</PRE>
   the domain of the other variable is updated accordingly. Index starts from 1.
<P>
   Note that unlike the element constraint in IC, the values in Collection 
   can be domain variables as well as integers. Also note that the actual
   Gecode constraint has an index that starts from 0 - a dummy element
   is added to start of Collection to map Index to ECLiPSe style index
   starting from 1. A version of this constraint that uses the native 
   Gecode indexing is element_g/3. 
<P>
   This constraint can be embedded in a constraint expression in its
   functional form (without the last argument).
<P>
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency.
</P><P> 
   This constraint is known as element in the global constraint catalog,
   and is implemented with Gecode's element() constraint.
</P>
"),
	eg:"
[eclipse 2]: element(I, [1,3,6,3], V).

I = I{[1 .. 4]}
V = V{[1, 3, 6]}


[eclipse 2]: element(I, [1,3,6,3], V),  V #\\= 3.

I = I{[1, 3]}
V = V{[1, 6]}

[eclipse 2]:  X :: [1..10], gfd_gac: element(I, [1,X,6,3],  V), V #\\= 3.


Z = Z{[1 .. 10]}
I = I{[1 .. 3]}
V = V{[1, 2, 4 .. 10]}


"
    ]).


:- comment(element_g/3, [
	summary:"Value is the Index'th element of the integer list List, with native Gecode indexing.",
	template:"<ConsistencyModule:> element_g(?Index, ++List, ?Value)",
	args:[
	    "?Index" : "A (domain) variable or an integer.",
	    "+Collection" : "A non-empty collection of integers or (domain) variable.",
	    "?Value" : "A (domain) variable or an integer (array notation accepted)."
	],
	fail_if:"Fails if Value is not the Index'th element of Collection.",
	see_also: [element/3],
        kind: [constraint:[extra:[gccat:element]]],
        desc:html("\
  This version of element/3 uses the native Gecode indexing, which starts 
  from 0, i.e. the first element of Collection has index 0. This is different 
  from normal ECLiPSe's indexing, which starts from 1.
</p><p>
  This predicate maps directly to Gecode's native implementation of the
  constraint, and may therefore be more efficient, but could also be
  incompatible with existing ECLiPSe code. 
</P><P>
   This constraint can be embedded in a constraint expression in its
   functional form (without the last argument).
</p><p>
  See element/3 for a more detailed description of this predicate.")
]).   

%----------------------------------------------------------------------

:- comment(occurrences/3, [
    summary:"The value Value occurs in Vars N times",
    template:"<ConsistencyModule:> occurrences(++Value,+Vars,?N)",
    amode:occurrences(++,+,?),
    args:[
	"Value":"Integer (or (domain) variable)",
	"Vars":"Collection (a la collection_to_list/2) of integers or (domain) variables",
	"N":"(Domain) variable or integer (array notation accepted)"
    ],
    kind: [constraint:[extra:[gccat:exactly]]],
    eg:"
[eclipse 11]: occurrences(1,[3,5,1,4,1,3], N).

N = 2

[eclipse 12]: occurrences(6, [], N).

N = 0

[eclipse 15]: [A,B,C] :: [1..10], occurrences(3, [A,B,C], 0). 

A = A{[1, 2, 4 .. 10]}
B = B{[1, 2, 4 .. 10]}
C = C{[1, 2, 4 .. 10]}


[eclipse 16]:  N :: [3, 5], occurrences(3, [3,A,3,5,3], N).

N = 3
A = A{[-1000000 .. 2, 4 .. 1000000]}  % A cannot be 3


[eclipse 17]:  N :: [3, 5], occurrences(3, [3,A,3,5,3,3], N).

N = 5
A = 3

",
    desc:html("\
    	  The value Value occurs in Vars N times.
<P>
          ConsistencyModule is the optional module specification to give the 
          consistency level for the propagation for this constraint: 
          gfd_gac for domain (generalised arc) consistency. Note that if
          Value is a domain variable, then the propagation is weak, achieving
          neither domain or bound consistency until Value becomes ground.
</P><P>
          This constraint can be embedded in a constraint expression in its
          functional form (without the last argument).
 </p><p>
          This constraint is a specialisation of the more general count/4
          constraint, with the (#=) relation, i.e. the occurrences of Value
          is equal to N. 
</p><p>
          This constraint is known as exactly in the global constraint
          catalog, where N is restricted to an integer; the more general 
          count/4 constraint is also known as count in the global 
          constraint catalog, and the constraint is implemented using 
          Gecode's count() constraint.
  </p>
"),
    see_also:[element/3, atmost/3, count/4, collection_to_list/2, _:occurrences/3]
    ]).

%----------------------------------------------------------------------

:- comment(atmost/3, [
	summary:"At most N elements of Vars have the value V.",
	template:"<ConsistencyModule:> atmost(?N, +Vars, +V)",
        amode: atmost(?,+,+),
        kind: [constraint:[extra:[gccat:atmost]]],
        eg:"
[eclipse 33]: atmost(N,  [3, 5, 1, 4, 1, 3], 1).

N = N{[2 .. 6]}

[eclipse 34]: N :: [3,5], atmost(N, [3,A,3,5,3,3], 3).

N = 5
A = A{[-1000000 .. 1000000]}

[eclipse 35]:  N :: [3,6], atmost(N, [3,A,3,5,3,3], 3).

N = 6
A = A{[-1000000 .. 1000000]}

[eclipse 36]: N::[1..3], atmost(N, [3,4,2,3,1], 3).

N = N{[2, 3]}

",
        desc:html("\
   This constraint ensures that at most N element of Vars have the value V.
</p><p>
   This constraint is a specialisation of the more general count/4
   constraint, with the (#=&lt;) relation, i.e. the occurrences of V
   is less than or equal to N. Note that the order in which N and V
   occur in the constraint are swapped with respect to occurrences/3
   and count/4; this is for compatibility with the argument order in
   IC.
<P>
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_gac for domain (generalised arc) consistency. Note that if
   V is a domain variable, then the propagation is weak, achieving
   neither domain or bound consistency until V becomes ground.
 </p><p>
   This constraint is also known as atmost in the global constraint
   catalog, where N is restricted to an integer; the more general 
   count/4 constraint is also known as count in the global 
   constraint catalog, and the constraint is implemented using 
   Gecode's count() constraint.
  </p>
"),
	args:["N" : "An integer or (domain) variable (array notation accepted)",
	      "+Vars" : "A collection (a la collection_to_list/2) of (domain) variables or integers",
	      "V" : "An integer."],
	fail_if:"   Fails if more than N elements of Vars can be instantiated to V.",
	see_also:[_:atmost/3, count/4, atleast/3, element/3, occurrences/3, collection_to_list/2]]).

%----------------------------------------------------------------------

:- comment(atleast/3, [
	summary:"Atleast N elements of Vars have the value V.",
	template:"<ConsistencyModule:> atleast(?N, +Vars, +V)",
        amode: atleast(?,+,+),
        kind: [constraint:[extra:[gccat:atleast]]],
        eg: "
[eclipse 2]: N :: [3,6], atleast(N, [3,A,3,5,3,3], 3).

N = 3
A = A{[-1000000 .. 1000000]}
",
        desc:html("\
   This constraint ensures that at least N elements of Vars have the value V.
</p><p>
   This constraint is a specialisation of the more general count/4
   constraint, with the (#&gt;=) relation, i.e. the occurrences of V
   is greater than or equal to N. Note that the order in which N and V
   occur in the constraint are swapped with respect to occurrences/3
   and count/4; this is for compatibility with the argument order in
   IC (for atmost/3).
<P>
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_gac for domain (generalised arc) consistency. Note that if
   V is a domain variable, then the propagation is weak, achieving
   neither domain or bound consistency until V becomes ground.
 </p><p>
   This constraint is also known as atleast in the global constraint
   catalog, where N is restricted to an integer; the more general 
   count/4 constraint is also known as count in the global 
   constraint catalog, and the constraint is implemented using 
   Gecode's count() constraint.
</p>
"),
	args:["N" : "An integer or (domain) variable (array notation accepted)",
	      "Vars" : "A collection (a la collection_to_list/2) of (domain) variables or integers",
	      "V" : "An integer"],
	fail_if:"   Fails if less than N elements of Vars can be instantiated to V.",
	see_also:[count/4, atmost/3, element/3, occurrences/3, collection_to_list/2]]).


%----------------------------------------------------------------------

:- comment(count/4, [
	summary: "Constrain the number of occurrence of Value in Vars (Occ) to satisfy  the relation Occ Rel N",
        amode: count(+,+,+,+),
        amode: count(?,+,+,+),
        amode: count(?,+,+,?),
        amode: count(+,+,+,?),
        template:"<ConsistencyModule:> count(+Value, ?Vars, +Rel, ?N)",
	args:["?Value" : "An integer or a (domain) variable (array notation accepted)",
	      "?Vars" : "A collection (a la collection_to_list/2) of (domain) variables or integers",
              "+Rel":"One of the atom: #>, #>=, #<, #=<, #=, #\\=",
	      "?N" : "An integer or (domain) variable (array notation accepted)"],
        kind: [constraint:[extra:[gccat:count]]],
        eg: "
[eclipse 33]: count(5, [](4,5,5,4,5), (#>=), 2).   % succeed

[eclipse 34]:  count(5, [](4,5,5,4,5), (#>), 2).   % succeed

[eclipse 35]: count(5, [](4,5,5,4,5), (#=), 2).    % fail

[eclipse 36]: count(5, [](4,5,5,4,5), (#\\=), 2).   % succeed

[eclipse 37]: count(5, [](4,5,5,4,5), (#=<), 2).   % fail

[eclipse 38]: count(5, [](4,5,5,4,5), (#<), 2).    % fail

[eclipse 39]: count(5, [](4,5,5,4,5), (#>=), 3).   % succeed

[eclipse 40]: count(5, [](4,5,5,4,5), (#>), 3).    % fail

[eclipse 41]: count(5, [](4,5,5,4,5), (#=), 3)     % succeed

[eclipse 42]: N :: [3,5], count(3, [3,A,3,5,3,3], (#=), N).

N = 5
A = 3

[eclipse 43]:  N :: [3,5], count(3, [3,A,3,5,3], (#=), N).

N = 3
A = A{[-1000000 .. 2, 4 .. 1000000]}

[eclipse 44]:  N :: [3,5], count(3, [3,A,3,5,3], (#<), N).

N = 5
A = A{[-1000000 .. 1000000]}

[eclipse 45]:  N :: [3,5], count(3, [3,A,3,5,3], (#>), N).

N = 3
A = 3

[eclipse 46]:  N :: [3,5], count(3, [3,A,3,5,3], (#>=), N).

N = 3
A = A{[-1000000 .. 1000000]}

[eclipse 47]: N :: [3,5], count(3, [3,A,3,5,3], (#=<), N).

N = N{[3, 5]}
A = A{[-1000000 .. 1000000]}

[eclipse 48]: N :: [3,5], count(3, [3,A,3,5,3], (#\\=), N).

N = N{[3, 5]}
A = A{[-1000000 .. 1000000]}

",
	desc:html("<P>\
   Constrain the number of occurrences of Value in Vars to satisfy the
   constraint defined by Rel:
<PRE>
          <number of occurrences of Value in Vars> Rel N
</PRE><P>
   Rel can be one of #&gt;, #&gt;=, #&lt;, #=&lt;, #=, #\\= (or equivalently,
   &gt;, &gt;=, &lt;, =&lt;, =, \\=).
</P><P>
   occurrences/3, atmost/3, atleast/3 are defined using count/3. For example,
<PRE>
         atmost(N, Vars, Value)
</PRE>
   is defined by:
<PRE>
        count(Value, Vars, (#=<), N)
</PRE><P>
   This constraint can be embedded in a constraint expression in its
   functional form (without the last argument).
</P><P>
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_gac the (default) and gfd_bc. Both propagation do not achive 
   bound consistency in all cases, and gfd_bc is different from gfd_gac
   only if Value is a domain variable, as its domain is not pruned
   with gfd_bc. 
</P><P>
   This constraint is known as count in the global constraint catalog.
   It is implemented using gecode's count() constraint (variants with
   int or IntVar for argument representing Value).
</p>
")
]).

%----------------------------------------------------------------------

:- comment(among/4, [
	summary: "The number of occurrence (Occ) in Vars of values taken from the set of values specified in Values satisfy  the relation Occ Rel N",
	template:"<ConsistencyModule:> among(+Values, ?Vars, +Rel, ?N)",
	args:["+Values" : "A collection of specifications for integer values",
	      "?Vars" : "A collection (a la collection_to_list/2) of (domain) variables or integers",
              "+Rel":"One of the atom: #>, #>=, #<, #=<, #=, #\\=",
	      "?N" : "An integer or (domain) variable (array notation accepted)"],
        kind: [constraint:[extra:[gccat:counts]]],
        eg:"\
[eclipse 24]: among([1,3,4,9], [4,5,5,4,1,5], (#=), N).

N = 3


[eclipse 25]: among([1..4,9],  [4,5,5,4,1,5], (#=), N).

N = 3


[eclipse 26]:  among([1..4,3,9], [4,5,5,4,1,5], (#=), N). % repeated value

N = 3

[eclipse 2]: among([], [4,5,5,4,1,5], (#=), N).

N = 0

[eclipse 3]: among([1,2,3], [], (#=), N).

N = 0

[eclipse 5]: among([1,3,4,9], [4,5,5,4,1,5], (#\\=), N).

N = N{[-1000000 .. 2, 4 .. 1000000]}

",
        desc:html("<P>\
   Constrain the number of occurrences in Vars of values taken from the set of
   values specified in Value to satisfy the constraint defined by Rel:
<PRE>
          <number of occurrences of values among Values in Vars> Rel N
</PRE><P>
   Rel can be one of #&gt;, #&gt;=, #&lt;, #=&lt;, #=, #\\= (or equivalently,
   &gt;, &gt;=, &lt;, =&lt;, =, \\=).
</P><P>
   Values specify the values whose occurrence are counted, and accept
   the same syntax as domain specification, i.e. each item can be a
   a simple element, or a range Lo .. Hi. Each element is either an
   integer, or is a ground expression that evaluates to an integer. 
</P><P>
   This constraint can be embedded in a constraint expression in its
   functional form (without the last argument).
</P><P>
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_gac for domain (generalised arc) consistency. 
</P><P>
   This constraint is known as counts in the global constraint catalog,
   with among and among_vars being the specialised form with #= as 
   the Rel (i.e. The number of occurrences of values from Values is
   exactly N), the name among is used here to better distinguish this
   constraint from count/4, for counting the occurrences of a single value.
   This constraint is implemented by gecode's count() constraint (the variant
   with an IntSet argument for Values).
</P>
")
]).

%----------------------------------------------------------------------

:- comment(count_matches/4, [
	summary: "The number of the elements in Vars that
 match its corresponding value in Values, Matches, satisfies the
 relation Matches Rel N.",
	template:"<ConsistencyModule:> count_matches(+Values, ?Vars, +Rel, ?N)",
	args:["+Values" : "A collection of M integer values",
	      "?Vars" : "A collection of M (domain) variables or integers",
              "+Rel":"One of the atom: #>, #>=, #<, #=<, #=, #\\=",
	      "?N" : "An integer or (domain) variable (array notation accepted)"],
        kind: [constraint],
        eg: "\
[eclipse 5]: count_matches([1,2,3,4], [A,B,C,D], (#=), N).

A = A{[-1000000 .. 1000000]}
B = B{[-1000000 .. 1000000]}
C = C{[-1000000 .. 1000000]}
D = D{[-1000000 .. 1000000]}
N = N{[0 .. 4]}

[eclipse 6]: L = [A,B,C,D], L :: 4..10, count_matches([1,2,3,4], L, (#=), N).

L = [A{[4 .. 10]}, B{[4 .. 10]}, C{[4 .. 10]}, D{[4 .. 10]}]
A = A{[4 .. 10]}
B = B{[4 .. 10]}
C = C{[4 .. 10]}
D = D{[4 .. 10]}
N = N{[0, 1]}

[eclipse 15]: count_matches([1,2,3,4], [4,3,2,1], (#=), N).

N = 0

[eclipse 16]: count_matches([1,2,3,4], [2,2,3,5],  (#=), N).

N = 2

[eclipse 17]:  count_matches([], [], (#=), N).

N = 0

",
        desc:html("<P>\
   Values and Vars are collections of the same size, and the
   number of elements in Vars taking on the value given by its corresponding 
   element in Values, Matches, is constrained by the relation:
<PRE>
          <Matches> Rel N
</PRE><P>
   Rel can be one of #&gt;, #&gt;=, #&lt;, #=&lt;, #=, #\\= (or equivalently,
   &gt;, &gt;=, &lt;, =&lt;, =, \\=).
</P><P>
   This constraint can be embedded in a constraint expression in its
   functional form (without the last argument).
</P><P>
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_gac for domain (generalised arc) consistency. 
</P><P>
   This constraint is implemented by gecode's count() constraint 
   (variant with an IntArgs for Values). 
</P>
")
]).

%----------------------------------------------------------------------

:- comment(sorted/2, [
    summary:"Sorted is a sorted permutation of Unsorted",
    amode:sorted(+,+),
    amode:sorted(+,-),
    amode:sorted(-,+),
    template:"<ConsistencyModule:> sorted(?Unsorted, ?Sorted)",
    args:["Unsorted":"Collection of N (domain) variables or integers",
    	"Sorted":"Collection of N (domain) variables or integers"],
    kind: [constraint:[extra:[gccat:sort]]],
    eg: "
[eclipse 2]: sorted([1,9,1,5,2|L], [1,1,1,2,5,9]).

L = [1]

[eclipse 3]:  sorted([1,9,1,5,2,1], S).

S = [1, 1, 1, 2, 5, 9]

[eclipse 4]: length(Xs,4), Xs::0..100, sorted(Xs,Ys), Xs = [8,20|_].

Xs = [8, 20, _694{[0 .. 100]}, _714{[0 .. 100]}]
Ys = [_774{[0 .. 8]}, _794{[0 .. 20]}, _814{[8 .. 100]}, _834{[20 .. 100]}]

[eclipse 5]: length(Ys,4), Ys::0..100, sorted(Xs,Ys), Ys = [8,20|_].

Ys = [8, 20, _694{[20 .. 100]}, _714{[20 .. 100]}]
Xs = [_832{[8 .. 100]}, _852{[8 .. 100]}, _872{[8 .. 100]}, _892{[8 .. 100]}]

    ",
    desc:html("\
    Declaratively: The two collections have the same length and Sorted is
    a sorted permutation of Unsorted.
<P>
    Operationally:  the elements in both collections are constrained such
    that their domains are consistent with the assumption that Sorted
    is the sorted version of Unsorted.
<P>
    One of the two arguments can be uninstantiated at call time.
<P>
    Any input variables which is not already a domain variable will be
    turned into a domain variable with default bounds.
<P>
    ConsistencyModule is the optional module specification to give the 
    consistency level for the propagation for this constraint: 
    gfd_bc for bounds consistency.
<P>
    This constraint is known as sort in the global constraint catalog,
    and is implemented using Gecode's sorted() constraint.
<P>
"),
    see_also:[_:sorted/2,sorted/3,ordered/2]
    ]).

:- comment(sorted/3, [
    summary:"Sorted is a sorted permutation (described by Positions) of Unsorted",
    amode:sorted(+,?,?),
    amode:sorted(?,+,?),
    amode:sorted(?,?,+),
    template:"<ConsistencyModule:> sorted(?Unsorted, ?Sorted, ?Positions)",
    args:["Unsorted":"Collection of N (domain) variables or integers",
    	"Sorted":"Collection of N (domain) variables or integers",
    	"Positions":"Collection of N (domain) variables or integers"],
    kind: [constraint:[extra:[gccat:sort_permutation]]],
    desc:html("\
    Declaratively:  Sorted is a sorted permutation of Unsorted.  Positions
    is a collection whose elements range from 1 to N (where N is the 
    cardinality of the collections) indicating the position of each 
    unsorted list element within the sorted list.  The positions are all 
    different. The three collections are constrained to have the same size.
<P>
    Operationally:  the elements in all three collections are constrained
    such that their domains are consistent with the declarative
    meaning.
<P>
    Two of the three arguments can be uninstantiated or partial lists
    at call time.
<P>
    Any input variables which is not already a domain variable will be
    turned into a domain variable with default bounds.
<P>
   Note that the gecode implementation of the constraint use 
   positions starting from 0. An extra dummy element smaller than all
   the elements in List is added so that the position returned correspond
   to the usual ECLiPSe index starting from 1. In addition, the complexity
   of the algorithm used by gecode is linear in time with respect to 
   Max - Min, where Max and Min are the Maximum and Minimum possible values
   for elements in List, respectively. Therefore, this constraint will 
   behave badly for variables with large domain widths. For a version of this
   constraint that uses native Gecode indexing, see sorted_g/3.
<P>
    ConsistencyModule is the optional module specification to give the 
    consistency level for the propagation for this constraint: 
    gfd_bc for bounds consistency.
<P>
    This constraint is known as sort_permutation in the global
    constraint catalog, and is implemented using Gecode's sorted() constraint.
"),
    eg:"
[eclipse 2]: length(Xs,4),Xs :: 1 .. 100,sorted(Xs, Ys, Ps),Xs = [8, 20|_].

Xs = [8, 20, _715{[1 .. 100]}, _735{[1 .. 100]}]
Ys = [_804{[1 .. 8]}, _824{[1 .. 20]}, _844{[8 .. 100]}, _864{[20 .. 100]}]
Ps = [_969{[1 .. 3]}, _989{[2 .. 4]}, _1009{[1 .. 4]}, _1029{[1 .. 4]}]

    ",
    see_also:[_:sorted/3,sorted/2,ordered/2,sorted_g/3]
    ]).

:- comment(sorted_g/3, [
    summary:"Sorted is a sorted permutation (described by Positions) of Unsorted, with native Gecode indexing.",
    amode:sorted_g(+,?,?),
    amode:sorted_g(?,+,?),
    amode:sorted_g(?,?,+),
    template:"<ConsistencyModule:> sorted_g(?Unsorted, ?Sorted, ?Positions)",
    args:["Unsorted":"Collection of N (domain) variables or integers",
    	"Sorted":"Collection of N (domain) variables or integers",
    	"Positions":"Collection of N (domain) variables or integers"],
    see_also: [sorted/3],	
    kind: [constraint:[extra:[gccat:sort_permutation]]],
    desc:html("\
  This version of sorted/3 uses the native Gecode indexing, which starts 
  from 0, i.e. the first element of the collections has index 0. This is 
  different from normal ECLiPSe's indexing, which starts from 1.
</p><p>
  This predicate maps more directly to Gecode's native implementation of 
  the constraint, without the conversion between Gecode and ECLiPSe
  indexing of sorted/3. It may therefore be more efficient, but could also
  be incompatible with existing ECLiPSe code. 
</p><p>
  See sorted/3 for a more detailed description of this predicate.")
]).   

%----------------------------------------------------------------------

:- comment(bool_channeling/3, [
        amode: bool_channeling(?, +, +),
       template:"<ConsistencyModule:> bool_channeling(?Var, +DomainBools, +Min)",
        args: ["Var": "A (domain) variable (array notation accepted)",
               "DomainBools": "A collection of N 0/1 (domain) variables or"
                           " integers",
               "Min": "An integer"],
        summary: "Channel the domain values of Vars to the 0/1 boolean"
                 " variables in DomainBools",
        kind: [constraint:[extra:[gccat:domain_constraint]]],
        see_also: [_:bool_channeling/3],
        eg:"
[eclipse 53]: bool_channeling(V, [0,1,1,0,1,0,0], 3).  % fail

[eclipse 54]:  bool_channeling(V, [0,0,1,0,0,0,B], 3).

V = 5
B = 0

[eclipse 55]: bool_channeling(V, [B1,B2,B3,B4,B5], 6), B4 = 0.

V = V{[6 .. 8, 10]}
B1 = B1{[0, 1]}
B2 = B2{[0, 1]}
B3 = B3{[0, 1]}
B4 = 0
B5 = B5{[0, 1]}

",
        desc: html("\
<P>
    Var is a domain variable whose initial interval is Min..(Min+N),
    and this constraint links the domain values of Var with the N 0/1
    variables in DomainBools such that the i'th variable in DomainBools
    represents the value Min+i, and its value is 0 if the value is not in
    Var's domain, and 1 if Var is assigned the value [Thus, only one variable
    in DomainBools can take the value 1].
</P><P>
    ConsistencyModule is the optional module specification to give the 
    consistency level for the propagation for this constraint: 
    gfd_gac for generalised arc consistency (domain consistency).
</P><P>
    A variant of this constraint, called 'domain_constraint' is in the global 
    constraint catalog. There, instead of having DomainBools and Min, there
    is a collection of Value-Bool pairs, representing a possible domain value
    and its associated 0/1 variable. This constraint is implemented using
    Gecode's channel() constraint (variant with BoolVarArgs and IntVar).
</P>
")]).


%----------------------------------------------------------------------

:- comment(gcc/2, [
        amode: gcc(+,+),
        template:"<ConsistencyModule:> gcc(+Bounds,+Vars)",
        args: ["Bounds":"A list of elements specifying the cardinality of"
                        " values occurring in Vars of the form "
                        "gcc(Low,High,Value) or occ(Occ,Value).",
               "Vars":"A collection of different (domain) variables or integers"
              ],
        summary:"Constrain the cardinality of each Value according to the specification in Bounds.",
        eg: "\
[eclipse 2]: gcc([occ{occ:2,value:3},occ{occ:0,value:5},occ{occ:1,value:6}], 
                 [3,3,8,6]).  % fails -- value 8 not specified in Bounds

[eclipse 3]: gcc([occ{occ:2,value:3},occ{occ:0,value:5},occ{occ:1,value:6},
                 gcc{low:0,high:3,value:8}], [3,3,8,6]).  % succeed
",

        kind: [constraint:[extra:[gccat:[global_cardinality,global_cardinality_low_up]]]],
        see_also: [_:gcc/2],
        desc:html("\
<P>
    This constraint ensures that the cardinality (the number of occurrences)
    of values in Vars conforms to the specifications in Bounds. Bounds is a
    list of specifications in one of the following forms:
<DL><P>
 <DT><STRONG><TT>gcc(Low,High,Value)</TT></STRONG>
    <DD>where Value is an integer, a value that Vars is to be assigned to, 
    and must occur only once as a Value in Bounds, and whose cardinality 
    |Value| is specified by Low =&lt; |Value| =&lt; High, where Low and High are 
    non-negative integers.
 <DT><STRONG><TT>occ(Occ,Value)</TT></STRONG>
    <DD>where Value is an integer, a value that Vars is to be assigned to, 
    and must occur only once as a Value in Bounds, and whose cardinality 
    |Value| is specified by Occ. Occ is either a non-negative integer, or 
    a domain variable whose domain are the possible cardinality for Value.
    Note that if Occ is a non-gfd domain variable, a type error would
    be raised. 
</DL></P><P>
    Note that all values that Vars can take must be specified in Bounds.
</P><P>
    ConsistencyModule is the optional module specification to give the 
    consistency level for the propagation for this constraint: 
    gfd_gac for generalised arc consistency (domain consistency), 
    gfd_bc for bounds consistency, and gfd_vc for value consistency.
</P><P>
    This constraint is known as global_cardinality (occ{} spec) and
    global_cardinality_low_up (gcc{} spec) in the Global Constraint
    Catalog, and is implemented using Gecode's count() constraint (variant 
    with two IntVarArgs and an IntArgs). The semantics is different
    from that given in the catalog, which does not require all values
    that Vars can take be specified by Bounds.
</P>
")
                  ]).


%----------------------------------------------------------------------

:- comment(inverse/2, [
        amode: inverse(+,+),
        args: ["Succ":"A collection of N different (domain) variables or integers",
               "Pred":"A collection  of N different (domain) variables or integers"
              ],
        template:"<ConsistencyModule:> inverse(+Succ,+Pred)",
        summary: "Constrains elements of Succ to be the successors and"
                 " Pred to be the predecessors of nodes in a digraph",
	see_also:[inverse_g/2, inverse/2],
        kind: [constraint:[extra:[gccat:inverse]]],
        desc: html("\
<P>
     Succ and Pred are collections of N elements, representing a digraph of 
     N nodes, where the i'th element of Succ and Pred represents the 
     successor and predecessor of the node i respectively. The constraint 
     enforces each node in the digraph to have one successor and one 
     predecessor node, and that if node y is the successor of node x, then 
     node x is the predecessor of node y.
</P><P>
    One of the two arguments can be uninstantiated or partial list
    at call time.
</P><P>
     Note that the Gecode implementation of this constraint actually index
     from 0. A dummy element is added to the start of Succ and Pred so that
     the indices returned corresponds to ECLiPSe's (starting from 1). A
     version of this constraint using native Gecode indexing is available
     as inverse_g/2.
</P><P>
    ConsistencyModule is the optional module specification to give the 
    consistency level for the propagation for this constraint: 
    gfd_gac for generalised arc consistency (domain consistency), 
    and gfd_vc for value consistency.
</P><P>
     This constraint is known as inverse in the global constraint catalog,
     but with implicit node index based on the position in the list, and
     is implemented using Gecode's channel() constraint (variant with two
     IntVarArgs).
</P>
")]).

:- comment(inverse_g/2, [
        amode: inverse_g(+,+),
        args: ["Succ":"A collection of N different (domain) variables or integers",
               "Pred":"A collection  of N different (domain) variables or integers"
              ],
        template:"<ConsistencyModule:> inverse_g(+Succ,+Pred)",
        summary: "Constrains elements of Succ to be the successors and"
                 " Pred to be the predecessors of nodes in a digraph, using"
		 " native Gecode indexing.",
	see_also:[inverse/2],
        kind: [constraint:[extra:[gccat:inverse]]],
        desc: html("\
  This version of inverse/2 uses the native Gecode indexing, which starts 
  from 0, i.e. the first elements in Succ and Pred has position 0. This is 
  different from normal ECLiPSe's indexing, which starts from 1.
</p><p>
  This predicate maps more directly to Gecode's native implementation of 
  the constraint, without the conversion between Gecode and ECLiPSe
  indexing of inverse/2. It may therefore be more efficient, but could also
  be incompatible with existing ECLiPSe code. 
</p><p>
  See inverse/2 for a more detailed description of this predicate.")
]).   

:- comment(inverse/4, [
        amode: inverse(+,+,+,+),
        args: ["Succ":"A collection of N different(domain)  variables or integers",
               "SuccOffset":"An integer.",
               "Pred":"A collection  of N different (domain) variables or integers",
               "PredOffset":"An integer."
              ],
        template:"<ConsistencyModule:> inverse(+Succ,+SuccOffset,+Pred,+PredOffset)",
        summary: "Constrains elements of Succ (with SuccOffset) to be the successors and"
                 " Pred (with PredOffset) to be the predecessors of nodes in a digraph",
	see_also:[inverse_g/4],
        kind: [constraint:[extra:[gccat:inverse_offset]]],
        desc: html("\
<P>
     Succ and Pred are list of N elements, representing a digraph of N nodes,
     where the i'th element of Succ and Pred represents the successor and
     predecessor of the node i respectively. The constraint enforces each
     node in the digraph to have one successor and one predecessor node, and
     that if the successor of node y minus SuccOffset is equal to x, then
     the predecessor of node x minus PredOffset is equal to y.
</P><P>
    One of the two collection arguments (Succ and Pred) can be uninstantiated 
    or partial list at call time.
</P><P>
     Note that the gecode implementation of this constraint actually index
     from 0. The SuccOfffset and PredOffset are adjusted accordingly before
     posting to gecode so that the indices returned corresponds to 
     ECLiPSe's (starting from 1).
</P><P>
    ConsistencyModule is the optional module specification to give the 
    consistency level for the propagation for this constraint: 
    gfd_gac for generalised arc consistency (domain consistency), 
    and gfd_vc for value consistency.
</P><P>
     This constraint is known as inverse_offset in the global constraint 
     catalog, but with implicit node index based on the position in the list.  
     It is implemented using Gecode's channel() constraint (variant with two
     IntVarArgs and two integer offsets).
</P>
")]).

:- comment(inverse_g/4, [
        amode: inverse_g(+,+,+,+),
        args: ["Succ":"A collection of N different (domain) variables or integers",
               "SuccOffset":"An integer.",
               "Pred":"A collection  of N different (domain) variables or integers",
               "PredOffset":"An integer."
              ],
        template:"<ConsistencyModule:> inverse_g(+Succ,+SuccOffset,+Pred,+PredOffset)",
        summary: "Constrains elements of Succ (with SuccOffset) to be the successors and"
                 " Pred (with PredOffset) to be the predecessors of nodes in a digraph",
	see_also:[inverse/4],
        kind: [constraint:[extra:[gccat:inverse_offset]]],
        desc: html("\
  This version of inverse/4 uses the native Gecode indexing, which starts 
  from 0, i.e. the first elements in Succ and Pred has position 0. This is 
  different from normal ECLiPSe's indexing, which starts from 1.
</p><p>
  This predicate maps directly to Gecode's native implementation of 
  the constraint, without the offset adjustments of inverse/4.
</p><p>
  See inverse/4 for a more detailed description of this predicate.")
]).   

%----------------------------------------------------------------------

:- comment(circuit/1, [
        amode: circuit(+),
        args: ["Succ":"A collection of different (domain) variables or integers"
              ],
        template:"<ConsistencyModule:> circuit(+Succ)",
        summary: "Constrains elements in Succ to form a Hamiltonian circuit.", 
	see_also: [circuit_g/1,circuit_offset_g/2],
        kind: [constraint:[extra:[gccat:circuit]]],
        eg: "\
[eclipse 7]: circuit([2,A,4,1]).

A = 3

[eclipse 2]: circuit([]).

No (0.00s cpu)

[eclipse 11]: circuit([A]).

A = 1

",
        desc: html("<P>\
  Succ is a collection of N elements presenting a digraph of N nodes, where
  the i'th element of Succ represents the successor to node i. The constraint
  enforces Succ to form a Hamiltonian circuit, a path through every node in
  the graph, visiting each node once and forming a circuit.</P><P>

  Note that the Gecode implementation of this constraint has index (node id)
  starting from 0, rather than 1. This constraint is actually posted
  as circuit_offset_g/2 with an offset of 1. A version of this constraint
  with native Gecode indexing is available as circuit_g/1.
</P><P>
  ConsistencyModule is the optional module specification to give the 
  consistency level for the propagation for this constraint: 
  gfd_gac for generalised arc consistency (domain consistency), 
  and gfd_vc for value consistency.
</P><P>
  This constraint is known as circuit in the global constraint catalog. It is
  implemented with Gecode's circuit() constraint with an offset of 1.
</P>
")
                      ]).

:- comment(circuit_g/1, [
        amode: circuit_g(+),
        args: ["Succ":"A collection of different (domain) variables or integers"
              ],
        template:"<ConsistencyModule:> circuit_g(+Succ)",
        summary: "Constrains elements in Succ to form a Hamiltonian circuit, with native Gecode indexing.", 
	see_also: [circuit/1],
        kind: [constraint:[extra:[gccat:circuit]]],
        eg: "
circuit_g([A,2,3,0])

A = 1


",
        desc: html("<P>\
  This version of circuit/1 uses the native Gecode indexing, which starts 
  from 0. This is different from normal ECLiPSe's indexing, which starts 
  from 1, and may be incompatible with existing ECLiPSe code. 
</p><p>
  See circuit/1 for a more detailed description of this predicate.")
]).   

%----------------------------------------------------------------------

:- comment(circuit/3, [
        amode: circuit(+,++,?),
        args: ["Succ":"A collection of N different (domain) variables or integers",
               "CostMatrix":"A NxN matrix collection of integers.",
               "Cost": "A (domain) variable or integer (array notation accepted)."
              ],
        template:"<ConsistencyModule:> circuit(+Succ,++CostMatrix,?Cost)",
        summary: "Constrains elements in Succ to form a Hamiltonian circuit with cost Cost.", 
        see_also: [circuit_offset_g/4, circuit/1,
                   circuit/4, circuit_g/3],
        kind: [constraint],
        eg: "\
[eclipse 9]: CostM = []([](0,3,5,7),[](4,0,9,6),[](2,1,0,5),[](-7,8,-2,0)),
             circuit([2,3,4,1], CostM, C).   

CostM = []([](0, 3, 5, 7), [](4, 0, 9, 6), [](2, 1, 0, 5), [](-7, 8, -2, 0))
C = 10
",
        desc: html("<P>\
  Succ is a collection of N elements presenting a digraph of N nodes, where
  the i'th element of Succ represents the successor to node i. The constraint
  enforces Succ to form a Hamiltonian circuit, a path through every node in
  the graph, visiting each node once and forming a circuit. Additionally,
  CostMatrix specifies the cost for traversing between each pair of nodes:
  CostMatrix[i,j] represents the cost of travelling from node i to j, and 
  Cost is constrained to the total cost for the circuit.
</P><P>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</P><P>
  Note that the Gecode implementation of this constraint has index (node id)
  starting from 0, rather than 1. This constraint is actually posted
  as circuit_offset_g/4 with an offset of 1. A version of this constraint
  with native Gecode indexing is available as circuit_g/3.
</P><P>
  ConsistencyModule is the optional module specification to give the 
  consistency level for the propagation for this constraint: 
  gfd_gac for generalised arc consistency (domain consistency), 
  and gfd_vc for value consistency.
</P><P>
  This constraint is implemented by Gecode's circuit() constraint (variant with 
  cost), using an offset of 1.
</P>
")
                      ]).

:- comment(circuit_g/3, [
        amode: circuit_g(+,++,?),
        args: ["Succ":"A collection of N different (domain) variables or integers",
               "CostMatrix":"A NxN matrix of integers.",
               "Cost": "A (domain) variable or integer (array notation accepted)."
              ],
        template:"<ConsistencyModule:> circuit_g(+Succ,++CostMatrix,?Cost)",
        summary: "Constrains elements in Succ to form a Hamiltonian circuit with cost Cost. This version uses native Gecode indexing.", 
	see_also: [circuit/3],
        kind: [constraint],
        eg: "\
[eclipse 10]: CostM = []([](0,3,5,7),[](4,0,9,6),[](2,1,0,5),[](-7,8,-2,0)),
        circuit_g([1,2,3,0], CostM, C).   

CostM = []([](0, 3, 5, 7), [](4, 0, 9, 6), [](2, 1, 0, 5), [](-7, 8, -2, 0))
C = 10


[eclipse 6]: CostM = []([](0,3,5,7),[](4,0,9,6),[](2,1,0,5),[](-7,8,-2,0)),
        C #= circuit([2,3,4,1], CostM) + 1.

CostM = []([](0, 3, 5, 7), [](4, 0, 9, 6), [](2, 1, 0, 5), [](-7, 8, -2, 0))
C = 11

",
        desc: html("<P>\
  This version of circuit/3 uses the native Gecode indexing, which starts 
  from 0. This is different from normal ECLiPSe's indexing, which starts from 1.</p><p>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</P><P>
  See circuit/3 for a more detailed description of this predicate.")
]).   

%----------------------------------------------------------------------

:- comment(circuit/4, [
        amode: circuit(+,++,+,?),
        args: ["Succ":"A collection of N different (domain) variables or integers",
               "CostMatrix":"A NxN matrix collection of integers",
               "ArcCosts": "A collection of N (domain) variables or integers.",
               "Cost": "A (domain) variable or integer (array notation accepted)."
              ],
        template:"<ConsistencyModule:> circuit(+Succ,++CostMatrix,+ArcCosts,?Cost)",
	see_also:[circuit/1,circuit/3,circuit_g/4],
        summary: "Constrains elements in Succ to form a Hamiltonian"
                 " circuit with cost Cost.", 
        kind: [constraint],
        eg:"\
[eclipse 5]: CostM = []([](0,3,5,7),[](4,0,9,6),[](2,1,0,5),[](-7,8,-2,0)),
        circuit([2,3,4,1], CostM,        [C1,C2,C3,C4], C).

CostM = []([](0, 3, 5, 7), [](4, 0, 9, 6), [](2, 1, 0, 5), [](-7, 8, -2, 0))
C1 = 3
C2 = 9
C3 = 5
C4 = -7
C = 10
",
        desc: html("<P>\
  Succ is a collection of N elements presenting a digraph of N nodes, where
  the i'th element of Succ represents the successor to node i. The constraint
  enforces Succ to form a Hamiltonian circuit, a path through every node in
  the graph, visiting each node once and forming a circuit. Additionally,
  CostMatrix specifies the cost for traversing between each pair of nodes:
  CostMatrix[i,j] represents the cost of travelling from node i to j, and 
  Cost is constrained to the total Cost for the circuit. The i'th element of 
  ArcCosts is constrained to the cost of the arc in the circuit from node i.
</P><P>
  Note that the Gecode implementation of this constraint has index (node id)
  starting from 0, rather than 1. This constraint is actually posted
  as circuit_offset_g/5 with an offset of 1. A version of this constraint
  with native Gecode indexing is available as circuit_g/4.
</P><P>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</P><P>
  ConsistencyModule is the optional module specification to give the 
  consistency level for the propagation for this constraint: 
  gfd_gac for generalised arc consistency (domain consistency), 
  and gfd_vc for value consistency.
</P><P>
  This constraint is implemented by Gecode's circuit() constraint (variant with 
  cost and arc costs), using an offset of 1.
</P>
")
                      ]).

:- comment(circuit_g/4, [
        amode: circuit_g(+,++,+,?),
        args: ["Succ":"A collection of N different (domain) variables or integers",
               "CostMatrix":"A NxN matrix collection of integers",
               "ArcCosts": "A collection of N (domain) variables or integers.",
               "Cost": "An (domain) variable or integer (array notation accepted)."
              ],
        template:"<ConsistencyModule:> circuit_g(+Succ,++CostMatrix,+ArcCosts,?Cost)",
        summary: "Constrains elements in Succ to form a Hamiltonian circuit with cost Cost, using native Gecode indexing.", 
	see_also:[circuit/4],
        kind: [constraint],
        eg: "\
CostM = []([](0,3,5,7),[](4,0,9,6),[](2,1,0,5),[](-7,8,-2,0)),
        circuit_g([1,2,3,0], CostM, [C0,C1,C2,C3], C).
",
        desc: html("<P>\
  This version of circuit/4 uses the native Gecode indexing, which starts 
  from 0. This is different from normal ECLiPSe's indexing, which starts from 1.
</P><P>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</p><p>
  See circuit/4 for a more detailed description of this predicate.")
]).   

%----------------------------------------------------------------------

:- comment(circuit_offset/2, [
        amode: circuit_offset(+,+),
        args: ["Succ":"A collection of different (domain) variables or integers",
               "Offset":"Offset for Succ (An integer)"
              ],
        template:"<ConsistencyModule:> circuit_offset(+Succ,+Offset)",
        summary: "Constrains elements (offset by Offset) in Succ to form a Hamiltonian circuit.", 
	see_also: [circuit_offset_g/2],
        kind: [constraint],
        desc: html("<P>\
  Succ is a collection of N elements presenting a digraph of N nodes, where
  the value of the i'th element of Succ - Offset represents the successor to 
  node i. The constraint enforces Succ to form a Hamiltonian circuit,
  a path through every node in the graph, visiting each node once and
  forming a circuit.</P><P>

  Note that the Gecode implementation of this constraint has index (node id)
  starting from 0, rather than 1. The value of Offset is incremented by 1 
  when the constraint is posted to Gecode.  A version of this constraint with
  native Gecode indexing, i.e. without adjusting Offset, is available 
  as circuit_offset_g/2.
</P><P>
  ConsistencyModule is the optional module specification to give the 
  consistency level for the propagation for this constraint: 
  gfd_gac for generalised arc consistency (domain consistency), 
  and gfd_vc for value consistency.
</P><P>
  This constraint is implemented by Gecode's circuit() constraint, using an 
  offset of Offset + 1.
</P>
")
]).

:- comment(circuit_offset_g/2, [
        amode: circuit_offset_g(+,+),
        args: ["Succ":"A collection of different (domain) variables or integers",
               "Offset":"Offset for Succ (An integer)"
              ],
        template:"<ConsistencyModule:> circuit_offset_g(+Succ, +Offset)",
        summary: "Constrains elements (offset by Offset) in Succ to form a Hamiltonian circuit, with native Gecode indexing.", 
	see_also: [circuit_offset/2],
        kind: [constraint],
        desc: html("<P>\
  This version of circuit_offset/2 uses the native Gecode indexing, which
  starts from 0. This is different from normal ECLiPSe's indexing, which
  starts from 1. Offset is not adjusted in this version. This version of 
  the constraint is provided for completeness, in case the user is using
  native Gecode indexing in their code, so that Offset does not need to
  be adjusted manually by the user. 
 </p><p>
  See circuit_offset/2 for a more detailed description of this predicate.")
]).   

%----------------------------------------------------------------------

:- comment(circuit_offset/4, [
        amode: circuit_offset(+,+,++,?),
        args: ["Succ":"A collection of N different (domain) variables or integers",
               "Offset":"Offset for Succ (An integer)",
               "CostMatrix":"A NxN matrix collection of integers.",
               "Cost": "A (domain) variable or integer (array notation accepted)."
              ],
        template:"<ConsistencyModule:> circuit_offset(+Succ,+Offset,++CostMatrix,?Cost)",
        summary: "Constrains elements in Succ (offset by Offset) to form a Hamiltonian circuit with cost Cost.", 
        see_also: [circuit_offset_g/4, circuit_offset/2,
                   circuit_offset/5, circuit/3],
        kind: [constraint],
        desc: html("<P>\
  Succ is a collection of N elements presenting a digraph of N nodes, where
  the value of the i'th element of Succ - Offset represents the successor to 
  node i. The constraint enforces Succ to form a Hamiltonian circuit,
  a path through every node in the graph, visiting each node once and
  forming a circuit. Additionally,CostMatrix specifies the cost for 
  traversing between each pair of nodes:
  CostMatrix[i,j] represents the cost of travelling from node i to j, and 
  Cost is constrained to the total cost for the circuit.
</P><P>
  Note that the Gecode implementation of this constraint has index (node id)
  starting from 0, rather than 1. The value of Offset is incremented by 1 
  when the constraint is posted to Gecode.  A version of this constraint with
  native Gecode indexing, i.e. without adjusting Offset, is available 
  as circuit_offset_g/4.
</P><P>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</P><P>
  ConsistencyModule is the optional module specification to give the 
  consistency level for the propagation for this constraint: 
  gfd_gac for generalised arc consistency (domain consistency), 
  and gfd_vc for value consistency.
</P><P>
  This constraint is implemented by Gecode's circuit() constraint (variant with 
  cost), using an offset of Offset + 1.
</P>
")
                      ]).

:- comment(circuit_offset_g/4, [
        amode: circuit_offset_g(+,+,++,?),
        args: ["Succ":"A collection of N different (domain) variables or integers",
               "Offset":"Offset for Succ (An integer)",
               "CostMatrix":"A NxN matrix collection of integers.",
               "Cost": "A (domain variable or integer (array notation accepted)."
              ],
        template:"<ConsistencyModule:> circuit_offset_g(+Succ,+Offset,++CostMatrix,?Cost)",
        summary: "Constrains elements in Succ (offset by Offset) to form a Hamiltonian circuit with cost Cost. This version uses native Gecode indexing.", 
	see_also: [circuit_offset/4],
        kind: [constraint],
        desc: html("<P>\
  This version of circuit_offset/4 uses the native Gecode indexing, which
  starts from 0. This is different from normal ECLiPSe's indexing, which
  starts from 1. Offset is not adjusted in this version. This version of 
  the constraint is provided for completeness, in case the user is using
  native Gecode indexing in their code, so that Offset does not need to
  be adjusted manually by the user. 
</P><P>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</p><p>
  See circuit_offset/4 for a more detailed description of this predicate.")
]).   

%----------------------------------------------------------------------

:- comment(circuit_offset/5, [
        amode: circuit_offset(+,+,++,+,?),
        args: ["Succ":"A collection of N different (domain) variables or integers",
               "Offset":"Offset for Succ (An integer)",
               "CostMatrix":"A NxN matrix collection of integers",
               "ArcCosts": "A collection of N (domain) variables or integers.",
               "Cost": "A (domain) variable or integer (array notation accepted)."
              ],
        template:"<ConsistencyModule:> circuit_offset(+Succ,+Offset,++CostMatrix,+ArcCosts,?Cost)",
	see_also:[circuit_offset/2,circuit_offset/4,circuit_offset_g/5],
        summary: "Constrains elements in Succ (offset by Offset) to form a Hamiltonian circuit with cost Cost.", 
        kind: [constraint],
        desc: html("<P>\
  Succ is a collection of N elements presenting a digraph of N nodes, where
  the value of the i'th element of Succ - Offset represents the successor to 
  node i. The constraint enforces Succ to form a Hamiltonian circuit,
  a path through every node in the graph, visiting each node once and
  forming a circuit. Additionally,CostMatrix specifies the cost for 
  traversing between each pair of nodes:
  CostMatrix[i,j] represents the cost of travelling from node i to j, and 
  Cost is constrained to the total cost for the circuit. The i'th element of 
  ArcCosts is constrained to the cost of the arc in the circuit from node i.
</P><P>
  Note that the Gecode implementation of this constraint has index (node id)
  starting from 0, rather than 1. The value of Offset is incremented by 1 
  when the constraint is posted to Gecode.  A version of this constraint with
  native Gecode indexing, i.e. without adjusting Offset, is available 
  as circuit_offset_g/5.
</P><P>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</P><P>
  ConsistencyModule is the optional module specification to give the 
  consistency level for the propagation for this constraint: 
  gfd_gac for generalised arc consistency (domain consistency), 
  and gfd_vc for value consistency.
</P><P>
  This constraint is implemented by Gecode's circuit() constraint (variant with 
  cost and arc costs), using an offset of Offset + 1.
</P>
")
]).

:- comment(circuit_offset_g/5, [
        amode: circuit_offset_g(+,+,++,+,?),
        args: ["Succ":"A collection of N different (domain) variables or integers",
               "Offset":"Offset for Succ (An integer)",
               "CostMatrix":"A NxN matrix collection of integers",
               "ArcCosts": "A collection of N (domain) variables or integers.",
               "Cost": "A (domain) variable or integer (array notation accepted)."
              ],
        template:"<ConsistencyModule:> circuit_offset_g(+Succ,+Offset,++CostMatrix,+ArcCosts,?Cost)",
        summary: "Constrains elements in Succ (offset by Offset) to form a Hamiltonian circuit with cost Cost, using native Gecode indexing.", 
	see_also:[circuit_offset/5],
        kind: [constraint],
        desc: html("<P>\
  This version of circuit_offset/5 uses the native Gecode indexing, which starts 
  from 0. This is different from normal ECLiPSe's indexing, which starts from 1.
</P><P>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</p>")
]).   

%----------------------------------------------------------------------

:- comment(ham_path/3, [
        amode: ham_path(?,?,+),
        args: ["Start": "An integer or (domain) variable (array notation accepted)",
               "End": "An integer or (domain) variable (array notation accepted)",
               "Succ":"A collection of different (domain) variables or integers"
              ],
        template:"<ConsistencyModule:> ham_path(?Start,?End,+Succ)",
        summary: "Constrains elements in Succ to form a Hamiltonian path from Start to End.",
	see_also: [ham_path_g/3,ham_path_offset_g/4],
        kind: [constraint],
        eg: "\
[eclipse 5]: ham_path(S,E,[X]).

S = 1
E = 1
X = 2


[eclipse 2]: ham_path(S,E,[A,B]).

S = S{[1, 2]}
E = E{[1, 2]}
A = A{[2, 3]}
B = B{[1, 3]}

[eclipse 3]: ham_path(2,1,[A,B]).

A = 3
B = 1

[eclipse 5]: ham_path(S,E,[2,4,1]).

S = 3
E = 2

",
        desc: html("<P>\
  Succ is a collection of N elements presenting a digraph of N nodes, where
  the i'th element of Succ represents the successor to node i. The constraint
  enforces Succ to form a Hamiltonian path, a path through every node in
  the graph, visiting each node once, with Start giving the first node
  of the path, and End giving the last node of the path. Note that the
  Succ of the last node will be N+1, i.e. a dummy node not in the graph. 
</P><P>
  Note that the Gecode implementation of this constraint has index (node id)
  starting from 0, rather than 1. This constraint is actually posted
  as ham_path_offset_g/3 with an offset of 1. A version of this constraint
  with native Gecode indexing is available as circuit_g/1.
</P><P>
  ConsistencyModule is the optional module specification to give the 
  consistency level for the propagation for this constraint: 
  gfd_gac for generalised arc consistency (domain consistency), 
  and gfd_vc for value consistency.
</P><P>
  This constraint is implemented by Gecode's path() constraint, using an offset 
  of 1.
</P>
")
                      ]).

:- comment(ham_path_g/3, [
        amode: ham_path_g(?,?,+),
        args: ["Start": "An integer or (domain) variable (array notation accepted)",
               "End": "An integer or (domain) variable (array notation accepted)",
               "Succ":"A collection of different (domain) variables or integers"
              ],
        template:"<ConsistencyModule:> ham_path_g(?Start,?End,+Succ)",
        summary: "Constrains elements in Succ to form a Hamiltonian path from Start to End, with native Gecode indexing.", 
	see_also: [ham_path/3],
        kind: [constraint],
        eg: "\
[eclipse 6]: ham_path_g(S,E,[A,B]).

S = S{[0, 1]}
E = E{[0, 1]}
A = A{[1, 2]}
B = B{[0, 2]}

[eclipse 7]: ham_path_g(1,0,[A,B]).

A = 2
B = 0


[eclipse 8]: ham_path_g(S,E,[1,3,0]).

S = 2
E = 1

",
        desc: html("<P>\
  This version of ham_path/3 uses the native Gecode indexing, which starts 
  from 0. This is different from normal ECLiPSe's indexing, which starts 
  from 1, and may be incompatible with existing ECLiPSe code. 
</p><p>
  See ham_path/3 for a more detailed description of this predicate.")
]).   

%----------------------------------------------------------------------

:- comment(ham_path/5, [
        amode: ham_path(?,?,+,++,?),
        args: ["Start": "An integer or )domain) variable (array notation accepted)",
               "End": "An integer or (domain) variable (array notation accepted)",
               "Succ":"A collection of N different (domain) variables or integers",
               "CostMatrix":"A NxN matrix collection of integers.",
               "Cost": "A (domain) variable or integer (array notation accepted)."
              ],
        template:"<ConsistencyModule:> ham_path(?Start,?End,+Succ,++CostMatrix,?Cost)",
        summary: "Constrains elements in Succ to form a Hamiltonian path from Start to End with cost Cost.",
        see_also: [ham_path_offset_g/6, ham_path/3,
                   ham_path/6, ham_path_g/5],
        kind: [constraint],
        eg: "\
[eclipse 2]: CostM = []([](0,3,5,7),[](4,0,9,6),[](2,1,0,5),[](-7,8,-2,0)),
        ham_path(4,3,[2,3,5,1], CostM, C).

CostM = []([](0, 3, 5, 7), [](4, 0, 9, 6), [](2, 1, 0, 5), [](-7, 8, -2, 0))
C = 5

",
        desc: html("<P>\
  Succ is a collection of N elements presenting a digraph of N nodes, where
  the i'th element of Succ represents the successor to node i. The constraint
  enforces Succ to form a Hamiltonian path, a path through every node in
  the graph, visiting each node once, with Start giving the first node
  of the path, and End giving the last node of the path. Note that the
  Succ of the last node will be N+1, i.e. a dummy node not in the graph. 
  Additionally, CostMatrix specifies the cost for traversing between
  each pair of nodes: CostMatrix[i,j] represents the cost of
  travelling from node i to j, and Cost is constrained to the total cost 
  for the path.
</P><P>
  Note that the Gecode implementation of this constraint has index (node id)
  starting from 0, rather than 1. This constraint is actually posted
  as ham_path_offset_g/4 with an offset of 1. A version of this constraint
  with native Gecode indexing is available as ham_path_g/3.
</P><P>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</P><P>
  ConsistencyModule is the optional module specification to give the 
  consistency level for the propagation for this constraint: 
  gfd_gac for generalised arc consistency (domain consistency), 
  and gfd_vc for value consistency.
</P><P>
  This constraint is implemented by Gecode's path() constraint (variant with 
  cost), using an offset of 1.
</P>
")
                      ]).

:- comment(ham_path_g/5, [
        amode: ham_path_g(?,?,+,++,?),
        args: ["Start": "An integer or (domain) variable (array notation accepted)",
               "End": "An integer or (domain) variable (array notation accepted)",
               "Succ":"A collection of N different (domain) variables or integers",
               "CostMatrix":"A NxN matrix collection of integers.",
               "Cost": "A )domain) variable or integer (array notation accepted)."
              ],
        template:"<ConsistencyModule:> ham_path_g(?Start,?End,+Succ,++CostMatrix,?Cost)",
        summary: "Constrains elements in Succ to form a Hamiltonian path from Start to End with cost Cost. This version uses native Gecode indexing.", 
	see_also: [ham_path/5],
        kind: [constraint],
        eg: "\
[eclipse 2]: CostM = []([](0,3,5,7),[](4,0,9,6),[](2,1,0,5),[](-7,8,-2,0)),
        ham_path_g(3,2,[1,2,4,0], CostM, C).

CostM = []([](0, 3, 5, 7), [](4, 0, 9, 6), [](2, 1, 0, 5), [](-7, 8, -2, 0))
C = 5
",
        desc: html("<P>\
  This version of ham_path/5 uses the native Gecode indexing, which starts 
  from 0. This is different from normal ECLiPSe's indexing, which starts from 1.
</P><P>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</p><p>
  See ham_path/5 for a more detailed description of this predicate.")
]).   

%----------------------------------------------------------------------

:- comment(ham_path/6, [
        amode: ham_path(?,?,+,++,+,?),
        args: ["Start": "An integer or (domain) variable (array notation accepted)",
               "End": "An integer or (domain) variable (array notation accepted)",
               "Succ":"A collection of N different (domain) variables or integers",
               "CostMatrix":"A NxN matrix collection of integers",
               "ArcCosts": "A collection of N (domain) variables or integers.",
               "Cost": "A (domain) variable or integer (array notation accepted)."
              ],
        template:"<ConsistencyModule:> ham_path(?Start,?End,+Succ,++CostMatrix,+ArcCosts,?Cost)",
	see_also:[ham_path/3,ham_path/5,ham_path_g/6],
        summary: "Constrains elements in Succ to form a Hamiltonian path from Start to End with cost Cost.", 
        kind: [constraint],
        eg: "\
[eclipse 2]:  CostM = []([](0,3,5,7),[](4,0,9,6),[](2,1,0,5),[](-7,8,-2,0)),
              ham_path(4,3,[2,3,5,1], CostM, [C1,C2,C3,C4], C).

CostM = []([](0, 3, 5, 7), [](4, 0, 9, 6), [](2, 1, 0, 5), [](-7, 8, -2, 0))
C1 = 3
C2 = 9
C3 = 0
C4 = -7
C = 5
",

        desc: html("<P>\
  Succ is a collection of N elements presenting a digraph of N nodes, where
  the i'th element of Succ represents the successor to node i. The constraint
  enforces Succ to form a Hamiltonian path, a path through every node in
  the graph, visiting each node once, with Start giving the first node
  of the path, and End giving the last node of the path. Note that the
  Succ of the last node will be N+1, i.e. a dummy node not in the graph. 
  Additionally, CostMatrix specifies the cost for traversing between
  each pair of nodes: CostMatrix[i,j] represents the cost of
  travelling from node i to j, and Cost is constrained to the total cost 
  for the path. The i'th element of ArcCosts is constrained to the cost of 
  the arc in the path from node i.
</P><P>
  Note that the Gecode implementation of this constraint has index (node id)
  starting from 0, rather than 1. This constraint is actually posted
  as ham_path_offset_g/7 with an offset of 1. A version of this constraint
  with native Gecode indexing is available as ham_path_g/6.
</P><P>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</P><P>
  ConsistencyModule is the optional module specification to give the 
  consistency level for the propagation for this constraint: 
  gfd_gac for generalised arc consistency (domain consistency), 
  and gfd_vc for value consistency.
</P><P>
  This constraint is implemented by Gecode's path() constraint (variant with 
  cost and arc costs), using an offset of 1.
</P>
")
                      ]).

:- comment(ham_path_g/6, [
        amode: ham_path_g(?,?,+,++,+,?),
        args: ["Start": "An integer or (domain) variable (array notation accepted)",
               "End": "An integer or (domain) variable (array notation accepted)",
               "Succ":"A collection of N different (domain) variables or integers",
               "CostMatrix":"A NxN matrix collection of integers",
               "ArcCosts": "A collection of N (domain) variables or integers.",
               "Cost": "A  (domain) variable or integer (array notation accepted)."
              ],
        template:"<ConsistencyModule:> ham_path_g(?Start,?End,+Succ,++CostMatrix,+ArcCosts,?Cost)",
        summary: "Constrains elements in Succ to form a Hamiltonian path from Start to End, with cost Cost, using native Gecode indexing.", 
	see_also:[ham_path/6],
        kind: [constraint],
        eg:"\
[eclipse 3]: CostM = []([](0,3,5,7),[](4,0,9,6),[](2,1,0,5),[](-7,8,-2,0)),
        ham_path_g(3,2,[1,2,4,0], CostM, [C0,C1,C2,C3], C).

CostM = []([](0, 3, 5, 7), [](4, 0, 9, 6), [](2, 1, 0, 5), [](-7, 8, -2, 0))
C0 = 3
C1 = 9
C2 = 0
C3 = -7
C = 5
",
        desc: html("<P>\
  This version of ham_path/6 uses the native Gecode indexing, which starts 
  from 0. This is different from normal ECLiPSe's indexing, which starts from 1.
</P><P>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</p><p>
  See ham_path/6 for a more detailed description of this constraint.")
]).   

%----------------------------------------------------------------------

:- comment(ham_path_offset/4, [
        amode: ham_path_offset(?,?,+,+),
        args: ["Start": "An integer or (domain) variable (array notation accepted)",
               "End": "An integer or (domain) variable (array notation accepted)",
               "Succ":"A collection of different (domain) variables or integers",
               "Offset":"Offset for Succ (An integer)"
              ],
        template:"<ConsistencyModule:> ham_path_offset(?Start,?End,+Succ,+Offset)",
        summary: "Constrains elements (offset by Offset) in Succ to form a Hamiltonian path from Start to End.", 
	see_also: [ham_path_offset_g/4],
        kind: [constraint],
        desc: html("<P>\
  Succ is a collection of N elements presenting a digraph of N nodes, where
  the i'th element of Succ represents the successor to node i. The constraint
  enforces (Succ -Offset) to form a Hamiltonian path, a path through
  every node in the graph, visiting each node once, with Start giving
  the first node of the path, and End giving the last node of the path.
  Note that the Succ of the last node will be N+1, i.e. a dummy node
  not in the graph. 
</P><P>
  Note that the Gecode implementation of this constraint has index (node id)
  starting from 0, rather than 1. The value of Offset is incremented by 1 
  when the constraint is posted to Gecode.  A version of this constraint with
  native Gecode indexing, i.e. without adjusting Offset, is available 
  as ham_path_offset_g/4.
</P><P>
  ConsistencyModule is the optional module specification to give the 
  consistency level for the propagation for this constraint: 
  gfd_gac for generalised arc consistency (domain consistency), 
  and gfd_vc for value consistency.
</P><P>
  This constraint is implemented by Gecode's path() constraint, with
  an actual offset of 1 + Offset.
</P>
")
                      ]).

:- comment(ham_path_offset_g/4, [
        amode: ham_path_offset_g(?,?,+,+),
        args: ["Start": "An integer or (domain) variable (array notation accepted)",
               "End": "An integer or (domain) variable (array notation accepted)",
               "Succ":"A collection of different (domain) variables or integers",
               "Offset":"Offset for Succ (An integer)"
              ],
        template:"<ConsistencyModule:> ham_path_g(?Start,?End,+Succ,+Offset)",
        summary: "Constrains elements (offset by Offset) in Succ to form a Hamiltonian path from Start to End, with native Gecode indexing.", 
	see_also: [ham_path_offset/4],
        kind: [constraint],
        desc: html("<P>\
  This version of ham_path_offset/4 uses the native Gecode indexing, which
  starts from 0. This is different from normal ECLiPSe's indexing, which
  starts from 1. Offset is not adjusted in this version. This version of 
  the constraint is provided for completeness, in case the user is using
  native Gecode indexing in their code, so that Offset does not need to
  be adjusted manually by the user. 
 </p><p>
  See ham_path_offset/4 for a more detailed description of this predicate.")
]).   

%----------------------------------------------------------------------

:- comment(ham_path_offset/6, [
        amode: ham_path_offset(?,?,+,+,++,?),
        args: ["Start": "An integer or (domain) variable (array notation accepted)",
               "End": "An integer or (domain) variable (array notation accepted)",
               "Succ":"A collection of N different (domain) variables or integers",
               "Offset":"Offset for Succ (An integer)",
               "CostMatrix":"A NxN matrix collection of integers.",
               "Cost": "A (domain) variable or integer (array notation accepted)."
              ],
        template:"<ConsistencyModule:> ham_path_offset(?Start,?End,+Succ,+Offset,++CostMatrix,?Cost)",
        summary: "Constrains elements in Succ (offset by Offset) to form a Hamiltonian path from Start to End with cost Cost.", 
        see_also: [ham_path_offset_g/6, ham_path_offset/4,
                   ham_path_offset/7, ham_path/5],
        kind: [constraint],
        desc: html("<P>\
  Succ is a collection of N elements presenting a digraph of N nodes, where the
  i'th element of (Succ - Offset) represents the successor to node i.The
  constraint enforces Succ to form a Hamiltonian path, a path through every
  node in the graph, visiting each node once, with Start giving the first node
  of the path, and End giving the last node of the path. Note that the
  Succ of the last node will be N+1, i.e. a dummy node not in the graph. 
  Additionally, CostMatrix specifies the cost for traversing between
  each pair of nodes: CostMatrix[i,j] represents the cost of
  travelling from node i to j, and Cost is constrained to the total cost 
  for the path.
</P><P>
  Note that the Gecode implementation of this constraint has index (node id)
  starting from 0, rather than 1. The value of Offset is incremented by 1 
  when the constraint is posted to Gecode.  A version of this constraint with
  native Gecode indexing, i.e. without adjusting Offset, is available 
  as ham_path_offset_g/6.
</P><P>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</P><P>
  ConsistencyModule is the optional module specification to give the 
  consistency level for the propagation for this constraint: 
  gfd_gac for generalised arc consistency (domain consistency), 
  and gfd_vc for value consistency.
</P><P>
  This constraint is implemented by Gecode's path() constraint (variant with 
  cost), using an actual offset of Offset + 1.
</P>
")
]).

:- comment(ham_path_offset_g/6, [
        amode: ham_path_offset_g(?,?,+,+,++,?),
        args: ["Start": "An integer or (domain) variable (array notation accepted)",
               "End": "An integer or (domain) variable (array notation accepted)",
               "Succ":"A collection of N different (domain) variables or integers",
               "Offset":"Offset for Succ (An integer)",
               "CostMatrix":"A NxN matrix collection of integers.",
               "Cost": "A (domain) variable or integer (array notation accepted)."
              ],
        template:"<ConsistencyModule:> ham_path_offset_g(?Start,?End,+Succ,+Offset,++CostMatrix,?Cost)",
        summary: "Constrains elements in Succ (offset by Offset) to
 form a Hamiltonian path from Start to End with cost Cost. This version uses native Gecode indexing.", 
	see_also: [ham_path_offset/6],
        kind: [constraint],
        desc: html("<P>\
  This version of ham_path_offset/4 uses the native Gecode indexing, which
  starts from 0. This is different from normal ECLiPSe's indexing, which
  starts from 1. Offset is not adjusted in this version. This version of 
  the constraint is provided for completeness, in case the user is using
  native Gecode indexing in their code, so that Offset does not need to
  be adjusted manually by the user. 
</P><P>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</p><p>
  See ham_path_offset/6 for a more detailed description of this predicate.")
]).   

%----------------------------------------------------------------------

:- comment(ham_path_offset/7, [
        amode: ham_path_offset(?,?,+,+,++,+,?),
        args: ["Start": "An integer or (domain) variable (array notation accepted)",
               "End": "An integer or (domain) variable (array notation accepted)",
               "Succ":"A collection of N different (domain) variables or integers",
               "Offset":"Offset for Succ (An integer)",
               "CostMatrix":"A NxN matrix collection of integers",
               "ArcCosts": "A collection of N (domain) variables or integers.",
               "Cost": "A (domain) variable or integer (array notation accepted)."
              ],
        template:"<ConsistencyModule:> ham_path_offset(?Start,?End,+Succ,+Offset,++CostMatrix,+ArcCosts,?Cost)",
	see_also:[ham_path_offset/4,ham_path_offset/6,ham_path_offset_g/7],
        summary: "Constrains elements in Succ (offset by Offset) to
 form a Hamiltonian path from Start to End with cost Cost.", 
        kind: [constraint],
        desc: html("<P>\
  Succ is a collection of N elements presenting a digraph of N nodes, where the
  i'th element of (Succ - Offset) represents the successor to node i. The
  constraint enforces Succ to form a Hamiltonian path, a path through every
  node in the graph, visiting each node once, with Start giving the first node
  of the path, and End giving the last node of the path. Note that the
  Succ of the last node will be N+1, i.e. a dummy node not in the graph. 
  Additionally, CostMatrix specifies the cost for traversing between
  each pair of nodes: CostMatrix[i,j] represents the cost of
  travelling from node i to j, and Cost is constrained to the total cost 
  for the path. The i'th element of ArcCosts is constrained to the cost of 
  the arc in the path from node i.
</P><P>
  Note that the Gecode implementation of this constraint has index (node id)
  starting from 0, rather than 1. The value of Offset is incremented by 1 
  when the constraint is posted to Gecode.  A version of this constraint with
  native Gecode indexing, i.e. without adjusting Offset, is available 
  as ham_path_offset_g/5.
</P><P>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</P><P>
  ConsistencyModule is the optional module specification to give the 
  consistency level for the propagation for this constraint: 
  gfd_gac for generalised arc consistency (domain consistency), 
  and gfd_vc for value consistency.
</P><P>
  This constraint is implemented by Gecode's path() constraint (variant with 
  cost and arc costs), using an offset of Offset + 1.
</P>
")
]).

:- comment(ham_path_offset_g/7, [
        amode: ham_path_offset_g(?,?,+,+,++,+,?),
        args: ["Start": "An integer or (domain) variable (array notation accepted)",
               "End": "An integer or (domain) variable (array notation accepted)",
               "Succ":"A collection of N different (domain) variables or integers",
               "Offset":"Offset for Succ (An integer)",
               "CostMatrix":"A NxN matrix collection of integers",
               "ArcCosts": "A collection of N (domain) variables or integers (array notation accepted).",
               "Cost": "An domain variable or integer."
              ],
        template:"<ConsistencyModule:> ham_path_offset_g(?Start,?End,+Succ,+Offset,++CostMatrix,+ArcCosts,?Cost)",
        summary: "Constrains elements in Succ (offset by Offset) to form a Hamiltonian path from Start to End with cost Cost, using native Gecode indexing.", 
	see_also:[ham_path_offset/7],
        kind: [constraint],
        desc: html("<P>\
  This version of ham_path_offset/7 uses the native Gecode indexing, which starts 
  from 0. This is different from normal ECLiPSe's indexing, which starts from 1.
</P><P>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</p><p>
  See ham_path_offset/7 for a more detailed description of this predicate.")
]).   

%----------------------------------------------------------------------

:- comment(disjunctive/2, [
  amode:   disjunctive(+,+),
  args:    ["StartTimes":  "Collection of N start times for tasks (domain variables or integers)",
            "Durations":   "Collection of N durations for tasks (non-negative domain variables or integers)"
           ],
  summary: "Constrain the tasks with specified start times and durations to not overlap in time.",
  see_also: [_:disjunctive/2, disjunctive_optional/3, cumulative/4, cumulatives/5,collection_to_list/2],
  kind: [constraint:[extra:[gccat:disjunctive]]],
  eg: "
[eclipse 2]: disjunctive([1,7,4],[3,2,1]).    % succeed

[eclipse 3]: disjunctive([1,7,3], [3,2,1]).   % fail

[eclipse 4]: disjunctive([1,4,7,4],[3,0,2,1]). % succeed 

[eclipse 5]: disjunctive([1,2,7,4],[3,0,2,1]). % fail 


[eclipse 5]: [S2,S4]::[1..9], disjunctive([1,S2,7,S4], [3,1,2,1]).

S2 = S2{[4 .. 9]}
S4 = S4{[4 .. 9]}

",

  desc:    html("\
<P>
    A disjunctive scheduling constraint. StartTimes and Durations are
    collections (a la collection_to_list/2) of equal size N of integer
    variables or integers. Durations must be non-negative.
 </P><P>
    The declarative meaning is that the N tasks with the given start 
    times and durations do not overlap at any point in time, i.e. for
    any pairs of tasks X and Y, the following holds:
<PRE>
        (Xstart + Xduration =< Ystart) or (Ystart + Yduration =< Xstart)
</PRE></P><P>
    Note that the constraint is implemented by different Gecode propagators,
    depending on if Durations contains domain variables or not. If
    Durations does have domain variables, the Gecode propagator requires
    an extra End domain variable specifying the end time, and a constraint 
<PRE>        
      End #= Start + Duration  
</PRE>
    for each task. These are posted as part of the constraint (the End 
    variables are not accessible by the user).
</P><P>
    Any input variables which are not already domain variables will be
    converted into domain variables with default bounds.
</P><P>
    This constraint is also known as disjunctive in the global constraint
    catalog, but in the catalog, tasks with zero duration are allowed
    to overlap with other tasks. The constraint is implemented using 
    Gecode's unary constraint (with extra constraints on task end times if 
    any task duration is a domain variable).
</P>
")
]).

:- comment(disjunctive_optional/3, [
  amode:   disjunctive_optional(+,+,+),
  args:    ["StartTimes":  "Collection of N start times for tasks (domain variables or integers)",
            "Durations":   "Collection of N durations for tasks (non-negative domain variables or integers)",
            "Scheduled":   "Collection of N scheduled booleans for task (0/1"
" domain variables or integers)"
           ],
  summary: "Constrain the optional tasks with specified start times and durations to"
           " not overlap in time.",
  see_also: [disjunctive/2, collection_to_list/2],
  kind: [constraint],
  desc:    html("\
<P>  
    A disjunctive scheduling constraint. StartTimes, Durations and Scheduled
    are collections (a la collection_to_list/2) of equal size N. Durations
    must be non-negative, and Scheduled are booleans (0/1).
    The declarative meaning is that the scheduled tasks with the given start 
    times and durations do not overlap at any point in time. A task would not
    be scheduled if its Scheduled boolean is 0, and must be scheduled if 1.
</P><P>
    Note that the constraint is implemented by different Gecode propagators,
    depending on if Durations contains domain variables or not. If
    Durations does have domain variables, the Gecode propagator requires
    an extra End domain variable specifying the end time, and a constraint 
<PRE>        
      End #= Start + Duration  
</PRE>
    for each task. These are posted as part of the constraint (the End 
    variables are not accessible by the user).
</P><P>
    Any input variables which are not already domain variables will be
    converted into domain variables with default bounds.
</P><P>
    This constraint is implemented using Gecode's unary() constraint (with 
    extra constraints on task end times if any task duration is a domain 
    variable).
</P>
")
]).


%----------------------------------------------------------------------

:- comment(disjoint2/1, [
  amode:   disjoint2(+),
  args:    ["Rectangles":  "Collection of rect{} structures specifying
 the position and size of rectangles on a grid."
           ],
  summary: "Constrains the position (and possibly size) of the rectangles in Rectangles so that none overlaps.",
  see_also: [disjoint2_optional/1,collection_to_list/2],
  kind: [constraint:[extra:[gccat:diffn]]],
  eg: "\
[eclipse 17]: disjoint2([rect{x:2,y:1,w:2,h:3},rect{x:4,y:3,w:4,h:3},
                   rect{x:9,w:2,y:4,h:3}]).    % succeed

",
  desc:    html("\
<P>
    A two dimensional disjunctive constraint that constrains the replacement
    of a collection of rectangles specified by Rectangles to not overlap in
    their areas. 
</P><P>
    Each rectangle is defined by a rect named structure, using the
    following fields:
<DL>
      <DT>x:<DD> The x co-ordinate of the left side of the rectangle 
      <DT>y:<DD> The y co-ordinate of the bottom side of the rectangle.
      <DT>w:<DD> The width of the rectangle
      <DT>h:<DD> The height of the rectangle
</DL>
    x, y, w, h can be domain variables or integers. If w and h are
    integers, then the rectangle is of a fixed size. Note the rect{}
    structure has an additional 'o' field, which is ignored for this
    constraint as it is only used for disjoint2_optional/1.
</P><P>
    Note that the constraint is implemented by different Gecode propagators,
    depending on if all the rectangles are of fixed size or not. If at
    least one rectangle is not of fixed size, then the Gecode
    propagator requires additional variables for the right and top
    sides of the rectangles, plus additional constraints
<PRE>        
      Xright #= Xleft + Width
      Ytop   #= Ybot + Height
</PRE>
    for each rectangles. These are posted as part of the constraint (the Xright 
    and Ytop variables are not accessible by the user).
</P><P>
    Any input variables which are not already domain variables will be
    converted into domain variables with default bounds.
</P><P>
    A version of this constraint, generalised from two to multi-
    dimension, is known as diffn in the Global Constraint Catalog.
</P><P>
    This constraint is implemented using Gecode's nooverlap() constraint.
</P>
")
]).

:- comment(disjoint2_optional/1, [
  amode:   disjoint2_optional(+),
  args:    ["Rectangles":  "Collection of rect{} structures specifying
 the position and size of rectangles on a grid."
           ],
  summary: "Constrains the position (and possibly size) of the (possibly optional) rectangles in Rectangles so that none overlaps.",
  see_also: [disjoint2/1, collection_to_list/2],
  kind: [constraint],
  desc:    html("\
<P>
    A two dimensional disjunctive constraint that constrains the replacement
    of a collection of rectangles specified by Rectangles to not overlap in
    their areas. The placement of each rectangle can be optional, i.e.
    they may not need to be placed.
</P><P>
    Each rectangle is defined by a rect named structure, using the
    following fields:
<DL>
      <DT>x:<DD> The x co-ordinate of the left side of the rectangle 
      <DT>y:<DD> The y co-ordinate of the bottom side of the rectangle.
      <DT>w:<DD> The width of the rectangle
      <DT>h:<DD> The height of the rectangle
      <DT>b:<DD> Boolean specifying if rectangle is placed or not
</DL>
    x, y, w, h can be domain variables or integers. If w and h are
    integers, then the rectangle is of a fixed size. o is a 0/1
    integer or domain variable, 1 specifies that the rectangle needs
    to be placed, and 0 that it is not placed. 
</P><P>
    Note that the constraint is implemented by different Gecode propagators,
    depending on if all the rectangles are of fixed size or not. If at
    least one rectangle is not of fixed size, then the Gecode
    propagator requires additional variables for the right and top
    sides of the rectangles, plus additional constraints
<PRE>        
      Xright #= Xleft + Width
      Ytop   #= Ybot + Height
</PRE>
    for each rectangles. These are posted as part of the constraint (the Xright 
    and Ytop variables are not accessible by the user).
</P><P>
    Any input variables which are not already domain variables will be
    converted into domain variables with default bounds.
</P><P>
    A version of this constraint, generalised from two to multi-
    dimension, and without optional placement, is known as diffn in
    the Global Constraint Catalog. It is implemented using Gecode's 
    nooverlap() constraint (variant with optional placement).
</P>")
]).

%----------------------------------------------------------------------

:- comment(cumulative/4, [
  amode: cumulative(+,+,+,+),
  args:  ["StartTimes":  "Collection of start times for tasks (integer variables or integers)",
          "Durations":   "Collection of duration for tasks (non-negative integer variables or integers)",
          "Usages":   "Collection of resource usages (positive integers)",
          "ResourceLimit": "Maximum amount of resource available (domain variable, array notation accepted)"
         ],
  summary: "Single resource cumulative constraint on scheduling tasks.",
  see_also: [disjunctive/2, cumulative_optional/5, cumulatives/5, collection_to_list/2, _:cumulative/4],
  kind: [constraint:[extra:[gccat:cumulative]]],
  eg: "

% success (peak consumption is 7) 
[eclipse 7]: cumulative([1,2,3,6,7], [3,9,10,6,2], [1,2,1,1,3], 8).

% fail (peak consumption is 7)
[eclipse 8]:  cumulative([1,2,3,6,7], [3,9,10,6,2], [1,2,1,1,3], 6).

",
  desc:    html("\
<P>
   A cumulative scheduling constraint. StartTimes, Durations and Usages 
   are collections (a la collection_to_list/2) of equal size N representing
   N tasks. Durations are non-negative, Usages and ResourceLimit are 
   strictly positive. The declarative meaning is:
</P><P>
   The N tasks, each starting at a certain start time, having
   a certain duration and consuming a certain (constant) amount of
   resource, then the sum of resource usage of all the tasks does not
   exceed ResourceLimit at any time. 
</P><P>
   A zero duration task both consume and release the resource it uses
   at the same time-point, so effectively it does not consume any resources 
   at that time-point, but there must be sufficient resources available at 
   that time-point to allow the task to be scheduled.
</P><P>
   Note that the constraint is implemented by different Gecode propagators,
   depending on if Durations contains domain variables or not. If
   Durations does have domain variables, the Gecode propagator requires
   an extra End domain variable specifying the end time, and a constraint 
<PRE>        
     End #= Start + Duration  
</PRE>
   for each task. These are posted as part of the constraint (the End 
   variables are not accessible by the user).
</P><P>
   Any input variables which are not already domain variables are turned
   into domain variables with default domain.
</P><P>
    This constraint is also known as cumulative in the global constraint
    catalog, but in the catalog, tasks with zero duration have a different
    definition of overlap with other tasks. The constraint is implemented 
    using Gecode's cumulative constraint (with extra constraints on task 
    end-times if any task duration is a domain variable).
</P>")
]).

:- comment(cumulative_optional/5, [
  amode: cumulative_optional(+,+,+,+,+),
  args:  ["StartTimes":  "Collection of start times for tasks (integer variables or integers)",
          "Durations":   "Collection of duration for tasks (non-negative integer variables or integers)",
          "Usages":   "Collection of resource usages (positive integers)",
          "ResourceLimit": "Maximum amount of resource available
                            (domain variable or integer, array notation accepted)",
          "Scheduled":   "Collection of N scheduled booleans for task (0/1"
" domain variables or integers)"
         ],
  summary: "Single resource cumulative constraint on scheduling optional tasks.",
  see_also: [disjunctive/2, disjunctive_optional/3, cumulative/4, collection_to_list/2, _:cumulative/4],
  kind: [constraint],
  desc:    html("\
<P>
   A cumulative scheduling constraint. StartTimes, Durations, Usages and 
   Scheduled are collections (a la collection_to_list/2) of equal size N,
   representing N task. Durations must be non-negative, Usages and 
   ResourceLimit must be strictly positive, and Scheduled are booleans 
   (values of 0/1). The declarative meaning is:
</P><P>
   The N tasks, each starting at a certain start time, having
   a certain duration and consuming a certain (constant) amount of
   resource, then the sum of resource usage of all the tasks does not
   exceed ResourceLimit at any time. A task would not be scheduled 
   if its Scheduled boolean is 0, and must be scheduled if 1.
/P><P>
   A zero duration task both consume and release the resource it uses
   at the same time-point, so effectively it does not consume any resources 
   at that time-point, but there must be sufficient resources available at 
   that time-point to allow the task to be scheduled.
<</P><P>
   Note that the constraint is implemented by different Gecode propagators,
   depending on if Durations contains domain variables or not. If
   Durations does have domain variables, the Gecode propagator requires
   an extra End domain variable specifying the end time, and a constraint 
<PRE>        
     End #= Start + Duration  
</PRE>
   for each task. These are posted as part of the constraint (the End 
   variables are not accessible by the user).
</P><P>
   Any input variables which are not already domain variables are turned
   into domain variables with default domain.
</P>
")
]).


%----------------------------------------------------------------------

:- comment(cumulatives/5, [
  amode: cumulatives(+,+,+,+,++),
  template:"<ConsistencyModule:> cumulatives(+StartTimes, +Durations, +Heights, +Assigned, +MachineCapacities)",
  args:  ["StartTimes":  "Collection of N start times for tasks (domain variables or integers)",
          "Durations":   "Collection of N duration for tasks (domain variables or integers)",
          "Heights":   "Collection of N resource usages (positive) or productions"
" (negative) by tasks (domain variables or integers) with the assigned machine",
          "Assigned": "Collection of N ID of machine assigned to tasks"
" (domain variables or integers)",
          "MachineCapacities": "Collection of M maximum amount of resource"
" available for machines (integers)"
         ],
  summary: "Multi-resource cumulatives constraint on specified tasks.",
  see_also: [disjunctive/2, cumulative/4, collection_to_list/2, cumulatives_g/5],
    kind: [constraint:[extra:[gccat:cumulatives]]],
  eg:"
[eclipse 4]: cumulatives([2,1,4,2,5,3,1],[2,4,2,3,2,2,4],[-2,1,-1,2,2,-1,1], 
                [1,1,1,1,1,2,2], [2,1]).  % Succeed

[eclipse 5]: cumulatives([2,1,4,2,5,3,1],[2,4,2,3,2,2,4],[-2,1,-1,2,2,-1,1], 
                [1,1,1,1,1,2,2], [1,1]).  % Fails
",
  desc:    html("\
<P>
   A multi-resource cumulatives scheduling constraint - scheduling of M
   machines providing resources for N tasks. StartTimes, Durations, Heights 
   and Assigned are collections (a la collection_to_list/2) of equal size N 
   of domain variables or integers. MachineLimits is a collection of M 
   integers. The declarative meaning is:
   If there are N tasks and M machines, each machine having a limit of 
   resource that can be consumed at any single time-point, and each task 
   starting at a certain start time, having a certain duration and 
   consuming/producing a certain (constant) amount of resource for the 
   machine assigned to the task, then the sum of resource usage for each
   machine by all the tasks does not exceed the capacity for that machine at
   any time. 
</P><P>
   Any input variables which are not already domain variables are turned
   into domain variables with default domain.
</P><P>
   This constraint generalise the cumulative constraint to multi-resources,
   and is also more flexible, in that tasks can both produce and consume a
   resource.
</P><P>
    Note that the Gecode implementation of this constraint has index starting
    from 0, i.e. the numbering for the machines starts from 0. These native 
    indices are mapped to the  ECLiPSe indices starting from 1 with an 
    additional dummy zero'th machine that is not used. A version of this 
    constraint that uses native Gecode indexing is available 
    as cumulatives_g/5.
</P><P>
    ConsistencyModule is the optional module specification to give the 
    consistency level for the propagation for this constraint: 
    gfd_vc for value consistency.
</P><P>
    This constraint is also known as cumulatives in the global constraint
    catalog, where CTR is \"less than or equal to\" case  The constraint is 
    implemented using Gecode's cumulatives constraint (with extra constraints 
    on task end-times if any task duration is a domain variable).
</P>")
]).



:- comment(cumulatives_g/5, [
  amode: cumulatives_g(+,+,+,+,++),
  template:"<ConsistencyModule:> cumulatives_g(+StartTimes, +Durations, +Heights, +Assigned, +MachineCapacities)",
  args:  ["StartTimes":  "Collection of N start times for tasks (domain variables or integers)",
          "Durations":   "Collection of N duration for tasks (domain variables or integers)",
          "Heights":   "Collection of N resource usages (positive) or productions"
" (negative) by tasks (domain variables or integers) with the assigned machine",
          "Assigned": "Collection of N ID of machine assigned to tasks"
" (domain variables or integers)",
          "MachineCapacities": "Collection of M maximum amount of resource"
" available for machines (integers)"
         ],
  summary: "Multi-resource cumulatives constraint on specified tasks, using native Gecode indexing.",
  see_also: [cumulatives/5],
  kind: [constraint:[extra:[gccat:cumulatives]]],
  desc:    html("\
  This version of the constraint uses the native Gecode indexing, which starts 
  from 0. This is different from normal ECLiPSe's indexing, which starts from 1.
</p><p>
  This predicate maps more directly to Gecode's native implementation of 
  the constraint, without the conversion between Gecode and ECLiPSe
  indexing of cumulatives/5. It may therefore be more efficient, but could 
  also be incompatible with existing ECLiPSe code. 
</p><p>
  See cumulatives/5 for a more detailed description of this predicate.")
]).   

:- comment(cumulatives_min/5, [
  amode: cumulatives_min(+,+,+,+,++),
  template:"<ConsistencyModule:> cumulatives_min(+StartTimes, +Durations, +Heights, +Assigned, +MachineConsumptions)",
  args:  ["StartTimes":  "Collection of N start times for tasks (domain variables or integers)",
          "Durations":   "Collection of N duration for tasks (domain variables or integers)",
          "Heights":   "Collection of N resource usages (positive) or productions"
" (negative) by tasks (domain variables or integers) with the assigned machine",
          "Assigned": "Collection of N ID of machine assigned to tasks"
" (domain variables or integers)",
          "MachineConsumptions": "Collection of M minimum amount of resource"
" consumptions for machines (integers)"
         ],
  summary: "Multi-resource cumulatives constraint on specified tasks with"
" required minimum resource consumptions.",
  see_also: [disjunctive/2, cumulatives/5, collection_to_list/2,cumulative/4],
  kind: [constraint:[extra:[gccat:cumulatives]]],
  eg:"

[eclipse 7]: cumulatives_min([2,1,4,2,5,3,1],[-2,4,2,3,2,2,4],
                [2,1,-1,2, 2,-1,1], [1,1,1,1,1,2,2], [1,0]).  % Succeed
 
[eclipse 8]: cumulatives_min([2,1,4,2,5,3,1],[2,4,2,3,2,2,4],
                [-2,1,-1,2,2,-1,1], [1,1,1,1,1,2,2], [1,1]). % fails 


",
  desc:    html("\
<P>
   A multi-resource cumulatives scheduling constraint - scheduling of M
   machines providing resources for N tasks. StartTimes, Durations, Heights 
   and Assigned are collections (a la collection_to_list/2) of equal size N 
   of domain variables or integers. MinUsages is a collection of M 
   integers. The declarative meaning is:
   If there are N tasks and M machines, each machine having a minimum of 
   produce that must be consumed at any single time-point, and each task 
   starting at a certain start  time, having a certain duration and 
   consuming/producing a certain (constant) amount of produce for the 
   machine assigned to the task, then the sum of resource consumption for each
   machine by all the tasks must be at least the minimum for that machine at
   any time. 
</P><P>
   Any input variables which are not already domain variables are turned
   into domain variables with default domain.
</P><P>
   This constraint generalise the cumulative constraint to multi-resources,
   and is also more flexible, in that tasks can both produce and consume a
   resource.
</P><P>
    Note that the Gecode implementation of this constraint has index starting
    from 0, i.e. the numbering for the machines starts from 0. These native 
    indices are mapped to the  ECLiPSe indices starting from 1 with an 
    additional dummy zero'th machine that is not used. A version of this 
    constraint that uses native Gecode indexing is available 
    as cumulatives_min_g/5.
</P><P>
    ConsistencyModule is the optional module specification to give the 
    consistency level for the propagation for this constraint: 
    gfd_vc for value consistency.
</P><P>
    This constraint is known as cumulatives in the global constraint
    catalog, where CTR is \"greater than or equal to\" case  The constraint is 
    implemented using Gecode's cumulatives constraint (with extra constraints 
    on task end-times if any task duration is a domain variable).
</P>"
)
]).


:- comment(cumulatives_min_g/5, [
  amode: cumulatives_min_g(+,+,+,+,++),
  template:"<ConsistencyModule:> cumulatives_min_g(+StartTimes, +Durations, +Heights, +Assigned, +MachineConsumptions)",
  args:  ["StartTimes":  "Collection of N start times for tasks (domain variables or integers)",
          "Durations":   "Collection of N duration for tasks (domain variables or integers)",
          "Heights":   "Collection of N resource usages (positive) or productions"
" (negative) by tasks (domain variables or integers) with the assigned machine",
          "Assigned": "Collection of N ID of machine assigned to tasks"
" (domains variables or integers)",
          "MachineConsumptions": "Collection of M minimum amount of resource"
" consumptions for machines (integers)"
         ],
  summary: "Multi-resource cumulatives constraint on specified tasks with"
" required minimum resource consumptions, using native Gecode indexing.",
  see_also: [cumulatives_min/5],
  kind: [constraint:[extra:[gccat:cumulatives]]],
  desc:    html("\
  This version of the constraint uses the native Gecode indexing, which starts 
  from 0. This is different from normal ECLiPSe's indexing, which starts from 1.
</p><p>
  This predicate maps more directly to Gecode's native implementation of 
  the constraint, without the conversion between Gecode and ECLiPSe
  indexing of cumulatives_min/5. It may therefore be more efficient, but 
  could also be incompatible with existing ECLiPSe code. 
</p><p>
  See cumulatives_min/5 for a more detailed description of this predicate.")
]).   

% ----------------------------------------------------------------------

:- comment(sequence/5, [
        template:"<ConsistencyModule:> sequence(+Low,+High,+K,+Vars,++Values)",
        amode: sequence(+,+,+,+,++),
        args: ["Low":"Non-negative integer",
               "High":"Positive integer",
               "K": "Positive integer",
               "Vars": "A collection of (domain) variables or integers",
               "Values": "A collection of (different) integers"
              ],
        summary: "The number of values taken from Values is between Low and"
                 " High for all sequences of K variables in Vars.", 
        see_also: [sequence/4,_:sequence/5],
        kind: [constraint:[extra:[gccat:among_seq]]],
        eg:"
[eclipse 22]: sequence(1,2,4,[9,2,4,5,5,7,2], [0,2,4,6,8]). % Succeed

",
        desc: html("\
<P>
    This constraint ensures that the number of values taken from the set
    specified in Values is at least Low and at most High for all sequences 
    of K consecutive variables/values in Vars. 
</P><P>
    ConsistencyModule is the optional module specification to give the 
    consistency level for the propagation for this constraint: 
    gfd_gac for generalised arc consistency (domain consistency). 
</P><P>
    This constraint is known as among_seq in the global constraint catalog,
    and is implemented using Gecode's sequence() constraint.
</P>
") 
         ]
).

:- comment(sequence/4, [
        amode: sequence(+,+,+,+),
        template:"<ConsistencyModule:> sequence(+Low,+High,+K,+ZeroOnes)",
        args: ["Low":"Non-negative integer",
               "High":"Positive integer",
               "K": "Positive integer",
               "ZeroOnes": "A collection of 0/1 domain) variables or integers"
              ],
        summary: "The number of occurrences of the value 1 is between Low and"
                 " High for all sequences of K variables in ZeroOnes", 
        see_also: [sequence/5, _:sequence/4],
        kind: [constraint:[extra:[gccat:among_seq]]],
        eg:"
[eclipse 20]: sequence(2,3,3,[1,0,1,1,0,1]).    % Succeed

[eclipse 21]: sequence(2,3,3,[1,0,1,1,0,0,1]).  % Fail

",
        desc: html("\
<P>
    This constraint ensures that the number of occurrences of the value 1
    is at least Low and at most High for all sequences of K consecutive 
    variables/values in ZeroOnes. ZeroOnes are 0/1 variables (or integers), 
    i.e. they have the domain [0,1]. 
</P><P>
    The ZeroOnes can be interpreted as the fulfillment of various
    conditions if the variables are linked to these conditions. 
</P><P>
    ConsistencyModule is the optional module specification to give the 
    consistency level for the propagation for this constraint: 
    gfd_gac for generalised arc consistency (domain consistency), 
</P><P>
    The more general version of this constraint, sequence/5, where the 
    values being checked is not limited to 1 is known as among_seq in the 
    global constraint catalog, and this constraint is also implemented
    via the more general Gecode sequence() constraint by limiting the value
    set to 1. This version is provided here for compatibility with IC and 
    FD, where the more general sequence/5 constraint is implemented on top
    of this more restrictive sequence/4.
</P>
") 
         ]
).


%----------------------------------------------------------------------

:- comment(bin_packing/3, [
       amode: bin_packing(+,++,+),
       args: ["Items": "A collection of M (domain) variables or integers (domain/value"
                       " between 1 and N)",
              "ItemSizes": "A collection of M non-negative integers",
              "BinLoads": "A collection of N (domain) variables or non-negative integers"
             ],
       see_also:[bin_packing/4,bin_packing_g/3,_:bin_packing/3],
       summary:"The one-dimensional bin packing constraint with loads: packing "
               "M items into N bins, each bin having a load",
       kind: [constraint:[extra:[gccat:bin_packing_capa]]],
       eg:"
[eclipse 7]: bin_packing([3,1,3], [4,3,1], [L1,L2,L3,L4]).

L1 = 3
L2 = 0
L3 = 5
L4 = 0

",
       desc: html("\
   This constraint is for one-dimensional bin-packing, that is, to pack M
   items with individual sizes into N bins, such that the sum of sizes of
   items in each bin equals the load of that bin, as specified in BinLoads.
   Each element of Items and its corresponding element in ItemSizes
   represents an item, such that the i'th element of ItemSizes is the size
   of the i'th item, and the i'th element of Item is the bin this item is
   packed into. BinLoads represent the load of each bin, i.e. the sum
   of the sizes of items assigned to that bin, with the j'th element 
   representing the load for bin j. An (integer finite domain) variable for 
   the load allows a constraint on the load to be specified, such as a
   minimum and/or maximum load for the bin.
</P><P>
    Note that the Gecode implementation of this constraint has index starting
    from 0, i.e. the numbering for the bins starts from 0. These native 
    indices are mapped to the  ECLiPSe indices starting from 1 with an 
    additional dummy zero'th bin that is assigned a dummy item 0. A version
    of this constraint that uses native Gecode indexing is available 
    as bin_packing_g/3.
</P><P>
   The global constraint catalog describes this constraint as a
   variation of their bin_packing constraint where the fixed capacity for
   every bin is replaced by the BinLoads variables. In addition, a more
   restricted version of this constraint is also described in the global 
   constraint catalog as bin_packing_capa, where instead of BinLoads,
   each bin is given its own integer capacity, the maximum load that
   bin can take, this is equivalent to giving the corresponding variable 
   for the bin in BinLoads the domain 0..Capacity. 
</p><p>
   This constraint is implemented using Gecode's binpacking() constraint.
</p>
")
          ]).

:- comment(bin_packing_g/3, [
       amode: bin_packing_g(+,++,+),
       args: ["Items": "A collection of M (domain) variables or integers (domain/value"
                       " between 0 and N-1)",
              "ItemSizes": "A collection of M non-negative integers",
              "BinLoads": "A collection of N v(domain) ariables or non-negative integers"
             ],
       see_also:[bin_packing/4,bin_packing/3],
       summary:"The one-dimensional bin packing constraint with loads, using native Gecode indexing",
       kind: [constraint:[extra:[gccat:bin_packing]]],
  desc:    html("\
  This version of the constraint uses the native Gecode indexing, which starts 
  from 0. This is different from normal ECLiPSe's indexing, which starts from 1.
</p><p>
  This predicate maps more directly to Gecode's native implementation of 
  the constraint, without the conversion between Gecode and ECLiPSe
  indexing of cumulatives_min/5. It may therefore be more efficient, but 
  could also be incompatible with existing ECLiPSe code. 
</p><p>
  See bin_packing/3 for a more detailed description of this predicate.")
]).   

:- comment(bin_packing/4, [
       amode: bin_packing(+,++,+,+),
       args: ["Items": "A collection of M (domain) variables or integers (domain/value"
                       " between 1 and N)",
              "ItemSizes": "A collection of M non-negative integers",
              "N": "A positive Integer",
              "BinSize": "A non-negative integer"
             ],
       see_also:[bin_packing/3, cumulative/4,_:bin_packing/4],
       summary:"The one-dimensional bin packing constraint: packing M items"
               " into N bins of size BinSize.",
       eg: "
[eclipse 4]: bin_packing([3,1,3], [4,3,1], 3, 5).  % Succeed

[eclipse 5]: bin_packing([3,3,3], [4,3,1], 3, 5).  % Fails

",
       kind: [constraint:[extra:[gccat:bin_packing]]],
       desc: html("\
   This constraint is for one-dimensional bin-packing, that is, to pack M
   items with individual sizes into N bins, such that the sum of sizes of 
   items in each bin does not exceed BinSize. Each element of Items and its 
   corresponding element in ItemSizes represents an item, such that the i'th 
   element of ItemSizes is the size of the i'th item, and the i'th element in
   Items is the bin this item is packed into. 
</P><P>
   This constraint can be seen as a special case of the cumulative/4
   constraint, where all task durations are equal to 1, each bin
   represents a time point, and BinSize corresponds to the Resource.
</p><p>
   This constraint is implemented using the more general 
   bin_packing/3, where each bin has its own size, represented by a domain
   variable, as this is what is implemented by Gecode. This form of
   the constraint with a fixed BinSize is more common. so it is
   provided for convenience and compatibility. Note that this constraint
   uses ECLiPSe indexing -- bins are indexed starting from 1. There is no     
   Gecode indexing version of this constraint as it is not implemented
   directly in Gecode.
</P><P>
   This constraint is described in the global constraint catalog as 
   bin_packing, but with slightly different arguments: in the catalog, N
   (the number of bins) is implicitly defined by the domains of the variables 
   in Items, and the representation of item is grouped into a single argument
   of collection of pairs, each pair representing an item: the bin to pack 
   the item, and its size. It is implemented using Gecode's binpacking() 
   constraint, with the loads of all bins set to the domain 0..BinSize,i.e..
   that they all have capacity of BinSize.
</p>
")
          ]).

:- comment(bin_packing_md/3, [
       amode: bin_packing_md(+,++,+),
       args: ["Items": "A collection of M (domain) variables or integers (domain/value"
                       " between 1 and N)",
              "ItemMDSizes": "A 2-D collection of M*L non-negative integers",
              "BinMDLoads": "A 2-D collection of N*L (domai) variables or non-negative integers"
             ],
       see_also:[bin_packing_md/4,bin_packing/4,bin_packing/3],
       summary:"The multi-dimensional bin packing constraint with loads: packing "
               "M L-Dimensional items into N L-Dimensional bins, each"
               " bin having a load in each dimension",
       kind: [constraint],
       eg:"
[eclipse 2]: bin_packing_md([3,1,3], [[4,2], [3,0], [1,3]], [[L11,L12],[L21,L22],[L31,L32]]).

L11 = 3
L12 = 0
L21 = 0
L22 = 0
L31 = 5
L32 = 5

",
       desc: html("\
   This constraint is for multi-dimensional bin-packing, that is, to
   pack M L-dimensional items with individual sizes in each dimension
   into N L-dimensional bins, such that the sum of sizes of items in
   each dimension of each bin equals the load for that dimension of
   that bin, as specified in BinMDLoads. The constraint is logically
   equivalent to posting L 1-dimensional bin_packing constraints, one
   for each dimension, with the additional constraint that the items
   are placed in the same bin for all the constraints.
</p><p>
   Items and Bins are L-dimensional, i.e. each Item/Bin has a
   size/load in each dimension. Thus,ItemMDSizes and BinMDLoads are
   2-D collections, i.e. a 2-D matrix or a list of lists, such that
   each element is itself a collection of L sub-elements.
</p><p>
   Each element of Items and its corresponding element in ItemMDSizes
   represents an item, such that the i'th element of ItemMDSizes is
   the size of the i'th item, and the i'th element of Item is the bin
   this item is packed into. BinMDLoads represent the loads of each
   bin, i.e. the sum of the sizes of items (in each dimension)
   assigned to that bin, with the j'th element representing the load
   for bin j. An (integer finite domain) variable for the load in each
   dimension allows a constraint on the load to be specified, such as
   a minimum and/or maximum load for the bin in that dimension.
</P><P>
    Note that the Gecode implementation of this constraint has index
    starting from 0, i.e. the numbering for the bins starts from
    0. These native indices are mapped to the ECLiPSe indices starting
    from 1 with an additional dummy zero'th bin that is assigned a
    dummy item 0.
</p><p>
   This constraint is implemented using Gecode's multi-dimensional
   variant of binpacking() constraint, which requires both the maximum
   bin size (as in bin_packing_md/4) and all the bin loads to be specified. 
   This gfd version of the constraint deduces the maximum bin size from 
   BinMDLoads.
</p>
")
          ]).

:- comment(bin_packing_md/4, [
       amode: bin_packing_md(+,++,+,+),
       args: ["Items": "A collection of M (domain) variables or integers (domain/value"
                       " between 1 and N)",
              "ItemMDSizes": "A 2-D collection of M*L non-negative integers",
              "N": "A positive Integer",
              "BinMDSize": "A collection of L non-negative integer"
             ],
       see_also:[bin_packing_md/3, bin_packing/3, bin_packing/4],
       summary:"The multi-dimensional bin packing constraint: packing "
               "M L-dimensional items into N L-dimensional bins of size BinMDSize.",
       eg: "
[eclipse 4]: bin_packing_md([3,1,3], [[4,1],[3,2],[1,1]], 3, [5,4]).  % Succeed

[eclipse 5]: bin_packing_md([3,3,3], [[4,1],[3,2],[1,1]], 3, [5,4]).  % Fails

",
       kind: [constraint],
       desc: html("\
   This constraint is for multi-dimensional bin-packing, that is, to
   pack M L-dimensional items with individual sizes into N
   L-dimensional bins, such that the sum of sizes of items in each bin
   does not exceed BinMDSize in any dimension.  The constraint is
   logically equivalent to posting L 1-dimensional bin_packing
   constraints, one for each dimension, with the additional constraint
   that the items are placed in the same bin for all the constraints.

   Items and Bins are L-dimensional, i.e. each Item/Bin has a
   size/load in each dimension, and in this version of the constraint,
   all the Bins have the same size, as specified by BinMDSize.
   Thus,ItemMDSizes is a 2-D collection, i.e. a 2-D matrix or a list
   of lists, such that each element is itself a collection of L
   sub-elements.
</P><P>
   Each element of Items and its corresponding element in ItemMDSizes
   represents an item, such that the i'th element of ItemSizes is the
   size of the i'th item, and the i'th element in Items is the bin
   this item is packed into.
</P><P>
    Note that the Gecode implementation of this constraint has index
    starting from 0, i.e. the numbering for the bins starts from
    0. These native indices are mapped to the ECLiPSe indices starting
    from 1 with an additional dummy zero'th bin that is assigned a
    dummy item 0.
</p><p>
   This constraint is implemented using Gecode's multi-dimensional
   variant of binpacking() constraint, which requires both the maximum
   bin size and all the bin loads (as in bin_packing_md/3) to be specified. 
   This gfd version of the constraint constructs the bin loads from BinMDSize.
</p>
")
          ]).


% ----------------------------------------------------------------------

:- comment(lex_le/2, [
    summary:"Collection1 is lexicographically less or equal to Collection2",
    amode:lex_le(+,+),
    args:[
	"Collection1":"Collection of integers or (domain) variables",
	"Collection2":"Collection of integers or (domain) variables"
    ],
    kind: [constraint:[extra:[gccat:lex_leq]]],
    eg:"
[eclipse 31]: lex_le([5,2,3,1], [5,2,6,2]).
   ...
Yes (0.00s cpu)
[eclipse 32]: lex_le([5,2,3,9], [5,2,3,9]).
   ...
Yes (0.00s cpu)
[eclipse 33]: lex_le([5,2,4], [5,2,4,1]).
  ...
Yes (0.00s cpu)
[eclipse 34]: lex_le([5,2,4,1], [5,2,4,0]).

No (0.00s cpu)

",
    see_also: [lex_lt/2, lex_gt/2, lex_ge/2, lex_eq/2, lex_ne/2, _:lex_le/2],
    desc:html("\
    	Imposes a lexicographic ordering between the two lists. 
	I.e.  either is the first element of Collection1 strictly smaller
	than the first element of Collection2, or the first elements are
	equal and the lexicographic order holds between the two list
	tails. A non-existing element (i.e. when the end of list is 
        reached) is strictly smaller than any existing element.
</P><P>
        This constraint is known as lex_lesseq in the global constraint
        catalog, but the catalog's definition requires Collection1 and
        Collection2 to be the same size. It is implemented using Gecode's
        rel() constraint (variant that takes two IntVarArgs arguments),
        with the IRT_LQ IntRelType.
</P>
")
]).


:- comment(lex_lt/2, [
    summary:"Collection1 is lexicographically less than  Collection2",
    amode:lex_lt(+,+),
    args:[
	"Collection1":"Collection of integers or (domain) variables",
	"Collection2":"Collection of integers or (domain) variables"
    ],
    kind: [constraint:[extra:[gccat:lex_less]]],
    see_also: [lex_le/2, lex_gt/2, lex_ge/2, lex_eq/2, lex_ne/2, _:lex_lt/2],
    eg:"
[eclipse 36]: lex_lt([5,2,3,9], [5,2,6,2]).

 ...
Yes (0.00s cpu)
[eclipse 37]: lex_lt([5,2,3,9], [5,2,6]).

 ...
Yes (0.00s cpu)
[eclipse 38]: lex_lt([5,2,3], [5,2,3,9]).

 ...
Yes (0.00s cpu)
[eclipse 39]: lex_lt([5,2,3,4], [5,2,3,4]).

No (0.00s cpu)

",
    desc:html("\
    	Imposes a lexicographic ordering between the two lists. 
	I.e.  either is the first element of Collection1 strictly smaller
	than the first element of Collection2, or the first elements are
	equal and the lexicographic order holds between the two list
	tails. A non-existing element (i.e. when the end of list is 
        reached)is strictly smaller than any existing element.
</P><P>
        This constraint is known as lex_less in the global constraint
        catalog. but the catalog's definition requires Collection1 and
        Collection2 to be the same size. It is implemented using Gecode's
        rel() constraint (variant that takes two IntVarArgs arguments),
        with the IRT_LE IntRelType.
</P>

")
]).

:- comment(lex_ge/2, [
    summary:"Collection1 is lexicographically greater or equal to Collection2",
    amode:lex_ge(+,+),
    args:[
	"Collection1":"Collection of integers or (domain) variables",
	"Collection2":"Collection of integers or (domain) variables"
    ],
    kind: [constraint:[extra:[gccat:lex_greatereq]]],
    eg:"
[eclipse 40]: lex_ge([5,2,8,9],[5,2,6,2]).

...
Yes (0.00s cpu)
[eclipse 41]: lex_ge([5,2,3,9], [5,2,3,9]).

...
Yes (0.00s cpu)

[eclipse 42]: lex_ge([5,2,3,9], [5,2,3]).

...
Yes (0.00s cpu)
[eclipse 43]: lex_ge([5,2,3,9], [5,3,1]).

No (0.00s cpu)
",
    see_also: [lex_lt/2, lex_gt/2, lex_eq/2, lex_ne/2],
    desc:html("\
    	Imposes a lexicographic ordering between the two lists. 
	I.e.  either is the first element of Collection1 strictly larger
	than the first element of Collection2, or the first elements are
	equal and the lexicographic order holds between the two list
	tails. A non-existing element (i.e. when the end of list is 
        reached) is strictly smaller than any existing element.
</P><P>
        This constraint is known as lex_greatereq in the global constraint
        catalog, but the catalog's definition requires Collection1 and
        Collection2 to be the same size. It is implemented using Gecode's
        rel() constraint (variant that takes two IntVarArgs arguments),
        with the IRT_GQ IntRelType.
</P>
")
]).


:- comment(lex_gt/2, [
    summary:"Collection1 is lexicographically greater than  Collection2",
    amode:lex_gt(+,+),
    args:[
	"Collection1":"Collection of integers or (domain) variables",
	"Collection2":"Collection of integers or (domain) variables"
    ],
    kind: [constraint:[extra:[gccat:lex_greater]]],
    eg:"
[eclipse 44]: lex_gt([5,2,7,1], [5,2,6,2]).

...
Yes (0.00s cpu)
[eclipse 45]: lex_gt([5,2,7,1], [5,2,7]).

...
Yes (0.00s cpu)
[eclipse 46]: lex_gt([5,2,7,1], [5,2,7,3]).

No (0.00s cpu)
",
    see_also: [lex_lt/2, lex_ge/2, lex_eq/2, lex_ne/2],
    desc:html("\
    	Imposes a lexicographic ordering between the two lists. 
	I.e.  either is the first element of Collection1 strictly greater
	than the first element of Collection2, or the first elements are
	equal and the lexicographic order holds between the two list
	tails. A non-existing element (i.e. when the end of list is 
        reached)is strictly smaller than any existing element.
</P><P>
        This constraint is known as lex_greater in the global constraint
        catalog, but the catalog's definition requires Collection1 and
        Collection2 to be the same size. It is implemented using Gecode's
        rel() constraint (variant that takes two IntVarArgs arguments),
        with the IRT_GR IntRelType.
</P>
")
]).

:- comment(lex_eq/2, [
    summary:"Collection1 is lexicographically equal to Collection2",
    amode:lex_eq(+,+),
    args:[
	"Collection1":"Collection of integers or (domain) variables",
	"Collection2":"Collection of integers or (domain) variables"
    ],
    kind: [constraint:[extra:[gccat:lex_equal]]],
    eg:"
[eclipse 48]: lex_eq([1,9,1,5], [1,9,1,5]).

...
Yes (0.00s cpu)
[eclipse 49]: lex_eq([1,9,1,5], [1,9,2,5]).

No (0.00s cpu)
[eclipse 50]: lex_eq([1,9,1,5], [1,9,0,5]).

No (0.00s cpu)
[eclipse 51]: lex_eq([1,9,1], [1,9,1,5]).

No (0.00s cpu)
",
    see_also: [lex_lt/2, lex_gt/2, lex_ge/2, lex_ne/2],
    desc:html("\
    	Constrains the two collections to be lexicographically equal, i.e.
	the two collections are the same length, and each
        element is identical to its corresponding element in the
        other collection.
</P><P>
        This constraint is known as lex_equal in the global constraint
        catalog, but the catalog's definition requires Collection1 and
        Collection2 to be the same size. It is implemented using Gecode's
        rel() constraint (variant that takes two IntVarArgs arguments),
        with the IRT_EQ IntRelType.
</P>

")
]).

:- comment(lex_ne/2, [
    summary:"Collection1 is lexicographically not equal to Collection2",
    amode:lex_ne(+,+),
    args:[
	"Collection1":"Collection of integers or (domain) variables",
	"Collection2":"Collection of integers or (domain) variables"
    ],
    kind: [constraint:[extra:[gccat:lex_different]]],
    eg:"
[eclipse 52]: lex_ne([5,2,7,1], [5,3,7,1]).

...
Yes (0.00s cpu)
[eclipse 53]: lex_ne([5,2,7,1], [5,2,7]).

...
Yes (0.00s cpu)
[eclipse 54]: lex_ne([5,2,7], [5,2,7,1]).

...
Yes (0.00s cpu)
[eclipse 55]: lex_ne([5,2,7,1], [5,2,7,1]).

No (0.00s cpu)

",
    see_also: [lex_lt/2, lex_gt/2, lex_eq/2, lex_ge/2],
    desc:html("\
    	Constrains the two collections to be lexicographically different, i.e.
	the two collections are either different lengths, or at least
        one element in one collection is different from its corresponding
        element in the other collection.
</P><P>
        This constraint is known as lex_different in the global constraint
        catalog, but the catalog's definition requires Collection1 and
        Collection2 to be the same size. It is implemented using Gecode's
        rel() constraint (variant that takes two IntVarArgs arguments),
        with the IRT_NQ IntRelType.
</P>
")
]).


%----------------------------------------------------------------------

:- comment(ordered/2, [
    summary:"Constrains Vars to be ordered according to Relation",
    template:"<ConsistencyModule:> ordered(+Relation,+Vars)",
    amode:ordered(++,+),
    args:[
	"Relation":"One of the atoms #<, #=<, #>, #>=, #=, #\\=",
	"Vars":"Collection of integers or (domain) variables"
    ],
    kind: [constraint:[extra:[gccat:[strictly_increasing,
                                     increasing,
                                     strictly_decreasing,
                                     decreasing,
                                     all_equal,
                                     not_all_equal 
                                    ]
                             ]
                      ]
          ],
    eg: "\
[eclipse 9]: ordered((#<), [1,2,3,4]).

Yes (0.00s cpu)
[eclipse 10]: ordered((#<), [1,2,2,3,4]).

No (0.00s cpu)
[eclipse 11]: ordered((#=<), [1,2,3,4]).

Yes (0.00s cpu)
[eclipse 12]: ordered((#=<),  [1,2,2,3,4]).

Yes (0.00s cpu)
[eclipse 13]: ordered((#>), [4,3,2,1]).

Yes (0.00s cpu)
[eclipse 14]: ordered((#>), [4,3,3,2,1]).

No (0.00s cpu)
[eclipse 15]: ordered((#>=), [4,3,2,1]).

Yes (0.00s cpu)

[eclipse 16]:  ordered((#>=), [4,3,3,2,1]).

Yes (0.00s cpu)
[eclipse 17]: ordered((#=), [2,2,3,3]).

No (0.00s cpu)
[eclipse 18]: ordered((#=), [2,2,2,2]).

Yes (0.00s cpu)
[eclipse 19]: ordered((#\\=), [2,2,3,3]).

Yes (0.00s cpu)
[eclipse 20]: ordered((#\\=), [2,2,2,2]).

No (0.00s cpu)
[eclipse 21]: ordered((#>), [2]).

Yes (0.00s cpu)
[eclipse 22]: ordered((#\\=), [X]).

No (0.00s cpu)
[eclipse 23]: [A,B] :: 3..7, [C,D] :: 4..10, ordered((#=), [A,B,C,D]).

A = A{[4 .. 7]}
B = B{[4 .. 7]}
C = C{[4 .. 7]}
D = D{[4 .. 7]}

Yes (0.00s cpu)
[eclipse 24]:  [A,B] :: 3..7, [C,D] :: 4..10, ordered((#>), [A,B,C,D]).

A = 7
B = 6
C = 5
D = 4

",
    desc: html("\
      Constrains the elements in Vars to be ordered according to Relation,
      which is one of  #&gt;, #&gt;=, #&lt;, #=&lt;, #=, #\\= (or equivalently,
      &gt;, &gt;=, &lt;, =&lt;, =, \\=). Except for #\\=, the relation must
      hold between any two adjacent two elements in Vars, for  #\\=, the #\\=
      must hold for at least one adjacent pair of elements in Vars, i.e.
      not all elements in Vars are equal.
</P><P>
      ConsistencyModule is the optional module specification to give the 
      consistency level for the propagation for this constraint: 
        gfd_gac for generalised arc consistency (domain consistency), 
        gfd_bc for bounds consistency, and
        gfd_vc for value consistency
</P><P>
     This constraint is known as strictly_increasing (#&gt), increasing (#=&gt;), .
     strictly_decreasing (#&lt;), decreasing (#&lt;=), all_equal (#=), 
     not_all_equal (#\\=) in the Global Constraint Catalog, and is implemented
     using Gecode's rel() constraint (variant with an IntVarArgs and an 
     IntRelType).
</P>
    "),
    see_also:[lex_le/2,lex_lt/2,lex_ge/2,lex_gt/2,sorted/2,_:ordered/2,collection_to_list/2]
    ]).


%----------------------------------------------------------------------

:- comment(precede/3, [
    summary:"Constrains S to precede T in Collection",
    amode:precede(+,+,+),
    args:[
	"S": "Integer",
	"T": "Integer",
	"Collection":"Collection of integers or (domain) variables"
    ],
    kind: [constraint:[extra:[gccat:int_value_precede]]],
    eg: "\
[eclipse 14]: precede(0,1, [4,0,6,1,0]).  % succeed (0 appears before 1)

[eclipse 15]: precede(0,1, [](4,0,6,1,0)). % succeed (0 appears before 1)

[eclipse 16]: precede(0,1, [A,B,C,D,E]).

A = A{[-1000000 .. 0, 2 .. 1000000]}
B = B{[-1000000 .. 1000000]}
C = C{[-1000000 .. 1000000]}
D = D{[-1000000 .. 1000000]}
E = E{[-1000000 .. 1000000]}

[eclipse 17]:  precede(0,1, [4,1,6,0,0]).   % fail (1 appears before 0)

",

    desc: html("\
      Constrains the first appearance of value S to precede the first
      appearance of value T in the ordered collection of elements in 
      Collection. S and T do not have to appear in Collection: if only
      S appears, the constraint will succeed, and if only T appears,
      the constraint will fail. If neither appears, the constraint will
      succeed.
</P><P>
      This constraint is known as int_value_precede in the Global
      Constraint Catalog, and is implemented using Gecode's precede()
      constraint (variant with int arguments for s and t).
</P>
    "),
    see_also:[precede/2,collection_to_list/2]
    ]).

%----------------------------------------------------------------------

:- comment(precede/2, [
    summary:"Constrains each value in Values to precede its succeeding
 value in Collection",
    amode:precede(++,+),
    args:[
	"Values": "Collection of integers",
	"Collection":"Collection of integers or (domain) variables"
    ],
    kind: [constraint:[extra:[gccat:int_value_precede_chain]]],
    eg: "\
[eclipse 18]: precede([4,0,1], [4,0,6,1,0]).   % succeed
[eclipse 19]: precede([4,0,1], [4,0,6,1,0]).  % succeed
[eclipse 20]: precede([4,0,1], [4,1,6,1,0]).   % fail

[eclipse 21]: precede([4,0,1], [A,B,C,D,E]).

A = A{[-1000000 .. -1, 2 .. 1000000]}
B = B{[-1000000 .. 0, 2 .. 1000000]}
C = C{[-1000000 .. 1000000]}
D = D{[-1000000 .. 1000000]}
E = E{[-1000000 .. 1000000]}

",


    desc: html("\
      Constrains the first appearance of every value of the ordered
      collection of integers in Values to precede the first
      appearance of the next value in Values in the ordered collection of 
      elements in Collection, i.e. the precede/3 constraint to hold
      for every adjacent integers in Values.
</P><P>
      This constraint is known as int_value_precede_chain in the Global
      Constraint Catalog, and is implemented using Gecode's precede()
      constraint (variant with IntArg argument for Values).
</P>
    "),
    see_also:[precede/3,collection_to_list/2]
    ]).

%----------------------------------------------------------------------

:- comment(table/2, [
    template: "<ConsistencyModule:> table(+Vars, ++Table)",    
    summary:"Constrain Vars' solutions to be those defined by the tuples in Table.",
    amode:table(+,++),
    args:[
	"Vars": "Collection of N (domain) variables or integers,
 or a collection of a collection of N (domain) variables or integers.",
	"Table":"Collection of tuples, each of which is a collection
 of N integer values"
    ],
    kind: [constraint:[extra:[gccat:in_relation]]],
    eg: "
[eclipse 9]: table([5,3,3], [[](5,2,3),[](5,2,6),[](5,3,3)]).  % succeed
                                                               
[eclipse 10]:  table([[5,3,3],[5,2,3]],  
                     [[](5,2,3),[](5,2,6),[](5,3,3)]).         % succeed

[eclipse 11]: table([5,3,2], [[](5,2,3),[](5,2,6),[](5,3,3)]). % fail

[eclipse 12]: L = [A,B,C], table(L, [[](5,2,3),[](5,2,6),[](5,3,3)]), 
        labeling(L), writeln(L), fail.
[5, 2, 3]
[5, 2, 6]
[5, 3, 3]

No (0.00s cpu)

",
    see_also: [table/3],
    desc: html("\
   table is an extensional constraint or user defined constraint, i.e.
   the solutions for the each posted constraint is explicitly defined within 
   the constraint. Each table constraint specifies the solutions to N
   variables, with all the solutions for this constraint specified in Table,
   in the form of tuples, each of N values that is one solution to the
   constraint. 
</p><p>
   Vars represents the variables that are to be satisfied for this
   constraint. It can be one collection of N variables (or integers),
   or a collection of a collections of N variables (or integers), if
   the constraint is to be satisfied by more than one collection of 
   variables. Posting the constraint with multiple collections of 
   variables is logically equivalent to posting individual table
   constraint with the same Table for each collection, but should be
   more efficient as the same Table is shared by all the collections.
</p><p>
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_gac for generalised arc consistency (domain consistency).
</p><p>
   This constraint is known as in_relation in the global constraint catalog, 
   with the allowance for multiple collections of variables taken from
   SICStus Prolog's table/2 constraint. This constraint is implemented in
   Gecode as the extensional() constraint with the variant that takes a
   TupleSet as an argument.
</p>
")
]).

:- comment(table/3, [
    template: "<ConsistencyModule:> table(+Vars, ++Table, +Option)",    
    summary:"Constrain Vars' solutions to be those defined by the tuples in Table.",
    amode:table(+,++,+),
    args:[
	"Vars": "Collection of N (domain) variables or integers,
 or a collection of a collection of N (domain) variables or integers.",
	"Table":"Collection of tuples, each of which is a collection
 of N integer values",
        "Option": "the atom 'mem' or 'speed' or 'default'"
    ],
    kind: [constraint:[extra:[gccat:in_relation]]],
    eg: "
[eclipse 9]: table([5,3,3], [[](5,2,3),[](5,2,6),[](5,3,3)], speed).  % succeed
                                                               
[eclipse 10]: table([[5,3,3],[5,2,3]],  
                     [[](5,2,3),[](5,2,6),[](5,3,3)], default).       % succeed

[eclipse 11]: table([5,3,2], [[](5,2,3),[](5,2,6),[](5,3,3)], mem).   % fail

",

    see_also: [table/2],
    desc: html("\
   table is a user defined constraint, i.e. the solutions for the each 
   posted constraint is explicitly defined within the constraint. Each 
   table constraint specifies the solutions to N variables, with all
   the solutions for this constraint specified in Table, in the form
   of tuples, each of N values that is one solution to the constraint. 
</p><p>
   Vars represents the variables that are to be satisfied for this
   constraint. It can be one collection of N variables (or integers),
   or a collection of a collections of N variables (or integers), if
   the constraint is to be satisfied by more than one collection of 
   variables. Posting the constraint with multiple collections of 
   variables is logically equivalent to posting individual table
   constraint with the same Table for each collection, but should be
   more efficient as the same Table is shared by all the collections.
</p><p>
   Option currently allows the selection of algorithm to use for the 
   propagation: mem for an algorithm that prefer smaller memory
   consumption over amount of  computation, speed for an algorithm that
   prefer reducing computation over memory consumption, and default
   for the default algorithm (as defined by Gecode). Note that table/2
   is mapped to table/3 with Option set to default.
</p><p>
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_gac for generalised arc consistency (domain consistency).
</p><p>
   This constraint is known as in_relation in the global constraint catalog, 
   with the allowance for multiple collections of variables taken from
   SICStus Prolog's table/2 constraint. This constraint is implemented in
   Gecode as the extensional() constraint with the variant that takes a
   TupleSet as an argument.
</p>
")
]).

%----------------------------------------------------------------------

:- comment(regular/2, [
    template: "<ConsistencyModule:> regular(+Vars, ++RegExp)",    
    summary:"Constrain Vars' solutions to conform to that defined in"
            " the regular expression RegExp.",
    amode:regular(+,++),
    args:[
	"Vars": "Collection of (domain) variables or integers,
 or a collection of a collection of (domain) variables or integers.",
	"RegExp":"A regular expression"

    ],
    kind: [constraint],
    eg: "
[eclipse 8]: regular([0,0,0,1,1,0,0], *(0) + *(1) + +(0)).  % succeed

[eclipse 9]:  L = [A,B,C,D,E], regular(L,  *(0) + *(1) + +(0)), 
               labeling(L), writeln(L), fail.
[0, 0, 0, 0, 0]
[0, 0, 0, 1, 0]
[0, 0, 1, 0, 0]
[0, 0, 1, 1, 0]
[0, 1, 0, 0, 0]
[0, 1, 1, 0, 0]
[0, 1, 1, 1, 0]
[1, 0, 0, 0, 0]
[1, 1, 0, 0, 0]
[1, 1, 1, 0, 0]
[1, 1, 1, 1, 0]

No (0.00s cpu)

[eclipse 10]: regular([A,B,C,D,E,F], ([1, 3, 5], {1, 2}) + *(4))

A = A{[1, 3, 5]}
B = B{[1, 3 .. 5]}
C = 4
D = 4
E = 4
F = 4

",
    see_also: [extensional/4, table/2, table/3],
    desc: html("\
   regular is a user defined constraint, i.e. the solutions for the
   each posted constraint is defined within the constraint. For regular, 
   the regular expression in RegExp defines the sequence of values that 
   the variables for the constraint can take. 
</p><p>
   Vars represents the variables that are to be satisfied for this
   constraint. It can be one collection of variables (or integers),
   or a collection of a collections of variables (or integers), if
   the constraint is to be satisfied by more than one collection of 
   variables. Each collection can be of different size, i.e. have
   different number of variables. Posting the constraint with multiple 
   collections of variables is logically equivalent to posting individual 
   constraint with the same RegExp for each collection, but should be
   more efficient as the same RegExp is shared by all the collections.
</p><p>
   RegExp is a regular expression that defines the allowable sequence of
   values that can be taken by the variables. The syntax is as follows:
   <DL>
   <DT><STRONG>N</STRONG><DD>
	    N is the integer value taken by a variable.
   <DT><STRONG>+(RegExp)</STRONG><DD>
	    RegExpr is repeated 1 or more times
   <DT><STRONG>*(RegExp)</STRONG><DD>
	    RegExpr is repeated 0 or more times
   <DT><STRONG>RegExp1 + RegExp2</STRONG><DD>
	    RegExp1 is followed by RegExp2
   <DT><STRONG>(RegExp1 | RegExp2)</STRONG><DD>
	    RegExp1 and RegExp2 are alternatives
   <DT><STRONG>(RegExp,  {N,M})</STRONG><DD>
            RegExp is repeated at least N times, and at most M times.
            (N, M are non-negative integers)
   <DT><STRONG>(RegExp,  {N})</STRONG><DD>
            RegExp is repeated at least N times (N is a non-negative integer)
   <DT><STRONG>ValueCollection</STRONG><DD>
            ValueCollection is a collection of integer values. Each value
            is a value that the variable is allowed to take in this position.
   </DL>
</p><p>
   Regular expression uses existing standard ECLiPSe operators and
   functors, so the syntax is slightly different from the standard
   syntax, and it maps to the regular expression used by Gecode
   for this constraint. For example, the following
<pre>
        RegExp = +(0) + (1, {3,3})
</pre>
  specifies a sequence of 1 or more 0s followed by 3 1s. e.g. [0,0,1,1,1].
</p><p>
   The possible values for a sequence of variables can also be specified
   by extensional/4, but using regular expression is probably more
   convenient, both constraints map to same underlying implementation.
   For a sequence of fixed length, the solutions can also be specified
   using the table/2,3 constraints.
 </p><p>
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_gac for generalised arc consistency (domain consistency).
</p><p>
   This constraint is implemented in Gecode as the extensional() constraint 
   with the variant that takes a DFA (Deterministic Finite-state
   Automaton) as an argument, with the regular expression converted to a DFA.
</p>
")
]).

%----------------------------------------------------------------------

:- comment(extensional/4, [
    template: "<ConsistencyModule:> extensional(+Vars, ++Transitions, +Start, +Finals)",    
    summary:"Constrain Vars' solutions to conform to the finite-state "
            "automaton specified by Transitions with start state Start"
            " and  final states Finals.",
    amode:extensional(+,++,+,++),
    args:[
	"Vars": "Collection of (domain) variables or integers,
 or a collection of a collection of (domain) variables or integers",
	"Transitions":"A collection of transitions of the form trans{f,"
                     "l,t)",
        "Start":"Start state (non-negative integer)",
        "Finals":"Final states (collection of non-negative integers)"
    ],
    kind: [constraint],
    eg: "
[eclipse 7]: L = [A,B,C,D,E], extensional(L, [trans(0,0,0),trans(0,1,1),trans(1,0,0)], 0, [0]), labeling(L), writeln(L), fail.
[0, 0, 0, 0, 0]
[0, 0, 0, 1, 0]
[0, 0, 1, 0, 0]
[0, 1, 0, 0, 0]
[0, 1, 0, 1, 0]
[1, 0, 0, 0, 0]
[1, 0, 0, 1, 0]
[1, 0, 1, 0, 0]

No (0.00s cpu)

",
    see_also: [regular/2, table/2, table/3],
    desc: html("\
   extensional is a user defined constraint, i.e. the solutions for each 
   posted constraint is defined within the constraint. For extensional, 
   the solutions is defined by the deterministic finite-state automaton (DFA)
   specified by the transitions in Transitions with start state Start and 
   final states Finals.The DFA defines the sequence of values that the 
   variables for the constraint can take. 
</p><p>
   Vars represents the variables that are to be satisfied for this
   constraint. It can be one collection of variables (or integers),
   or a collection of a collections of variables (or integers), if
   the constraint is to be satisfied by more than one collection of 
   variables. Each collection can be of different size, i.e. have
   different number of variables. Posting the constraint with multiple 
   collections of variables is logically equivalent to posting individual 
   constraint with the same DFA for each collection, but should be more
   efficient as the same is shared by all the collections.
</p><p>
   A collection of variables in Vars represents a sequence (i.e. they
   are ordered), and the DFA describes the values that can be taken by
   the sequence of variables, starting from the first variable, the
   DFA starts at the Start state, and moves to the next variable in
   the sequence via a transition given in Transition. A transition is
   a triple (from,to,input) that specifies the move from one state of the 
   DFA to another (from,to), accompanied by the labelling of the original
   variable with the value specified by input. A transition is specified  
   using the named structure trans(f,l,t), for the transition from
   state f to t (states are non-negative integers), and l is the input
   -- the integer value a variable is labelled to by the transition.
   The DFA is deterministic in that there should be at most a single 
   transition for for each unique input and from state. 
</p><p>
   In addition to the transitions, the DFA requires a unique start
   state, which is given by Start, and must terminate in one of the final 
   states, which is given in Finals as a collection of values. The
   Transitions, Start and Finals are mapped onto the data structures
   for representing a DFA in Gecode. Note however that for Gecode, both
   the final states and the Transitions must be terminated with a
   dummy entry (finals with a -1 state, and transitions with a
   transition with a -1 for the from state), these dummy entries are
   added by gfd before the constraint is passed to Gecode, so the user
   should not supply these entries.
</p><p>
   The possible values for a sequence of variables can also be specified
   by regular/2, and using regular expression is probably more
   convenient. Both constraints map to same underlying implementation.
   For a sequence of fixed length, the solutions can also be specified
   using the table/2,3 constraints.
</p><p>
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_gac for generalised arc consistency (domain consistency).
</p><p>
   This constraint is implemented in Gecode as the extensional() constraint 
   with the variant that takes a variant that takes a DFA as an argument.
</p>
")
]).

  
%----------------------------------------------------------------------

:-comment(search/6,[
summary:"Interface to gecode search-engines to perform search in gecode.",
amode:(search(+,++,++,+,++,+) is nondet),

args:[
      "L" : "is a collection (a la collection_to_list/2) of (domain)
	    variables (Arg = 0) or a collection of terms (Arg > 0)",

      "Arg" :"is an integer, which is 0 if L is a collection of
	    domain variables or greater than 0 if L consists of terms of
	    arity greater than Arg, the value Arg indicates the
	    selected argument of the term",


      "Select" :  "is a predefined variable selection method. Available methods are 
            input_order, first_fail, anti_first_fail, smallest, largest, 
            occurrence, anti_occurrence, most_constrained, 
            most_constrained_per_value, least_constrained_per_value, 
            max_regret, max_regret_lwb, min_regret_lwb, max_regret_upb,
            min_regret_upb, random, random(+Seed), 
	    max_weighted_degree, max_weighted_degree_per_value, 
	    min_weighted_degree, min_weighted_degree_per_value, 
	    max_activity, max_activity_per_value, 
	    min_activity, min_activity_per_value,
	    max_weighted_degree(+WP), max_weighted_degree_per_value(+WP), 
	    min_weighted_degree(+WP), min_weighted_degree_per_value(+WP), 
	    max_activity(+AP), max_activity_per_value(+AP), 
	    min_activity(+AP), min_activity_per_value(+AP)",

      "Choice" :  "is the name of a predefine value choice method for choosing
            the value to try for a variable; Predefined choice methods are:
            indomain, indomain_reverse_enum, min, max, median, split, 
            reverse_split, random, random(+Seed), interval_min, interval_max,
            from_smaller(P,Vals), from_larger(P,Vals), from_down(P,Vals),
            from_up(P,Vals)",

      "Method" :  "is one of the following:  complete,
            bb_min(Cost:domain variable),
	    restart_min(Cost:domain variable),
	    restart_min(Cost:domain variable,Cutoff),
	    restart_min(Cost:domain variable,Cutoff,Lim),
            restart(Cutoff), restart(Cutoff,Lim)",

       "Option" :  "is a list of option terms.  Currently recognized are:
	  tiebreak(+Select), stats(+Stats), limits(+Stop), 
          timeout(+Seconds), control(+Control), backtrack(-N), 
          nodes(+N), ldsb_syms(+Syms)"
],
kind: [search],
desc:html("<b>Search/6</b> provides an interface to gecode's search-engine,
allowing search to be performed by gecode. It is designed to have the same 
arguments as the generic search/6 routine available for integer domain solvers.
so that for common cases, the call will work for both search/6. The generic
search/6 is available in the gfd_search module. The difference is that here
the search is performed by gecode, and is an atomic step when viewed from
ECLiPSe. In addition, this predicat support features of gecode's search
engine not in generic search, such as variable selection methods based
 on propagation history, optimising and restart-based search methods, 
symmetry breaking with LDSB and searching in parallel.  
<p?
The predicate perform search on the search variables in <b>L</b>, using the
search method specified by <b>Method</b>, with variable and value selection
methods specified by <b>Select</b> and <b>Choice</b>,respectively. For search
methods that cab produce multiple solutions, backtracking into the predicate
will produce the next solution if it exists. The search can be controlled 
further via the terms specified in <b>Options</b>.
</P><P>
In order to allow more structure in the application program, L can be a
collection of compound terms rather than just domain variables. In this 
way all information about some entity can be easily grouped together. 
The terms can include items that are associated with the
domain variable needed in some of the labelling methods. In addition, 
to allow L to reflect structures in the problem, it can be a nested
collection such as a matrix.
<p>
Search methods are implmeneted by gecode's search engines. These include
optimising and restart based search methods, which are not available in
generic search. The optimising search methods provide some of the 
functionality in.<b>lib(branch_and_bound)</b>, and is likely to be
faster, but less flexible. The optimising search methods returns a
single optimal solution by finding succesively better feasible solutions
until no better solution can be found. If the search is terminated early, 
e.g. because of time-out, the best feasible solution, will be returned. 
<p>
Symmetry breaking using Lightweight Dynamic Symmetry Breaking (LDSB) can be
specified in the Option argument, and can be used with all search methods 
and most variable selection methods.
<p>
The variable selection and value choice methods are defined by gecode. They
are mapped to the closest matching methods in the generic search/6 (or with
a name following the same convention if the method have no correspondence).
Note that except for indomain and indomain_reverse_enum, the indomain style
selection methods of generic search, are not supported. Instead, most
value choice methods makes a two-way choice like the methods in try_value/2.
At each choice, a variable is selected, and two alternatives are
 created that reduces the variable's domain in complementary ways. 
<p>
For source compatibility with code written for the IC and FD solvers, 
the generic search's indomain style method names are also accepted,
but are mapped to the closest two-way choice method, e.g.\
indomain_min is translated to min.
<p>
For variable selection, if several entries have the same heuristic value, 
then a tiebreak selection method, specified by the tiebreak option term, can 
be used to chose from these entries.  
<p>
Several of the variable selection methods makes use of one of two 
dynamic measurements of gecode's constraint propagation performed 
by the program so far: 1) Weighted degree, called AFC 
(accumulated failure count) in gecode; 2) Activity. These two measurements
may make better variable selection in many programs because it is based
on actual constraint propagation performed.
<p>
Weighted degree is a count of the number of failures so far of
propagators associated with the variable. By default, the starting count
is set to the number of propagator attached to the variable,
to give reasonable starting values for variable selection.
<p>
Activity is a count of the number of domain reductions on the variable
during propagation so far. 
<p>
A decay factor (between 0 and 1) can be specified for both measure so that
the count for a variable can be reduced by the Factor if it is not 
incremented. 
</p><p>
The pre-defined <b>selection methods</b> (with the gecode name in brackets) 
use the following criteria:
<ul>
<li><b>input_order</b> (INT_VAR_NONE) the first entry in the list is selected</li>
<li><b>random</b> (INT_VAR_RND) an entry is selected at random.</li>

<li><b>random(+Seed)</b> (INT_VAR_RND) an entry is selected at random,
 with the random seed Seed. Seed is a non-negative integer.</li>

<li><b>anti_occurrence</b> (INT_VAR_DEGREE_MIN) the entry whose corresponding gecode variable with the
smallest number of attached propagators is selected</li>

<li><b>occurrence</b> (INT_VAR_DEGREE_MAX) the entry whose corresponding gecode variable with the
largest number of attached propagators is selected</li>

<li><b>max_weighted_degree max_weighted_degree(+WDParams)</b> (INT_VAR_AFC_MAX)
the entry with the largest weighted degree is selected. The optional WDParams
argument is a list of parameters for initialising the weighted degree.
</li>

<li><b>max_weighted_degree_per_value  max_weighted_degree_per_value(+WDParams)</b> 
(INT_VAR_AFC_SIZE_MIN) the entry with the smallest weighted degree divided by 
domain size is selected. The optional WDParams argument is a list of 
parameters for initialising the weighted degree. The parameters are:

<ul>
<li><b>decay(Decay)</b> specify the decay factor (number between
 0 and 1, where 1 is no decay). Default is the existing decay factor 
for weighted degree (normally 1, unless explicitly changed).
</li>
<li><b>init(Init)</b> re-initialise the weighted degree values before
starting the search according to Init. Init can be:
<ul>
<li>degree(+Factor)</b>  set the value of the weighted degree
for a problem variable to Factor*degree for the variable. Factor is a 
non-negative number. If Factor is 1, then the weighted degree for each 
variable will be its degree, the default initial value for weighted
 degree.
</li>
<li><b>none</b> no initialisation is done. This is the default.
</li></ul>
</li></ul>
</li> 

<li><b>min_weighted_degree min_weighted_degree(+WDParams)</b> (INT_VAR_AFC_MIN) the entry with the smallest
weighted degree is selected. The optional WDParams
argument is a list of parameters for initialising the weighted degree.</li>

<li><b>min_weighted_degree_per_value  min_weighted_degree_per_value(+WDParams)</b> 
(INT_VAR_AFC_SIZE_MAX) the entry with the largest weighted degree divided by 
domain size is selected. The optional WDParams
argument is a list of parameters for initialising the weighted degree.</li> 

<li><b>max_activity max_activity(+AParams)</b> (INT_VAR_ACTIVITY_MAX) the entry with the
 largest activity is selected. Activity for a variable  Activity is a
 measure of how much that variable was involved in constraint
 propagation. Note that activity is maintained by gecode's search engine 
 for the search variables, and is not available outside.
<p>
The parameters are:
<ul>
<li><b>decay(Decay)</b> specify the decay factor (number between
 0 and 1, where 1 is no decay). Default is 1.
</li>
<li><b>init(Init)</b> set the start values for activity for the
search variables according to Init. Init can be:
<ul>
<li> <b>degree(+Factor)</b> set the start value to 
Factor*degree for the variable. Factor is a non-negative number.
</li>
 <li><b>vals(Pos,Starts)</b> set the start values to those specified in
Starts. Starts is a collection of either the start values (Pos=0), or 
compound terms containing the value, in the Pos'th argument. The number
of start values is the same as the number of variable, and each value
is a non-negative number. 
</li>
 <li><b>none</b> no initialisation (default)
</li>
</ul>
</li>
</ul>

</li>

<li><b>max_activity_per_value max_activity_per_value(+AParams)</b> (INT_VAR_ACTIVITY_SIZE_MAX) the entry with the
largest activity divided by domain size is selected.
</li>

<li><b>min_activity min_activity(+AParams)</b> (INT_VAR_ACTIVITY_MIN) the entry with the
 smallest activity is selected. </li?

<li><b>min_activity_per_value min_activity_per_value(+AParams)</b> (INT_VAR_ACTIVITY_SIZE_MIN) the entry with the
smallest activity divided by domain size is selected.
</li>

<li><b>smallest</b> (INT_VAR_MIN_MIN) the entry with the smallest value in the domain is selected</li>

<li><b>smallest_upb</b> (INT_VAR_MIN_MAX) the entry with the smallest
 upper bound in the domain is selected</li>

<li><b>largest_lwb</b> (INT_VAR_MAX_MIN) the entry with the largest lower
  bound in the domain is selected</li>

<li><b>largest</b> (INT_VAR_MAX_MAX) the entry with the largest value in the domain is selected</li>

<li><b>first_fail</b> (INT_VAR_SIZE_MIN) the entry with the smallest domain size is selected</li>

<li><b>anti_first_fail</b> (INT_VAR_SIZE_MAX) the entry with the largest domain size is selected</li>

<li><b>least_constrained_per_value</b> (INT_VAR_DEGREE_SIZE_MAX) the entry with the largest number of attached propagators divided by domain size.</li> 

<li><b>most_constrained_per_value</b> (INT_VAR_DEGREE_SIZE_MIN) the entry with the smallest 
number of attached propagators divided by the domain size.</li> 


<li><b>min_regret_lwb</b> (INT_VAR_REGRET_MIN_MIN) the entry with the smallest difference between the
smallest and second smallest value in the domain is selected.</li>

<li><b>max_regret</b> (INT_VAR_REGRET_MIN_MAX) the entry with the largest difference between the
smallest and second smallest value in the domain is selected. This method is
typically used if the variable represents a cost, and we are interested in the
choice which could increase overall cost the most if the best possibility is
not taken. Unfortunately, the implementation sometimes does not always
work. If two decision variables incur the same minimal cost, the regret is not
calculated as zero, but as the difference from this minimal value to the next
greater value. Note this is an alias for max_regret_lwb</li>

<li><b>max_regret_lwb</b> (INT_VAR_REGRET_MIN_MAX) is an alias to max_regret.</li>

<li><b>min_regret_upb</b> (INT_VAR_REGRET_MAX_MIN) the entry with the smallest difference between the
largest and second largest value in the domain is selected.</li>

<li><b>max_regret_upb</b> (INT_VAR_REGRET_MAX_MAX) the entry with the largest difference between the
largest and second largest value in the domain is selected.</li>

<li><b>most_constrained</b> (INT_VAR_SIZE_MIN, INT_VAR_DEGREE_MAX) the entry with the smallest domain size is
 selected. If several entries have the same domain size, the entry with the
 largest number of attached constraints is selected. This is provided for
 compatibility, as this define a tiebreak method (occurrence). Any tiebreak
 method defined in options is ignored.</li>

</ul><p>
The pre-defined <b>choice methods</b> (with gecode name in brackets) have the following meaning:
<ul>
<li><b>indomain</b> (INT_VALUES_MIN) 
Values are tried in increasing order.</li>

<li><b>indomain_reverse_enum</b> (INT_VALUES_MAX)
Values are tried in decreasing order.</li> 

<li><b>min</b> (INT_VAL_MIN)
Minimum value in the domain (alias: indomain_min). </li>

<li><b>max</b> (INT_VAL_MAX)
Maximum value in the domain (alias: indomain_max). </li>

<li><b>median</b>(INT_VAL_MED)
Median value of the domain Note median is defined differently from IC (alias: indomain_median). 
</li>

<li><b>random</b> (INT_VAL_RND)
Random element in the domain is selected (alias: indomain_random).</li> 

<li><b>random(+Seed)</b> (INT_VAL_RND)
Random element in the domain is selected  The random sequence is
initialised with seed Seed, which is a non-negative integer.
</li> 

<li><b>split</b> (INT_VAL_SPLIT_MIN)
try first the lower domain half, then the upper (alias: indomain_split).</li>

<li><b>reverse_split</b> (INT_VAL_SPLIT_MAX)
try first the upper domain half, then the lower 
(alias: indomain_reverse_split).</li>

<li><b>interval_min</b> (INT_VAL_RANGE_MIN)
If the domain consists of several intervals, chose the smallest interval.
For one interval, split is used.
(alias: indomain_interval)
</li>

<li><b>interval_max</b> (INT_VAL_RANGE_MAX)
If the domain consists of several intervals, chose the largest interval.
the interval, choosing the largest interval.  For one interval,
 reverse_split is used 
</li>

<li><b>from_larger(+Pos,+Suggestions)</b> (INT_VAL_NEAR_MAX)
Suggestions is a collection containing suggested values, one for each search
 variable. For each variable, pick the domain value nearest its
corresponding suggested value. If there is a tie, chose the larger
 value.
<p>
If Pos is 0, then Suggestions is a collection of values. If Pos &gt; 0,
 then Suggestions is a collection of structures, with the suggested
 value in the Pos'th argument of the structure. The idea is that the same
structure can be used to store both the variable and its suggested
 value, e.g. as a pair Var-2.
</li>

<li><b>from_smaller(+Pos,+Suggestions)</b> (INT_VAL_NEAR_MAX)
similar to from_larger, except chose the smaller value if there is a tie.
</li>

<li><b>from_down(+Pos,+Suggestions)</b> (INT_VAL_NEAR_DEC)
similar to from_larger, except chose values smaller than the suggested
 value first.
</li>

<li><b>from_up(+Pos,+Suggestions)</b> (INT_VAL_NEAR_INCC)
similar go from_down, except chose values larger than its suggested
value first.  
</li>

</ul><p>

<p>
The different <b>search methods</b> are
<ul>
<li><b>complete</b> (DFS)
a complete search routine which explores all alternative choices. Alternative
solutions are returned on backtracking</li>


<li><b>bb_min(Cost)</b> (BAB)
Branch-and-bound search to find the minimal value for the cost variable Cost.
This should be a domain variable that is instantiated at the end of the
search. The search will return an optimal solution, unless terminated early,
in which case, the best solution found (if there is one) is returned. If Cost
variable is not instantiated at the end of the search, the search is aborted.
This provide some of the functionality of branch-and-bound search in
lib(branch_and_bound), but is less flexible (no user defined search) but is 
likely to be faster. Will only return one optimal solution.

<li><b>restart(+Cutoff [,+Nogoodslim])</b> (RBS with DFS)
Restart search will restart the search when the number of nodes searched
exceeds the cutoff threshold. The threshold can change with successive
restart, and is specified by Cutoff. Nogoodslim is an optional argument, which
if supplied, nogoods learning will be performed during the search, and
no-good constraints learned from the old search are posted before restarting
the search.
<p>
Cutoff specifies the cutoff threshold sequence. The following are available:

<ul>
<li><b>geo(+S, +Base)</b> Geometric cutoff with a scale factor S and base Base (both
non-negative integers). The cutoff thresholds are defined by S*(Base^N), where
N is the Nth restart.
</li>
<li><b>luby(+S)</b> the cutoff sequence is based on the Luby sequence, multiplied by 
the scale factor S (non-negative integer). The Luby sequence is a sequence of 
restart thresholds suggested by Luby and others in 1993.
</li>
<li><b>random(+Min,+Max,+N,+Seed)</b> the cutoff value is chosen randomly between Min and
Max, with only N+1 values distributed evenly in the range are available for
selection, to make sure that values chosen are significantly different from 
each other. 
</li>
<li><b>con(+C)</b> the same cutoff value C (positive integer) is used for all 
restarts.
</li>
<li><b<lin(+S)</b> The cutoff value is increased by S (positive integer) for each
restart.
</li>
</ul>
<p>
Nogoodslim (non-negative integer) specifies the depth limit in the search-tree to which no-goods will be learned. Setting this to 0 (zero) is the same as not
using no-goods. 

</li>
<li><b>restart_min(Cost [,+Cutoff [,+Nogoodslim]])</b> (RBS with BAB)
Branch-and-bound search as in bb_min, but the search is restarted after finding
a new solution and imposing the new bound. Optionally, if Cutoff is supplied,
the search will also restart when the number of nodes searched exceed the given
Cutoff. An additional optional argument NogoodsLim can also be supplied that
will use no-good learning.
<p>
</ul>
The option list is used to pass additional parameters to and from the
procedure.  The currently recognized options are:
<ul>
<li><b>tiebreak(+Selection)</b>
Selection is one of the variable selection methods, and is used as a tie-break
if the primary selection method yields more than one candidate. Obviously not
all combinations of selection methods makes sense (e.g. it should not be the 
same as the primary), but no check is done, they are simply passed to gecode.</li>
 <li><b>stats(+Stats)</b>
Stats is a named gfd_stats structure, defined as:
<pre>
:- export struct(gfd_stats(prop,fail,node,depth,mem)).
</pre>
The fields of the structure should be uninstantiated, and the search predicate
will instantiate the fields with statistics obtained from gecode for the search:
prop for the number of propagations, fail for the number of failed nodes,
node for number of nodes expanded, depth for maximum depth of search stack,
mem for peak memory allocated (in bytes).</li>
<li><b>timeout(+Seconds)</b>
Specify the number of seconds that the search will be performed before it is
terminated. Seconds can be a real or integer number, and 0 means there is
no timeout. For multi-solution search, the timer is reset each time a
new solution is obtained. For optimising search, the best feasible
solution found so far, if any, is returned. 

<li><b>limits(+Stats)</b>
Specify limits to stop the search. Stats is the same gfd_stats struct used for
obtaining statistics. To specify a limit for a particular statistics, the
corresponding field should be instantiated to the limit. Only the prop, node, 
fail and mem fields are significant. Entries in the other fields are ignored.
For optimising search, the best feasible solution found so far, if any, is 
returned.
</li>

<li><b>control(+Control)</b>
 Control is a named gfd_control structure, defined as:
 <pre>
 :- export struct(gfd_control(commit_distance,adaptive_distance,threads)).
</pre>
This is used to pass information to gecode to control the search. The
 corresponding field should be instantiated to the value passed to gecode. 
 threads may be of most interest as if threads is set to a value &gt;= 2,
 this will allow parallel search. See the gecode manual for more
 details on the options.</li>
 <li><b>backtrack(-N)</b>
Provided for compatibility with generic search/6. Returns the number of fail
 nodes (fail field of statistics.</li> 

<li><b>nodes(++N)</b>
Provided for compatibility with generic search/6. Equivalent to setting the
node field of limits. The node field will be unified with N</li>

<li><b>ldsb_syms(+Syms)</b>
Use Lightweight Dynamic Symmetry Breaking (LDSB) to reduce the search-space.
The symmetries for the problem are specified in Syms, which is a list of
symmetry specifications. The syntax for the symmetry specifications are
compatible with the ones used in \bip{ldsb_initialise/2} of lib(ldsb),
the LDSB library for IC.
<p>
Some of the symmetry specifications expect a matrix of NRows rows by NCols 
columns. Such an argument should be supplied as a 2-D collection, i.e. 
either as a Matrix M with dimensions dim(M, [NRows,NCols]),
or a list of NRows lists, where the sub-lists are all the same length of NCols 
items.
<p>
Some of the symmetry specifications can be applied to all the search variables
(i.e. the first argument (L) of the predicate), or to a subset.
For such specifications, if the specification includes a collection of
variables, these will be used; otherwise it is applied to all search variables.
Note that the same form is expected for variables given with the specification
and those in L, i.e. the variables will be in the same Arg'th position
of the items in the collection given for L and in any symmetry 
specifications.
<p>
The following are available:
<ul>
<li><b>value_interchange(+Vars)</b> specifies that the variables in collection 
Vars are interchangeable.
</li>
<li><b>variable_interchange(++Vals)</b> specifies that the values in collection
Vals are interchangeable. 
</li>
<li><b>variables_interchange</b> specifies that all search variables are 
interchangeable.
</li>
<li><b>parallel_variable_interchange(+Vars)</b> Vars is a 2-D collection of
search variables. This specifies that the variables
in the sub-list or sub-vector are interchangeable, e.g. 
parallel_variable_interchange([]([](A,B),[](C,D),[](E,F))) says that the
sequence A-B can be interchanged with the sequence C-D and E-F.
</li> 
<li><b>parallel_value_interchange(++Vals)</b> Vals is a 2-D collection of 
values. This specifies that the value sequences in each sub-vector or sub-list 
in Vals are interchangeable. 
</li> 
<li><b>value_reflection(+L,+U)</b> specifies the values L..U can be reflected,
i.e. vale L+i maps to U-1. 
</li> 
<li><b>rows_interchange rows_interchange(+MatVars)</b> specifies that 
the rows of the matrix is interchangeable. The matrix is either given
 in MatVars, or is all the search variables (L), and must be in 
matrix form.
</li> 
<li><b>columns_interchange columns_interchange(+MatVars)</b> specifies that
the columns of matrix are interchangeable. The matrix is either given in 
MatVars, or is all the search variables L, and must be
in matrix form.
</li> 
<li><b>row_reflection rows_reflection(+MatVars)</b>  specifies that the rows
of the matrix can be reflected around their centre. The matrix is either given in  MatVars, or is all the search variables L), and must be
in matrix form.
</li> 
<li><b>column_reflection columns_reflection(+MatVars)</b> specifies that the columns
of the matrix can be reflected around their centre. The matrix must be
 a square matrix, and can be either given in  MatVars, or is all the search variables (L), and must be
in matrix form.
</li> 
<li><b>diagonal_reflection diagonal_reflection(+MatVars)</b> specifies that the 
variables of the matrix can be reflected around the main diagonal of the
matrix. The matrix must be a square matrix, and is either given in  MatVars, or is all the search 
variables (L), and must be in matrix form.
</li>
</ul> 
</li> 
</ul>
"),
fail_if:"Fails if the search engine does not find any solution.
For partial search methods, this does not mean that the problem does not 
have a solution.",
resat: 'yes (non-optimising and non-restart searches)',
eg:"
top:-
	length(L,8),
	L :: 1..8,
	search(L,0,input_order,indomain,complete,[]).

top:-
        length(L,8),
        L::1..8,
        L = [Cost|L],
        search(L,0,input_order,indomain_max,bb_min(Cost),[]).

:- local struct(data(var,start)).
top(Ds) :-
	length(L, 8),
	L :: 1..8,
	(foreach(V, L),
 	 count(I, 1, _),
	 foreach(D, Ds) do
		D = data{var:V,start:I}
	),
	search(Ds, var of data, input_order, 
	       from_larger(start of data,Ds), complete, []).

",

see_also:[indomain/1,gfd_search:indomain/2,labeling/1,gfd_search:delete/5,gfd_search:search/6] 

]).


:-comment(select_var/5,[
summary:"Pick a domain variable from a collection according to selection criterion.",
amode:(select_var(-,+,+,+,?) is semidet),

args:[
      "X" : " a free variable",
      "Vars" : " a collection of existing domain variables or terms, or a handle ",
      "Handle" : "free variable, will be bound to handle for remaining collection",
      "Arg" : " an integer",
      "Select" : "a predefined selection method."
],
kind: [search],
desc:html("<p>
This predicate picks one domain variable in Vars based on some selection 
criterion. The selected entry is returned in X. Vars is either a 
collection of domain variables/terms containing domain variables, or 
a handle representing domain variables returned in Handle from a
previous call to select_var/5. 
</p><p>
This predicate provides similar functionality as delete/5 of gfd_search,
and is designed to be used in a similar way -- the selection is done on
the variables represented in Vars, while Handle is then passed as the
next Vars argument for variable selection in the next call to select_var/5,
as is done with Rest for delete/5. select_var/5 can thus be used as a 
replacement for delete/5 in gfd_search:search/6.
</p><p>
The main difference with delete/5 is that Handle is a low-level
representation of all the domain variables in Vars, rather than the
rest of the domain variables with the selected variable removed as in Rest
for delete/5. This allows select_var/5 to be used for both the 'indomain'
style labelling (where a selected variable is labelled to different values on 
backtracking), or the more Gecode-like labelling (where variable
selection and a binary value choice is performed for each labelling step).
Unlike delete/5, a domain variable that is instantiated will not be selected,
and the search is complete when select_var/5 fails because all the 
domain variables in Vars are instantiated. 
</p><p>
When select_var/5 is called with Vars being a collection, the domain variables
in the collection are extracted according to Arg in the same way as
delete/5, i.e. the Arg'th argument of each element in the collection is the
domain variable for that element. In addition to creating the low-level
 handle representation of the domain variables in Handle, additional
initialisation is done for some selection methods that have initialisation
parameters (i.e. those involving weighted degree or activity). When 
select_var/5 is called with Vars being a handle created from a previous 
call to select_var/5, then Args and any initialisation parameters given 
with Select are ignored. 
</p><p>
Select is one of the following predefined selection methods:
input_order, occurrence, anti_occurrence, 
smallest, largest, smallest_upb, largest_lwb,
first_fail, anti_first_fail, 
most_constrained,  most_constrained_per_value, least_constrained_per_value, 
max_regret, max_regret_lwb, min_regret_lwb, max_regret_upb.
max_weighted_degree, min_weighted_degree,
max_weighted_degree_per_value, min_weighted_degree_per_value,
max_activity, min_activity,
max_activity_per_value, min_activity_per_value
</p><p>
These are essentially the same selection methods supported for using
Gecode's search engine (search/6), except for random, which is not
supported here. For methods that uses activity or weighted degree,
Select can include an optional argument in the form of a list, where each 
list item is a parameter setting. If a parameter is not specified in the
list, the default setting for the parameter will be used.
These parameters are:
</p><p>
For weighted degree:
<ul>
<li><b>decay(Decay)</b> specify the decay factor (number between
 0 and 1, where 1 is no decay). Default is the existing decay factor 
for weighted degree (normally 1, unless explicitly changed).
</li>
<li><b>init(Init)</b> set the start values for activity for the
search variables according to Init. Init can be:
<ul>
<li> <b>degree(+Factor)</b> set the start value to 
Factor*degree for the variable. Factor is a non-negative number.
</li>
 <li><b>vals(Pos,Starts)</b> set the start values to those specified in
Starts. Starts is a collection of either the start values (Pos=0), or 
compound terms containing the value, in the Pos'th argument. The number
of start values is the same as the number of variable, and each value
is a non-negative number. Starts can be the same collection as Vars,
with each item in the collection storing both a variable and its
activity start value.
</li>
 <li><b>none</b> no initialisation (default)
</li>
</ul>
</ul>
</p><p>
For activity:
<ul>
<li><b>decay(Decay)</b> specify the decay factor (number between
 0 and 1, where 1 is no decay). Default is 1.
</li>
<li><bi>init(Init)</bi> set the initial values for activity according
 to Init. Init can be:
<ul>
<li> <b>degree</b? the initial value for each search variable will be the degree (number of
 propagator) for that variable.</li>
 <li><b>vals(Starts)</b> set the initial values to those specified in
 Starts.Starts is a collection of non-negative numbers, and is the
 same size as Vars, i.e. each item in Starts represents the initial
 activity value of the corresponding search variable in Vars.
 corresponding variable in Vars.
</li>
 </li><b>none</b> no initialisation (default)
</li>
</ul>
</li>
</ul>

Due to the way gfd implement activity, there are some limitations to
the way activity can be used. Gecode requires activity to be declared
for a fixed set of variables -- and activity information for these variables
will then be collected subsequently. In gfd this is done during the
 initialisation call to select_var/5. Thus, to use selection methods
 that involve activity, it must be used in the initialisation call. In
addition, gfd supports only one activity declaration per Gecode solver
 state, so activity can only be used as a selection method for one set
 of variable in each branch of the search.  
 </p>
"),
eg: "
% Simple labelling implemented using select_var/5 and indomain/2
labelling1(Vars, Select, Choice) :-
        (select_var(V, Vars, Rest, 0, Select) ->
            indomain(V, Choice),
            labelling1(Rest, Select, Choice)
        ;
            true
        ).

% Variant using select_var/5 and try_value/2
labelling2(Vars, Select, Choice) :-
        (select_var(V, Vars, Rest, 0, Select) ->
            try_value(V, Choice),
            labelling2(Rest, Select, Choice)
        ;
            true
        ).



        % A call with max_activity with parameters
        select_var(V, Vars, Rest, 0, 
                   max_activity([init(degree),
                                 decay(0.9)
                                ])),

",
fail_if:"fails if no variable can be selected",

see_also:[try_value/2,gfd_search:indomain/2,search/6, gfd_search:search/6]

]).


:-comment(try_value/2,[
summary:"Two-way and multi-way choice predicate",
amode:(try_value(?,++) is nondet),
amode:(try_value(+,++) is det),
args:[
    "Var":"a domain variable or an integer",
    "Method":"an atom denoting the value choice method"
],
kind: [search],
desc:html("<P>
    This search predicate makes a choice on the domain of the variable.
    This choice can be a binary choice, or an indomain style multi-way
    choice which branches on different values in the domain of the variable.
</P><P>
    The binary choice methods create two search alternatives, which 
    reduce the variable domain  in complementary ways.  The details
    are determined by the Method.
</P><P>
    The first group of binary methods tries to set the variable to a 
    particular value, and in the alternative excludes this value from 
    the domain, logically:
<PRE>
	( Var = Value ; Var #\\= Value )
</PRE>
<DL>
    <DT>min</DT>
	<DD>try the minimum value in the domain</DD>
    <DT>max</DT>
	<DD>try the maximum value in the domain</DD>
    <DT>median</DT>
	<DD>try the median value in the domain</DD>
</DL>
    The next binary group only halves the domain in each alternative (where
    the split value is the arithmetic mean of the lower and upper domain
    bound, rounded down):
<PRE>
	( Var #=&lt; Split ; Var #&gt; Split )
</PRE>
<DL>
    <DT>split</DT>
	<DD>try first the lower domain half, then the upper</DD>
    <DT>reverse_split</DT>
	<DD>try first the upper domain half, then the lower</DD>
</DL>
    The indomain style group chooses an initial value from the variable's
    domain of according to the given method, and on backtracking 
    select other values in the domain. 
<DL>
    <DT>indomain_min</DT>
	<DD>Values are tried in increasing order.</DD>
    <DT>indomain_max</DT>
	<DD>Values are tried in decreasing order.</DD>
    <DT>indomain_median</DT>
	<DD>Values are tried starting from the median value of the 
        domain, then alternately the next larger and smaller values 
        in the domain.</DD>
    <DT>indomain_middle</DT>
	<DD>Values are tried starting from  the middle of the range
        (which does not need to be in domain), then alternately the
         next larger and smaller values in the domain.</DD>
    <DT>indomain_from(+Val)</DT>
	<DD>Values are tried starting from  Val (which does not need
        to be in domain), then alternately the next larger and smaller 
        values in the domain.</DD>
</DL> 
</P><P>
    As opposed to the value choice predicates indomain/1 and indomain/2,
    the binary choice methods of try_value/2 does not necessarily 
    instantiate the variable.  If used in a tree search, this means
    that the variable must remain available to the variable selection,
    as it may be selected repeatedly at different depth of the search
    tree.
</P><P>
    The indomain style methods do instantiate the variable. If used in
    a tree search, try_value represents a n-ary choice for all the
    values of a variable. In this case, the maximum depth of the
    search-tree is the number of problem variables. 
</P><P>
    This predicate should be more efficient than using generic value
    choice predicates or explicitly writing code to do the choice. The
    exclusion of values before a new choice is done by specialised
    low-level primitives that are more efficient than user-level
    constraints. This efficiency is particularly important because
    they make recomputation more efficient. For the binary choice
    methods, the exclusion of values are performed in every
    recomputation of the choice; and for the indomain style methods,
    the old values are excluded from the variable's domain to allow
    the next value to be chosen. On recomputation, the variable is
    directly set to the chosen value.  This reduces the amount of
    recomputation, but any reduction in the search space resulting
    from propagating the exclusion of old values may also be lost.
</P><P>
    try_value/2 is best used in combination with select_var/5. Both can
    be used to parameterise the generic gfd_search:search/6 procedure.
</P>"),
eg:"
    % Used with generic search/6
    gfd_search:search(Xs,0,select_var(first_fail),try_value(min),complete,[]).


    % Simple labelling implemented using select_var/5 and try_value/2
    labelling(Vars, Select, Choice) :-
        (select_var(V, Vars, Rest, 0, Select) ->
            try_value(V, Choice),
            labelling(Rest, Select, Choice)
        ;
            true
        ).
"
]).


:- comment(max_regret_lwb/2, [
        summary:"Generic search compatible variable selection method,"
                " returns the regret value for Var",
        args: [
                  "Var": "Domain variable or integer",
                  "Criterion": "Variable" 
              ],
        see_also: [gfd_search:delete/5, gfd_search:search/6],
        desc: html("<P>
This predicate is designed to be used as a variable selection method
with generic search (lib(gfd_search)), and should be used with 
gfd_search's delete/5 and search/6. The effect is to select the first
variable with the maximum regret (the largest difference between the 
smallest and second smallest value in the domain).
</P><P>
Unlike the built-in generic search's max_regret method, where the regret
value is calculated in ECLiPSe from the variable's domain, this predicate 
obtain the regret value from Gecode.
")
]).

:- comment(max_regret_upb/2, [
        summary:"Generic search compatible variable selection method,"
                " returns the upper-bound regret value for Var",
        args: [
                  "Var": "Domain variable or integer",
                  "Criterion": "Variable" 
              ],
        see_also: [gfd_search:delete/5, gfd_search:search/6],
        desc: html("<P>
This predicate is designed to be used as a variable selection method
with generic search (lib(gfd_search)), and should be used with 
gfd_search's delete/5 and search/6. The effect is to select the first
variable with the maximum regret on the upper bound (the largest
difference between the largest and second largest value in the domain).
</P><P>
This predicate obtain the regret value from Gecode.
")
]).
        
:- comment(max_weighted_degree/2, [
        summary:"Generic search compatible variable selection method,"
                " returns the weighted degree for Var",
        args: [
                  "Var": "Domain variable or integer",
                  "Criterion": "Variable" 
              ],
        see_also: [gfd_search:delete/5, gfd_search:search/6],
        desc: html("<P>
This predicate is designed to be used as a variable selection method
with generic search (lib(gfd_search)), and should be used with 
gfd_search's delete/5 and search/6. The effect is to select the first
variable with the maximum weighted degree.
</P><P>
This predicate obtain the weighted degree for the variable from Gecode.
")
]).
        
:- comment(max_weighted_degree_per_value/2, [
        summary:"Generic search compatible variable selection method,"
                " returns the domain size divided by weighted degree for Var",
        args: [
                  "Var": "Domain variable or integer",
                  "Criterion": "Variable" 
              ],
        see_also: [gfd_search:delete/5, gfd_search:search/6],
        desc: html("<P>
This predicate is designed to be used as a variable selection method
with generic search (lib(gfd_search)), and should be used with 
gfd_search's delete/5 and search/6. The effect is to select the first
variable with the maximum (domain size)/(weighted degree). (rounded to
 the nearest integer)
</P><P>
This predicate obtain the weighted degree for the variable from Gecode.
")
]).

:- comment(most_constrained_per_value/2, [
        summary:"Generic search compatible variable selection method,",
        args: [
                  "Var": "Domain variable or integer",
                  "Criterion": "Variable" 
              ],
        see_also: [gfd_search:delete/5, gfd_search:search/6],
        desc: html("<P>
This predicate is designed to be used as a variable selection method
with generic search (lib(gfd_search)), and should be used with 
gfd_search's delete/5 and search/6. The effect is to select the first
variable with the the largest value of (domain size)/(number of constraints).
(rounded to the nearest integer)
")
]).

/*
:-comment(indomain/2,[
summary:"a flexible way to assign values to finite domain variables",
amode:(indomain(?,++) is nondet),

args:[
"Var":"a domain variable or an integer",
"Method":"one of the atoms min, max, middle, median, split, interval, random or an integer"],
kind: [search],
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
    downwards, removing previously tested values on backtracking.</li>

<li><b>middle</b> Try the enumeration starting from the middle of the
    domain.  On backtracking, this chooses alternatively values above and
    below the middle value, until all alternatives have been tested.</li>

<li><b>median</b> Try the enumeration starting from the median value
    of the domain.  On backtracking, this chooses alternatively values
    above and below the median value, until all alternatives have been
    tested.</li>

<li><b>Value:integer</b> Like middle, but start with the given integer
    <b>Value</b></li>

</ul>
On backtracking, all methods except enum first remove the previously 
tested value before choosing a new one.  This sometimes can have a 
huge impact on the constraint propagation, and normally does not cause 
much overhead, even if no additional propagation occurs.
</p><p>
This predicate is partly based on indomain/2 from generic search. It is
optimised for gfd compared to generic search's indomain/2, and should 
be more efficient. Only a subset of the available methods available to
gfd_search's indomain/2 are implemented.
"),
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

",
see_also:[search/6,indomain/1,gfd_search:indomain/2]

]).
*/

%---------------------------------------------------------------------

:- comment(impose_min/2, [
    amode: impose_min(?, ++),
    args: [
    	"Var":   "(Domain) variable or integer",
	"Bound": "Lower bound (integer)"
    ],
    summary: "Update (if required) the lower bound of Var.",
    see_also: [impose_max/2, impose_bounds/3, impose_domain/2, exclude/2, exclude_range/3],
    kind:[dom],
    desc: html("<P>
   This predicate is provided mainly for compatibility with IC solver.
   If you intend to impose the same minimum on multiple variables, it
   is more efficient to use the lib(gfd) specific gfd_vars_impose_min/2.
</P><P>
   Primitive for updating the lower bound of Var so that it is at least
   Bound.  A bound update on a variable may fail (when the update empties
   the domain), succeed (possibly updating the variable's bounds), or
   instantiate the variable (in the case where the domain gets restricted to
   a singleton value).</P><P>
</P><P>
   Note that this predicate is intended for use only in implementing
   co-operation with other solvers and constraint propagators at the
   ECLiPSe level, and should not be called from ordinary user code.
   It differs from the usual constraint in that the propagation is not
   performed immediately. Normally, propagation in gecode is performed by
   a suspended goal that is scheduled and woken after each constraint is
   posted.  However, impose_min/2 does not call the woken goal
   scheduler (wake/0), so the propagation goal may not be executed 
   immediately. (It is possible that under some circumstances the goal 
   will be executed, if wake/0 is called indirectly - one example would be
   by the unify handler if the variable becomes ground - but this should not
   be relied upon.) To ensure that the goals are eventually executed, the 
   caller should arrange for wake/0 to be called at some appropriate point 
   in the subsequent execution.  Please see the \"Advanced Control Features\" 
   section of the User Manual for more information about woken goal 
   management.
</P>
")

]).

%---------------------------------------------------------------------

:- comment(impose_max/2, [
    amode: impose_max(?, ++),
    args: [
    	"Var":   "(Domain) variable or integer",
	"Bound": "Upper bound (integer)"
    ],
    summary: "Update (if required) the upper bound of Var.",
    see_also: [impose_min/2, impose_bounds/3, impose_domain/2, exclude/2, exclude_range/3],
    kind:[dom],
    desc: html("<P>
   This predicate is provided mainly for compatibility with IC solver.
   If you intend to impose the same maximum on multiple variables, it
   is more efficient to use the lib(gfd) specific gfd_vars_impose_max/2.
</P><P>
   Primitive for updating the upper bound of Var so that it is at most
   Bound.  A bound update on a variable may fail (when the update empties
   the domain), succeed (possibly updating the variable's bounds), or
   instantiate the variable (in the case where the domain gets restricted to
   a singleton value).  Note that if the variable's type is integer, its
   bounds will always be adjusted to integral values.</P><P>
</P><P>
   Note that this predicate is intended for use only in implementing
   co-operation with other solvers and constraint propagators at the
   ECLiPSe level, and should not be called from ordinary user code.
   It differs from the usual constraint in that the propagation is not
   performed immediately. Normally, propagation in gecode is performed by
   a suspended goal that is scheduled and woken after each constraint is
   posted.  However, impose_max/2 does not call the woken goal
   scheduler (wake/0), so the propagation goal may not be executed 
   immediately. (It is possible that under some circumstances the goal 
   will be executed, if wake/0 is called indirectly - one example would be
   by the unify handler if the variable becomes ground - but this should not
   be relied upon.) To ensure that the goals are eventually executed, the 
   caller should arrange for wake/0 to be called at some appropriate point 
   in the subsequent execution.  Please see the \"Advanced Control Features\" 
   section of the User Manual for more information about woken goal 
   management.</P>
")
]).


%---------------------------------------------------------------------

:- comment(impose_bounds/3, [
    amode: impose_bounds(?, ++, ++),
    args: [
    	"Var": "(Domain) variable or integer",
	"Lo":  "Lower bound (integer)",
	"Hi":  "Upper bound (integer)"
    ],
    summary: "Update (if required) the bounds of Var.",
    see_also: [impose_min/2, impose_max/2],
    kind:[dom],
    desc: html("<P>
   This predicate is provided mainly for compatibility with IC solver.
   If you intend to impose the same bounds on multiple variables, it
   is more efficient to use the lib(gfd) specific gfd_vars_impose_min/2
   (which additionally does not call wake).
</P><P>
   Primitive for updating the upper and lower bounds of Var, As with 
   impose_min/2 and impose_max/2, it is intended for use in implementing 
   co-operation with other solvers, and constraint propagators at the
   ECLiPSe level, and should not be called from ordinary user 
   code (use ::/2 instead).  Its semantics is essentially:
<PRE>
       impose_min(Var, Lo), impose_max(Var, Hi), wake.
</PRE>
</P>
")
]).


%---------------------------------------------------------------------

:- comment(impose_domain/2, [
    amode: impose_domain(?, ?),
    args: [
    	"Var":   "(Domain) variable or integer",
	"DomVar": "(Domain) variable or integer"
    ],
    summary: "Restrict (if required) the domain of Var to the domain of DomVar.",
    see_also: [impose_min/2, impose_max/2, impose_bounds/3, exclude/2, exclude_range/3],
    kind:[dom],
    desc: html("<P>
   This predicate is provided mainly for compatibility with IC solver.
   If you intend to impose the same domain on multiple variables, it
   is more efficient to use the lib(gfd) specific gfd_vars_impose_domain/2,
   which additionally offer more flexibility in allowing the domain to be
   specified as a list.
</P><P>
   Primitive for restricting the domain of Var to the domain of DomVar.
   Any values in the domain of Var, which are not also in the domain of
   DomVar, are removed.  DomVar remains unaffected.  
   The domain update on Var may fail (when the update empties the domain),
   succeed (possibly updating the variable's domain), or instantiate the
   variable (in the case where the domain gets restricted to a singleton
   value).</P><P>

   Note that this predicate is intended for use only in implementing
   constraint propagators, and should not be called from ordinary user code.
   The waking behaviour is the same as discussed for impose_min/2 and
   impose_max/2.  Apart from this, the effect is similar to unifying
   Var with a copy of DomVar.
</P>
"),
    eg: "\
    ?- X::1..9, Y::5..7, impose_domain(X, Y).
    X = X{5 .. 7}
    Y = Y{5 .. 7}


    ?- X::1..9, impose_domain(X, 7).
    X = 7


    ?- X::1..3, Y::5..7, impose_domain(X, Y).
    No (0.00s cpu)

    ?- Y::1..5, impose_domain(3, Y).
    Y = Y{1 .. 5}

    ?- Y::1..5, impose_domain(6, Y).
    No (0.00s cpu)


    ?- Y::1..5, impose_domain(X, Y).
    Y = Y{1 .. 5}
    X = X{1 .. 5}

"
]).

%---------------------------------------------------------------------

:- comment(exclude/2, [
    amode: exclude(?, ++),
    args: [
    	"Var":  "(Domain) variable or integer",
	"Excl": "Integer value to exclude"
    ],
    summary: "Exclude the element Excl from the domain of Var.",
    see_also: [exclude_range/3, impose_min/2, impose_max/2, impose_domain/2],
    kind:[dom],
    desc: html("<P>
   This predicate is mainly provided for compatibility with IC solver.
   If you want to remove multiple values from the domain of one or 
   more variables, it is more efficient to use the lib(gfd) specific 
   gfd_vars_exclude/2.
</P><P>
   Primitive for excluding an element from the domain of a variable.
   The call may fail (when Var is the same integer as Excl),
   succeed (possibly updating the variable's domain), or instantiate the
   variable (when Excl was one of only two domain elements left).</P><P>
</P><P>
   Note that this predicate is intended for use only in implementing
   co-operation with other solvers and constraint propagators at the
   ECLiPSe level, and should not be called from ordinary user code.
   It differs from the usual constraint in that the propagation is not
   performed immediately. Normally, propagation in gecode is performed by
   a suspended goal that is scheduled and woken after each constraint is
   posted.  However, exclude/2 does not call the woken goal
   scheduler (wake/0), so the propagation goal may not be executed 
   immediately. (It is possible that under some circumstances the goal 
   will be executed, if wake/0 is called indirectly - one example would be
   by the unify handler if the variable becomes ground - but this should not
   be relied upon.) To ensure that the goals are eventually executed, the 
   caller should arrange for wake/0 to be called at some appropriate point 
   in the subsequent execution.  Please see the \"Advanced Control Features\" 
   section of the User Manual for more information about woken goal 
   management.
</P>
")
]).


%---------------------------------------------------------------------

:- comment(exclude_range/3, [
    amode: exclude_range(?, ++, ++),
    args: [
    	"Var": "Existing domain variable or integer",
	"Lo":  "Integer lower bound of range to exclude",
	"Hi":  "Integer upper bound of range to exclude"
    ],
    summary: "Exclude the elements Lo..Hi from the domain of Var.",
    see_also: [exclude/2, impose_min/2, impose_max/2],
    kind:[dom],
    desc: html("<P>
   This predicate is mainly provided for compatibility with IC solver.
   If you want to exclude the same range of values from multiple variables,
   it is more efficient to use the lib(gfd) specific gfd_vars_exclude_range/3.
</P><P>
   Primitive for excluding the integers between Lo and Hi (inclusive) from
   the domain of an integer variable.  The call may fail (when the domain of
   Var has no elements outside the range Lo..Hi), succeed (possibly updating
   the variable's domain), or instantiate the variable (in the case where
   the domain gets restricted to a singleton value).</P><P>
</P><P>
   Note that this predicate is intended for use only in implementing
   co-operation with other solvers and constraint propagators at the
   ECLiPSe level, and should not be called from ordinary user code.
   It differs from the usual constraint in that the propagation is not
   performed immediately. Normally, propagation in gecode is performed by
   a suspended goal that is scheduled and woken after each constraint is
   posted.  However, exclude_range/3 does not call the woken goal
   scheduler (wake/0), so the propagation goal may not be executed 
   immediately. (It is possible that under some circumstances the goal 
   will be executed, if wake/0 is called indirectly - one example would be
   by the unify handler if the variable becomes ground - but this should not
   be relied upon.) To ensure that the goals are eventually executed, the 
   caller should arrange for wake/0 to be called at some appropriate point 
   in the subsequent execution.  Please see the \"Advanced Control Features\" 
   section of the User Manual for more information about woken goal 
   management.
</P>
")
]).

%---------------------------------------------------------------------

:- comment(gfd_vars_impose_domain/2, [
    amode: gfd_vars_impose_domain(+, ?),
    amode: gfd_vars_impose_domain(+, ++),
    args: [
    	"Vars":   "Collection of (domain) variables or integers",
	"Domain": "List of domain specs or existing domain Variable or integer"
    ],
    summary: "Restrict (if required) the domain of Var to the domain"
             " specified  in Domain",
    see_also: [gfd_vars_impose_min/2, gfd_vars_impose_max/2,
               gfd_vars_impose_bounds/3, 
               gfd_vars_exclude/2, gfd_vars_exclude_range/3, 
               gfd_vars_exclude_domain/2],
    kind:[dom],
    desc: html("<P>
</P><P>
   Primitive for restricting the domains of Vars to the domain
   specified by  Domain. 
</P><P>
   Domain can be a list (note only a list is supported in this
   primitive, and not a collection) of domain specifications, a la the
   domain specifications of (::)/2 -- i.e. each element can be an
   integer, or an integer range in the form <pre>Lo..Hi</pre>. Alternatively,
   Domain can be an existing GFD domain variable, or an integer. 
</P><P>
   The domain update on Vars may fail (when the update empties the
   domain of any variables in Vars), succeed (possibly updating the 
   variables' domain), or instantiate the variables (in the case where the 
   domain gets restricted to a singleton value).</P><P>

   Note that this predicate is intended for use only in implementing
   constraint propagators, and should not be called from ordinary user code.
   It differs from the usual constraint in that the propagation is not
   performed immediately. Normally, propagation in gecode is performed by
   a suspended goal that is scheduled and woken after each constraint is
   posted.  However, this predicate does not call the woken goal
   scheduler (wake/0), so the propagation goal may not be executed 
   immediately. (It is possible that under some circumstances the goal 
   will be executed, if wake/0 is called indirectly - one example would be
   by the unify handler if the variable becomes ground - but this should not
   be relied upon.) To ensure that the goals are eventually executed, the 
   caller should arrange for wake/0 to be called at some appropriate point 
   in the subsequent execution.  Please see the \"Advanced Control Features\" 
   section of the User Manual for more information about woken goal 
   management.</P><P>

   This predicate is specific to lib(gfd).
")
]).

%---------------------------------------------------------------------

:- comment(gfd_vars_impose_min/2, [
    amode: gfd_vars_impose_min(+, ++),
    args: [
    	"Vars":   "Collection of (domain) variables or integers",
	"Bound": "Lower bound (integer)"
    ],
    summary: "Update (if required) the lower bounds of Vars.",
    kind:[dom],
    desc: html("<P>
   Primitive for updating the upper bound of Vars so that they are at most
   Bound.  A bound update on a variable in Vars may fail (when the
   update empties the domain), succeed (possibly updating the variable's 
   bounds), or instantiate the variable (in the case where the domain gets 
   restricted to a singleton value).</P><P>

   Note that this predicate is intended for use only in implementing
   co-operation with other solvers and constraint propagators at the
   ECLiPSe level, and should not be called from ordinary user code.
   It differs from the usual constraint in that the propagation is not
   performed immediately. Normally, propagation in gecode is performed by
   a suspended goal that is scheduled and woken after each constraint is
   posted.  However, this predicate does not call the woken goal
   scheduler (wake/0), so the propagation goal may not be executed 
   immediately. (It is possible that under some circumstances the goal 
   will be executed, if wake/0 is called indirectly - one example would be
   by the unify handler if the variable becomes ground - but this should not
   be relied upon.) To ensure that the goals are eventually executed, the 
   caller should arrange for wake/0 to be called at some appropriate point 
   in the subsequent execution.  Please see the \"Advanced Control Features\" 
   section of the User Manual for more information about woken goal 
   management.
</P><P>
   This predicate is specific to lib(gfd), and is more efficient than
   using multiple impose_min/2 for updating each variable in Vars,
   both because this is implementing as one rather than multiple
   events, and the Gecode rel() constraint for multiple IntVars is
   used to implement this primitive.
")

]).

%---------------------------------------------------------------------

:- comment(gfd_vars_impose_max/2, [
    amode: gfd_vars_impose_max(+, ++),
    args: [
    	"Vars":   "Collection of (domain) variables or integers",
	"Bound": "Upper bound (integer)"
    ],
    summary: "Update (if required) the upper bounds of Vars.",
    kind:[dom],
    desc: html("<P>
   Primitive for updating the lower bound of Vars so that they are at least
   Bound.  A bound update on a variable may fail (when the update empties
   the domain), succeed (possibly updating the variable's bounds), or
   instantiate the variable (in the case where the domain gets restricted to
   a singleton value).</P><P>
</P><P>
   Note that this predicate is intended for use only in implementing
   co-operation with other solvers and constraint propagators at the
   ECLiPSe level, and should not be called from ordinary user code.
   It differs from the usual constraint in that the propagation is not
   performed immediately. Normally, propagation in gecode is performed by
   a suspended goal that is scheduled and woken after each constraint is
   posted.  However, this predicate does not call the woken goal
   scheduler (wake/0), so the propagation goal may not be executed 
   immediately. (It is possible that under some circumstances the goal 
   will be executed, if wake/0 is called indirectly - one example would be
   by the unify handler if the variable becomes ground - but this should not
   be relied upon.) To ensure that the goals are eventually executed, the 
   caller should arrange for wake/0 to be called at some appropriate point 
   in the subsequent execution.  Please see the \"Advanced Control Features\" 
   section of the User Manual for more information about woken goal 
   management.
</P><P>
   This predicate is specific to lib(gfd), and is more efficient than
   using multiple impose_max/2 for updating each variable in Vars,
   both because this is implementing as one rather than multiple
   events, and the Gecode rel() constraint for multiple IntVars is
   used to implement this primitive.
")

]).

%---------------------------------------------------------------------

:- comment(gfd_vars_impose_bounds/3, [
    amode: gfd_vars_impose_bounds(+, ++, ++),
    args: [
    	"Vars":   "Collection of (domain) variables or integers",
	"Lo": "Lower bound (integer)",
	"Hi": "Upper bound (integer)"
    ],
    summary: "Update (if required) the bounds of Vars.",
    kind:[dom],
    desc: html("<P>
   Primitive for updating the bounds of Vars so that the Lower bounds
   are at least Lo and the upper bounds are at most Hi.
   A bound update on a variable may fail (when the update empties
   the domain), succeed (possibly updating the variable's bounds), or
   instantiate the variable (in the case where the domain gets restricted to
   a singleton value).</P><P>
</P><P>
   Note that this predicate is intended for use only in implementing
   co-operation with other solvers and constraint propagators at the
   ECLiPSe level, and should not be called from ordinary user code.
   It differs from the usual constraint in that the propagation is not
   performed immediately. Normally, propagation in gecode is performed by
   a suspended goal that is scheduled and woken after each constraint is
   posted.  However, this predicate does not call the woken goal
   scheduler (wake/0), so the propagation goal may not be executed 
   immediately. (It is possible that under some circumstances the goal 
   will be executed, if wake/0 is called indirectly - one example would be
   by the unify handler if the variable becomes ground - but this should not
   be relied upon.) To ensure that the goals are eventually executed, the 
   caller should arrange for wake/0 to be called at some appropriate point 
   in the subsequent execution.  Please see the \"Advanced Control Features\" 
   section of the User Manual for more information about woken goal 
   management.
</P><P>
   This predicate is specific to lib(gfd), and is more efficient than
   using multiple impose_max/2 and impose_min/2 for updating each 
   variable in Vars. Also, unlike impose_bounds/2, this primitive does
   not call wake, so it behaviour is consistent with the other gfd_vars
   primitives.
")

]).

%---------------------------------------------------------------------

:- comment(gfd_vars_exclude/2, [
    amode: gfd_vars_exclude(+, ++),
    args: [
    	"Vars":  "Collection of existing domain variable or integer",
	"Excl": "Integer value to exclude"
    ],
    summary: "Exclude the element Excl from the domains of Vars.",
    kind:[dom],
    desc: html("<P>
   Primitive for excluding an element from the domains of variables in Vars.
</P><P>
   Note that this predicate is intended for use only in implementing
   co-operation with other solvers and constraint propagators at the
   ECLiPSe level, and should not be called from ordinary user code.
   It differs from the usual constraint in that the propagation is not
   performed immediately. Normally, propagation in gecode is performed by
   a suspended goal that is scheduled and woken after each constraint is
   posted.  However, this predicate does not call the woken goal
   scheduler (wake/0), so the propagation goal may not be executed 
   immediately. (It is possible that under some circumstances the goal 
   will be executed, if wake/0 is called indirectly - one example would be
   by the unify handler if the variable becomes ground - but this should not
   be relied upon.) To ensure that the goals are eventually executed, the 
   caller should arrange for wake/0 to be called at some appropriate point 
   in the subsequent execution.  Please see the \"Advanced Control Features\" 
   section of the User Manual for more information about woken goal 
   management.</P>
</P><P>
   This predicate is specific to lib(gfd), and is more efficient than
   using multiple exclude/2 for updating each variable in Vars,
   because this is implementing as one rather than multiple
   events.
")
]).

%---------------------------------------------------------------------

:- comment(gfd_vars_exclude_range/3, [
    amode: gfd_vars_exclude_range(+, ++, ++),
    args: [
    	"Vars": "Collection of existing domain variable or integer",
	"Lo":  "Integer lower bound of range to exclude",
	"Hi":  "Integer upper bound of range to exclude"
    ],
    summary: "Exclude the elements Lo..Hi from the domains of Vars.",
    kind:[dom],
    desc: html("<P>
   Primitive for excluding the integers between Lo and Hi (inclusive) from
   the domains of variables in Vars. 
</P><P>
   Note that this predicate is intended for use only in implementing
   co-operation with other solvers and constraint propagators at the
   ECLiPSe level, and should not be called from ordinary user code.
   It differs from the usual constraint in that the propagation is not
   performed immediately. Normally, propagation in gecode is performed by
   a suspended goal that is scheduled and woken after each constraint is
   posted.  However, this predicate does not call the woken goal
   scheduler (wake/0), so the propagation goal may not be executed 
   immediately. (It is possible that under some circumstances the goal 
   will be executed, if wake/0 is called indirectly - one example would be
   by the unify handler if the variable becomes ground - but this should not
   be relied upon.) To ensure that the goals are eventually executed, the 
   caller should arrange for wake/0 to be called at some appropriate point 
   in the subsequent execution.  Please see the \"Advanced Control Features\" 
   section of the User Manual for more information about woken goal 
   management.</P>
</P><P>
   This predicate is specific to lib(gfd), and is more efficient than
   using multiple exclude_range/2 for updating each variable in Vars,
   because this is implementing as one rather than multiple
   events.
")
]).

%---------------------------------------------------------------------

:- comment(gfd_vars_exclude_domain/2, [
    amode: gfd_vars_exclude_domain(+, ?),
    amode: gfd_vars_exclude_domain(+, ++),
    args: [
    	"Vars":   "Collection of existing domain variables or integers",
	"Domain": "Domain specs or existing domain variable or integer"
    ],
    summary: "Exclude the values specified in Domain from the domains"
             " of Vars.",
    kind:[dom],
    desc: html("
</P><P>
   Primitive for excluding the values specified in Domain from the
   domains of Vars.
</P><P>
   Domain can be a list (note only a list is supported in this
   primitive, and not a collection) of domain specifications, a la the
   domain specifications of (::)/2 -- i.e. each element can be an
   integer, or an integer range in the form <pre>Lo..Hi</pre>. Alternatively,
   Domain can be an existing GFD domain variable, or an integer. 
</P><P>
   Note that this predicate is intended for use only in implementing
   co-operation with other solvers and constraint propagators at the
   ECLiPSe level, and should not be called from ordinary user code.
   It differs from the usual constraint in that the propagation is not
   performed immediately. Normally, propagation in gecode is performed by
   a suspended goal that is scheduled and woken after each constraint is
   posted.  However, this predicate does not call the woken goal
   scheduler (wake/0), so the propagation goal may not be executed 
   immediately. (It is possible that under some circumstances the goal 
   will be executed, if wake/0 is called indirectly - one example would be
   by the unify handler if the variable becomes ground - but this should not
   be relied upon.) To ensure that the goals are eventually executed, the 
   caller should arrange for wake/0 to be called at some appropriate point 
   in the subsequent execution.  Please see the \"Advanced Control Features\" 
   section of the User Manual for more information about woken goal 
   management.</P>
</P><P>
   This predicate is specific to lib(gfd). For Gecode, if multiple
   values need to   be excluded from the domain of a variable, it is much more
   efficient to do it in one go, rather than excluding each value 
   individually (e.g. using gfd_vars_exclude/2). Also, if the values
   has to be excluded from multiple variables, it is more efficient to use
   one primitive to do this, rather than exclude the values from each 
   variable individually, as this is done with one event.

")]).

%---------------------------------------------------------------------

:- comment(get_weighted_degree_decay/1, [
     amode: get_weighted_degree_decay(-),
     args: ["Decay": "(Domain) variable"],
     summary: "Return the current decay rate for weighted degree.",
     desc: html("<P>
   Return the current decay rate for weighted degree. The decay rate
   governs how fast the weighted degree decays -- each time a failure
   occurs during Gecode's constraint propagation, the constraint that
   failed will have 1 added to its failure count, while
   all other constraints' failure count is updated by multiplying its
   old value by the decay rate. The decay rate is a floating point value
   between 1 (no decay) and 0 (decay to 0 immediately). The weighted
   degree for a domain variable is the sum of all the failure
   counts of the constraints that the variable occurs in. 
")]).

:- comment(set_weighted_degree_decay/1, [
     amode: set_weighted_degree_decay(+),
     args: ["Decay": "Number between 0 and 1."],
     summary: "Change the current decay rate for weighted degree to Decay.",
     desc: html("<P>
   Change the current decay rate for weighted degree to Decay. The decay rate
   governs how fast the weighted degree decays -- each time a failure
   occurs during Gecode's constraint propagation, the constraint that
   failed will have 1 added to its failure count, while all
   other constraints' failure count is updated by multiplying its
   old value by the decay rate. The decay rate is a floating point value
   between 1 (no decay) and 0 (decay to 0 immediately). The weighted
   degree for a domain variable is the sum of all the failure
   counts of the constraints that the variable occurs in. 
")]).

:- comment(init_weighted_degree/1, [
     amode: init_weighted_degree(+),
     args: ["Factor": "Non-negative Number."],
     summary: "Initialise weighted degree so that the initial value"
              " for a variable is Factor*degree..",
     desc: html("<P>
   Initialise weighted degree by setting the initial value for 
   each constraint's failure count to Factor. The weighted degree for
   a variable is the sum of all its constraints failure count, i.e.
   Factor*(degree for variable).By default, the initial failure count
   for each constraint is set to 1.0, so the initial weighted
   degree for a domain variable is its degree (number of associated
   constraints).
</p><p>
   As with failure count for a constraint is automatically set, 
   init_weighted_degree/1 is only needed if the user want to erase the 
   accumulated failure counts and reset the weighted degree for 
   variables, and/or if a different value for Factor is wanted.
</p><p>
   Changing Factor changes the relative importance of subsequent failures,
   The smaller the Factor, the more important each failure will be in the 
   subsequent changes in the weighted degree. Note that unless Factor
   is  set to 0, the initial  relative ordering of weighted degree
   will not change (subject to floating point precision considerations).
")]).

%---------------------------------------------------------------------

:- comment(solver_constraints_number/1, [
     amode: solver_constraints_number(-),
     args: ["NumberOfConstraints": "Free variable"],
     summary: "Returns the number of constraints in the gecode solver state",
     see_also: [solver_vars_number/1],
     desc: html("<P>\
  Returns the current total number of active constraints (propagators)
  in the gecode solver state.</P>")
]).

%---------------------------------------------------------------------

:- comment(solver_vars_number/1, [
     amode: solver_vars_number(-),
     args: ["NumberOfVariables": "Free variable"],
     summary: "Returns the number of domain variables in the gecode solver state",
     see_also: [solver_constraints_number/1],
     desc: html("<P>\
  Returns the current total number of domain variables in the gecode solver
  state. This is for the variables that are created at the ECLiPSe
  level by the program or by GFD.
</P>")
]).

%---------------------------------------------------------------------

:- comment(gfd_maxint/1, [
     amode: gfd_maxint(-),
     args: ["Var": "Free variable"],
     summary: "Returns the maximum value allowed in gecode's domain.",
     see_also: [gfd_minint/1],
     desc: html("<P>\
   Returns the maximum value allowed in gecode's domain. It is strongly
   recommended that the user values used in the domain to not approach
   this value, because propagation can easily lead to values outside
   what gecode can support.")]
 ).


%---------------------------------------------------------------------

:- comment(gfd_minint/1, [
     amode: gfd_minint(-),
     args: ["Var": "Free variable"],
     summary: "Returns the minimum value allowed in gecode's domain.",
     see_also: [gfd_minint/1],
     desc: html("<P>\
   Returns the minimum value allowed in gecode's domain. It is strongly
   recommended that the user values used in the domain to not approach
   this value, because propagation can easily lead to values outside
   what gecode can support.")]
 ).

%---------------------------------------------------------------------

:- comment(gfd_set_default/2, [
     amode: gfd_set_default(+,+),
     args: ["Parameter": "GFD parameter to set (atom).",
            "DefaultValue": "Default value for Parameter."
           ],
     summary: "Set the default value for GFD Parameter.",
     see_also: [gfd_get_default/2,gfd_minint/1,gfd_maxint/1],
     desc: html("<P>\
   Set the default value for parameters:
 <UL>
    <li><b>interval_min</b>
          Minimum for the default interval for domain variables. When a domain
          variable is created implicitly in a constraint, it is given a
          default interval, and this interval should be as small as possible.
          as the efficiency of various propagator depends on the domain
          size. (integer no smaller than gfd_minint).</li>
    <li><b>interval_max</b>
          Maximum for the default interval for domain variables.
	  (integer no larger than gfd_maxint).</li>
    <li><b>array_size</b>
          Initial size for the variable array for storing domain variables
          When more variables than can be accommodated in the array is required,
          a new array double the size is created, and the old variables copied
          to the new. Changing the initial size can reduce or avoid this 
          copying overhead. (positive integer).</li>
    <li><b>cloning_distance</b>
          This controls how often the gecode state is cloned. The smaller
          the distance, the more frequent the cloning. Cloning is only done
          at places where the new clone might be useful, roughly if there are
          changes to the state since the last clone, and it is possible to 
          backtrack and make use of the new clone (i.e. there should be
          at least one choice-point between the last clone and the current
          one. Distance is a measure of such points, so a distance of 1 is 
          the minimal distance where a clone may be needed. (positive 
          integer).</li>
    <li><b>events_max</b>
          Maximum number of events during non-search (deterministic) 
          computation before a new clone is created to replace the
          parent (positive integer).</li>
</ul>")]
).


%---------------------------------------------------------------------

:- comment(gfd_get_default/2, [
     amode: gfd_get_default(+,-),
     args: ["Parameter": "GFD parameter (atom).",
            "DefaultValue": "Current default value for Parameter."
           ],
     summary: "Get the current default value for GFD Parameter.",
     see_also: [gfd_set_default/2],
     desc: html("<P>\
   Get the default value for parameters:
 <UL>
    <li><b>interval_min</b>
          Minimum for the default interval for domain variables. 
          Initial value: -1000000.</li>
    <li><b>interval_max</b>
          Maximum for the default interval for domain variables. 
          Initial value: 1000000.</li>
    <li><b>array_size</b>
          Initial size for the variable array for storing domain variables
          Initial value: 100.</li>
    <li><b>cloning_distance</b>
          This controls how often the gecode state is cloned. The smaller
          the distance, the more frequent the cloning. Initial value: 2.</li>
</ul>")]
).


%---------------------------------------------------------------------

:- comment(msg/3, [
amode: msg(-,-,?), amode:msg(++,++,?),
args:  ["Var1": "A (domain) variable or number",
	"Var2": "A (domain) variable or number",
	"MSG": "Most specific generalisation (variable)"
       ],
summary: "Computes the most specific generalisation of Var1 and Var2 that is expressible with GFD variables.",
desc: html("\
<P>
   The most specific generalisation of two intervals is computed and
   returned as MSG.  MSG will be the union of the domains of the two
   variables, with integers treated as a singleton domain.
   If either Var1 or Var2 are domain-less, or have values that cannot be
   expressed as domains, MSG remains unbound.
</P>")
]).

%---------------------------------------------------------------------
:- comment(gfd_var_print/2, hidden).
:- comment(gfd_handle_tr_out/2, hidden).
:- comment(gfd_copy_var/2, hidden).

%---------------------------------------------------------------------
%
% Named struct documentation
%
%---------------------------------------------------------------------

:- comment(struct(gfd_stats), [
        summary: "Structure for obtaining statistics or providing stopping"
                 " limits for gecode search-engines",
/*        
        amode: gfd_stats(-,?,?,-,?),
        amode: gfd_stats(-,-,-,-,-),
*/
        desc: "\
  This structure is used in search/6 predicate, which interface to gecode's
  search-engines. The structure can be used to obtain statistics of the
  search via the stats option, in this case the fields of the structure 
  should be uninstantiated, and search/6 will instantiate it when a solution
  is returned. Secondly, the struct can be used in the limits option, to
  specify limits for the search, such that the search will be terminated when 
  the specified limit is exceeded. In this case, the fields for which limits 
  are required should be set. Note that not all fields can be used as limits.
  If the field cannot be used as a limit, it will be ignored.",
        fields: [
            "prop" : "Number of propagations performed. (stats only)",
            "fail":  "Number of failed nodes.",
            "nodes": "Number of nodes expanded.",
            "depth": "Maximum depth of search stack. (stats only)",
            "mem": "peak memory usage (in bytes) by gecode." 
                ]
         ]).

:- comment(struct(gfd_control), [
        summary: "Structure for passing low-level control parameters to gecode"
                 " search-engines.",
          desc: "\
  This structure is used in search/6 predicate, which interface to gecode's
  search-engines. The structure is used by the control option to pass values
  for low-level parameters that control the behaviour gecode search-engine.
  See the gecode documentation for more details explanation of the
  parameters. For threads, if >= 1, this specifies number of threads
  to use in search, but for < 1, this specify the number of threads in
  relation to the number of processors on the machine, see the gecode
  documentation for more detail. ",
        fields: [
            "commit_distance": "the commit recomputation distance (integer)"
                               "(member c_d of Gecode::Search::Options)",
            
            "adaptive_distance": "the adaptive recomputation distance (integer)"
                               "(member a_d of Gecode::Search::Options)",
            "threads": "number of threads to use in search (integer or float)"
                               "(member threads of Gecode::Search::Options)"
        ]
  ]).


:- comment(struct(gcc), [
        summary: "Bounds specification for gcc constraint.",
          desc: html("\
    This structure is used to specify the cardinality (number of occurrences)
    of one value for the gcc constraint."),
        fields: ["low": "Lower bound on the cardinality of Value (integer).",
                 "high": "Upper bound on the cardinality of Value (integer).",
                 "value": "Value whose cardinality is being specified."
        ]
   ]).


:- comment(struct(occ), [
        summary: "Bounds specification for gcc constraint.",
          desc: html("\
    This structure is used to specify the cardinality (number of occurrences)
    of one value for the gcc constraint."),
        fields: ["occ": "Domain variable or integer specifying the cardinality of Value.",
                 "value": "Value whose cardinality is being specified."
        ]
   ]).

:- comment(struct(rect), [
        summary: "Specification for rectangles used in disjoint2 and disjoint2_optional constraints.",
        desc: "This structure is used for specify the rectangles used
 in disjoint2 and disjoint2_optional constraints. These rectangles are
 placed on a grid.",
        fields: ["x": "the x co-ordinate of the left-side of the rectangle",
                 "y": "the y co-ordinate of the bottom-side of the rectangle",
                 "w": "the width of the rectangle",
                 "h": "the height of the rectangle",
                 "b": "boolean specifying if rectangle is placed (1=placed)"
                ]
                         ]).

:- comment(struct(trans), [
        summary: "Specification of s transition in the DFA for"
                 " extensional/4.",
        fields: ["f": "From state",
                 "t": "To state",
                 "l": "label"],
        desc: html("\
<P>
This is used to specify a transition in the DFA for the extensional constraint
extensional/4. The transition trans{f:F,t:T,l:L} is a transition from state
F to state T, and labelling the variable with the value L.</P>")
]).


:- comment(struct(gfd), [
        summary: "Attribute for gfd domain variable (for"
                 " implementation use only)",
        fields: [
            idx:"index for variable in array",	
            bool:"index for linked boolean in C++, if any",
            prob:"pointer to problem handle",
            any: "any suspension list",
            set: "" 	% use for marking variable as already set in Gecode
                ],
        desc:html("This is the structure used for gfd's attribute, and is"
             " part of the implementation of gfd.")
]).

:- comment(struct(gfd_prob), [
        summary: "ECLiPSe level problem handle (for implementation use"
                 " only)",
        fields:[
             cp_stamp: "",		% for time-stamp
             nvars: "current number of problem variables",
             nevents:"number of events in events queue",
             vars: "problem variable array",
             prop:"suspension for performing propagation in Gecode",
             last_anc:"ancestor to clone from",
             space: "handle for Gecode space",
             events:"events queue for recomputation",
             events_tail:""	% new events added here
               ],
        desc: html("This is the structure used to represent the Gecode"
              " computation state at the ECLiPSe level, and is part of"
              " the implementation of gfd.")
]).

:- comment(struct(gfd_space), [
        summary: "Handle for Gecode solver state (for implementation"
                 " use only)",
        fields:[
                   handle: "low level handle to current Gecode space"
                           " in C++", 
                   stamp:""   
               ],
        desc: html("The ECLiPSe level handle for the Gecode's solver"
                   " state, and is part of the implementation of gfd")

]).
