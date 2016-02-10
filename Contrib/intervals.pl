:- module(intervals).			% SEPIA header

:- export
	intervals_are_disjoint/2,
	intervals_meet/2,
	interval_union/3,
	interval_intersection/3,
	interval_difference/3,
	interval_is_less_and_not_coalescable/2,
	interval_is_less/2,
	intervals_are_not_coalescable/2,
	intervals_are_coalescable/2,
	interval_ends_first/2,
	interval_includes/2,
	interval_contains/2,
	iset_union/3,
	iset_intersection/3,
	iset_difference/3,
	iset_complement/3,
	canonical_iset/2,
	iset_includes/2,
	iset_contains/2.


/*  INTERVALS.PL
    Shelved on the 21st of December 1987
    Updated on the 20th of February 1988 to fix 'canonical_iset', which
    previously did not coalesce all adjacent intervals.
*/


/*
This file defines predicates for set algebra on sets of integers.

Each set is to be thought of  as the union of disjoint intervals, and is
represented as a list, each element of which is one such interval.

The intervals are represented as
    L -- U
where L and U are integers, and L  must be less than or equal to U. Note
that 10--9 (corresponding  to the null set) is not a valid interval, and
the predicates will not cope with it.

I  call the  sets of  intervals I-sets.  Their representation  has these
properties:

(1) [] is an I-set.

(2) [ I ] is an I-set, for any interval I.

(3) [ I1, I2, ... In ] is an I-set if:

    (3.1)  All Ii, Ij are disjoint.

    (3.2)  For any  pair of adjacent intervals,  Ii and Ii+1,  the upper
           bound of I1 is less than the lower bound of I2. This ordering
           makes the I-sets more efficient to process.

    (3.3)  For any  pair of adjacent intervals,  Ii and Ii+1,  the upper
           bound of I1  is at least 2  less than the lower  bound of I2.
           This  enforces a  unique  representation, prohibiting  I-sets
           like
               [ 1--9, 10--12 ] and [ 1--3, 4--12 ]
           being equivalent to
               [ 1--12 ]


Proofs of correctness:

I'll summarise these for iset_union, iset_intersection, iset_difference,
and iset_complement, since those predicates are rather tricky.

We can divide each proof into five parts:
(1) Proof that the predicates terminate on correct arguments.
(2) Proof that  the result is algebraically correct (even  though it may
    not be a valid I-set).
(3) Proof that the result's intervals are disjoint.
(4) Proof that the result's intervals are ordered in ascending order.
(5) Proof that  the result  contains no  adjacent pairs  like 1--2,3--4
    which should be coalesced.

The last  three correspond  to proving  that the  result is  a canonical
I-set.

(1) Proof of  termination.

 This is easy. Sets  are represented as lists. We can  show that on each
 recursive call, the sets grow smaller - either because a whole interval
 is removed from a list, or because it is reduced in size.

(2) Proof of algebraic correctness.

 For 'iset_union', this is trivial,  because the only operations used in
 the tail  of 'iset_union'  are set  unions. The  operation [H|T]  on an
 I-set  corresponds  to  H u T.   The  predicates  'interval_join',  and
 'union_overlap' also correspond  to union. All we need do  is show that
 the union denoted by  the head of each clause is  equal to that denoted
 by its tail.

 For 'iset_intersection', we use the distributive law:

  (A1 u A2 u A3...) /\ (B1 u B2 u B3... ) =
    (A1 /\ B1 ) u (A2 /\ B1 ) u (A1 /\ B2) u (A2 /\ B2) ...

 We can optimise by noting that Ai/\Bi = {} if Ai<Bi. Hence the use of
 'is_less'.

 We also use the ordering and disjointness of intervals within I-sets to
 show that if A1 /\ B1 is not empty, then either A1 /\ B2 is empty or A2
 /\ B1 is empty. Thus, after intersecting  A1 and B1, we can drop either
 A1 or B1 in the recursive call. This is what 'drop_lowest' does.

 For 'iset_difference', we use the equivalence

  A1 - B1 = A1 /\ -B1

 So
    (A1 u A2 ... ) - (B1 u B2 ...)
        =
    (A1 u A2 ... ) /\ -(B1 u B2 u B3...)
        = (by De Morgan)
    (A1 u A2 ... ) /\ -B1 /\ -B2 /\ B3 ...
        = (distributive)
    (A1 /\ -B1 /\ -B2...) u (A2 /\ -B1 /\ -B2...) u ...

 Since An-Bn =  An if An and Bn  are disjoint, we can ignore  all the Bi
 which don't overlap with An in each term, reducing to

    (A1 /\ overlapping Bi) u (A2 /\ overlapping Bi) u ...

 Most  of the  code in  'iset_difference' and  'iset_complement' is  for
 detecting  which  Bi overlaps  which  Ai,  and  passing along  bits  of
 intervals which overlap more than one other.

(3) Disjointness of intervals within I-sets.

 For 'iset_intersection', 'iset_complement',  and 'iset_difference' this
 is trivial.  If the intervals in  A are disjoint, then  intersecting or
 differencing can only take parts away, so preserving disjointness.

 For 'iset_union', if Ai  overlaps Bi, we must take care  not to add the
 overlap twice  to the  result. This is  what 'union_overlap'  does; its
 first argument is Ai u Bi; an interval which may need to have its upper
 bound raised if A(i+1) or B(i+1) overlaps it.

(4) Ordering of intervals within I-sets.

 This  is trivial,  as  long as  the inputs  are  properly ordered.  The
 operation [ H | T ] corresponds to uniting the interval H with I-set T.
 When new  I-sets are built,  lesser intervals  are always put  onto the
 head of a list which is either empty or contains greater intervals.

(5) Non-coalescability of intervals within I-sets.

 This follows  in the same  way as (3).  For 'iset_union', we  must take
 care to  coalesce intervals which come  from different sets,  and which
 are  disjoint   but  coalescable.   This  is  why   'iset_union'  calls
 'interval_is_less_and_not_coalescable'.
*/


% /*  UTILITIES  */
% 
% 
% /*  append( L1+, L2+, L3- ):
% 
%     L3 is the result of appending list L2 to list L1.
% */
% append( [], L, L ) :- !.
% append( [H|T], L, [H|T1] ) :-
%     append( T, L, T1 ), !.
% 
% 
% /*  max( A+, B+, M- ):
% 
%     M is the maximum of A and B.
% */
% max( A, B, A ) :- A > B, !.
% max( A, B, B ).
% 
% 
% /*  min( A+, B+, M- ):
% 
%     M is the minimum of A and B.
% */
% min( A, B, A ) :- A < B, !.
% min( A, B, B ).


/*  INTERVALS and I-SETS  */


/*  L--U is the interval [L,U] : i.e. that containing
    L and U as endpoints.
    L and U must be integers, not reals.
*/
:- op( 31, xfx, -- ).


/*  PUBLIC intervals_are_disjoint( I1+, I2+ ):

    Intervals I1 and I2 are disjoint - i.e. have no points in common.
*/
intervals_are_disjoint( _L1--U1, L2--_U2 ) :-
    U1 < L2, !.

intervals_are_disjoint( L1--_U1, _L2--U2 ) :-
    U2 < L1, !.


/*  PUBLIC intervals_meet( I1+, I2+ ):

    Intervals I1 and I2 are not disjoint - i.e. have at least one
    point in common.
*/
intervals_meet( I1, I2 ) :-
    not( intervals_are_disjoint(I1,I2) ).


/*  PUBLIC interval_union( I1+, I2+, I3- ):              

    I3 is that interval which is the union of intervals I1 and I2.
    Only defined if I1 and I2 are coalescable.
*/
interval_union( L1--U1, L2--U2, L3--U3 ) :-
    min( L1, L2, L3 ),
    max( U1, U2, U3 ).


/*  PUBLIC interval_intersection( I1+, I2+, I3- ):

    I3 is that interval which is the intersection of intervals I1 and I2.
    Only defined if I1 and I2 are not disjoint.
*/
interval_intersection( L1--U1, L2--U2, L3--U3 ) :-
    max( L1, L2, L3 ),
    min( U1, U2, U3 ).


/*  PUBLIC interval_difference( I1+, I2+, I3- ):

    I3 is that I-set which is the difference of intervals I1 and I2.
*/
interval_difference( I1, I2, [I1] ) :-
    intervals_are_disjoint( I1, I2 ), !.

interval_difference( L1--U1, L2--U2, [ L1--L2_less_1, U2_add_1--U1 ] ) :-
    L1 < L2, U1 > U2,
    U2_add_1 is U2 + 1,
    L2_less_1 is L2 - 1, !.

interval_difference( L1--U1, L2--U2, [ L1--L2_less_1 ] ) :-
    L1 < L2, U1 =< U2,
    L2_less_1 is L2 - 1, !.

interval_difference( L1--U1, L2--U2, [ U2_add_1--U1 ] ) :-
    L1 >= L2, U1 > U2,
    U2_add_1 is U2 + 1, !.

interval_difference( L1--U1, L2--U2, [] ) :-
    L1 >= L2, U1 =< U2, !.


/*  PUBLIC interval_is_less_and_not_coalescable( I1+, I2+ ):              

    Interval I1 is strictly less than I2; and I1 union I2 can not     
    be expressed as one interval.
    I.e. the upper bound of I1 is at least 2 less than the lower bound of I2.
*/
interval_is_less_and_not_coalescable( _L1--U1, L2--_U2 ) :-
    L2_less_1 is L2-1,
    U1 < L2_less_1, !.


/*  PUBLIC interval_is_less( I1+, I2+ ):              

    Interval I1 is strictly less than I2.
    I.e. there is no point P2 in I2 such that I1 contains a point
    greater than or equal to P2.

    |-----|  |----|
      I1       I2

    interval_is_less( I1, I2 ) implies disjoint( I1, I2 ).
*/
interval_is_less( _L1--U1, L2--_U2 ) :-
    U1 < L2, !.


/*  PUBLIC intervals_are_not_coalescable( I1+, I2+ ):

    True if I1 union I2 can not be expressed as one interval.
*/
intervals_are_not_coalescable( I1, I2 ) :-
    interval_is_less_and_not_coalescable( I1, I2 ), !.

intervals_are_not_coalescable( I1, I2 ) :-
    interval_is_less_and_not_coalescable( I2, I1 ), !.


/*  PUBLIC intervals_are_coalescable( I1+, I2+ ):

    True if I1 union I2 can be expressed as one interval.
*/
intervals_are_coalescable( I1, I2 ) :-
    not( intervals_are_not_coalescable( I1, I2 ) ).


/*  PUBLIC interval_ends_first( I1+, I2+ ):

    The upper bound of I1 is less than that of I2.
*/
interval_ends_first( _--U1, _--U2 ) :-
    U1 < U2, !.


/*  PUBLIC interval_includes( I1+, I2+ ):
    I1 is a superset of (or equal to) I2.
*/
interval_includes( L1--U1, L2--U2 ) :-
    L1 =< L2, U1 >= U2.


/*  PUBLIC interval_contains( I1+, P+ ):
    P is a member of I1.
*/
interval_contains( L1--U1, P ) :-
    L1 =< P, U1 >= P.


/*  PUBLIC iset_union( A+, B+, U- ):

    U is the union of A and B, where A, B, and U are I-sets.
*/
iset_union( A, [], A ) :- !.

iset_union( [], B, B ) :- !.

iset_union( [A1|A2_n], [B1|B2_n], [B1|URest] ) :-
    interval_is_less_and_not_coalescable( B1, A1 ),
    iset_union( [A1|A2_n], B2_n, URest ), !.

iset_union( [A1|A2_n], [B1|B2_n], [A1|URest] ) :-
    interval_is_less_and_not_coalescable( A1, B1 ),
    iset_union( A2_n, [B1|B2_n], URest ), !.

iset_union( [A1|A2_n], [B1|B2_n], U ) :-
    /*  A1 overlaps B1  */
    interval_union( A1, B1, U1 ),
    union_overlap( U1, A2_n, B2_n, U ).


union_overlap( U1, [A1|A2_n], B, URest ) :-
    intervals_are_coalescable( U1, A1 ),
    interval_union( U1, A1, J ),
    union_overlap( J, A2_n, B, URest ), !.

union_overlap( U1, A, [B1|B2_n], URest ) :-
    intervals_are_coalescable( U1, B1 ),
    interval_union( U1, B1, J ),
    union_overlap( J, A, B2_n, URest ), !.

union_overlap( U1, A, B, [U1|URest] ) :-
    iset_union( A, B, URest ), !.


/*  PUBLIC iset_intersection( A+, B+, I- ):

    U is the intersection of A and B, where A, B, and I are I-sets.
*/
iset_intersection( _A, [], [] ) :- !.

iset_intersection( [], _B, [] ) :- !.

iset_intersection( [A1|A2_n], [B1|B2_n], IRest ) :-
    interval_is_less( A1, B1 ),
    iset_intersection( A2_n, [B1|B2_n], IRest ), !.

iset_intersection( [A1|A2_n], [B1|B2_n], IRest ) :-
    interval_is_less( B1, A1 ),
    iset_intersection( [A1|A2_n], B2_n, IRest ), !.

iset_intersection( [A1|A2_n], [B1|B2_n], [I1|IRest] ) :-
    interval_intersection( A1, B1, I1 ),
    drop_lowest( [A1|A2_n], [B1|B2_n], A_Rest, B_Rest ),
    iset_intersection( A_Rest, B_Rest, IRest ).


drop_lowest( [I|A2_n], [I|B2_n], A2_n, B2_n ) :- !.

drop_lowest( [A1|A2_n], [B1|B2_n], A2_n, [B1|B2_n] ) :-
    interval_ends_first( A1, B1 ), !.

drop_lowest( A, [_B1|B2_n], A, B2_n ).


/*  PUBLIC iset_difference( A+, B+, D- ):

    U is the difference of A and B, where A, B, and D are I-sets.
*/
iset_difference( A, [], A ) :- !.

iset_difference( [], _B, [] ) :- !.

iset_difference( [A1|A2_n], [B1|B2_n], [A1|DRest] ) :-
    interval_is_less( A1, B1 ),
    iset_difference( A2_n, [B1|B2_n], DRest ), !.

iset_difference( [A1|A2_n], [B1|B2_n], DRest ) :-
    interval_is_less( B1, A1 ),
    iset_difference( [A1|A2_n], B2_n, DRest ), !.

iset_difference( [A1_low--A1_high|A2_n], [B1_low--B1_high|B2_n],
                 [ A1_low--B1_low_less_1 | D_Rest ]  ) :-
    A1_low < B1_low,
    !,
    B1_low_less_1 is B1_low - 1,
    iset_difference( [ B1_low--A1_high | A2_n ],
                     [ B1_low--B1_high | B2_n ],
                     D_Rest ), !.

iset_difference( [_A1_low--High|A2_n], [_B1_low--High|B2_n], D ) :-
    iset_difference( A2_n, B2_n, D ), !.

iset_difference( [_A1_low--A1_high|A2_n], [_B1_low--B1_high|B2_n], D ) :-
    A1_high > B1_high,
    !,
    B1_high_add_1 is B1_high + 1,
    iset_difference( [ B1_high_add_1--A1_high | A2_n ], B2_n, D ), !.

iset_difference( [_A1_low--A1_high|A2_n], [_B1_low--B1_high|B2_n], D ) :-
    A1_high < B1_high,
    !,
    A1_high_add_1 is A1_high + 1,
    iset_difference( A2_n, [ A1_high_add_1--B1_high | B2_n ], D ), !.


/*  PUBLIC iset_complement( I+, A+, C- ):
    C is the complement of I-set A in interval I.
*/
iset_complement( L--U, A, C ) :-
    iset_complement( L, U, A, C ).


iset_complement( L, U, _, [] ) :-
    L > U, !.

iset_complement( L, U, [], [L--U] ) :- !.

iset_complement( L, U, [ _A1_low--A1_high | A2_n ], C ) :-
    A1_high < L,
    !,
    /*  A1 disjoint from and less than I.  */
    iset_complement( L, U, A2_n, C ).

iset_complement( L, U, [ A1_low--_ | _ ], [L--U] ) :-
    A1_low > U,
    /*  A1 disjoint from and greater than I.  */
    !.

iset_complement( L, U, [ A1_low--A1_high | A2_n ], [ L -- A1_low_less_1 | C_Rest ] ) :-
    A1_low > L,
    !,
    /*  A1 greater than lower end of I.  */
    A1_low_less_1 is A1_low - 1,
    A1_high_add_1 is A1_high + 1,
    iset_complement( A1_high_add_1, U, A2_n, C_Rest ).

iset_complement( L, U, [ A1_low--A1_high | A2_n ], C_Rest ) :-
    A1_low =< L,
    !,
    /*  A1 overlaps lower end of I.  */
    A1_high_add_1 is A1_high + 1,
    iset_complement( A1_high_add_1, U, A2_n, C_Rest ).


/*  PUBLIC canonical_iset( S+, C- ):

    S is a list of intervals. They can be in any order, and
    need not be disjoint.

    C is the corresponding I-set.
*/
canonical_iset( [], [] ) :- !.

canonical_iset( [X|L], U ) :-
    canonical_iset( L, LC ),
    iset_union( [X], LC, U ).


/*  PUBLIC iset_includes( I1+, I2+ ):
    I1 is a superset of (or equal to) I2.
*/
iset_includes( _, [] ) :- !.

iset_includes( [A1|A2_n], [B1|B2_n] ) :-
    interval_is_less( A1, B1 ),
    !,
    iset_includes( A2_n, [B1|B2_n] ).

iset_includes( [A1|A2_n], [B1|B2_n] ) :-
    interval_includes( A1, B1 ),
    iset_includes( [A1|A2_n], B2_n ), !.


/*  PUBLIC iset_contains( I1+, P+ ):
    P is a member of I-set I1.
*/
iset_contains( [A1|_], P ) :-
    interval_contains( A1, P ), !.

iset_contains( [_|A2_n], P ) :-
    iset_contains( A2_n, P ), !.
