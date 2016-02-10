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
% Copyright (C) 1995 - 2009 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s):       Helmut Simonis, 4C, Univerity College Cork, Cork
%                       Kish Shen
%                       Joachim Schimpf, Coninfer Ltd, 2014
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------

:- export bin_packing/3,
          bin_packing/4.

:- comment(bin_packing/4, [
       amode: bin_packing(+,++,+,+),
       args: ["ItemBins": "A collection of N variables or integers (domain/value"
                       " between 1 and M)",
              "ItemSizes": "A collection of N non-negative integers",
              "M": "A non-negative Integer, the number of bins",
              "BinSize": "A non-negative integer"
             ],
       see_also:[bin_packing/3, cumulative/4],
       summary:"The one-dimensional bin packing constraint: packing N items"
               " into M bins",
       kind:[constraint:[root:[ic,fd]]],
       desc: html("\
   This constraint is for one-dimensional bin-packing, that is, to pack N
   items with individual sizes into M bins, such that the sum of sizes of 
   items in each bin does not exceed BinSize. Each element of ItemBins and its 
   corresponding element in ItemSizes represents an item, such that the i'th 
   element of ItemSizes is the size of the i'th item, and the i'th element in
   ItemBins is the bin this item is packed into. 
</P><P>
   This constraint can be seen as a special case of the cumulative/4
   constraint, where all task durations are equal to 1, each bin
   represents a time point, and BinSize corresponds to the Resource.
</P><P>
   This constraint and the algorithm used to implement it is described in
   P. Shaw, 'A Constraint for Bin Packing', CP'2004, with a fixed size for 
   the bins. It is also described in the global constraint catalog as 
   bin_packing, but with slightly different arguments: in the catalog, M
   (the number of bins) is implicitly defined by the domain of the variables 
   in ItemBins, and the representation of item is grouped into a single
   argument of collection of pairs, each pair representing an item:
   the bin to pack the item, and its size.
")
          ]).

bin_packing(ItemBins,Sizes,M,BinSize):-
        length(Bins,M),
        Bins :: 0..BinSize,
        bin_packing(ItemBins,Sizes,Bins).

:- comment(bin_packing/3, [
       amode: bin_packing(+,++,+),
       args: ["ItemBins": "A collection of N variables or integers (domain/value"
                       " between 1 and M)",
              "ItemSizes": "A collection of N non-negative integers",
              "BinLoads": "A collection of M variables or non-negative integers"
             ],
       see_also:[bin_packing/4],
       summary:"The one-dimensional bin packing constraint with loads: packing "
               "N items into M bins, each bin having a load",
       kind:[constraint:[root:[ic,fd]]],
       desc: html("\
   This constraint is for one-dimensional bin-packing, that is, to pack N
   items with individual sizes into M bins, such that the sum of sizes of
   items in each bin equals the load of that bin, as specified in BinLoads.
   Each element of ItemBins and its corresponding element in ItemSizes
   represents an item, such that the i'th element of ItemSizes is the size
   of the i'th item, and the i'th element of Item is the bin this item is
   packed into. BinLoads represent the load of each bin, i.e. the sum
   of the sizes of items assigned to that bin, with the j'th element 
   representing the load for bin j. An (integer finite domain) variable for 
   the load allows a constraint on the load to be specified, such as a
   minimum and/or maximum load for the bin.
</P><P>
   This constraint and the algorithm used to implement it is described in
   P. Shaw, 'A Constraint for Bin Packing', CP'2004, and is described in
   the global constraint catalog as bin_packing_capa, where the CAPACITY
   parameter is replaced by a collection of domain variables
   
")
          ]).

bin_packing(Items,Sizes,Bins):-
        collection_to_list(Items,L),    %%% ItemBins
        collection_to_list(Sizes,S),
        collection_to_list(Bins,B),     %%% Loads
        ( B==[] ->
            L=[], S=[]  % can't pack anything if no bins
        ; L==[] ->
            S=[], ( foreach(0,B) do true )      % bins all empty
        ;
            (
                foreach(InBin,L),
                foreach(Size,S),
                foreach(item(InBin,Size),ItemTerms),
                fromto(0,Sum1,Sum2,Total),
                count(_,1,NrItems)
            do
                Size >= 0,
                Sum2 is Sum1+Size
            ),
            sort(2,>=,ItemTerms,Sorted),
            length(B,NrBins),
            sumlist(B,Total),   % this is the constant-time sumlist constraint

            bin_packing01(L,S,B,NrItems,NrBins),
            bin_packing(L,S,B,Total,NrItems,NrBins,Sorted)
        ).

bin_packing01(L,S,B,NrItems,NrBins):-
        dim(Boolean,[NrItems,NrBins]),
        Boolean[1..NrItems,1..NrBins] :: 0..1,
        (foreach(X,L),
         count(I,1,NrItems),
         param(Boolean,NrBins) do
            bool_channeling(X,Boolean[I,1..NrBins],1)
        ),
        (foreach(Y,B),
         count(J,1,NrBins),
         param(Boolean,NrItems,S) do
            (for(I,1,NrItems),
             foreach(Size,S),
             foreach(Size*X,Sum),
             param(Boolean,J) do
                subscript(Boolean,[I,J],X)
            ),
            sum(Sum) #= Y       % load maintenance
        ).


bin_packing(L,_S,B,_Total,_NrItems,NrBins,Items):-
        hash_create(Hash),
        call_priority((
            check_bin_packing(B,NrBins,Items,Hash),
            term_variables(L-B,AllVars),
            ( AllVars==[] -> true ;
                suspend(update_bin_packing(AllVars,B,
                                           NrBins,Items,Hash,Susp),
                        5,[%AllVars->inst, % implicit
                           L->any,
                           B->[min,max]],Susp)
            )
        ), 2).

:-demon(update_bin_packing/6).
update_bin_packing(AllVars,B,NrBins,Items,Hash,Susp):-
        check_bin_packing(B,NrBins,Items,Hash),
        (ground(AllVars) ->
            kill_suspension(Susp)
        ;
            true
        ).


% this is the routine that checks if the bin packing is currently
%  feasible and if some domains can be reduced
% it applies two types of reasoning: the L2 bounds and no_sum
% Items are sorted by increasing size

check_bin_packing(Bins,NrBins,Items,Hash):-
        l2_limit(Bins,NrBins,Items),
        no_sum_reasoning(Bins,1,Items,Hash),
        true.

% this iterates over all bins and checks the space in the bins compared
%  to things that might be placed there 

no_sum_reasoning([],_J,_Items,_).
no_sum_reasoning([Bin|Bins],J,Items,Hash):-
        get_bounds(Bin,BMin,BMax),
        % find items (Vars, Candidates) that might be in the bin, but
        % are not fixed; Fixed is the size of items already in bin
        candidates(Items,J,Variables,Candidates,0,_Possible,0,Fixed),
        %writeln(cand(J,BMin,BMax,Candidates,Fixed)),
        (Candidates = [] ->
            true
        ;
            % Alpha, Beta is min/max capacity of bin after deducing the fixed items
            Alpha is BMin - Fixed,
            Beta is BMax-Fixed,
            % compute the total size (Sum) and number (N) of
            % candidates that might be in bin
            sum_cand(Candidates,0,Sum,0,N),
            % perform NOSUM algorithm of Shaw (Section 3.2)
            no_sum(Hash,Candidates,Sum,N,Alpha,Beta,Result,_Alpha1,_Beta1),
            Result == feasible,
            % update bin load bounds (Section 3.3)
            update_lower_bound(Candidates,Sum,N,Fixed,BMin,Bin,Hash),
            update_upper_bound(Candidates,Sum,N,Fixed,BMax,Bin,Hash),
            N1 is N-1,
            % perform shaving on items (Section 3.4)
            internal_shave(Variables,Candidates,[],Sum,N1,Alpha,Beta,J,Hash)
        ),
        J1 is J+1,
        no_sum_reasoning(Bins,J1,Items,Hash).


% check what happens if a potential item would or wouldn't be in the bin, i.e.
%  remove it from candidates, adjust alpha and beta and perform no_sum
%  reasoning considering item inside or outside bin; if the no_sum is
%  infeasible, then fix assignment to opposite
% N1 is number of potential items without the shaved one

internal_shave([],[],_Previous,_Sum,_N1,_Alpha,_Beta,_J,_Hash).
internal_shave([Var|Variables],[C|Candidates],Previous,Sum,N1,
               Alpha,Beta,J,Hash):-
        (check_in(J,Var) ->
            append(Previous,Candidates,Current),
            % Current is still sorted by increasing size
            Sum1 is Sum-C,
            %        writeln(current(Sum1,Current)),
            Alpha1 is Alpha-C,
            Beta1 is Beta-C,
            no_sum(Hash,Current,Sum1,N1,Alpha1,Beta1,Result,_,_),
            (Result == infeasible ->
                %            writeln(removed(Var,J)),
                Var #\= J
            ;
                true
            ),
            no_sum(Hash,Current,Sum1,N1,Alpha,Beta,Result2,_,_),
            (Result2 == infeasible ->
                %            writeln(fix(Var,J)),
                Var = J
            ;
                true
            )
        ;
            true
        ),
        append(Previous,[C],Previous1),
        internal_shave(Variables,Candidates,Previous1,Sum,N1,
                       Alpha,Beta,J,Hash).

update_lower_bound(Candidates,Sum,N,Fixed,BMin,Bin,Hash):-
        Alpha is BMin - Fixed,
        Beta is BMin-Fixed,
        no_sum(Hash,Candidates,Sum,N,Alpha,Beta,Result,_Alpha1,Beta1),
        (Result == infeasible ->
%            writeln(update_lower(Bin,Fixed,Beta1)),
            Bin #>= Fixed+Beta1
        ;
            true
        ).

update_upper_bound(Candidates,Sum,N,Fixed,BMax,Bin,Hash):-
        Alpha is BMax - Fixed,
        Beta is BMax-Fixed,
        no_sum(Hash,Candidates,Sum,N,Alpha,Beta,Result,Alpha1,_Beta1),
        (Result == infeasible ->
%            writeln(update_upper(Bin,Fixed,Alpha1)),
            Bin #=< Fixed+Alpha1
        ;
            true
        ).


% compare size of items that might be placed in bin to adjusted min/max bound

% candidate size is always between lower and upper limits, feasible
:- mode no_sum(+,+,+,+,+,+,-,-,-). % is det
no_sum(_Hash,_Candidates,Sum,_N,Alpha,Beta,feasible,nan,nan):-
        Alpha =< 0, Sum =< Beta,        % Bug in Shaw's paper!
        !.
% there is no way to reach the lower bound with candidates left
no_sum(_Hash,_Candidates,Sum,_N,Alpha,_Beta,infeasible,Sum,Sum):-
        Sum < Alpha,
        !.
% beta is negative, no way to satisfy bound
no_sum(_Hash,_Candidates,_Sum,_N,_Alpha,Beta,infeasible,0,0):- 
        Beta < 0,
        !.
% we've seen this problem before, use hashed result
no_sum(Hash,Candidates,Sum,N,Alpha,Beta,Result,Alpha1,Beta1):-
        hash_find(Hash,key(Alpha,Beta,N,Sum,Candidates),
                  result(Result,Alpha1,Beta1)),
        !.
% compute Alpha1,Beta1, decide feasible/infeasible and store result
no_sum(Hash,Candidates,Sum,N,Alpha,Beta,Result,Alpha1,Beta1):-
        array_list(Array, Candidates),
%        writeln(no_sum(Candidates,Sum,N,Alpha,Beta)),
        % setting up SumB and SumC
        % we need atleast K1 items 
        % their size without the smallest one is SumC 
        set_c(Array,N,Alpha,0,K1,0,SumC),
%        writeln(c(K1,SumC)),
        % the size of the smallest item required is SumB
        I is N-K1, arg(I, Array, SumB),
        lp(0,SumA,SumB,SumC,0,K1,Array,N,Alpha,Beta,Alpha1,Beta1),
        (SumA < Alpha ->
            Result = infeasible
        ;
            Result = feasible
        ),
        % remember result
        hash_add(Hash,key(Alpha,Beta,N,Sum,Candidates),
                  result(Result,Alpha1,Beta1)).


% find how many items (Kend) are atleast required to reach alpha
% and their total size (SumEnd) without the smallest one

set_c(Array,N,Alpha,K1,Kend,SumC,SumEnd):-
        I is N-K1,
        arg(I,Array,Size),
        SumC1 is SumC+Size,
        SumC1 < Alpha,
        !,
        K1a is K1+1,
        set_c(Array,N,Alpha,K1a,Kend,SumC1,SumEnd).
set_c(_Array,_N,_Alpha,K1,K1,SumC,SumC).


% Implement the NOSUM routine of Shaw. Result either feasible or
%  infeasible, for infeasible updated Alpha1 and Beta1 are provided

lp(SumA,SumAEnd,SumB,SumC,K,K1,Array,N,Alpha,Beta,Alpha1,Beta1):-
        SumA < Alpha,
        SumB =< Beta,
        !,
        Ka is K+1,
        arg(Ka, Array, Size),
        SumA1 is SumA+Size,
        (SumA1 < Alpha ->
            K1a is K1-1,
            I is N-K1a, arg(I, Array, SizeI),
            SumB1 is SumB+SizeI,
            SumC1 is SumC-SizeI,
            lp1(SumA1,SumB1,SumB2,SumC1,SumC2,K1a,K1b,Array,N,Alpha,Ka)

        ;
            SumB2 = SumB,
            SumC2 = SumC,
            K1b = K1
        ),
%        writeln(k(Ka,K1b)),
        lp(SumA1,SumAEnd,SumB2,SumC2,Ka,K1b,Array,N,Alpha,Beta,Alpha1,Beta1).
lp(SumA,SumA,SumB,SumC,_,_,_,_,_,_,Alpha1,SumB):-
        Alpha1 is SumA+SumC.

lp1(SumA,SumB,SumBEnd,SumC,SumCEnd,K1,K1End,Array,N,Alpha,K):-
        SumA+SumC >= Alpha,
        !,
        K1a is K1-1,
        SumC1 is SumC-Array[N-K1a],
        SumB1 is SumB+Array[N-K1a]-Array[N-K1a-K-1],
        lp1(SumA,SumB1,SumBEnd,SumC1,SumCEnd,K1a,K1End,Array,N,Alpha,K).
lp1(_SumA,SumB,SumB,SumC,SumC,K1,K1,_Array,_N,_Alpha,_K).

        
% compute total size and number of candidates
sum_cand([],S,S,N,N).
sum_cand([Size|C1],S,Send,N,Nend):-
        S1 is S+Size,
        N1 is N+1,
        sum_cand(C1,S1,Send,N1,Nend).

% find candidates that are or can be placed in bin J
% the fixed size computes the size of all items that are already in
% the bin
% the potential size computes the items that either are or might be in
% the bin
% Vars and Cand are only items that might be in the bin, not already fixed
:- mode candidates(+,+,-,-,+,-,+,-).
candidates([],_,[],[],P,P,F,F).
candidates([item(X,Size)|Items],J,Vars,Cand,P,PEnd,F,FEnd):-
        ( integer(X), X == J ->
            F1 is F+Size,
            P1 is P+Size,
            candidates(Items,J,Vars,Cand,P1,PEnd,F1,FEnd)
        ; var(X), check_in(J,X) ->
            Vars = [X|Vars1],
            Cand = [Size|Cand1],
            P1 is P+Size,
            candidates(Items,J,Vars1,Cand1,P1,PEnd,F,FEnd)
        ;
            candidates(Items,J,Vars,Cand,P,PEnd,F,FEnd)
        ).


% Compute and apply L2 bound from Shaw section 4.1:
% First, we translate the current situation into an equivalent problem with
% bins of uniform capacity C, each having an artificial item per bin, that
% brings the remaining capacity down to the actual remaining bin capacity.
% We can then compute Martello&Toth's L2 bound for the problem of packing
% these artificial items plus the remaining unfixed items into fixed size
% bins of uniform capacity C.
l2_limit(Loads,NrBins,Items):-
        % find the maximum bin size C
        ( foreach(Load,Loads), fromto(-1,Max1,Max2,C) do
            get_upb(Load,Max),
            Max2 is max(Max1,Max)
        ),
        % initialize FixedLoads with differences between bin capacity and C
        dim(FixedLoads, [NrBins]),
        ( foreach(Load,Loads), foreacharg(F1,FixedLoads), param(C) do
            get_upb(Load,Max),
            F1 is C-Max
        ),
        % add fixed item's sizes to FixedLoad of the bin they are in
        % also collect sizes of remaining unfixed items
        (
            foreach(Item,Items),        % item/2
            fromto(NonFixed,N1,N2,[]),  % sizes of unfixed items
            param(FixedLoads)           % destructively updated
        do
            Item = item(Bin,Size),
            ( var(Bin) ->
                N1=[Size|N2]
            ;
                % item fixed to Bin
                N1=N2,
                arg(Bin, FixedLoads, Old),
                New is Old+Size,
                setarg(Bin, FixedLoads, New)
            )
        ),
        % collect nonzero artificial fixed items in list, then sort
        ( foreacharg(BinFixed,FixedLoads), fromto(NewItems,F1,F2,NonFixed) do
            ( BinFixed > 0 -> F1=[BinFixed|F2] ; F1=F2 )
        ),
	% Note: instead of sorting the fixed items and then merging the result
	% with the (already sorted) NonFixed items, we simply concatenate them
	% and sort all at once.  Since sort/4 uses natural merge sort
	% (exploiting any presorted sublists), this should be more efficient.
	sort(0,>=,NewItems,SortedSizes),
	% Finally compute L2 bound for the transformed problem
        l2_bound(SortedSizes, C, L2),
        NrBins >= L2.  % may fail


% Given non-increasingly sorted item sizes, and assuming uniform capacity C
% per bin, compute the lower bound L2 on the number of bins needed.

l2_bound(SortedSizes, C, L2) :-
        CHalf is C//2,
        prefix_gt_count_sum_rest(SortedSizes, CHalf, 0, N2Ct0, 0 , N2Sum0, Smalls),
        reverse_sum(Smalls, [], N3s0, 0, N3Sum0),
        l2_loop(0, C, SortedSizes, N2Ct0, N2Sum0, N3s0, N3Sum0, 0, Bound),
        L2 is N2Ct0 + Bound.

    l2_loop(K, C, N2s, N2Ct, N2Sum, N3s, N3Sum, Bound0, Bound) :-
        Bound1 is max(Bound0,integer(ceiling((N3Sum-(N2Ct*C-N2Sum))/C))),
        ( N2Ct>0, N3s=[_|_] ->
            [N2Largest|_] = N2s,
            K1Hi is C-N2Largest+1,
            prefix_le_rest(N3s, K, N3s2),
            ( [K1Lo|_] = N3s2 ->        % first in N3 > K
                K1 is min(K1Hi,K1Lo)
            ;
                K1 = K1Hi
            ),
            CK1 is C-K1,
            prefix_gt_count_sum_rest(N2s, N2Ct, CK1, 0, N2to1Ct, 0, N2to1Sum, N2s1),
            N2Ct1 is N2Ct-N2to1Ct,
            N2Sum1 is N2Sum-N2to1Sum,
            prefix_lt_sum_rest(N3s, K1, 0, N3delSum, N3s1),
            N3Sum1 is N3Sum-N3delSum,
            l2_loop(K1, C, N2s1, N2Ct1, N2Sum1, N3s1, N3Sum1, Bound1, Bound)
        ;
            Bound = Bound1
        ).
        

reverse_sum([], Rs, Rs, Sum, Sum).
reverse_sum([X|Xs], Rs0, Rs, Sum0, Sum) :-
        Sum1 is Sum0+X,
        reverse_sum(Xs, [X|Rs0], Rs, Sum1, Sum).
        
% count and sum up the leading list elements greater than Limit
prefix_gt_count_sum_rest([X|Xs],Limit,N0,N,Size0,Size,Rest):-
        X > Limit,
        !,
        N1 is N0+1,
        Size1 is Size0+X,
        prefix_gt_count_sum_rest(Xs,Limit,N1,N,Size1,Size,Rest).
prefix_gt_count_sum_rest(Rest,_,N,N,Size,Size,Rest).

% same, but looks only at the first Len elements of the list
prefix_gt_count_sum_rest([X|Xs],Len,Limit,N0,N,Size0,Size,Rest):-
        Len > 0,
        X > Limit,
        !,
        Len1 is Len-1,
        N1 is N0+1,
        Size1 is Size0+X,
        prefix_gt_count_sum_rest(Xs,Len1,Limit,N1,N,Size1,Size,Rest).
prefix_gt_count_sum_rest(Rest,_,_,N,N,Size,Size,Rest).

% sum up the leading list elements less than Limit
prefix_lt_sum_rest([X|Xs], Limit, Size0, Size, Rest):-
        X < Limit,
        !,
        Size1 is Size0+X,
        prefix_lt_sum_rest(Xs, Limit, Size1, Size, Rest).
prefix_lt_sum_rest(Xs, _, Size, Size, Xs).

% drop the leading list elements less or equal to Limit
prefix_le_rest([X|Xs], Limit, Rest):-
        X =< Limit,
        !,
        prefix_le_rest(Xs, Limit, Rest).
prefix_le_rest(Xs, _, Xs).

