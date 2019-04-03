%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2009, 2011, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-lib(ic).
:-lib(ic_global).
:-use_module(library(ic_edge_finder)).
:- ["decoding_net4_support"].

:- set_flag(print_depth, 200).

:-dynamic(currentbar/5).
:-dynamic(addr/3).

pci_id_node_id(addr(Bus, Dev, Fun, BarNum, Pref), [Bus, Dev, Fun, BarNum, "PCI ":Pref]).
pcibus_node_id(["PCIBUS"]).

merge_window([], [], _).
merge_window(R, [], L) :-
    L = [R].

merge_window([], [H | T], L) :-
    merge_window(H, T, L).

merge_window(range(B, H), [range(B2, H2) | T2], L) :-
    (H == B2 ->  
        merge_window(range(B, H2), T2, L)
    ;
        merge_window([], T2, L2),
        (member(L2, range(B2, H2)) ->
            L = [range(B, H) | L2]
        ;
            append([range(B,H)], [range(B2, H2) | L2], L)
        )
    ).
       
merge_address_windows(Addr, Output) :-
   
    findall(range(Base, High), (rootbridge_address_window(Addr, mem(B, H)),
                               LT1 is B / 4096,
                               ceiling(LT1, LT2),
                               integer(LT2, Base),
                               HT1 is H / 4096,
                               ceiling(HT1, HT2),
                               integer(HT2, High)              
                               ),
            Memory),
    merge_window([], Memory, Output).
      
get_address_window(Addr, Min, Max) :-
    findall(Low, rootbridge_address_window(Addr, mem(Low, _)), LowList),
    findall(High, rootbridge_address_window(Addr, mem(_, High)), HighList),
    (not(LowList == []) -> 
        ic:minlist(LowList, Min)
    ;
        Min = 0
    ),
    (not(HighList == []) -> 
        ic:maxlist(HighList, Max)
    ;
        Max = 0
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% main goal to be called from outside
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bridge_programming(Plan, NrElements) :-
   Granularity is 4096,
   %Granularity is 16384,
   %Granularity is 1048576,
% find all the root bridges
    findall(root(Addr,Child,mem(LP,HP), Ranges),
            (  rootbridge(Addr,Child, _),
               merge_address_windows(Addr, Ranges),
               get_address_window(Addr, L, H),
               LT1 is L / Granularity,
               ceiling(LT1, LT2),
               integer(LT2, LP),
               HT1 is H / Granularity,
               ceiling(HT1, HT2),
               integer(HT2, HP)              
            ),Roots),

% exclude fixed memory from being allocated to devices
    ( is_predicate(fixed_memory/2) ->
        findall(range(ResLowP,ResSizeP),
          (
            fixed_memory(ResLow,ResHigh), T1 is ResLow / Granularity, floor(T1,T2),
            integer(T2,ResLowP),
            T3 is (ResHigh - ResLow) / Granularity,
            ceiling(T3,T4),
            integer(T4,ResSizeP)
          ), ExclRangesFixed);
        ExclRangesFixed = []
    ),

% exclude fixed memory from being allocated to devices
%    ( is_predicate(memory_region/5) ->
%        findall(range(ResLowP,ResSizeP),
%          (
%            memory_region(ResLow, _, Size, _, _), 
%            T1 is ResLow / Granularity, 
%            floor(T1,T2),
%            integer(T2,ResLowP),
%            T3 is Size / Granularity,
%            ceiling(T3,T4),
%            integer(T4,ResSizeP)
%          ), ExclRangesMemory);
%        ExclRangesMemory = []
%    ),

% exclude IOAPIC regions from being allocated to devices
    ( is_predicate(ioapic/3) ->
      % according to the spec we need 64Bytes in the Intel case. Reserve a page
      % anyway, since currently we cannot query the real requested size
      TSz is (4096 / Granularity),
      ceiling(TSz, TSz2),
      integer(TSz2, IOAPIC_MinSize),
      findall(range(Bs,IOAPIC_MinSize),
               (
                ioapic(_,B,_),
                T1 is B / Granularity,
                floor(T1, T2),
                integer(T2, Bs)
               ),IOAPICs)
      ;
      IOAPICs = []
    ),


%if IOAPIC appears as BAR, do not add this region to the "avoid" regions
    findall(range(SubBase, SubSize),
        (
            member(IOAPICRegionMember, IOAPICs),
            range(SubBase, SubSize) = IOAPICRegionMember,
            bar(_,_,OrigBarBase,_,_,_,_),
            T27 is OrigBarBase / Granularity,
            floor(T27, T28),
            integer(T28, SubBase)
        ), RemoveRegionList),
    subtract(IOAPICs,RemoveRegionList,IOAPICsRemoveRegionList),   

%all the regions to avoid
    append(ExclRangesFixed, IOAPICsRemoveRegionList, ExclRanges),
    %append(ExclRanges2, ExclRangesMemory, ExclRanges),
% create an assignment for all PCI buses (all root bridges and their children)
    ( foreach(Root,Roots),
      foreach(P,Plan),
      foreach(L,Lengths),
      param(Granularity),
      param(ExclRanges),
      param(IOAPICs)
      do
        bridge_assignment(P,Root, Granularity, ExclRanges, IOAPICs),
        length(P,L)
    ),
    sum(Lengths,NrElements).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% small tools
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

adjust_range(X, buselement(T,A,Sec,B1,H1,S,Tp,PF, PCIe, Bits), buselement(T,A,Sec,B2,H2,S,Tp,PF, PCIe, Bits)) :-
    B2 is B1 + X,
    H2 is H1 + X.

back_to_bytes(Granularity, buselement(T,A,Sec,BP,HP,SP,Tp,PF, PCIe, Bits), buselement(T,A,Sec,B,H,S,Tp,PF, PCIe, Bits)) :-
    B is BP * Granularity,
    H is HP * Granularity,
    S is SP * Granularity.


should_shift_bridge(Addr, Sec, Root, Granularity, OrigP) :-
     secondary(Bus) = Sec,
     (bar(addr(Bus, _, _), _ , Orig, _ , _, prefetchable, _ ) ->
          % Bridge has a bar that is above shifting limit
          Limit is 10000000*Granularity,
          Orig @> Limit,
          OrigF is Orig/Granularity,
          integer(OrigF, OrigP)
     ;
        % The root bridge needs also shifting
        root(RootAddr,childbus(MinBus,MaxBus), _, _) = Root,
        (Addr == RootAddr -> 
          % get max bar since they are all under this rootbridge
          findall(Orig, (bar(addr(Bus2, _ , _), _, Orig, _ , _, prefetchable , _),
                         Bus2 @=< MaxBus,
                         Bus2 @>= MinBus), 
                 Origs),
          (not(Origs == []) ->
              ic:maxlist(Origs, Max),           
              % Sanity checks
              Limit is 10000000*Granularity,
              Max @> Limit,
              OrigF is Max/Granularity,
              integer(OrigF, OrigP)
          )
        ;
            false
        )
     ).

shift_into_window_64_bit(Granularity, Windows, Root, buselement(T,A,Sec,B1,H1,S,Tp,PF, PCIe, Bits), 
                         buselement(T,A,Sec,B2,H2,S,Tp,PF, PCIe, Bits)) :-

	(T == device ->
        %BAR
	    bar(A,Sec, Orig, _, _, _, _),
 
        O1 is Orig / Granularity,
        ceiling(O1, O2),
        integer(O2, OrigP),

        (OrigP > 10000000, PF == prefetchable, Bits == 64 ->
            (foreach(range(B, H), Windows),
             param(B2),
             param(H2),
             param(B1),
             param(H1),
             param(OrigP)
             do
                (H @>= OrigP, B @=< OrigP ->
                    B2 is B1 + B,
                    H2 is H1 + B
                ;
                    true
                )
            )
        ;
            B2 is B1,
            H2 is H1
        )
    ;
        %Bridge
        findall(OrigP, (should_shift_bridge(A, Sec, Root, Granularity, OrigP),
                        PF == prefetchable)
                ,Origs),
        (Origs == [] ->
            %no bar under bridge with size > 10'000'000
            B2 = B1,
            H2 = H1
        ;
            ic:maxlist(Origs, Max),
            (foreach(range(B, H), Windows),
             param(B2),
             param(B1),
             param(H1),
             param(H2),
             param(Max)
             do
                (H @>= Max, B @=< Max ->
                    B2 is B1 + B,
                    H2 is H1 + B
                ;
                    true
                )
            )
        )
    ).

create_vf_busele_list(VFs, LMem, HMem, Granularity) :-
	 findall(buselement(device,addr(Bus,Dev,Fun),BAR,Base,High,SizeP,Type,Prefetch, PCIe, Bits),
	            ( vf(pfaddr(_, _, _), addr(Bus, Dev, Fun)),
                  device(PCIe, addr(Bus,Dev,Fun),_,_,_,_,_,_),
	              bar(addr(Bus,Dev,Fun),BAR, _,Size, Type, Prefetch, Bits),
                  ST1 is Size / Granularity,
                  ceiling(ST1, ST2),
                  integer(ST2, SizeP),
                  Base::[LMem..HMem],
                  High::[LMem..HMem]
	            ),VFs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tree allocation etc, once per prefetchable and non prefetchable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
assign_addresses(Plan, Root, Tree, Granularity, ExclRanges, IOAPICs, HMem2) :-
    root(_ , _, mem(LMem,HMem), Ranges) = Root,
    setrange(Tree,_,_,_),
    nonoverlap(Tree),
    naturally_aligned(Tree, 256, LMem, HMem, HMem2, _),
    %naturally_aligned(Tree, 256, LMem, HMem, HMem2, ExtraVars),
    tree2list(Tree,ListaU),
    sort(6, >=, ListaU, Lista),
    not_overlap_memory_ranges(Lista, ExclRanges),

    keep_orig_addr(Lista, 12, 3, _, _, _, _),
    keep_ioapic_bars(Lista, IOAPICs),
    %labelall(Lista, ExtraVars),
    labelall(Lista),

    % Shift 64 bit addresses back into their window since
    % disjunctive() only takes numbers uf to 10'000'000
    maplist(shift_into_window_64_bit(Granularity, Ranges, Root),Lista, Plan).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Building the decoding net
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
build_decoding_net(Tree, Granularity, RelativeAddr):-
    t(buselement(_, addr(Bus, Dev, Fun), BarNum, Base, High, _, _, Pref, _, _) ,Children) = Tree,
    Size is High - Base, 
    pci_id_node_id(addr(Bus, Dev, Fun, BarNum, Pref), Id),

    (Children == [] ->
        % This is a node that has to accept since there are no children
        (Size @> 0 ->
            SP is Size*Granularity -1,
            (RelativeAddr == true ->
                %writeln(bar_new(region(Id, block(0, SP)))),
                assert_accept(region(Id, block(0, SP)))
            ;   
                B is Base*Granularity,
                H is High*Granularity -1, 
                %writeln(bar_new(region(Id, block(B, H)))),
                assert_accept(region(Id, block(B, H)))
            )
        ;
            true
        )
    ;
        % This is a node that has to translate since there are children on this bridge
        %writeln("=================================================="),
        ( foreach(El, Children),
          param(Id),
          param(Base),
          param(Granularity),
          param(RelativeAddr)
          do  
            t(buselement(_, addr(Bus2, Dev2, Fun2), BarNum2, Base2, High2, _, _, Pref2, _, _) , _) = El,
            pci_id_node_id(addr(Bus2, Dev2, Fun2, BarNum2, Pref2), PCIId),
            
            B2 is Base2 * Granularity,
            H2 is High2 * Granularity -1,   
            S is (H2-B2),

            (RelativeAddr == true ->
                AcceptStart is (Base2 - Base) * Granularity,
                AcceptEnd is AcceptStart + S,
                ChildStart is AcceptStart
            ;
                AcceptStart is B2,
                AcceptEnd is H2,
                ChildStart is B2
            ),

            (S @> 0 ->
                assert_translate(region(Id, block(AcceptStart, AcceptEnd)), name(PCIId, ChildStart)),
                %writeln(bridge_new((region(Id, block(AcceptStart, AcceptEnd)), S, name(PCIId, ChildStart)))),
                build_decoding_net(El, Granularity, RelativeAddr)
            ;
                true
            )
        )
        %writeln("==================================================")
    ).


% TODO 32 Bit prefetchable
%add_root_to_decoding_net(Addr, MinBus, Base, High, Ranges, Prefetchable) :-
%    Size is High - Base,
%    (Size @> 0 ->
%        addr(Bus, Dev, Fun) = Addr,
%        pci_id_node_id(addr(Bus, Dev, Fun, secondary(MinBus)), PCIId),
%        pcibus_node_id(Id),
%        ( Prefetchable == prefetchable ->
%            (foreach(range(B, H), Ranges),
%             param(Id),
%             param(Base),
%             param(High),
%             param(PCIId)
%             do
%                ( Base @>= B, Base @=< H, High @>= H,
%                    writeln("Prefetchable"),
%                    writeln((region(Id, block(Base, H)), name(PCIId, Base))),
%                    assert_translate(region(Id, block(Base, H)), name(PCIId, Base))
%                ;
%                    true
%                ),
%
%                (  Base @=< B, Base @=< H, High @=< H,
%                    writeln("Prefetchable"),
%                    writeln((region(Id, block(B, High)), name(PCIId, B))),
%                    assert_translate(region(Id, block(B, High)), name(PCIId, B))
%                ;
%                    true
%                )
%            )
%        ;
%            writeln("Nonprefetchable"),
%            writeln((region(Id, block(Base, High)), name(PCIId, Base))),
%            assert_translate(region(Id, block(Base, High)), name(PCIId, Base))
%        )
%    ).

add_root_to_decoding_net(Addr, MinBus, Pref, Base, High) :-
        addr(Bus, Dev, Fun) = Addr,
        pci_id_node_id(addr(Bus, Dev, Fun, secondary(MinBus), Pref), PCIId),
        pcibus_node_id(Id),
        assert_translate(region(Id, block(Base, High)), name(PCIId, Base)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the main part of the allocation. Called once per root bridge
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bridge_assignment(Plan, Root, Granularity, ExclRanges, IOAPICs) :-
    root(Addr,childbus(MinBus,MaxBus),mem(LMem,HMem), Ranges) = Root,
    X is HMem - LMem,
    Tmp is 4294963200 / Granularity,
    ceiling(Tmp, Tmp2),
    integer(Tmp2, HMem2),
    Type = mem,

% prefetchable (Shifts addresses above 10'000'000 to 0)
    constrain_bus(Granularity, Type, prefetchable, Addr,MinBus,MaxBus,LMem,HMem,BusElementListP, Ranges),
    RBaseP::[LMem..HMem],
    RHighP::[LMem..HMem],
    RSizeP::[0..X],
    devicetree(BusElementListP,buselement(bridge,Addr,secondary(MinBus),RBaseP,RHighP,RSizeP, Type, prefetchable, pcie, 0),TP),

% nonprefetchable, Highest address must be less than 4GB
    constrain_bus(Granularity, Type, nonprefetchable, Addr,MinBus,MaxBus,LMem,HMem2,BusElementListNP, Ranges),
    RBaseNP::[LMem..HMem2],
    RHighNP::[LMem..HMem2],
    RSizeNP::[0..X],
    devicetree(BusElementListNP,buselement(bridge,Addr,secondary(MinBus),RBaseNP,RHighNP,RSizeNP, Type, nonprefetchable, pcie, 0),TNP),

    assign_addresses(PPlan, Root, TP, Granularity, ExclRanges, IOAPICs, HMem2),
    assign_addresses(NPPlan, Root, TNP, Granularity, ExclRanges, IOAPICs, HMem2),

    append(PPlan, NPPlan, Plan),
    subtract(PPlan, [buselement(bridge,Addr, _, _, _,_,_,prefetchable,_,_)],PPlan2),
    devicetree(PPlan2, buselement(bridge,Addr,secondary(MinBus), RBaseP, RHighP, RSizeP, Type, prefetchable, _, _), TP2),
    add_root_to_decoding_net(Addr, MinBus, prefetchable, RBaseP, RHighP),
    build_decoding_net(TP2, Granularity, true),

    %Add TNP Root
    subtract(NPPlan, [buselement(bridge,Addr, _, _, _,_,_,nonprefetchable,_,_)],NPPlan2),
    devicetree(NPPlan2, buselement(bridge,Addr,secondary(MinBus),RBaseNP,RHighNP,RSizeNP, Type, prefetchable, _, _), TNP2),
    add_root_to_decoding_net(Addr, MinBus, nonprefetchable, RBaseNP, RHighNP),
    build_decoding_net(TNP2, Granularity, true).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% instantiating the variables
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

base(buselement(_,_,_,Base,_,_,_,_,_,_),Base).
high(buselement(_,_,_,_,High,_,_,_,_,_),High).
size(buselement(_,_,_,_,_,Size,_,_,_,_),Size).

labelall(BusElementList) :-
    maplist(base, BusElementList, Base),
    maplist(size, BusElementList, Size),
    maplist(high, BusElementList, High),
    append(Size, Base , L1),
    append(L1, High , L2),
    labeling(L2).

labelall(BusElementList, ExtraVars) :-
    maplist(base, BusElementList, Base),
    maplist(size, BusElementList, Size),
    maplist(high, BusElementList, High),
    append(Size, Base , L1),
    append(L1, High , L2),
    append(L2, ExtraVars , L3),
    labeling(L3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create the list of devices and bridges in form of buselements and create the
% variables.
% we care about the allocation of memory mapped registers here, therefore we only
% look at bar located in "mem", not "io"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

constrain_bus(Granularity, Type, Prefetch, RootAddr,Bus,MaxBus,LMem,HMem,OutBusElementList, Ranges) :-
    constrain_bus_ex(Granularity, Type, Prefetch, RootAddr,Bus,MaxBus,LMem,HMem,[],OutBusElementList, Ranges).

constrain_bus_ex(_, _, _, _,Bus,MaxBus,_,_,InL,InL, _) :- Bus > MaxBus.
constrain_bus_ex(Granularity, Type, Prefetch, RootAddr,Bus,MaxBus,LMem,HMem,InBusElementList,OutBusElementList, Ranges) :-
    Bus =< MaxBus,
    % 32 Bit device and bridges that are non prefetchable require an address below 4GB
    Tmp is 4294963200 / Granularity,
    ceiling(Tmp, Tmp2),
    integer(Tmp2, HMem2),
    ( is_predicate(bridge/8) ->
	    findall(buselement(bridge,addr(Bus,Dev,Fun),secondary(Sec),Base,High,Size,Type,Prefetch, PCIe, 0),
	            ( bridge(PCIe, addr(Bus,Dev,Fun), _, _, _, _, _, secondary(Sec)),
	              not addr(Bus,Dev,Fun) = RootAddr,
                  (Prefetch == nonprefetchable ->
                    SMax is HMem2 - LMem,
	                Base::[LMem..HMem2],
	                High::[LMem..HMem2],
                    Size::[0..SMax]
                  ;
                    SMax is HMem - LMem,
	                Base::[LMem..HMem],
	                High::[LMem..HMem],
                    Size::[0..SMax]
                  )
	            ),BridgeList);
        BridgeList = []
    ),
    ( is_predicate(device/8) ->
	    findall(buselement(device,addr(Bus,Dev,Fun),BAR,Base,High,SizeP,Type,Prefetch, PCIe, Bits),
	            ( device(PCIe, addr(Bus,Dev,Fun),_,_,_,_,_,_),
	              bar(addr(Bus,Dev,Fun),BAR, Orig, Size, Type, Prefetch, Bits),
                  ST1 is Size / Granularity,
                  ceiling(ST1, ST2),
                  integer(ST2, SizeP),
 
                  O1 is Orig / Granularity,
                  ceiling(O1, O2),
                  integer(O2, OrigP),
                  (Bits == 32 ->
                    Base::[LMem..HMem2],
                    High::[LMem..HMem2],
                    % We have multiple ranges, add constraint for each of them.
                    (foreach(range(B, H), Ranges),
                     param(OrigP),
                     param(Base),
                     param(High)
                     do
                        (H @>= OrigP, B @=< OrigP ->
                            Base::[B..H],
                            High::[B..H]
                        ;
                            true
                        )
                    )
                  ;
                    Base::[LMem..HMem],
                    High::[LMem..HMem],
                    % We have multiple ranges, add constraint for each of them.
                    (foreach(range(B, H), Ranges),
                     param(OrigP),
                     param(Base),
                     param(High)
                     do
                        (H @>= OrigP, B @=< OrigP ->
                            (OrigP > 10000000 ->
                                %shift to smallest address
                                H2 is H - B,
                                Base::[0..H2],
                                High::[0..H2]
                            ;
                                Base::[B..H],
                                High::[B..H]
                            )
                        ;
                            true
                        )
                    )
                  )
	            ),DeviceList);
        DeviceList = []
    ),
    append(BridgeList, DeviceList, MyBusElementList),
    append(InBusElementList, MyBusElementList, NewBusElementList),
    NextBus is Bus + 1,
    constrain_bus_ex(Granularity, Type, Prefetch, RootAddr, NextBus, MaxBus, LMem,HMem,NewBusElementList,OutBusElementList, Ranges).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create the PCI(e) device tree from a list of "buselement" and return it in Tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

devicetree(List,CurrRoot,Tree) :-
    buselement(bridge,_,secondary(Sec),_,_,_,_,_,_,_) = CurrRoot,
    findall(X,(
               member(Y,List),
               buselement(_,addr(Sec,_,_),_,_,_,_,_,_,_,_) = Y,
               devicetree(List, Y, X)),Children
           ),
    Tree = t(CurrRoot,Children).

devicetree(_,CurrRoot,Tree) :-
    buselement(device,_,_,_,_,_,_,_,_,_) = CurrRoot,
    Tree = t(CurrRoot, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% convert a tree to a list of buselements
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tree2list([],[]).
tree2list(Tree, List) :-
    t(Node,Children) = Tree,
    ( foreach(El,Children),
      foreach(L1,ChildList)
      do
        tree2list(El,L1)
    ),
    flatten(ChildList,L2),
    List = [Node|L2].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% store the new values of the BARs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
replace_current_BAR_values(L) :-
    delete_current_BAR_values(L),
    store_current_BAR_values(L).

store_current_BAR_values([]).
store_current_BAR_values([H|T]) :-
    ( buselement(device,Addr,BAR,Base,High,Size,_,_,_,_) = H ->
         assert(currentbar(Addr,BAR,Base,High,Size));
        true
    ),
    store_current_BAR_values(T).


delete_current_BAR_values([]).
delete_current_BAR_values([H|T]) :-
    ( buselement(device,Addr,BAR,_,_,_,_,_,_,_) = H ->
        ( currentbar(Addr,BAR,_,_,_) ->
            retract(currentbar(Addr,BAR,_,_,_));
            true
        );
        true
    ),
    delete_current_BAR_values(T).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% add constraints to the tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
range(buselement(_,_,_,Base,High,_,_,_,_,_),range(Base, High)).

% make sure that the bridge has a range which includes all the children
setrange(Tree,SubTreeSize,SubTreeMin,SubTreeMax) :-
    t(Node,Children) = Tree,
    ( foreach(El,Children),
      foreach(Sz,SizeList),
      foreach(Mi,MinList),
      foreach(Ma,MaxList)
      do
        setrange(El,Sz,Mi,Ma)
    ),
    ic_global:sumlist(SizeList,Size),
    buselement(_,_,_,Base,High,ElemSize,_,_,_,_) = Node,
    ElemSize #>= Size,
    ( not MinList=[] ->
        ic:minlist(MinList,Min),
        ic:maxlist(MaxList,Max),
        Min #>= Base,
        Max #=< High;
        true
    ),
    High #= Base + ElemSize,
    SubTreeSize #= ElemSize,
    SubTreeMin #= Base,
    SubTreeMax #= High.
setrange([],0,_,_).


% make sure that the children do not overlap
child(t(C,_),C).
nonoverlap(Tree) :-
    t(_ ,Children) = Tree,
    maplist(child,Children,ChildList),
    ( not ChildList=[] ->
        maplist(base,ChildList,Base),
        maplist(size,ChildList,Size),
        disjunctive(Base, Size)
        ;
        true
    ),
    ( foreach(El, Children)
      do
        nonoverlap(El)
    ).


naturally_aligned(Tree, BridgeAlignment, LMem, HMem, HMem2, ExtraVars) :-

    t(Node,Children) = Tree,
    ( buselement(device,_,_,Base,High,Size,_,_,_,Bits) = Node ->
      Divisor is Size,
      (Bits == 32 ->
        LimitRange is 1
      ;
        LimitRange is 0
      )
    ;
      buselement(bridge, Addr,_,Base,High,_, _, Prefetch,_,_) = Node ->
      Divisor is BridgeAlignment,
      (not Addr == addr(-1,-1,-1) ->
          (Prefetch == nonprefetchable ->
            LimitRange is 1
          ;
            LimitRange is 0
          )
      ;
        LimitRange is 0
      )
    ),

    ( LimitRange == 1 ->
        T1 is (HMem2 - LMem) / Divisor
    ;
        T1 is (HMem - LMem) / Divisor
    ),

    ceiling(T1, T2),
    integer(T2, Nr),
    N::[0..Nr],
    N2::[0..Nr],
    mod(LMem,Divisor,Remainder),
    ( Remainder =:= 0 ->
        Corr is 0;
        Corr is Divisor - Remainder
    ),
    Base #= N*Divisor + LMem + Corr,
    High #>= Base,
    High #= N2*Divisor + LMem + Corr,
    ( foreach(El, Children),
      fromto([N], XtraIn, XtraOut, ExtraVars),
      param(BridgeAlignment),
      param(LMem),
      param(HMem),
      param(HMem2)
      do
        naturally_aligned(El, BridgeAlignment, LMem, HMem, HMem2, E1),
        append(XtraIn, E1, XtraOut)
    ).


% do not overlap with the given list of memory ranges
not_overlap_memory_ranges([], _).
not_overlap_memory_ranges(_, []).
not_overlap_memory_ranges([buselement(bridge,_,_,_,_,_,_,_,_,_)|PCIList], MemoryRanges) :-
    not_overlap_memory_ranges(PCIList, MemoryRanges).
not_overlap_memory_ranges([H|PCIList], MemoryRanges) :-
    ( foreach(range(RBase,RSize),MemoryRanges),
      param(H)
      do
      buselement(device,_,_,Base,_,Size,_,_,_,_) = H,
      append([Base],[RBase],Bases),
      append([Size],[RSize],Sizes),
      disjunctive(Bases,Sizes)
    ),
    not_overlap_memory_ranges(PCIList, MemoryRanges).


keep_orig_addr([], _, _, _, _, _, _).
keep_orig_addr([H|Buselements], Class, SubClass, ProgIf, Bus, Dev, Fun) :-
    ( buselement(device,addr(Bus,Dev,Fun),BAR,Base,_,_,_,_,_,_) = H,
      device(_,addr(Bus,Dev,Fun),_,_,Class, SubClass, ProgIf,_),
      bar(addr(Bus,Dev,Fun),BAR,OrigBase,_,_,_,_) ->
       T1 is OrigBase / 4096,
       floor(T1,T2),
       integer(T2,KeepBase),
        Base #= KeepBase
        ;
        true
    ),
    keep_orig_addr(Buselements, Class, SubClass, ProgIf, Bus, Dev, Fun).

% on some machines (sbrinz1) one of the two IOAPICs appears as a BAR
% on a device which claims to be a RAM memory controller. If this occurs,
% we want to avoid moving this BAR as otherwise the IOAPIC cannot be reached
% anymore.
keep_ioapic_bars(_, []).
keep_ioapic_bars(Buselements, [H|IOAPICList]) :-
    (
    range(B, _) = H,
    bar(addr(Bus,Dev,Fun),_,OrigBase,_,_,_,_),
    T1 is OrigBase / 4096,
    floor(T1,T2),
    integer(T2,KeepBase),
    KeepBase =:= B ->
    keep_orig_addr(Buselements, _, _, _, Bus, Dev, Fun);
    true
    ),
    keep_ioapic_bars(Buselements, IOAPICList).


:-dynamic(root/4).
:-dynamic(mem/2).
:-dynamic(childbus/2).

setup_test(Tree) :-
    ["clean_facts/babybel1.pl"],
    Granularity is 4096,
    Addr = addr(0, 0, 0),
    rootbridge(Addr, childbus(MinBus, MaxBus), _),
    merge_address_windows(Addr, Ranges),
    get_address_window(Addr, L, H),
    LT1 is L / Granularity,
    ceiling(LT1, LT2),
    integer(LT2, LP),
    HT1 is H / Granularity,
    ceiling(HT1, HT2),
    integer(HT2, HP),

    Tmp is 4294963200 / Granularity,
    ceiling(Tmp, Tmp2),
    integer(Tmp2, HMem2),

    Root = root(Addr, childbus(MinBus, MaxBus), mem(LP, HP), Ranges),

    % prefetchable (Shifts addresses above 10'000'000 to 0)
    constrain_bus(Granularity, mem, prefetchable, Addr,MinBus,MaxBus,LP,HP,BusElementListP, Ranges),
    RBaseP::[LP..HP],
    RHighP::[LP..HP],
    Size is HP - LP,
    RSizeP::[0..Size],
    devicetree(BusElementListP,buselement(bridge,Addr,secondary(MinBus),RBaseP,RHighP,RSizeP,mem, prefetchable, _, _),Tree),
    assign_addresses(_, Root, Tree, Granularity, [], [], HMem2).

test_tree_shift :-  
    setup_test(Tree),
    tree2list(Tree, Lista),
    writeln(Lista),
    devicetree(Lista, Tree2),
    writeln(Tree2).
    
    

          
