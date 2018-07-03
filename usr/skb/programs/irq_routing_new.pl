% this file is prolog
% It contains special comments 
%>> GENERIC
:- lib(ic).
:- lib(lists).

:- [objects3].

% The mapf predicate describes a single mapping entry of a controller
% mapf(ControllerLabel, InPort, InMsg, OutPort, OutMsg)
% Example:
% mapf(msi_a, 100, nullMsg, 11, mem_write(110, 230))
:- dynamic(mapf/5).        

% The controller predicate describes an instance of a controller
% See below for details.
% controller(ControllerLabel, ControllerClass, InputRange, OutputRange)
:- dynamic(controller/4).

% The int_dest_used predicate describes which interrupt sinks are currently
% in use.
% int_dest_used(CpuNumber,VectorNumber).
:- dynamic(int_dest_used/2). 

% This is also defined in queries.pl
:- dynamic(interrupt_override/4). 


%>> X86
% X86 specific. To find an IOAPIC given a GSI, the system must know 
% which GSI base number belongs to a.
% ioapic_gsi_base(Label, Base)
:- dynamic(ioapic_gsi_base/2).

% Link the PCI controller label with an addr(...)
:- dynamic(pci_lbl_addr/2).

% X86 int model with one argument with a single atom that indicates
% the system is using one of the interrupt models.
% atoms currently used is 
% pic, apic (pic and apic at the same time are not possible)
% iommu, x2_apic_opt_out
%:- dynamic(x86_interrupt_model/1).
%


% X86 specific. irte index links the Index used in the dmar_* predicates
% to the corresponding irte and iommu controller label.
% Example: irte_index(0, irte_a, iommu_a).
:- dynamic(irte_index/3).
:- dynamic(dmar/1).
:- dynamic(dmar_device/5).
% PCI specific, Links an ACPI PCI LNK name to a controller label
% Example pcilnk_index("\\_SB_.GSIE", pcilnk_a)
:- dynamic(pcilnk_index/2).
%>> X86



%>> GENERIC
% Documentation
% =============
 
%% Examples:
% Checking if a single mapping is valid for a given controller: 
% mapf_valid(msi_a, 100, nullMsg, 11, mem_write(110, 230)).    --> true
% assert(mapf(msi_a, 100, nullMsg, 11, mem_write(110, 230))).   --> true
% mapf_valid(msi_a, 101, nullMsg, 11, mem_write(110, 231)).     --> true
% mapf_valid(msi_a, 101, nullMsg, 11, mem_write(110, 230)).     --> false

% Example 1: Calculate an arbitrary destination for interrupt input 100.
% route(100, nullMsg, Port, Msg, Li), term_variables(Li,List), labeling(List).

% Example 2: Calculate a from interrupt input 100 to core=3
% Port=3, route(100, nullMsg, Port, Msg, Li), term_variables(Li,List), labeling(List).

% Example 3: Calculate, install and print the route
% find_and_add_irq_route(100, 1).
%
% Example 4: Remove the route installed in Ex3
% get_route(100,nullMsg,Li), remove_route(Li).


%% Controller class
% There are class labels that describe a controller class, such as 
% pic, msi, msix. Each controller class consists of the following predicates.
% It must take care of ensuring the OutMsg has the right format, either a 
% IC constraint must be applied (ieL OutMsg :: [1..10]) or it must be unified to a 
% mem_write(_,_) term (ie: OutMsg = mem_write(A,B)).

% mapf_valid_class(controller_class, controller_label, InPort, nullMsg, OutPort, OutMsg)

%% Controller 
% Each controller gets a label (an atom, ie pic_a, belongs to a given
% class (atom, such as pic), has a InPort range and a OutPort range.
% controller_class(ControllerLabel, ControllerClass, InPortRange, OutPortRange).
% controller_class(pic_a, pic, [0 .. 8], [100]).
% This checks for a concrete instance if a mapping is legal.
% mapf_valid(controller_label, InPort, nullMsg, OutPort, OutMsg)
% Examples:
% controller(pic_a, pic, [100 .. 116], [1]).
% controller(msi_a, msi, [200 .. 204], [300]).
% controller(msix_a, msix, [400 .. 416], [300]).
% controller(msireceiver_a, msireceiver, [300], [1 .. 48]).
% controller(irte_a, irte, [300], [301]).
% controller(iommu_a, iommu, [301], [1 .. 48]).

%% Words
% It is not possible to directly set constraints on bit fields in IC
% constrained integers.
% Example how to use words:
% Constrain bits 20 to 21 to contain the value 2:
% assert_word(W,32), subword(W, Sw, 20..21), word_to_num(Sw,2).


% Utility predicates
% ==================
% last element of list (from lib listut)
last(Last, [Last]) :- !.
last(Last, [_|List]) :-
        last(Last, List).

% This predicate gets the upper bound of a range
get_max_range(Range, Hi) :-
    X :: Range,
    get_max(X, Hi).

% This predicate gets the upper bound of a range
get_min_range(Range, Lo) :-
    X :: Range,
    get_min(X, Lo).

% Constrains word to be a N bit word
assert_word(W, N) :-
    dim(W,[N]), W :: [0 .. 1].

% This beauty converts words (array of bit values) to a numeric representation.
word_to_num(W, Num) :-
    dim(W, [Len]),
    (for(I,1,Len), fromto(0,In,Out,NumT), param(W) do
        Out = W[I] * 2^(I-1) + In),
    Num $= eval(NumT).

% A part of word is etracted into Subword, Range specifies
subword(Word,Subword, Range) :- 
    SW is Word[Range],
    array_list(Subword,SW).



        

%>> ARM
% Controller constraints

mapf_valid_class(gicv2, CtrlLabel, InPort, InMsg, OutPort, OutMsg) :-
    OutMsg = InMsg.

%>> X86
% Controller constraints
% ======================

% The PCI controller simply forwards (or discards) nullMsg
mapf_valid_class(pci, _, _, nullMsg, _, nullMsg) :-
    true.

%% A MSI controller must output to the same port, and there must be consecutive
%% data words for all inputs.
mapf_valid_class(msi, CtrlLabel, InPort, nullMsg, _, mem_write(OutAddr, OutData)) :-
    % If there is a mapf for this controller, same OutPort and 
    % Consecutive data words
    (mapf(CtrlLabel, EInPort, _, _, mem_write(EOutAddr, EOutData) )
    -> ((EInPort - InPort) $= (EOutData - OutData), OutAddr = EOutAddr) ; true).

% A MSIx controller must output a mem_write, otherwise no constraints.
% For every source of MSIx interrupts, one of this controllers
% should be instantiated (ie one for each PCI function)
mapf_valid_class(msix, _, _, _, _, mem_write(_, _)) :-
    true.

% This one enables the MSI-X in the PCI conf header. Can only forward interrupts without
% remap.
mapf_valid_class(pci_msix, CtrlLabel, InPort, Msg, OutPort, Msg) :-
    controller(CtrlLabel, _, InRange, OutRange),
    get_min_range(InRange, InLo),
    get_min_range(OutRange, OutLo),
    (InPort - InLo) $= (OutPort - OutLo).


% In systems without an IOMMU, the translation from memory
% writes to CPU interrupts happens in the northbridge?
% We model this (fixed) translation here.
% The vector is encoded in the lower 8 bit of the data word,
% The destination is encoded in the 12..19 bits of the address. 
mapf_valid_class(msireceiver, _, _, mem_write(Addr, Data), OutPort, OutMsg) :-
    int_dest_port(OutPort),

    % This forces the upper bits to be 0, which must not be true. See Vol. 3A 10-35
    Data :: [ 16'10 .. 16'FE ],
    Data = OutMsg,
    Addr :: [ 16'FEE00000 .. 16'FEE0FFFF],
    assert_word(AddrW, 32),
    word_to_num(AddrW, Addr),
    subword(AddrW,DestW, 13 .. 20), % The range is 1 based equals to 0 based 12 .. 19
    word_to_num(DestW, OutPort).
    

% The PIC has a fixed function. The lowest input port number must map
% to vector 32 and consecutive
mapf_valid_class(pic, CtrlLabel, InPort, nullMsg, _, OutMsg) :-
    controller(CtrlLabel, _, InRange, _),
    get_min_range(InRange, Xlo),
    OutMsg $= InPort - Xlo + 32.

% The IRTE mapper has a fixed function.
mapf_valid_class(irte, _, _, mem_write(InAddr,InData), _, OutMsg) :-
    InAddr :: [16'0FEE0000 .. 16'0FEEFFFF],
    % Actually the InData is not constrained to 0..2^16, but only the 
    % lower 16 bits are considered for the index calculation.
    InData :: [0 .. 2^16],
    InAddrLo $= InAddr - 16'0FEE0000, %No bitwise operations with IC lib.
    OutMsg $= InAddrLo + InData.

% The mem write gets captured by the irte, hence the iommu is constrained by the number of slots.
mapf_valid_class(iommu, _, _, InMsg, _, _) :-   
    InMsg :: [ 0 .. 2 ^ 16].

% IOAPIC that is directly connected to CPU
mapf_valid_class(ioapic, _, _, _, _, OutMsg) :-
    OutMsg :: [ 32 .. 255 ].

% The pci link device can install any mapping between
% its in- and outport
mapf_valid_class(pcilnk, _, _, nullMsg, _, nullMsg) :-
    true.

% IOAPIC that is connected to the iommu.
% The fields of the ioredtbl are interpreted differently, depending on
% the presence of an iommu.
mapf_valid_class(ioapic_iommu, _, _, _, _, OutMsg) :-
    OutMsg :: [ 0 .. 2^16 ].

%>> GENERIC
input_to_int_tuple((InPort,InMsg), OutInt) :- input_to_int(InPort, InMsg, OutInt).

% delay if there is any variable in InPort or InMsg.
delay input_to_int(InPort, InMsg, OutInt) if var(InPort); var(InMsg); (InMsg = mem_write(A,B), (var(A) ; var(B))).
input_to_int(InPort, InMsg, OutInt) :-
    (InMsg = nullMsg) -> OutInt is InPort ;
    (InMsg = mem_write(A,B)) -> OutInt is A * 1024 + B ;
    OutInt is InPort + InMsg * 1024.
    
% The mapf validity check function for a controller instance.
% It takes into account static constraints from the controller class
% but also the runtime configuration. It will not allow to overrite
% an already installed mapping.
mapf_valid(ControllerLabel, InPort, Msg, OutPort, OutMsg) :-
    % Constraint In and OutRange.
    controller(ControllerLabel, _, InRange, OutRange),
    InPort :: InRange,
    OutPort :: OutRange,

    % Check class constraints.
    controller(ControllerLabel, ControllerClass, _, _),
    mapf_valid_class(ControllerClass, ControllerLabel, InPort, Msg, OutPort, OutMsg),

    ((findall((YIn,YMsg), mapf(ControllerLabel, YIn, YMsg, _, _), Li),
    maplist(input_to_int_tuple, Li, Li2),
    input_to_int(InPort, Msg, InInt),
    append(Li2, [InInt], Li3),
    flatten(Li3, Li4),
    alldifferent(Li4)
    )).

    % ; (   % This section is commented out. It would allow to reuse an existing mapping,
    %       % but as the int dest are not used, this makes not sense yet.
    %% Option 2: Re-use an installed mapping.
    %    mapf(ControllerLabel, InPort, Msg, OutPort, OutMsg)
    %)),



% Locate a controller given an inport
find_controller(InPort, CtrlLabel) :-
    controller(CtrlLabel, _, InRange, _),
    InPort :: InRange.

% Makes sure int_dest_used(OutPort,OutMsg) are unique.
int_dest_unique(OutPort, OutMsg) :-
    findall(X, int_dest_used(OutPort,X), Li),
    append(Li, [OutMsg], Li2),
    alldifferent(Li2).
    

% Try to find a route (without installing the mapping) between InPort and OutPort and OutMsg
route(InPort, InMsg, OutPort, OutMsg, List) :-
    find_controller(InPort, CtrlLabel),
    mapf_valid(CtrlLabel, InPort, InMsg, NOutPort, NOutMsg),
    (
        (int_dest_port(NOutPort),NOutPort = OutPort)  ->
            (
             NOutMsg = OutMsg,
             int_dest_msg(OutMsg), 
             int_dest_unique(OutPort, OutMsg),
             List = [mapf(CtrlLabel, InPort, InMsg, NOutPort, NOutMsg)]
            ) ; (
                route(NOutPort, NOutMsg, OutPort, OutMsg, NList),
                append([mapf(CtrlLabel, InPort, InMsg, NOutPort, NOutMsg)], NList, List)
            )
    ).
    

% Translate the port indices of a mapf entry to zero based entries
to_mapf_local(mapf(Lbl,A,B,C,D), Out) :-
    controller(Lbl, _, InRange, OutRange),
    get_min_range(InRange, InLo),
    get_min_range(OutRange, OutLo),
    AOut #= A - InLo,
    COut #= C - OutLo,
    Out = mapf(Lbl,AOut,B,COut,D).
    
    

% Given a list of mapf entries, it adds the terms to the database,
% effectively installing the route. Also assert int_dest_used for the
% last entry.
add_route(Li) :-
    (foreach(L, Li) do
        assert(L)),
    last(mapf(_,_,_,LastPort,LastMsg), Li),
    assert(int_dest_used(LastPort,LastMsg)).

% Removes a route. The opposite of add_route. To 
% get a route, use get_route first.
remove_route(Li) :-
    (foreach(L, Li) do
        retract(L)),
    last(mapf(_,_,_,LastPort,LastMsg), Li),
    retract(int_dest_used(LastPort,LastMsg)).



% Gets a list with consecutive mapf entries for a installed route
% Same format as produced from route(..).
get_route(InPort, InMsg, Li) :-
    mapf(CtrlLbl, InPort, InMsg, NextPort, NextMsg) ->
        ( get_route(NextPort, NextMsg, NLi),
          append([mapf(CtrlLbl, InPort, InMsg, NextPort, NextMsg)], NLi, Li))
        ; ( Li = []).   
    

% Given a list of mapf entries, print parseable list of entries.
% All port numbers are  0 based with respect to the controller in- resp. out-port range.
% The ouput is multiple lines, each describing the mapping for one controller.
% The format of a line is:
% controllerlabel,inport,inmessage,outport,outmessage
% Except the last line, that denotes the vector triggered at the cpu, the line
% follows the format
% cpu,cpuid,vector
print_route(Li) :-
    (foreach(mapf(Lbl,A,B,C,D), Li) do (
        to_mapf_local(mapf(Lbl,A,B,C,D), mapf(_,ALocal,_,CLocal,_)),
        controller(Lbl, Class, _,_),
        printf(output, "%s,%s,%d,%Kw,%d,%Kw\n",[Lbl,Class,ALocal,B,CLocal,D]))).
    %% This adds an extra line indicating dest cpu and vector
    %last(mapf(Lbl,A,B,C,D), Li),
    %to_mapf_local(mapf(Lbl,A,B,C,D), mapf(_,_,_,CLocal,_)),
    %printf(output, "cpu,%d,%d\n", [CLocal,D]).


% Returns true if this PortNumber is a interrupt destination.
% We use the Barrelfish CoreId as identifier
% TODO: Upper bound of CoreId shouldnt be hardcoded (here). 
int_dest_port(Port) :-
    %corename(Port, _, _). % Does not work bc synchronization issues
    Port :: [0 .. 1000],
    labeling([Port]).

% Returns a list of integers representing the int destinations
int_dest_port_list(Li) :-
    findall(X, int_dest_port(X), Li).
    
%>> X86
% Returns true if this Msg is acceptable as being delivered to a destination
int_dest_msg(Msg) :-
    Msg :: [32 .. 255].
    

% Scans all controller and sinkPortRange and returns the maximum used port number
max_used_port(OutMax) :-
    findall(Ri, controller(_,_,Ri,_), RangeList1),
    findall(Ri, controller(_,_,_,Ri), RangeList2),
    append(RangeList1, RangeList2, RangeList),
    maplist(get_max_range, RangeList, MaxList2),
    % Due to synchronization issues, we cant use the following two lines
    %findall(Ri, int_dest_port(Ri), MaxList1), 
    %append(MaxList1, MaxList2, MaxList),
    append([1000], MaxList2, MaxList),
    ic:max(MaxList, OutMax).
    

% Prints mapf routing entries for a given controller.
print_controller_config(CtrlLbl) :-
    findall( mapf(CtrlLbl, A, B, C, D), mapf(CtrlLbl, A, B, C, D), ML),
    printf(output, "Printing controller config for %s\n", [CtrlLbl]),
    (foreach(mapf(Lbl,A,B,C,D), ML) do (
        to_mapf_local(mapf(Lbl,A,B,C,D), mapf(_,ALocal,_,CLocal,_)),
        printf("%d,%Kw,%d,%Kw\n", [ALocal,B,CLocal,D]))).


get_unused_range(Size, RangeOut) :-
    max_used_port(MaxPort),
    Lo is MaxPort + 1,
    Hi is MaxPort + Size,
    RangeOut = [Lo .. Hi].

% Helper for maplist...
sub_rev(A,B,C) :- C is B-A.
    

%% Front-end functions
% Routes the IntNr to a unused vector on CpuNr.
% Prints controller configurations according to print_route
find_and_add_irq_route(IntNr, CpuNr, VecNr) :-
    route(IntNr, nullMsg, CpuNr, VecNr, Li),
    term_variables(Li,List),
    labeling(List),
    add_route(Li),
    print_route(Li).

%>> X86
x86_iommu_mode :-
    dmar(X), Y is 1 /\ X, Y = 1.


% Sets up default X86 controllers.
% It uses facts that are added by the acpi and pci discovery, hence
% it must be run when these facts are added.
add_x86_controllers :-
    % pic + iommu is not a valid combination...
    (x86_interrupt_model(apic) ; not(x86_iommu_mode)),
    int_dest_port_list(CpuPorts),

    % iommu or msireceiver
    (x86_iommu_mode -> (
        % Then instantiate iommus.
        findall(X, dmar_hardware_unit(X, _, _, _), Li),
        (foreach(X, Li) do add_iommu_controller(_, X) )
    ) ; (
        % instantiate a MSI receiver
        get_unused_range(1, MsiInRange),
        assert_controller(msireceiver_a, msireceiver, MsiInRange, CpuPorts)
    )),

    % ioapic -> We assert them directly from the controller driver

    %%findall((Id,GsiBase), ioapic(Id,_, GsiBase), IoapicLi),
    %%(foreach((Id,GsiBase), IoapicLi) do
    %%    add_ioapic_controller(_, Id, GsiBase)
    %%),
    (not(x86_interrupt_model(apic)) -> (
        get_min_range(CpuPorts, MinCpu),
        get_unused_range(16, PicInRange),
        assert_controller(pic_a, pic, PicInRange, [MinCpu])
    ) ; true).

    % PIR to pcilnk controllers
    % This is now added on the fly when all pir facts
    % become available. ACPI calls
    % add_pcilnk_controller_by_name(Name, Lbl)
    % after adding all relevant PIR facts.
    %findall(Name, pir(Name, _),Li),
    %sort(Li,LiUnique),
    %(foreach(Name,LiUnique) do (
    %    findall(Gsi, pir(Name, Gsi), GSIListT),
    %    sort(GSIListT,GSIList),
    %    add_pcilnk_controller(GSIList, Name, _)
    %)).



%>> GENERIC

% Base is atom, index integer, Out is atom that is not yet in use in
% any controller predicate.
get_unused_controller_label(Base, Index, Out) :-
    atom_string(Base, BaseS),
    append_strings(BaseS, "_", BaseSx),
    number_string(Index, IndexS),
    append_strings(BaseSx, IndexS, R),
    atom_string(Xn, R),
    (not(controller(Xn, _,_,_)) , Out = Xn) ;
    ( NIndex is Index + 1, get_unused_controller_label(Base,NIndex, Out)).
    
% Some classes of controller can be added at runtime. The 
% InSize specifies the number of input ports the controller has.
% Type should be an atom, one of:
% msi, msix, pcilnk

%>> X86
add_controller(InSize, msi, Lbl) :-
    InSize :: [1,2,4,8,16],
    get_unused_range(InSize, InRange),
    get_unused_controller_label(msi, 0, Lbl),
    (controller(_, irte, MsiOut, _) ; controller(_, msireceiver, MsiOut, _)),
    assert_controller(Lbl, msi, InRange, MsiOut).

% Deprecated, use add_pci_msix_controller
%%add_controller(InSize, msix, Lbl) :-
%%    InSize :: [1 .. 1024],
%%    get_unused_range(InSize, InRange),
%%    get_unused_controller_label(msix, 0, Lbl),
%%    (controller(_, irte, MsiOut, _) ; controller(_, msireceiver, MsiOut, _)),
%%    assert_controller(Lbl, msix, InRange, MsiOut).


% Get the DmarIndex for a PCI address. 
% It checks for a direct entry, and if not, recurses up the PCI hierarchy
% to find a bus entry.
% DmarIndex = The DmarIndex, this is the output of the relation
% EntryType = 1 for the first invocation, 2 for the recursive calls.
% Add = The pci address to be looked up.
dmar_device_pci(DmarIndex, EntryType, addr(Bus,Device, Function)) :-
    % Check if there is a device specific entry.
    dmar_device(DmarIndex, 0, EntryType, addr(_, Bus, Device, Function), _)
    ; (
        bridge(_,NewAddr, _, _, _, _, _, secondary(Bus)),
        dmar_device_pci(DmarIndex, 2, NewAddr)
    ).


% Adds a MSI controller. It needs the Addr to find the correct I/OMMU
add_msi_controller(Lbl, InSize, Type, addr(Bus, Device, Function)) :-
    (Type = msi ; Type = msix),
    % First Check if there is an endpoint device
    (x86_iommu_mode -> (
        dmar_device(DmarIndex, 1, addr(Bus,Device,Function)),
        irte_index(DmarIndex, IrteLbl, _),
        controller(IrteLbl, _, MSIOutRange, _)) ;
        controller(_, msireceiver, MSIOutRange, _)
    ),
    get_unused_range(InSize, InRange),
    get_unused_controller_label(Type, 0, Lbl),
    assert_controller(Lbl, Type, InRange, MSIOutRange).


%%%% Functions that map various interrupt numbers to internal representation

% Convert ACPI PIR name to a internal base number.
prt_entry_to_num(pir(Name), Nu) :-
    pcilnk_index(Name, Lbl),
    controller(Lbl, _, InRange, _),
    get_min_range(InRange, Nu).

% Convert GSI to internal number
prt_entry_to_num(gsi(Gsi), Nu) :-
    controller_for_gsi(Gsi, Lbl, Base),
    Offset is Gsi - Base,
    controller(Lbl, _, InRange, _),
    get_min_range(InRange, InLo),
    Nu is InLo + Offset.

%>> GENERIC
% Filter none atoms out of a list.
filter_none([], []).
filter_none([none | Xs], Out) :- filter_none(Xs, Out).
filter_none([A | Xs], Out) :- filter_none(Xs, OutT), Out = [A | OutT].

%>> X86
% Calculate the PCI bus swizzle until a PRT entry is found
% same algorithm as findgsi in the old irq_routing.pl
find_prt_entry(Pin, Addr, PrtEntry) :-
    (
        % lookup routing table to see if we have an entry
        prt(Addr, Pin, PrtEntry)
    ;
        % if not, compute standard swizzle through bridge
        Addr = addr(Bus, Device, _),
        NewPin is (Device + Pin) mod 4,

        % recurse, looking up mapping for the bridge itself
        bridge(_, BridgeAddr, _, _, _, _, _, secondary(Bus)),
        find_prt_entry(NewPin, BridgeAddr, PrtEntry)
    ).


% when using legacy interrupts, the PCI card does not need a controller
% however it needs to be aware of the int numbers to use.
% This function returns always only one interrupt, but for compatibility,
% it returns a Lo,Hi tuple
% A = addr(Bus,Device,Function)
% LiR = (Lo,Hi)
get_pci_legacy_int_range(A, (Lo,Hi)) :-
    device(_, A, _, _, _, _, _, Pin),
    find_prt_entry(Pin, A, X),
    prt_entry_to_num(X, IntNu),
    Lo = IntNu,
    Hi = IntNu.

% Translates fixed x86 legacy interrupt numbers to internal interrupt source number.
% It first translates Legacy to GSI, then GSI to internal.
% The first translation involves the ACPI int override table
isa_irq_to_int(Legacy, Nr) :-
    (interrupt_override(0, Legacy, Gsi, _) ; Gsi = Legacy),
    prt_entry_to_num(gsi(Gsi), Nr).


    
%>> GENERIC
% Add a dynamic controller and a octopus object
assert_controller(Lbl, Class, InRange, MSIOutRange) :-
    assert( controller(Lbl, Class, InRange, MSIOutRange)),
    atom_string(Lbl,LblStr),
    atom_string(Class, ClassStr),
    add_seq_object('hw.int.controller.',
        [val(label, LblStr), val(class, ClassStr)], []).

% GSIList is a list of GSI that this pci link device can output. 
add_pcilnk_controller(GSIList, Name, Lbl) :-
    length(GSIList, LiLe),
    get_unused_range(LiLe, InRange),
    get_unused_controller_label(pcilnk, 0, Lbl),

    % Calculate OutRange. Subtract GSI Base, then add the minimum of the IoApic ctrl.
    GSIList = [GSI0 | _],
    !,
    controller_for_gsi(GSI0, Ioapiclbl, GSIBase),
    controller(Ioapiclbl, _, IoIn, _),
    get_min_range(IoIn, IoApicIn),
    maplist(sub_rev(GSIBase), GSIList, LocalList), maplist(+(IoApicIn), LocalList, OutRange),
    assert(pcilnk_index(Name, Lbl)),
    assert_controller(Lbl, pcilnk, InRange, OutRange).

%>> X86
% For a given (ACPI) pci link controller name, this looks
% up all the GSI from the pir(..) facts and instantiates the controller
add_pcilnk_controller_by_name(Name, Lbl) :-
    findall(Gsi, pir(Name, Gsi), GSIListT),
    sort(GSIListT,GSIList),
    add_pcilnk_controller(GSIList, Name, Lbl).


% The PCI controller can not route interrupts to different destinations,
% however, it will enable/disable interrupts in the PCI conf space. This controller
% is only instantiated for legacy interrupts.
% A = addr(Bus,Device,Function)
add_pci_controller(Lbl, A) :-
    % get fresh inputs 
    get_unused_range(1, PciInRange),
    get_unused_controller_label(pci, 0, Lbl),

    % now, determine the output range, using the pci address
    device(_, A, _, _, _, _, _, Pin),
    find_prt_entry(Pin, A, X),
    prt_entry_to_num(X, IntNu),
    OutRange :: [IntNu, IntNu],

    assert(pci_lbl_addr(Lbl, A)),

    assert_controller(Lbl, pci, PciInRange, OutRange).

% Instantiates two linke controllers:
% * pci_msix: can not route interrupts to different destinations,
%   however, it will enable/disable interrupts in the PCI conf space.
% * msix: The MSIX vector table 
%
% A = addr(Bus,Device,Function)
add_pci_msix_controller(PciMsixLbl, MsixLbl, A) :-
    TBLSIZE = 16, % TODO read TBLSIZE from conf space and pass in as argument
    % First, build msix controller
    get_unused_range(TBLSIZE, PciOutRange),
    get_unused_controller_label(msix, 0, MsixLbl),
    (controller(_, irte, MsiOut, _) ; controller(_, msireceiver, MsiOut, _)),
    assert_controller(MsixLbl, msix, PciOutRange, MsiOut),

    % Then pci_msix controller, that sits before the msix ctrl
    get_unused_range(TBLSIZE, InRange),
    get_unused_controller_label(pci_msix, 0, PciMsixLbl),
    assert(pci_lbl_addr(PciMsixLbl, A)),
    assert_controller(PciMsixLbl, pci_msix, InRange, PciOutRange).



    

add_ioapic_controller(Lbl, IoApicId, GSIBase) :-
    ((
        % Check if there is a dmar_hardware_unit entry that covers this controller
        % If so, we instantiate a ioapic_iommu controller
        % that is connected directly to the iommu
        % (not the irte), because the ioapic driver knows
        % how to address an entry directly
        dmar_device(DmarIndex, _, 3, _, IoApicId), 
        irte_index(DmarIndex, _, CtrlLbl),
        controller(CtrlLbl, _, OutRange, _), % OutRange is the Input Range of the ioapic
        CtrlClass = ioapic_iommu
    ) ; (
        % No IOMMU applicable
        int_dest_port_list(OutRange),
        CtrlClass = ioapic
    )),
    get_unused_range(24, IoApicInRange),
    get_unused_controller_label(ioapic, 0, Lbl),
    assert_controller(Lbl, CtrlClass, IoApicInRange, OutRange),
    assert( ioapic_gsi_base(Lbl, GSIBase) ).

add_iommu_controller(Lbl, DmarIndex) :-
    int_dest_port_list(CpuPorts),
    max_used_port(MaxPort),
    Lo1 is MaxPort + 1,
    Hi1 is MaxPort + 1,
    Lo2 is MaxPort + 2,
    Hi2 is MaxPort + 2,
    IrteOutRange = [Lo1 .. Hi1],
    IommuInRange = [Lo2 .. Hi2],
    get_unused_controller_label(iommu, 0, IommuLbl),
    get_unused_controller_label(irte, 0, IrteLbl),

    assert_controller(IommuLbl, iommu, IrteOutRange, CpuPorts),
    assert( irte_index(DmarIndex, IrteLbl, IommuLbl) ),
    assert_controller(IrteLbl, irte, IommuInRange, IrteOutRange),
    Lbl = IrteLbl.


% iommu
print_controller_class_details(Lbl, iommu) :-
    irte_index(DmarIndex, _, Lbl),
    dmar_hardware_unit(DmarIndex, Flags, Segment, Address),
    printf(",%u,%u,%u", [Flags,Segment,Address]).
     
% ioapic
print_controller_class_details(Lbl, ioapic) :-
    ioapic_gsi_base(Lbl, GSIBase),
    ioapic(_, MemBase, GSIBase), 
    printf(",%u", [MemBase]).

% ioapic-iommu
print_controller_class_details(Lbl, ioapic_iommu) :-
    print_controller_class_details(Lbl, ioapic).

% Default, print nothing
print_controller_class_details(_, _) :- true.

%>> GENERIC
% This predicate indicates which binary to start for a given controller class
% If there is no such binary, no driver is started
% controller_driver_binary(ioapic, "ioapic").
% controller_driver_binary(ioapic_iommu, "ioapic").
% controller_driver_binary(iommu, "iommu").

find_int_controller_driver(Lbl) :-
    controller(Lbl, Class, InRange, OutRange),
    %Binary = "None",
    controller_driver_binary(Class, Binary),
    get_min_range(InRange,InLo),
    get_max_range(InRange,InHi),
    get_min_range(OutRange,OutLo),
    get_max_range(OutRange,OutHi),

    printf("%s,%w,%w,%u,%u,%u,%u", [Binary,Lbl, Class, InLo, InHi, OutLo, OutHi]),
    print_controller_class_details(Lbl, Class),
    printf("\n",[]).

% This function prints a CSV file in the following format:
% Lbl,Class,InRangeLow,InRangeHigh,OutRangeLow,OutRangeHigh
% followed by controller specific details needed for controller
% driver startup (such as a MMIO base address for the IOMMU)
print_int_controller(Lbl) :-
    controller(Lbl, Class, InRange, OutRange),
    get_min_range(InRange,InLo),
    get_max_range(InRange,InHi),
    get_min_range(OutRange,OutLo),
    get_max_range(OutRange,OutHi),
    printf("%w,%w,%u,%u,%u,%u", [Lbl, Class, InLo, InHi, OutLo, OutHi]),
    print_controller_class_details(Lbl, Class),
    printf("\n",[]).

print_controller_driver :-
    findall(Lbl, controller(Lbl, _,_,_), Li),
    (foreach(Lbl,Li) do
        (find_int_controller_driver(Lbl);true)).


%>> X86
% Returns the controller label for a GSI
% Can also be used to map GSI to internal interrupt source number.
controller_for_gsi(GSI, Lbl, Base) :-
    ioapic_gsi_base(Lbl, Base),
    Base > GSI-24,
    Base =< GSI.



%>> GENERIC
print_controller_dot_file_handle(Handle) :-
    printf(Handle, "digraph controllergraph {\n", []),
    findall( controller(CtrlLbl, CtrlClass, In, Out), controller(CtrlLbl, CtrlClass, In, Out), CtrlLi),
    (foreach( controller(CtrlLbl, _, InRange, OutRange), CtrlLi), param(Handle) do (
        % In Connections
        InTemp :: InRange,
        findall( InTemp, labeling([InTemp]), InLi),
        (foreach(Y, InLi), param(CtrlLbl), param(Handle) do (
            mapf(CtrlLbl, Y, _, _, _) -> printf(Handle, "%d -> %Kw [color=blue];\n", [Y, CtrlLbl]) ;
            printf(Handle, "%d -> %Kw;\n", [Y, CtrlLbl])
            
        )),
        OutTemp :: OutRange,
        findall( OutTemp, labeling([OutTemp]), OutLi),
        (foreach(Y, OutLi), param(CtrlLbl), param(Handle) do (
            int_dest_port(Y) -> ( 
                mapf(CtrlLbl, _,_, Y, _) -> printf(Handle, "%Kw -> cpu_%d [color=blue];\n", [CtrlLbl, Y]);
                printf(Handle, "%Kw -> cpu_%d;\n", [CtrlLbl, Y]) 
            ) ; (
                mapf(CtrlLbl, _,_, Y, _) -> printf(Handle, "%Kw -> %d [color=blue];\n", [CtrlLbl, Y]);
                printf(Handle, "%Kw -> %d;\n", [CtrlLbl, Y]) 
            )
        ))
    )),
    printf(Handle, "}\n",[]).

print_controller_dot_file_local :-
    open("/home/luki/ETH/IRQ route/dot-test/out.dot", write, Handle), 
    print_controller_dot_file_handle(Handle),
    close(Handle).

print_controller_dot_file:-
    print_controller_dot_file_handle(stdout).

%%% DEBUG:  Some facts that are helpful for experimentation %%%
%controller(pic_a, pic, [100 .. 116], [1]).
%controller(msi_a, msi, [200 .. 204], [300]).
%controller(msix_a, msix, [400 .. 416], [300]).
%controller(msireceiver_a, msireceiver, [300], [1 .. 48]).
%controller(irte_a, irte, [300], [301]).
%controller(iommu_a, iommu, [301], [1 .. 48]).


%%% DEBUG:  Some facts that are helpful for experimentation %%%
%:- [debugfacts].
%
%:- add_x86_controllers.
%:- add_ioapic_controller(Lbl,0), print_controller(Lbl).
%:- add_ioapic_controller(Lbl,24), print_controller(Lbl).
%:- add_pcilnk_controller([12,13,14,15], Lbl), print_controller(Lbl).
%:- add_pcilnk_controller([16,17,18,19], Lbl), print_controller(Lbl).
%:- add_pcilnk_controller([16,17,18,19], Lbl), print_controller(Lbl).
%:- add_controller(4, msi, Lbl), print_controller(Lbl).
%:- add_controller(5, msix, Lbl), print_controller(Lbl).
%
%% install route going from the first input of the ioapic_0 to the first CPU.
%:- controller(ioapic_0,_,InRange,_), get_min_range(InRange,Lo), int_dest_port_list(PoLi), PoLi = [CPU|_], find_and_add_irq_route(Lo, CPU).
%:- controller(pcilnk_1,_,InRange,_), get_min_range(InRange,Lo), int_dest_port_list(PoLi), PoLi = [CPU|_], find_and_add_irq_route(Lo, CPU).
%:- controller(pcilnk_1,_,InRange,_), get_min_range(InRange,Lo), int_dest_port_list(PoLi), PoLi = [CPU|_], get_route(Lo, nullMsg, Li), print_route(Li).
%:- controller(pcilnk_1,_,InRange,_), get_min_range(InRange,Lo), Lo1 is Lo + 1, int_dest_port_list(PoLi), PoLi = [CPU|_], find_and_add_irq_route(Lo1, CPU).
%:- controller(msi_0,_,InRange,_), get_min_range(InRange,Lo), int_dest_port_list(PoLi), PoLi = [CPU|_], find_and_add_irq_route(Lo, CPU).
%:- controller(msix_0,_,InRange,_), get_min_range(InRange,Lo), int_dest_port_list(PoLi), PoLi = [CPU|_], find_and_add_irq_route(Lo, CPU).
%
%%remove and re-install a route.
%:- controller(msi_0,_,InRange,_), get_min_range(InRange,Lo), get_route(Lo, nullMsg, Li), remove_route(Li).
%:- controller(msi_0,_,InRange,_), get_min_range(InRange,Lo), int_dest_port_list(PoLi), PoLi = [CPU|_], find_and_add_irq_route(Lo, CPU).
%
%:- print_controller_dot_file.
%
%:- print_controller_config(piclnk_1).






% Other useful facts, not defined here but relevant.
% 


