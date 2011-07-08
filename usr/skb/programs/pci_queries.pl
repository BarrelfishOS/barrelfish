%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2009, 2011, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% query the current physical address of a BAR of a given device
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pci_physical_address(Bus,Dev,Fun,Vendor,DeviceID,Class,SubClass,ProgIf,BAR,Base,High,Size) :-
    device(_,addr(Bus,Dev,Fun),Vendor,DeviceID, Class, SubClass, ProgIf,_),
    currentbar(addr(Bus,Dev,Fun),BAR,Base,High,Size).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get the list of implemented BARs for a given device
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pci_get_implemented_bars(Bus,Device,Function,BAR, BARList) :-
    findall(BAR, bar(addr(Bus,Device,Function),BAR,_,_,_,_,_), BARList).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get the current physical address of all implemented BARs of the given device
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pci_get_implemented_BAR_addresses(Bus,Dev,Fun,Vendor,DeviceID,Class,SubClass,ProgIf,BARAddrList) :-
    findall(baraddr(BAR,Base,High,Size),
            pci_physical_address(Bus,Dev,Fun,Vendor,DeviceID,Class,SubClass,ProgIf,BAR,Base,High,Size),
            BARAddrListUnsorted),
            % need to pass the BAR addresses in increasing BAR number/index,
            % because the drivers expect them in this order. A explicit
            % number transferred over the IDC msg mybe be better.
            sort(1, =<, BARAddrListUnsorted, BARAddrList).
