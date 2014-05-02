%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2009, 2011, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-lib(ic).
:-lib(ic_global).
:-use_module(library(ic_edge_finder)).

:-dynamic(currentbar/5).

bridge_programming(Plan, NrElements) :-
    get_devices(Devices),
    convert_devices(Devices,DeviceElements),
    get_bridges(Bridges),
    convert_bridges(Bridges,BridgeElements),
    append(DeviceElements, BridgeElements, Plan),
    length(Plan, NrElements).

% note: device with addr(-1,-1,-1) will be removed because it has no
%       regions -> nuet. The clean way would be to really remove it which leads
%       to other prolog problems... (two solutions).
convert_devices([], []).
convert_devices([buselement(device, Addr ,Regions)|T], L) :-
    subtract(Regions,[nuet],RegionsClean),
    ( foreach(R,RegionsClean),
      foreach(El,Elements),
      param(Addr)
      do
          region(BAR,Base,Bits,Prefetch,Sz,MulL) = R,
          ( MulL = b ->
              Mul is 1
          ;
            MulL = k ->
              Mul is 1024
          ;
            MulL = g ->
              Mul is 1024 * 1024 * 1024
          ;
              Mul is 1024 * 1024
          ),
          Size is Sz * Mul,
          High is Base + Size,
          El = buselement(device, Addr, BAR, Base, High, Size, mem, Prefetch, pcie, Bits),
          assert(bar(Addr,BAR,_,Size,_,_,_))
    ),
    convert_devices(T, L2),
    append(L2, Elements, L).

convert_bridges([], []).
convert_bridges([buselement(bridge, _, _)|T], L) :-
    convert_bridges(T, L).
convert_bridges([buselement(bridge, Addr, S, m(B1,H1), p(B2, H2),_)|T], L) :-
    ( H1 >= B1 ->
        S1 is H1 - B1,
        Bridge1 = [buselement(bridge, Addr, S, B1, H1+1, S1+1, mem, nonprefetchable, pcie, 0)];
        Bridge1 = []
    ),
    ( H2 >= B2 ->
        S2 is H2 - B2,
        Bridge2 = [buselement(bridge, Addr, S, B2, H2+1, S2+1, mem, prefetchable, pcie, 0)];
        Bridge2 = []
    ),
    append(Bridge1,Bridge2,BridgeList),
    convert_bridges(T, L2),
    append(L2, BridgeList, L).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% tools
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


base(buselement(_,_,_,Base,_,_,_,_,_,_),Base).
high(buselement(_,_,_,_,High,_,_,_,_,_),High).
size(buselement(_,_,_,_,_,Size,_,_,_,_),Size).

get_devices(Devices) :-
    Devices = [buselement(device, addr(-1,-1,-1),[
    nuet]),
    buselement(device, addr(16'00,16'00,16'0),[
    nuet]),
    buselement(device, addr(16'00,16'01,16'0),[
    nuet]),
    buselement(device, addr(16'00,16'01,16'1),[
    nuet]),
    buselement(device, addr(16'00,16'02,16'0),[
    nuet]),
    buselement(device, addr(16'00,16'02,16'2),[
    nuet]),
    buselement(device, addr(16'00,16'03,16'0),[
    nuet]),
    buselement(device, addr(16'00,16'04,16'0),[
	    region(0,16'3803fff90000,64,nonprefetchable,16,k),
    nuet]),
    buselement(device, addr(16'00,16'04,16'1),[
	    region(0,16'3803fff80000,64,nonprefetchable,16,k),
    nuet]),
    buselement(device, addr(16'00,16'04,16'2),[
	    region(0,16'3803fff70000,64,nonprefetchable,16,k),
    nuet]),
    buselement(device, addr(16'00,16'04,16'3),[
	    region(0,16'3803fff60000,64,nonprefetchable,16,k),
    nuet]),
    buselement(device, addr(16'00,16'04,16'4),[
	    region(0,16'3803fff50000,64,nonprefetchable,16,k),
    nuet]),
    buselement(device, addr(16'00,16'04,16'5),[
	    region(0,16'3803fff40000,64,nonprefetchable,16,k),
    nuet]),
    buselement(device, addr(16'00,16'04,16'6),[
	    region(0,16'3803fff30000,64,nonprefetchable,16,k),
    nuet]),
    buselement(device, addr(16'00,16'04,16'7),[
	    region(0,16'3803fff20000,64,nonprefetchable,16,k),
    nuet]),
    buselement(device, addr(16'00,16'05,16'0),[
    nuet]),
    buselement(device, addr(16'00,16'05,16'1),[
    nuet]),
    buselement(device, addr(16'00,16'05,16'2),[
    nuet]),
    buselement(device, addr(16'00,16'05,16'4),[
	    region(0,16'd0f60000,32,nonprefetchable,4,k),
    nuet]),
    buselement(device, addr(16'00,16'11,16'0),[
    nuet]),
    buselement(device, addr(16'00,16'16,16'0),[
	    region(0,16'd0f50000,64,nonprefetchable,16,b),
    nuet]),
    buselement(device, addr(16'00,16'16,16'1),[
	    region(0,16'd0f40000,64,nonprefetchable,16,b),
    nuet]),
    buselement(device, addr(16'00,16'1a,16'0),[
	    region(0,16'd0f20000,32,nonprefetchable,1,k),
    nuet]),
    buselement(device, addr(16'00,16'1c,16'0),[
    nuet]),
    buselement(device, addr(16'00,16'1c,16'7),[
    nuet]),
    buselement(device, addr(16'00,16'1d,16'0),[
	    region(0,16'd0f10000,32,nonprefetchable,1,k),
    nuet]),
    buselement(device, addr(16'00,16'1e,16'0),[
    nuet]),
    buselement(device, addr(16'00,16'1f,16'0),[
    nuet]),
    buselement(device, addr(16'00,16'1f,16'2),[
	    region(5,16'd0f00000,32,nonprefetchable,2,k),
    nuet]),
    buselement(device, addr(16'00,16'1f,16'3),[
	    region(0,16'3803fff10000,64,nonprefetchable,256,b),
    nuet]),
    buselement(device, addr(16'02,16'00,16'0),[
	    region(0,16'd0960000,32,nonprefetchable,128,k),
	    region(3,16'd09b0000,32,nonprefetchable,16,k),
    nuet]),
    buselement(device, addr(16'02,16'00,16'1),[
	    region(0,16'd0940000,32,nonprefetchable,128,k),
	    region(3,16'd09a0000,32,nonprefetchable,16,k),
    nuet]),
    buselement(device, addr(16'02,16'00,16'2),[
	    region(0,16'd0920000,32,nonprefetchable,128,k),
	    region(3,16'd0990000,32,nonprefetchable,16,k),
    nuet]),
    buselement(device, addr(16'02,16'00,16'3),[
	    region(0,16'd0900000,32,nonprefetchable,128,k),
	    region(3,16'd0980000,32,nonprefetchable,16,k),
    nuet]),
    buselement(device, addr(16'04,16'00,16'0),[
	    region(0,16'd0e20000,64,nonprefetchable,128,k),
	    region(4,16'd0e50000,64,nonprefetchable,16,k),
    nuet]),
    buselement(device, addr(16'04,16'00,16'1),[
	    region(0,16'd0e00000,64,nonprefetchable,128,k),
	    region(4,16'd0e40000,64,nonprefetchable,16,k),
    nuet]),
    buselement(device, addr(16'06,16'00,16'0),[
	    region(1,16'd0d60000,64,nonprefetchable,16,k),
	    region(3,16'd0d00000,64,nonprefetchable,256,k),
    nuet]),
    buselement(device, addr(16'07,16'00,16'0),[
	    region(0,16'380000000000,64,prefetchable,8,g),
	    region(4,16'd0c00000,64,nonprefetchable,128,k),
    nuet]),
    buselement(device, addr(16'08,16'00,16'0),[
	    region(0,16'380200400000,64,prefetchable,16,k),
	    region(2,16'380200000000,64,prefetchable,4,m),
    nuet]),
    buselement(device, addr(16'08,16'00,16'3),[
	    region(0,16'd0b00000,32,nonprefetchable,4,k),
    nuet]),
    buselement(device, addr(16'0a,16'00,16'0),[
	    region(0,16'ea000000,32,prefetchable,16,m),
	    region(1,16'd0810000,32,nonprefetchable,16,k),
	    region(2,16'd0000000,32,nonprefetchable,8,m),
    nuet]),
    buselement(device, addr(16'7f,16'08,16'0),[
    nuet]),
    buselement(device, addr(16'7f,16'09,16'0),[
    nuet]),
    buselement(device, addr(16'7f,16'0a,16'0),[
    nuet]),
    buselement(device, addr(16'7f,16'0a,16'1),[
    nuet]),
    buselement(device, addr(16'7f,16'0a,16'2),[
    nuet]),
    buselement(device, addr(16'7f,16'0a,16'3),[
    nuet]),
    buselement(device, addr(16'7f,16'0b,16'0),[
    nuet]),
    buselement(device, addr(16'7f,16'0b,16'3),[
    nuet]),
    buselement(device, addr(16'7f,16'0c,16'0),[
    nuet]),
    buselement(device, addr(16'7f,16'0c,16'1),[
    nuet]),
    buselement(device, addr(16'7f,16'0c,16'2),[
    nuet]),
    buselement(device, addr(16'7f,16'0c,16'3),[
    nuet]),
    buselement(device, addr(16'7f,16'0c,16'4),[
    nuet]),
    buselement(device, addr(16'7f,16'0d,16'0),[
    nuet]),
    buselement(device, addr(16'7f,16'0d,16'1),[
    nuet]),
    buselement(device, addr(16'7f,16'0d,16'2),[
    nuet]),
    buselement(device, addr(16'7f,16'0d,16'3),[
    nuet]),
    buselement(device, addr(16'7f,16'0d,16'4),[
    nuet]),
    buselement(device, addr(16'7f,16'0e,16'0),[
    nuet]),
    buselement(device, addr(16'7f,16'0e,16'1),[
    nuet]),
    buselement(device, addr(16'7f,16'0f,16'0),[
    nuet]),
    buselement(device, addr(16'7f,16'0f,16'1),[
    nuet]),
    buselement(device, addr(16'7f,16'0f,16'2),[
    nuet]),
    buselement(device, addr(16'7f,16'0f,16'3),[
    nuet]),
    buselement(device, addr(16'7f,16'0f,16'4),[
    nuet]),
    buselement(device, addr(16'7f,16'0f,16'5),[
    nuet]),
    buselement(device, addr(16'7f,16'10,16'0),[
    nuet]),
    buselement(device, addr(16'7f,16'10,16'1),[
    nuet]),
    buselement(device, addr(16'7f,16'10,16'2),[
    nuet]),
    buselement(device, addr(16'7f,16'10,16'3),[
    nuet]),
    buselement(device, addr(16'7f,16'10,16'4),[
    nuet]),
    buselement(device, addr(16'7f,16'10,16'5),[
    nuet]),
    buselement(device, addr(16'7f,16'10,16'6),[
    nuet]),
    buselement(device, addr(16'7f,16'10,16'7),[
    nuet]),
    buselement(device, addr(16'7f,16'13,16'0),[
    nuet]),
    buselement(device, addr(16'7f,16'13,16'1),[
    nuet]),
    buselement(device, addr(16'7f,16'13,16'4),[
    nuet]),
    buselement(device, addr(16'7f,16'13,16'5),[
    nuet]),
    buselement(device, addr(16'7f,16'13,16'6),[
    nuet]),
    buselement(device, addr(16'7f,16'16,16'0),[
    nuet]),
    buselement(device, addr(16'7f,16'16,16'1),[
    nuet]),
    buselement(device, addr(16'7f,16'16,16'2),[
    nuet]),
    buselement(device, addr(16'80,16'01,16'0),[
    nuet]),
    buselement(device, addr(16'80,16'02,16'0),[
    nuet]),
    buselement(device, addr(16'80,16'03,16'0),[
    nuet]),
    buselement(device, addr(16'80,16'03,16'2),[
    nuet]),
    buselement(device, addr(16'80,16'04,16'0),[
	    region(0,16'3805fff70000,64,nonprefetchable,16,k),
    nuet]),
    buselement(device, addr(16'80,16'04,16'1),[
	    region(0,16'3805fff60000,64,nonprefetchable,16,k),
    nuet]),
    buselement(device, addr(16'80,16'04,16'2),[
	    region(0,16'3805fff50000,64,nonprefetchable,16,k),
    nuet]),
    buselement(device, addr(16'80,16'04,16'3),[
	    region(0,16'3805fff40000,64,nonprefetchable,16,k),
    nuet]),
    buselement(device, addr(16'80,16'04,16'4),[
	    region(0,16'3805fff30000,64,nonprefetchable,16,k),
    nuet]),
    buselement(device, addr(16'80,16'04,16'5),[
	    region(0,16'3805fff20000,64,nonprefetchable,16,k),
    nuet]),
    buselement(device, addr(16'80,16'04,16'6),[
	    region(0,16'3805fff10000,64,nonprefetchable,16,k),
    nuet]),
    buselement(device, addr(16'80,16'04,16'7),[
	    region(0,16'3805fff00000,64,nonprefetchable,16,k),
    nuet]),
    buselement(device, addr(16'80,16'05,16'0),[
    nuet]),
    buselement(device, addr(16'80,16'05,16'1),[
    nuet]),
    buselement(device, addr(16'80,16'05,16'2),[
    nuet]),
    buselement(device, addr(16'80,16'05,16'4),[
	    region(0,16'ec100000,32,nonprefetchable,4,k),
    nuet]),
    buselement(device, addr(16'82,16'00,16'0),[
	    region(0,16'380600000000,64,prefetchable,8,g),
	    region(4,16'ec000000,64,nonprefetchable,128,k),
    nuet])].

get_bridges(Bridges) :-
    Bridges = [buselement(bridge, addr(-1,-1,-1),
    _),
    buselement(bridge, addr(16'00,16'00,16'0),
    _),
    buselement(bridge, addr(16'00,16'01,16'0),
    secondary(16'01),
    m(16'fff00000,16'000fffff),
    p(16'00000000fff00000,16'00000000000fffff),
    _),
    buselement(bridge, addr(16'00,16'01,16'1),
    secondary(16'02),
    m(16'd0900000,16'd0afffff),
    p(16'00000000fff00000,16'00000000000fffff),
    _),
    buselement(bridge, addr(16'00,16'02,16'0),
    secondary(16'04),
    m(16'd0e00000,16'd0efffff),
    p(16'0000380200600000,16'00003802009fffff),
    _),
    buselement(bridge, addr(16'00,16'02,16'2),
    secondary(16'06),
    m(16'd0d00000,16'd0dfffff),
    p(16'00000000fff00000,16'00000000000fffff),
    _),
    buselement(bridge, addr(16'00,16'03,16'0),
    secondary(16'07),
    m(16'd0c00000,16'd0cfffff),
    p(16'0000380000000000,16'00003801ffffffff),
    _),
    buselement(bridge, addr(16'00,16'04,16'0),
    _),
    buselement(bridge, addr(16'00,16'04,16'1),
    _),
    buselement(bridge, addr(16'00,16'04,16'2),
    _),
    buselement(bridge, addr(16'00,16'04,16'3),
    _),
    buselement(bridge, addr(16'00,16'04,16'4),
    _),
    buselement(bridge, addr(16'00,16'04,16'5),
    _),
    buselement(bridge, addr(16'00,16'04,16'6),
    _),
    buselement(bridge, addr(16'00,16'04,16'7),
    _),
    buselement(bridge, addr(16'00,16'05,16'0),
    _),
    buselement(bridge, addr(16'00,16'05,16'1),
    _),
    buselement(bridge, addr(16'00,16'05,16'2),
    _),
    buselement(bridge, addr(16'00,16'05,16'4),
    _),
    buselement(bridge, addr(16'00,16'11,16'0),
    secondary(16'08),
    m(16'd0b00000,16'd0bfffff),
    p(16'0000380200000000,16'00003802004fffff),
    _),
    buselement(bridge, addr(16'00,16'16,16'0),
    _),
    buselement(bridge, addr(16'00,16'16,16'1),
    _),
    buselement(bridge, addr(16'00,16'1a,16'0),
    _),
    buselement(bridge, addr(16'00,16'1c,16'0),
    secondary(16'09),
    m(16'fff00000,16'000fffff),
    p(16'00000000fff00000,16'00000000000fffff),
    _),
    buselement(bridge, addr(16'00,16'1c,16'7),
    secondary(16'0a),
    m(16'd0000000,16'd08fffff),
    p(16'00000000ea000000,16'00000000eaffffff),
    _),
    buselement(bridge, addr(16'00,16'1d,16'0),
    _),
    buselement(bridge, addr(16'00,16'1e,16'0),
    secondary(16'0b),
    m(16'fff00000,16'000fffff),
    p(16'00000000fff00000,16'00000000000fffff),
    _),
    buselement(bridge, addr(16'00,16'1f,16'0),
    _),
    buselement(bridge, addr(16'00,16'1f,16'2),
    _),
    buselement(bridge, addr(16'00,16'1f,16'3),
    _),
    buselement(bridge, addr(16'02,16'00,16'0),
    _),
    buselement(bridge, addr(16'02,16'00,16'1),
    _),
    buselement(bridge, addr(16'02,16'00,16'2),
    _),
    buselement(bridge, addr(16'02,16'00,16'3),
    _),
    buselement(bridge, addr(16'04,16'00,16'0),
    _),
    buselement(bridge, addr(16'04,16'00,16'1),
    _),
    buselement(bridge, addr(16'06,16'00,16'0),
    _),
    buselement(bridge, addr(16'07,16'00,16'0),
    _),
    buselement(bridge, addr(16'08,16'00,16'0),
    _),
    buselement(bridge, addr(16'08,16'00,16'3),
    _),
    buselement(bridge, addr(16'0a,16'00,16'0),
    _),
    buselement(bridge, addr(16'7f,16'08,16'0),
    _),
    buselement(bridge, addr(16'7f,16'09,16'0),
    _),
    buselement(bridge, addr(16'7f,16'0a,16'0),
    _),
    buselement(bridge, addr(16'7f,16'0a,16'1),
    _),
    buselement(bridge, addr(16'7f,16'0a,16'2),
    _),
    buselement(bridge, addr(16'7f,16'0a,16'3),
    _),
    buselement(bridge, addr(16'7f,16'0b,16'0),
    _),
    buselement(bridge, addr(16'7f,16'0b,16'3),
    _),
    buselement(bridge, addr(16'7f,16'0c,16'0),
    _),
    buselement(bridge, addr(16'7f,16'0c,16'1),
    _),
    buselement(bridge, addr(16'7f,16'0c,16'2),
    _),
    buselement(bridge, addr(16'7f,16'0c,16'3),
    _),
    buselement(bridge, addr(16'7f,16'0c,16'4),
    _),
    buselement(bridge, addr(16'7f,16'0d,16'0),
    _),
    buselement(bridge, addr(16'7f,16'0d,16'1),
    _),
    buselement(bridge, addr(16'7f,16'0d,16'2),
    _),
    buselement(bridge, addr(16'7f,16'0d,16'3),
    _),
    buselement(bridge, addr(16'7f,16'0d,16'4),
    _),
    buselement(bridge, addr(16'7f,16'0e,16'0),
    _),
    buselement(bridge, addr(16'7f,16'0e,16'1),
    _),
    buselement(bridge, addr(16'7f,16'0f,16'0),
    _),
    buselement(bridge, addr(16'7f,16'0f,16'1),
    _),
    buselement(bridge, addr(16'7f,16'0f,16'2),
    _),
    buselement(bridge, addr(16'7f,16'0f,16'3),
    _),
    buselement(bridge, addr(16'7f,16'0f,16'4),
    _),
    buselement(bridge, addr(16'7f,16'0f,16'5),
    _),
    buselement(bridge, addr(16'7f,16'10,16'0),
    _),
    buselement(bridge, addr(16'7f,16'10,16'1),
    _),
    buselement(bridge, addr(16'7f,16'10,16'2),
    _),
    buselement(bridge, addr(16'7f,16'10,16'3),
    _),
    buselement(bridge, addr(16'7f,16'10,16'4),
    _),
    buselement(bridge, addr(16'7f,16'10,16'5),
    _),
    buselement(bridge, addr(16'7f,16'10,16'6),
    _),
    buselement(bridge, addr(16'7f,16'10,16'7),
    _),
    buselement(bridge, addr(16'7f,16'13,16'0),
    _),
    buselement(bridge, addr(16'7f,16'13,16'1),
    _),
    buselement(bridge, addr(16'7f,16'13,16'4),
    _),
    buselement(bridge, addr(16'7f,16'13,16'5),
    _),
    buselement(bridge, addr(16'7f,16'13,16'6),
    _),
    buselement(bridge, addr(16'7f,16'16,16'0),
    _),
    buselement(bridge, addr(16'7f,16'16,16'1),
    _),
    buselement(bridge, addr(16'7f,16'16,16'2),
    _),
    buselement(bridge, addr(16'80,16'01,16'0),
    secondary(16'81),
    m(16'fff00000,16'000fffff),
    p(16'00000000fff00000,16'00000000000fffff),
    _),
    buselement(bridge, addr(16'80,16'02,16'0),
    secondary(16'82),
    m(16'ec000000,16'ec0fffff),
    p(16'0000380600000000,16'00003807ffffffff),
    _),
    buselement(bridge, addr(16'80,16'03,16'0),
    secondary(16'83),
    m(16'fff00000,16'000fffff),
    p(16'00000000fff00000,16'00000000000fffff),
    _),
    buselement(bridge, addr(16'80,16'03,16'2),
    secondary(16'84),
    m(16'fff00000,16'000fffff),
    p(16'00000000fff00000,16'00000000000fffff),
    _),
    buselement(bridge, addr(16'80,16'04,16'0),
    _),
    buselement(bridge, addr(16'80,16'04,16'1),
    _),
    buselement(bridge, addr(16'80,16'04,16'2),
    _),
    buselement(bridge, addr(16'80,16'04,16'3),
    _),
    buselement(bridge, addr(16'80,16'04,16'4),
    _),
    buselement(bridge, addr(16'80,16'04,16'5),
    _),
    buselement(bridge, addr(16'80,16'04,16'6),
    _),
    buselement(bridge, addr(16'80,16'04,16'7),
    _),
    buselement(bridge, addr(16'80,16'05,16'0),
    _),
    buselement(bridge, addr(16'80,16'05,16'1),
    _),
    buselement(bridge, addr(16'80,16'05,16'2),
    _),
    buselement(bridge, addr(16'80,16'05,16'4),
    _),
    buselement(bridge, addr(16'82,16'00,16'0),
    _)].


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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the main part of the allocation. Called once per root bridge
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bridge_assignment(Plan, Root, Granularity, ExclRanges, IOAPICs) :-
    root(Addr,childbus(MinBus,MaxBus),mem(LMem,HMem)) = Root,
    X is HMem - LMem,
    Type = mem,

% prefetchable
    constrain_bus(Granularity, Type, prefetchable, Addr,MinBus,MaxBus,LMem,HMem,BusElementListP),
    RBaseP::[LMem..HMem],
    RHighP::[LMem..HMem],
    RSizeP::[0..X],
    devicetree(BusElementListP,buselement(bridge,Addr,secondary(MinBus),RBaseP,RHighP,RSizeP, Type, prefetchable, _, _),TP),

% nonprefetchable
    constrain_bus(Granularity, Type, nonprefetchable, Addr,MinBus,MaxBus,LMem,HMem,BusElementListNP),
    RBaseNP::[LMem..HMem],
    RHighNP::[LMem..HMem],
    RSizeNP::[0..X],
    devicetree(BusElementListNP,buselement(bridge,Addr,secondary(MinBus),RBaseNP,RHighNP,RSizeNP, Type, nonprefetchable, _, _),TNP),

% pseudo-root of both trees
    PseudoBase::[LMem..HMem],
    PseudoHigh::[LMem..HMem],
    PseudoSize::[0..X],
    T = t(buselement(bridge, addr(-1, -1, -1), childbus(-1, -1), PseudoBase, PseudoHigh, PseudoSize, _, _, _, _), [TP, TNP]),
    setrange(T,_,_,_),
    nonoverlap(T),
    naturally_aligned(T, 256, LMem, HMem),
    tree2list(T,ListaU),
    sort(6, >=, ListaU, Lista),
    not_overlap_memory_ranges(Lista, ExclRanges),
    keep_orig_addr(Lista, 12, 3, _, _, _, _),
    keep_ioapic_bars(Lista, IOAPICs),
    labelall(Lista),
    subtract(Lista,[buselement(bridge,Addr,_,_,_,_,_,prefetchable,_,_)],Pl3),
    subtract(Pl3,[buselement(bridge,Addr,_,_,_,_,_,nonprefetchable,_,_)],Pl2),
    subtract(Pl2,[buselement(bridge,addr(-1,-1,-1),_,_,_,_,_,_,_,_)],Pl),
    maplist(adjust_range(0),Pl,PR),
    maplist(back_to_bytes(Granularity),PR,Plan).

% dot output:
%    PrBaseBytePref is RBaseP * Granularity,
%    PrHighBytePref is RHighP * Granularity,
%    PrBaseByteNonPref is RBaseNP * Granularity,
%    PrHighByteNonPref is RHighNP * Granularity,
%    plan_to_dot(Granularity, Plan, Root, PrBaseBytePref, PrHighBytePref, PrBaseByteNonPref, PrHighByteNonPref).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% instantiating the variables
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


labelall(BusElementList) :-
    maplist(base, BusElementList, Base),
    maplist(high, BusElementList, High),
    maplist(size, BusElementList, Size),
    append(Base, High, L1),
    append(L1, Size, L2),
    labeling(L2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create the list of devices and bridges in form of buselements and create the
% variables.
% we care about the allocation of memory mapped registers here, therefore we only
% look at bar located in "mem", not "io"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

constrain_bus(Granularity, Type, Prefetch, RootAddr,Bus,MaxBus,LMem,HMem,OutBusElementList) :-
    constrain_bus_ex(Granularity, Type, Prefetch, RootAddr,Bus,MaxBus,LMem,HMem,[],OutBusElementList).

constrain_bus_ex(_, _, _, _,Bus,MaxBus,_,_,InL,InL) :- Bus > MaxBus.
constrain_bus_ex(Granularity, Type, Prefetch, RootAddr,Bus,MaxBus,LMem,HMem,InBusElementList,OutBusElementList) :-
    Bus =< MaxBus,
    SMax is HMem - LMem,
    ( is_predicate(bridge/8) ->
	    findall(buselement(bridge,addr(Bus,Dev,Fun),secondary(Sec),Base,High,Size,Type,Prefetch, PCIe, 0),
	            ( bridge(PCIe, addr(Bus,Dev,Fun), _, _, _, _, _, secondary(Sec)),
	              not addr(Bus,Dev,Fun) = RootAddr,
	              Base::[LMem..HMem],High::[LMem..HMem],Size::[0..SMax]
	            ),BridgeList);
        BridgeList = []
    ),
    ( is_predicate(device/8) ->
	    findall(buselement(device,addr(Bus,Dev,Fun),BAR,Base,High,SizeP,Type,Prefetch, PCIe, Bits),
	            ( device(PCIe, addr(Bus,Dev,Fun),_,_,_,_,_,_),
	              bar(addr(Bus,Dev,Fun),BAR,_,Size, Type, Prefetch, Bits),
	              Base::[LMem..HMem],High::[LMem..HMem],
	              ST1 is Size / Granularity,
	              ceiling(ST1, ST2),
	              integer(ST2, SizeP)
	            ),DeviceList);
        DeviceList = []
    ),
    append(BridgeList, DeviceList, MyBusElementList),
    append(InBusElementList, MyBusElementList, NewBusElementList),
    NextBus is Bus + 1,
    constrain_bus_ex(Granularity, Type, Prefetch, RootAddr, NextBus, MaxBus, LMem,HMem,NewBusElementList,OutBusElementList).


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
    ElemSize $>= Size,
    ( not MinList=[] ->
        ic:minlist(MinList,Min),
        ic:maxlist(MaxList,Max),
        Min $>= Base,
        Max $=< High;
        true
    ),
    High $= Base + ElemSize,
    SubTreeSize $= ElemSize,
    SubTreeMin $= Base,
    SubTreeMax $= High.
setrange([],0,_,_).


% make sure that the children do not overlap
child(t(C,_),C).
nonoverlap(Tree) :-
    t(_ ,Children) = Tree,
    maplist(child,Children,ChildList),
    ( not ChildList=[] ->
        maplist(base,ChildList,Base),
        maplist(size,ChildList,Size),
        disjunctive(Base,Size);
        true
    ),
    ( foreach(El, Children)
      do
        nonoverlap(El)
    ).


naturally_aligned(Tree, BridgeAlignment, LMem, HMem) :-
    t(Node,Children) = Tree,
    ( buselement(device,_,_,Base,High,Size,_,_,_,_) = Node ->
      Divisor is Size
      ;
      buselement(bridge,_,_,Base,High,_,_,_,_,_) = Node ->
      Divisor is BridgeAlignment
    ),

    T1 is (HMem - LMem) / Divisor,
    ceiling(T1, T2),
    integer(T2, Nr),
    N::[0..Nr],
    N2::[0..Nr],
    mod(LMem,Divisor,Remainder),
    ( Remainder =:= 0 ->
        Corr is 0;
        Corr is Divisor - Remainder
    ),
    Base $= N*Divisor + LMem + Corr,
    High $>= Base,
    High $= N2*Divisor + LMem + Corr,
    ( foreach(El, Children),
      param(BridgeAlignment),
      param(LMem),
      param(HMem)
      do
        naturally_aligned(El, BridgeAlignment, LMem, HMem)
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
    ( buselement(device,addr(Bus,Dev,Fun),BAR,Base,_,_,_,_,_,_) = H,device(_,addr(Bus,Dev,Fun),_,_,Class, SubClass, ProgIf,_),bar(addr(Bus,Dev,Fun),BAR,OrigBase,_,_,_,_) ->
       T1 is OrigBase / 4096,
       floor(T1,T2),
       integer(T2,KeepBase),
        Base $= KeepBase;
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
