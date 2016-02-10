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
% The data for the Network Scheduling/Routing Problem
% The BBC network -
% arc( node1, node2, id, capacity ).



arc(1,2,1,29).
arc(1,3,2,17).
arc(4,5,3,16).
arc(4,2,4,31).
arc(4,6,5,55).
arc(4,7,6,17).
arc(5,4,7,17).
arc(5,8,8,27).
arc(5,6,9,29).
arc(5,9,10,21).
arc(5,10,11,17).
arc(2,1,12,31).
arc(2,11,13,33).
arc(2,12,14,27).
arc(8,5,15,24).
arc(8,13,16,17).
arc(8,14,17,17).
arc(3,1,18,16).
arc(3,11,19,16).
arc(15,12,20,50).
arc(16,17,21,16).
arc(6,4,22,61).
arc(6,5,23,31).
arc(6,18,24,27).
arc(6,19,25,35).
arc(6,20,26,17).
arc(6,21,27,23).
arc(6,30,69,17).
arc(17,16,28,17).
arc(17,11,29,27).
arc(17,22,30,27).
arc(17,7,31,19).
arc(11,2,32,27).
arc(11,3,33,17).
arc(11,17,34,24).
arc(22,2,35,25).
arc(22,17,36,24).
arc(22,23,37,21).
arc(9,5,38,19).
arc(9,24,39,21).
arc(18,6,40,24).
arc(18,25,41,17).
arc(13,8,42,16).
arc(10,5,43,16).
arc(23,22,44,19).
arc(23,12,45,17).
arc(26,27,46,17).
arc(26,12,47,16).
arc(28,27,48,16).
arc(24,9,49,19).
arc(27,26,50,16).
arc(27,28,51,17).
arc(27,12,52,16).
arc(27,29,53,17).
arc(12,2,54,24).
arc(12,15,55,51).
arc(12,23,56,16).
arc(12,26,57,17).
arc(12,27,58,21).
arc(19,6,59,31).
arc(7,4,60,16).
arc(7,6,61,21).
arc(7,17,62,21).
arc(7,20,63,21).
arc(29,27,64,16).
arc(25,18,65,16).
arc(20,6,66,16).
arc(14,8,67,16).
arc(21,6,68,24).






% The list of nodes in the graph - node( node_list ).

nodes(
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29]
 
).

node_no( 29 ).

max_path( 20 ).


% The goegraphical location of the nodes - node( id, x-coord, y-coord ).

node( 1, be, 195, 576 ).
node( 2, ce, 414, 611 ).
node( 3, dn, 151, 387 ).
node( 4, bm, 482, 314 ).
node( 5, bs, 434, 190 ).
node( 6, l, 608, 191 ).
node( 7, ymor, 588, 320 ).
node( 8, cf, 392, 235 ).
node( 9, py, 322, 62 ).
node( 10, ycdw, 295, 211 ).
node( 11, mr, 428, 436 ).
node( 12, yksh, 403, 726 ).
node( 13, ybnp, 297, 298 ).
node( 14, ysxc, 340, 235 ).
node( 15, gw, 332, 731 ).
node( 16, hu, 585, 473 ).
node( 17, ls, 505, 477 ).
node( 18, so, 517, 124 ).
node( 19, ymhr, 700, 270 ).
node( 20, yshc, 699, 332 ).
node( 21, yzth, 691, 151 ).
node( 22, nt, 501, 621 ).
node( 23, ycra, 450, 768 ).
node( 24, ygoy, 249, 20 ).
node( 25, yrow, 517, 78 ).
node( 26, ycrg, 409, 822 ).
node( 27, ygrh, 495, 937 ).
node( 28, ydur, 399, 963 ).
node( 29, yrmk, 348, 942 ).


% The in-out nodes for each node in the network

nodes_in_out([1 - [2, 3] - [2, 3], 2 - [1, 4, 11, 12, 22] - [1, 11, 12], 3 
- [1, 11] - [1, 11], 4 - [5, 6, 7] - [2, 5, 6, 7], 5 - [4, 6, 8, 9, 10] - 
[4, 6, 8, 9, 10], 6 - [4, 5, 7, 18, 19, 20, 21] - [4, 5, 18, 19, 20, 21], 
7 - [4, 17] - [4, 6, 17, 20], 8 - [5, 13, 14] - [5, 13, 14], 9 - [5, 24] - 
[5, 24], 10 - [5] - [5], 11 - [2, 3, 17] - [2, 3, 17], 12 - [2, 15, 23, 
26, 27] - [2, 15, 23, 26, 27], 13 - [8] - [8], 14 - [8] - [8], 15 - [12] - 
[12], 16 - [17] - [17], 17 - [7, 11, 16, 22] - [7, 11, 16, 22], 18 - [6, 
25] - [6, 25], 19 - [6] - [6], 20 - [6, 7] - [6], 21 - [6] - [6], 22 - 
[17, 23] - [2, 17, 23], 23 - [12, 22] - [12, 22], 24 - [9] - [9], 25 - 
[18] - [18], 26 - [12, 27] - [12, 27], 27 - [12, 26, 28, 29] - [12, 26, 
28, 29], 28 - [27] - [27], 29 - [27] - [27]]).

disj_subnets([27-[28,29],12-[26,27,28,29],12-[15],5-[10],5-[14,8,13],5-[9,24],8-[14],
8-[13],9-[24],6-[18,25],6-[21],6-[19],18-[25],17-[16],b-[12,23,2,22,1,3,11,17,
4,7,20,5,6]]).

disj_links([[(12,2),(2,12),(22,23),(23,22)]-[26,28,29,27,15,12,23],
[(4,2),(17,7),(7,17)]-[26,28,29,27,15,12,23,2,22,1,3,11,17,16],
[(4,5),(5,4),(4,6),(6,4),(7,6),(6,7),(20,6),(6,20)]-[13,14,8,5,10,
9,24,6,18,25,21,19]]).

