:-module(graphviz_license).

:-pragma(nodebug).
:-pragma(system).

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

:-export get_license/1.

:-local initialization(print_license).

:-comment(get_license/1, [
        summary:"Returns the license description for this module",
        amode:get_license(+),
        amode:get_license(-),
        args:[license:"The license string" ],
        desc:"Returns the license description for this module"]).

get_license(
"   This  product  contains  certain  software  code or other information
   (\"AT&T  Software\")  proprietary  to  AT&T  Corp.  (\"AT&T\").  The  AT&T
   Software  is  provided to you \"AS IS\". YOU ASSUME TOTAL RESPONSIBILITY
   AND  RISK  FOR  USE  OF  THE  AT&T  SOFTWARE.  AT&T DOES NOT MAKE, AND
   EXPRESSLY  DISCLAIMS,  ANY  EXPRESS  OR IMPLIED WARRANTIES OF ANY KIND
   WHATSOEVER,  INCLUDING,  WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
   MERCHANTABILITY  OR  FITNESS  FOR  A PARTICULAR PURPOSE, WARRANTIES OF
   TITLE  OR  NON-INFRINGEMENT  OF  ANY INTELLECTUAL PROPERTY RIGHTS, ANY
   WARRANTIES  ARISING  BY USAGE OF TRADE, COURSE OF DEALING OR COURSE OF
   PERFORMANCE, OR ANY WARRANTY THAT THE AT&T SOFTWARE IS \"ERROR FREE\" OR
   WILL MEET YOUR REQUIREMENTS.
    ").


print_license:-
        get_license(License),
        writeln(log_output, License),
        flush(log_output).
