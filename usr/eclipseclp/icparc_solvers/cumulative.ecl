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
% Copyright (C) 1995 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Vassilis Liatsos, IC-Parc
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: cumulative.ecl,v 1.2 2009/07/16 09:11:25 jschimpf Exp $
%
% Specialise the generic code of generic_cumulative.ecl to
% create the FD cumulative library.
% ----------------------------------------------------------------------

:- module(cumulative).

:- comment(categories, ["Constraints"]).
:- comment(summary, "Cumulative scheduling constraint library for FD").
:- comment(author, "Vassilis Liatsos").
:- comment(copyright, "Cisco Systems, Inc.").
:- comment(date, "$Date: 2009/07/16 09:11:25 $").
:- comment(desc, "\
    This library implements the cumulative scheduling constraint for the
    FD solver.  It provides weaker propagation than the two edge finder
    libraries: edge_finder and edge_finder3.
").

:- lib(fd).
:- use_module(fd_generic_interface).

:- include(generic_cumulative).

