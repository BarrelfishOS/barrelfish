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
% Contributor(s): Kish Shen and Joachim Schimpf, CrossCore Optimization
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: eplex_osi_clpcbc.ecl,v 1.1 2012/07/31 02:17:06 jschimpf Exp $
%
% Description:		ECLiPSe/OSI interface
%
% ----------------------------------------------------------------------

:- module(eplex_osi_clpcbc, [], [empty_language]).

:- comment(categories, ["Constraints","Interfacing"]).
:- comment(summary, "Load lib(eplex) with COIN-OR's CLP (linear) with CBC"
        " (mixed integer) via OSI's OSIClpSolverInterface.").
:- comment(date, "$Date: 2012/07/31 02:17:06 $").

% eplex setup checks for existence of module eplex_cplex!
:- local initialization(ensure_loaded(library(eplex))).
:- export initialization(import(eplex)).

