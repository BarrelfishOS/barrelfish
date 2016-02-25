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
% The Original Code is  The ECLiPSe/Gurobi Interface
% The Initial Developer of the Original Code is Joachim Schimpf
% Portions created by the Initial Developer are
% Copyright (C) 2012 Joachim Schimpf
% 
% Contributor(s): Joachim Schimpf, Coninfer Ltd
% 
% END LICENSE BLOCK

% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: eplex_gurobi.ecl,v 1.1 2012/07/31 02:17:06 jschimpf Exp $
%
% Description:		ECLiPSe/Gurobi interface
%
% Authors:		J.Schimpf, Coninfer Ltd
% ----------------------------------------------------------------------

:- module(eplex_gurobi, [], [empty_language]).

:- comment(categories, ["Constraints","Interfacing"]).
:- comment(summary, "Load lib(eplex) with the Gurobi solver").
:- comment(author, "Joachim Schimpf, Coninfer Ltd").
:- comment(copyright, "Joachim Schimpf").
:- comment(date, "$Date: 2012/07/31 02:17:06 $").

% eplex setup checks for existence of module eplex_gurobi!
:- local initialization(ensure_loaded(eplex)).
:- export initialization(import(eplex)).

