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
% Contributor(s): Warwick Harvey and Kish Shen, IC-Parc
% 
% END LICENSE BLOCK
%---------------------------------------------------------------------
%
% GFD search module.
%
% System:       ECLiPSe Constraint Logic Programming System
% Author/s:     Warwick Harvey, IC-Parc
%               Kish Shen,      IC-Parc
%
%	This module is essentially an extract of the search-related
%	predicates from the RIA module, written by Joachim Schimpf and
%	Stefano Novello; and from the fd_search module, by Helmut Simonis.
%
% This module provides the IC/FD compatible search-related components 
%
%---------------------------------------------------------------------

:- module(gfd_search).

%---------------------------------------------------------------------
%
% Imports and exports.
%

:- comment(categories, ["Constraints"]).
:- comment(summary, "This library provides the IC/FD compatible search-related components for the GFD-library").

:-comment(desc,html("<P>
  This library provides the generic search facilities search/6, delete/5
  and indomain/2 for the GFD solver, compatible with the corresponding
  versions in library(fd_search) and library(ic).
</P><P>
  This GFD version allows the following additional variable selection
  criteria (in addition to the generic ones) in delete/5 and search/6:
<UL>
  <LI>max_regret_lwb</LI>
  <LI>max_regret_upb</LI>
  <LI>max_weighted_degree</LI>
  <LI>most_constrained_per_value</LI>
  <LI>max_weighted_degree_per_value</LI>
</UL>
</P>
")).


%---------------------------------------------------------------------
% generalised search/6 and friends originally from fd_search
%
:- lib(gfd).
:- lib(gfd_generic_interface).

:- include(generic_search).
:- comment(include, generic_search_comments).

