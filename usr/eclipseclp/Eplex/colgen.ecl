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
% Contributor(s): Andrew Eremin, IC-Parc
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
%
% Description:	ECLiPSe column generation library
%
% System:	ECLiPSe Constraint Logic Programming System
% Author/s:	Andrew Eremin, IC-Parc
% Version:      $Id: colgen.ecl,v 1.1 2012/07/31 02:17:06 jschimpf Exp $
%
% ----------------------------------------------------------------------



:- module(colgen_).
% ----------------------------------------------------------------------

:- include(colgen_).

% ----------------------------------------------------------------------
:- module(colgen).
% ----------------------------------------------------------------------

:- comment(include, colgen_comments).

:- reexport colgen_ except
   
        var_dual1/7,
        get_dual1/3,
        get_coeff1/3,
        get_idx1/3,
        get_rhs1/3,
        always_set_dual1/3,
        set_dual1/3,
   
        bp_solve1/2,
        cg_solver_setup/3,
        cg_solver_setup/4,
        cg_integers1/2,
        add_cg_pool_constraint/3,
	cg_eq/3,
	cg_ge/3,
	cg_le/3,
        cg_sp_count1/2,
        cg_sp_sol/2,
        cg_valid_columns1/2,
        cg_sp_rc_sum/2,
        cg_optimal_rc1/2,
        cg_minimize/4,
        cg_minimize/5,
        cg_var_get1/4,
        cg_get1/3.

%------------------------------------------------------------------------

