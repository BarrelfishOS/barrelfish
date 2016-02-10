/* BEGIN LICENSE BLOCK
 * Version: CMPL 1.1
 *
 * The contents of this file are subject to the Cisco-style Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file except
 * in compliance with the License.  You may obtain a copy of the License
 * at www.eclipse-clp.org/license.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
 * the License for the specific language governing rights and limitations
 * under the License. 
 * 
 * The Original Code is  The ECLiPSe Constraint Logic Programming System. 
 * The Initial Developer of the Original Code is  Cisco Systems, Inc. 
 * Portions created by the Initial Developer are
 * Copyright (C) 1999-2011 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): Kish Shen 
 * 
 * END LICENSE BLOCK */
/*
 *
 * System:	ECLiPSe Constraint Logic Programming System
 * Author/s:	Kish Shen
 * Version:	$Id: coinplex_params.h,v 1.1 2012/07/31 02:17:06 jschimpf Exp $
 *
 */


/* This file defines informations specific to parameters for solvers using
   the COIN OSI interface. It is needed by eplex_params.h, and coinplex.cpp
*/

#ifdef COIN_USE_CLP

/* Solver dependent parameters - these define the positions in the arrays
   that maps to the corresponding actual paramters of the solver
   (e.g. cbc_iparam[] and cbc_dparam[]) in coinplex.cpp
*/
#define SolverMaxNumNode		0
#define SolverMaxNumSol			1
#define NumSolverMipIntParams           2 /* end marker */

#define SolverIntegerTolerance		0
#define SolverAllowableGap		1
#define SolverAllowableFractionGap	2
#define SolverCutoffIncrement		3
#define SolverInfeasibilityWeight       4
#define SolverHeuristicGap              5
#define SolverHeuristicFractionGap      6
#define NumSolverMipDblParams           7 /* end marker */

#define SolverLPPresolveTolerance       7
#define NumSolverLpDblParams            1 /* end marker (-NumSolverMipDblParams) */

/* eplex parameters for Clp/Cbc */

/* string params */
#define EpxClpParam_bar_ordering     0
#define EpxClpParam_ns               1  /* end marker */
/* int params */
#define EpxClpParam_print_freq       0
#define EpxClpParam_loglevel	     1
#define EpxClpParam_mip_lploglevel   2
#define EpxClpParam_doKKT	     3
#define EpxClpParam_ni               4  /* end marker */

#endif
