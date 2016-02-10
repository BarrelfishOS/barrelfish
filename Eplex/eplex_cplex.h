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
 * Copyright (C) 1995-2012 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): Joachim Schimpf, Kish Shen and Andrew Eremin, IC-Parc
 * 
 * END LICENSE BLOCK */

/*
 * ECLiPSe/CPLEX interface (for inclusion in eplex.c)
 */

/* Hack needed for Cplex 12.3: to select the correct definitions, pretend to be the MSC compiler. */
#if defined(_WIN32) && !defined(_MSC_VER) /* Note that _WIN32 is also defined for _WIN_64 */
#define _MSC_VER
#include "cplex.h"		/* CPLEX declarations */
#undef _MSC_VER
#else
#include "cplex.h"		/* CPLEX declarations */
#endif

/* The values CPX_VERSION_VERSION and CPX_VERSION_RELEASE were introduced in CPLEX 12.3.
 * For earlier versions, we compute them from the 4-digit CPX_VERSION.
 */
#ifndef CPX_VERSION_VERSION
#define CPX_VERSION_VERSION (CPX_VERSION/100)
#endif
#ifndef CPX_VERSION_RELEASE
#define CPX_VERSION_RELEASE ((CPX_VERSION/10)%10)
#endif

#if (CPLEX == 0)
#define CPLEX CPX_VERSION_VERSION
#elif (CPLEX != CPX_VERSION_VERSION)
Version mismatch!
#endif

#ifndef CPLEXMINOR
#define CPLEXMINOR CPX_VERSION_RELEASE
#elif (CPLEXMINOR != CPX_VERSION_RELEASE)
Version mismatch!
#endif

#ifndef CPXPUBLIC
#define CPXPUBLIC
#endif

typedef int param_id_t;
typedef char sostype_t;

#include "eplex.h"		/* needs declarations above! */


#define SOLVER_SHORT_NAME CPX
#define SOLVER_ATOMIC_NAME "cplex"
#define SOLVER_VERSION_INT	(10*CPLEX+CPLEXMINOR)
#define SOLVER_HAS_STR_PARAMS
#define STRBUFFERSIZE  CPX_STR_PARAM_MAX  /* size of string parameter buffer size */


#define SOLVER_SENSE_LE	'L'
#define SOLVER_SENSE_GE	'G'
#define SOLVER_SENSE_EQ	'E'

#define SOLVER_SOS_TYPE1	'1'
#define SOLVER_SOS_TYPE2	'2'

# define CPX_COL_AT_LOWER   CPX_AT_LOWER
# define CPX_COL_AT_UPPER   CPX_AT_UPPER
# define CPX_COL_BASIC      CPX_BASIC
# define CPX_COL_FREE_SUPER CPX_FREE_SUPER
/* next two not tested */
# define CPX_COL_NONBASIC_ZERO_BOUNDED	CPX_COL_AT_LOWER
# define CPX_COL_NONBASIC_ZERO_UNBOUNDED CPX_COL_FREE_SUPER

#define SUPPORT_IIS

#if CPLEX >= 9
#define HAS_CONCURRENT
#endif

#if CPLEX >= 12
#define HAS_INDICATOR_CONSTRAINTS
#endif

#if CPLEX >= 10
/* CPLEX 10+ has more generic error for no solution state and the more general conflict set 
   rather than IIS (which is for LP only) for infeasible analyses
*/
# define CPXERR_NO_INT_SOLN CPXERR_NO_SOLN 

# define HAS_GENERAL_CONFLICT_REFINER

# define Find_Conflict(Res, L, NRows, NCols)  Res = CPXrefineconflict(cpx_env, L, &(NRows), &(NCols))

# define Get_Conflict(L, Status, RowIdxs, RowStat, NRows_p, ColIdxs, ColStat, NCols_p) \
	CPXgetconflict(cpx_env, L, &(Status), RowIdxs, RowStat, NRows_p, ColIdxs, ColStat, NCols_p)

#else

/* mapping the calls to find a conflict set to the older and less general routines (LP only) to get 
   the IIS 
*/
# define Find_Conflict(Res, L, NRows, NCols)    Res = CPXfindiis(cpx_env, L, &(NRows), &(NCols))

# define Get_Conflict(L, Status, RowIdxs, RowStat, NRows_p, ColIdxs, ColStat, NCols_p) \
	CPXgetiis(cpx_env, L, &(Status), RowIdxs, RowStat, NRows_p, ColIdxs, ColStat, NCols_p)

# define CPX_CONFLICT_LB           CPXIIS_AT_LOWER
# define CPX_CONFLICT_UB           CPXIIS_AT_UPPER
# define CPX_STAT_CONFLICT_MINIMAL CPXIIS_COMPLETE

#endif 

#if CPLEX >= 7
# define CPX_HAS_LPOPT
#endif

#if CPLEX < 9
/* CPLEX 8 and older has no valid bestobjval for an optimal MIP solution! 
   cutoff
*/
# define NO_MIPBESTBOUND_IF_OPTIMAL
#endif

#if (CPLEX >= 8)

/* uniform treatment of solution status (independent of primal/dual) */
# define UNIFORM_SOL_STAT       
# define HAS_SIFT
# define CPX_HAS_DEFAULTALG /* has way of specifying `default' method without
			       naming method, for all LP/MIP/QP */
# define HAS_MIQP

# define SetCPXAlg(cpx_env, method)  { \
    CPXsetintparam(cpx_env, CPX_PARAM_LPMETHOD, method);\
    CPXsetintparam(cpx_env, CPX_PARAM_STARTALG, method);\
    if (method != CPX_ALG_SIFTING) \
        CPXsetintparam(cpx_env, CPX_PARAM_QPMETHOD, method);\
    else \
        CPXsetintparam(cpx_env, CPX_PARAM_QPMETHOD, CPX_ALG_AUTOMATIC);\
}

# define SetSiftAlg(cpx_env, meth) {  \
    switch (meth)  \
    { \
    case METHOD_AUTO: \
	CPXsetintparam(cpx_env, CPX_PARAM_SIFTALG, CPX_ALG_AUTOMATIC); \
	break; \
    case METHOD_PRIMAL: \
	CPXsetintparam(cpx_env, CPX_PARAM_SIFTALG, CPX_ALG_PRIMAL); \
	break; \
    case METHOD_DUAL: \
	CPXsetintparam(cpx_env, CPX_PARAM_SIFTALG, CPX_ALG_DUAL); \
	break; \
    case METHOD_BAR: \
	CPXsetintparam(cpx_env, CPX_PARAM_SIFTALG, CPX_ALG_BARRIER); \
	break; \
    case METHOD_NET: \
	CPXsetintparam(cpx_env, CPX_PARAM_SIFTALG, CPX_ALG_NET); \
	break; \
    default: \
	/* use error stream as warning stream unavailable */ \
	Fprintf(Current_Error, "Eplex warning: aux. method chosen for" \
		" sifting is unavailable. Using default instead\n"); \
	ec_flush(Current_Error); \
        meth = METHOD_DEFAULT; \
    case METHOD_DEFAULT: \
	break; \
    } \
}

#else /* CPLX < 8 */

/* pre-CPLEX 8 names */
# define CPXPROB_MILP			  CPXPROB_MIP
# define CPXPROB_FIXEDMILP		  CPXPROB_FIXED

# define CPX_STAT_OPTIMAL                 CPX_OPTIMAL
# define CPX_STAT_INFEASIBLE              CPX_INFEASIBLE
# define CPX_STAT_OPTIMAL_INFEAS          CPX_OPTIMAL_INFEAS
# define CPX_STAT_UNBOUNDED               CPX_UNBOUNDED
# define CPX_STAT_INForUNBD               CPX_INForUNBD
# define CPX_STAT_OPTIMAL_FACE_UNBOUNDED  CPX_OPTIMAL_FACE_UNBOUNDED

# define CPXqpopt(A,B) CPXbaropt(A,B) /* no CPXqpopt() */

/* DualMethod(lpd,m,am) should be true for methods that require different
 * interpretation of the optimization result code */

# define DualMethod(lpd,m,am) \
     (((lpd)->prob_type == PROBLEM_LP || (lpd)->prob_type == PROBLEM_RELAXEDL) && \
 	((m) == METHOD_DUAL || ((m) == METHOD_NET && (am) == METHOD_DUAL)) )

# define CPX_HAS_RELAXEDLP

# if (CPLEX >= 7)  /* CPLEX 7 only */

#  define SetCPXAlg(cpx_env, method)  { \
    CPXsetintparam(cpx_env, CPX_PARAM_LPMETHOD, method);\
    CPXsetintparam(cpx_env, CPX_PARAM_STARTALG, method);\
}

# elif (CPLEX >= 6)  /* CPLEX 6 only */

#  define CPX_ALG_NET                     CPX_NODEALG_HYBNETOPT

#  define HAS_NO_BARCROSSOVER  /* does not have `no crossover' for barrier */

# define SetCPXAlg(cpx_env, method)  { \
    CPXsetintparam(cpx_env, CPX_PARAM_STARTALG, method);\
}

# endif

#endif /* CPLEX >= 8 */

#if CPLEX > 12 || (CPLEX == 12  &&  CPLEXMINOR >= 3)
#define IfAtLeast123(X) (X)
#else
#define IfAtLeast123(X) 0
#endif

#define SuccessState(d) ( \
	(d)->sol_state == CPX_STAT_OPTIMAL || \
	(d)->sol_state == CPX_STAT_OPTIMAL_INFEAS || \
        (d)->sol_state == CPX_STAT_OPTIMAL_FACE_UNBOUNDED || \
	IfAtLeast123((d)->sol_state == CPX_STAT_FIRSTORDER) || \
        MIPSuccessState(d))

#define MIPSuccessState(d) ( \
	(d)->sol_state == CPXMIP_OPTIMAL || \
	(d)->sol_state == CPXMIP_OPTIMAL_TOL || \
	(d)->sol_state == CPXMIP_OPTIMAL_INFEAS)

#define FailState(d) ( \
	(d)->sol_state == CPX_STAT_INFEASIBLE || \
	(d)->sol_state == CPXMIP_INFEASIBLE)

#if CPLEX >= 8
#define UnboundedState(d) ( \
	(d)->sol_state == CPX_STAT_UNBOUNDED || \
        (d)->sol_state == CPXMIP_UNBOUNDED)

#define MaybeFailState(d) ( \
	(d)->sol_state == CPX_STAT_INForUNBD || \
        (d)->sol_state == CPXMIP_INForUNBD || \
	(d)->sol_state == CPXERR_PRESLV_INForUNBD )

#define LPAbortedState(d) ( \
	(d)->sol_state == CPX_STAT_ABORT_IT_LIM || \
	(d)->sol_state == CPX_STAT_ABORT_TIME_LIM || \
	IfAtLeast123((d)->sol_state == CPX_STAT_ABORT_DETTIME_LIM) || \
	(d)->sol_state == CPX_STAT_ABORT_OBJ_LIM || \
	(d)->sol_state == CPX_STAT_ABORT_USER || \
        (d)->sol_state == CPX_STAT_NUM_BEST || \
	(d)->sol_state == CPX_STAT_ABORT_PRIM_OBJ_LIM || \
	(d)->sol_state == CPX_STAT_ABORT_DUAL_OBJ_LIM )

#else

/* aborted with no feasible (primal/dual) solution */
#define LPAbortedNoSolState(d) ( \
	(d)->sol_state == CPX_IT_LIM_INFEAS || \
	(d)->sol_state == CPX_TIME_LIM_INFEAS || \
	(d)->sol_state == CPX_NUM_BEST_INFEAS || \
	(d)->sol_state == CPX_ABORT_INFEAS  )

/* aborted with feasible solution */
#define LPAbortedSolState(d) ( \
	(d)->sol_state == CPX_OBJ_LIM || \
	(d)->sol_state == CPX_IT_LIM_FEAS || \
	(d)->sol_state == CPX_TIME_LIM_FEAS || \
	(d)->sol_state == CPX_NUM_BEST_FEAS || \
	(d)->sol_state == CPX_NUM_BEST_PRIM_DUAL_FEAS || \
	(d)->sol_state == CPX_ABORT_FEAS )

#define LPAbortedState(d) ( \
        LPAbortedSolState(d) || LPAbortedNoSolState(d) )

#define UnboundedState(d) ( \
	(d)->sol_state == CPX_STAT_UNBOUNDED )

#if CPLEX > 6 || ( CPLEX == 6 && CPLEXMINOR > 0 )
#define MaybeFailState(d) ( \
	(d)->sol_state == CPX_STAT_INForUNBD || \
	(d)->sol_state == CPXERR_PRESLV_INForUNBD )
#else
#define MaybeFailState(d) 0
#endif

#endif

#if CPLEX >= 7
# define MIPSemiFailState(d) ( \
	(d)->sol_state == CPXMIP_NODE_LIM_INFEAS || \
	(d)->sol_state == CPXMIP_TIME_LIM_INFEAS || \
	IfAtLeast123((d)->sol_state == CPXMIP_DETTIME_LIM_INFEAS) || \
	(d)->sol_state == CPXMIP_FAIL_INFEAS || \
	(d)->sol_state == CPXMIP_MEM_LIM_INFEAS || \
	(d)->sol_state == CPXMIP_ABORT_INFEAS || \
	(d)->sol_state == CPXMIP_FAIL_INFEAS_NO_TREE)

# define MIPSemiSuccessState(d) ( \
	(d)->sol_state == CPXMIP_SOL_LIM || \
	(d)->sol_state == CPXMIP_NODE_LIM_FEAS || \
	(d)->sol_state == CPXMIP_TIME_LIM_FEAS || \
	IfAtLeast123((d)->sol_state == CPXMIP_DETTIME_LIM_FEAS) || \
	(d)->sol_state == CPXMIP_FAIL_FEAS || \
	(d)->sol_state == CPXMIP_MEM_LIM_FEAS || \
	(d)->sol_state == CPXMIP_ABORT_FEAS || \
	(d)->sol_state == CPXMIP_FAIL_FEAS_NO_TREE)
#else
# define MIPSemiFailState(d) ( \
	(d)->sol_state == CPXMIP_NODE_LIM_INFEAS || \
	(d)->sol_state == CPXMIP_TIME_LIM_INFEAS || \
	(d)->sol_state == CPXMIP_FAIL_INFEAS || \
	(d)->sol_state == CPXMIP_MEM_LIM_INFEAS || \
	(d)->sol_state == CPXMIP_ABORT_INFEAS || \
	(d)->sol_state == CPXMIP_FAIL_INFEAS_NO_TREE || \
	(d)->sol_state == CPXMIP_NODE_FILE_LIM_INFEAS)

# define MIPSemiSuccessState(d) ( \
	(d)->sol_state == CPXMIP_SOL_LIM || \
	(d)->sol_state == CPXMIP_NODE_LIM_FEAS || \
	(d)->sol_state == CPXMIP_TIME_LIM_FEAS || \
	(d)->sol_state == CPXMIP_FAIL_FEAS || \
	(d)->sol_state == CPXMIP_MEM_LIM_FEAS || \
	(d)->sol_state == CPXMIP_ABORT_FEAS || \
	(d)->sol_state == CPXMIP_FAIL_FEAS_NO_TREE || \
	(d)->sol_state == CPXMIP_NODE_FILE_LIM_FEAS)
#endif


#define SetPreSolve(state) \
{\
   Log1(CPXsetintparam(cpx_env, CPX_PARAM_PREIND, %d), state); \
   CPXsetintparam(cpx_env, CPX_PARAM_PREIND, state); \
}

#define CPXupdatemodel(LP)

#define Get_Feasibility_Tolerance(E,L,T) CPXgetdblparam(E,CPX_PARAM_EPRHS,T)

#define Get_Int_Param(E,L,A1,A2) 	CPXgetintparam(E,A1,A2)
#define Get_Dbl_Param(E,L,A1,A2)	CPXgetdblparam(E,A1,A2)
#define Get_Str_Param(E,L,A1,A2)        CPXgetstrparam(E,A1,A2)
#define Set_Int_Param(E,L,A1,A2) 	CPXsetintparam(E,A1,A2)
#define Set_Dbl_Param(E,L,A1,A2)	CPXsetdblparam(E,A1,A2)
#define Set_Str_Param(E,L,A1,A2)        CPXsetstrparam(E,A1,A2)

# define Get_LP_Objval(A1,A2)		CPXgetobjval(cpx_env,(A1)->lp,A2)
# define Get_Best_Objbound(A1, A2)      CPXgetbestobjval(cpx_env,A1,A2)

#define Get_MIPCutOff(d, v) \
       ((d)->sense == SENSE_MIN ? CPXgetdblparam(cpx_env, CPX_PARAM_CUTUP, v) : CPXgetdblparam(cpx_env, CPX_PARAM_CUTLO, v))

#define UsingBarrierNoCrossOver(d) (CPXgetmethod(cpx_env, (d)) == CPX_ALG_BARRIER)
#define Get_Bar_Primal_Obj(d, obj) CPXgetdblquality(cpx_env, (d), obj, CPX_PRIMAL_OBJ)
#define Get_Bar_Dual_Obj(d, obj) CPXgetdblquality(cpx_env, (d), obj, CPX_DUAL_OBJ)

#define HAS_QUADRATIC
#define SOLVER_MAT_BASE   0
#define SOLVER_MAT_OFFSET 1

#define HAS_LIMITED_MIP_RESULTS
