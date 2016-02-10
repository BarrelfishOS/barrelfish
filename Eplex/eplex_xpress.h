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
 * ECLiPSe/XPRESSMP interface (for inclusion in eplex.c)
 */

#include "xprs.h"

#define CPXLPptr		XPRSprob /* prob. pointer 13+ only! */
#define CPXENVptr		XPRSprob /* 'default' problem */

#define SOLVE_MIP_COPY

typedef int param_id_t;
typedef char sostype_t;

#include "eplex.h"		/* needs declarations above! */


# define SOLVER_SHORT_NAME XPRS
# define SOLVER_ATOMIC_NAME "xpress"
# define SOLVER_VERSION_INT XPRESS
# define XP_PROBNAME_MAX 200  /* maximum length of problem name */
# define HAS_QUADRATIC
# define HAS_MIQP
# define SOLVER_HAS_STR_PARAMS /* has string parameters */
# define STRBUFFERSIZE  256  /* string parameter buffer size (256 used in example)*/
/* copying a problem with zeroed quad. coeff. can lead to core dumps */
# define HAS_MIQP_FIXEDCOPYBUG 
# define HAS_INTLB_BUG /* LB lost when converting col to int type */
# if XPRESS <= 14
#  define HAS_MIQP_CALLBACKBUG /* callback to get MIQP solution core dumps */
#  define HAS_SAMEBOUNDSBUG /* sol. value = 0 if both bounds set to same */
# endif
# define SOLVER_HAS_LOCAL_PARAMETERS
# if XPRESS >= 14      /* need to constrain integer range...*/
#  define HAS_NARROW_INT_RANGE
#  ifndef XPRS_MAXINT  /* in case we are using an old xprs.h file */
#   define XPRS_MAXINT         2147483647
#  endif
# endif
# if XPRESS >= 15
#  define HAS_POSTSOLVE
/* XPRSpostsolve is not documented and not declared in xprs.h */
int XPRS_CC XPRSpostsolve(XPRSprob prob);
# endif
# if XPRESS < 20
#define HAS_NO_ADDSOS
# endif

# ifndef XPRESSMINOR
#  define XPRESSMINOR 0
# endif

# ifdef __STDC__
#  define __ANSIC_	/* used in xpresso.h */
# endif

# define XP_GLSTAT_OFFSET 0x10

# define SOLVER_MAT_BASE   0
# define SOLVER_MAT_OFFSET 1

#define SOLVER_SENSE_LE	'L'
#define SOLVER_SENSE_GE	'G'
#define SOLVER_SENSE_EQ	'E'

#define SOLVER_SOS_TYPE1	'1'
#define SOLVER_SOS_TYPE2	'2'

# define CPX_INFBOUND			XPRS_PLUSINFINITY
# define CPX_COL_AT_LOWER               0
# define CPX_COL_BASIC                  1
# define CPX_COL_AT_UPPER               2
# define CPX_COL_FREE_SUPER             3
# define CPX_COL_NONBASIC_ZERO_BOUNDED	CPX_COL_AT_LOWER
# define CPX_COL_NONBASIC_ZERO_UNBOUNDED CPX_COL_FREE_SUPER
# define CPXgetrhs(E,A1,A2,A3,A4)	XPRSgetrhs(A1,A2,A3,A4)
# define CPXgetsense(E,A1,A2,A3,A4)	XPRSgetrowtype(A1,A2,A3,A4)
# define CPXgetlb(E,A1,A2,A3,A4)	XPRSgetlb(A1,A2,A3,A4)
# define CPXgetub(E,A1,A2,A3,A4)	XPRSgetub(A1,A2,A3,A4)
# define CPXgetctype(E,A1,A2,A3,A4)	XPRSgetcoltype(A1,A2,A3,A4)
# define CPXchgctype(E,A1,A2,A3,A4)	XPRSchgcoltype(A1,A2,A3,A4)
# define CPXchgbds(E,A1,A2,A3,A4,A5)	XPRSchgbounds(A1,A2,A3,A4,A5)
# define CPXcopybase(E,A1,A2,A3)	XPRSloadbasis(A1,A3,A2) /* args swapped! */
# define CPXgetbase(E,A1,A2,A3)		XPRSgetbasis(A1,A3,A2) /* args swapped! */
# define Get_LP_Objval(A1,A2)		XPRSgetdblattrib((A1)->lpcopy,XPRS_LPOBJVAL,A2)
# define Get_Best_Objbound(A1, A2)      XPRSgetdblattrib(A1,XPRS_BESTBOUND,A2)
# define CPXgetmipobjval(E,A1,A2)	XPRSgetdblattrib(A1,XPRS_MIPOBJVAL,A2)
# define CPXgetobj(E,A1,A2,A3,A4)	XPRSgetobj(A1,A2,A3,A4)
# define CPXchgobj(E,A1,A2,A3,A4)	XPRSchgobj(A1,A2,A3,A4)
# define CPXcopyorder(E,A1,A2,A3,A4,A5)	XPRSloaddirs(A1,A2,A3,A4,A5,NULL,NULL)
# define CPXchgqpcoef(E,A1,A2,A3,A4)	XPRSchgqobj(A1,A2,A3,A4)
# define CPXchgrhs(E,A1,A2,A3,A4)       XPRSchgrhs(A1,A2,A3,A4)
# define CPXcloseCPLEX(E)		XPRSfree()
# define CPXaddcols(E,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10) \
          XPRSaddcols(A1,A2,A3,A4,A5,A6,A7,A8,A9) /* diff args! */
# define CPXaddrows(E,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11) \
          XPRSaddrows(A1,A3,A4,A6,A5,NULL,A7,A8,A9) /* diff args! */
# define CPXaddsos(E,LP,NS,NE,ST,SB,SI,SW,SN) XPRSaddsets(LP,NS,NE,ST,SB,SI,SW)
# define CPXgetrows(E,A1,A2,A3,A4,A5,A6,A7,A8,A9) \
          XPRSgetrows(A1,A3,A4,A5,A6,A2,A8,A9) /* diff args! */
# define CPXchgobjsen(E,A1,A2) 		0
# define CPXchgprobtype(A1, A2, A3)     0 /* 0 for success return code */
# define CPXupdatemodel(LP)

# define CPXgetnumnz(E,A1)		xprs_getintattr(A1, XPRS_ELEMS)
# define CPXgetnumint(E,A1)		xprs_getintattr(A1, XPRS_MIPENTS)
# define CPXgetnumbin(E,A1)		0
# define CPXgetnumqpnz(E,A1)		xprs_getintattr(A1, XPRS_QELEMS)

# define SUPPORT_IIS
# define Find_Conflict(Err, L, NRows, NCols) { \
	Err = XPRSiis(L, ""); \
        if (!Err) Err = XPRSgetiis(L, &(NCols), &(NRows), NULL, NULL); \
}

# define Get_Conflict(L, Status, RowIdxs, RowStat, Nrows_p, ColIdxs, ColStat, Ncols_p)  \
	Status = XPRSgetiis(L, Ncols_p, Nrows_p,  ColIdxs, RowIdxs)

# define Get_Feasibility_Tolerance(E,L,T) XPRSgetdblcontrol((L)->lp, XPRS_FEASTOL, T)

# define Get_Int_Param(E,L,A1,A2) \
   ((L) == NULL ? XPRSgetintcontrol(E,A1,A2) : XPRSgetintcontrol((L)->lp,A1,A2))
# define Get_Dbl_Param(E,L,A1,A2) \
   ((L) == NULL ? XPRSgetdblcontrol(E,A1,A2) : XPRSgetdblcontrol((L)->lp,A1,A2))
# define Get_Str_Param(E,L,A1,A2) \
   ((L) == NULL ? XPRSgetstrcontrol(E,A1,A2) : XPRSgetstrcontrol((L)->lp,A1,A2))
# define Set_Int_Param(E,L,A1,A2) \
   ((L) == NULL ? XPRSsetintcontrol(E,A1,A2) : XPRSsetintcontrol((L)->lp,A1,A2))
# define Set_Dbl_Param(E,L,A1,A2) \
   ((L) == NULL ? XPRSsetdblcontrol(E,A1,A2) : XPRSsetdblcontrol((L)->lp,A1,A2))
# define Set_Str_Param(E,L,A1,A2) \
   ((L) == NULL ? XPRSsetstrcontrol(E,A1,A2) : XPRSsetstrcontrol((L)->lp,A1,A2))
# define Get_MIPCutOff(d, v) XPRSgetdblcontrol((d)->lpcopy, XPRS_MIPABSCUTOFF, v)
# define Get_Dual_Infeas(lp, v) XPRSgetintattrib(lp, XPRS_DUALINFEAS, v)
# define Get_Primal_Infeas(lp, v) XPRSgetintattrib(lp, XPRS_PRIMALINFEAS, v)

# define SetPreSolve(state) 

#ifdef XPRESS_OEM_ICPARC_2002
# include "xprsoem.h"
#endif


# define Get_Xp_Stat(lpd) \
	if (IsMIPProb(lpd->prob_type)) { \
	    (void) XPRSgetintattrib(lpd->lpcopy, XPRS_MIPSTATUS, &lpd->sol_state); \
	    if (lpd->sol_state == XPRS_MIP_LP_NOT_OPTIMAL || \
               lpd->sol_state == XPRS_MIP_LP_OPTIMAL) \
		(void) XPRSgetintattrib(lpd->lpcopy, XPRS_LPSTATUS, &lpd->sol_state); \
	    else \
		lpd->sol_state += XP_GLSTAT_OFFSET; \
	} else if (lpd->prob_type == PROBLEM_FIXEDL || lpd->prob_type == PROBLEM_FIXEDQ) { \
        /* fixglobal only performed if MIP was optimal */ \
            (void) XPRSgetintattrib(lpd->lpcopy, XPRS_MIPSTATUS, &lpd->sol_state); \
            if (lpd->sol_state == XPRS_MIP_OPTIMAL) \
               (void) XPRSgetintattrib(lpd->lpcopy, XPRS_LPSTATUS, &lpd->sol_state); \
            else \
            { \
	       if (lpd->sol_state == XPRS_MIP_LP_NOT_OPTIMAL || \
                  lpd->sol_state == XPRS_MIP_LP_OPTIMAL) \
		  (void) XPRSgetintattrib(lpd->lpcopy, XPRS_LPSTATUS, &lpd->sol_state); \
	       else \
		  lpd->sol_state += XP_GLSTAT_OFFSET; \
            } \
        } else if (lpd->prob_type == PROBLEM_RELAXEDL || lpd->prob_type == PROBLEM_RELAXEDQ) \
        { \
	    (void) XPRSgetintattrib(lpd->lpcopy, XPRS_LPSTATUS, &lpd->sol_state); \
        } \
	else \
        { \
	    (void) XPRSgetintattrib(lpd->lp, XPRS_LPSTATUS, &lpd->sol_state); \
	}



# define SuccessState(d)	( \
	(d)->sol_state == XP_GLSTAT_OFFSET + XPRS_MIP_OPTIMAL || \
	(d)->sol_state == XPRS_LP_OPTIMAL )
/* the LP_CUTOFF* LPSTATUS happens only with MIP search, and as we access
   LPSTATUS for MIP search only if the MIP search is stopped at the root,
   we know that these states means that a cutoff occurred at the root node,
   and that the MIP optimal solution cannot be better than the cutoff, and
   this is considered to be a failure state
*/
# define FailState(d) ( \
	(d)->sol_state == XP_GLSTAT_OFFSET + XPRS_MIP_INFEAS || \
	(d)->sol_state == XPRS_LP_INFEAS || \
	(d)->sol_state == XPRS_LP_CUTOFF || \
	(d)->sol_state == XPRS_LP_CUTOFF_IN_DUAL)
/* catches the MIP cases only */
# define MIPSemiFailState(d) ( \
	(d)->sol_state == XP_GLSTAT_OFFSET + XPRS_MIP_NO_SOL_FOUND)
/* catches the MIP cases only */
# define MIPSemiSuccessState(d) ( \
	(d)->sol_state == XP_GLSTAT_OFFSET + XPRS_MIP_SOLUTION)
/* An aborted LP can be either semi-fail or semi-success */
# define LPAbortedState(d) ( \
        (d)->sol_state == XPRS_LP_UNFINISHED )
# define MaybeFailState(d) (0)
# define UnboundedState(d) ( \
	(d)->sol_state == XPRS_LP_UNBOUNDED )

# define DualMethod(lpd,m,am) 0

# define UsingBarrierNoCrossOver(d) (meth->meth == METHOD_BAR && meth->auxmeth == METHOD_NONE) 
# define Get_Bar_Primal_Obj(d, obj) XPRSgetdblattrib((d), XPRS_BARPRIMALOBJ, obj)
# define Get_Bar_Dual_Obj(d, obj) XPRSgetdblattrib((d), XPRS_BARDUALOBJ, obj)
# define Bar_Is_Primal_Feasible(lpd) \
	(XPRSgetdblattrib(lpd->lp, XPRS_BARPRIMALINF, &infeas), infeas < 1e-6)
# define Bar_Is_Dual_Feasible(lpd) \
	(XPRSgetdblattrib(lpd->lp, XPRS_BARDUALINF, &infeas), infeas < 1e-6)


