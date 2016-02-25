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
 * ECLiPSe/COIN interface (for inclusion in eplex.c)
 */

/* C_TO_COIN is defined when compiling for COIN, mapping the C calls in 
   eplex.c to the procedures in C++ coinplex.cpp. Also defined if
   compiling the logged calls
*/

#ifdef C_TO_COIN
/*  these void are for void pointers, as the type is COIN solver specific
    and defined in coinplex.cpp only
*/
# define OsiXxxSolverInterface 		void
# define COINprob 			void
#endif

# define CPXENVptr			COINprob*
# define CPXLPptr			COINprob*

typedef int param_id_t;
typedef char sostype_t;

#include "eplex.h"	/* needs declarations above! */


# define SOLVER_SHORT_NAME OSI
# define SOLVER_ATOMIC_NAME "osi"

# define CPXgetrhs(E,A1,A2,A3,A4)	coin_getrhs(A1,A2,A3,A4)
# define CPXgetsense(E,A1,A2,A3,A4)	coin_getrowsense(A1,A2,A3,A4)
# define CPXgetlb(E,A1,A2,A3,A4)	coin_getlb(A1,A2,A3,A4)
# define CPXgetub(E,A1,A2,A3,A4)	coin_getub(A1,A2,A3,A4)
# define CPXgetctype(E,A1,A2,A3,A4)	coin_getcoltype(A1,A2,A3,A4)
# define CPXchgctype(E,A1,A2,A3,A4)	coin_chgcoltype(A1,A2,A3,A4)
# define CPXchgbds(E,A1,A2,A3,A4,A5)	coin_chgbds(A1,A2,A3,A4,A5)
# define CPXcopybase(E,A1,A2,A3)	coin_loadbasis(A1,A2,A3) 
# define CPXgetbase(E,A1,A2,A3)		coin_getbasis(A1,A2,A3) 
# define Get_LP_Objval(A1,A2)		coin_get_lpobjval(A1,A2)
# define Get_Best_Objbound(A1, A2)      coin_get_bestmipbound(A1,A2)
# define CPXgetmipobjval(E,A1,A2)	coin_get_mipobjval(A1,A2)
# define CPXgetobj(E,A1,A2,A3,A4)	coin_get_objcoeffs(A1,A2,A3,A4)
# define CPXchgobj(E,A1,A2,A3,A4)	coin_chg_objcoeffs(A1,A2,A3,A4)
# define CPXcopyorder(E,A1,A2,A3,A4,A5)	coin_get_order(A1,A2,A3,A4,A5,NULL,NULL)
# define CPXchgqpcoef(E,A1,A2,A3,A4)	coin_chgqobj(A1,A2,A3,A4)
# define CPXchgrhs(E,A1,A2,A3,A4)       coin_chgrhs(A1,A2,A3,A4)
# define CPXcopylp(E,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14) \
          coin_loadprob(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13)
# define CPXcopyctype(E,A1,A2)          coin_setcoltype(A1,A2)
# define CPXaddcols(E,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10) \
          coin_addcols(A1,A2,A3,A4,A5,A6,A7,A8,A9) /* diff args! */
# define CPXaddrows(E,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11) \
          coin_addrows(A1,A3,A4,A5,A6,A7,A8,A9) /* diff args! */
# define CPXchgobjsen(E,A1,A2) coin_chgobjsen(A1,A2)
# define CPXgetprobtype(E,A1)		coin_get_probtype(A1)
# define CPXchgprobtype(A1, A2, A3)     0 /* 0 for success return code */
# define CPXaddsos(E,LP,NS,NE,ST,SB,SI,SW,SN) coin_add_sos(LP,NS,NE,ST,SB,SI,SW)
# define CPXgetrows(E,A1,A2,A3,A4,A5,A6,A7,A8,A9) \
          coin_get_row(A1,A2,A4,A5,A8) /* gets one row only! */
# define CPXgetnumnz(E,A1) coin_getnumnz(A1)
# define CPXgetnumint(E,A1) coin_getnumint(A1) /* include binaries */
# define CPXchgname(E,A1,A2,A3,A4) coin_set_name(A1,A2,A3,A4)
# define CPXgetnumbin(E,A1) 0
# define CPXgetnumqpnz(E,A1) 0 /* for now */
# define XPRSdelrows(A1,A2,A3) coin_delrows(A1,A2,A3)
# define XPRSdelcols(A1,A2,A3) coin_delcols(A1,A2,A3)
# define CPXupdatemodel(LP)
# define CPXcloseCPLEX(E) coin_free_prob(*(E))

# define UsingBarrierNoCrossOver(d) 0
# define Get_Bar_Primal_Obj(d, obj) coin_get_bar_primal_objval(d, obj)
# define Get_Bar_Dual_Obj(d, obj) coin_get_bar_dual_objval(d, obj)
# define Bar_Is_Primal_Feasible(lpd) coin_bar_is_primal_feas(lpd->lp)
# define Bar_Is_Dual_Feasible(d) coin_bar_is_dual_feas(lpd->lp)

/* use only where solve_state is defined and set by call to 
   coin_get_result_state()
*/
# define SuccessState(d) (solve_state == state_success)
# define FailState(d) (solve_state == state_fail)
# define MIPSemiSuccessState(d) (solve_state == state_mipsemisucc)
# define MIPSemiFailState(d) (solve_state == state_mipsemifail)
# define LPAbortedState(d) (solve_state == state_lpaborted)
# define UnboundedState(d) (solve_state == state_unbounded)
# define MaybeFailState(d) (solve_state == state_unknown)

# define Get_MIPCutOff(d, v) coin_get_mipcutoff(d, v)
# define DualMethod(lpd,m,am) 0

# define Get_Dual_Infeas(lp, v) coin_get_dual_infeas(lp, v)
# define Get_Primal_Infeas(lp, v) coin_get_primal_infeas(lp, v)

# define SetPreSolve(state)

# define Get_Feasibility_Tolerance(E,L,T) coin_getdblparam((L)->lp, OsiPrimalTolerance, T)

# define Get_Int_Param(E,L,A1,A2) \
   coin_getintparam(((L) == NULL ? E : (L)->lp),A1,A2)
# define Get_Dbl_Param(E,L,A1,A2) \
   coin_getdblparam(((L) == NULL ? E : (L)->lp),A1,A2)
# define Get_Str_Param(E,L,A1,A2) \
   coin_getstrparam(((L) == NULL ? E : (L)->lp),A1,A2)
# define Set_Int_Param(E,L,A1,A2) \
   coin_setintparam(((L) == NULL ? E : (L)->lp),A1,A2)
# define Set_Dbl_Param(E,L,A1,A2) \
   coin_setdblparam(((L) == NULL ? E : (L)->lp),A1,A2)
# define Set_Str_Param(E,L,A1,A2) \
   coin_setstrparam(((L) == NULL ? E : (L)->lp),A1,A2)

/*
# define Set_Str_Param(E,L,A1,A2) \
   ((L) == NULL ? XPRSsetstrcontrol(E,A1,A2) : XPRSsetstrcontrol((L)->lp,A1,A2))
*/

# define SOLVER_MAT_BASE 0
# define SOLVER_MAT_OFFSET 1

#define SOLVER_SENSE_LE	'L'
#define SOLVER_SENSE_GE	'G'
#define SOLVER_SENSE_EQ	'E'

#define SOLVER_SOS_TYPE1	'1'
#define SOLVER_SOS_TYPE2	'2'

# define CPX_COL_AT_LOWER                   3
# define CPX_COL_BASIC                      1
# define CPX_COL_AT_UPPER                   2
# define CPX_COL_FREE_SUPER                 0
# define CPX_COL_NONBASIC_ZERO_BOUNDED	CPX_COL_AT_LOWER
# define CPX_COL_NONBASIC_ZERO_UNBOUNDED CPX_COL_FREE_SUPER

# define CPX_INFBOUND coin_infinity(cpx_env) /* use the default cpx_env */

# define CPXPROB_MILP	PROBLEM_MIP
# define CPXPROB_QP	PROBLEM_QP
# define CPXPROB_MIQP	PROBLEM_MIQP 
# define CPXPROB_LP	PROBLEM_LP

# define CPX_MIN	SENSE_MIN
# define CPX_MAX	SENSE_MAX

# define SOLVER_HAS_STR_PARAMS /* has string parameters */
# define SOLVER_HAS_LOCAL_PARAMETERS
# define STRBUFFERSIZE 256 /* actual string param uses C++ string */

# ifdef COIN_USE_CLP

# define HAS_QUADRATIC /* CLP has quadratic */

# endif


/* solution states used in code to extract information from solver */ 
typedef enum
{
    state_success,
    state_fail,
    state_mipsemisucc,
    state_mipsemifail,
    state_lpaborted,
    state_unbounded,
    state_unknown
} state_t;

/* these are used to return useful information to the user (mainly reason for
   abort in the solve
*/
#define S_UNKNOWN	    0
#define S_SUCCESS	    1
#define S_FAIL		    2
#define S_UNBOUND	    3
#define S_UNBOUND_OR_FAIL   4
#define S_ABORT_UNKNOWN     5
#define S_ABORT_NUM	    6
#define S_ABORT_TIMELIM	    7
#define S_ABORT_NODELIM     8
#define S_ABORT_SOLLIM	    9
#define S_ABORT_LIM	   10
#define S_ABORT_PRIMOBJLIM 11
#define S_ABORT_DUALOBJLIM 12


#ifdef C_TO_COIN
# define EXTERN_C extern
#else
#define EXTERN_C extern "C"
#endif

EXTERN_C int coin_get_objsen(COINprob * lp);
EXTERN_C int coin_get_numcols(COINprob* lp);
EXTERN_C int coin_get_numrows(COINprob* lp);
EXTERN_C int coin_get_probtype(COINprob* lp);
EXTERN_C int coin_getrhs(COINprob * lp, double *rhs, int start, int end);
EXTERN_C int coin_getrowsense(COINprob * lp, char *rsense, int start, int end);
EXTERN_C int coin_getlb(COINprob * lp, double *lb, int start, int end);
EXTERN_C int coin_getub(COINprob * lp, double *ub, int start, int end);
EXTERN_C int coin_getcoltype(COINprob * lp, char *ctype, int start, int end);
EXTERN_C int coin_chgcoltype(COINprob * lp, int cnt, int *idxs, char *ctype);
EXTERN_C int coin_chgbds(COINprob * lp, int cnt, int * idxs, char * lu, double *bd);
EXTERN_C int coin_loadbasis(COINprob * lp, const int *cbase, const int *rbase);
EXTERN_C int coin_getbasis(COINprob * lp, int *cbase, int *rbase);
EXTERN_C int coin_get_lpobjval(lp_desc* lp, double * objvalp);
EXTERN_C int coin_get_mipobjval(COINprob * lp, double * objvalp);
EXTERN_C int coin_get_bestmipbound(COINprob * lp, double * bound);
EXTERN_C int coin_get_objcoeffs(COINprob * lp, double *objc, int start, int end);
EXTERN_C int coin_chg_objcoeffs(COINprob * lp, int cnt, int * idxs, double * values);
EXTERN_C int coin_get_order(COINprob * lp, int cnt, int * idxs, int * prio, int * direction);
EXTERN_C int coin_chgqobj(COINprob * lp, int i, int j, double value);
EXTERN_C int coin_chgrhs(COINprob * lp, int cnt, int * idxs, double * values);
EXTERN_C int coin_loadprob(COINprob* lp, int mac, int mar, int objsen, double* objx, 
	double* rhsx, char* senx, 
	int * matbeg, int* matcnt, int* matind, double* matval, 
	double* lb, double* ub);
EXTERN_C int coin_setcoltype(COINprob* lp, char *ctype);
EXTERN_C int coin_addcols(COINprob* lp, int coladded, int matnz, const double* objx, 
	 int* matbeg, const int* matind, const double* matval, 
	 const double* bdl, const double* bdu);
EXTERN_C int coin_addrows(COINprob* lp, const int rowadded, int nzadded, 
	 const double* rhsx, const char* senx,
	 int* rmatbeg, int* rmatind, double* rmatval);
EXTERN_C int coin_chgobjsen(COINprob* lp, int objsen);
EXTERN_C int coin_get_row(COINprob* lp, int* nnz, int* rmatind, double* rmatval, int idx);
EXTERN_C int coin_delrows(COINprob* lp, int ndr, int* idx);
EXTERN_C int coin_delcols(COINprob* lp, int ndr, int* idx);
EXTERN_C int coin_get_bar_primal_objval(COINprob* lp, double* objval);
EXTERN_C int coin_get_bar_dual_objval(COINprob* lp, double* objval);
EXTERN_C state_t coin_get_result_state(lp_desc* lpd);
EXTERN_C int coin_get_mipcutoff(COINprob* lp, double* bestbound);
EXTERN_C double coin_infinity(COINprob* lp);
EXTERN_C int coin_getdblparam(COINprob* lp, int key, double* value);
EXTERN_C int coin_getintparam(COINprob* lp, int key, int* value);
EXTERN_C int coin_getstrparam(COINprob* lp, int key, char* value);
EXTERN_C int coin_setdblparam(COINprob* lp, int key, double value);
EXTERN_C int coin_setintparam(COINprob* lp, int key, int value);
EXTERN_C int coin_setstrparam(COINprob* lp, int key, const char* value);
EXTERN_C int coin_set_qobj(COINprob* lp, int mac, int cb_cnt, int* cb_index, int*
		  cb_index2, double* cb_value); 
EXTERN_C int coin_get_solver_dblparam(COINprob* lp, int key, double* value);
EXTERN_C int coin_get_solver_intparam(COINprob* lp, int key, int* value);
EXTERN_C int coin_set_solver_dblparam(COINprob* lp, int key, double value);
EXTERN_C int coin_set_solver_intparam(COINprob* lp, int key, int value);
EXTERN_C int coin_get_eplex_strparam(COINprob* lp, int key, char* value);
EXTERN_C int coin_get_eplex_intparam(COINprob* lp, int key, int* value);
EXTERN_C int coin_set_eplex_strparam(COINprob* lp, int key, const char* value);
EXTERN_C int coin_set_eplex_intparam(COINprob* lp, int key, int value);
EXTERN_C int coin_solve_problem(lp_desc* lpd, 
	int meth, int auxmeth, int node_meth, int node_auxmeth);
EXTERN_C int cpx_get_soln_state(lp_desc* lpd, struct lp_sol *sol);
EXTERN_C int coin_set_timeout(COINprob* lp, double timeout);
EXTERN_C int coin_create_prob(COINprob** lp, COINprob* def);
EXTERN_C int coin_reset_prob(lp_desc* lpd);
EXTERN_C int coin_writeprob(COINprob* lp, const char* file, char* otype);
EXTERN_C int coin_readprob(COINprob* lp, const char* file, char* otype);
EXTERN_C int coin_getnumnz(COINprob* lp);
EXTERN_C int coin_getnumint(COINprob* lp);
EXTERN_C int coin_set_name(COINprob* lp, char ntype, int idx, const char* name);
EXTERN_C int coin_get_dual_infeas(COINprob* lp, int* infeas);
EXTERN_C int coin_get_primal_infeas(COINprob* lp, int* infeas);
EXTERN_C int coin_bar_is_primal_feas(COINprob* lp);
EXTERN_C int coin_bar_is_dual_feas(COINprob* lp);
EXTERN_C void coin_get_solver_info(char* info);

