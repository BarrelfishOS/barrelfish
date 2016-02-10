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

#ifdef COIN

static int
cpx_prepare_solve(lp_desc* lpd, struct lp_meth *meth, double timeout)
{
    /* set timeout. If no timeout was set, vtimeout is integer 0 */
    if (timeout > 0.0)
    {
	Log1(coin_set_timeout(lpd->lp, %f), timeout);
	coin_set_timeout(lpd->lp, timeout);
    } 
/*    coin_set_solve_methods(lpd, meth, auxmeth, node_meth, node_auxmeth);*/
    return 0;
}


static int
cpx_solve(lp_desc* lpd, struct lp_meth *meth, struct lp_sol *sol, double* bestbound, double* worstbound)
{
    int solve_state = S_UNKNOWN;  

    Log2({lpd->prob_type = %d; lpd->presolve = %d;}, lpd->prob_type, lpd->presolve);
    Log4(coin_solve_problem(lpd, %d, %d, %d, %d), meth->meth, meth->auxmeth, meth->node_meth, meth->node_auxmeth);
    if (coin_solve_problem(lpd, meth->meth, meth->auxmeth, meth->node_meth, meth->node_auxmeth) == -1)
	return -1;

    /*********************************************************************
     *     Get State Information from External Solver                    *
     *********************************************************************/

    solve_state = coin_get_result_state(lpd);

/* Here we test for various states of the solution. The following macro tests
are defined for all the solvers:
  SuccessState: solution is `optimal' (may be optimal within tolerance)
  FailState: problem is proven infeasible or no MIP solution is better 
	     than cutoff.
  MIPSemiSuccessState: solution exist, but may be suboptimal (MIP only)
  MIPSemiFailState: no solution found yet, but problem not proven 
		    infeasible (MIP only)
  LPAbortedState: LP solve was aborted 
		  (for LP, or  root node LP solve for MIP (not CPLEX)) 
  UnboundedState: problem is unbounded
  MayBeFailState: problem is infeasible or unbounded, but solver cannot 
		  determine which
*/
  
    if (SuccessState(lpd)) {
	lpd->descr_state = DESCR_SOLVED_SOL;
	lpd->optimum_ctr++;
	if (IsMIPProb(lpd->prob_type))
	{
	    CallN(CPXgetmipobjval(cpx_env, lpd->lpcopy, worstbound));
	    /* bestbound may be different from objval because of tolerance */
	    lpd->objval = *worstbound;
	    CallN(Get_Best_Objbound(lpd->lpcopy, bestbound));
	} else
	{
	    CallN(Get_LP_Objval(lpd, &lpd->objval));
	    if (UsingBarrierNoCrossOver(lpd->lp))
	    { 
		CallN(Get_Bar_Primal_Obj(lpd->lp, worstbound));
		CallN(Get_Bar_Dual_Obj(lpd->lp, bestbound));
	    } else 
	    { 
		CallN(Get_LP_Objval(lpd, bestbound));
		*worstbound = *bestbound;
	    }
	}

    } else if (FailState(lpd)) {
	/* for MIP problems, the MIP search may have nodes that were not
	   explored further because of cutoff -- i.e. they cannot lead to
	   solutions better than the cutoff. If no solution is found, the
	   problem is considered infeasible, but strictly it means there is
	   no solution better than cutoff. Unfortunately, it is not easy to
	   know if cutoff had occurred in a MIP search, so bestbound is set
	   to cutoff unless we know otherwise
	*/
	if (DualMethod(lpd, meth->meth, meth->auxmeth)) {
	    lpd->descr_state = DESCR_UNKNOWN_NOSOL;
	    /* dual infeasible ==> primal infeasible or unbounded
	       no information: full interval */
	    *worstbound = (lpd->sense == SENSE_MIN ?  HUGE_VAL : -HUGE_VAL);
	    *bestbound = (lpd->sense ==  SENSE_MIN ? -HUGE_VAL :  HUGE_VAL);
	} else {
	    lpd->descr_state = DESCR_SOLVED_NOSOL;
	    *worstbound = (lpd->sense == SENSE_MIN ? -HUGE_VAL :  HUGE_VAL);
		/* infeasible LP for Xpress, infeasible LP/MIP for COIN */
		*bestbound = (lpd->sense ==  SENSE_MIN ?  HUGE_VAL : -HUGE_VAL);

	}
	lpd->objval = *worstbound;
	lpd->infeas_ctr++;
    } else if (MIPSemiSuccessState(lpd)) {
	lpd->descr_state = DESCR_ABORTED_SOL;
	lpd->abort_ctr++;

	CallN(Get_Best_Objbound(lpd->lpcopy, bestbound));
	CallN(CPXgetmipobjval(cpx_env, lpd->lpcopy, worstbound));
	lpd->objval = *worstbound;

    } else if (MIPSemiFailState(lpd)) {
    /* For Xpress and COIN, the MIPSemiFailState does not include 
       aborting/stopping at the root node solve */
	CallN(Get_Best_Objbound(lpd->lpcopy, bestbound)); 
	*worstbound = (lpd->sense == SENSE_MIN ? HUGE_VAL :  -HUGE_VAL);

	lpd->objval = *worstbound;
	lpd->descr_state = DESCR_ABORTED_NOSOL;
	lpd->abort_ctr++;

    } else if (UnboundedState(lpd)) {
	if (DualMethod(lpd, meth->meth, meth->auxmeth)) {
	    lpd->descr_state = DESCR_SOLVED_NOSOL;
	    lpd->infeas_ctr++;
	    *bestbound = (lpd->sense ==  SENSE_MIN ?  -HUGE_VAL : HUGE_VAL);
	    *worstbound = (lpd->sense == SENSE_MIN ? HUGE_VAL :  -HUGE_VAL);
	} else {
	    lpd->descr_state = DESCR_UNBOUNDED_NOSOL;
	    lpd->abort_ctr++;
	    *bestbound = *worstbound = (lpd->sense == SENSE_MIN ? -HUGE_VAL : HUGE_VAL);
	}
	lpd->objval = *worstbound;

    } else if (MaybeFailState(lpd)) {
	lpd->descr_state = DESCR_UNKNOWN_NOSOL;
	lpd->infeas_ctr++;
	/* no information on bounds */
	*worstbound = (lpd->sense == SENSE_MIN ?  HUGE_VAL : -HUGE_VAL);
	*bestbound = (lpd->sense ==  SENSE_MIN ? -HUGE_VAL :  HUGE_VAL);
	lpd->objval = *worstbound;
    } else if (LPAbortedState(lpd)) {
	/* The exact status of an aborted LP case depends on the solving 
	   method used */
	int attr; /* variable for integer attribute */

	/* meth->meth is used for LP and root MIP LP */

	if (meth->meth == METHOD_DEFAULT) 
	    meth->meth = METHOD_DUAL;

	if (IsMIPProb(lpd->prob_type))
	{ /* MIP search aborted in root LP solve */
	    lpd->descr_state = DESCR_ABORTED_NOSOL;
	    lpd->objval = *worstbound = (lpd->sense == SENSE_MIN ? HUGE_VAL :  -HUGE_VAL);
	    CallN(Get_Best_Objbound(lpd->lpcopy, bestbound));
	    if (meth->meth == METHOD_DUAL)
	    {
		Get_Dual_Infeas(lpd->lpcopy, &attr);
		if (attr == 0)
		{
		    /* attr == 0 ==> we have a feasible dual `solution' for 
		       root node. This is superoptimal, and so can form the
		       best bound for the MIP problem
		    */
		    Get_LP_Objval(lpd, bestbound);
		}
	    }
	} else
	{   /* !IsMIPProb */
	    switch (meth->meth)
	    {
	    case METHOD_DUAL:
		lpd->descr_state = DESCR_ABORTED_NOSOL;
		Get_Dual_Infeas(lpd->lp, &attr);
		/* attr == 0 ==> we have a feasible dual `solution' (i.e.
		   we are in phase II of the Simplex). This
		   is superoptimal for the original problem */
		if (attr == 0)
		    Get_LP_Objval(lpd, bestbound);
		else /* no information on bestbound */
		    *bestbound = (lpd->sense ==  SENSE_MIN ?  -HUGE_VAL : HUGE_VAL);
		lpd->objval = *worstbound = (lpd->sense == SENSE_MIN ? HUGE_VAL :  -HUGE_VAL);
		break;
	    case METHOD_BAR:
		lpd->descr_state = DESCR_ABORTED_NOSOL;
		    if (Bar_Is_Primal_Feasible(lpd))
		    {/* primal feasible */
			Get_Bar_Primal_Obj(lpd->lp, worstbound);
			lpd->objval = *worstbound;
			lpd->descr_state = DESCR_ABORTED_SOL;
		    }
		    if (Bar_Is_Dual_Feasible(lpd))
		    {/* dual feasible */
			Get_Bar_Dual_Obj(lpd->lp, bestbound);
		    }
		    break;
	    case METHOD_PRIMAL:
		Get_Primal_Infeas(lpd->lp, &attr);
		/* attr == 0 ==> we have a feasible primal solution */
		if (attr == 0)
		{
		    Get_LP_Objval(lpd, worstbound);
		    lpd->objval = *worstbound;
		    *bestbound = (lpd->sense ==  SENSE_MIN ? -HUGE_VAL :  HUGE_VAL);
		    lpd->descr_state = DESCR_ABORTED_SOL;
		} else
		{
		    lpd->descr_state = DESCR_ABORTED_NOSOL;
		    *bestbound = (lpd->sense ==  SENSE_MIN ?  -HUGE_VAL : HUGE_VAL);
		    lpd->objval = *worstbound = (lpd->sense == SENSE_MIN ? HUGE_VAL :  -HUGE_VAL);
		}
		break;
	    default:
		/* this should not happen! */
		Fprintf(Current_Error, "Eplex error: Unexpected method case while classifying results. Aborting.\n");
		return -1;
	    }

	} /* !IsMIPProb() */

	lpd->abort_ctr++;


    } else { 
	/* fall back case where we don't have any information */
	lpd->descr_state = DESCR_ABORTED_NOSOL;
	*bestbound = (lpd->sense ==  SENSE_MIN ?  -HUGE_VAL : HUGE_VAL);
	lpd->objval = *worstbound = (lpd->sense == SENSE_MIN ? HUGE_VAL :  -HUGE_VAL);
    }

    return 0;
}


static int
cpx_delsos(lp_desc *lpd, int from, int to)
{
    return coin_del_sos(lpd->lp, from, to);
}


static int
cpx_write(lp_desc *lpd, char *file, char *fmt)
{
    return coin_writeprob(lpd->lp, file, fmt);
}


static int
cpx_read(lp_desc *lpd, char *file, char *fmt)
{
    if (coin_create_prob(&(lpd->lp), cpx_env))
    	return -1;

    if (coin_readprob(lpd->lp, file, fmt))
    	return -1;

    /* initialize non-zero fields in lp_desc */
    CallN(lpd->lpcopy = lpd->lp);  /* no need for a copy */
    lpd->sense = coin_get_objsen(lpd->lp);
    lpd->mac = coin_get_numcols(lpd->lp);
    lpd->mar = coin_get_numrows(lpd->lp);
    lpd->prob_type = coin_get_probtype(lpd->lp);
    return 0;
}


/*
 * Parameter Table
 *
 * defines a table params[] which maps the symbolic names of the optimizer
 * flags to the internal numbers. The set of flags and the numbers differ
 * in each version, therefore this file has to be updated every time.
 * For a new version do the following:
 *
 * extract the CPX_PARAM_ lines out of cplex.h
 * ignore the aliases for old names in ifndef CPLEX_MODERN
 * ignore the boundary definitions CPX_PARAM_ALL_MIN/MAX
 * substitute as follows:
 *
 *   s/^#define[	 ]CPX_PARAM_\([^ ]*\).*$/{"\L\1\E", CPX_PARAM_\1, 0},/
 *
 * mark the int params with 0, the doubles with 1, the strings with 2
 * count the lines and define NUMPARAMS accordingly!
 * add the new section within the proper #if's
 */

#define NUMALIASES 2

# ifdef COIN_USE_CLP
#  define NUMPARAMS 22
#  include "coinplex_params.h"

# else
#  define NUMPARAMS 8
# endif

/* parameters for COIN OSI are more complicated, because there is
   no single uniform source for the parameters. The following types
   are currently defined:

   OSI Params -- common to all OSI based solvers
   =============================================
   0 - OSI integer params
   1 - OSI double  params
   2 - OSI string  params

   Solver(s) specific Params -- not defined by OSI
   ===============================================
   3 - Solver(s) integer params 
   4 - Solver(s) double  params 
   5 - Solver(s) string  params

   Eplex Params -- Params defined in eplex to control solver(s) behaviour
   ====================================================================== 
   6 - Eplex integer params 
   7 - Eplex double  params 
   8 - Eplex string  params

*/
/* these are taken from OsiSolverParameters.hpp */
#define OsiProbName 			0
#define OsiSolverName 			1
#define OsiDualObjectiveLimit 		0
#define OsiPrimallObjectiveLimit 	1
#define OsiDualTolerance 		2
#define OsiPrimalTolerance 		3
#define OsiMaxNumIteration 		0
#define OsiMaxNumIterationHotStart 	1

static struct param_desc params[NUMPARAMS+NUMALIASES] = {
/* OSI */
{"probname", OsiProbName, 2},
{"solvername", OsiSolverName, 2},
{"dualobjectivelimit", OsiDualObjectiveLimit, 1},
{"prinmalobjectivelimit", OsiPrimallObjectiveLimit, 1},
{"dualtolerance", OsiDualTolerance, 1},
{"primaltolerance", OsiPrimalTolerance, 1},
{"maxnumiteration", OsiMaxNumIteration, 0},
{"maxnumiterationhotstart", OsiMaxNumIterationHotStart, 0},

# ifdef COIN_USE_CLP
/* Solver */
{"node_limit", SolverMaxNumNode,  3},
{"solution_limit", SolverMaxNumSol, 3},
{"integrality", SolverIntegerTolerance, 4},
{"absmipgap", SolverAllowableGap, 4},
{"mipgap", SolverAllowableFractionGap, 4},
{"objdifference", SolverCutoffIncrement, 4},
{"absmipheuristicgap", SolverHeuristicGap, 4},
{"mipheuristicgap", SolverHeuristicFractionGap, 4},
{"lppresolve_tol", SolverLPPresolveTolerance, 4},

/* Eplex */
{"bar_ordering", EpxClpParam_bar_ordering, 8},
{"mip_print_freq", EpxClpParam_print_freq, 6},
{"loglevel", EpxClpParam_loglevel, 6},
{"mip_lploglevel", EpxClpParam_mip_lploglevel, 6},
{"bar_doKKT", EpxClpParam_doKKT, 6},
# endif


/*
 * Add some version-independent aliases to the table
 * This must remain at the end of the file!!!
 * If you add lines here, update NUMALIASES above!
 * NUMALIASES lines follow
 */

{"iteration_limit",OsiMaxNumIteration, 0},
{"feasibility_tol", OsiPrimalTolerance, 1},

};

#endif /*COIN*/
