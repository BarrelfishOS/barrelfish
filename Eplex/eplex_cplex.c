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

#ifdef CPLEX

static int
cpx_prepare_solve(lp_desc* lpd, struct lp_meth *meth, double timeout)
{

# if !defined(CPX_HAS_DEFAULTALG)
    /* the default method is dual Simplex in CPLEX 6+7, and we
       assume it here to avoid another switch statement 
    */
    if (meth->meth == METHOD_DEFAULT) meth->meth = METHOD_DUAL;
    if (meth->node_meth == METHOD_DEFAULT) meth->node_meth = METHOD_DUAL;
# endif
	
    /* set timeout. If no timeout was set, timeout is 0 */
    if (timeout > 0.0)
    {
	Log1(CPXsetdblparam(cpx_env, CPX_PARAM_TILIM, %f), timeout);
	CPXsetdblparam(cpx_env, CPX_PARAM_TILIM, timeout);
    } else
    {/* no timeouts, make sure it is set to the default no timeout value */
	CallN(CPXsetdblparam(cpx_env, CPX_PARAM_TILIM, 1e75));
    }

    switch (meth->meth)
    {
#ifdef CPX_ALG_AUTOMATIC
    case METHOD_AUTO:
	SetCPXAlg(cpx_env, CPX_ALG_AUTOMATIC);
	break;
#endif
#ifdef CPX_ALG_CONCURRENT
    case METHOD_CONCURRENT:
	SetCPXAlg(cpx_env, CPX_ALG_CONCURRENT);
	break;
#endif
    case METHOD_PRIMAL:
	SetCPXAlg(cpx_env, CPX_ALG_PRIMAL);
	break;
    case METHOD_DUAL:
	SetCPXAlg(cpx_env, CPX_ALG_DUAL);
	break;
    case METHOD_NET:
	SetCPXAlg(cpx_env, CPX_ALG_NET);
	break;
    case METHOD_BAR:
	SetCPXAlg(cpx_env, CPX_ALG_BARRIER);
#ifdef CPX_PARAM_BARCROSSALG
	switch(meth->auxmeth)
	{
	case METHOD_PRIMAL:
	    CPXsetintparam(cpx_env, CPX_PARAM_BARCROSSALG, CPX_ALG_PRIMAL); break;
	case METHOD_DUAL:
	    CPXsetintparam(cpx_env, CPX_PARAM_BARCROSSALG, CPX_ALG_DUAL); break;
	case METHOD_NONE:
	    CPXsetintparam(cpx_env, CPX_PARAM_BARCROSSALG, CPX_ALG_NONE); break;
	default:
	    Fprintf(Current_Error, "Eplex warning: Specified method unavilable for"
		    " solver. Using default instead.\n");
	    ec_flush(Current_Error);
	    meth->auxmeth = METHOD_DEFAULT;
	case METHOD_DEFAULT:
	    break;
	}
#endif
	break;
# ifdef HAS_SIFT
    case METHOD_SIFT:
	SetCPXAlg(cpx_env, CPX_ALG_SIFTING);

	/* this assumes that the CPX_PARAM_SIFTALG can use the CPX_ALG_*
	   macros. This is not explicitly stated in the manual, even though
	   the macro values corresponds
	*/
	SetSiftAlg(cpx_env, meth->auxmeth);
	break;
# endif
    default:
	Fprintf(Current_Error, "Eplex warning: Specified method unavilable for"
		    " solver. Using default instead.\n");
	ec_flush(Current_Error);
        meth->meth = METHOD_DEFAULT;
# ifdef CPX_HAS_DEFAULTALG
    case METHOD_DEFAULT:
	break;
# else
    case METHOD_DEFAULT:
	SetCPXAlg(cpx_env, CPX_ALG_DUAL);
        meth->meth = METHOD_DUAL;
	break;
# endif
    }

    switch (meth->node_meth)
    {
#ifdef CPX_ALG_AUTOMATIC
    case METHOD_AUTO:
	CPXsetintparam(cpx_env, CPX_PARAM_SUBALG, CPX_ALG_AUTOMATIC);
	break;
#endif
    case METHOD_PRIMAL:
	CPXsetintparam(cpx_env, CPX_PARAM_SUBALG, CPX_ALG_PRIMAL);
	break;
    case METHOD_DUAL:
	CPXsetintparam(cpx_env, CPX_PARAM_SUBALG, CPX_ALG_DUAL);
	break;
    case METHOD_NET:
	CPXsetintparam(cpx_env, CPX_PARAM_SUBALG, CPX_ALG_NET);
	break;
    case METHOD_BAR:
	CPXsetintparam(cpx_env, CPX_PARAM_SUBALG, CPX_ALG_BARRIER);
#ifdef CPX_PARAM_BARCROSSALG
	if (meth->meth != METHOD_BAR)
	    switch(meth->node_auxmeth)
	    {
	    case METHOD_PRIMAL:
		CPXsetintparam(cpx_env, CPX_PARAM_BARCROSSALG, CPX_ALG_PRIMAL); break;
	    case METHOD_DUAL:
		CPXsetintparam(cpx_env, CPX_PARAM_BARCROSSALG, CPX_ALG_DUAL); break;
	    case METHOD_NONE:
		CPXsetintparam(cpx_env, CPX_PARAM_BARCROSSALG, CPX_ALG_NONE); break;
	    default:
		Fprintf(Current_Error, "Eplex warning: Specified method unavilable for"
			" solver. Using default instead.\n");
		ec_flush(Current_Error);
		meth->auxmeth = METHOD_DEFAULT;
	    case METHOD_DEFAULT:
		break;
	    }
#endif
	break;
# ifdef HAS_SIFT
    case METHOD_SIFT:
	CPXsetintparam(cpx_env, CPX_PARAM_SUBALG, CPX_ALG_SIFTING);
	if (meth->meth != METHOD_SIFT  ||  meth->auxmeth == METHOD_DEFAULT)
	{
	    SetSiftAlg(cpx_env, meth->node_auxmeth);
	}
	else if (meth->node_auxmeth != meth->auxmeth)
	{
	    /* SiftAlg was already set above */
	    {
		if (meth->node_auxmeth != METHOD_DEFAULT)
		{
		    Fprintf(Current_Error, "Eplex warning: Sifting methods must "
			    "be the same for root and nodes for a"
			    " MIP.\n Node sifting method ignored\n");
		    ec_flush(Current_Error);
		}
		meth->node_auxmeth = meth->auxmeth;
	    }
	}
	break;
# endif
    default:
	Fprintf(Current_Error, "Eplex warning: Specified node method unavilable for"
		    " solver. Using default instead.\n");
	ec_flush(Current_Error);
	meth->node_meth = METHOD_DEFAULT;
	/*fall through*/
#ifdef CPX_HAS_DEFAULTALG
    case METHOD_DEFAULT:
	break;
#else
    case METHOD_DEFAULT:
	/* no `automatic' default, use the specified default dual instead */
	CPXsetintparam(cpx_env, CPX_PARAM_SUBALG, CPX_ALG_DUAL);
	meth->node_meth = METHOD_DUAL;
	break;
#endif
    }

    return 0;
}



int _lp_opt(lp_desc * lpd, struct lp_meth *meth)
{
    int res;
    switch (meth->meth)
    {
    default:	/* should not happen after cpx_prepare_solve() */
	Fprintf(Current_Error, "Eplex warning: Specified LP method unavilable for"
		" solver. Using default instead.\n");
	ec_flush(Current_Error);
    case METHOD_DEFAULT:
#  ifdef CPX_HAS_LPOPT
    case METHOD_PRIMAL:
    case METHOD_DUAL:
    case METHOD_AUTO:
#ifdef CPX_ALG_CONCURRENT
    case METHOD_CONCURRENT:
#endif
	/* method specified by parameters set earlier */
	Call(res, CPXlpopt(cpx_env, lpd->lp)); break;
#  else
    case METHOD_DUAL:	Call(res, CPXdualopt(cpx_env, lpd->lp)); break;
    case METHOD_PRIMAL:	Call(res, CPXprimopt(cpx_env, lpd->lp)); break;
#  endif
    case METHOD_NET:
	switch (meth->auxmeth)
	{
	case METHOD_PRIMAL:
	    Call(res, CPXhybnetopt(cpx_env, lpd->lp, CPX_ALG_PRIMAL)); break;
	default:	/* should not happen after cpx_prepare_solve() */
	    Fprintf(Current_Error, "Eplex warning: Invalid Simplex"
		    " method selected for Network-Simplex.\nUsing default.\n");
	    ec_flush(Current_Error);
	case METHOD_DEFAULT:
	    /* use CPXlpopt() if it exists, otherwise assume that the
	       default is dual Simplex
	    */
#ifdef CPX_HAS_LPOPT
	case METHOD_AUTO:
	    Call(res, CPXlpopt(cpx_env, lpd->lp));
	    break;
#endif
	case METHOD_DUAL:	
	    Call(res, CPXhybnetopt(cpx_env, lpd->lp, CPX_ALG_DUAL));
	    break;
	}
	break;

    case METHOD_BAR:
	switch (meth->auxmeth)
	{
#ifndef HAS_NO_BARCROSSOVER
	case METHOD_NONE:
	    Call(res, CPXhybbaropt(cpx_env, lpd->lp, CPX_ALG_NONE));
	    break;
#endif
	case METHOD_PRIMAL:
	    Call(res, CPXhybbaropt(cpx_env, lpd->lp, CPX_ALG_PRIMAL));
	    break;
	case METHOD_DUAL:	
	    Call(res, CPXhybbaropt(cpx_env, lpd->lp, CPX_ALG_DUAL));
	    break;
	default:	/* should not happen after cpx_prepare_solve() */
	    Fprintf(Current_Error, "Eplex warning: Selected crossover"
		    " method for Barrier method unavailable.\nUsing default crossover.\n");
	    ec_flush(Current_Error);
	case METHOD_DEFAULT:
#ifdef CPX_HAS_LPOPT
	    Call(res, CPXlpopt(cpx_env, lpd->lp));
#else
	    Call(res, CPXbaropt(cpx_env, lpd->lp));
#endif
	    break;
	}
	break; 

#ifdef HAS_SIFT
    case METHOD_SIFT:
	Call(res, CPXlpopt(cpx_env, lpd->lp));
	break;
#endif
    }
    return res;
} 

#define TryMIPBeforeFixing(res) { \
    /* check if there is already a current optimal MI(Q)P solution */\
    lpd->sol_state = CPXgetstat(cpx_env, lpd->lp);   \
    if (!MIPSuccessState(lpd)) \
    { \
        /* solve the MIP problem first */\
	Call(res, CPXmipopt(cpx_env, lpd->lp)); \
	lpd->sol_itcnt = lpd->sol_nodnum = 0; \
	lpd->sol_itcnt = CPXgetmipitcnt(cpx_env, lpd->lp); \
	lpd->sol_nodnum = CPXgetnodecnt(cpx_env, lpd->lp); \
    } else { \
        res = 0; \
    } \
}


static int
cpx_solve(lp_desc* lpd, struct lp_meth *meth, struct lp_sol *sol, double* bestbound, double* worstbound)
{
    int res;

    switch (lpd->prob_type)
    {

	case PROBLEM_RELAXEDL:
# ifdef HAS_RELAXEDLP
	    CallN(CPXchgprobtype(cpx_env, lpd->lp, CPXPROB_RELAXED));
# else
	    TryFree(lpd->ctype);
	    lpd->ctype = (char *) Malloc(lpd->mac * sizeof(char));
	    /* mac > 0 because we have a MIP problem */
	    CPXgetctype(cpx_env, lpd->lp, lpd->ctype, 0, (lpd->mac - 1));
	    CallN(CPXchgprobtype(cpx_env, lpd->lp, CPXPROB_LP));
# endif
	    res = _lp_opt(lpd, meth);
	    break;

	case PROBLEM_RELAXEDQ:
	    TryFree(lpd->ctype);
	    lpd->ctype = (char *) Malloc(lpd->mac * sizeof(char));
	    /* mac > 0 because we have a MIP problem */
	    CPXgetctype(cpx_env, lpd->lp, lpd->ctype, 0, (lpd->mac - 1));
	    CallN(CPXchgprobtype(cpx_env, lpd->lp, CPXPROB_QP));
	    Call(res, CPXqpopt(cpx_env, lpd->lp));
	    break;

	case PROBLEM_FIXEDL:
	    TryMIPBeforeFixing(res);
	    if (res != 0) break; 
	    Call(res,CPXchgprobtype(cpx_env, lpd->lp, CPXPROB_FIXEDMILP)); 
	    if (res != 0) { 
		/* reset res if no integer solution so the solution status for 
		   the previous CPXmipopt() will be obtained */ 
		if (res == CPXERR_NO_INT_SOLN) res = 0; 
		break; 
	    } 
	    /* if no error, falls through and solve as (fixed) LP  */ 

	case PROBLEM_LP:
	    res = _lp_opt(lpd, meth);
	    break; /* PROBLEM_LP */

# ifdef HAS_MIQP

	case PROBLEM_FIXEDQ:
	    TryMIPBeforeFixing(res);
	    if (res != 0) break; 
	    Call(res,CPXchgprobtype(cpx_env, lpd->lp, CPXPROB_FIXEDMIQP)); 
	    if (res != 0) { 
		/* reset res if no integer solution so the solution status for 
		   the previous CPXmipopt() will be obtained */ 
		if (res == CPXERR_NO_INT_SOLN) res = 0; 
		break; 
	    } 
	    /* if no error, falls through and solve as (fixed) QP  */ 

# endif
	case PROBLEM_QP:
	    Call(res, CPXqpopt(cpx_env, lpd->lp));
	    break;


	case PROBLEM_MIP:
	case PROBLEM_MIQP:
	    Call(res, CPXmipopt(cpx_env, lpd->lp));
	    break;

	default:
	    return -1;
    } /* switch lpd->prob_type */


    /*********************************************************************
     *     Get State Information from External Solver                    *
     *********************************************************************/

    /* sol_state is given the solution status unless an error has occurred
       (res != 0), in which case the error code is return unless the error
       is CPXERR_SUBPROB_SOLVE, where the MIP was aborted and there is a
       valid solution status
    */
    lpd->sol_state = res ? 
	(res  == CPXERR_SUBPROB_SOLVE ? CPXgetstat(cpx_env, lpd->lp) : res) :
	CPXgetstat(cpx_env, lpd->lp); 

    if ((lpd->prob_type == PROBLEM_FIXEDL && 
	 CPXgetprobtype(cpx_env, lpd->lp) == CPXPROB_FIXEDMILP)
# ifdef HAS_MIQP
	|| (lpd->prob_type == PROBLEM_FIXEDQ && 
	    CPXgetprobtype(cpx_env, lpd->lp) == CPXPROB_FIXEDMIQP)
# endif
	)
    {/* add in the counts for the fixedmilp solve */ 
	lpd->sol_itcnt += CPXgetitcnt(cpx_env, lpd->lp);
    }
    else
    {
	lpd->sol_itcnt = IsMIPProb(lpd->prob_type) ?
	    CPXgetmipitcnt(cpx_env, lpd->lp) : CPXgetitcnt(cpx_env, lpd->lp);
	lpd->sol_nodnum = IsMIPProb(lpd->prob_type) ?
	    CPXgetnodecnt(cpx_env, lpd->lp) : 0;
    }

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
	    /* *bestbound may be different from objval because of tolerance */
	    lpd->objval = *worstbound;
#ifdef NO_MIPBESTBOUND_IF_OPTIMAL
	    if (lpd->sol_state == CPXMIP_OPTIMAL_TOL)
	    { /* bestobjval only available if nodes have been cutoff! */
		CallN(Get_Best_Objbound(lpd->lpcopy, bestbound));
	    } else
	    {
		CallN(CPXgetmipobjval(cpx_env, lpd->lpcopy, bestbound));
	    }
#else
	    CallN(Get_Best_Objbound(lpd->lpcopy, bestbound));
#endif
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
	   know if cutoff had occurred in a MIP search, so *bestbound is set
	   to cutoff unless we know otherwise
	*/
#ifdef UNIFORM_SOL_STAT
	lpd->descr_state = DESCR_SOLVED_NOSOL;
	*worstbound = (lpd->sense == SENSE_MIN ? -HUGE_VAL :  HUGE_VAL);
	if (IsMIPProb(lpd->prob_type))
	{
	    Get_MIPCutOff(lpd, bestbound);
	} else
	    *bestbound = (lpd->sense ==  SENSE_MIN ?  HUGE_VAL : -HUGE_VAL);
#else
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
	    if (IsMIPProb(lpd->prob_type))
	    {
		Get_MIPCutOff(lpd, bestbound);
	    } else
		*bestbound = (lpd->sense ==  SENSE_MIN ?  HUGE_VAL : -HUGE_VAL);

	}
#endif /* !UNIFORM_SOL_STAT */
	lpd->objval = *worstbound;
	lpd->infeas_ctr++;
    } else if (MIPSemiSuccessState(lpd)) {
	/* For the MIPSemiSuccessState to test correctly, we need the MIP
	   solution status. However, if the MIP solve aborted in a LP
	   node, the solution status for that LP solve is more informative.
	   res must still be the return code from the optimisation call.
	*/
	if (res == CPXERR_SUBPROB_SOLVE) lpd->sol_state = CPXgetsubstat(cpx_env, lpd->lp); 
	lpd->descr_state = DESCR_ABORTED_SOL;
	lpd->abort_ctr++;

	CallN(Get_Best_Objbound(lpd->lpcopy, bestbound));
	CallN(CPXgetmipobjval(cpx_env, lpd->lpcopy, worstbound));
	lpd->objval = *worstbound;

    } else if (MIPSemiFailState(lpd)) {
    /* similar to MIPSemiSuccessState, but need to check if we have
       aborted at the root node solve, in which case there is no valid 
       bestobjval  */
	if (res == CPXERR_SUBPROB_SOLVE) 
	{
	    lpd->sol_state = CPXgetsubstat(cpx_env, lpd->lp); 
	    if (lpd->sol_nodnum > 0)
	    {/* did not abort at root node */
		CallN(Get_Best_Objbound(lpd->lpcopy, bestbound));
	    } else /* no valid bestobjval... */
		*bestbound = (lpd->sense == SENSE_MIN ? -HUGE_VAL :  HUGE_VAL);
	} else
	{
	    CallN(Get_Best_Objbound(lpd->lpcopy, bestbound));
	}
	*worstbound = (lpd->sense == SENSE_MIN ? HUGE_VAL :  -HUGE_VAL);

	lpd->objval = *worstbound;
	lpd->descr_state = DESCR_ABORTED_NOSOL;
	lpd->abort_ctr++;

    } else if (UnboundedState(lpd)) {
#ifdef UNIFORM_SOL_STAT
# if CPLEX >= 8
	int cpxmeth, type, isprimf, isdualf;

	/* CPLEX 8+ redefined the `unbounded' state to mean an unbounded
	   ray is detected, i.e. that solution will be unbounded *if* a
	   feasible solution exists, but feasiblity may be unproven
	*/
	CPXsolninfo(cpx_env, lpd->lp, &cpxmeth, &type, &isprimf, &isdualf);
	if (isprimf) {
	    /* is primary feasible, i.e. has feasible solution and is 
	       unbounded
	    */
	    lpd->descr_state = DESCR_UNBOUNDED_NOSOL;
	    lpd->abort_ctr++;
	    *bestbound = *worstbound = (lpd->sense == SENSE_MIN ? -HUGE_VAL : HUGE_VAL);
	} else {
	    /* not proven to be primary feasible */
	    lpd->descr_state = DESCR_UNKNOWN_NOSOL;
	    lpd->infeas_ctr++;
	    /* no information on bounds */
	    *worstbound = (lpd->sense == SENSE_MIN ?  HUGE_VAL : -HUGE_VAL);
	    *bestbound = (lpd->sense ==  SENSE_MIN ? -HUGE_VAL :  HUGE_VAL);
	    lpd->objval = *worstbound;
	}

# else
	lpd->descr_state = DESCR_UNBOUNDED_NOSOL;
	lpd->abort_ctr++;
	*bestbound = *worstbound = (lpd->sense == SENSE_MIN ? -HUGE_VAL : HUGE_VAL);
# endif

#else
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
#endif
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
# if CPLEX >= 8
	int cpxmeth, type, isprimf, isdualf;

	CPXsolninfo(cpx_env, lpd->lp, &cpxmeth, &type, &isprimf, &isdualf);

	/* initial *worstbound and objval, may be revised */
	lpd->objval = *worstbound = (lpd->sense == SENSE_MIN ? HUGE_VAL :  -HUGE_VAL);
	if (isprimf)
	{
	    lpd->descr_state = DESCR_ABORTED_SOL;
	    if (cpxmeth == CPX_ALG_BARRIER)
		CPXgetdblquality(cpx_env, lpd->lp, worstbound, CPX_PRIMAL_OBJ);
	    else 
		Get_LP_Objval(lpd, worstbound);
	    lpd->objval = *worstbound; 

	} else
	{
	    lpd->descr_state = DESCR_ABORTED_NOSOL;
	}
	if (isdualf)
	{
	    if (cpxmeth == CPX_ALG_BARRIER)
		CPXgetdblquality(cpx_env, lpd->lp, bestbound, CPX_DUAL_OBJ);
	    else
		Get_LP_Objval(lpd, bestbound);
	} else
	{
	    *bestbound = (lpd->sense == SENSE_MIN ?  -HUGE_VAL : HUGE_VAL);
	}

	lpd->abort_ctr++;

# else /* CPLEX < 8 */

	if (LPAbortedNoSolState(lpd))
	{ /* no feasible solution, no information on bounds */
	    lpd->descr_state = DESCR_ABORTED_NOSOL;
	    lpd->objval = *worstbound = (lpd->sense == SENSE_MIN ?  HUGE_VAL : -HUGE_VAL);
	    *bestbound = (lpd->sense ==  SENSE_MIN ? -HUGE_VAL :  HUGE_VAL);
	} else
	{
	    switch (meth->meth)
	    {
	    case METHOD_DEFAULT: /* default assumed to be dual simplex! */
	    case METHOD_DUAL: /* in phase II, with feasible dual sol. */
		Get_LP_Objval(lpd, bestbound);
		lpd->objval = *worstbound = (lpd->sense == SENSE_MIN ?  HUGE_VAL : -HUGE_VAL);
		lpd->descr_state = DESCR_ABORTED_NOSOL;
		break;
	    case METHOD_PRIMAL: /* in phase II, with feasible primal sol.*/
	    case METHOD_BAR: /* has feasible primal sol */
		Get_LP_Objval(lpd, worstbound);
		lpd->objval = *worstbound;
		*bestbound = (lpd->sense == SENSE_MIN ?  -HUGE_VAL : HUGE_VAL);
		lpd->descr_state = DESCR_ABORTED_SOL;
		break;
	    default:
		/* just assume there is no valid information */
		lpd->descr_state = DESCR_ABORTED_NOSOL;
		lpd->objval = *worstbound = (lpd->sense == SENSE_MIN ?  HUGE_VAL : -HUGE_VAL);
		*bestbound = (lpd->sense ==  SENSE_MIN ? -HUGE_VAL :  HUGE_VAL);
		break;
	    }
	}

	lpd->abort_ctr++;

# endif

    } else { 
	/* fall back case where we don't have any information */
	lpd->descr_state = DESCR_ABORTED_NOSOL;
	*bestbound = (lpd->sense ==  SENSE_MIN ?  -HUGE_VAL : HUGE_VAL);
	lpd->objval = *worstbound = (lpd->sense == SENSE_MIN ? HUGE_VAL :  -HUGE_VAL);
    }

    return 0;
}


static int
cpx_get_soln_state(lp_desc* lpd, struct lp_sol *sol)
{
    int res;

    if (lpd->mac > 0)	/* columns/variables */
    {
	if (sol->sols && (IsMIPProb(lpd->prob_type)
		? CPXgetmipx(cpx_env, lpd->lp, sol->sols, 0, lpd->mac-1)
		: CPXgetx(cpx_env, lpd->lp, sol->sols, 0, lpd->mac-1)))
	    return -1;
	if (sol->djs && CPXgetdj(cpx_env, lpd->lp, sol->djs, 0, lpd->mac-1))
	    return -1;
	if (sol->cbase && CPXgetbase(cpx_env, lpd->lp, sol->cbase, sol->rbase))
	    return -1;
    }
    if (lpd->mar > 0)	/* rows/constraints */
    {
	if (sol->slacks && (IsMIPProb(lpd->prob_type)
		? CPXgetmipslack(cpx_env, lpd->lp, sol->slacks, 0, lpd->mar-1)
		: CPXgetslack(cpx_env, lpd->lp, sol->slacks, 0, lpd->mar-1)))
	    return -1;
	if (sol->pis && CPXgetpi(cpx_env, lpd->lp, sol->pis, 0, lpd->mar-1))
	    return -1;
   }
   return 0;
}


static int
cpx_delsos(lp_desc *lpd, int from, int to)
{
    int total = CPXgetnumsos(cpx_env, lpd->lp);
    int *delset = Malloc(total*sizeof(int));
    int i=0;
    while (i < from)  delset[i++] = 0;
    while (i < to)    delset[i++] = 1;
    while (i < total) delset[i++] = 0;
    if (CPXdelsetsos(cpx_env, lpd->lp, delset))
	return -1;
    Free(delset);
    return 0;
}


static int
cpx_write(lp_desc *lpd, char *file, char *fmt)
{
    int res;

    if (strcmp(fmt, "pre") == 0)
    {
	double objoff;
	res = CPXpreslvwrite(cpx_env, lpd->lp, file, &objoff);
    }
    else if (strcmp(fmt, "bas") == 0)
    {
	res = CPXmbasewrite(cpx_env, lpd->lp, file);
    }
    else if (strcmp(fmt, "emb") == 0)
    {
	res = CPXembwrite(cpx_env, lpd->lp, file);
    }
# if defined(CPLEX) && CPLEX < 10
    /* removed in CPLEX 10 */
    else if (strcmp(fmt, "tre") == 0)
    {
	res = CPXtreewrite(cpx_env, lpd->lp, file);
    }
# endif
    else
    {
	res = CPXwriteprob(cpx_env, lpd->lp, file, fmt);
    }
    return res;
}


static int
cpx_read(lp_desc *lpd, char *file, char *fmt)
{
    int res;

    lpd->lp = CPXcreateprob(cpx_env, &res, "eclipse");
    if (lpd->lp == NULL)
	return -1;

    CallN(lpd->lpcopy = lpd->lp);  /* no need for a copy in CPLEX */

    if (CPXreadcopyprob(cpx_env, lpd->lp, file, fmt))
	return -1;

    lpd->sense = CPXgetobjsen(cpx_env, lpd->lp) == CPX_MIN ? SENSE_MIN : SENSE_MAX;
    lpd->mac = CPXgetnumcols(cpx_env, lpd->lp);
    lpd->mar = CPXgetnumrows(cpx_env, lpd->lp);

    switch(CPXgetprobtype(cpx_env, lpd->lp))
    {
    case CPXPROB_MILP:
	lpd->prob_type = PROBLEM_MIP;
	break;
    case CPXPROB_QP:
	lpd->prob_type = PROBLEM_QP;
	break;
# ifdef HAS_MIQP
    case CPXPROB_MIQP:
	lpd->prob_type = PROBLEM_MIQP;
	break;
# endif
    default:
    	lpd->prob_type = PROBLEM_LP;
	break;
    }
    CPXgetintparam(cpx_env, CPX_PARAM_PREIND, &lpd->presolve);
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

#define NUMALIASES 44

#if CPLEX==7 && (CPLEXMINOR>=1)

#define NUMPARAMS 129
static struct param_desc params[NUMPARAMS+NUMALIASES] = {
{"advind", CPX_PARAM_ADVIND, 0},
{"aggfill", CPX_PARAM_AGGFILL, 0},
{"aggind", CPX_PARAM_AGGIND, 0},
{"basinterval", CPX_PARAM_BASINTERVAL, 0},
{"cfilemul", CPX_PARAM_CFILEMUL, 0},
{"clocktype", CPX_PARAM_CLOCKTYPE, 0},
{"craind", CPX_PARAM_CRAIND, 0},
{"depind", CPX_PARAM_DEPIND, 0},
{"dpriind", CPX_PARAM_DPRIIND, 0},
{"pricelim", CPX_PARAM_PRICELIM, 0},
{"rimreadlim", CPX_PARAM_RIMREADLIM, 0},
{"rimnzreadlim", CPX_PARAM_RIMNZREADLIM, 0},
{"epmrk", CPX_PARAM_EPMRK, 1},
{"epopt", CPX_PARAM_EPOPT, 1},
{"epper", CPX_PARAM_EPPER, 1},
{"eprhs", CPX_PARAM_EPRHS, 1},
{"fastmip", CPX_PARAM_FASTMIP, 0},
{"iisind", CPX_PARAM_IISIND, 0},
{"simdisplay", CPX_PARAM_SIMDISPLAY, 0},
{"itlim", CPX_PARAM_ITLIM, 0},
{"rowreadlim", CPX_PARAM_ROWREADLIM, 0},
{"netfind", CPX_PARAM_NETFIND, 0},
{"colreadlim", CPX_PARAM_COLREADLIM, 0},
{"nzreadlim", CPX_PARAM_NZREADLIM, 0},
{"objllim", CPX_PARAM_OBJLLIM, 1},
{"objulim", CPX_PARAM_OBJULIM, 1},
{"perind", CPX_PARAM_PERIND, 0},
{"perlim", CPX_PARAM_PERLIM, 0},
{"ppriind", CPX_PARAM_PPRIIND, 0},
{"preind", CPX_PARAM_PREIND, 0},
{"reinv", CPX_PARAM_REINV, 0},
{"reverseind", CPX_PARAM_REVERSEIND, 0},
{"rfilemul", CPX_PARAM_RFILEMUL, 0},
{"scaind", CPX_PARAM_SCAIND, 0},
{"scrind", CPX_PARAM_SCRIND, 0},
{"simthreads", CPX_PARAM_SIMTHREADS, 0},
{"singlim", CPX_PARAM_SINGLIM, 0},
{"singtol", CPX_PARAM_SINGTOL, 0},
{"tilim", CPX_PARAM_TILIM, 1},
{"xxxind", CPX_PARAM_XXXIND, 0},
{"effslackind", CPX_PARAM_EFFSLACKIND, 0},
{"predual", CPX_PARAM_PREDUAL, 0},
{"rowgrowth", CPX_PARAM_ROWGROWTH, 0},
{"colgrowth", CPX_PARAM_COLGROWTH, 0},
{"nzgrowth", CPX_PARAM_NZGROWTH, 0},
{"epopt_h", CPX_PARAM_EPOPT_H, 0},
{"eprhs_h", CPX_PARAM_EPRHS_H, 0},
{"prepass", CPX_PARAM_PREPASS, 0},
{"datacheck", CPX_PARAM_DATACHECK, 0},
{"reduce", CPX_PARAM_REDUCE, 0},
{"prelinear", CPX_PARAM_PRELINEAR, 0},
{"lpmethod", CPX_PARAM_LPMETHOD, 0},
{"workdir", CPX_PARAM_WORKDIR, 2},
{"workmem", CPX_PARAM_WORKMEM, 1},
{"precompress", CPX_PARAM_PRECOMPRESS, 0},
{"bardstart", CPX_PARAM_BARDSTART, 0},
{"barepcomp", CPX_PARAM_BAREPCOMP, 1},
{"bargrowth", CPX_PARAM_BARGROWTH, 1},
{"barobjrng", CPX_PARAM_BAROBJRNG, 1},
{"barpstart", CPX_PARAM_BARPSTART, 0},
{"barvarup", CPX_PARAM_BARVARUP, 1},
{"baralg", CPX_PARAM_BARALG, 0},
{"barcolnz", CPX_PARAM_BARCOLNZ, 0},
{"bardisplay", CPX_PARAM_BARDISPLAY, 0},
{"baritlim", CPX_PARAM_BARITLIM, 0},
{"barmaxcor", CPX_PARAM_BARMAXCOR, 0},
{"barorder", CPX_PARAM_BARORDER, 0},
{"barrowsden", CPX_PARAM_BARROWSDEN, 0},
{"barthreads", CPX_PARAM_BARTHREADS, 0},
{"barstartalg", CPX_PARAM_BARSTARTALG, 0},
{"barcrossalg", CPX_PARAM_BARCROSSALG, 0},
{"barooc", CPX_PARAM_BAROOC, 0},
{"brdir", CPX_PARAM_BRDIR, 0}, 
{"bttol", CPX_PARAM_BTTOL, 1},
{"cliques", CPX_PARAM_CLIQUES, 0},
{"coeredind", CPX_PARAM_COEREDIND, 0},
{"covers", CPX_PARAM_COVERS, 0},
{"cutlo", CPX_PARAM_CUTLO, 1},
{"cutup", CPX_PARAM_CUTUP, 1},
{"epagap", CPX_PARAM_EPAGAP, 1},
{"epgap", CPX_PARAM_EPGAP, 1},
{"epint", CPX_PARAM_EPINT, 1},
{"heuristic", CPX_PARAM_HEURISTIC, 0},
{"mipdisplay", CPX_PARAM_MIPDISPLAY, 0},
{"mipinterval", CPX_PARAM_MIPINTERVAL, 0},
{"mipthreads", CPX_PARAM_MIPTHREADS, 0},
{"intsollim", CPX_PARAM_INTSOLLIM, 0},
{"nodefileind", CPX_PARAM_NODEFILEIND, 0},
{"nodelim", CPX_PARAM_NODELIM, 0},
{"nodesel", CPX_PARAM_NODESEL, 0},
{"objdif", CPX_PARAM_OBJDIF, 1},
{"mipordind", CPX_PARAM_MIPORDIND, 0},
{"relobjdif", CPX_PARAM_RELOBJDIF, 1},
{"startalg", CPX_PARAM_STARTALG, 0},
{"subalg", CPX_PARAM_SUBALG, 0},
{"trelim", CPX_PARAM_TRELIM, 1},
{"varsel", CPX_PARAM_VARSEL, 0},
{"bndstrenind", CPX_PARAM_BNDSTRENIND, 0},
{"heurfreq", CPX_PARAM_HEURFREQ, 0},
{"mipordtype", CPX_PARAM_MIPORDTYPE, 0},
{"cutsfactor", CPX_PARAM_CUTSFACTOR, 1},
{"relaxpreind", CPX_PARAM_RELAXPREIND, 0},
{"mipstart", CPX_PARAM_MIPSTART, 0},
{"preslvnd", CPX_PARAM_PRESLVND, 0},
{"bbinterval", CPX_PARAM_BBINTERVAL, 0},
{"flowcovers", CPX_PARAM_FLOWCOVERS, 0},
{"implbd", CPX_PARAM_IMPLBD, 0},
{"probe", CPX_PARAM_PROBE, 0},
{"gubcovers", CPX_PARAM_GUBCOVERS, 0},
{"strongcandlim", CPX_PARAM_STRONGCANDLIM, 0},
{"strongitlim", CPX_PARAM_STRONGITLIM, 0},
{"strongthreadlim", CPX_PARAM_STRONGTHREADLIM, 0},
{"fraccand", CPX_PARAM_FRACCAND, 0},
{"fraccuts", CPX_PARAM_FRACCUTS, 0},
{"fracpass", CPX_PARAM_FRACPASS, 0},
{"flowpaths", CPX_PARAM_FLOWPATHS, 0},
{"mircuts", CPX_PARAM_MIRCUTS, 0},
{"disjcuts", CPX_PARAM_DISJCUTS, 0},
{"aggcutlim", CPX_PARAM_AGGCUTLIM, 0},
{"mipcbredlp", CPX_PARAM_MIPCBREDLP, 0},
{"cutpass", CPX_PARAM_CUTPASS, 0},
{"mipemphasis", CPX_PARAM_MIPEMPHASIS, 0},
{"netitlim", CPX_PARAM_NETITLIM, 0},
{"netepopt", CPX_PARAM_NETEPOPT, 1},
{"neteprhs", CPX_PARAM_NETEPRHS, 1},
{"netppriind", CPX_PARAM_NETPPRIIND, 0},
{"netdisplay", CPX_PARAM_NETDISPLAY, 0},
{"qpnzreadlim", CPX_PARAM_QPNZREADLIM, 0},
{"qpnzgrowth", CPX_PARAM_QPNZGROWTH, 0},

#elif CPLEX==8

#define NUMPARAMS 134
static struct param_desc params[NUMPARAMS+NUMALIASES] = {
{"advind", CPX_PARAM_ADVIND, 0},
{"aggfill", CPX_PARAM_AGGFILL, 0},
{"aggind", CPX_PARAM_AGGIND, 0},
{"basinterval", CPX_PARAM_BASINTERVAL, 0},
{"cfilemul", CPX_PARAM_CFILEMUL, 0},
{"clocktype", CPX_PARAM_CLOCKTYPE, 0},
{"craind", CPX_PARAM_CRAIND, 0},
{"depind", CPX_PARAM_DEPIND, 0},
{"dpriind", CPX_PARAM_DPRIIND, 0},
{"pricelim", CPX_PARAM_PRICELIM, 0},
{"epmrk", CPX_PARAM_EPMRK, 1},
{"epopt", CPX_PARAM_EPOPT, 1},
{"epper", CPX_PARAM_EPPER, 1},
{"eprhs", CPX_PARAM_EPRHS, 1},
{"fastmip", CPX_PARAM_FASTMIP, 0},
{"iisind", CPX_PARAM_IISIND, 0},
{"simdisplay", CPX_PARAM_SIMDISPLAY, 0},
{"itlim", CPX_PARAM_ITLIM, 0},
{"rowreadlim", CPX_PARAM_ROWREADLIM, 0},
{"netfind", CPX_PARAM_NETFIND, 0},
{"colreadlim", CPX_PARAM_COLREADLIM, 0},
{"nzreadlim", CPX_PARAM_NZREADLIM, 0},
{"objllim", CPX_PARAM_OBJLLIM, 1},
{"objulim", CPX_PARAM_OBJULIM, 1},
{"perind", CPX_PARAM_PERIND, 0},
{"perlim", CPX_PARAM_PERLIM, 0},
{"ppriind", CPX_PARAM_PPRIIND, 0},
{"preind", CPX_PARAM_PREIND, 0},
{"reinv", CPX_PARAM_REINV, 0},
{"reverseind", CPX_PARAM_REVERSEIND, 0},
{"rfilemul", CPX_PARAM_RFILEMUL, 0},
{"scaind", CPX_PARAM_SCAIND, 0},
{"scrind", CPX_PARAM_SCRIND, 0},
{"simthreads", CPX_PARAM_SIMTHREADS, 0},
{"singlim", CPX_PARAM_SINGLIM, 0},
{"singtol", CPX_PARAM_SINGTOL, 0},
{"tilim", CPX_PARAM_TILIM, 1},
{"xxxind", CPX_PARAM_XXXIND, 0},
{"predual", CPX_PARAM_PREDUAL, 0},
{"rowgrowth", CPX_PARAM_ROWGROWTH, 0},
{"colgrowth", CPX_PARAM_COLGROWTH, 0},
{"nzgrowth", CPX_PARAM_NZGROWTH, 0},
{"epopt_h", CPX_PARAM_EPOPT_H, 0},
{"eprhs_h", CPX_PARAM_EPRHS_H, 0},
{"prepass", CPX_PARAM_PREPASS, 0},
{"datacheck", CPX_PARAM_DATACHECK, 0},
{"reduce", CPX_PARAM_REDUCE, 0},
{"prelinear", CPX_PARAM_PRELINEAR, 0},
{"lpmethod", CPX_PARAM_LPMETHOD, 0},
{"qpmethod", CPX_PARAM_QPMETHOD, 0},
{"workdir", CPX_PARAM_WORKDIR, 2},
{"workmem", CPX_PARAM_WORKMEM, 1},
{"precompress", CPX_PARAM_PRECOMPRESS, 0},
{"threads", CPX_PARAM_THREADS, 0},
{"siftdisplay", CPX_PARAM_SIFTDISPLAY, 0},
{"siftalg", CPX_PARAM_SIFTALG, 0},
{"siftitlim", CPX_PARAM_SIFTITLIM, 0}, 
{"bardstart", CPX_PARAM_BARDSTART, 0},
{"barepcomp", CPX_PARAM_BAREPCOMP, 1},
{"bargrowth", CPX_PARAM_BARGROWTH, 1},
{"barobjrng", CPX_PARAM_BAROBJRNG, 1},
{"barpstart", CPX_PARAM_BARPSTART, 0},
{"barvarup", CPX_PARAM_BARVARUP, 1},
{"baralg", CPX_PARAM_BARALG, 0},
{"barcolnz", CPX_PARAM_BARCOLNZ, 0},
{"bardisplay", CPX_PARAM_BARDISPLAY, 0},
{"baritlim", CPX_PARAM_BARITLIM, 0},
{"barmaxcor", CPX_PARAM_BARMAXCOR, 0},
{"barorder", CPX_PARAM_BARORDER, 0},
{"barrowsden", CPX_PARAM_BARROWSDEN, 0},
{"barthreads", CPX_PARAM_BARTHREADS, 0},
{"barstartalg", CPX_PARAM_BARSTARTALG, 0},
{"barcrossalg", CPX_PARAM_BARCROSSALG, 0},
{"barooc", CPX_PARAM_BAROOC, 0},
{"brdir", CPX_PARAM_BRDIR, 0}, 
{"bttol", CPX_PARAM_BTTOL, 1},
{"cliques", CPX_PARAM_CLIQUES, 0},
{"coeredind", CPX_PARAM_COEREDIND, 0},
{"covers", CPX_PARAM_COVERS, 0},
{"cutlo", CPX_PARAM_CUTLO, 1},
{"cutup", CPX_PARAM_CUTUP, 1},
{"epagap", CPX_PARAM_EPAGAP, 1},
{"epgap", CPX_PARAM_EPGAP, 1},
{"epint", CPX_PARAM_EPINT, 1},
{"heuristic", CPX_PARAM_HEURISTIC, 0},
{"mipdisplay", CPX_PARAM_MIPDISPLAY, 0},
{"mipinterval", CPX_PARAM_MIPINTERVAL, 0},
{"mipthreads", CPX_PARAM_MIPTHREADS, 0},
{"intsollim", CPX_PARAM_INTSOLLIM, 0},
{"nodefileind", CPX_PARAM_NODEFILEIND, 0},
{"nodelim", CPX_PARAM_NODELIM, 0},
{"nodesel", CPX_PARAM_NODESEL, 0},
{"objdif", CPX_PARAM_OBJDIF, 1},
{"mipordind", CPX_PARAM_MIPORDIND, 0},
{"relobjdif", CPX_PARAM_RELOBJDIF, 1},
{"startalg", CPX_PARAM_STARTALG, 0},
{"subalg", CPX_PARAM_SUBALG, 0},
{"trelim", CPX_PARAM_TRELIM, 1},
{"varsel", CPX_PARAM_VARSEL, 0},
{"bndstrenind", CPX_PARAM_BNDSTRENIND, 0},
{"heurfreq", CPX_PARAM_HEURFREQ, 0},
{"mipordtype", CPX_PARAM_MIPORDTYPE, 0},
{"cutsfactor", CPX_PARAM_CUTSFACTOR, 1},
{"relaxpreind", CPX_PARAM_RELAXPREIND, 0},
{"mipstart", CPX_PARAM_MIPSTART, 0},
{"preslvnd", CPX_PARAM_PRESLVND, 0},
{"bbinterval", CPX_PARAM_BBINTERVAL, 0},
{"flowcovers", CPX_PARAM_FLOWCOVERS, 0},
{"implbd", CPX_PARAM_IMPLBD, 0},
{"probe", CPX_PARAM_PROBE, 0},
{"gubcovers", CPX_PARAM_GUBCOVERS, 0},
{"strongcandlim", CPX_PARAM_STRONGCANDLIM, 0},
{"strongitlim", CPX_PARAM_STRONGITLIM, 0},
{"strongthreadlim", CPX_PARAM_STRONGTHREADLIM, 0},
{"fraccand", CPX_PARAM_FRACCAND, 0},
{"fraccuts", CPX_PARAM_FRACCUTS, 0},
{"fracpass", CPX_PARAM_FRACPASS, 0},
{"flowpaths", CPX_PARAM_FLOWPATHS, 0},
{"mircuts", CPX_PARAM_MIRCUTS, 0},
{"disjcuts", CPX_PARAM_DISJCUTS, 0},
{"aggcutlim", CPX_PARAM_AGGCUTLIM, 0},
{"mipcbredlp", CPX_PARAM_MIPCBREDLP, 0},
{"cutpass", CPX_PARAM_CUTPASS, 0},
{"mipemphasis", CPX_PARAM_MIPEMPHASIS, 0},
{"symmetry", CPX_PARAM_SYMMETRY, 0},
{"divetype", CPX_PARAM_DIVETYPE, 0},
{"netitlim", CPX_PARAM_NETITLIM, 0},
{"netepopt", CPX_PARAM_NETEPOPT, 1},
{"neteprhs", CPX_PARAM_NETEPRHS, 1},
{"netppriind", CPX_PARAM_NETPPRIIND, 0},
{"netdisplay", CPX_PARAM_NETDISPLAY, 0},
{"qpnzreadlim", CPX_PARAM_QPNZREADLIM, 0},
{"qpnzgrowth", CPX_PARAM_QPNZGROWTH, 0},
{"qpmakepsdind", CPX_PARAM_QPMAKEPSDIND, 0},

#elif CPLEX==9

#define NUMPARAMS 135
static struct param_desc params[NUMPARAMS+NUMALIASES] = {
{"advind", CPX_PARAM_ADVIND, 0},
{"aggfill", CPX_PARAM_AGGFILL, 0},
{"aggind", CPX_PARAM_AGGIND, 0},
{"basinterval", CPX_PARAM_BASINTERVAL, 0},
{"cfilemul", CPX_PARAM_CFILEMUL, 0},
{"clocktype", CPX_PARAM_CLOCKTYPE, 0},
{"craind", CPX_PARAM_CRAIND, 0},
{"depind", CPX_PARAM_DEPIND, 0},
{"dpriind", CPX_PARAM_DPRIIND, 0},
{"pricelim", CPX_PARAM_PRICELIM, 0},
{"epmrk", CPX_PARAM_EPMRK, 1},
{"epopt", CPX_PARAM_EPOPT, 1},
{"epper", CPX_PARAM_EPPER, 1},
{"eprhs", CPX_PARAM_EPRHS, 1},
{"fastmip", CPX_PARAM_FASTMIP, 0},
{"iisind", CPX_PARAM_IISIND, 0},
{"simdisplay", CPX_PARAM_SIMDISPLAY, 0},
{"itlim", CPX_PARAM_ITLIM, 0},
{"rowreadlim", CPX_PARAM_ROWREADLIM, 0},
{"netfind", CPX_PARAM_NETFIND, 0},
{"colreadlim", CPX_PARAM_COLREADLIM, 0},
{"nzreadlim", CPX_PARAM_NZREADLIM, 0},
{"objllim", CPX_PARAM_OBJLLIM, 1},
{"objulim", CPX_PARAM_OBJULIM, 1},
{"perind", CPX_PARAM_PERIND, 0},
{"perlim", CPX_PARAM_PERLIM, 0},
{"ppriind", CPX_PARAM_PPRIIND, 0},
{"preind", CPX_PARAM_PREIND, 0},
{"reinv", CPX_PARAM_REINV, 0},
{"reverseind", CPX_PARAM_REVERSEIND, 0},
{"rfilemul", CPX_PARAM_RFILEMUL, 0},
{"scaind", CPX_PARAM_SCAIND, 0},
{"scrind", CPX_PARAM_SCRIND, 0},
{"singlim", CPX_PARAM_SINGLIM, 0},
{"singtol", CPX_PARAM_SINGTOL, 0},
{"tilim", CPX_PARAM_TILIM, 1},
{"xxxind", CPX_PARAM_XXXIND, 0},
{"predual", CPX_PARAM_PREDUAL, 0},
{"rowgrowth", CPX_PARAM_ROWGROWTH, 0},
{"colgrowth", CPX_PARAM_COLGROWTH, 0},
{"nzgrowth", CPX_PARAM_NZGROWTH, 0},
{"epopt_h", CPX_PARAM_EPOPT_H, 0},
{"eprhs_h", CPX_PARAM_EPRHS_H, 0},
{"prepass", CPX_PARAM_PREPASS, 0},
{"datacheck", CPX_PARAM_DATACHECK, 0},
{"reduce", CPX_PARAM_REDUCE, 0},
{"prelinear", CPX_PARAM_PRELINEAR, 0},
{"lpmethod", CPX_PARAM_LPMETHOD, 0},
{"qpmethod", CPX_PARAM_QPMETHOD, 0},
{"workdir", CPX_PARAM_WORKDIR, 2},
{"workmem", CPX_PARAM_WORKMEM, 1},
{"precompress", CPX_PARAM_PRECOMPRESS, 0},
{"threads", CPX_PARAM_THREADS, 0},
{"siftdisplay", CPX_PARAM_SIFTDISPLAY, 0},
{"siftalg", CPX_PARAM_SIFTALG, 0},
{"siftitlim", CPX_PARAM_SIFTITLIM, 0},
{"finalfactor", CPX_PARAM_FINALFACTOR, 0},
{"bardstart", CPX_PARAM_BARDSTART, 0},
{"barepcomp", CPX_PARAM_BAREPCOMP, 1},
{"bargrowth", CPX_PARAM_BARGROWTH, 1},
{"barobjrng", CPX_PARAM_BAROBJRNG, 1},
{"barpstart", CPX_PARAM_BARPSTART, 0},
{"baralg", CPX_PARAM_BARALG, 0},
{"barcolnz", CPX_PARAM_BARCOLNZ, 0},
{"bardisplay", CPX_PARAM_BARDISPLAY, 0},
{"baritlim", CPX_PARAM_BARITLIM, 0},
{"barmaxcor", CPX_PARAM_BARMAXCOR, 0},
{"barorder", CPX_PARAM_BARORDER, 0},
{"barrowsden", CPX_PARAM_BARROWSDEN, 0},
{"barthreads", CPX_PARAM_BARTHREADS, 0},
{"barstartalg", CPX_PARAM_BARSTARTALG, 0},
{"barcrossalg", CPX_PARAM_BARCROSSALG, 0},
{"barooc", CPX_PARAM_BAROOC, 0},
{"barqcpepcomp", CPX_PARAM_BARQCPEPCOMP, 1},
{"brdir", CPX_PARAM_BRDIR, 0},
{"bttol", CPX_PARAM_BTTOL, 1},
{"cliques", CPX_PARAM_CLIQUES, 0},
{"coeredind", CPX_PARAM_COEREDIND, 0},
{"covers", CPX_PARAM_COVERS, 0},
{"cutlo", CPX_PARAM_CUTLO, 1},
{"cutup", CPX_PARAM_CUTUP, 1},
{"epagap", CPX_PARAM_EPAGAP, 1},
{"epgap", CPX_PARAM_EPGAP, 1},
{"epint", CPX_PARAM_EPINT, 1},
{"mipdisplay", CPX_PARAM_MIPDISPLAY, 0},
{"mipinterval", CPX_PARAM_MIPINTERVAL, 0},
{"mipthreads", CPX_PARAM_MIPTHREADS, 0},
{"intsollim", CPX_PARAM_INTSOLLIM, 0},
{"nodefileind", CPX_PARAM_NODEFILEIND, 0},
{"nodelim", CPX_PARAM_NODELIM, 0},
{"nodesel", CPX_PARAM_NODESEL, 0},
{"objdif", CPX_PARAM_OBJDIF, 1},
{"mipordind", CPX_PARAM_MIPORDIND, 0},
{"relobjdif", CPX_PARAM_RELOBJDIF, 1},
{"startalg", CPX_PARAM_STARTALG, 0},
{"subalg", CPX_PARAM_SUBALG, 0},
{"trelim", CPX_PARAM_TRELIM, 1},
{"varsel", CPX_PARAM_VARSEL, 0},
{"bndstrenind", CPX_PARAM_BNDSTRENIND, 0},
{"heurfreq", CPX_PARAM_HEURFREQ, 0},
{"mipordtype", CPX_PARAM_MIPORDTYPE, 0},
{"cutsfactor", CPX_PARAM_CUTSFACTOR, 1},
{"relaxpreind", CPX_PARAM_RELAXPREIND, 0},
{"mipstart", CPX_PARAM_MIPSTART, 0},
{"preslvnd", CPX_PARAM_PRESLVND, 0},
{"bbinterval", CPX_PARAM_BBINTERVAL, 0},
{"flowcovers", CPX_PARAM_FLOWCOVERS, 0},
{"implbd", CPX_PARAM_IMPLBD, 0},
{"probe", CPX_PARAM_PROBE, 0},
{"gubcovers", CPX_PARAM_GUBCOVERS, 0},
{"strongcandlim", CPX_PARAM_STRONGCANDLIM, 0},
{"strongitlim", CPX_PARAM_STRONGITLIM, 0},
{"strongthreadlim", CPX_PARAM_STRONGTHREADLIM, 0},
{"fraccand", CPX_PARAM_FRACCAND, 0},
{"fraccuts", CPX_PARAM_FRACCUTS, 0},
{"fracpass", CPX_PARAM_FRACPASS, 0},
{"flowpaths", CPX_PARAM_FLOWPATHS, 0},
{"mircuts", CPX_PARAM_MIRCUTS, 0},
{"disjcuts", CPX_PARAM_DISJCUTS, 0},
{"aggcutlim", CPX_PARAM_AGGCUTLIM, 0},
{"mipcbredlp", CPX_PARAM_MIPCBREDLP, 0},
{"cutpass", CPX_PARAM_CUTPASS, 0},
{"mipemphasis", CPX_PARAM_MIPEMPHASIS, 0},
{"symmetry", CPX_PARAM_SYMMETRY, 0},
{"divetype", CPX_PARAM_DIVETYPE, 0},
{"rinsheur", CPX_PARAM_RINSHEUR, 0},
{"submipnodelim", CPX_PARAM_SUBMIPNODELIM, 0},
{"netitlim", CPX_PARAM_NETITLIM, 0},
{"netepopt", CPX_PARAM_NETEPOPT, 1},
{"neteprhs", CPX_PARAM_NETEPRHS, 1},
{"netppriind", CPX_PARAM_NETPPRIIND, 0},
{"netdisplay", CPX_PARAM_NETDISPLAY, 0},
{"qpnzreadlim", CPX_PARAM_QPNZREADLIM, 0},
{"qpnzgrowth", CPX_PARAM_QPNZGROWTH, 0},
{"qpmakepsdind", CPX_PARAM_QPMAKEPSDIND, 0},

#elif CPLEX==10

#define NUMPARAMS 137
static struct param_desc params[NUMPARAMS+NUMALIASES] = {
{"advind", CPX_PARAM_ADVIND, 0},
{"aggfill", CPX_PARAM_AGGFILL, 0},
{"aggind", CPX_PARAM_AGGIND, 0},
{"basinterval", CPX_PARAM_BASINTERVAL, 0},
{"cfilemul", CPX_PARAM_CFILEMUL, 0},
{"clocktype", CPX_PARAM_CLOCKTYPE, 0},
{"craind", CPX_PARAM_CRAIND, 0},
{"depind", CPX_PARAM_DEPIND, 0},
{"dpriind", CPX_PARAM_DPRIIND, 0},
{"pricelim", CPX_PARAM_PRICELIM, 0},
{"epmrk", CPX_PARAM_EPMRK, 1},
{"epopt", CPX_PARAM_EPOPT, 1},
{"epper", CPX_PARAM_EPPER, 1},
{"eprhs", CPX_PARAM_EPRHS, 1},
{"fastmip", CPX_PARAM_FASTMIP, 0},
{"simdisplay", CPX_PARAM_SIMDISPLAY, 0},
{"itlim", CPX_PARAM_ITLIM, 0},
{"rowreadlim", CPX_PARAM_ROWREADLIM, 0},
{"netfind", CPX_PARAM_NETFIND, 0},
{"colreadlim", CPX_PARAM_COLREADLIM, 0},
{"nzreadlim", CPX_PARAM_NZREADLIM, 0},
{"objllim", CPX_PARAM_OBJLLIM, 1},
{"objulim", CPX_PARAM_OBJULIM, 1},
{"perind", CPX_PARAM_PERIND, 0},
{"perlim", CPX_PARAM_PERLIM, 0},
{"ppriind", CPX_PARAM_PPRIIND, 0},
{"preind", CPX_PARAM_PREIND, 0},
{"reinv", CPX_PARAM_REINV, 0},
{"reverseind", CPX_PARAM_REVERSEIND, 0},
{"rfilemul", CPX_PARAM_RFILEMUL, 0},
{"scaind", CPX_PARAM_SCAIND, 0},
{"scrind", CPX_PARAM_SCRIND, 0},
{"singlim", CPX_PARAM_SINGLIM, 0},
{"singtol", CPX_PARAM_SINGTOL, 0},
{"tilim", CPX_PARAM_TILIM, 1},
{"xxxind", CPX_PARAM_XXXIND, 0},
{"predual", CPX_PARAM_PREDUAL, 0},
{"epopt_h", CPX_PARAM_EPOPT_H, 0},
{"eprhs_h", CPX_PARAM_EPRHS_H, 0},
{"prepass", CPX_PARAM_PREPASS, 0},
{"datacheck", CPX_PARAM_DATACHECK, 0},
{"reduce", CPX_PARAM_REDUCE, 0},
{"prelinear", CPX_PARAM_PRELINEAR, 0},
{"lpmethod", CPX_PARAM_LPMETHOD, 0},
{"qpmethod", CPX_PARAM_QPMETHOD, 0},
{"workdir", CPX_PARAM_WORKDIR, 2},
{"workmem", CPX_PARAM_WORKMEM, 1},
{"threads", CPX_PARAM_THREADS, 0},
{"conflictdisplay", CPX_PARAM_CONFLICTDISPLAY, 0},
{"siftdisplay", CPX_PARAM_SIFTDISPLAY, 0},
{"siftalg", CPX_PARAM_SIFTALG, 0},
{"siftitlim", CPX_PARAM_SIFTITLIM, 0},
{"mpslongnum", CPX_PARAM_MPSLONGNUM, 0},
{"memoryemphasis", CPX_PARAM_MEMORYEMPHASIS, 0},
{"numericalemphasis", CPX_PARAM_NUMERICALEMPHASIS, 0},
{"feasoptmode", CPX_PARAM_FEASOPTMODE, 0},
{"bardstart", CPX_PARAM_BARDSTART, 0},
{"barepcomp", CPX_PARAM_BAREPCOMP, 1},
{"bargrowth", CPX_PARAM_BARGROWTH, 1},
{"barobjrng", CPX_PARAM_BAROBJRNG, 1},
{"barpstart", CPX_PARAM_BARPSTART, 0},
{"baralg", CPX_PARAM_BARALG, 0},
{"barcolnz", CPX_PARAM_BARCOLNZ, 0},
{"bardisplay", CPX_PARAM_BARDISPLAY, 0},
{"baritlim", CPX_PARAM_BARITLIM, 0},
{"barmaxcor", CPX_PARAM_BARMAXCOR, 0},
{"barorder", CPX_PARAM_BARORDER, 0},
{"barthreads", CPX_PARAM_BARTHREADS, 0},
{"barstartalg", CPX_PARAM_BARSTARTALG, 0},
{"barcrossalg", CPX_PARAM_BARCROSSALG, 0},
{"barqcpepcomp", CPX_PARAM_BARQCPEPCOMP, 1},
{"brdir", CPX_PARAM_BRDIR, 0},
{"bttol", CPX_PARAM_BTTOL, 1},
{"cliques", CPX_PARAM_CLIQUES, 0},
{"coeredind", CPX_PARAM_COEREDIND, 0},
{"covers", CPX_PARAM_COVERS, 0},
{"cutlo", CPX_PARAM_CUTLO, 1},
{"cutup", CPX_PARAM_CUTUP, 1},
{"epagap", CPX_PARAM_EPAGAP, 1},
{"epgap", CPX_PARAM_EPGAP, 1},
{"epint", CPX_PARAM_EPINT, 1},
{"mipdisplay", CPX_PARAM_MIPDISPLAY, 0},
{"mipinterval", CPX_PARAM_MIPINTERVAL, 0},
{"mipthreads", CPX_PARAM_MIPTHREADS, 0},
{"intsollim", CPX_PARAM_INTSOLLIM, 0},
{"nodefileind", CPX_PARAM_NODEFILEIND, 0},
{"nodelim", CPX_PARAM_NODELIM, 0},
{"nodesel", CPX_PARAM_NODESEL, 0},
{"objdif", CPX_PARAM_OBJDIF, 1},
{"mipordind", CPX_PARAM_MIPORDIND, 0},
{"relobjdif", CPX_PARAM_RELOBJDIF, 1},
{"startalg", CPX_PARAM_STARTALG, 0},
{"subalg", CPX_PARAM_SUBALG, 0},
{"trelim", CPX_PARAM_TRELIM, 1},
{"varsel", CPX_PARAM_VARSEL, 0},
{"bndstrenind", CPX_PARAM_BNDSTRENIND, 0},
{"heurfreq", CPX_PARAM_HEURFREQ, 0},
{"mipordtype", CPX_PARAM_MIPORDTYPE, 0},
{"cutsfactor", CPX_PARAM_CUTSFACTOR, 1},
{"relaxpreind", CPX_PARAM_RELAXPREIND, 0},
{"preslvnd", CPX_PARAM_PRESLVND, 0},
{"bbinterval", CPX_PARAM_BBINTERVAL, 0},
{"flowcovers", CPX_PARAM_FLOWCOVERS, 0},
{"implbd", CPX_PARAM_IMPLBD, 0},
{"probe", CPX_PARAM_PROBE, 0},
{"gubcovers", CPX_PARAM_GUBCOVERS, 0},
{"strongcandlim", CPX_PARAM_STRONGCANDLIM, 0},
{"strongitlim", CPX_PARAM_STRONGITLIM, 0},
{"strongthreadlim", CPX_PARAM_STRONGTHREADLIM, 0},
{"fraccand", CPX_PARAM_FRACCAND, 0},
{"fraccuts", CPX_PARAM_FRACCUTS, 0},
{"fracpass", CPX_PARAM_FRACPASS, 0},
{"flowpaths", CPX_PARAM_FLOWPATHS, 0},
{"mircuts", CPX_PARAM_MIRCUTS, 0},
{"disjcuts", CPX_PARAM_DISJCUTS, 0},
{"aggcutlim", CPX_PARAM_AGGCUTLIM, 0},
{"mipcbredlp", CPX_PARAM_MIPCBREDLP, 0},
{"cutpass", CPX_PARAM_CUTPASS, 0},
{"mipemphasis", CPX_PARAM_MIPEMPHASIS, 0},
{"symmetry", CPX_PARAM_SYMMETRY, 0},
{"divetype", CPX_PARAM_DIVETYPE, 0},
{"rinsheur", CPX_PARAM_RINSHEUR, 0},
{"submipnodelim", CPX_PARAM_SUBMIPNODELIM, 0},
{"lbheur", CPX_PARAM_LBHEUR, 0},
{"repeatpresolve", CPX_PARAM_REPEATPRESOLVE, 0},
{"probetime", CPX_PARAM_PROBETIME, 1},
{"polishtime", CPX_PARAM_POLISHTIME, 1},
{"repairtries", CPX_PARAM_REPAIRTRIES, 0},
{"eplin", CPX_PARAM_EPLIN, 1},
{"eprelax", CPX_PARAM_EPRELAX, 1},
{"netitlim", CPX_PARAM_NETITLIM, 0},
{"netepopt", CPX_PARAM_NETEPOPT, 1},
{"neteprhs", CPX_PARAM_NETEPRHS, 1},
{"netppriind", CPX_PARAM_NETPPRIIND, 0},
{"netdisplay", CPX_PARAM_NETDISPLAY, 0},
{"qpnzreadlim", CPX_PARAM_QPNZREADLIM, 0},
{"qpmakepsdind", CPX_PARAM_QPMAKEPSDIND, 0},

#elif CPLEX==11

#define NUMPARAMS 150
static struct param_desc params[NUMPARAMS+NUMALIASES] = {
{"advind", CPX_PARAM_ADVIND, 0},
{"aggfill", CPX_PARAM_AGGFILL, 0},
{"aggind", CPX_PARAM_AGGIND, 0},
{"basinterval", CPX_PARAM_BASINTERVAL, 0},
{"cfilemul", CPX_PARAM_CFILEMUL, 0},
{"clocktype", CPX_PARAM_CLOCKTYPE, 0},
{"craind", CPX_PARAM_CRAIND, 0},
{"depind", CPX_PARAM_DEPIND, 0},
{"dpriind", CPX_PARAM_DPRIIND, 0},
{"pricelim", CPX_PARAM_PRICELIM, 0},
{"epmrk", CPX_PARAM_EPMRK, 1},
{"epopt", CPX_PARAM_EPOPT, 1},
{"epper", CPX_PARAM_EPPER, 1},
{"eprhs", CPX_PARAM_EPRHS, 1},
{"fastmip", CPX_PARAM_FASTMIP, 0},
{"simdisplay", CPX_PARAM_SIMDISPLAY, 0},
{"itlim", CPX_PARAM_ITLIM, 0},
{"rowreadlim", CPX_PARAM_ROWREADLIM, 0},
{"netfind", CPX_PARAM_NETFIND, 0},
{"colreadlim", CPX_PARAM_COLREADLIM, 0},
{"nzreadlim", CPX_PARAM_NZREADLIM, 0},
{"objllim", CPX_PARAM_OBJLLIM, 1},
{"objulim", CPX_PARAM_OBJULIM, 1},
{"perind", CPX_PARAM_PERIND, 0},
{"perlim", CPX_PARAM_PERLIM, 0},
{"ppriind", CPX_PARAM_PPRIIND, 0},
{"preind", CPX_PARAM_PREIND, 0},
{"reinv", CPX_PARAM_REINV, 0},
{"reverseind", CPX_PARAM_REVERSEIND, 0},
{"rfilemul", CPX_PARAM_RFILEMUL, 0},
{"scaind", CPX_PARAM_SCAIND, 0},
{"scrind", CPX_PARAM_SCRIND, 0},
{"singlim", CPX_PARAM_SINGLIM, 0},
{"singtol", CPX_PARAM_SINGTOL, 0},
{"tilim", CPX_PARAM_TILIM, 1},
{"xxxind", CPX_PARAM_XXXIND, 0},
{"predual", CPX_PARAM_PREDUAL, 0},
{"epopt_h", CPX_PARAM_EPOPT_H, 0},
{"eprhs_h", CPX_PARAM_EPRHS_H, 0},
{"prepass", CPX_PARAM_PREPASS, 0},
{"datacheck", CPX_PARAM_DATACHECK, 0},
{"reduce", CPX_PARAM_REDUCE, 0},
{"prelinear", CPX_PARAM_PRELINEAR, 0},
{"lpmethod", CPX_PARAM_LPMETHOD, 0},
{"qpmethod", CPX_PARAM_QPMETHOD, 0},
{"workdir", CPX_PARAM_WORKDIR, 2},
{"workmem", CPX_PARAM_WORKMEM, 1},
{"threads", CPX_PARAM_THREADS, 0},
{"conflictdisplay", CPX_PARAM_CONFLICTDISPLAY, 0},
{"siftdisplay", CPX_PARAM_SIFTDISPLAY, 0},
{"siftalg", CPX_PARAM_SIFTALG, 0},
{"siftitlim", CPX_PARAM_SIFTITLIM, 0},
{"mpslongnum", CPX_PARAM_MPSLONGNUM, 0},
{"memoryemphasis", CPX_PARAM_MEMORYEMPHASIS, 0},
{"numericalemphasis", CPX_PARAM_NUMERICALEMPHASIS, 0},
{"feasoptmode", CPX_PARAM_FEASOPTMODE, 0},
{"parallelmode", CPX_PARAM_PARALLELMODE, 0},
{"tuningmeasure", CPX_PARAM_TUNINGMEASURE, 0},
{"tuningrepeat", CPX_PARAM_TUNINGREPEAT, 0},
{"tuningtilim", CPX_PARAM_TUNINGTILIM, 1},
{"tuningdisplay", CPX_PARAM_TUNINGDISPLAY, 0},
{"bardstart", CPX_PARAM_BARDSTART, 0},
{"barepcomp", CPX_PARAM_BAREPCOMP, 1},
{"bargrowth", CPX_PARAM_BARGROWTH, 1},
{"barobjrng", CPX_PARAM_BAROBJRNG, 1},
{"barpstart", CPX_PARAM_BARPSTART, 0},
{"baralg", CPX_PARAM_BARALG, 0},
{"barcolnz", CPX_PARAM_BARCOLNZ, 0},
{"bardisplay", CPX_PARAM_BARDISPLAY, 0},
{"baritlim", CPX_PARAM_BARITLIM, 0},
{"barmaxcor", CPX_PARAM_BARMAXCOR, 0},
{"barorder", CPX_PARAM_BARORDER, 0},
{"barstartalg", CPX_PARAM_BARSTARTALG, 0},
{"barcrossalg", CPX_PARAM_BARCROSSALG, 0},
{"barqcpepcomp", CPX_PARAM_BARQCPEPCOMP, 1},
{"brdir", CPX_PARAM_BRDIR, 0},
{"bttol", CPX_PARAM_BTTOL, 1},
{"cliques", CPX_PARAM_CLIQUES, 0},
{"coeredind", CPX_PARAM_COEREDIND, 0},
{"covers", CPX_PARAM_COVERS, 0},
{"cutlo", CPX_PARAM_CUTLO, 1},
{"cutup", CPX_PARAM_CUTUP, 1},
{"epagap", CPX_PARAM_EPAGAP, 1},
{"epgap", CPX_PARAM_EPGAP, 1},
{"epint", CPX_PARAM_EPINT, 1},
{"mipdisplay", CPX_PARAM_MIPDISPLAY, 0},
{"mipinterval", CPX_PARAM_MIPINTERVAL, 0},
{"intsollim", CPX_PARAM_INTSOLLIM, 0},
{"nodefileind", CPX_PARAM_NODEFILEIND, 0},
{"nodelim", CPX_PARAM_NODELIM, 0},
{"nodesel", CPX_PARAM_NODESEL, 0},
{"objdif", CPX_PARAM_OBJDIF, 1},
{"mipordind", CPX_PARAM_MIPORDIND, 0},
{"relobjdif", CPX_PARAM_RELOBJDIF, 1},
{"startalg", CPX_PARAM_STARTALG, 0},
{"subalg", CPX_PARAM_SUBALG, 0},
{"trelim", CPX_PARAM_TRELIM, 1},
{"varsel", CPX_PARAM_VARSEL, 0},
{"bndstrenind", CPX_PARAM_BNDSTRENIND, 0},
{"heurfreq", CPX_PARAM_HEURFREQ, 0},
{"mipordtype", CPX_PARAM_MIPORDTYPE, 0},
{"cutsfactor", CPX_PARAM_CUTSFACTOR, 1},
{"relaxpreind", CPX_PARAM_RELAXPREIND, 0},
{"preslvnd", CPX_PARAM_PRESLVND, 0},
{"bbinterval", CPX_PARAM_BBINTERVAL, 0},
{"flowcovers", CPX_PARAM_FLOWCOVERS, 0},
{"implbd", CPX_PARAM_IMPLBD, 0},
{"probe", CPX_PARAM_PROBE, 0},
{"gubcovers", CPX_PARAM_GUBCOVERS, 0},
{"strongcandlim", CPX_PARAM_STRONGCANDLIM, 0},
{"strongitlim", CPX_PARAM_STRONGITLIM, 0},
{"fraccand", CPX_PARAM_FRACCAND, 0},
{"fraccuts", CPX_PARAM_FRACCUTS, 0},
{"fracpass", CPX_PARAM_FRACPASS, 0},
{"flowpaths", CPX_PARAM_FLOWPATHS, 0},
{"mircuts", CPX_PARAM_MIRCUTS, 0},
{"disjcuts", CPX_PARAM_DISJCUTS, 0},
{"aggcutlim", CPX_PARAM_AGGCUTLIM, 0},
{"mipcbredlp", CPX_PARAM_MIPCBREDLP, 0},
{"cutpass", CPX_PARAM_CUTPASS, 0},
{"mipemphasis", CPX_PARAM_MIPEMPHASIS, 0},
{"symmetry", CPX_PARAM_SYMMETRY, 0},
{"divetype", CPX_PARAM_DIVETYPE, 0},
{"rinsheur", CPX_PARAM_RINSHEUR, 0},
{"submipnodelim", CPX_PARAM_SUBMIPNODELIM, 0},
{"lbheur", CPX_PARAM_LBHEUR, 0},
{"repeatpresolve", CPX_PARAM_REPEATPRESOLVE, 0},
{"probetime", CPX_PARAM_PROBETIME, 1},
{"polishtime", CPX_PARAM_POLISHTIME, 1},
{"repairtries", CPX_PARAM_REPAIRTRIES, 0},
{"eplin", CPX_PARAM_EPLIN, 1},
{"eprelax", CPX_PARAM_EPRELAX, 1},
{"fpheur", CPX_PARAM_FPHEUR, 0},
{"eachcutlim", CPX_PARAM_EACHCUTLIM, 0},
{"solnpoolcapacity", CPX_PARAM_SOLNPOOLCAPACITY, 0},
{"solnpoolreplace", CPX_PARAM_SOLNPOOLREPLACE, 0},
{"solnpoolgap", CPX_PARAM_SOLNPOOLGAP, 1},
{"solnpoolagap", CPX_PARAM_SOLNPOOLAGAP, 1},
{"solnpoolintensity", CPX_PARAM_SOLNPOOLINTENSITY, 0},
{"populatelim", CPX_PARAM_POPULATELIM, 0},
{"mipsearch", CPX_PARAM_MIPSEARCH, 0},
{"miqcpstrat", CPX_PARAM_MIQCPSTRAT, 0},
{"zerohalfcuts", CPX_PARAM_ZEROHALFCUTS, 0},
{"netitlim", CPX_PARAM_NETITLIM, 0},
{"netepopt", CPX_PARAM_NETEPOPT, 1},
{"neteprhs", CPX_PARAM_NETEPRHS, 1},
{"netppriind", CPX_PARAM_NETPPRIIND, 0},
{"netdisplay", CPX_PARAM_NETDISPLAY, 0},
{"qpnzreadlim", CPX_PARAM_QPNZREADLIM, 0},
{"qpmakepsdind", CPX_PARAM_QPMAKEPSDIND, 0},

#elif CPLEX==12 && CPLEXMINOR<=2

#define NUMPARAMS 157
static struct param_desc params[NUMPARAMS+NUMALIASES] = {
{"advind", CPX_PARAM_ADVIND, 0},
{"aggfill", CPX_PARAM_AGGFILL, 0},
{"aggind", CPX_PARAM_AGGIND, 0},
{"basinterval", CPX_PARAM_BASINTERVAL, 0},
{"cfilemul", CPX_PARAM_CFILEMUL, 0},
{"clocktype", CPX_PARAM_CLOCKTYPE, 0},
{"craind", CPX_PARAM_CRAIND, 0},
{"depind", CPX_PARAM_DEPIND, 0},
{"dpriind", CPX_PARAM_DPRIIND, 0},
{"pricelim", CPX_PARAM_PRICELIM, 0},
{"epmrk", CPX_PARAM_EPMRK, 1},
{"epopt", CPX_PARAM_EPOPT, 1},
{"epper", CPX_PARAM_EPPER, 1},
{"eprhs", CPX_PARAM_EPRHS, 1},
{"fastmip", CPX_PARAM_FASTMIP, 0},
{"simdisplay", CPX_PARAM_SIMDISPLAY, 0},
{"itlim", CPX_PARAM_ITLIM, 0},
{"rowreadlim", CPX_PARAM_ROWREADLIM, 0},
{"netfind", CPX_PARAM_NETFIND, 0},
{"colreadlim", CPX_PARAM_COLREADLIM, 0},
{"nzreadlim", CPX_PARAM_NZREADLIM, 0},
{"objllim", CPX_PARAM_OBJLLIM, 1},
{"objulim", CPX_PARAM_OBJULIM, 1},
{"perind", CPX_PARAM_PERIND, 0},
{"perlim", CPX_PARAM_PERLIM, 0},
{"ppriind", CPX_PARAM_PPRIIND, 0},
{"preind", CPX_PARAM_PREIND, 0},
{"reinv", CPX_PARAM_REINV, 0},
{"reverseind", CPX_PARAM_REVERSEIND, 0},
{"rfilemul", CPX_PARAM_RFILEMUL, 0},
{"scaind", CPX_PARAM_SCAIND, 0},
{"scrind", CPX_PARAM_SCRIND, 0},
{"singlim", CPX_PARAM_SINGLIM, 0},
{"singtol", CPX_PARAM_SINGTOL, 0},
{"tilim", CPX_PARAM_TILIM, 1},
{"xxxind", CPX_PARAM_XXXIND, 0},
{"predual", CPX_PARAM_PREDUAL, 0},
{"epopt_h", CPX_PARAM_EPOPT_H, 0},
{"eprhs_h", CPX_PARAM_EPRHS_H, 0},
{"prepass", CPX_PARAM_PREPASS, 0},
{"datacheck", CPX_PARAM_DATACHECK, 0},
{"reduce", CPX_PARAM_REDUCE, 0},
{"prelinear", CPX_PARAM_PRELINEAR, 0},
{"lpmethod", CPX_PARAM_LPMETHOD, 0},
{"qpmethod", CPX_PARAM_QPMETHOD, 0},
{"workdir", CPX_PARAM_WORKDIR, 2},
{"workmem", CPX_PARAM_WORKMEM, 1},
{"threads", CPX_PARAM_THREADS, 0},
{"conflictdisplay", CPX_PARAM_CONFLICTDISPLAY, 0},
{"siftdisplay", CPX_PARAM_SIFTDISPLAY, 0},
{"siftalg", CPX_PARAM_SIFTALG, 0},
{"siftitlim", CPX_PARAM_SIFTITLIM, 0},
{"mpslongnum", CPX_PARAM_MPSLONGNUM, 0},
{"memoryemphasis", CPX_PARAM_MEMORYEMPHASIS, 0},
{"numericalemphasis", CPX_PARAM_NUMERICALEMPHASIS, 0},
{"feasoptmode", CPX_PARAM_FEASOPTMODE, 0},
{"parallelmode", CPX_PARAM_PARALLELMODE, 0},
{"tuningmeasure", CPX_PARAM_TUNINGMEASURE, 0},
{"tuningrepeat", CPX_PARAM_TUNINGREPEAT, 0},
{"tuningtilim", CPX_PARAM_TUNINGTILIM, 1},
{"tuningdisplay", CPX_PARAM_TUNINGDISPLAY, 0},
{"writelevel", CPX_PARAM_WRITELEVEL, 0},
{"bardstart", CPX_PARAM_BARDSTART, 0},
{"barepcomp", CPX_PARAM_BAREPCOMP, 1},
{"bargrowth", CPX_PARAM_BARGROWTH, 1},
{"barobjrng", CPX_PARAM_BAROBJRNG, 1},
{"barpstart", CPX_PARAM_BARPSTART, 0},
{"baralg", CPX_PARAM_BARALG, 0},
{"barcolnz", CPX_PARAM_BARCOLNZ, 0},
{"bardisplay", CPX_PARAM_BARDISPLAY, 0},
{"baritlim", CPX_PARAM_BARITLIM, 0},
{"barmaxcor", CPX_PARAM_BARMAXCOR, 0},
{"barorder", CPX_PARAM_BARORDER, 0},
{"barstartalg", CPX_PARAM_BARSTARTALG, 0},
{"barcrossalg", CPX_PARAM_BARCROSSALG, 0},
{"barqcpepcomp", CPX_PARAM_BARQCPEPCOMP, 1},
{"brdir", CPX_PARAM_BRDIR, 0},
{"bttol", CPX_PARAM_BTTOL, 1},
{"cliques", CPX_PARAM_CLIQUES, 0},
{"coeredind", CPX_PARAM_COEREDIND, 0},
{"covers", CPX_PARAM_COVERS, 0},
{"cutlo", CPX_PARAM_CUTLO, 1},
{"cutup", CPX_PARAM_CUTUP, 1},
{"epagap", CPX_PARAM_EPAGAP, 1},
{"epgap", CPX_PARAM_EPGAP, 1},
{"epint", CPX_PARAM_EPINT, 1},
{"mipdisplay", CPX_PARAM_MIPDISPLAY, 0},
{"mipinterval", CPX_PARAM_MIPINTERVAL, 0},
{"intsollim", CPX_PARAM_INTSOLLIM, 0},
{"nodefileind", CPX_PARAM_NODEFILEIND, 0},
{"nodelim", CPX_PARAM_NODELIM, 0},
{"nodesel", CPX_PARAM_NODESEL, 0},
{"objdif", CPX_PARAM_OBJDIF, 1},
{"mipordind", CPX_PARAM_MIPORDIND, 0},
{"relobjdif", CPX_PARAM_RELOBJDIF, 1},
{"startalg", CPX_PARAM_STARTALG, 0},
{"subalg", CPX_PARAM_SUBALG, 0},
{"trelim", CPX_PARAM_TRELIM, 1},
{"varsel", CPX_PARAM_VARSEL, 0},
{"bndstrenind", CPX_PARAM_BNDSTRENIND, 0},
{"heurfreq", CPX_PARAM_HEURFREQ, 0},
{"mipordtype", CPX_PARAM_MIPORDTYPE, 0},
{"cutsfactor", CPX_PARAM_CUTSFACTOR, 1},
{"relaxpreind", CPX_PARAM_RELAXPREIND, 0},
{"preslvnd", CPX_PARAM_PRESLVND, 0},
{"bbinterval", CPX_PARAM_BBINTERVAL, 0},
{"flowcovers", CPX_PARAM_FLOWCOVERS, 0},
{"implbd", CPX_PARAM_IMPLBD, 0},
{"probe", CPX_PARAM_PROBE, 0},
{"gubcovers", CPX_PARAM_GUBCOVERS, 0},
{"strongcandlim", CPX_PARAM_STRONGCANDLIM, 0},
{"strongitlim", CPX_PARAM_STRONGITLIM, 0},
{"fraccand", CPX_PARAM_FRACCAND, 0},
{"fraccuts", CPX_PARAM_FRACCUTS, 0},
{"fracpass", CPX_PARAM_FRACPASS, 0},
{"flowpaths", CPX_PARAM_FLOWPATHS, 0},
{"mircuts", CPX_PARAM_MIRCUTS, 0},
{"disjcuts", CPX_PARAM_DISJCUTS, 0},
{"aggcutlim", CPX_PARAM_AGGCUTLIM, 0},
{"mipcbredlp", CPX_PARAM_MIPCBREDLP, 0},
{"cutpass", CPX_PARAM_CUTPASS, 0},
{"mipemphasis", CPX_PARAM_MIPEMPHASIS, 0},
{"symmetry", CPX_PARAM_SYMMETRY, 0},
{"divetype", CPX_PARAM_DIVETYPE, 0},
{"rinsheur", CPX_PARAM_RINSHEUR, 0},
{"submipnodelim", CPX_PARAM_SUBMIPNODELIM, 0},
{"lbheur", CPX_PARAM_LBHEUR, 0},
{"repeatpresolve", CPX_PARAM_REPEATPRESOLVE, 0},
{"probetime", CPX_PARAM_PROBETIME, 1},
{"polishtime", CPX_PARAM_POLISHTIME, 1},
{"repairtries", CPX_PARAM_REPAIRTRIES, 0},
{"eplin", CPX_PARAM_EPLIN, 1},
{"eprelax", CPX_PARAM_EPRELAX, 1},
{"fpheur", CPX_PARAM_FPHEUR, 0},
{"eachcutlim", CPX_PARAM_EACHCUTLIM, 0},
{"solnpoolcapacity", CPX_PARAM_SOLNPOOLCAPACITY, 0},
{"solnpoolreplace", CPX_PARAM_SOLNPOOLREPLACE, 0},
{"solnpoolgap", CPX_PARAM_SOLNPOOLGAP, 1},
{"solnpoolagap", CPX_PARAM_SOLNPOOLAGAP, 1},
{"solnpoolintensity", CPX_PARAM_SOLNPOOLINTENSITY, 0},
{"populatelim", CPX_PARAM_POPULATELIM, 0},
{"mipsearch", CPX_PARAM_MIPSEARCH, 0},
{"miqcpstrat", CPX_PARAM_MIQCPSTRAT, 0},
{"zerohalfcuts", CPX_PARAM_ZEROHALFCUTS, 0},
{"polishafterepagap", CPX_PARAM_POLISHAFTEREPAGAP, 1},
{"polishafterepgap", CPX_PARAM_POLISHAFTEREPGAP, 1},
{"polishafternode", CPX_PARAM_POLISHAFTERNODE, 0},
{"polishafterintsol", CPX_PARAM_POLISHAFTERINTSOL, 0},
{"polishaftertime", CPX_PARAM_POLISHAFTERTIME, 1},
{"mcfcuts", CPX_PARAM_MCFCUTS, 0},
{"netitlim", CPX_PARAM_NETITLIM, 0},
{"netepopt", CPX_PARAM_NETEPOPT, 1},
{"neteprhs", CPX_PARAM_NETEPRHS, 1},
{"netppriind", CPX_PARAM_NETPPRIIND, 0},
{"netdisplay", CPX_PARAM_NETDISPLAY, 0},
{"qpnzreadlim", CPX_PARAM_QPNZREADLIM, 0},
{"qpmakepsdind", CPX_PARAM_QPMAKEPSDIND, 0},

#elif CPLEX==12 && CPLEXMINOR<=4

#define NUMPARAMS 165
static struct param_desc params[NUMPARAMS+NUMALIASES] = {
{"advind", CPX_PARAM_ADVIND, 0},
{"aggfill", CPX_PARAM_AGGFILL, 0},
{"aggind", CPX_PARAM_AGGIND, 0},
{"basinterval", CPX_PARAM_BASINTERVAL, 0},
{"cfilemul", CPX_PARAM_CFILEMUL, 0},
{"clocktype", CPX_PARAM_CLOCKTYPE, 0},
{"craind", CPX_PARAM_CRAIND, 0},
{"depind", CPX_PARAM_DEPIND, 0},
{"dpriind", CPX_PARAM_DPRIIND, 0},
{"pricelim", CPX_PARAM_PRICELIM, 0},
{"epmrk", CPX_PARAM_EPMRK, 1},
{"epopt", CPX_PARAM_EPOPT, 1},
{"epper", CPX_PARAM_EPPER, 1},
{"eprhs", CPX_PARAM_EPRHS, 1},
{"fastmip", CPX_PARAM_FASTMIP, 0},
{"simdisplay", CPX_PARAM_SIMDISPLAY, 0},
{"itlim", CPX_PARAM_ITLIM, 0},
{"rowreadlim", CPX_PARAM_ROWREADLIM, 0},
{"netfind", CPX_PARAM_NETFIND, 0},
{"colreadlim", CPX_PARAM_COLREADLIM, 0},
{"nzreadlim", CPX_PARAM_NZREADLIM, 0},
{"objllim", CPX_PARAM_OBJLLIM, 1},
{"objulim", CPX_PARAM_OBJULIM, 1},
{"perind", CPX_PARAM_PERIND, 0},
{"perlim", CPX_PARAM_PERLIM, 0},
{"ppriind", CPX_PARAM_PPRIIND, 0},
{"preind", CPX_PARAM_PREIND, 0},
{"reinv", CPX_PARAM_REINV, 0},
{"reverseind", CPX_PARAM_REVERSEIND, 0},
{"rfilemul", CPX_PARAM_RFILEMUL, 0},
{"scaind", CPX_PARAM_SCAIND, 0},
{"scrind", CPX_PARAM_SCRIND, 0},
{"singlim", CPX_PARAM_SINGLIM, 0},
{"singtol", CPX_PARAM_SINGTOL, 0},
{"tilim", CPX_PARAM_TILIM, 1},
{"xxxind", CPX_PARAM_XXXIND, 0},
{"predual", CPX_PARAM_PREDUAL, 0},
{"epopt_h", CPX_PARAM_EPOPT_H, 0},
{"eprhs_h", CPX_PARAM_EPRHS_H, 0},
{"prepass", CPX_PARAM_PREPASS, 0},
{"datacheck", CPX_PARAM_DATACHECK, 0},
{"reduce", CPX_PARAM_REDUCE, 0},
{"prelinear", CPX_PARAM_PRELINEAR, 0},
{"lpmethod", CPX_PARAM_LPMETHOD, 0},
{"qpmethod", CPX_PARAM_QPMETHOD, 0},
{"workdir", CPX_PARAM_WORKDIR, 2},
{"workmem", CPX_PARAM_WORKMEM, 1},
{"threads", CPX_PARAM_THREADS, 0},
{"conflictdisplay", CPX_PARAM_CONFLICTDISPLAY, 0},
{"siftdisplay", CPX_PARAM_SIFTDISPLAY, 0},
{"siftalg", CPX_PARAM_SIFTALG, 0},
{"siftitlim", CPX_PARAM_SIFTITLIM, 0},
{"mpslongnum", CPX_PARAM_MPSLONGNUM, 0},
{"memoryemphasis", CPX_PARAM_MEMORYEMPHASIS, 0},
{"numericalemphasis", CPX_PARAM_NUMERICALEMPHASIS, 0},
{"feasoptmode", CPX_PARAM_FEASOPTMODE, 0},
{"parallelmode", CPX_PARAM_PARALLELMODE, 0},
{"tuningmeasure", CPX_PARAM_TUNINGMEASURE, 0},
{"tuningrepeat", CPX_PARAM_TUNINGREPEAT, 0},
{"tuningtilim", CPX_PARAM_TUNINGTILIM, 1},
{"tuningdisplay", CPX_PARAM_TUNINGDISPLAY, 0},
{"writelevel", CPX_PARAM_WRITELEVEL, 0},
{"dettilim", CPX_PARAM_DETTILIM, 1},
{"fileencoding", CPX_PARAM_FILEENCODING, 2},
{"apiencoding", CPX_PARAM_APIENCODING, 2},
{"solutiontarget", CPX_PARAM_SOLUTIONTARGET, 0},
{"clonelog", CPX_PARAM_CLONELOG, 0},
{"bardstart", CPX_PARAM_BARDSTART, 0},
{"barepcomp", CPX_PARAM_BAREPCOMP, 1},
{"bargrowth", CPX_PARAM_BARGROWTH, 1},
{"barobjrng", CPX_PARAM_BAROBJRNG, 1},
{"barpstart", CPX_PARAM_BARPSTART, 0},
{"baralg", CPX_PARAM_BARALG, 0},
{"barcolnz", CPX_PARAM_BARCOLNZ, 0},
{"bardisplay", CPX_PARAM_BARDISPLAY, 0},
{"baritlim", CPX_PARAM_BARITLIM, 0},
{"barmaxcor", CPX_PARAM_BARMAXCOR, 0},
{"barorder", CPX_PARAM_BARORDER, 0},
{"barstartalg", CPX_PARAM_BARSTARTALG, 0},
{"barcrossalg", CPX_PARAM_BARCROSSALG, 0},
{"barqcpepcomp", CPX_PARAM_BARQCPEPCOMP, 1},
{"brdir", CPX_PARAM_BRDIR, 0},
{"bttol", CPX_PARAM_BTTOL, 1},
{"cliques", CPX_PARAM_CLIQUES, 0},
{"coeredind", CPX_PARAM_COEREDIND, 0},
{"covers", CPX_PARAM_COVERS, 0},
{"cutlo", CPX_PARAM_CUTLO, 1},
{"cutup", CPX_PARAM_CUTUP, 1},
{"epagap", CPX_PARAM_EPAGAP, 1},
{"epgap", CPX_PARAM_EPGAP, 1},
{"epint", CPX_PARAM_EPINT, 1},
{"mipdisplay", CPX_PARAM_MIPDISPLAY, 0},
{"mipinterval", CPX_PARAM_MIPINTERVAL, 0},
{"intsollim", CPX_PARAM_INTSOLLIM, 0},
{"nodefileind", CPX_PARAM_NODEFILEIND, 0},
{"nodelim", CPX_PARAM_NODELIM, 0},
{"nodesel", CPX_PARAM_NODESEL, 0},
{"objdif", CPX_PARAM_OBJDIF, 1},
{"mipordind", CPX_PARAM_MIPORDIND, 0},
{"relobjdif", CPX_PARAM_RELOBJDIF, 1},
{"startalg", CPX_PARAM_STARTALG, 0},
{"subalg", CPX_PARAM_SUBALG, 0},
{"trelim", CPX_PARAM_TRELIM, 1},
{"varsel", CPX_PARAM_VARSEL, 0},
{"bndstrenind", CPX_PARAM_BNDSTRENIND, 0},
{"heurfreq", CPX_PARAM_HEURFREQ, 0},
{"mipordtype", CPX_PARAM_MIPORDTYPE, 0},
{"cutsfactor", CPX_PARAM_CUTSFACTOR, 1},
{"relaxpreind", CPX_PARAM_RELAXPREIND, 0},
{"preslvnd", CPX_PARAM_PRESLVND, 0},
{"bbinterval", CPX_PARAM_BBINTERVAL, 0},
{"flowcovers", CPX_PARAM_FLOWCOVERS, 0},
{"implbd", CPX_PARAM_IMPLBD, 0},
{"probe", CPX_PARAM_PROBE, 0},
{"gubcovers", CPX_PARAM_GUBCOVERS, 0},
{"strongcandlim", CPX_PARAM_STRONGCANDLIM, 0},
{"strongitlim", CPX_PARAM_STRONGITLIM, 0},
{"fraccand", CPX_PARAM_FRACCAND, 0},
{"fraccuts", CPX_PARAM_FRACCUTS, 0},
{"fracpass", CPX_PARAM_FRACPASS, 0},
{"flowpaths", CPX_PARAM_FLOWPATHS, 0},
{"mircuts", CPX_PARAM_MIRCUTS, 0},
{"disjcuts", CPX_PARAM_DISJCUTS, 0},
{"aggcutlim", CPX_PARAM_AGGCUTLIM, 0},
{"mipcbredlp", CPX_PARAM_MIPCBREDLP, 0},
{"cutpass", CPX_PARAM_CUTPASS, 0},
{"mipemphasis", CPX_PARAM_MIPEMPHASIS, 0},
{"symmetry", CPX_PARAM_SYMMETRY, 0},
{"divetype", CPX_PARAM_DIVETYPE, 0},
{"rinsheur", CPX_PARAM_RINSHEUR, 0},
{"submipnodelim", CPX_PARAM_SUBMIPNODELIM, 0},
{"lbheur", CPX_PARAM_LBHEUR, 0},
{"repeatpresolve", CPX_PARAM_REPEATPRESOLVE, 0},
{"probetime", CPX_PARAM_PROBETIME, 1},
{"polishtime", CPX_PARAM_POLISHTIME, 1},
{"repairtries", CPX_PARAM_REPAIRTRIES, 0},
{"eplin", CPX_PARAM_EPLIN, 1},
{"eprelax", CPX_PARAM_EPRELAX, 1},
{"fpheur", CPX_PARAM_FPHEUR, 0},
{"eachcutlim", CPX_PARAM_EACHCUTLIM, 0},
{"solnpoolcapacity", CPX_PARAM_SOLNPOOLCAPACITY, 0},
{"solnpoolreplace", CPX_PARAM_SOLNPOOLREPLACE, 0},
{"solnpoolgap", CPX_PARAM_SOLNPOOLGAP, 1},
{"solnpoolagap", CPX_PARAM_SOLNPOOLAGAP, 1},
{"solnpoolintensity", CPX_PARAM_SOLNPOOLINTENSITY, 0},
{"populatelim", CPX_PARAM_POPULATELIM, 0},
{"mipsearch", CPX_PARAM_MIPSEARCH, 0},
{"miqcpstrat", CPX_PARAM_MIQCPSTRAT, 0},
{"zerohalfcuts", CPX_PARAM_ZEROHALFCUTS, 0},
{"polishafterepagap", CPX_PARAM_POLISHAFTEREPAGAP, 1},
{"polishafterepgap", CPX_PARAM_POLISHAFTEREPGAP, 1},
{"polishafternode", CPX_PARAM_POLISHAFTERNODE, 0},
{"polishafterintsol", CPX_PARAM_POLISHAFTERINTSOL, 0},
{"polishaftertime", CPX_PARAM_POLISHAFTERTIME, 1},
{"mcfcuts", CPX_PARAM_MCFCUTS, 0},
{"mipkappastats", CPX_PARAM_MIPKAPPASTATS, 0},
{"auxrootthreads", CPX_PARAM_AUXROOTTHREADS, 0},
{"intsolfileprefix", CPX_PARAM_INTSOLFILEPREFIX, 2},
{"netitlim", CPX_PARAM_NETITLIM, 0},
{"netepopt", CPX_PARAM_NETEPOPT, 1},
{"neteprhs", CPX_PARAM_NETEPRHS, 1},
{"netppriind", CPX_PARAM_NETPPRIIND, 0},
{"netdisplay", CPX_PARAM_NETDISPLAY, 0},
{"qpnzreadlim", CPX_PARAM_QPNZREADLIM, 0},
{"qpmakepsdind", CPX_PARAM_QPMAKEPSDIND, 0},

#elif CPLEX==12

#define NUMPARAMS 171
static struct param_desc params[NUMPARAMS+NUMALIASES] = {
{"advind", CPX_PARAM_ADVIND, 0},
{"aggfill", CPX_PARAM_AGGFILL, 0},
{"aggind", CPX_PARAM_AGGIND, 0},
{"basinterval", CPX_PARAM_BASINTERVAL, 0},
{"cfilemul", CPX_PARAM_CFILEMUL, 0},
{"clocktype", CPX_PARAM_CLOCKTYPE, 0},
{"craind", CPX_PARAM_CRAIND, 0},
{"depind", CPX_PARAM_DEPIND, 0},
{"dpriind", CPX_PARAM_DPRIIND, 0},
{"pricelim", CPX_PARAM_PRICELIM, 0},
{"epmrk", CPX_PARAM_EPMRK, 1},
{"epopt", CPX_PARAM_EPOPT, 1},
{"epper", CPX_PARAM_EPPER, 1},
{"eprhs", CPX_PARAM_EPRHS, 1},
{"fastmip", CPX_PARAM_FASTMIP, 0},
{"simdisplay", CPX_PARAM_SIMDISPLAY, 0},
{"itlim", CPX_PARAM_ITLIM, 0},
{"rowreadlim", CPX_PARAM_ROWREADLIM, 0},
{"netfind", CPX_PARAM_NETFIND, 0},
{"colreadlim", CPX_PARAM_COLREADLIM, 0},
{"nzreadlim", CPX_PARAM_NZREADLIM, 0},
{"objllim", CPX_PARAM_OBJLLIM, 1},
{"objulim", CPX_PARAM_OBJULIM, 1},
{"perind", CPX_PARAM_PERIND, 0},
{"perlim", CPX_PARAM_PERLIM, 0},
{"ppriind", CPX_PARAM_PPRIIND, 0},
{"preind", CPX_PARAM_PREIND, 0},
{"reinv", CPX_PARAM_REINV, 0},
{"reverseind", CPX_PARAM_REVERSEIND, 0},
{"rfilemul", CPX_PARAM_RFILEMUL, 0},
{"scaind", CPX_PARAM_SCAIND, 0},
{"scrind", CPX_PARAM_SCRIND, 0},
{"singlim", CPX_PARAM_SINGLIM, 0},
{"singtol", CPX_PARAM_SINGTOL, 0},
{"tilim", CPX_PARAM_TILIM, 1},
{"xxxind", CPX_PARAM_XXXIND, 0},
{"predual", CPX_PARAM_PREDUAL, 0},
{"epopt_h", CPX_PARAM_EPOPT_H, 0},
{"eprhs_h", CPX_PARAM_EPRHS_H, 0},
{"prepass", CPX_PARAM_PREPASS, 0},
{"datacheck", CPX_PARAM_DATACHECK, 0},
{"reduce", CPX_PARAM_REDUCE, 0},
{"prelinear", CPX_PARAM_PRELINEAR, 0},
{"lpmethod", CPX_PARAM_LPMETHOD, 0},
{"qpmethod", CPX_PARAM_QPMETHOD, 0},
{"workdir", CPX_PARAM_WORKDIR, 2},
{"workmem", CPX_PARAM_WORKMEM, 1},
{"threads", CPX_PARAM_THREADS, 0},
{"conflictdisplay", CPX_PARAM_CONFLICTDISPLAY, 0},
{"siftdisplay", CPX_PARAM_SIFTDISPLAY, 0},
{"siftalg", CPX_PARAM_SIFTALG, 0},
{"siftitlim", CPX_PARAM_SIFTITLIM, 0},
{"mpslongnum", CPX_PARAM_MPSLONGNUM, 0},
{"memoryemphasis", CPX_PARAM_MEMORYEMPHASIS, 0},
{"numericalemphasis", CPX_PARAM_NUMERICALEMPHASIS, 0},
{"feasoptmode", CPX_PARAM_FEASOPTMODE, 0},
{"parallelmode", CPX_PARAM_PARALLELMODE, 0},
{"tuningmeasure", CPX_PARAM_TUNINGMEASURE, 0},
{"tuningrepeat", CPX_PARAM_TUNINGREPEAT, 0},
{"tuningtilim", CPX_PARAM_TUNINGTILIM, 1},
{"tuningdisplay", CPX_PARAM_TUNINGDISPLAY, 0},
{"writelevel", CPX_PARAM_WRITELEVEL, 0},
{"randomseed", CPX_PARAM_RANDOMSEED, 0},
{"dettilim", CPX_PARAM_DETTILIM, 1},
{"fileencoding", CPX_PARAM_FILEENCODING, 2},
{"apiencoding", CPX_PARAM_APIENCODING, 2},
{"solutiontarget", CPX_PARAM_SOLUTIONTARGET, 0},
{"clonelog", CPX_PARAM_CLONELOG, 0},
{"tuningdettilim", CPX_PARAM_TUNINGDETTILIM, 1},
{"bardstart", CPX_PARAM_BARDSTART, 0},
{"barepcomp", CPX_PARAM_BAREPCOMP, 1},
{"bargrowth", CPX_PARAM_BARGROWTH, 1},
{"barobjrng", CPX_PARAM_BAROBJRNG, 1},
{"barpstart", CPX_PARAM_BARPSTART, 0},
{"baralg", CPX_PARAM_BARALG, 0},
{"barcolnz", CPX_PARAM_BARCOLNZ, 0},
{"bardisplay", CPX_PARAM_BARDISPLAY, 0},
{"baritlim", CPX_PARAM_BARITLIM, 0},
{"barmaxcor", CPX_PARAM_BARMAXCOR, 0},
{"barorder", CPX_PARAM_BARORDER, 0},
{"barstartalg", CPX_PARAM_BARSTARTALG, 0},
{"barcrossalg", CPX_PARAM_BARCROSSALG, 0},
{"barqcpepcomp", CPX_PARAM_BARQCPEPCOMP, 1},
{"brdir", CPX_PARAM_BRDIR, 0},
{"bttol", CPX_PARAM_BTTOL, 1},
{"cliques", CPX_PARAM_CLIQUES, 0},
{"coeredind", CPX_PARAM_COEREDIND, 0},
{"covers", CPX_PARAM_COVERS, 0},
{"cutlo", CPX_PARAM_CUTLO, 1},
{"cutup", CPX_PARAM_CUTUP, 1},
{"epagap", CPX_PARAM_EPAGAP, 1},
{"epgap", CPX_PARAM_EPGAP, 1},
{"epint", CPX_PARAM_EPINT, 1},
{"mipdisplay", CPX_PARAM_MIPDISPLAY, 0},
{"mipinterval", CPX_PARAM_MIPINTERVAL, 0},
{"intsollim", CPX_PARAM_INTSOLLIM, 0},
{"nodefileind", CPX_PARAM_NODEFILEIND, 0},
{"nodelim", CPX_PARAM_NODELIM, 0},
{"nodesel", CPX_PARAM_NODESEL, 0},
{"objdif", CPX_PARAM_OBJDIF, 1},
{"mipordind", CPX_PARAM_MIPORDIND, 0},
{"relobjdif", CPX_PARAM_RELOBJDIF, 1},
{"startalg", CPX_PARAM_STARTALG, 0},
{"subalg", CPX_PARAM_SUBALG, 0},
{"trelim", CPX_PARAM_TRELIM, 1},
{"varsel", CPX_PARAM_VARSEL, 0},
{"bndstrenind", CPX_PARAM_BNDSTRENIND, 0},
{"heurfreq", CPX_PARAM_HEURFREQ, 0},
{"mipordtype", CPX_PARAM_MIPORDTYPE, 0},
{"cutsfactor", CPX_PARAM_CUTSFACTOR, 1},
{"relaxpreind", CPX_PARAM_RELAXPREIND, 0},
{"preslvnd", CPX_PARAM_PRESLVND, 0},
{"bbinterval", CPX_PARAM_BBINTERVAL, 0},
{"flowcovers", CPX_PARAM_FLOWCOVERS, 0},
{"implbd", CPX_PARAM_IMPLBD, 0},
{"probe", CPX_PARAM_PROBE, 0},
{"gubcovers", CPX_PARAM_GUBCOVERS, 0},
{"strongcandlim", CPX_PARAM_STRONGCANDLIM, 0},
{"strongitlim", CPX_PARAM_STRONGITLIM, 0},
{"fraccand", CPX_PARAM_FRACCAND, 0},
{"fraccuts", CPX_PARAM_FRACCUTS, 0},
{"fracpass", CPX_PARAM_FRACPASS, 0},
{"flowpaths", CPX_PARAM_FLOWPATHS, 0},
{"mircuts", CPX_PARAM_MIRCUTS, 0},
{"disjcuts", CPX_PARAM_DISJCUTS, 0},
{"aggcutlim", CPX_PARAM_AGGCUTLIM, 0},
{"mipcbredlp", CPX_PARAM_MIPCBREDLP, 0},
{"cutpass", CPX_PARAM_CUTPASS, 0},
{"mipemphasis", CPX_PARAM_MIPEMPHASIS, 0},
{"symmetry", CPX_PARAM_SYMMETRY, 0},
{"divetype", CPX_PARAM_DIVETYPE, 0},
{"rinsheur", CPX_PARAM_RINSHEUR, 0},
{"submipnodelim", CPX_PARAM_SUBMIPNODELIM, 0},
{"lbheur", CPX_PARAM_LBHEUR, 0},
{"repeatpresolve", CPX_PARAM_REPEATPRESOLVE, 0},
{"probetime", CPX_PARAM_PROBETIME, 1},
{"polishtime", CPX_PARAM_POLISHTIME, 1},
{"repairtries", CPX_PARAM_REPAIRTRIES, 0},
{"eplin", CPX_PARAM_EPLIN, 1},
{"eprelax", CPX_PARAM_EPRELAX, 1},
{"fpheur", CPX_PARAM_FPHEUR, 0},
{"eachcutlim", CPX_PARAM_EACHCUTLIM, 0},
{"solnpoolcapacity", CPX_PARAM_SOLNPOOLCAPACITY, 0},
{"solnpoolreplace", CPX_PARAM_SOLNPOOLREPLACE, 0},
{"solnpoolgap", CPX_PARAM_SOLNPOOLGAP, 1},
{"solnpoolagap", CPX_PARAM_SOLNPOOLAGAP, 1},
{"solnpoolintensity", CPX_PARAM_SOLNPOOLINTENSITY, 0},
{"populatelim", CPX_PARAM_POPULATELIM, 0},
{"mipsearch", CPX_PARAM_MIPSEARCH, 0},
{"miqcpstrat", CPX_PARAM_MIQCPSTRAT, 0},
{"zerohalfcuts", CPX_PARAM_ZEROHALFCUTS, 0},
{"polishafterepagap", CPX_PARAM_POLISHAFTEREPAGAP, 1},
{"polishafterepgap", CPX_PARAM_POLISHAFTEREPGAP, 1},
{"polishafternode", CPX_PARAM_POLISHAFTERNODE, 0},
{"polishafterintsol", CPX_PARAM_POLISHAFTERINTSOL, 0},
{"polishaftertime", CPX_PARAM_POLISHAFTERTIME, 1},
{"mcfcuts", CPX_PARAM_MCFCUTS, 0},
{"mipkappastats", CPX_PARAM_MIPKAPPASTATS, 0},
{"auxrootthreads", CPX_PARAM_AUXROOTTHREADS, 0},
{"intsolfileprefix", CPX_PARAM_INTSOLFILEPREFIX, 2},
{"probedettime", CPX_PARAM_PROBEDETTIME, 1},
{"polishafterdettime", CPX_PARAM_POLISHAFTERDETTIME, 1},
{"landpcuts", CPX_PARAM_LANDPCUTS, 0},
{"netitlim", CPX_PARAM_NETITLIM, 0},
{"netepopt", CPX_PARAM_NETEPOPT, 1},
{"neteprhs", CPX_PARAM_NETEPRHS, 1},
{"netppriind", CPX_PARAM_NETPPRIIND, 0},
{"netdisplay", CPX_PARAM_NETDISPLAY, 0},
{"qpnzreadlim", CPX_PARAM_QPNZREADLIM, 0},
{"calcqcpduals", CPX_PARAM_CALCQCPDUALS, 0},
{"qpmakepsdind", CPX_PARAM_QPMAKEPSDIND, 0},
#endif


/*
 * Add some version-independent aliases to the table
 * This must remain at the end of the table!!!
 * If you add lines here, update NUMALIASES above!
 * NUMALIASES lines follow
 */

{"timelimit", CPX_PARAM_TILIM, 1},
{"time_limit", CPX_PARAM_TILIM, 1},
{"perturbation_const", CPX_PARAM_EPPER, 1},
{"lowerobj_limit", CPX_PARAM_OBJLLIM, 1},
{"upperobj_limit", CPX_PARAM_OBJULIM, 1},
{"feasibility_tol", CPX_PARAM_EPRHS, 1},
{"markowitz_tol", CPX_PARAM_EPMRK, 1},
{"optimality_tol", CPX_PARAM_EPOPT, 1},
{"backtrack", CPX_PARAM_BTTOL, 1},
{"treememory", CPX_PARAM_TRELIM, 1},
{"lowercutoff", CPX_PARAM_CUTLO, 1},
{"uppercutoff", CPX_PARAM_CUTUP, 1},
{"absmipgap", CPX_PARAM_EPAGAP, 1},
{"mipgap", CPX_PARAM_EPGAP, 1},
{"integrality", CPX_PARAM_EPINT, 1},
{"objdifference", CPX_PARAM_OBJDIF, 1},
{"relobjdifference", CPX_PARAM_RELOBJDIF, 1},
{"crash", CPX_PARAM_CRAIND, 0},
{"dgradient", CPX_PARAM_DPRIIND, 0},
{"pricing", CPX_PARAM_PRICELIM, 0},
/*{"iisfind", CPX_PARAM_IISIND, 0}, removed in CPLEX 10 */
{"perturbation_ind", CPX_PARAM_PERIND, 0},
{"pgradient", CPX_PARAM_PPRIIND, 0},
{"refactor", CPX_PARAM_REINV, 0},
{"iteration_limit", CPX_PARAM_ITLIM, 0},
{"singularity_limit", CPX_PARAM_SINGLIM, 0},
{"simplex_display", CPX_PARAM_SIMDISPLAY, 0},
{"basisinterval", CPX_PARAM_BASINTERVAL, 0},
{"branch", CPX_PARAM_BRDIR, 0},
{"nodeselect", CPX_PARAM_NODESEL, 0},
{"order", CPX_PARAM_MIPORDIND, 0},
{"startalgorithm", CPX_PARAM_STARTALG, 0},
{"subalgorithm", CPX_PARAM_SUBALG, 0},
{"variableselect", CPX_PARAM_VARSEL, 0},
{"solution_limit", CPX_PARAM_INTSOLLIM, 0},
{"node_limit", CPX_PARAM_NODELIM, 0},
{"mip_display", CPX_PARAM_MIPDISPLAY, 0},
{"mip_interval", CPX_PARAM_MIPINTERVAL, 0},
{"advance", CPX_PARAM_ADVIND, 0},
{"aggregator", CPX_PARAM_AGGIND, 0},
{"coeffreduce", CPX_PARAM_COEREDIND, 0},
{"dependency", CPX_PARAM_DEPIND, 0},
{"presolve", CPX_PARAM_PREIND, 0},
{"scale", CPX_PARAM_SCAIND, 0},
{"xxxstart", CPX_PARAM_XXXIND, 0},

};

#endif /*CPLEX*/
