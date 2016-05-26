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

#ifdef XPRESS

static int
xprs_getintattr(XPRSprob lp, int attr)
{
    int value;
    XPRSgetintattrib(lp, attr, &value);
    return value;
}


void XPRS_CC
_get_xpress_sol(XPRSprob lp, void * solution)
{
    struct lp_sol *sol = (struct lp_sol *) solution;

    XPRSgetsol(lp, sol->sols, sol->slacks, sol->pis, sol->djs);
}


static int
cpx_prepare_solve(lp_desc* lpd, struct lp_meth *meth,
	struct lp_sol *sol, double timeout)
{
#ifdef HAS_MIQP_CALLBACKBUG
    /* too much trouble to support MIQP with older XPRESS with this bug;
       the MIQP method is `not recommended' for use in these versions
       by DASH in anycase
    */
    switch (lpd->prob_type)
    {
    case PROBLEM_MIQP:
    case PROBLEM_FIXEDQ:
    case PROBLEM_RELAXEDQ:
	Fprintf(Current_Error, "Eplex error: quadratic MIP not supported for this solver because it is unstable.\n");
	ec_flush(Current_Error);
	return -1;
    }
#endif

    CallN(XPRSsetintcontrol(lpd->lp, XPRS_SOLUTIONFILE, 0));

    /* set up call-back to get solution state at each integer solution */
    if (IsMIPProb(lpd->prob_type))
    {
	CallN(XPRSsetcbintsol(lpd->lp, _get_xpress_sol, (void *)sol));
    }

    if (timeout >= 0.0)
    {
	/* 0 is no timeout, negative for desired semantics */
	int timeout_i;
	timeout = ceil(timeout);	/* avoid round to zero and overflow */
	timeout_i = timeout > INT_MAX ? INT_MAX : (int)timeout;
	Log1(XPRSsetintcontrol(lpd->lp, XPRS_MAXTIME, %d), -timeout_i);
	XPRSsetintcontrol(lpd->lp, XPRS_MAXTIME, -timeout_i);
    }

    /* meth, auxmeth, node_meth are kept upto-date as they may be required
       to determine the actual method used to solve the problem
    */
    switch (meth->meth)
    {
    case METHOD_DEFAULT:        meth->meth_string = "";  break;
    case METHOD_PRIMAL:		meth->meth_string = "p"; break;
    case METHOD_DUAL:		
	if (IsQPProb(lpd->prob_type))
	{
	    Fprintf(Current_Error, 
		    "Eplex warning: Dual Simplex method not available to solve a quadratic"
		    " problem for Xpress MP. Using Primal instead.\n");
	    ec_flush(Current_Error);
	    meth->meth_string = "p";
	    meth->meth = METHOD_PRIMAL;
	} else	meth->meth_string = "d"; 
	break;
    case METHOD_BAR:
        meth->meth_string = "b";
	switch (meth->auxmeth)
	{
	default:
	case METHOD_DUAL:
	    Fprintf(Current_Error, 
		    "Eplex warning: Specified crossover method unavilable for"
		    " barrier method. Using Primal instead.\n");
	    ec_flush(Current_Error);
	case METHOD_DEFAULT:
	    meth->auxmeth = METHOD_PRIMAL;
	case METHOD_PRIMAL:
	    /* Richard Laundy says primal Simplex is used to do crossover */
	    XPRSsetintcontrol(lpd->lp, XPRS_CROSSOVER, 1);
	    break;
	case METHOD_NONE:
	    switch (lpd->prob_type)
	    {
	    default:
		XPRSsetintcontrol(lpd->lp, XPRS_CROSSOVER, 0);
		break;
	    case PROBLEM_MIP:
	    case PROBLEM_FIXEDL:
	    case PROBLEM_MIQP:
	    case PROBLEM_FIXEDQ:
		/* according to Richard Laundy @ DASH, 2004-03-26,
                   turning off CROSSOVER for MIP case would lead to problems
		*/
		Fprintf(Current_Error, 
			"Eplex warning: For MIP problems, crossover must be "
                        "enabled for barrier method. Using Primal crossover.\n");
		ec_flush(Current_Error);
		XPRSsetintcontrol(lpd->lp, XPRS_CROSSOVER, 1);
		meth->auxmeth = METHOD_PRIMAL;
		break;
	    }
	    break;
	}
	break;
    default:
	Fprintf(Current_Error, "Eplex warning: Specified method unavilable for"
		    " solver. Using default instead.\n");
	ec_flush(Current_Error);
	meth->meth_string = "";
	meth->meth = METHOD_DEFAULT;
	break;
    }

    /* ignore node_auxmeth */
    switch (meth->node_meth)
    {
    case METHOD_DEFAULT:        meth->node_meth_string = "g";  break;
    case METHOD_PRIMAL:		meth->node_meth_string = "gp"; break;
    case METHOD_DUAL:		
	if (IsQPProb(lpd->prob_type))
	{
	    Fprintf(Current_Error, 
		    "Eplex warning: Dual Simplex method not available to solve a quadratic"
		    " problem for Xpress MP. Using Primal instead.\n");
	    ec_flush(Current_Error);
	    meth->meth_string = "gp";
	    meth->node_meth = METHOD_PRIMAL;
	} else	meth->meth_string = "gd"; 
	break;
    case METHOD_BAR:            meth->node_meth_string = "gb"; break;
    default:
	Fprintf(Current_Error, "Eplex warning: Specified node method unavilable for"
		    " solver. Using default instead.\n");
	ec_flush(Current_Error);
	meth->node_meth_string = "";
	meth->node_meth = METHOD_DEFAULT;
	break;
    }
    return 0;
}


static int
_mip_opt(lp_desc * lpd, struct lp_meth *meth, void * solution)
{
    int res;
    struct lp_sol *sol = (struct lp_sol *) solution;

    /* can use same probname as we are not going to solve using the original */
    if (lpd->copystatus != XP_COPYOFF)
    {
	/* XPRScopyprob() should cleanup any existing problem 
	   According to Michael Perregaard@DASH, XPRScopycontrols() is
	   no longer needed after an XPRScopyprob() in Xpress 14+, as
	   controls are copied by the copyprob
	*/
	CallN(XPRScopyprob(lpd->lpcopy, lpd->lp, lpd->probname));
	CallN(XPRScopycontrols(lpd->lpcopy, lpd->lp));
	CallN(XPRScopycallbacks(lpd->lpcopy, lpd->lp));
	lpd->copystatus = XP_COPYCURRENT;
    }
    else
    {/* this turns off operations which might delete columns, to avoid
	the problem of bounds being sent to the wrong column
     */       
	XPRSsetintcontrol(lpd->lp, XPRS_PRESOLVEOPS,86);
    }


    /* the global optimisation is broken down into getting the initial
       root LP relaxation and subsequent MIP so that the basis for 
       the root node can be obtained: this should be better than a solution
    */
    CallN(XPRSscale(lpd->lp, NULL, NULL));
    if (lpd->sense == SENSE_MIN) 
    {

	Log1(XPRSminim(lpd->lpcopy, "%s"), meth->meth_string);
	res = XPRSminim(lpd->lpcopy, meth->meth_string);
	if (res == 0)
	{
	    /* proceed with the global solve only if there is an optimal LP:
	       otherwise the XPRS_MIPSTATUS loses its XPRS_MIP_LP_NOT_OPTIMAL
               status when XPRSminim is called again
	    */
	    XPRSgetintattrib(lpd->lpcopy, XPRS_MIPSTATUS, &lpd->sol_state);
	    if (lpd->sol_state != XPRS_MIP_LP_NOT_OPTIMAL)
	    {
		if (sol->cbase != NULL) 
		    XPRSgetbasis(lpd->lpcopy, sol->rbase, sol->cbase);
		Log1(XPRSminim(lpd->lpcopy, "%s"), meth->node_meth_string);
		res = XPRSminim(lpd->lpcopy, meth->node_meth_string);
	    }
	}
    }
    else
    {
	Log1(XPRSmaxim(lpd->lpcopy, "%s"), meth->meth_string);
	res = XPRSmaxim(lpd->lpcopy, meth->meth_string);
	if (res == 0)
	{
	    XPRSgetintattrib(lpd->lpcopy, XPRS_MIPSTATUS, &lpd->sol_state);
	    if (lpd->sol_state != XPRS_MIP_LP_NOT_OPTIMAL)
	    {
		if (sol->cbase != NULL) 
		    XPRSgetbasis(lpd->lpcopy, sol->rbase, sol->cbase);
		Log1(XPRSmaxim(lpd->lpcopy, "%s"), meth->node_meth_string);
		res = XPRSmaxim(lpd->lpcopy, meth->node_meth_string);
	    }
	}
    }
    return res;
}


static int
cpx_solve(lp_desc* lpd, struct lp_meth *meth, struct lp_sol *sol,
	double* bestbound, double* worstbound)
{
    int res;

    switch (lpd->prob_type)
    {

	case PROBLEM_FIXEDL:
	case PROBLEM_FIXEDQ:
	{
	    int status;
	    char c[1];
	    double * oldsols;

	    if (lpd->copystatus == XP_COPYOFF) 
	    { 
		return -1;
	    }
	    if (lpd->copystatus == XP_COPYCURRENT)
	    {
		XPRSgetintattrib(lpd->lpcopy, XPRS_MIPSTATUS, &status);
		if (status == XPRS_MIP_OPTIMAL)
		    oldsols = sol->oldsols;
	    }
	    else
		status = XPRS_MIP_NOT_LOADED;
	    if (status != XPRS_MIP_OPTIMAL)
	    {/* solve the MIP first (no cutpool cstrs) */
		CallN(XPRSsetcbintsol(lpd->lp, _get_xpress_sol, (void *)sol));
		if (lpd->lpcopy == lpd->lp) CallN(XPRScreateprob(&lpd->lpcopy));
		res = _mip_opt(lpd, meth, (void *)sol);
		oldsols = sol->sols;
		XPRSgetintattrib(lpd->lpcopy, XPRS_MIPSTATUS, &status);
	    }
    
	    /* solve the fixed LP if we have an optimal MIP */
	    if (status == XPRS_MIP_OPTIMAL)
	    {
		int i;
# ifdef HAS_MIQP_FIXEDCOPYBUG
	   /* According to Richard Laundy@Dash, 2004-10-01, copying a prob.
	      that originally has quadratic coeffs which has been zeroed
	      (in our case because of a probe's linear objective) lead
	      to the crash when trying to fix the columns. 
	      To work round this, we always put in a dummy 
	      quadratic term before copying, and remove it afterwards. 
	      This is only needed for changing quad. obj -> linear obj,
	      but we can't determine this easily.
	   */
		if (lpd->prob_type == PROBLEM_FIXEDL) XPRSchgqobj(lpd->lp, 0, 0, 1.0);
# endif
		CallN(XPRScopyprob(lpd->lpcopy, lpd->lp, lpd->probname));
		CallN(XPRScopycontrols(lpd->lpcopy, lpd->lp));
		CallN(XPRScopycallbacks(lpd->lpcopy, lpd->lp));
# ifdef HAS_MIQP_FIXEDCOPYBUG
		/* now remove the dummy quadratic term */
		if (lpd->prob_type == PROBLEM_FIXEDL)
		{
		    XPRSchgqobj(lpd->lpcopy, 0, 0, 0.0);
		    XPRSchgqobj(lpd->lp, 0, 0, 0.0);
		}
# endif
		lpd->copystatus = XP_COPYCURRENT;

		for(i=SOLVER_MAT_BASE; i<lpd->mac; i++)
		{
		    XPRSgetcoltype(lpd->lpcopy, c, i, i);
		    if (c[0] == 'I' || c[0] == 'B') 
		    {
			Log2(
			{\n\
			int i = %d;\n\
			double oldsols = %f;\n\
			XPRSchgbounds(lpd->lpcopy, 1, &i, "B", &oldsols);\n\
			}, i, oldsols[i]
			    );
			XPRSchgbounds(lpd->lpcopy, 1, &i, "B", &(oldsols[i]));
		    }
		}

		CallN(XPRSscale(lpd->lp, NULL, NULL));
		if (lpd->sense == SENSE_MIN)
		{
		    Log1(XPRSminim(lpd->lpcopy, "%s"), meth->meth_string);
		    res = XPRSminim(lpd->lpcopy, meth->meth_string);
		}
		else
		{
		    Log1(XPRSmaxim(lpd->lpcopy, "%s"), meth->meth_string);
		    res = XPRSmaxim(lpd->lpcopy, meth->meth_string);
		}
	    }
	    break;
	}

	case PROBLEM_RELAXEDL:
	case PROBLEM_RELAXEDQ:
	    /* must solve a copy, as Xpress considers the MIP as started with
	       the solving of the root node, and problem cannot be modified
	    */
	    if (lpd->copystatus == XP_COPYOFF) 
	    { 
		return -1;
	    }
	    if (lpd->lpcopy != lpd->lp)
	    {
		CallN(XPRScopyprob(lpd->lpcopy, lpd->lp, lpd->probname));
		CallN(XPRScopycontrols(lpd->lpcopy, lpd->lp));
		CallN(XPRScopycallbacks(lpd->lpcopy, lpd->lp));
		lpd->copystatus = XP_COPYCURRENT;
	    }
	    CallN(XPRSscale(lpd->lp, NULL, NULL));
	    if (lpd->sense == SENSE_MIN) 
	    {
		Log1(XPRSminim(lpd->lpcopy,"%s"), meth->meth_string);
		res = XPRSminim(lpd->lpcopy, meth->meth_string);
	    }
	    else
	    {
		Log1(XPRSmaxim(lpd->lpcopy,"%s"), meth->meth_string);
		res = XPRSmaxim(lpd->lpcopy, meth->meth_string);
	    }
	    break;

	case PROBLEM_LP:
	    CallN(XPRSscale(lpd->lp, NULL, NULL));
	    if (lpd->sense == SENSE_MIN) 
	    { 
		Log1(XPRSminim(lpd->lp,"%s"), meth->meth_string);
		res = XPRSminim(lpd->lp, meth->meth_string);
	    }
	    else
	    {
		Log1(XPRSmaxim(lpd->lp,"%s"), meth->meth_string);
		res = XPRSmaxim(lpd->lp, meth->meth_string);
	    }
	    break;

	case PROBLEM_QP:
	    CallN(XPRSscale(lpd->lp, NULL, NULL));
	    if (lpd->sense == SENSE_MIN)
	    {
		Call(res, XPRSminim(lpd->lp, meth->meth_string));
	    }
	    else
	    {
		Call(res, XPRSmaxim(lpd->lp, meth->meth_string));
	    }
	    break;

	case PROBLEM_MIP:
	case PROBLEM_MIQP:
	    res = _mip_opt(lpd, meth, (void *)sol);
	    break;

	default:
	    return -1;
    }

    /*********************************************************************
     *     Get State Information from External Solver                    *
     *********************************************************************/

    /* here lpcopy should be the problem that has been solved, either:
	 a) It is the copy that is solved (MIP)
	 b) It is the same as lp (non-MIP or if XP_COPYOFF)
       so always obtain solution information from lpcopy
    */
    if (res) 
    { 
	int err;
	char errmsg[256];

	XPRSgetintattrib(lpd->lpcopy, XPRS_ERRORCODE,&err); 
	Fprintf(Current_Error, "XPRESS problem: %d\n", err);
	XPRSgetlasterror(lpd->lpcopy, errmsg);
	Fprintf(Current_Error, "problem: %s\n", errmsg);
	ec_flush(Current_Error);
    }
    Get_Xp_Stat(lpd);
    XPRSgetintattrib(lpd->lpcopy, XPRS_SIMPLEXITER, &lpd->sol_itcnt);
    XPRSgetintattrib(lpd->lpcopy, XPRS_NODES, &lpd->sol_nodnum); 

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

	    /* In Xpress, we can get more information as we can tell if 
	       cutoff occurred at the root MIP node 
	    */
	    if (IsMIPProb(lpd->prob_type))
	    {
		if (lpd->sol_state == XPRS_LP_CUTOFF) /* at root node */
		{ /* primal or barrier - have a feasible root LP worse than
		     cutoff */
		    XPRSgetdblattrib(lpd->lpcopy, XPRS_MIPOBJVAL, bestbound);
		} else if (lpd->sol_state == XPRS_LP_INFEAS)
		{/* root is infesible => MIP is infeasible  */
		    *bestbound = (lpd->sense ==  SENSE_MIN ?  HUGE_VAL : -HUGE_VAL);
		} else
		{/* safe option of returning cutoff as best bound */
		    Get_MIPCutOff(lpd, bestbound);
		} 
	    } else /* infeasible LP problem  */
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
	/* Xpress can reach the LPAbortedState either when an LP
	   solving was aborted for an LP problem, or if the root LP solve
	   was aborted for a MIP problem
	*/
	int attr; /* variable for integer attribute */

	/* meth is used for LP and root MIP LP */

	if (meth->meth == METHOD_DEFAULT) 
	{/* turn `default' method into actual method used 
	    CAUTION: may need revising if Xpress changes the default
	    method mapping! [they have no call to get actual method used]
	 */
	    int method;

	    XPRSgetintcontrol(lpd->lpcopy, XPRS_DEFAULTALG, &method);
	    switch (method)
	    {
	    case 1: 
		meth->meth = IsQPProb(lpd->prob_type) ? METHOD_BAR : METHOD_DUAL; 
		break;
	    case 2: meth->meth = METHOD_DUAL; break;
	    case 3: meth->meth = METHOD_PRIMAL; break;
	    case 4: meth->meth = METHOD_BAR; break;
	    }
	}

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
	    /* In XPRESS, cross-over may be performed after aborting
	       from a barrier solve. So we may arrive here if the 
	       cross-over (using primal simplex) was also aborted. We
	       check if this is the case with SIMPLEXITER -- if non-zero,
	       we fall through and treat like METHOD_PRIMAL
	    */
		lpd->descr_state = DESCR_ABORTED_NOSOL;
		XPRSgetintattrib(lpd->lpcopy, XPRS_SIMPLEXITER, &attr);
		if (attr == 0)
		{
		    double infeas;
		    /* Oliver Bastert @ DASH, 2005-06 states that if the
		       BARPRIMAL/DUAL infeasibilities are small enough,
		       the solution can be considered primal and dual feasible
		       respectively. Otherwise, the BARPRIMAL/DUAL objective
		       is usually bounds on the optimal value, but this may
		       be wrong in degenerate cases, so we don't use them
		       here unless they are feasible
		    */
		    lpd->descr_state = DESCR_ABORTED_NOSOL;
		    *bestbound = (lpd->sense ==  SENSE_MIN ?  -HUGE_VAL : HUGE_VAL);
		    lpd->objval = *worstbound = (lpd->sense == SENSE_MIN ? HUGE_VAL :  -HUGE_VAL);
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
		}
		/* if the crossover simplex is performed, fall through and
		   treat as primal simplex
		*/
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
cpx_get_soln_state(lp_desc* lpd, struct lp_sol *sol)
{
    int res;

    /* if MIP callback is used, then the MIP solution state is already
       gathered by the callbacks into the appropriate arrays for
       the MIP case (and basis was obtained already from the root LP)
    */
    if (!IsMIPProb(lpd->prob_type))
    {
	/* using lpcopy because same as lp for LP and QP, but point at
	   actual problem for FIXED
	*/
	res = XPRSgetsol(lpd->lpcopy, sol->sols, sol->slacks, sol->pis, sol->djs);
	Log4(XPRSgetsol(lpd->lpcopy, (double *)malloc(%d*sizeof(double)), 
			(double *)malloc(%d*sizeof(double)),
			(double *)malloc(%d*sizeof(double)),
			(double *)malloc(%d*sizeof(double))),
	     lpd->mac, lpd->mar, lpd->mar, lpd->mac); 

# ifdef HAS_POSTSOLVE
	if (lpd->descr_state == DESCR_ABORTED_SOL)
	    CallN(XPRSpostsolve(lpd->lp)); /* post-solve problem if possible */
# endif
	if ((sol->rbase || sol->cbase) && res == 0)
	    res = XPRSgetbasis(lpd->lpcopy, sol->rbase, sol->cbase);
	if (res)
	    return -1;
    }
    return 0;
}


static int
cpx_delsos(lp_desc *lpd, int from, int to)
{
#ifdef HAS_NO_ADDSOS
    return UNIMPLEMENTED;
#else
    _grow_numbers_array(lpd, to);	/* if necessary */
    return XPRSdelsets(lpd->lp, to-from, &lpd->numbers[from]);
#endif
}


static int
cpx_write(lp_desc *lpd, char *file, char *fmt)
{
    int res;

    if (strcmp(fmt, "lp")==0 || strcmp(fmt, "mps")==0)
    {
	char *flag = strcmp(fmt,"lp")==0 ? "lp" : "p";

	if (lpd->copystatus == XP_COPYCURRENT) 
	{
	    Log2(XPRSwriteprob(lpd->lpcopy, "%s", "%s"), file, flag);
	    res = XPRSwriteprob(lpd->lpcopy, file, flag);
	} else
	{
	    Log2(XPRSwriteprob(lpd->lp, "%s", "%s"), file, flag);
	    res = XPRSwriteprob(lpd->lp, file, flag);
	}
	if (res == 352)		/* not authorised in this version */
	    res = 0;		/* ignore */
    }
    else if (strcmp(fmt, "svf")==0 || strcmp(fmt, "sav")==0)
    {
	if (strcmp(file, "") != 0) 
	{/* give a warning if a file name was given */
	    Fprintf(Current_Error, "Eplex warning: filename %s ignored, as"
		    " filenames cannot be given for Xpress MP's .svf format. "
		    "The problem name %s is used instead.\n",
		    file, lpd->probname);
	    ec_flush(Current_Error);
	}

	if (lpd->copystatus == XP_COPYCURRENT) 
	{
	    Call(res, XPRSsave(lpd->lpcopy));
	} else
	{
	    Call(res, XPRSsave(lpd->lp));
	}
    }
    else
    	res = -1;

    return res;
}


static int
cpx_read(lp_desc *lpd, char *file, char *fmt)
{
    int res;

    if (strlen(file) > XP_PROBNAME_MAX)
    {
	Fprintf(Current_Error, "Eplex error: filename for problem is too"
		" long for Xpress MP.\n");
	ec_flush(Current_Error);
	return -1;
    }
	
    Call(res, XPRScreateprob(&lpd->lp));
    if (res)
    	return -1;
    /* set the defaults *before* reading in the problem */
    CallN(XPRScopycontrols(lpd->lp, cpx_env)); 

    XPRSsetcbmessage(lpd->lp, eclipse_out, NULL);

    if (strcmp(fmt,"lp")==0)
    {
	Call(res, XPRSreadprob(lpd->lp, file, "l"));
    }
    else if (strcmp(fmt,"svf")==0 || strcmp(fmt,"sav")==0)
    {
#if XPRESS < 20
	Call(res, XPRSrestore(lpd->lp, file));
#else
	Call(res, XPRSrestore(lpd->lp, file, ""));
#endif
    }
    else /* (strcmp(fmt,"mps")==0) */
    {
	Call(res, XPRSreadprob(lpd->lp, file, ""));
    }
    /* need to check for possible errors as XPress returns 0 for the
       read functions even if a problem occurred.
       This was suggested by Oliver Bastert @ DASH, 2004-12-08
    */
    if (res == 0) { XPRSgetintattrib(lpd->lp, XPRS_ERRORCODE, &res); }
    if (res)
    	return -1;

    lpd->sense = SENSE_MIN; /* Xpress ignores the sense! */
    CallN(XPRSgetintattrib(lpd->lp, XPRS_COLS, &(lpd->mac)));
    CallN(XPRSgetintattrib(lpd->lp, XPRS_ROWS, &(lpd->mar)));
    CallN(lpd->probname = (char *) Malloc((strlen(file) + 1) * sizeof(char)));
    CallN(strcpy(lpd->probname, file));
    {
	int nmips, nq;

	XPRSgetintattrib(lpd->lp, XPRS_MIPENTS, &nmips);
	XPRSgetintattrib(lpd->lp, XPRS_QELEMS, &nq);
	if (nq > 0)
	{
	    lpd->prob_type = (nmips == 0 ? PROBLEM_QP : PROBLEM_MIQP);
	}
	else
	{
	    lpd->prob_type = (nmips == 0 ? PROBLEM_LP : PROBLEM_MIP);
	}
    }

    lpd->copystatus = XP_COPYINVALID;

    if (lpd->copystatus != XP_COPYOFF)
    {
	Mark_Copy_As_Modified(lpd);
	if (IsMIPProb(lpd->prob_type))
	{
	    Call(res, XPRScreateprob(&lpd->lpcopy));
	    if (res)
	    	return -1;
	}
	else
	    CallN(lpd->lpcopy = lpd->lp);
    }
    else
	CallN(lpd->lpcopy = lpd->lp);

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
 * extract the XPRS_ lines out of xprs.h (only the control variable ones)
 * and substitute as follows:
 *
 *   s/^#define XPRS_\([^ ]*\).*$/{"\L\1\E", XPRS_\1, 0},/
 *
 * mark the int params with 0, the doubles with 1, the strings with 2
 * count the lines and define NUMPARAMS accordingly!
 * add the new section within the proper #if's
 */

#define NUMALIASES 13

#if (XPRESS==13)

#define NUMPARAMS 105
static struct param_desc params[NUMPARAMS+NUMALIASES] = {
{"mpsrhsname", XPRS_MPSRHSNAME, 2},
{"mpsobjname", XPRS_MPSOBJNAME, 2},
{"mpsrangename", XPRS_MPSRANGENAME, 2},
{"mpsboundname", XPRS_MPSBOUNDNAME, 2},
{"outputmask", XPRS_OUTPUTMASK, 2},
{"omnidataname", XPRS_OMNIDATANAME, 2},
{"matrixtol", XPRS_MATRIXTOL, 1},
{"pivottol", XPRS_PIVOTTOL, 1},
{"feastol", XPRS_FEASTOL, 1},
{"outputtol", XPRS_OUTPUTTOL, 1},
{"sosreftol", XPRS_SOSREFTOL, 1},
{"optimalitytol", XPRS_OPTIMALITYTOL, 1},
{"etatol", XPRS_ETATOL, 1},
{"relpivottol", XPRS_RELPIVOTTOL, 1},
{"miptol", XPRS_MIPTOL, 1},
{"degradefactor", XPRS_DEGRADEFACTOR, 1},
{"miptarget", XPRS_MIPTARGET, 1},
{"mipaddcutoff", XPRS_MIPADDCUTOFF, 1},
{"mipabscutoff", XPRS_MIPABSCUTOFF, 1},
{"miprelcutoff", XPRS_MIPRELCUTOFF, 1},
{"pseudocost", XPRS_PSEUDOCOST, 1},
{"penalty", XPRS_PENALTY, 1},
{"bigm", XPRS_BIGM, 1},
{"mipabsstop", XPRS_MIPABSSTOP, 1},
{"miprelstop", XPRS_MIPRELSTOP, 1},
{"choleskytol", XPRS_CHOLESKYTOL, 1},
{"bargapstop", XPRS_BARGAPSTOP, 1},
{"bardualstop", XPRS_BARDUALSTOP, 1},
{"barprimalstop", XPRS_BARPRIMALSTOP, 1},
{"barstepstop", XPRS_BARSTEPSTOP, 1},
{"elimtol", XPRS_ELIMTOL, 1},
{"perturb", XPRS_PERTURB, 1},
{"markowitztol", XPRS_MARKOWITZTOL, 1},
{"recsteplength", XPRS_RECSTEPLENGTH, 1},
{"recexpand", XPRS_RECEXPAND, 1},
{"recshrink", XPRS_RECSHRINK, 1},
{"recstop", XPRS_RECSTOP, 1},
{"ppfactor", XPRS_PPFACTOR, 1},
{"extrarows", XPRS_EXTRAROWS, 0},
{"extracols", XPRS_EXTRACOLS, 0},
{"extraelems", XPRS_EXTRAELEMS, 0},
{"lpiterlimit", XPRS_LPITERLIMIT, 0},
{"lplog", XPRS_LPLOG, 0},
{"scaling", XPRS_SCALING, 0},
{"presolve", XPRS_PRESOLVE, 0},
{"crash", XPRS_CRASH, 0},
{"pricingalg", XPRS_PRICINGALG, 0},
{"invertfreq", XPRS_INVERTFREQ, 0},
{"invertmin", XPRS_INVERTMIN, 0},
{"maxnode", XPRS_MAXNODE, 0},
{"maxtime", XPRS_MAXTIME, 0},
{"maxmipsol", XPRS_MAXMIPSOL, 0},
{"keepmipsol", XPRS_KEEPMIPSOL, 0},
{"defaultalg", XPRS_DEFAULTALG, 0},
{"varselection", XPRS_VARSELECTION, 0},
{"nodeselection", XPRS_NODESELECTION, 0},
{"backtrack", XPRS_BACKTRACK, 0},
{"miplog", XPRS_MIPLOG, 0},
{"mpserrignore", XPRS_MPSERRIGNORE, 0},
{"keepnrows", XPRS_KEEPNROWS, 0},
{"mpsecho", XPRS_MPSECHO, 0},
{"maxpagelines", XPRS_MAXPAGELINES, 0},
{"outputlog", XPRS_OUTPUTLOG, 0},
{"extrapresolve", XPRS_EXTRAPRESOLVE, 0},
{"cpmaxcuts", XPRS_CPMAXCUTS, 0},
{"cpmaxelems", XPRS_CPMAXELEMS, 0},
{"cpkeepallcuts", XPRS_CPKEEPALLCUTS, 0},
{"cachesize", XPRS_CACHESIZE, 0},
{"crossover", XPRS_CROSSOVER, 0},
{"bariterlimit", XPRS_BARITERLIMIT, 0},
{"choleskyalg", XPRS_CHOLESKYALG, 0},
{"baroutput", XPRS_BAROUTPUT, 0},
{"cstyle", XPRS_CSTYLE, 0},
{"extramipents", XPRS_EXTRAMIPENTS, 0},
{"refactor", XPRS_REFACTOR, 0},
{"barthreads", XPRS_BARTHREADS, 0},
{"keepbasis", XPRS_KEEPBASIS, 0},
{"omniformat", XPRS_OMNIFORMAT, 0},
{"version", XPRS_VERSION, 0},
{"recmaxpasses", XPRS_RECMAXPASSES, 0},
{"bigmmethod", XPRS_BIGMMETHOD, 0},
{"rel10style", XPRS_REL10STYLE, 0},
{"mpsnamelength", XPRS_MPSNAMELENGTH, 0},
{"solutionfile", XPRS_SOLUTIONFILE, 0},
{"presolveops", XPRS_PRESOLVEOPS, 0},
{"mippresolve", XPRS_MIPPRESOLVE, 0},
{"maxslave", XPRS_MAXSLAVE, 0},
{"barorder", XPRS_BARORDER, 0},
{"breadthfirst", XPRS_BREADTHFIRST, 0},
{"autoperturb", XPRS_AUTOPERTURB, 0},
{"densecollimit", XPRS_DENSECOLLIMIT, 0},
{"cutfreq", XPRS_CUTFREQ, 0},
{"trace", XPRS_TRACE, 0},
{"maxiis", XPRS_MAXIIS, 0},
{"cputime", XPRS_CPUTIME, 0},
{"covercuts", XPRS_COVERCUTS, 0},
{"gomcuts", XPRS_GOMCUTS, 0},
{"mpsformat", XPRS_MPSFORMAT, 0},
{"cutstrategy", XPRS_CUTSTRATEGY, 0},
{"cutdepth", XPRS_CUTDEPTH, 0},
{"treecovercuts", XPRS_TREECOVERCUTS, 0},
{"treegomcuts", XPRS_TREEGOMCUTS, 0},
{"barmemory", XPRS_BARMEMORY, 0},
/*{"dualgradient", XPRS_DUALGRADIENT, 0}, not in 13.10 */
{"sbiterlimit", XPRS_SBITERLIMIT, 0},
{"sbbest", XPRS_SBBEST, 0},

#elif (XPRESS==14)

#define NUMPARAMS 109
static struct param_desc params[NUMPARAMS+NUMALIASES] = {
{"mpsrhsname", XPRS_MPSRHSNAME, 2},
{"mpsobjname", XPRS_MPSOBJNAME, 2},
{"mpsrangename", XPRS_MPSRANGENAME, 2},
{"mpsboundname", XPRS_MPSBOUNDNAME, 2},
{"outputmask", XPRS_OUTPUTMASK, 2},
{"omnidataname", XPRS_OMNIDATANAME, 2},
{"matrixtol", XPRS_MATRIXTOL, 1},
{"pivottol", XPRS_PIVOTTOL, 1},
{"feastol", XPRS_FEASTOL, 1},
{"outputtol", XPRS_OUTPUTTOL, 1},
{"sosreftol", XPRS_SOSREFTOL, 1},
{"optimalitytol", XPRS_OPTIMALITYTOL, 1},
{"etatol", XPRS_ETATOL, 1},
{"relpivottol", XPRS_RELPIVOTTOL, 1},
{"miptol", XPRS_MIPTOL, 1},
{"degradefactor", XPRS_DEGRADEFACTOR, 1},
{"miptarget", XPRS_MIPTARGET, 1},
{"mipaddcutoff", XPRS_MIPADDCUTOFF, 1},
{"mipabscutoff", XPRS_MIPABSCUTOFF, 1},
{"miprelcutoff", XPRS_MIPRELCUTOFF, 1},
{"pseudocost", XPRS_PSEUDOCOST, 1},
{"penalty", XPRS_PENALTY, 1},
{"bigm", XPRS_BIGM, 1},
{"mipabsstop", XPRS_MIPABSSTOP, 1},
{"miprelstop", XPRS_MIPRELSTOP, 1},
{"choleskytol", XPRS_CHOLESKYTOL, 1},
{"bargapstop", XPRS_BARGAPSTOP, 1},
{"bardualstop", XPRS_BARDUALSTOP, 1},
{"barprimalstop", XPRS_BARPRIMALSTOP, 1},
{"barstepstop", XPRS_BARSTEPSTOP, 1},
{"elimtol", XPRS_ELIMTOL, 1},
{"perturb", XPRS_PERTURB, 1},
{"markowitztol", XPRS_MARKOWITZTOL, 1},
{"ppfactor", XPRS_PPFACTOR, 1},
{"extrarows", XPRS_EXTRAROWS, 0},
{"extracols", XPRS_EXTRACOLS, 0},
{"extraelems", XPRS_EXTRAELEMS, 0},
{"lpiterlimit", XPRS_LPITERLIMIT, 0},
{"lplog", XPRS_LPLOG, 0},
{"scaling", XPRS_SCALING, 0},
{"presolve", XPRS_PRESOLVE, 0},
{"crash", XPRS_CRASH, 0},
{"pricingalg", XPRS_PRICINGALG, 0},
{"invertfreq", XPRS_INVERTFREQ, 0},
{"invertmin", XPRS_INVERTMIN, 0},
{"maxnode", XPRS_MAXNODE, 0},
{"maxtime", XPRS_MAXTIME, 0},
{"maxmipsol", XPRS_MAXMIPSOL, 0},
{"keepmipsol", XPRS_KEEPMIPSOL, 0},
{"defaultalg", XPRS_DEFAULTALG, 0},
{"varselection", XPRS_VARSELECTION, 0},
{"nodeselection", XPRS_NODESELECTION, 0},
{"backtrack", XPRS_BACKTRACK, 0},
{"miplog", XPRS_MIPLOG, 0},
{"mpserrignore", XPRS_MPSERRIGNORE, 0},
{"keepnrows", XPRS_KEEPNROWS, 0},
{"mpsecho", XPRS_MPSECHO, 0},
{"maxpagelines", XPRS_MAXPAGELINES, 0},
{"outputlog", XPRS_OUTPUTLOG, 0},
{"extrapresolve", XPRS_EXTRAPRESOLVE, 0},
{"cpmaxcuts", XPRS_CPMAXCUTS, 0},
{"cpmaxelems", XPRS_CPMAXELEMS, 0},
{"cpkeepallcuts", XPRS_CPKEEPALLCUTS, 0},
{"cachesize", XPRS_CACHESIZE, 0},
{"crossover", XPRS_CROSSOVER, 0},
{"bariterlimit", XPRS_BARITERLIMIT, 0},
{"choleskyalg", XPRS_CHOLESKYALG, 0},
{"baroutput", XPRS_BAROUTPUT, 0},
{"cstyle", XPRS_CSTYLE, 0},
{"extramipents", XPRS_EXTRAMIPENTS, 0},
{"refactor", XPRS_REFACTOR, 0},
{"barthreads", XPRS_BARTHREADS, 0},
{"keepbasis", XPRS_KEEPBASIS, 0},
{"omniformat", XPRS_OMNIFORMAT, 0},
{"version", XPRS_VERSION, 0},
{"bigmmethod", XPRS_BIGMMETHOD, 0},
{"rel10style", XPRS_REL10STYLE, 0},
{"mpsnamelength", XPRS_MPSNAMELENGTH, 0},
{"solutionfile", XPRS_SOLUTIONFILE, 0},
{"presolveops", XPRS_PRESOLVEOPS, 0},
{"mippresolve", XPRS_MIPPRESOLVE, 0},
{"maxslave", XPRS_MAXSLAVE, 0},
{"barorder", XPRS_BARORDER, 0},
{"breadthfirst", XPRS_BREADTHFIRST, 0},
{"autoperturb", XPRS_AUTOPERTURB, 0},
{"densecollimit", XPRS_DENSECOLLIMIT, 0},
{"cutfreq", XPRS_CUTFREQ, 0},
{"trace", XPRS_TRACE, 0},
{"maxiis", XPRS_MAXIIS, 0},
{"cputime", XPRS_CPUTIME, 0},
{"covercuts", XPRS_COVERCUTS, 0},
{"gomcuts", XPRS_GOMCUTS, 0},
{"mpsformat", XPRS_MPSFORMAT, 0},
{"cutstrategy", XPRS_CUTSTRATEGY, 0},
{"cutdepth", XPRS_CUTDEPTH, 0},
{"treecovercuts", XPRS_TREECOVERCUTS, 0},
{"treegomcuts", XPRS_TREEGOMCUTS, 0},
{"barmemory", XPRS_BARMEMORY, 0},
{"dualgradient", XPRS_DUALGRADIENT, 0},
{"sbiterlimit", XPRS_SBITERLIMIT, 0},
{"sbbest", XPRS_SBBEST, 0},
{"maxcuttime", XPRS_MAXCUTTIME, 0},
{"activeset", XPRS_ACTIVESET, 0},
{"barindeflimit", XPRS_BARINDEFLIMIT, 0},
{"heurstrategy", XPRS_HEURSTRATEGY, 0},
{"heurfreq", XPRS_HEURFREQ, 0},
{"heurdepth", XPRS_HEURDEPTH, 0},
{"heurmaxsol", XPRS_HEURMAXSOL, 0},
{"heurnodes", XPRS_HEURNODES, 0},

#elif XPRESS==15

#define NUMPARAMS 114
static struct param_desc params[NUMPARAMS+NUMALIASES] = {
{"mpsrhsname", XPRS_MPSRHSNAME, 2},
{"mpsobjname", XPRS_MPSOBJNAME, 2},
{"mpsrangename", XPRS_MPSRANGENAME, 2},
{"mpsboundname", XPRS_MPSBOUNDNAME, 2},
{"outputmask", XPRS_OUTPUTMASK, 2},
{"omnidataname", XPRS_OMNIDATANAME, 2},
{"matrixtol", XPRS_MATRIXTOL, 1},
{"pivottol", XPRS_PIVOTTOL, 1},
{"feastol", XPRS_FEASTOL, 1},
{"outputtol", XPRS_OUTPUTTOL, 1},
{"sosreftol", XPRS_SOSREFTOL, 1},
{"optimalitytol", XPRS_OPTIMALITYTOL, 1},
{"etatol", XPRS_ETATOL, 1},
{"relpivottol", XPRS_RELPIVOTTOL, 1},
{"miptol", XPRS_MIPTOL, 1},
{"degradefactor", XPRS_DEGRADEFACTOR, 1},
{"miptarget", XPRS_MIPTARGET, 1},
{"mipaddcutoff", XPRS_MIPADDCUTOFF, 1},
{"mipabscutoff", XPRS_MIPABSCUTOFF, 1},
{"miprelcutoff", XPRS_MIPRELCUTOFF, 1},
{"pseudocost", XPRS_PSEUDOCOST, 1},
{"penalty", XPRS_PENALTY, 1},
{"bigm", XPRS_BIGM, 1},
{"mipabsstop", XPRS_MIPABSSTOP, 1},
{"miprelstop", XPRS_MIPRELSTOP, 1},
{"choleskytol", XPRS_CHOLESKYTOL, 1},
{"bargapstop", XPRS_BARGAPSTOP, 1},
{"bardualstop", XPRS_BARDUALSTOP, 1},
{"barprimalstop", XPRS_BARPRIMALSTOP, 1},
{"barstepstop", XPRS_BARSTEPSTOP, 1},
{"elimtol", XPRS_ELIMTOL, 1},
{"perturb", XPRS_PERTURB, 1},
{"markowitztol", XPRS_MARKOWITZTOL, 1},
{"ppfactor", XPRS_PPFACTOR, 1},
{"extrarows", XPRS_EXTRAROWS, 0},
{"extracols", XPRS_EXTRACOLS, 0},
{"extraelems", XPRS_EXTRAELEMS, 0},
{"lpiterlimit", XPRS_LPITERLIMIT, 0},
{"lplog", XPRS_LPLOG, 0},
{"scaling", XPRS_SCALING, 0},
{"presolve", XPRS_PRESOLVE, 0},
{"crash", XPRS_CRASH, 0},
{"pricingalg", XPRS_PRICINGALG, 0},
{"invertfreq", XPRS_INVERTFREQ, 0},
{"invertmin", XPRS_INVERTMIN, 0},
{"maxnode", XPRS_MAXNODE, 0},
{"maxtime", XPRS_MAXTIME, 0},
{"maxmipsol", XPRS_MAXMIPSOL, 0},
{"keepmipsol", XPRS_KEEPMIPSOL, 0},
{"defaultalg", XPRS_DEFAULTALG, 0},
{"varselection", XPRS_VARSELECTION, 0},
{"nodeselection", XPRS_NODESELECTION, 0},
{"backtrack", XPRS_BACKTRACK, 0},
{"miplog", XPRS_MIPLOG, 0},
{"mpserrignore", XPRS_MPSERRIGNORE, 0},
{"keepnrows", XPRS_KEEPNROWS, 0},
{"mpsecho", XPRS_MPSECHO, 0},
{"maxpagelines", XPRS_MAXPAGELINES, 0},
{"outputlog", XPRS_OUTPUTLOG, 0},
{"extrapresolve", XPRS_EXTRAPRESOLVE, 0},
{"cpmaxcuts", XPRS_CPMAXCUTS, 0},
{"cpmaxelems", XPRS_CPMAXELEMS, 0},
{"cpkeepallcuts", XPRS_CPKEEPALLCUTS, 0},
{"cachesize", XPRS_CACHESIZE, 0},
{"crossover", XPRS_CROSSOVER, 0},
{"bariterlimit", XPRS_BARITERLIMIT, 0},
{"choleskyalg", XPRS_CHOLESKYALG, 0},
{"baroutput", XPRS_BAROUTPUT, 0},
{"cstyle", XPRS_CSTYLE, 0},
{"extramipents", XPRS_EXTRAMIPENTS, 0},
{"refactor", XPRS_REFACTOR, 0},
{"barthreads", XPRS_BARTHREADS, 0},
{"keepbasis", XPRS_KEEPBASIS, 0},
{"omniformat", XPRS_OMNIFORMAT, 0},
{"version", XPRS_VERSION, 0},
{"bigmmethod", XPRS_BIGMMETHOD, 0},
{"rel10style", XPRS_REL10STYLE, 0},
{"mpsnamelength", XPRS_MPSNAMELENGTH, 0},
{"solutionfile", XPRS_SOLUTIONFILE, 0},
{"presolveops", XPRS_PRESOLVEOPS, 0},
{"mippresolve", XPRS_MIPPRESOLVE, 0},
{"maxslave", XPRS_MAXSLAVE, 0},
{"barorder", XPRS_BARORDER, 0},
{"breadthfirst", XPRS_BREADTHFIRST, 0},
{"autoperturb", XPRS_AUTOPERTURB, 0},
{"densecollimit", XPRS_DENSECOLLIMIT, 0},
{"cutfreq", XPRS_CUTFREQ, 0},
{"trace", XPRS_TRACE, 0},
{"maxiis", XPRS_MAXIIS, 0},
{"cputime", XPRS_CPUTIME, 0},
{"covercuts", XPRS_COVERCUTS, 0},
{"gomcuts", XPRS_GOMCUTS, 0},
{"mpsformat", XPRS_MPSFORMAT, 0},
{"cutstrategy", XPRS_CUTSTRATEGY, 0},
{"cutdepth", XPRS_CUTDEPTH, 0},
{"treecovercuts", XPRS_TREECOVERCUTS, 0},
{"treegomcuts", XPRS_TREEGOMCUTS, 0},
{"barmemory", XPRS_BARMEMORY, 0},
{"dualgradient", XPRS_DUALGRADIENT, 0},
{"sbiterlimit", XPRS_SBITERLIMIT, 0},
{"sbbest", XPRS_SBBEST, 0},
{"maxcuttime", XPRS_MAXCUTTIME, 0},
{"activeset", XPRS_ACTIVESET, 0},
{"barindeflimit", XPRS_BARINDEFLIMIT, 0},
{"heurstrategy", XPRS_HEURSTRATEGY, 0},
{"heurfreq", XPRS_HEURFREQ, 0},
{"heurdepth", XPRS_HEURDEPTH, 0},
{"heurmaxsol", XPRS_HEURMAXSOL, 0},
{"heurnodes", XPRS_HEURNODES, 0},
{"lnpbest", XPRS_LNPBEST, 0},
{"lnpiterlimit", XPRS_LNPITERLIMIT, 0},
{"branchchoice", XPRS_BRANCHCHOICE, 0},
{"sbselect", XPRS_SBSELECT, 0},
{"sbthreads", XPRS_SBTHREADS, 0},

#elif XPRESS==16

#define NUMPARAMS 115
static struct param_desc params[NUMPARAMS+NUMALIASES] = {
{"mpsrhsname", XPRS_MPSRHSNAME, 2},
{"mpsobjname", XPRS_MPSOBJNAME, 2},
{"mpsrangename", XPRS_MPSRANGENAME, 2},
{"mpsboundname", XPRS_MPSBOUNDNAME, 2},
{"outputmask", XPRS_OUTPUTMASK, 2},
{"omnidataname", XPRS_OMNIDATANAME, 2},
{"matrixtol", XPRS_MATRIXTOL, 1},
{"pivottol", XPRS_PIVOTTOL, 1},
{"feastol", XPRS_FEASTOL, 1},
{"outputtol", XPRS_OUTPUTTOL, 1},
{"sosreftol", XPRS_SOSREFTOL, 1},
{"optimalitytol", XPRS_OPTIMALITYTOL, 1},
{"etatol", XPRS_ETATOL, 1},
{"relpivottol", XPRS_RELPIVOTTOL, 1},
{"miptol", XPRS_MIPTOL, 1},
{"degradefactor", XPRS_DEGRADEFACTOR, 1},
{"miptarget", XPRS_MIPTARGET, 1},
{"mipaddcutoff", XPRS_MIPADDCUTOFF, 1},
{"mipabscutoff", XPRS_MIPABSCUTOFF, 1},
{"miprelcutoff", XPRS_MIPRELCUTOFF, 1},
{"pseudocost", XPRS_PSEUDOCOST, 1},
{"penalty", XPRS_PENALTY, 1},
{"bigm", XPRS_BIGM, 1},
{"mipabsstop", XPRS_MIPABSSTOP, 1},
{"miprelstop", XPRS_MIPRELSTOP, 1},
{"choleskytol", XPRS_CHOLESKYTOL, 1},
{"bargapstop", XPRS_BARGAPSTOP, 1},
{"bardualstop", XPRS_BARDUALSTOP, 1},
{"barprimalstop", XPRS_BARPRIMALSTOP, 1},
{"barstepstop", XPRS_BARSTEPSTOP, 1},
{"elimtol", XPRS_ELIMTOL, 1},
{"perturb", XPRS_PERTURB, 1},
{"markowitztol", XPRS_MARKOWITZTOL, 1},
{"ppfactor", XPRS_PPFACTOR, 1},
{"extrarows", XPRS_EXTRAROWS, 0},
{"extracols", XPRS_EXTRACOLS, 0},
{"extraelems", XPRS_EXTRAELEMS, 0},
{"lpiterlimit", XPRS_LPITERLIMIT, 0},
{"lplog", XPRS_LPLOG, 0},
{"scaling", XPRS_SCALING, 0},
{"presolve", XPRS_PRESOLVE, 0},
{"crash", XPRS_CRASH, 0},
{"pricingalg", XPRS_PRICINGALG, 0},
{"invertfreq", XPRS_INVERTFREQ, 0},
{"invertmin", XPRS_INVERTMIN, 0},
{"maxnode", XPRS_MAXNODE, 0},
{"maxtime", XPRS_MAXTIME, 0},
{"maxmipsol", XPRS_MAXMIPSOL, 0},
{"keepmipsol", XPRS_KEEPMIPSOL, 0},
{"defaultalg", XPRS_DEFAULTALG, 0},
{"varselection", XPRS_VARSELECTION, 0},
{"nodeselection", XPRS_NODESELECTION, 0},
{"backtrack", XPRS_BACKTRACK, 0},
{"miplog", XPRS_MIPLOG, 0},
{"mpserrignore", XPRS_MPSERRIGNORE, 0},
{"keepnrows", XPRS_KEEPNROWS, 0},
{"mpsecho", XPRS_MPSECHO, 0},
{"maxpagelines", XPRS_MAXPAGELINES, 0},
{"outputlog", XPRS_OUTPUTLOG, 0},
{"extrapresolve", XPRS_EXTRAPRESOLVE, 0},
{"cpmaxcuts", XPRS_CPMAXCUTS, 0},
{"cpmaxelems", XPRS_CPMAXELEMS, 0},
{"cpkeepallcuts", XPRS_CPKEEPALLCUTS, 0},
{"cachesize", XPRS_CACHESIZE, 0},
{"crossover", XPRS_CROSSOVER, 0},
{"bariterlimit", XPRS_BARITERLIMIT, 0},
{"choleskyalg", XPRS_CHOLESKYALG, 0},
{"baroutput", XPRS_BAROUTPUT, 0},
{"cstyle", XPRS_CSTYLE, 0},
{"extramipents", XPRS_EXTRAMIPENTS, 0},
{"refactor", XPRS_REFACTOR, 0},
{"barthreads", XPRS_BARTHREADS, 0},
{"keepbasis", XPRS_KEEPBASIS, 0},
{"omniformat", XPRS_OMNIFORMAT, 0},
{"version", XPRS_VERSION, 0},
{"bigmmethod", XPRS_BIGMMETHOD, 0},
{"rel10style", XPRS_REL10STYLE, 0},
{"mpsnamelength", XPRS_MPSNAMELENGTH, 0},
{"solutionfile", XPRS_SOLUTIONFILE, 0},
{"presolveops", XPRS_PRESOLVEOPS, 0},
{"mippresolve", XPRS_MIPPRESOLVE, 0},
{"mipthreads", XPRS_MIPTHREADS, 0},
{"barorder", XPRS_BARORDER, 0},
{"breadthfirst", XPRS_BREADTHFIRST, 0},
{"autoperturb", XPRS_AUTOPERTURB, 0},
{"densecollimit", XPRS_DENSECOLLIMIT, 0},
{"cutfreq", XPRS_CUTFREQ, 0},
{"trace", XPRS_TRACE, 0},
{"maxiis", XPRS_MAXIIS, 0},
{"cputime", XPRS_CPUTIME, 0},
{"covercuts", XPRS_COVERCUTS, 0},
{"gomcuts", XPRS_GOMCUTS, 0},
{"mpsformat", XPRS_MPSFORMAT, 0},
{"cutstrategy", XPRS_CUTSTRATEGY, 0},
{"cutdepth", XPRS_CUTDEPTH, 0},
{"treecovercuts", XPRS_TREECOVERCUTS, 0},
{"treegomcuts", XPRS_TREEGOMCUTS, 0},
{"dualgradient", XPRS_DUALGRADIENT, 0},
{"sbiterlimit", XPRS_SBITERLIMIT, 0},
{"sbbest", XPRS_SBBEST, 0},
{"maxcuttime", XPRS_MAXCUTTIME, 0},
{"activeset", XPRS_ACTIVESET, 0},
{"barindeflimit", XPRS_BARINDEFLIMIT, 0},
{"heurstrategy", XPRS_HEURSTRATEGY, 0},
{"heurfreq", XPRS_HEURFREQ, 0},
{"heurdepth", XPRS_HEURDEPTH, 0},
{"heurmaxsol", XPRS_HEURMAXSOL, 0},
{"heurnodes", XPRS_HEURNODES, 0},
{"lnpbest", XPRS_LNPBEST, 0},
{"lnpiterlimit", XPRS_LNPITERLIMIT, 0},
{"branchchoice", XPRS_BRANCHCHOICE, 0},
{"sbselect", XPRS_SBSELECT, 0},
{"sbthreads", XPRS_SBTHREADS, 0},
{"heurdivestrategy", XPRS_HEURDIVESTRATEGY, 0},
{"heurselect", XPRS_HEURSELECT, 0},

#elif XPRESS==20

#define NUMPARAMS 164
static struct param_desc params[NUMPARAMS+NUMALIASES] = {
{"mpsrhsname", XPRS_MPSRHSNAME, 2},
{"mpsobjname", XPRS_MPSOBJNAME, 2},
{"mpsrangename", XPRS_MPSRANGENAME, 2},
{"mpsboundname", XPRS_MPSBOUNDNAME, 2},
{"outputmask", XPRS_OUTPUTMASK, 2},
{"matrixtol", XPRS_MATRIXTOL, 1},
{"pivottol", XPRS_PIVOTTOL, 1},
{"feastol", XPRS_FEASTOL, 1},
{"outputtol", XPRS_OUTPUTTOL, 1},
{"sosreftol", XPRS_SOSREFTOL, 1},
{"optimalitytol", XPRS_OPTIMALITYTOL, 1},
{"etatol", XPRS_ETATOL, 1},
{"relpivottol", XPRS_RELPIVOTTOL, 1},
{"miptol", XPRS_MIPTOL, 1},
{"degradefactor", XPRS_DEGRADEFACTOR, 1},
{"miptarget", XPRS_MIPTARGET, 1},
{"mipaddcutoff", XPRS_MIPADDCUTOFF, 1},
{"mipabscutoff", XPRS_MIPABSCUTOFF, 1},
{"miprelcutoff", XPRS_MIPRELCUTOFF, 1},
{"pseudocost", XPRS_PSEUDOCOST, 1},
{"penalty", XPRS_PENALTY, 1},
{"bigm", XPRS_BIGM, 1},
{"mipabsstop", XPRS_MIPABSSTOP, 1},
{"miprelstop", XPRS_MIPRELSTOP, 1},
{"choleskytol", XPRS_CHOLESKYTOL, 1},
{"bargapstop", XPRS_BARGAPSTOP, 1},
{"bardualstop", XPRS_BARDUALSTOP, 1},
{"barprimalstop", XPRS_BARPRIMALSTOP, 1},
{"barstepstop", XPRS_BARSTEPSTOP, 1},
{"elimtol", XPRS_ELIMTOL, 1},
{"perturb", XPRS_PERTURB, 1},
{"markowitztol", XPRS_MARKOWITZTOL, 1},
{"ppfactor", XPRS_PPFACTOR, 1},
{"sbeffort", XPRS_SBEFFORT, 1},
{"heurdiverandomize", XPRS_HEURDIVERANDOMIZE, 1},
{"heursearcheffort", XPRS_HEURSEARCHEFFORT, 1},
{"cutfactor", XPRS_CUTFACTOR, 1},
{"eigenvaluetol", XPRS_EIGENVALUETOL, 1},
{"indlinbigm", XPRS_INDLINBIGM, 1},
{"treememorysavingtarget", XPRS_TREEMEMORYSAVINGTARGET, 1},
{"globalfilebias", XPRS_GLOBALFILEBIAS, 1},
{"extrarows", XPRS_EXTRAROWS, 0},
{"extracols", XPRS_EXTRACOLS, 0},
{"extraelems", XPRS_EXTRAELEMS, 0},
{"lpiterlimit", XPRS_LPITERLIMIT, 0},
{"lplog", XPRS_LPLOG, 0},
{"scaling", XPRS_SCALING, 0},
{"presolve", XPRS_PRESOLVE, 0},
{"crash", XPRS_CRASH, 0},
{"pricingalg", XPRS_PRICINGALG, 0},
{"invertfreq", XPRS_INVERTFREQ, 0},
{"invertmin", XPRS_INVERTMIN, 0},
{"maxnode", XPRS_MAXNODE, 0},
{"maxtime", XPRS_MAXTIME, 0},
{"maxmipsol", XPRS_MAXMIPSOL, 0},
{"keepmipsol", XPRS_KEEPMIPSOL, 0},
{"defaultalg", XPRS_DEFAULTALG, 0},
{"varselection", XPRS_VARSELECTION, 0},
{"nodeselection", XPRS_NODESELECTION, 0},
{"backtrack", XPRS_BACKTRACK, 0},
{"miplog", XPRS_MIPLOG, 0},
{"keepnrows", XPRS_KEEPNROWS, 0},
{"mpsecho", XPRS_MPSECHO, 0},
{"maxpagelines", XPRS_MAXPAGELINES, 0},
{"outputlog", XPRS_OUTPUTLOG, 0},
{"extrapresolve", XPRS_EXTRAPRESOLVE, 0},
{"cachesize", XPRS_CACHESIZE, 0},
{"crossover", XPRS_CROSSOVER, 0},
{"bariterlimit", XPRS_BARITERLIMIT, 0},
{"choleskyalg", XPRS_CHOLESKYALG, 0},
{"baroutput", XPRS_BAROUTPUT, 0},
{"cstyle", XPRS_CSTYLE, 0},
{"extramipents", XPRS_EXTRAMIPENTS, 0},
{"refactor", XPRS_REFACTOR, 0},
{"barthreads", XPRS_BARTHREADS, 0},
{"keepbasis", XPRS_KEEPBASIS, 0},
{"version", XPRS_VERSION, 0},
{"bigmmethod", XPRS_BIGMMETHOD, 0},
{"mpsnamelength", XPRS_MPSNAMELENGTH, 0},
{"solutionfile", XPRS_SOLUTIONFILE, 0},
{"presolveops", XPRS_PRESOLVEOPS, 0},
{"mippresolve", XPRS_MIPPRESOLVE, 0},
{"mipthreads", XPRS_MIPTHREADS, 0},
{"barorder", XPRS_BARORDER, 0},
{"breadthfirst", XPRS_BREADTHFIRST, 0},
{"autoperturb", XPRS_AUTOPERTURB, 0},
{"densecollimit", XPRS_DENSECOLLIMIT, 0},
{"cutfreq", XPRS_CUTFREQ, 0},
{"trace", XPRS_TRACE, 0},
{"maxiis", XPRS_MAXIIS, 0},
{"cputime", XPRS_CPUTIME, 0},
{"covercuts", XPRS_COVERCUTS, 0},
{"gomcuts", XPRS_GOMCUTS, 0},
{"mpsformat", XPRS_MPSFORMAT, 0},
{"cutstrategy", XPRS_CUTSTRATEGY, 0},
{"cutdepth", XPRS_CUTDEPTH, 0},
{"treecovercuts", XPRS_TREECOVERCUTS, 0},
{"treegomcuts", XPRS_TREEGOMCUTS, 0},
{"cutselect", XPRS_CUTSELECT, 0},
{"treecutselect", XPRS_TREECUTSELECT, 0},
{"dualize", XPRS_DUALIZE, 0},
{"dualgradient", XPRS_DUALGRADIENT, 0},
{"sbiterlimit", XPRS_SBITERLIMIT, 0},
{"sbbest", XPRS_SBBEST, 0},
{"maxcuttime", XPRS_MAXCUTTIME, 0},
{"activeset", XPRS_ACTIVESET, 0},
{"barindeflimit", XPRS_BARINDEFLIMIT, 0},
{"heurstrategy", XPRS_HEURSTRATEGY, 0},
{"heurfreq", XPRS_HEURFREQ, 0},
{"heurdepth", XPRS_HEURDEPTH, 0},
{"heurmaxsol", XPRS_HEURMAXSOL, 0},
{"heurnodes", XPRS_HEURNODES, 0},
{"lnpbest", XPRS_LNPBEST, 0},
{"lnpiterlimit", XPRS_LNPITERLIMIT, 0},
{"branchchoice", XPRS_BRANCHCHOICE, 0},
{"sbselect", XPRS_SBSELECT, 0},
{"localchoice", XPRS_LOCALCHOICE, 0},
{"localbacktrack", XPRS_LOCALBACKTRACK, 0},
{"dualstrategy", XPRS_DUALSTRATEGY, 0},
{"l1cache", XPRS_L1CACHE, 0},
{"heurdivestrategy", XPRS_HEURDIVESTRATEGY, 0},
{"heurselect", XPRS_HEURSELECT, 0},
{"extrasets", XPRS_EXTRASETS, 0},
{"extrasetelems", XPRS_EXTRASETELEMS, 0},
{"feasibilitypump", XPRS_FEASIBILITYPUMP, 0},
{"precoefelim", XPRS_PRECOEFELIM, 0},
{"predomcol", XPRS_PREDOMCOL, 0},
{"heursearchfreq", XPRS_HEURSEARCHFREQ, 0},
{"heurdivespeedup", XPRS_HEURDIVESPEEDUP, 0},
{"sbestimate", XPRS_SBESTIMATE, 0},
{"historycosts", XPRS_HISTORYCOSTS, 0},
{"algaftercrossover", XPRS_ALGAFTERCROSSOVER, 0},
{"linelength", XPRS_LINELENGTH, 0},
{"mutexcallbacks", XPRS_MUTEXCALLBACKS, 0},
{"barcrash", XPRS_BARCRASH, 0},
{"heursearchrootselect", XPRS_HEURSEARCHROOTSELECT, 0},
{"heursearchtreeselect", XPRS_HEURSEARCHTREESELECT, 0},
{"mps18compatible", XPRS_MPS18COMPATIBLE, 0},
{"rootpresolve", XPRS_ROOTPRESOLVE, 0},
{"crossoverdrp", XPRS_CROSSOVERDRP, 0},
{"forceoutput", XPRS_FORCEOUTPUT, 0},
{"deterministic", XPRS_DETERMINISTIC, 0},
{"preprobing", XPRS_PREPROBING, 0},
{"extraqcelements", XPRS_EXTRAQCELEMENTS, 0},
{"extraqcrows", XPRS_EXTRAQCROWS, 0},
{"treememorylimit", XPRS_TREEMEMORYLIMIT, 0},
{"treecompression", XPRS_TREECOMPRESSION, 0},
{"treediagnostics", XPRS_TREEDIAGNOSTICS, 0},
{"maxglobalfilesize", XPRS_MAXGLOBALFILESIZE, 0},
{"tempbounds", XPRS_TEMPBOUNDS, 0},
{"ifcheckconvexity", XPRS_IFCHECKCONVEXITY, 0},
{"primalunshift", XPRS_PRIMALUNSHIFT, 0},
{"repairindefiniteq", XPRS_REPAIRINDEFINITEQ, 0},
{"maxlocalbacktrack", XPRS_MAXLOCALBACKTRACK, 0},
{"backtracktie", XPRS_BACKTRACKTIE, 0},
{"branchdisj", XPRS_BRANCHDISJ, 0},
{"lpthreads", XPRS_LPTHREADS, 0},
{"maxscalefactor", XPRS_MAXSCALEFACTOR, 0},
{"heurthreads", XPRS_HEURTHREADS, 0},
{"threads", XPRS_THREADS, 0},
{"predomrow", XPRS_PREDOMROW, 0},
{"branchstructural", XPRS_BRANCHSTRUCTURAL, 0},
{"quadraticunshift", XPRS_QUADRATICUNSHIFT, 0},
{"barpresolveops", XPRS_BARPRESOLVEOPS, 0},

#endif


/*
 * Add some version-independent aliases to the table
 * This must remain at the end of the table!!!
 * If you add lines here, update NUMALIASES above!
 * NUMALIASES lines follow
 */

{"timelimit", XPRS_MAXTIME, 0},
{"time_limit", XPRS_MAXTIME, 0},
{"feasibility_tol", XPRS_FEASTOL, 1},
{"integrality", XPRS_MIPTOL, 1},
{"objdifference", XPRS_MIPADDCUTOFF, 1},
{"iteration_limit", XPRS_LPITERLIMIT, 0},
{"presolve", XPRS_PRESOLVE, 0},
{"crash", XPRS_CRASH, 0},
{"refactor", XPRS_INVERTFREQ, 0},
{"node_limit", XPRS_MAXNODE, 0},
{"scrind", XPRS_OUTPUTLOG, 0},
{"logfile", 0, 3},		/* special */
{"subalgorithm", XPRS_DEFAULTALG, 0},

};

#endif /*XPRESS*/
