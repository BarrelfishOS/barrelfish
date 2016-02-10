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
 * The Original Code is  The ECLiPSe/Gurobi Interface
 * The Initial Developer of the Original Code is Joachim Schimpf
 * Portions created by the Initial Developer are
 * Copyright (C) 2012 Joachim Schimpf
 * 
 * Contributor(s): Joachim Schimpf, Coninfer Ltd
 * 
 * END LICENSE BLOCK */

/*
 * ECLiPSe/Gurobi interface (for inclusion in eplex.c)
 */

#ifdef GUROBI

/*
 * Notes on Gurobi
 *
 * Parameters
 *	Parameters are part of the "environment". When a model is created,
 *	it gets a copy of the specified environment.
 * Strangely uses strings for attribute/param identifiers
 * Methods
 *	auto
 *	primal
 *	dual
 *	barrier
 *	barrier(auto|none|1|2|3|4)
 *	concurrent
 *	concurrent_det
 *	sift(auto|primal|dual|barrier)
 * Node_methods
 *	primal
 *	dual
 *	barrier
 * Need to "flush" model modifications with GRBupdatemodel()
 *	between adding columns and rows with these columns,
 *	and on many other occasions after the model changed.
 * No notion of different message streams (log,warning,error,result)
 *	we send everything to Log
 *
 * Licence
 *	license file gurobi.lic in HOME, C:\gurobiVV or /opt/gurobiVV
 *	alternatively GRB_LICENSE_FILE contains path name of license file
 */

static int
grb_getintattr(GRBmodel* lp, const char* attr)
{
    int value = 0;
    GRBgetintattr(lp, attr, &value);
    return value;
}


static int
grb_chgbds(GRBmodel* lp, int cnt, int* idxs, char* lu, double* bd)
{
    int err, i, nvars;
    if (GRBgetintattr(lp, GRB_INT_ATTR_NUMVARS, &nvars))
	goto _return_err_;
    for (i=0; i<cnt; i++)
    {
	int j=idxs[i];
	if (j < 0 || j >= nvars) return -1;
	switch(lu[i])
	{
	    case 'L': err = GRBsetdblattrelement(lp,GRB_DBL_ATTR_LB,j,bd[i]);
	    	break;
	    case 'B': err = GRBsetdblattrelement(lp,GRB_DBL_ATTR_LB,j,bd[i]);
	    	/*fall through*/
	    case 'U': err = GRBsetdblattrelement(lp,GRB_DBL_ATTR_UB,j,bd[i]);
	    	break;
	    default:
		return -1;
	}
	if (err) goto _return_err_;
    }
    return 0;
_return_err_:
    Report_Solver_Error(cpx_env);
    return -1;
}


/* caution: this function assumes that the arrays are the right size */
static int
grb_loadbasis(GRBmodel* lp, int* cbase, int* rbase)
{
    int nvars, ncstr;
    if (GRBgetintattr(lp, GRB_INT_ATTR_NUMVARS, &nvars))
	goto _return_err_;
    if (GRBgetintattr(lp, GRB_INT_ATTR_NUMCONSTRS, &ncstr))
	goto _return_err_;
    if (GRBsetintattrarray(lp,GRB_INT_ATTR_CBASIS,0,ncstr,cbase))
	goto _return_err_;
    if (GRBsetintattrarray(lp,GRB_INT_ATTR_VBASIS,0,nvars,rbase))
	goto _return_err_;
    return 0;
_return_err_:
    Report_Solver_Error(cpx_env);
    return -1;
}


static int
cpx_loadstart(lp_desc* lpd, int option, double *sols, int sz)
{
    int nvars, i;

    if (GRBgetintattr(lpd->lp, GRB_INT_ATTR_NUMVARS, &nvars))
	goto _return_err_;
    if (sz == 0) option = MIPSTART_NONE;
    if (sz > nvars) sz = nvars;

    switch(option)
    {
    case MIPSTART_NONE:
	if (lpd->mipstart_dirty)
	{
	    for (i=0; i<nvars; ++i)
	    {
		if (GRBsetdblattrelement(lpd->lp,GRB_DBL_ATTR_START,i,GRB_UNDEFINED))
		    goto _return_err_;
	    }
	    lpd->mipstart_dirty = 0;
	}
	break;

    case MIPSTART_ALL:
	if (GRBsetdblattrarray(lpd->lp,GRB_DBL_ATTR_START,0,sz,sols))
	    goto _return_err_;
	for (i=sz; i<nvars; ++i)
	{
	    if (GRBsetdblattrelement(lpd->lp,GRB_DBL_ATTR_START,i,GRB_UNDEFINED))
		goto _return_err_;
	}
	lpd->mipstart_dirty = 1;
	break;

    case MIPSTART_INT:
	{
	    char *vtype = (char*) Malloc(nvars*sizeof(char));
	    if (GRBgetcharattrarray(lpd->lp,GRB_CHAR_ATTR_VTYPE,0,nvars,vtype))
	    {
		Free(vtype);
		goto _return_err_;
	    }
	    for (i=0; i<sz; ++i)
	    {
		if (GRBsetdblattrelement(lpd->lp,GRB_DBL_ATTR_START,i,
			vtype[i]=='C' ? GRB_UNDEFINED : sols[i]))
		{
		    Free(vtype);
		    goto _return_err_;
		}
	    }
	    Free(vtype);
	    for (i=sz; i<nvars; ++i)
	    {
		if (GRBsetdblattrelement(lpd->lp,GRB_DBL_ATTR_START,i,GRB_UNDEFINED))
		    goto _return_err_;
	    }
	    lpd->mipstart_dirty = 1;
	}
	break;
    }
    return 0;

_return_err_:
    Report_Solver_Error(cpx_env);
    return -1;
}


/* caution: this function assumes that the arrays are the right size */
static int
grb_getbasis(GRBmodel* lp, int* cbase, int* rbase)
{
    int nvars, ncstr;
    if (GRBgetintattr(lp, GRB_INT_ATTR_NUMVARS, &nvars))
	goto _return_err_;
    if (GRBgetintattr(lp, GRB_INT_ATTR_NUMCONSTRS, &ncstr))
	goto _return_err_;
    if (GRBgetintattrarray(lp,GRB_INT_ATTR_CBASIS,0,ncstr,cbase))
	goto _return_err_;
    if (GRBgetintattrarray(lp,GRB_INT_ATTR_VBASIS,0,nvars,rbase))
	goto _return_err_;
    return 0;
_return_err_:
    Report_Solver_Error(cpx_env);
    return -1;
}


static int
grb_setname(GRBmodel* lp, char ntype, int idx, char * name)
{
    switch(ntype)
    {
    case 'c':
	if (GRBsetstrattrelement(lp,GRB_STR_ATTR_VARNAME,idx,name))
	    goto _return_err_;
	break;
    case 'r':
	if (GRBsetstrattrelement(lp,GRB_STR_ATTR_CONSTRNAME,idx,name))
	    goto _return_err_;
	break;
    default:
    	return -1;
    }
    return 0;
_return_err_:
    Report_Solver_Error(cpx_env);
    return -1;
}


/*
 * Set parameters for methods and timeout
 */
static int
cpx_prepare_solve(lp_desc* lpd, struct lp_meth *meth, double timeout)
{
    int method, val, min, max;
    GRBenv* grb_env = GRBgetenv(lpd->lp);

    if (timeout <= 0.0)
    	timeout = HUGE_VAL;
    if (GRBsetdblparam(grb_env, GRB_DBL_PAR_TIMELIMIT, timeout))
	goto _return_err_;

    if (meth->meth != METHOD_DEFAULT)
    {
	switch(meth->meth)
	{
	    case METHOD_AUTO:	method = GRB_METHOD_AUTO; break;
	    case METHOD_PRIMAL:	method = GRB_METHOD_PRIMAL; break;
	    case METHOD_DUAL:	method = GRB_METHOD_DUAL; break;
	    case METHOD_CONCURRENT:	method = GRB_METHOD_CONCURRENT; break;
	    case METHOD_CONCURRENTDET:	method = GRB_METHOD_DETERMINISTIC_CONCURRENT; break;
	    case METHOD_BAR:
		if (meth->auxmeth != METHOD_DEFAULT)
		{
		    switch(meth->auxmeth)
		    {
			case METHOD_AUTO:	method = -1; break;
			case METHOD_NONE:	method = 0; break;
			case METHOD_PRIMAL:	method = 3; break;
			case METHOD_DUAL:	method = 2; break;
			default:		goto _illegal_method_;
		    }
		    if (GRBsetintparam(grb_env, GRB_INT_PAR_CROSSOVER, method))
			goto _return_err_;
		}
		method = GRB_METHOD_BARRIER;
		break;
	    case METHOD_SIFT:
		if (meth->auxmeth != METHOD_DEFAULT)
		{
		    switch(meth->auxmeth)
		    {
			case METHOD_AUTO:	method = -1; break;
			case METHOD_PRIMAL:	method = 0; break;
			case METHOD_DUAL:	method = 1; break;
			case METHOD_BAR:	method = 2; break;
			default:		goto _illegal_method_;
		    }
		    if (GRBsetintparam(grb_env, GRB_INT_PAR_SIFTMETHOD, method))
			goto _return_err_;
		}
		/* We leave the SIFTING parameter untouched! */
		method = GRB_METHOD_DUAL;
		break;
	    default:	goto _illegal_method_;
	}
	if (GRBsetintparam(grb_env, GRB_INT_PAR_METHOD, method))
	    goto _return_err_;
    }

    if (meth->node_meth != METHOD_DEFAULT)
    {
	switch(meth->node_meth)
	{
	    case METHOD_PRIMAL:	method = GRB_METHOD_PRIMAL; break;
	    case METHOD_DUAL:	method = GRB_METHOD_DUAL; break;
	    case METHOD_BAR:
		if (meth->meth != METHOD_BAR && meth->node_auxmeth != METHOD_DEFAULT)
		{
		    switch(meth->node_auxmeth)
		    {
			case METHOD_AUTO:	method = -1; break;
			case METHOD_NONE:	method = 0; break;
			case METHOD_PRIMAL:	method = 3; break;
			case METHOD_DUAL:	method = 2; break;
			default:		goto _illegal_method_;
		    }
		    if (GRBsetintparam(grb_env, GRB_INT_PAR_CROSSOVER, method))
			goto _return_err_;
		}
		method = GRB_METHOD_BARRIER;
		break;
	    case METHOD_SIFT:
		if (meth->meth != METHOD_SIFT && meth->node_auxmeth != METHOD_DEFAULT)
		{
		    switch(meth->node_auxmeth)
		    {
			case METHOD_AUTO:	method = -1; break;
			case METHOD_PRIMAL:	method = 0; break;
			case METHOD_DUAL:	method = 1; break;
			case METHOD_BAR:	method = 2; break;
			default:		goto _illegal_method_;
		    }
		    if (GRBsetintparam(grb_env, GRB_INT_PAR_SIFTMETHOD, method))
			goto _return_err_;
		}
		/* We leave the SIFTING parameter untouched! */
		method = GRB_METHOD_DUAL;
		break;
	    default:	goto _illegal_method_;
	}
	if (GRBsetintparam(grb_env, GRB_INT_PAR_NODEMETHOD, method))
	    goto _return_err_;
    }

    return 0;

_illegal_method_:
    Report_Error("Unsupported solver method");
    return -1;
_return_err_:
    Report_Solver_Error(cpx_env);
    return -1;
}


static int
cpx_solve(lp_desc* lpd, struct lp_meth *meth, struct lp_sol *sol, double* bestbound, double* worstbound)
{
    int sol_count;
    int rev_state = GRB_LOADED;

    if (cpx_loadstart(lpd, meth->option_mipstart, sol->oldsols, sol->oldmac))
	return -1;

_retry_:
    if (GRBoptimize(lpd->lp))
    	goto _return_err_;

    if (GRBgetintattr(lpd->lp, GRB_INT_ATTR_STATUS, &lpd->sol_state))
    	goto _return_err_;

    switch(lpd->sol_state)
    {
    case GRB_OPTIMAL:			/* SuccessState */
	    lpd->descr_state = DESCR_SOLVED_SOL;
	    lpd->optimum_ctr++;
	    if (GRBgetdblattr(lpd->lp, GRB_DBL_ATTR_OBJVAL, &lpd->objval))
		goto _return_err_;
	    *worstbound = *bestbound = lpd->objval;
	    break;

    case GRB_INFEASIBLE:		/* FailState */
_grb_infeasible_:
	    lpd->descr_state = DESCR_SOLVED_NOSOL;
	    lpd->infeas_ctr++;
	    lpd->objval = 0.0;
	    *worstbound = (lpd->sense == SENSE_MIN ? -HUGE_VAL :  HUGE_VAL);
	    *bestbound = (lpd->sense ==  SENSE_MIN ?  HUGE_VAL : -HUGE_VAL);
	    break;

    case GRB_CUTOFF:			/* FailState */
	    lpd->descr_state = DESCR_SOLVED_NOSOL;
	    lpd->infeas_ctr++;
	    lpd->objval = 0.0;
	    *worstbound = (lpd->sense == SENSE_MIN ? -HUGE_VAL :  HUGE_VAL);
	    if (GRBgetdblparam(GRBgetenv(lpd->lp), GRB_DBL_PAR_CUTOFF, bestbound))
		goto _return_err_;
	    break;

    case GRB_INF_OR_UNBD:		/* MaybeFailState */
#ifdef TRY_RESOLVE_UNCERTAIN_RESULT
	    /* try to check for infeasibility by reversing the sense */
	    switch(rev_state)
	    {
	    case GRB_LOADED:
		{
		    int sense, res;
		    GRBgetintattr(lpd->lp, GRB_INT_ATTR_MODELSENSE, &sense);
		    GRBsetintattr(lpd->lp, GRB_INT_ATTR_MODELSENSE, -sense);
		    res = GRBoptimize(lpd->lp);
		    GRBsetintattr(lpd->lp, GRB_INT_ATTR_MODELSENSE, sense);
		    GRBgetintattr(lpd->lp, GRB_INT_ATTR_STATUS, &rev_state);
		    if (res == 0)
			goto _retry_;
		}
		break;
	    case GRB_INFEASIBLE:
		goto _grb_infeasible_;
	    case GRB_OPTIMAL:
	    case GRB_SUBOPTIMAL:
	    case GRB_CUTOFF:
	    case GRB_UNBOUNDED:
		goto _grb_unbounded_;
	    }
#endif

	    /* can't get a more precise result */
	    lpd->descr_state = DESCR_UNKNOWN_NOSOL;
	    lpd->infeas_ctr++;
	    lpd->objval = 0.0;
	    *worstbound = (lpd->sense == SENSE_MIN ?  HUGE_VAL : -HUGE_VAL);
	    *bestbound = (lpd->sense ==  SENSE_MIN ? -HUGE_VAL :  HUGE_VAL);
	    break;

    case GRB_UNBOUNDED:			/* UnboundedState */
_grb_unbounded_:
	    /* From Gurobi manual: an unbounded status indicates the presence
	     * of an unbounded ray that allows the objective to improve
	     * without limit. It says nothing about whether the model has a
	     * feasible solution. If you require information on feasibility,
	     * you should set the objective to zero and reoptimize.
	     */
	    lpd->descr_state = DESCR_UNBOUNDED_NOSOL;
	    lpd->abort_ctr++;
	    *bestbound = (lpd->sense == SENSE_MIN ? -HUGE_VAL : HUGE_VAL);
	    /* try to obtain a better worstbound */
	    if (GRBgetintattr(lpd->lp, GRB_INT_ATTR_SOLCOUNT, &sol_count))
		goto _return_err_;
	    if (sol_count > 0) {
		if (GRBgetdblattr(lpd->lp, GRB_DBL_ATTR_OBJVAL, worstbound))
		    goto _return_err_;
		*worstbound = lpd->objval;
	    } else {
		lpd->objval = 0.0;
		*worstbound = (lpd->sense == SENSE_MIN ?  HUGE_VAL : -HUGE_VAL);
	    }
	    break;

    case GRB_ITERATION_LIMIT:
    case GRB_NODE_LIMIT:
    case GRB_TIME_LIMIT:
    case GRB_SOLUTION_LIMIT:
    case GRB_INTERRUPTED:
    case GRB_NUMERIC:
	    lpd->abort_ctr++;
	    if (GRBgetintattr(lpd->lp, GRB_INT_ATTR_SOLCOUNT, &sol_count))
		goto _return_err_;
	    if (sol_count > 0) {
		lpd->descr_state = DESCR_ABORTED_SOL;
		if (GRBgetdblattr(lpd->lp, GRB_DBL_ATTR_OBJVAL, &lpd->objval))
		    goto _return_err_;
		*worstbound = lpd->objval;
	    } else {
		lpd->descr_state = DESCR_ABORTED_NOSOL;
		/* KISH: what worstbound value? */
		*worstbound = (lpd->sense == SENSE_MIN ? HUGE_VAL : -HUGE_VAL);
		lpd->objval = 0.0;
	    }
	    if (GRBgetdblattr(lpd->lp, GRB_DBL_ATTR_OBJBOUND, bestbound))
		goto _return_err_;
	    break;

    case GRB_SUBOPTIMAL:
	    lpd->descr_state = DESCR_ABORTED_SOL;
	    lpd->abort_ctr++;
	    if (GRBgetdblattr(lpd->lp, GRB_DBL_ATTR_OBJVAL, &lpd->objval))
		goto _return_err_;
	    *worstbound = lpd->objval;
	    if (GRBgetdblattr(lpd->lp, GRB_DBL_ATTR_OBJBOUND, bestbound))
		goto _return_err_;
	    break;

    default:
	    return -1;
    }
    return 0;
_return_err_:
    Report_Solver_Error(cpx_env);
    return -1;
}


static int
cpx_get_soln_state(lp_desc* lpd, struct lp_sol *sol)
{
    if (lpd->mac > 0)	/* columns/variables */
    {
	if (sol->sols && GRBgetdblattrarray(lpd->lp, GRB_DBL_ATTR_X, 0, lpd->mac, sol->sols))
	    goto _return_err_;
	if (sol->djs && GRBgetdblattrarray(lpd->lp, GRB_DBL_ATTR_RC, 0, lpd->mac, sol->djs))
	    goto _return_err_;
	if (sol->cbase && GRBgetintattrarray(lpd->lp, GRB_INT_ATTR_VBASIS, 0, lpd->mac, sol->cbase))
	    goto _return_err_;
    }
    if (lpd->mar > 0)	/* rows/constraints */
    {
	if (sol->slacks && GRBgetdblattrarray(lpd->lp, GRB_DBL_ATTR_SLACK, 0, lpd->mar, sol->slacks))
	    goto _return_err_;
	if (sol->pis && GRBgetdblattrarray(lpd->lp, GRB_DBL_ATTR_PI, 0, lpd->mar, sol->pis))
	    goto _return_err_;
	if (sol->rbase && GRBgetintattrarray(lpd->lp, GRB_INT_ATTR_CBASIS, 0, lpd->mar, sol->rbase))
	    goto _return_err_;
   }
   return 0;
_return_err_:
    Report_Solver_Error(cpx_env);
    return -1;
}


/* Model callback function, currently only for messages */
static int
_grb_callback(GRBmodel *model, void *cbdata, int where, void *usrdata)
{
    switch(where)
    {
	case GRB_CB_MESSAGE:
	{
	    char *msg;
	    if (solver_streams[LogType] == Current_Null)
		return 0;
	    if (GRBcbget(cbdata, where, GRB_CB_MSG_STRING, &msg))
	    	return 0;
	    (void) ec_outfs(solver_streams[LogType], msg);
	    (void) ec_flush(solver_streams[LogType]);
	    break;
	}
    }
    return 0;
}


/* Extra initialisation for new models, if needed */
static int
cpx_newmodel(GRBmodel *lp)
{
    int grb_output;
    /* install messaging callback */
    if (GRBsetcallbackfunc(lp, _grb_callback, NULL))
	return -1;
    return 0;
}


static int
cpx_loadprob(lp_desc *lpd)
{
    if (GRBloadmodel(cpx_env, &lpd->lp, "eclipse",
    		lpd->mac, lpd->mar, lpd->sense,
		0.0,		/*objcon*/
		lpd->objx, lpd->senx, lpd->rhsx,
		lpd->matbeg, lpd->matcnt, lpd->matind, lpd->matval,
		lpd->bdl, lpd->bdu, lpd->ctype, 
		NULL,		/*varnames*/
		NULL		/*cstrnames*/
		))
	goto _return_err_;

    if (lpd->nsos && GRBaddsos(lpd->lp, lpd->nsos, lpd->nsosnz,
		lpd->sostype, lpd->sosbeg, lpd->sosind, lpd->sosref))
	goto _return_err_;

    /* Switch presolve off if requested, otherwise leave cpx_env's defaults */
    if (lpd->presolve == 0) 
	GRBsetintparam(GRBgetenv(lpd->lp), GRB_INT_PAR_PRESOLVE, GRB_PRESOLVE_OFF);

    return cpx_newmodel(lpd->lp);
_return_err_:
    Report_Solver_Error(cpx_env);
    return -1;
}


static int
cpx_delsos(lp_desc *lpd, int from, int to)
{
    _grow_numbers_array(lpd, to);	/* if necessary */
    return GRBdelsos(lpd->lp, to-from, &lpd->numbers[from]);
}


static int
cpx_write(lp_desc *lpd, char *file, char *fmt)
{
    char buf[1000];
    int suflen;
    int flen = strlen(file);
    suflen = strlen(fmt);
    /* append the correct suffix, unless already there */
    if (!(flen>suflen && file[flen-suflen-1] == '.' && !strcmp(file+flen-suflen,fmt)))
	file = strcat(strcat(strcpy(buf,file),"."),fmt);
    CPXupdatemodel(lpd->lp);
    if (GRBwrite(lpd->lp, file))
	goto _return_err_;
    return 0;
_return_err_:
    Report_Solver_Error(cpx_env);
    return -1;
}


static int
cpx_read(lp_desc *lpd, char *file, char *fmt)
{
    int sen, pre, mip, qp;
    char buf[1000];
    char *suf;

    /* append correct suffix, if not already there */
    suf = strrchr(file, '.');
    if (!suf || strcmp(suf+1, fmt))
	file = strcat(strcat(strcpy(buf,file),"."),fmt);

    if (GRBreadmodel(cpx_env, file, &lpd->lp))
	goto _return_err_;

    /* initialize non-zero fields in lp_desc */
    CallN(lpd->lpcopy = lpd->lp);  /* no need for a copy */
    GRBgetintattr(lpd->lp, GRB_INT_ATTR_MODELSENSE, &sen);
    lpd->sense = sen == GRB_MINIMIZE ? SENSE_MIN : SENSE_MAX;
    GRBgetintattr(lpd->lp, GRB_INT_ATTR_NUMVARS, &lpd->mac);
    GRBgetintattr(lpd->lp, GRB_INT_ATTR_NUMCONSTRS, &lpd->mar);
    GRBgetintattr(lpd->lp, GRB_INT_ATTR_IS_MIP, &mip);
    GRBgetintattr(lpd->lp, GRB_INT_ATTR_IS_QP, &qp);
    lpd->prob_type = mip ? ( qp ? PROBLEM_MIQP : PROBLEM_MIP )
			 : ( qp ? PROBLEM_QP   : PROBLEM_LP );

    return cpx_newmodel(lpd->lp);
_return_err_:
    Report_Solver_Error(cpx_env);
    return -1;
}


/*
 * Parameter Table
 *
 * Extract #define GRB_xxx_PAR_ lines from gurobi_c.h and substitute as follows:
 * s/^#define[	 ]GRB_INT_PAR_\([^ ]*\).*$/{"\L\1\E", GRB_INT_PAR_\1, 0},/
 * s/^#define[	 ]GRB_DBL_PAR_\([^ ]*\).*$/{"\L\1\E", GRB_DBL_PAR_\1, 1},/
 * s/^#define[	 ]GRB_STR_PAR_\([^ ]*\).*$/{"\L\1\E", GRB_STR_PAR_\1, 2},/
 */

#define NUMPARAMS 83
#define NUMALIASES 10
static struct param_desc params[NUMPARAMS+NUMALIASES] = {
{"bariterlimit", GRB_INT_PAR_BARITERLIMIT, 0},
{"cutoff", GRB_DBL_PAR_CUTOFF, 1},
{"iterationlimit", GRB_DBL_PAR_ITERATIONLIMIT, 1},
{"nodelimit", GRB_DBL_PAR_NODELIMIT, 1},
{"solutionlimit", GRB_INT_PAR_SOLUTIONLIMIT, 0},
{"timelimit", GRB_DBL_PAR_TIMELIMIT, 1},
{"feasibilitytol", GRB_DBL_PAR_FEASIBILITYTOL, 1},
{"intfeastol", GRB_DBL_PAR_INTFEASTOL, 1},
{"markowitztol", GRB_DBL_PAR_MARKOWITZTOL, 1},
{"mipgap", GRB_DBL_PAR_MIPGAP, 1},
{"mipgapabs", GRB_DBL_PAR_MIPGAPABS, 1},
{"optimalitytol", GRB_DBL_PAR_OPTIMALITYTOL, 1},
{"psdtol", GRB_DBL_PAR_PSDTOL, 1},
{"method", GRB_INT_PAR_METHOD, 0},
{"perturbvalue", GRB_DBL_PAR_PERTURBVALUE, 1},
{"objscale", GRB_DBL_PAR_OBJSCALE, 1},
{"scaleflag", GRB_INT_PAR_SCALEFLAG, 0},
{"simplexpricing", GRB_INT_PAR_SIMPLEXPRICING, 0},
{"quad", GRB_INT_PAR_QUAD, 0},
{"normadjust", GRB_INT_PAR_NORMADJUST, 0},
{"sifting", GRB_INT_PAR_SIFTING, 0},
{"siftmethod", GRB_INT_PAR_SIFTMETHOD, 0},
{"barconvtol", GRB_DBL_PAR_BARCONVTOL, 1},
{"barcorrectors", GRB_INT_PAR_BARCORRECTORS, 0},
{"barhomogeneous", GRB_INT_PAR_BARHOMOGENEOUS, 0},
{"barorder", GRB_INT_PAR_BARORDER, 0},
{"barqcpconvtol", GRB_DBL_PAR_BARQCPCONVTOL, 1},
{"crossover", GRB_INT_PAR_CROSSOVER, 0},
{"crossoverbasis", GRB_INT_PAR_CROSSOVERBASIS, 0},
{"branchdir", GRB_INT_PAR_BRANCHDIR, 0},
{"heuristics", GRB_DBL_PAR_HEURISTICS, 1},
{"improvestartgap", GRB_DBL_PAR_IMPROVESTARTGAP, 1},
{"improvestarttime", GRB_DBL_PAR_IMPROVESTARTTIME, 1},
{"minrelnodes", GRB_INT_PAR_MINRELNODES, 0},
{"mipfocus", GRB_INT_PAR_MIPFOCUS, 0},
{"nodefiledir", GRB_STR_PAR_NODEFILEDIR, 2},
{"nodefilestart", GRB_DBL_PAR_NODEFILESTART, 1},
{"nodemethod", GRB_INT_PAR_NODEMETHOD, 0},
{"pumppasses", GRB_INT_PAR_PUMPPASSES, 0},
{"rins", GRB_INT_PAR_RINS, 0},
{"submipnodes", GRB_INT_PAR_SUBMIPNODES, 0},
{"symmetry", GRB_INT_PAR_SYMMETRY, 0},
{"varbranch", GRB_INT_PAR_VARBRANCH, 0},
{"solutionnumber", GRB_INT_PAR_SOLUTIONNUMBER, 0},
{"zeroobjnodes", GRB_INT_PAR_ZEROOBJNODES, 0},
{"cuts", GRB_INT_PAR_CUTS, 0},
{"cliquecuts", GRB_INT_PAR_CLIQUECUTS, 0},
{"covercuts", GRB_INT_PAR_COVERCUTS, 0},
{"flowcovercuts", GRB_INT_PAR_FLOWCOVERCUTS, 0},
{"flowpathcuts", GRB_INT_PAR_FLOWPATHCUTS, 0},
{"gubcovercuts", GRB_INT_PAR_GUBCOVERCUTS, 0},
{"impliedcuts", GRB_INT_PAR_IMPLIEDCUTS, 0},
{"mipsepcuts", GRB_INT_PAR_MIPSEPCUTS, 0},
{"mircuts", GRB_INT_PAR_MIRCUTS, 0},
{"modkcuts", GRB_INT_PAR_MODKCUTS, 0},
{"zerohalfcuts", GRB_INT_PAR_ZEROHALFCUTS, 0},
{"networkcuts", GRB_INT_PAR_NETWORKCUTS, 0},
{"submipcuts", GRB_INT_PAR_SUBMIPCUTS, 0},
{"cutaggpasses", GRB_INT_PAR_CUTAGGPASSES, 0},
{"cutpasses", GRB_INT_PAR_CUTPASSES, 0},
{"gomorypasses", GRB_INT_PAR_GOMORYPASSES, 0},
{"aggregate", GRB_INT_PAR_AGGREGATE, 0},
{"aggfill", GRB_INT_PAR_AGGFILL, 0},
{"displayinterval", GRB_INT_PAR_DISPLAYINTERVAL, 0},
{"dualreductions", GRB_INT_PAR_DUALREDUCTIONS, 0},
{"iismethod", GRB_INT_PAR_IISMETHOD, 0},
{"infunbdinfo", GRB_INT_PAR_INFUNBDINFO, 0},
{"logfile", GRB_STR_PAR_LOGFILE, 2},
{"logtoconsole", GRB_INT_PAR_LOGTOCONSOLE, 0},
{"miqcpmethod", GRB_INT_PAR_MIQCPMETHOD, 0},
{"outputflag", GRB_INT_PAR_OUTPUTFLAG, 0},
{"precrush", GRB_INT_PAR_PRECRUSH, 0},
{"predeprow", GRB_INT_PAR_PREDEPROW, 0},
{"predual", GRB_INT_PAR_PREDUAL, 0},
{"prepasses", GRB_INT_PAR_PREPASSES, 0},
{"preqlinearize", GRB_INT_PAR_PREQLINEARIZE, 0},
{"presolve", GRB_INT_PAR_PRESOLVE, 0},
{"presparsify", GRB_INT_PAR_PRESPARSIFY, 0},
{"qcpdual", GRB_INT_PAR_QCPDUAL, 0},
{"resultfile", GRB_STR_PAR_RESULTFILE, 2},
{"threads", GRB_INT_PAR_THREADS, 0},
{"feasrelaxbigm", GRB_DBL_PAR_FEASRELAXBIGM, 1},
{"dummy", GRB_STR_PAR_DUMMY, 2},

/* NUMALIASES lines follow */
{"time_limit", GRB_DBL_PAR_TIMELIMIT, 1},
{"perturbation_const", GRB_DBL_PAR_PERTURBVALUE, 1},
{"feasibility_tol", GRB_DBL_PAR_FEASIBILITYTOL, 1},
{"markowitz_tol", GRB_DBL_PAR_MARKOWITZTOL, 1},
{"absmipgap", GRB_DBL_PAR_MIPGAPABS, 1},
{"optimality_tol", GRB_DBL_PAR_OPTIMALITYTOL, 1},
{"integrality", GRB_DBL_PAR_INTFEASTOL, 1},
{"iteration_limit", GRB_DBL_PAR_ITERATIONLIMIT, 1},
{"solution_limit", GRB_INT_PAR_SOLUTIONLIMIT, 0},
{"node_limit", GRB_DBL_PAR_NODELIMIT, 1},

};

#endif
