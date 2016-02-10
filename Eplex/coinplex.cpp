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
 * Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): Kish Shen, CrossCore Optimization. 
 * 
 * END LICENSE BLOCK */

// NOECLIPSE for debugging logged calls without ECLiPSe. 
//#define NOECLIPSE
#undef NOECLIPSE
// LOG_CALLS defined when generating logged calls, but not when linking
// with the debugging logged call C program
//#define LOG_CALLS
#undef LOG_CALLS

// this is needed to keep eplex.c's lpd consistent during LOG_CALLS
#if defined(LOG_CALLS) 
# define USE_PROBLEM_ARRAY
#endif

#ifdef COIN_USE_CLP
#define GetCbcSolver(lp) ((lp)->mipmodel)
#define CBC_IS_MIPSOLVER

#include "OsiSolverInterface.hpp"
#include "OsiClpSolverInterface.hpp"
#include "CbcModel.hpp"
// Cut Generation and Heuristics
#include "CbcCutGenerator.hpp"
#include "CbcStrategy.hpp"
#include "CbcHeuristic.hpp"
#include "CbcHeuristicLocal.hpp"
#include "CbcBranchUser.hpp"
#include "CbcBranchActual.hpp"
#include "CbcCompareUser.hpp"
#include "CglGomory.hpp"
#include "CglProbing.hpp"
#include "CglKnapsackCover.hpp"
//#include "CglOddHole.hpp"
#include "CglRedSplit.hpp"
#include "CglClique.hpp"
#include "CglFlowCover.hpp"
#include "CglMixedIntegerRounding2.hpp"
// Preprocessing
#include "CglPreProcess.hpp"
// Barrier
#include "ClpInterior.hpp"
#include "ClpSimplex.hpp"
#ifdef UFL_BARRIER
// Use the University of Florida AMD library for pre-ordering sparse matrices
// This considerably improves the Barrier performance. Clp must also be 
// compiled with UFL_BARRIER support. Note that the UFL AMD library is not
// part of COIN-OR and is distributed under GNU LGPL license. 
#include "ClpCholeskyUfl.hpp"
#endif
#include "ClpCholeskyDense.hpp"
//#include "ClpCholeskyWssmp.hpp"
#include "CbcSolver.hpp"
typedef OsiClpSolverInterface OsiXxxSolverInterface;

#endif

#ifdef COIN_USE_CBC
#define GetCbcSolver(lp) ((lp)->Solver->getModelPtr())
#define CBC_IS_MIPSOLVER

#include "OsiCbcSolverInterface.hpp"
#include "OsiClpSolverInterface.hpp"
#undef COIN_USE_CLP
#include "CbcModel.hpp"
// Cut Generation and Heuristics
#include "CbcCutGenerator.hpp"
#include "CbcStrategy.hpp"
#include "CbcHeuristic.hpp"
#include "CbcHeuristicLocal.hpp"
#include "CbcBranchUser.hpp"
#include "CbcCompareUser.hpp"
#include "CglGomory.hpp"
#include "CglProbing.hpp"
#include "CglKnapsackCover.hpp"
//#include "CglOddHole.hpp"
#include "CglRedSplit.hpp"
#include "CglClique.hpp"
#include "CglFlowCover.hpp"
#include "CglMixedIntegerRounding2.hpp"
// Preprocessing
#include "CglPreProcess.hpp"
// Barrier
#include "ClpInterior.hpp"
#include "ClpSimplex.hpp"
#ifdef UFL_BARRIER
#include "ClpCholeskyUfl.hpp"
#endif
#include "ClpCholeskyDense.hpp"
//#include "ClpCholeskyWssmp.hpp"


typedef OsiCbcSolverInterface OsiXxxSolverInterface;

#endif

#ifdef COIN_USE_SYM

#include "OsiSymSolverInterface.hpp"

typedef OsiSymSolverInterface OsiXxxSolverInterface;

#endif

#ifdef COIN_USE_GLPK

#include "OsiGlpkSolverInterface.hpp"

typedef OsiGlpkSolverInterface OsiXxxSolverInterface;


#endif

#include "CoinPackedVector.hpp"
#include "CoinPackedMatrix.hpp"
#include "CoinBuild.hpp"
#include "CoinError.hpp"
#include "CoinMessageHandler.hpp"
#include <stdio.h>
#include <exception>
#include <string>
using std::string;
using namespace std;

#include <fstream>

#include "coinplex_params.h"
// must be defined before eplex_coin.h
typedef struct {
    OsiXxxSolverInterface * Solver;
    char** varnames;     /* for names of variables (columns) */
    unsigned int vnsize; /* number of variable names */
    char notfirst; /* has problem been solved? */
    /* solver specific */
#ifdef COIN_USE_CLP
    char mipIsShared; /* 1 if shared with Solver, 0 if copied */ 
    CbcModel* mipmodel;
    ClpInterior* interiormodel;
#define MIPOBJSZ	1000	// temporary, replace with std::vector
    CbcObject** mipobjects; // information such as SOS to be added to mipmodel 
    int nsos; // number of SOSs
    double timeout;
    string sparam[EpxClpParam_ns]; 
    int  iparam[EpxClpParam_ni];
    double dparam[];
#endif
} COINprob;

#include "eplex_coin.h"


// utility to check if a file exists (and is readable)
bool fileExists(const char* file)
{
    std::fstream fin;
    fin.open(file, std::ios::in);
    if (fin.is_open() )
    {
	fin.close();
	return true;
    }
    fin.close();
    return false;
}

/*****************************************************************************
 * Handlers                                                                  *
 *****************************************************************************/

#ifndef NOECLIPSE /* normal */

#include "external.h"

extern "C"
void eclipse_out(int msgtype, const char* message);

#else /* debug without ECLiPSe */

void eclipse_out(int msgtype, const char* message)
{
  printf("%s\n",message);
}
#endif

class DerivedHandler : public CoinMessageHandler 
{
public:
    virtual int print();
};

int DerivedHandler::print()
{
    int id = currentMessage().externalNumber();
    int mtype = (id<3000 ? LogType : (id<6000 ? WrnType : ErrType));
    eclipse_out(mtype, messageBuffer());

    return 0;
}

void coin_error_handler(CoinError &e)
{
    eclipse_out(ErrType, e.message().c_str());
}

/*************************************************************************
 * Solver Specific Code                                                  *
 *************************************************************************/

#if defined(COIN_USE_CLP) || defined(COIN_USE_CBC)

/* these parameters must correspond to their COIN Solver* declarations
   in eplex_params.h
*/

static CbcModel::CbcIntParam cbc_iparam[] = {CbcModel::CbcMaxNumNode, 
					     CbcModel::CbcMaxNumSol};

static CbcModel::CbcDblParam cbc_dparam[] = {CbcModel::CbcIntegerTolerance,
					     CbcModel::CbcAllowableGap,
					     CbcModel::CbcAllowableFractionGap,
					     CbcModel::CbcCutoffIncrement,
                                             CbcModel::CbcHeuristicGap,
                                             CbcModel::CbcHeuristicFractionGap};

static ClpDblParam clp_dparam[] = {ClpPresolveTolerance};

/* Meaning of whereFrom:
   1 after initial solve by dualsimplex etc
   2 after preprocessing
   3 just before branchAndBound (so user can override)
   4 just after branchAndBound (before postprocessing)
   5 after postprocessing
*/
/* Meaning of model status is as normal
   status
      -1 before branchAndBound
      0 finished - check isProvenOptimal or isProvenInfeasible to see if solution found
      (or check value of best solution)
      1 stopped - on maxnodes, maxsols, maxtime
      2 difficulties so run was abandoned
      (5 event user programmed event occurred) 

      cbc secondary status of problem
        -1 unset (status_ will also be -1)
	0 search completed with solution
	1 linear relaxation not feasible (or worse than cutoff)
	2 stopped on gap
	3 stopped on nodes
	4 stopped on time
	5 stopped on user event
	6 stopped on solutions
	7 linear relaxation unbounded

   but initially check if status is 0 and secondary status is 1 -> infeasible
   or you can check solver status.
*/
/* Return non-zero to return quickly */   
static int callBack(CbcModel * model, int whereFrom)
{
  int returnCode=0;
  switch (whereFrom) {
  case 1:
  case 2:
    if (!model->status()&&model->secondaryStatus())
      returnCode=1;
    break;
  case 3:
    {
      CbcCompareUser compare;
      model->setNodeComparison(compare);
    }
    break;
  case 4:
    // If not good enough could skip postprocessing
    break;
  case 5:
    break;
  default:
    returnCode=-1;
  }
  return returnCode;
}

int coin_branchAndBound(lp_desc* lpd, int meth, int auxmeth)
{
    // copying original bounds before presolve -- Cbc's integer presolve and
    // MIP branch-and-bound can fix some column bounds. The original bounds
    // needs to be restored before continuing
    int mac = lpd->lp->Solver->getNumCols();
    double* ups = new double[mac];
    double* lws = new double[mac];
    memcpy(ups, lpd->lp->Solver->getColUpper(), mac*sizeof(double));
    memcpy(lws, lpd->lp->Solver->getColLower(), mac*sizeof(double));

    // Tell solver to return fast if presolve or initial solve infeasible
    lpd->lp->Solver->getModelPtr()->setMoreSpecialOptions(3);

    // model is a new copy of lpd->lp->Solver
    CbcModel* model = new CbcModel(static_cast<OsiSolverInterface &>(*lpd->lp->Solver));

    int loglevel = lpd->lp->iparam[EpxClpParam_loglevel];
    DerivedHandler* mipMessageHandler = new DerivedHandler;
    model->passInMessageHandler(mipMessageHandler);
    // From John Forrest 2011-03-13, to get message logging with CbcSolver: 
    model->messageHandler()->setLogLevel(0,loglevel); // CBC
    model->messageHandler()->setLogLevel(1,lpd->lp->iparam[EpxClpParam_mip_lploglevel]); // CLP  
    model->messageHandler()->setLogLevel(2,loglevel); // Coin
    model->messageHandler()->setLogLevel(3,loglevel); // CGL
    model->setPrintFrequency(lpd->lp->iparam[EpxClpParam_print_freq]);
    CbcMain0(*model);

    /*
    CbcSolver* control = new CbcSolver(*(lpd->lp->Solver));
    control->fillValuesInSolver();
    CbcModel* model = control->model();
    */
    if (lpd->lp->mipmodel != NULL)
    {
	// copy mipmodel's parameters -- as these may contain settings from
        // the user (set through OSI or Solver parameters)
      
      for (int i=0; i<CbcModel::CbcLastIntParam; i++)
	  model->setIntParam(CbcModel::CbcIntParam(i), lpd->lp->mipmodel->getIntParam(CbcModel::CbcIntParam(i)));
	for (int i=0; i<CbcModel::CbcLastIntParam; i++)
	  model->setDblParam(CbcModel::CbcDblParam(i), lpd->lp->mipmodel->getDblParam(CbcModel::CbcDblParam(i)));
    }

    if (lpd->lp->nsos > 0)
    {// Add any SOSs
	model->addObjects(lpd->lp->nsos, lpd->lp->mipobjects);
    }

    if (lpd->lp->mipmodel != NULL) 
    {
      // mipmodel has an old copy of the model that needs to be deleted
	delete lpd->lp->mipmodel->messageHandler();
	delete lpd->lp->mipmodel;
	// if mipIsShared, then Solver is also deleted, so set to NULL
	if (lpd->lp->mipIsShared) lpd->lp->Solver = NULL;
    }

    lpd->lp->mipmodel = model;
    //model->solver()->setHintParam(OsiDoReducePrint, true, OsiHintTry);

    if (lpd->lp->timeout > 0) model->setMaximumSeconds(lpd->lp->timeout);
    //    const char * argv2="-preprocess on -solve ";
    //control->solve(argv2, 1);
    const char * cbc_args[12];
    cbc_args[0] = "eplexcbcclpsolver";
    cbc_args[1] = "-preprocess";
    cbc_args[2] = (lpd->presolve ? "on" : "off");
    int next = 3;

    switch (meth) {
    case METHOD_DUAL:
      cbc_args[next++] = "-dualSimplex";
      break;
    case METHOD_PRIMAL:
      cbc_args[next++] = "-primalSimplex";
      break;
    case METHOD_BAR:
      cbc_args[next++] = "-chol";
#ifdef UFL_BARRIER
      if (lpd->lp->sparam[EpxClpParam_bar_ordering] == "uflamd") 
	cbc_args[next++] = "Uni"; // UFL
      else 
#endif
      if (lpd->lp->sparam[EpxClpParam_bar_ordering] == "dense") 
	cbc_args[next++] = "dense";
      else
	cbc_args[next++] = "native"; // default

      cbc_args[next++] = "-cross";
      cbc_args[next++] = (auxmeth == METHOD_NONE ? "off" : "on");
      if (lpd->lp->iparam[EpxClpParam_doKKT]) {
	cbc_args[next++] =  "-KKT";
	cbc_args[next++] = "on";
      }
      cbc_args[next++] = "-barrier";
      if (auxmeth == METHOD_PRIMAL) {
	eclipse_out(WrnType, "Eplex Warning: CbcSolver supports cross-over for barrier using dual simplex only -- dual simplex used instead of primal simplex.\n");
      }
      break;
      // falls through if METHOD_DEFAULT
    }
    cbc_args[next++] = "-solve";
    cbc_args[next++] = "-quit";
    CbcMain1(next,cbc_args,*model,callBack);

    lpd->sol_itcnt = model->getIterationCount();
    lpd->sol_nodnum = model->getNodeCount();

    if (lpd->lp->Solver != NULL) 
    {
	delete lpd->lp->Solver->getModelPtr()->messageHandler();
	//coin_free_solver_handlers(lpd->lp->Solver);
	delete lpd->lp->Solver;
    }
    lpd->lp->Solver = dynamic_cast< OsiXxxSolverInterface*>(model->solver());
    //DerivedHandler* solMessageHandler = new DerivedHandler;
    //lpd->lp->Solver->getModelPtr()->passInMessageHandler(solMessageHandler);
    lpd->lp->mipIsShared = 1;

    if (lpd->prob_type == PROBLEM_FIXEDL && 
	lpd->lp->Solver->isProvenOptimal())
    {
	/* integer col bounds are already fixed to their sol values */
	lpd->lp->Solver->initialSolve();
    }

    // reset the column bounds (undo fixed bounds for integer cols)
    for (int i=0; i<mac; i++) 
	lpd->lp->Solver->setColBounds(i,lws[i],ups[i]);

    delete [] lws;
    delete [] ups;

    return 0;

}    

int coin_solveLinear(lp_desc* lpd, int meth, int auxmeth)
{
    switch (meth)
    {
    case METHOD_BAR:
        {
	ClpModel* clpmodel = lpd->lp->Solver->getModelPtr();
	lpd->lp->interiormodel = new ClpInterior;
	lpd->lp->interiormodel->borrowModel(*clpmodel);

	lpd->lp->interiormodel->messageHandler()->setLogLevel(lpd->lp->iparam[EpxClpParam_loglevel]);
#ifdef UFL_BARRIER
	if (lpd->lp->sparam[EpxClpParam_bar_ordering] == "uflamd") {
	  ClpCholeskyUfl* cholesky = new ClpCholeskyUfl(-1);
	  // Quadratic QP aparently needs a KKT factorization
	  if (lpd->prob_type == PROBLEM_QP || lpd->lp->iparam[EpxClpParam_doKKT]) 
	    cholesky->setKKT(true);
	  lpd->lp->interiormodel->setCholesky(cholesky);
	} else
#endif
	if (lpd->lp->sparam[EpxClpParam_bar_ordering] == "dense") {
	  ClpCholeskyBase* cholesky = new ClpCholeskyDense();
	  // Quadratic QP aparently needs a KKT factorization
	  if (lpd->prob_type == PROBLEM_QP || lpd->lp->iparam[EpxClpParam_doKKT]) 
	    cholesky->setKKT(true);
	  lpd->lp->interiormodel->setCholesky(cholesky);
	} else {
	  ClpCholeskyBase* cholesky = new ClpCholeskyBase(-1);
	  // Quadratic QP aparently needs a KKT factorization
	  if (lpd->prob_type == PROBLEM_QP || lpd->lp->iparam[EpxClpParam_doKKT]) 
	    cholesky->setKKT(true);
	  lpd->lp->interiormodel->setCholesky(cholesky);
	}
	lpd->lp->interiormodel->primalDual();
	// Barrier done

	//lpd->lp->interiormodel->checkSolution();
	if (lpd->lp->interiormodel->isProvenOptimal()
	    // infeasibility not correctly detected by ClpInterior, so need
	    // the next test to make sure solution is feasible
	    && lpd->lp->interiormodel->sumPrimalInfeasibilities() < 1e-5)
	{
	    // Do crossover if optimal...
	    ClpSimplex model2(*lpd->lp->interiormodel);
	    // make sure no status left
	    model2.createStatus();
	    model2.messageHandler()->setLogLevel(lpd->lp->iparam[EpxClpParam_loglevel]);

	    switch (auxmeth) {
	    case METHOD_PRIMAL:
	    case METHOD_DEFAULT:
	        model2.primal(1);
	        break;
	    case METHOD_DUAL:
	        model2.dual(1);
		break;
	    case METHOD_NONE:
	      break;
	    }
	}
	// getIterationCount() is for barrier + crossover (if any)
	lpd->sol_itcnt = lpd->lp->interiormodel->getIterationCount();
	lpd->lp->interiormodel->returnModel(*clpmodel);
        }
	break;
    case METHOD_PRIMAL:
    case METHOD_DUAL:
    case METHOD_DEFAULT:
	if (lpd->lp->notfirst)
	{
	    lpd->lp->Solver->resolve();
	}
	else
	{
	    //lpd->lp->Solver->writeLp("cointest");
	    lpd->lp->Solver->initialSolve();
	    lpd->lp->notfirst= 1;
	    /* timeout for CLP not turned off here, but only before 
	       branchAndBound() is called, because the timeout setting is 
	       needed for detecting if timeout happened or not
	    */ 
	}
	lpd->sol_itcnt = lpd->lp->Solver->getIterationCount();
	break;
    }
}


extern "C"
int coin_set_timeout(COINprob* lp, double timeout)
{
#ifdef COIN_USE_CLP
    if (timeout > 0) lp->timeout = timeout;
#endif
    
    return 0;
}


#else 

int coin_branchAndBound(lp_desc *lpd, int meth, int auxmeth)
{
  if (meth != METHOD_DEFAULT) {
    eclipse_out(WrnType, "Eplex Warning: OSI does not support specification of linear solving method for MIP problems. Specification ignored.\n");
  } 
  lpd->lp->Solver->branchAndBound();
  if (lpd->prob_type == PROBLEM_FIXEDL && 
      lpd->lp->Solver->isProvenOptimal()) {
      int mac = lpd->lp->Solver->getNumCols();
      double* ups = new double[mac];
      double* lws = new double[mac];
      memcpy(ups, lpd->lp->Solver->getColUpper(), mac*sizeof(double));
      memcpy(lws, lpd->lp->Solver->getColLower(), mac*sizeof(double));
      //fix
      lpd->lp->Solver->initialSolve();
      // restore original bounds
      for (int i=0; i<mac; i++) 
	lpd->lp->Solver->setColBounds(i,lws[i],ups[i]);
      delete [] lws;
      delete [] ups;

  }
  lpd->sol_itcnt = lpd->lp->Solver->getIterationCount();

  return 0;
}

int coin_solveLinear(lp_desc* lpd, int meth, int aux_meth)
{
#ifndef COIN_USE_SYM
    // with OsiSym, resolve seem to ignore added constraints
    if (lpd->lp->notfirst)
    {
	lpd->lp->Solver->resolve();
    }
    else
#endif
    {
	//lpd->lp->Solver->writeLp("cointest");
	lpd->lp->Solver->initialSolve();
	lpd->lp->notfirst= 1;
	lpd->sol_itcnt = lpd->lp->Solver->getIterationCount();
	/* timeout for CLP not turned off here, but only before 
	   branchAndBound() is called, because the timeout setting is 
	   needed for detecting if timeout happened or not
	*/ 
    }
}

extern "C"
int coin_set_timeout(COINprob* lp, double timeout)
{
    // Osi does not provide a generic timeout, do nothing by default
    return 0;
}

#endif
#ifdef COIN_USE_CLP
extern "C"
int coin_get_solver_dblparam(COINprob* lp, int key, double* value)
{
    if (lp->mipmodel == NULL) return -1; // should not happen
    if (key >= NumSolverMipDblParams) {
      // CLP param
      key -= NumSolverMipDblParams;
      lp->Solver->getModelPtr()->getDblParam(clp_dparam[key], *value);
    } else {
      // CBC Param
      *value = lp->mipmodel->getDblParam(cbc_dparam[key]);
    }

    return 0;
}

extern "C"
int coin_get_solver_intparam(COINprob* lp, int key, int* value)
{
    if (lp->mipmodel == NULL) return -1; // should not happen
    *value = lp->mipmodel->getIntParam(cbc_iparam[key]);

    return 0;
}

extern "C"
int coin_set_solver_dblparam(COINprob* lp, int key, double value)
{
    if (lp->mipmodel == NULL) return -1;
    if (key >= NumSolverMipDblParams) {
      // CLP param
      key -= NumSolverMipDblParams;
      lp->Solver->getModelPtr()->setDblParam(clp_dparam[key], value);
    } else
      lp->mipmodel->setDblParam(cbc_dparam[key], value); // CBC param

    return 0;
}

extern "C"
int coin_set_solver_intparam(COINprob* lp, int key, int value)
{
    if (lp->mipmodel == NULL) return -1;
    lp->mipmodel->setIntParam(cbc_iparam[key], value);

    return 0;
}

extern "C"
int coin_get_eplex_intparam(COINprob* lp, int key, int* value)
{
    if (lp->mipmodel == NULL) return -1;
    *value = lp->iparam[key];

    return 0;
}

extern "C"
int coin_get_eplex_strparam(COINprob* lp, int key, char* value)
{
    if (lp->mipmodel == NULL) return -1;
    
    int size = lp->sparam[key].length()+1;    
    if (size > STRBUFFERSIZE) size = STRBUFFERSIZE;
    string::traits_type::copy(value, lp->sparam[key].c_str(), size);
   
   return 0;

}

extern "C"
int coin_set_eplex_intparam(COINprob* lp, int key, int value)
{
    if (lp->mipmodel == NULL) return -1;
    switch (key) {
    case EpxClpParam_print_freq:
      lp->mipmodel->setPrintFrequency(value);
      lp->iparam[key] = value;
      break;
    case EpxClpParam_loglevel:
      if (value >= 0 && value <= 3) {
	lp->iparam[key] = value;
	lp->Solver->messageHandler()->setLogLevel(value);
      } else return -1;
      break;
    case EpxClpParam_mip_lploglevel:
      if (value >= 0 && value <= 3)
	lp->iparam[key] = value;
      else return -1;
      break;
    case EpxClpParam_doKKT:
      lp->iparam[key] = value;
      break;
    default:
      return -1;
      break;
    }
    return 0;
}

extern "C"
int coin_set_eplex_strparam(COINprob* lp, int key, const char* value)
{
    if (lp->mipmodel == NULL) return -1;
    switch (key) {
    case EpxClpParam_bar_ordering:
      lp->sparam[key] = value;
      break;
    default:
      return -1;
    }
    
    return 0;
}

void coin_set_solver_outputs(OsiXxxSolverInterface* Solver)
{
    DerivedHandler* solMessageHandler = new DerivedHandler;
    Solver->getModelPtr()->passInMessageHandler(solMessageHandler);
}

void coin_free_solver_handlers(OsiXxxSolverInterface* Solver)
{
    delete Solver->getModelPtr()->messageHandler();
}


#elif defined(COIN_USE_CBC)

extern "C"
int coin_get_solver_dblparam(COINprob* lp, int key, double* value)
{
    CbcModel* model = lp->Solver->getModelPtr();
    
    *value = model->getDblParam(cbc_dparam[key]);

    return 0;
}

extern "C"
int coin_get_solver_intparam(COINprob* lp, int key, int* value)
{
    CbcModel* model = lp->Solver->getModelPtr();
    
    *value = model->getIntParam(cbc_iparam[key]);

    return 0;
}

extern "C"
int coin_set_solver_intparam(COINprob* lp, int key, int value)
{
    CbcModel* model = lp->Solver->getModelPtr();

    model->setIntParam(cbc_iparam[key], value);
    return 0;
}

extern "C"
int coin_set_solver_dblparam(COINprob* lp, int key, double value)
{
    CbcModel* model = lp->Solver->getModelPtr();

    model->setDblParam(cbc_dparam[key], value);
    return 0;
}

void coin_set_solver_outputs(OsiXxxSolverInterface* Solver)
{
    CbcModel* model = Solver->getModelPtr();
    DerivedHandler* cbcMessageHandler = new DerivedHandler;
    model->passInMessageHandler(cbcMessageHandler);
    model->messageHandler()->setLogLevel(1);

    OsiClpSolverInterface* clp = 
	dynamic_cast< OsiClpSolverInterface*> (Solver->getRealSolverPtr());
    if (clp != NULL)
    {/* != NULL if using CLP */
	DerivedHandler* clpMessageHandler = new DerivedHandler;
	clp->passInMessageHandler(clpMessageHandler);
	clp->messageHandler()->setLogLevel(1);
    }
}

void coin_free_solver_handlers(OsiXxxSolverInterface* Solver)
{
    CbcModel* model = Solver->getModelPtr();
    delete model->messageHandler();

    OsiClpSolverInterface* clp = 
	dynamic_cast< OsiClpSolverInterface*> (Solver->getRealSolverPtr());
    if (clp != NULL) 
    {/* != NULL if using CLP */
	delete clp->messageHandler();
    }
}

#else

extern "C"
int coin_get_solver_dblparam(COINprob* lp, int key, double* value)
{
    return -1;
}

extern "C"
int coin_set_solver_dblparam(COINprob* lp, int key, double value)
{
    return -1;
}

extern "C"
int coin_get_solver_intparam(COINprob* lp, int key, int* value)
{
    return -1;
}

extern "C"
int coin_set_solver_intparam(COINprob* lp, int key, int value)
{
    return -1;
}

extern "C"
int coin_get_eplex_intparam(COINprob* lp, int key, int* value)
{
    return -1;
}

extern "C"
int coin_set_eplex_intparam(COINprob* lp, int key, int value)
{
    return -1;
}

extern "C"
int coin_get_eplex_strparam(COINprob* lp, int key, char* value)
{
    return -1;
}

extern "C"
int coin_set_eplex_strparam(COINprob* lp, int key, const char* value)
{
    return -1;
}

extern "C"
int coin_set_solver_methods(lp_desc* lpd, int method, int auxmethod, 
			   int node_meth, int node_auxmeth)
{
    return -1;
}

void coin_set_solver_outputs(OsiXxxSolverInterface* Solver)
{
}

void coin_free_solver_handlers(OsiXxxSolverInterface* Solver)
{
}


#endif


/*****************************************************************************
 * Generic OSI/Coin Code                                                     *
 *****************************************************************************/


extern "C"
int coin_getrhs(COINprob* lp, double* rhs, int start, int end)
{
    const double *rhs0 = lp->Solver->getRightHandSide();
    int j=0;

    if (start < 0) return -1;
    if (end >= lp->Solver->getNumRows()) return -1;

    for (int i=start; i<=end; i++)
    {
	rhs[j++] = rhs0[i];
    }
    return 0;
}

extern "C"
int coin_getrowsense(COINprob* lp, char* rsense, int start, int end)
{
    const char* rsense0 = lp->Solver->getRowSense();
    int j=0;

    if (start < 0) return -1;
    if (end >= lp->Solver->getNumRows()) return -1;

    for (int i=start; i<=end; i++)
    {
	// sense type is the same for OSI and CPLEX/Xpress
	rsense[j++] = rsense0[i];
    }
    return 0;
}

extern "C"
int coin_getlb(COINprob* lp, double* lb, int start, int end)
{
    const double* lb0 = lp->Solver->getColLower();
    int j=0;

    if (start < 0) return -1;
    if (end >= lp->Solver->getNumCols()) return -1;

    for (int i=start; i<=end; i++)
    {
	lb[j++] = lb0[i];
    }
    return 0;
}

extern "C"
int coin_getub(COINprob* lp, double* ub, int start, int end)
{
    const double* ub0 = lp->Solver->getColUpper();
    int j=0;

    if (start < 0) return -1;
    if (end >= lp->Solver->getNumCols()) return -1;

    for (int i=start; i<=end; i++)
    {
	ub[j++] = ub0[i];
    }
    return 0;
}

extern "C"
int coin_getcoltype(COINprob* lp, char* ctype, int start, int end)
{
    int j=0;

    if (start < 0) return -1;
    if (end >= lp->Solver->getNumCols()) return -1;

    for (int i=start; i<=end; i++)
    {
	if (lp->Solver->isContinuous(i)) ctype[j++] = 'C';
	else if (lp->Solver->isInteger(i)) ctype[j++] = 'I';
	else if (lp->Solver->isBinary(i)) ctype[j++] = 'B';
	else return -1;  /* unknown type -- error! */
    }
    return 0;
}

extern "C"
int coin_chgcoltype(COINprob* lp, int cnt, int* idxs, char* ctype)
{

    for (int i=0; i<cnt; i++)
    {
	int j=idxs[i];
	if (j < 0 || j >= lp->Solver->getNumCols()) return -1;

	if (ctype[i] == 'C')      lp->Solver->setContinuous(j);
	else if (ctype[i] == 'I') lp->Solver->setInteger(j);
	else if (ctype[i] == 'B') lp->Solver->setInteger(j); // no setBinary()
	else return -1;  /* unknown type -- error! */
    }
    return 0;
}

extern "C"
int coin_chgbds(COINprob* lp, int cnt, int* idxs, char* lu, double* bd)
{

    for (int i=0; i<cnt; i++)
    {
	int j=idxs[i];
	if (j < 0 || j >= lp->Solver->getNumCols()) return -1;

	if (lu[i] == 'U')      lp->Solver->setColUpper(j, bd[i]);
	else if (lu[i] == 'L') lp->Solver->setColLower(j, bd[i]);
	else if (lu[i] == 'B') lp->Solver->setColBounds(j, bd[i], bd[i]);
	else return -1; /* unknown type -- error! */
    }
    return 0;
}

extern "C"
int coin_loadbasis(COINprob* lp, const int* cbase, const int* rbase)
{
#ifndef COIN_USE_CBC
    lp->Solver->setBasisStatus(cbase, rbase);
#endif
    return 0;
}

extern "C"
int coin_getbasis(COINprob* lp, int* cbase, int* rbase)
{
#ifndef COIN_USE_CBC
    // coin_getbasis() is only called when both cbase and rbase != NULL
    lp->Solver->getBasisStatus(cbase,rbase);
#endif

    return 0;
}

extern "C"
int coin_getobjval(COINprob* lp, double &objval)
{
#ifdef COIN_USE_SYM
    // solving empty problem with OsiSym core dumps
    if (lp->Solver->getNumCols() == 0) objval = 0;
    else
#endif
    objval = lp->Solver->getObjValue();
    return 0;
}

extern "C"
int coin_get_lpobjval(lp_desc* lpd, double* objval)
{
#ifdef COIN_USE_CLP
    /* return the current linear objective value */
    if (IsMIPProb(lpd->prob_type))
    {
	*objval = lpd->lp->mipmodel->getCurrentObjValue();
	return 0;
    }
#endif
#ifdef COIN_USE_SYM
    // solving empty problem with OsiSym core dumps
    if (lpd->lp->Solver->getNumCols() == 0) *objval = 0;
    else
#endif
    *objval = lpd->lp->Solver->getObjValue();
    return 0;
}

extern "C"
int coin_get_mipobjval(COINprob* lp, double* objval)
{
#ifdef COIN_USE_CLP
    *objval = lp->mipmodel->getObjValue();
#else
    *objval = lp->Solver->getObjValue();
#endif
    return 0;
}

extern "C"
int coin_get_bestmipbound(COINprob* lp, double* bound)
{
#ifdef COIN_USE_CLP
    *bound = lp->mipmodel->getBestPossibleObjValue();
#elif defined(COIN_USE_CBC)
    *bound = lp->Solver->getModelPtr()->getBestPossibleObjValue();
#else
    // generic: just return the right infinity (i.e. no information)
    *bound = (lp->Solver->getObjSense() == 1 ? -1*lp->Solver->getInfinity() : lp->Solver->getInfinity());
#endif
    return 0;
}

extern "C"
int coin_get_objcoeffs(COINprob* lp, double* objc, int start, int end)
{
    const double* objc0 = lp->Solver->getObjCoefficients();
    int j=0;

    if (start < 0) return -1;
    if (end >= lp->Solver->getNumCols()) return -1;

    for (int i=start; i<=end; i++)
    {
	objc[j++] = objc0[i];
    }
    return 0;
}

extern "C"
int coin_chg_objcoeffs(COINprob* lp, int cnt, int* idxs, double* values)
{

    for (int i=0; i<cnt; i++)
    {
	int j=idxs[i];
	if (j < 0 || j >= lp->Solver->getNumCols()) return -1;

	lp->Solver->setObjCoeff(j,values[i]);
    }
    return 0;
}

extern "C"
int coin_get_order(COINprob* lp, int cnt, int* idxs, int* prio, int* direction)
{
    return -1;
}

extern "C"
int coin_set_qobj(COINprob* lp, int mac, int cb_cnt, int* cb_index,
		  int* cb_index2, double* cb_value)
{
#ifdef COIN_USE_CLP
    if (cb_cnt > 0)
    {
	CoinBigIndex* starts = new CoinBigIndex[mac+1];
	int* colidx = new int[cb_cnt];
	double* coeffs = new double[cb_cnt];
	int cur_col = 0;
	int cur_start = 0;

	for (int i=0; i<cb_cnt; i++)
	{
	    // cb_index are sorted in ascending order
	    colidx[i] = cb_index2[i];
	    coeffs[i] = cb_value[i];
	    if (cb_index[i] > cur_col)
	    {// starting coeffs for new col
	        for (int s=cur_col; s < cb_index[i]; s++) 
		    starts[s] = cur_start;
		cur_start = i;
		cur_col = cb_index[i];
	    }
	}
	// no coeffs for rest of cols, fill the starts in
	starts[cur_col] = cur_start; // last col with coeffs
	for (int s=cur_col+1; s <= mac; s++) starts[s] = cb_cnt;

	lp->Solver->getModelPtr()->loadQuadraticObjective(mac, starts, colidx, coeffs);

	delete [] starts;
	delete [] colidx;
	delete [] coeffs;

    }

    return 0;

#else

    return -1;

#endif
}

extern "C"
int coin_chgqobj(COINprob* lp, int i, int j, double value)
{
    return -1;
}

extern "C"
int coin_chgrhs(COINprob* lp, int cnt, int* idxs, double* values)
{

    const char* rsen = lp->Solver->getRowSense();
    for (int i=0; i<cnt; i++)
    {
	int j=idxs[i];
	if (j < 0 || j >= lp->Solver->getNumRows()) return -1;

	switch (rsen[j])
	{
	case 'L': lp->Solver->setRowUpper(j, values[i]); break;
	case 'G': lp->Solver->setRowLower(j, values[i]); break;
	case 'E': lp->Solver->setRowBounds(j,values[i],values[i]);; break;
	default: return -1;
	}
    }
    return 0;
}

extern "C"
int coin_getnumnz(COINprob* lp)
{
    return lp->Solver->getNumElements();
}

extern "C"
int coin_getnumint(COINprob* lp)
{
    return lp->Solver->getNumIntegers();
}

extern "C"
int coin_loadprob(COINprob* lp, int mac, int mar, int objsen, double* objx, 
		  double* rhsx, char* senx, 
		  int* matbeg, int* matcnt, int* matind, double* matval, 
		  double* lb, double* ub)
{
    double* range = new double[mar];

    /* coin doesn't use matcnt, but needs the matbeg for one more column to 
       be specifified
    */
    matbeg[mac] = (mac > 0 ? matbeg[mac-1]+matcnt[mac-1] : 0); 
    for (int i=0; i<mar; i++) range[i] = 0.0;

    //    CoinPackedMatrix* mat = 
    //new CoinPackedMatrix(true, mar, mac, matbeg[mac],
    //			   matval, matind, matbeg, matcnt, 0.6, 0.6);
    try {
      lp->Solver->loadProblem(mac, mar, matbeg, matind, matval, lb, ub, objx, senx, rhsx, range);
      //lp->Solver->loadProblem(*mat,lb, ub, objx, senx, rhsx, range);
      lp->Solver->setObjSense((objsen == SENSE_MIN ? 1 : -1));
      delete [] range;
      //delete mat;
    }
    catch (CoinError e)
    {
      coin_error_handler(e);
      return -1;
    }
    catch (bad_alloc&)
    {
      eclipse_out(ErrType, "Memory allocation error in external solver\n");
      return -1;
    }

    return 0;
}

extern "C"
int coin_setcoltype(COINprob* lp, char *ctype)
{
    int mac = lp->Solver->getNumCols();
    for (int i=0; i<mac; i++)
    {
	if (ctype[i] == 'C') lp->Solver->setContinuous(i);
	else if (ctype[i] == 'I') lp->Solver->setInteger(i);
	else if (ctype[i] == 'B') lp->Solver->setInteger(i); // no setBinary() 
	else return -1;  /* unknown type -- error! */
    }
    return 0;
}

extern "C"
int coin_addcols(COINprob* lp, int coladded, int matnz, const double* objx, 
		 int* matbeg, const int* matind, const double* matval, 
		 const double* bdl, const double* bdu)
{

    matbeg[coladded] = matnz;

    CoinBuild build;

    for (int i=0; i<coladded; i++)
    {
	build.addColumn(matbeg[i+1]-matbeg[i],
			&(matind[matbeg[i]]),
			&(matval[matbeg[i]]),
			bdl[i], bdu[i], objx[i]);
    }

    static_cast<OsiSolverInterface* >(lp->Solver)->addCols(build);

    return 0;
}

extern "C"
int coin_addrows(COINprob* lp, const int rowadded, int nzadded, 
		 const double* rhsx, const char* senx,
		 int* rmatbeg, int* rmatind, double* rmatval)
{
    //CoinPackedVector * rows = new CoinPackedVector[rowadded];

    rmatbeg[rowadded] = nzadded;

    //    double*  rrange = new double[rowadded];
    double inf = lp->Solver->getInfinity();

    CoinBuild build;
    for (int i=0; i < rowadded; i++)
    {
	double ub, lb;
	if (senx[i] == 'L') {lb= -inf; ub= rhsx[i];}
	else if (senx[i] == 'E') {lb= ub= rhsx[i];}
	else if (senx[i] == 'G') {lb= rhsx[i]; ub= inf;}
	else return -1;

	build.addRow(rmatbeg[i+1]-rmatbeg[i],
		     &(rmatind[rmatbeg[i]]),
		     &(rmatval[rmatbeg[i]]),
		     lb, ub);
    }
    static_cast<OsiSolverInterface* >(lp->Solver)->addRows(build);
    /*
    for (int i=0; i < rowadded; i++)
    {
	rrange[i] = 0;

	rows[i].setVector(rmatbeg[i+1]-rmatbeg[i],
			  &(rmatind[rmatbeg[i]]),
			  &(rmatval[rmatbeg[i]]),
			  false);
    }


        CoinPackedVectorBase* rows1 = static_cast<CoinPackedVectorBase *>(rows);
    lp->Solver->addRows(rowadded, const_cast<CoinPackedVectorBase* const*>(&rows1), senx, rhsx, const_cast<const double *>(rrange)); 

    for (int l=0; l<rowadded; l++) {
	double* elms=rows[l].getElements();
	for (int m=0; m<rows[l].getNumElements(); m++) std::cout<<elms[m]<<" ";
	std::cout<<std::endl;
    }
    const CoinPackedMatrix* mat = lp->Solver->getMatrixByRow();
    const double* elms = mat->getElements();
    for (int k=0; k<mat->getNumElements(); k++)  std::cout<<elms[k]<<" ";
    std::cout<<std::endl;
    */
    return 0;
}

extern "C"
int coin_chgobjsen(COINprob* lp, int objsen)
{
    lp->Solver->setObjSense((objsen == SENSE_MIN ? 1 : -1));
    return 0;
}

extern "C"
int coin_get_row(COINprob* lp, int* nnz, int* rmatind, double* rmatval, int idx)
{
    try
    {
	const CoinShallowPackedVector row = 
	    lp->Solver->getMatrixByRow()->getVector(idx);

	*nnz = row.getNumElements();
	memcpy(rmatind, row.getIndices(), (*nnz)*sizeof(int));
	memcpy(rmatval, row.getElements(), (*nnz)*sizeof(double));
    }
    catch (CoinError e)
    {
	coin_error_handler(e);
	return -1;
    }

    return 0;
}

extern "C"
int coin_delrows(COINprob* lp, int ndr, int* idxs)
{
    lp->Solver->deleteRows(ndr, idxs);
	    
    return 0;
}

extern "C"
int coin_delcols(COINprob* lp, int ndr, int* idxs)
{
    lp->Solver->deleteCols(ndr, idxs);

    return 0;
}

extern "C"
int coin_get_bar_primal_objval(COINprob* lp, double* objval)
{
#ifdef COIN_USE_CLP
    if (lp->interiormodel != NULL)
    {
	*objval = lp->interiormodel->rawObjectiveValue()*lp->interiormodel->optimizationDirection();
	return 0;
    }
#endif
    return -1;
}

extern "C"
int coin_get_bar_dual_objval(COINprob* lp, double* objval)
{
#ifdef COIN_USE_CLP
    if (lp->interiormodel != NULL)
    {
	// no information at the moment, just return the right infinity 
	*objval =  ( 
		   lp->interiormodel->isProvenOptimal()
		   // optimal, just return obj. value 
		   ? lp->interiormodel->rawObjectiveValue()*lp->interiormodel->optimizationDirection()
		   // opt.dir = -1 max, 1 min => inf for max, -inf for min
		   : -1.0*lp->interiormodel->optimizationDirection()*lp->Solver->getInfinity() 
		   );

	return 0;
    }
#endif
    return -1;
}

/* this should be called soon after a call to coin_solve_problem(), before
   any backtracking, because the result state etc. are not stored logically
*/
extern "C"
state_t coin_get_result_state(lp_desc* lpd)
{
    //    OsiXxxSolverInterface* Solver = lpd->lp->Solver;
#ifdef COIN_USE_SYM
    // solving empty problem with OsiSym core dumps
  if (lpd->lp->Solver->getNumCols() == 0) {
      lpd->sol_state = S_SUCCESS;
      return state_success;
  }
#endif
#ifdef COIN_USE_CLP
    // get more MIP information using CLP specific methods...
    if (IsMIPProb(lpd->prob_type))
    {
	CbcModel* model = lpd->lp->mipmodel;
	if (model->isProvenOptimal()) {
	    lpd->sol_state = S_SUCCESS;
	    return state_success;
	}
	if (model->isInitialSolveProvenOptimal()) // succeeded at root
	{
	    if (model->isProvenInfeasible()) {
	        lpd->sol_state = S_FAIL;
	        return state_fail;
	    }
	    // MIP was aborted -- determine why
	    if (model->isNodeLimitReached()) lpd->sol_state = S_ABORT_NODELIM;
	    else if (model->isSecondsLimitReached()) lpd->sol_state = S_ABORT_TIMELIM;
	    else if (model->isSolutionLimitReached()) lpd->sol_state = S_ABORT_SOLLIM;
	    else if (model->isAbandoned()) lpd->sol_state = S_ABORT_NUM;
	    else lpd->sol_state = S_ABORT_UNKNOWN;

	    if (model->bestSolution()) return state_mipsemisucc;
	    return state_mipsemifail;
	}
	if (model->isInitialSolveAbandoned()) {
	    lpd->sol_state = S_ABORT_NUM;
	    return state_lpaborted;
	}
	// unbounded at root => MIP can be unbounded or infeasible
	if (model->isContinuousUnbounded()) {
	    lpd->sol_state = S_UNBOUND_OR_FAIL;
	    return state_unknown;
	} 
	if (model->isInitialSolveProvenPrimalInfeasible()) {
	  lpd->sol_state = S_FAIL;
	  return state_fail;
	}
	//if (model->isInitialSolveProvenDualInfeasible()) return state_unbounded;
	if (lpd->lp->Solver->getModelPtr()->hitMaximumIterations()) lpd->sol_state = S_ABORT_LIM;
	else if (lpd->lp->Solver->getModelPtr()->isPrimalObjectiveLimitReached()) lpd->sol_state = S_ABORT_PRIMOBJLIM;
	else if (lpd->lp->Solver->getModelPtr()->isDualObjectiveLimitReached()) lpd->sol_state = S_ABORT_DUALOBJLIM;
	else lpd->sol_state = S_ABORT_UNKNOWN;
	return state_mipsemifail;
    }
    if (lpd->lp->interiormodel != NULL)
    {// CLP's interior needs special test to detect failure
	if (lpd->lp->Solver->isProvenOptimal())
        {
	    if (lpd->lp->interiormodel->sumPrimalInfeasibilities() < 1e-5) {
	        lpd->sol_state = S_SUCCESS;
		return state_success;
	    } else {
	        lpd->sol_state = S_FAIL;
		return state_fail;
	    }
	}
    } else if (lpd->lp->Solver->isProvenOptimal()) {
	lpd->sol_state = S_SUCCESS;
        return state_success;
    }
#else // !COIN_USE_CLP

    if (lpd->lp->Solver->isProvenOptimal()) {
	lpd->sol_state = S_SUCCESS;
        return state_success;
    }
#endif
    // isAbandoned() due to numeric difficulties only
    if (lpd->lp->Solver->isAbandoned()) 
    {
        lpd->sol_state = S_ABORT_NUM;
	if (IsMIPProb(lpd->prob_type)) return state_mipsemifail;
	else return state_lpaborted;
    }
    if (lpd->lp->Solver->isProvenPrimalInfeasible()) {
	lpd->sol_state = S_FAIL;
        return state_fail;
    }
    if (lpd->lp->Solver->isProvenDualInfeasible()) {
      lpd->sol_state = S_UNBOUND;
        return state_unbounded;
    }
    // problem is not optimal, infeasible or unbounded, solving is incomplete. 
    // For MIP, we need to extract information from the MIP to determine
    // if there is any feasible solution or not. OSI's API does not provide
    // this, so we return semifail by default if solver specific methods
    // are not used earlier
    if (IsMIPProb(lpd->prob_type)) {
        lpd->sol_state = S_ABORT_UNKNOWN;
        return state_mipsemifail;
    }
    // is LP...
#ifdef COIN_USE_CLP
    // hit max. iterations *or timeout* 
    if (lpd->lp->Solver->getModelPtr()->hitMaximumIterations()) {
        lpd->sol_state = S_ABORT_LIM;
        return state_lpaborted;
    }
#else
    if (lpd->lp->Solver->isIterationLimitReached()) {
        lpd->sol_state = S_ABORT_LIM;
        return state_lpaborted;
    }
#endif
    if (lpd->lp->Solver->isPrimalObjectiveLimitReached()) {
        lpd->sol_state = S_ABORT_LIM;
        return state_lpaborted;
    }
    if (lpd->lp->Solver->isDualObjectiveLimitReached()) {
        lpd->sol_state = S_ABORT_DUALOBJLIM;
	return state_lpaborted;
    }
    // no better information....
    lpd->sol_state = S_ABORT_UNKNOWN;
    return state_unknown; 
}

extern "C"
int coin_get_mipcutoff(COINprob* lp, double* cutoff)
{
#ifdef CBC_IS_MIPSOLVER
    CbcModel* mip = GetCbcSolver(lp);

    *cutoff = (lp->Solver->getObjSense() == 1 ? mip->getCutoff() : -1.0*mip->getCutoff());
#else
    *cutoff = (lp->Solver->getObjSense() == 1 ? lp->Solver->getInfinity() : -1.0*lp->Solver->getInfinity());
#endif 

    return 0;
}

extern "C"
double coin_infinity(COINprob* lp)
{
    return lp->Solver->getInfinity();
}

extern "C"
int coin_getdblparam(COINprob* lp, int key, double* value)
{
    lp->Solver->getDblParam(OsiDblParam(key), *value);
    return 0;
}

extern "C"
int coin_getintparam(COINprob* lp, int key, int* value)
{
    lp->Solver->getIntParam(OsiIntParam(key), *value);
    return 0;
}

extern "C"
int coin_getstrparam(COINprob* lp, int key, char* value)
{
    string svalue;
    lp->Solver->getStrParam(OsiStrParam(key), svalue);

    int size = svalue.length()+1;
    if (size > STRBUFFERSIZE) size = STRBUFFERSIZE;
    string::traits_type::copy(value, svalue.c_str(), size);

    return 0;
}

extern "C"
int coin_setdblparam(COINprob* lp, int key, double value)
{
    lp->Solver->setDblParam(OsiDblParam(key), value);
    return 0;
}

extern "C"
int coin_setintparam(COINprob* lp, int key, int value)
{
    lp->Solver->setIntParam(OsiIntParam(key), value);
    return 0;
}

extern "C"
int coin_setstrparam(COINprob* lp, int key, const char* value)
{
  const string svalue = value;
    if (lp->Solver->setStrParam(OsiStrParam(key), svalue))
        return 0;
    else return -1;
}

extern "C"
int coin_solve_problem(lp_desc* lpd, 
		       int meth, int auxmeth, int node_meth, int node_auxmeth)
{
    bool doDual;

#ifdef COIN_USE_SYM
    // solving empty problem with OsiSym core dumps
    if (lpd->lp->Solver->getNumCols() == 0) return 0;
    if (lpd->lp->Solver->getNumRows() == 0) return 0;
#endif
#ifdef COIN_USE_CLP
    // delete any old interior model of problem
    if (lpd->lp->interiormodel != NULL)
    {
	delete lpd->lp->interiormodel;
	lpd->lp->interiormodel = NULL;
    }
#endif
    switch (meth)
    {// OSI allows only primal/dual to be specified. Barrier done later
    default:
    case METHOD_DEFAULT:
    case METHOD_DUAL:
	doDual = true;
	break;
    case METHOD_PRIMAL:
	doDual = false;
	break;
    }

    lpd->lp->Solver->setHintParam(OsiDoDualInInitial, doDual, OsiHintDo, NULL);
    lpd->lp->Solver->setHintParam(OsiDoDualInResolve, doDual, OsiHintDo, NULL);

    lpd->lp->Solver->setHintParam(OsiDoPresolveInInitial, lpd->presolve, OsiHintDo, NULL);
    lpd->lp->Solver->setHintParam(OsiDoPresolveInResolve, lpd->presolve, OsiHintDo, NULL);

    try {
      switch (lpd->prob_type) {
      case PROBLEM_MIP:
      case PROBLEM_FIXEDL:
#ifdef COIN_USE_CLP
	/* turn off timeout in CLP (otherwise may cause problems for MIP */
	lpd->lp->Solver->getModelPtr()->setMaximumSeconds(-1);
	/* turn off OSI presolve hint, as this may casue CLP to presolve the
           problem without synchronising it with CBC, as suggested by 
	   John Forrest @ IBM (main author of CLP and CBC)
	*/
	lpd->lp->Solver->setHintParam(OsiDoPresolveInInitial, false, OsiHintDo, NULL);
	lpd->lp->Solver->setHintParam(OsiDoPresolveInResolve, false, OsiHintDo, NULL);
	
#endif
	if (node_meth != METHOD_DEFAULT) {
	  eclipse_out(WrnType, "Eplex Warning: node solving method for MIP problems not supported by COIN solvers, method ignored.\n");
	}
	lpd->lp->Solver->setHintParam(OsiDoInBranchAndCut, true, OsiHintDo);
	coin_branchAndBound(lpd, meth, auxmeth);
	break;
      case PROBLEM_LP:
      case PROBLEM_RELAXEDL:
#ifdef COIN_USE_CLP
      case PROBLEM_QP:
	// case PROBLEM_RELAXEDQ:
	//	lpd->lp->Solver->getModelPtr()->setPerturbation(50);
	if (lpd->lp->timeout > 0) 
	    lpd->lp->Solver->getModelPtr()->setMaximumSeconds(lpd->lp->timeout);
#endif
	//lpd->lp->Solver->setHintParam(OsiDoCrash, true, OsiHintDo);
	lpd->lp->Solver->setHintParam(OsiDoInBranchAndCut, false, OsiHintDo);
	coin_solveLinear(lpd, meth, auxmeth);
	break;

      default:
	eclipse_out(ErrType, "Eplex Error: cannot solve problem type with this solver.\n"); 
	return -1;
	break;
      }
    } /* try */
    catch (CoinError e)
    {
      coin_error_handler(e);
      return -1;
    }
    catch (bad_alloc&)
    {
      eclipse_out(ErrType, "Memory allocation error in external solver\n");
      return -1;
    }

    return 0;
}

extern "C"
int cpx_get_soln_state(lp_desc* lpd, struct lp_sol *sol)
{
    int mac = lpd->lp->Solver->getNumCols();
    int mar = lpd->lp->Solver->getNumRows();

#ifdef COIN_USE_SYM
    // solving empty problem with OsiSym core dumps
    if (mac == 0) return 0;
#endif
    if (lpd->mar != mar || lpd->mac != mac)
    {
	eclipse_out(ErrType, "Eplex Error: rows and columns does not match the problem!\n");
	return -1;
    }

    if (sol->sols != NULL)
	memcpy(sol->sols, lpd->lp->Solver->getColSolution(), mac*sizeof(double));
    if (sol->pis != NULL)
	memcpy(sol->pis, lpd->lp->Solver->getRowPrice(), mar*sizeof(double));
    if (sol->slacks != NULL)
    {
	const double* act = lpd->lp->Solver->getRowActivity();
	const double* rhs = lpd->lp->Solver->getRightHandSide();

	for (int i=0; i<mar; i++) 
	{
	    sol->slacks[i] = rhs[i] - act[i];
	}
    }
    if (sol->djs != NULL)
	memcpy(sol->djs, lpd->lp->Solver->getReducedCost(), mac*sizeof(double));
#ifndef COIN_USE_CBC
    // basis not available for CBC 
    if (sol->cbase != NULL || sol->rbase != NULL)
    {
	int* cbase0 = (sol->cbase == NULL ? new int[mac] : sol->cbase);
	int* rbase0 = (sol->rbase == NULL ? new int[mar] : sol->rbase);

	lpd->lp->Solver->getBasisStatus(cbase0, rbase0);
	if (sol->cbase == NULL) delete [] cbase0; 
	if (sol->rbase == NULL) delete [] rbase0;
    }
#endif
    return 0;
}

extern "C"
int coin_get_objsen(COINprob* lp)
{
    return (lp->Solver->getObjSense() == 1 ? SENSE_MIN : SENSE_MAX);
}

extern "C"
int coin_get_numcols(COINprob* lp)
{
    return (lp->Solver->getNumCols());
}

extern "C"
int coin_get_numrows(COINprob* lp)
{
    return (lp->Solver->getNumRows());
}

extern "C"
int coin_get_probtype(COINprob* lp)
{
#ifdef COIN_USE_CBC
    CbcModel* model = lp->Solver->getModelPtr();

    if (model->numberIntegers() > 0) return PROBLEM_MIP;
    else return PROBLEM_LP;
#else
    int mac = lp->Solver->getNumCols();
    // there is no constant time method of getting the integer cols info yet
    // so this should be more efficient than getNumIntegers()
    for (int i=0; i<mac; i++)
    {
	if (lp->Solver->isInteger(i)) return PROBLEM_MIP;
    }
    return PROBLEM_LP; // no integer columns - LP problem
#endif
    
}

extern "C"
int coin_create_prob(COINprob** plp, COINprob* def)
{
    // def is `default' problem with default settings. NULL if creating default
    DerivedHandler* coinMessageHandler = new DerivedHandler;
    COINprob* lp;
    *plp = lp = new COINprob;
    lp->Solver = new OsiXxxSolverInterface();
    lp->notfirst = 0;
    lp->varnames = NULL;
    lp->vnsize = 0;
#ifdef COIN_USE_CLP
    lp->mipmodel = new CbcModel(static_cast<OsiSolverInterface &>(*lp->Solver));
    lp->mipmodel->passInMessageHandler(coinMessageHandler);
    lp->mipIsShared = 0;
    //    lp->control = NULL;
    lp->mipobjects = new CbcObject* [MIPOBJSZ];
    lp->nsos = 0;
    lp->interiormodel = NULL;

    if (def)
    {// copy the parameter values from default
        // this should copy the parameters from def to lp, but it does not
        // seem to work, so params are copied individually
        //lp->Solver->copyParameters(*def->Solver);
        for (int i=0; i<OsiLastIntParam; i++) {
	    int val;
	    if (def->Solver->getIntParam(OsiIntParam(i), val))
	        lp->Solver->setIntParam(OsiIntParam(i), val);
        } 
        for (int i=0; i<OsiLastDblParam; i++) {
	    double val;
	    if (def->Solver->getDblParam(OsiDblParam(i), val))
	        lp->Solver->setDblParam(OsiDblParam(i), val);
        } 
        for (int i=0; i<OsiLastStrParam; i++) {
	    string val;
	    if (def->Solver->getStrParam(OsiStrParam(i), val))
	        lp->Solver->setStrParam(OsiStrParam(i), val);
        } 
	for (int i=0; i<NumSolverMipIntParams; i++)
	    lp->mipmodel->setIntParam(cbc_iparam[i], def->mipmodel->getIntParam(cbc_iparam[i]));
	for (int i=0; i<NumSolverMipDblParams; i++)
	    lp->mipmodel->setDblParam(cbc_dparam[i], def->mipmodel->getDblParam(cbc_dparam[i]));
	for (int i=0; i<NumSolverLpDblParams; i++) {
	  double value;
	  def->Solver->getModelPtr()->getDblParam(clp_dparam[i], value);
	  lp->Solver->getModelPtr()->setDblParam(clp_dparam[i], value);
	}

	for (int i=0; i<EpxClpParam_ns; i++) lp->sparam[i] = def->sparam[i];
	for (int i=0; i<EpxClpParam_ni; i++) lp->iparam[i] = def->iparam[i];
    } else {
      // initialise the defaults for eplex params 
# ifdef UFL_BARRIER
      lp->sparam[EpxClpParam_bar_ordering] = "uflamd";
# else
      lp->sparam[EpxClpParam_bar_ordering] = "native";
# endif
      lp->iparam[EpxClpParam_print_freq] = lp->mipmodel->printFrequency();
      lp->iparam[EpxClpParam_loglevel] = 1;
      lp->iparam[EpxClpParam_mip_lploglevel] = 0;
      lp->iparam[EpxClpParam_doKKT] = 0;
    }
    lp->timeout = -1; // no timeouts

    lp->Solver->passInMessageHandler(coinMessageHandler);
    lp->Solver->messageHandler()->setLogLevel(lp->iparam[EpxClpParam_loglevel]);
#else
    if (def) 
    {// this should copy the parameters from def to lp, but it does not
     // seem to work, so should add specific code to copy params 
	lp->Solver->copyParameters(*def->Solver);
    }

    lp->Solver->passInMessageHandler(coinMessageHandler);
    lp->Solver->messageHandler()->setLogLevel(1);
#endif

    coin_set_solver_outputs(lp->Solver);

    return 0;
}

extern "C"
int coin_get_dual_infeas(COINprob* lp, int* infeas)
{
#ifdef COIN_USE_CLP
    ClpSimplex* simplex = dynamic_cast<ClpSimplex*>( lp->Solver->getModelPtr());
    if (simplex == NULL) return -1;
    *infeas = simplex->numberDualInfeasibilities();
#endif
    return 0;
}

extern "C"
int coin_get_primal_infeas(COINprob* lp, int* infeas)
{
#ifdef COIN_USE_CLP
    ClpSimplex* simplex = dynamic_cast<ClpSimplex*>( lp->Solver->getModelPtr());
    if (simplex == NULL) return -1;
    *infeas = simplex->numberPrimalInfeasibilities();
#endif
    return 0;
}

extern "C"
int coin_bar_is_dual_feas(COINprob* lp)
{
#ifdef COIN_USE_CLP
    if (lp->interiormodel != NULL)
    {
	return lp->interiormodel->dualFeasible();
    }
#endif
    return 0;
}

extern "C"
int coin_bar_is_primal_feas(COINprob* lp)
{
#ifdef COIN_USE_CLP
    if (lp->interiormodel != NULL)
    {
	return lp->interiormodel->primalFeasible();
    }
#endif
    return 0;
}

extern "C"
int coin_reset_prob(lp_desc* lpd)
{
    // CBC modifies the problem state (including column bounds), and this 
    // needs to be reset before we can solve the problem again
#ifdef COIN_USE_CBC
    if (lpd->prob_type == PROBLEM_MIP)
    {
	CbcModel*  model = lpd->lp->Solver->getModelPtr();
	model->resetToReferenceSolver();
    }
#endif
    return 0;
}

extern "C"
int coin_writeprob(COINprob* lp, const char* file, char* otype)
{

    try
    {
	if (strcmp(otype, "lp") == 0) 
	{
	    //	    lp->Solver->writeLp(file, "", 1e-5, 10, 10, lp->Solver->getObjSense());
	    lp->Solver->writeLpNative(file, NULL, lp->varnames, 1e-5, 10, 10, 
				      lp->Solver->getObjSense());
	}
	else if (strcmp(otype, "mps") == 0)
	{
	    //	    lp->Solver->writeMps(file, "", lp->Solver->getObjSense());
	    lp->Solver->writeMpsNative(file, NULL, 
				       const_cast<const char**>(lp->varnames), 1, 2, 
				       lp->Solver->getObjSense());
	}
	else return -1;
    }
    catch (CoinError e)
    {
	coin_error_handler(e);
	return -1;
    }
    return 0;
}

bool coin_read_prob_file(OsiXxxSolverInterface* Solver, 
			 const char* file,
			 const char* ext,
			 int format)
{
    char* file1 = new char[strlen(file)+strlen(ext)+1];
    int err = 0;
    try
    {
	strcpy(file1, file);
	strcat(file1, ext);
	// check for file existance as exit() is called if there is anything
	// wrong with the file!
	if (!fileExists(file1)) 
	{
	    delete [] file1;
	    return false;
	}
	switch (format)
	{
	case 1: // LP
	    err = Solver->readLp(file1);
	    break;
	case 2: // MPS
	    err = Solver->readMps(file1,"");
	    break;
	}
	delete [] file1;
	return (err ? false : true);
    }
    catch (CoinError e)
    {
	delete [] file1;
	coin_error_handler(e);
	return false;
    }
}

extern "C"
int coin_readprob(COINprob* lp, const char* file, char* otype)
{

    if (strcmp(otype, "lp") == 0) 
    {
	if (coin_read_prob_file(lp->Solver, file, "", 1)) return 0;
	else if (coin_read_prob_file(lp->Solver, file, ".lp", 1)) return 0;
	else return -1;
    }
    else if (strcmp(otype, "mps") == 0)
    {
	if (coin_read_prob_file(lp->Solver, file, "", 2)) return 0;
	else if (coin_read_prob_file(lp->Solver, file, ".mps", 2)) return 0;
	else if (coin_read_prob_file(lp->Solver, file, ".mat", 2)) return 0;
	else return -1;
    }
    else return -1;
}


extern "C"
int coin_set_name(COINprob* lp, char ntype, int idx, const char * name)
{

    if (ntype == 'c')
    {
	int nc = lp->Solver->getNumCols();
	if (lp->vnsize < nc)
	{
	    int newvnsize = (int) ceil(nc*1.5)+100;
	    lp->varnames = (char**) realloc(lp->varnames, newvnsize*sizeof(char**));
	    for (int i=lp->vnsize; i < newvnsize; i++) 
	    {
		// adapted from write.c's _int_to_string()
		int number = i, pos = 0;
		do
		{
		    ++pos;
		    number /= 10;
		} while (number);
		pos += 1; // leading 'x' and terminating '\0'
		lp->varnames[i] = new char[pos+1];
		/* use x as default varname -- not valid var name in ECLiPSE,
                   so cannot conflict with user supplied var names */
		lp->varnames[i][0] = 'x'; 
		lp->varnames[i][pos--] = '\0';
		number = i;
		do
		{
		    int ch = number % 10;
		    lp->varnames[i][pos--] = ch + '0';
		    number /= 10;
		} while (number);

	    }

	    lp->vnsize = newvnsize;
	}
	if (idx < 0 || idx >= nc)
	    return -1;
	delete lp->varnames[idx]; // get rid of old name
	lp->varnames[idx] = new char[strlen(name)+1]; // +1 for terminating \0
	strcpy(lp->varnames[idx], name);
    }
    else return -1; // row names not supported 
    return 0;
}


extern "C"
int coin_add_sos(COINprob* lp, int nsos, int nsosnz, char* sostype, 
		  int* sosbeg, int* sosind, double* soswt)
{
#ifdef COIN_USE_CLP
    int new_nsos = lp->nsos + nsos;

    try {
      if (new_nsos > MIPOBJSZ)
	  throw new bad_alloc;
      for (int i=0; i<nsos-1; i++) {
	lp->mipobjects[lp->nsos+i] = new CbcSOS(lp->mipmodel, sosbeg[i+1]-sosbeg[i], 
				       &sosind[sosbeg[i]], &soswt[i], i, 
				       (sostype[i] == '1' ? 1 : 2));
      }
      if (nsos > 0) {// last set
	int i = nsos - 1;
	lp->mipobjects[lp->nsos+i] = new CbcSOS(lp->mipmodel, nsosnz-sosbeg[i], 
				       &sosind[sosbeg[i]], &soswt[i], i, 
				       (sostype[i] == '1' ? 1 : 2));
      }
    }
    catch (CoinError e) {
      coin_error_handler(e);
      return -1;
    }
    catch (bad_alloc&) {
      eclipse_out(ErrType, "Memory allocation error in external solver\n");
      return -1;
    }

    lp->nsos = new_nsos;
    return 0;
#else

    // unimplemented
    return -1;
#endif
}


extern "C"
int coin_del_sos(COINprob* lp, int from, int to)
{
#ifdef COIN_USE_CLP
    if (from > to || to > lp->nsos)
	return -1;
    int ndel = to-from;
    for (int i=to; i<lp->nsos; i++) {
	lp->mipobjects[i-ndel] = lp->mipobjects[i];
    }
    lp->nsos -= ndel;
    return 0;
#else

    // unimplemented
    return -1;
#endif
}


extern "C"
int coin_free_prob(COINprob* lp)
{
    if (lp == NULL) return 0;
    if (lp->varnames != NULL)
    {
	for (int i=0; i < lp->vnsize; i++) 
	    delete lp->varnames[i];
	free(lp->varnames);
    }
    
    delete lp->Solver->messageHandler();
    /* solver specific stuff */
#ifdef COIN_USE_CLP
    if (!lp->mipIsShared) coin_free_solver_handlers(lp->Solver);

    if (lp->nsos > 0)
    {
	for (int i=0; i<lp->nsos; i++) delete lp->mipobjects[i];
	delete [] lp->mipobjects;
    }

    if (!lp->mipIsShared) 
    {
	delete lp->Solver;
	//    delete lp->mipmodel->messageHandler();
    }
    delete lp->mipmodel;

    if (lp->interiormodel != NULL) delete lp->interiormodel;
#endif

    delete lp;
    lp = NULL;

    return 0;
}

extern "C"
void coin_get_solver_info(char* info)
{
#ifdef COIN_USE_CLP
# ifdef UFL_BARRIER
    strcpy(info, "clp(uflamd)-cbc");
# else
    strcpy(info, "clp-cbc");
# endif
#endif

#ifdef COIN_USE_CBC
    strcpy(info, "cbc-clp");
#endif

#ifdef COIN_USE_SYM
    strcpy(info, "symphony");
#endif

#ifdef COIN_USE_GLPK
    strcpy(info, "glpk");
#endif

}

