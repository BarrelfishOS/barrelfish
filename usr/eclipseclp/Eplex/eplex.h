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
 * Copyright (C) 1997-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): Joachim Schimpf, Kish Shen and Andrew Eremin, IC-Parc
 * 
 * END LICENSE BLOCK */

#define PROBLEM_LP	0	/* these correspond to cplex_problem_code/2 */
#define PROBLEM_MIP	1
#define PROBLEM_QP	2
#define PROBLEM_MIQP    3 
#define PROBLEM_FIXEDL	4
#define PROBLEM_FIXEDQ	5 
#define PROBLEM_RELAXEDL  6
#define PROBLEM_RELAXEDQ  7

#define METHOD_DEFAULT	(-1)	/* correspond to cplex_crossover_codes/3 */
#define METHOD_AUTO	0
#define METHOD_PRIMAL	1
#define METHOD_DUAL	2
#define METHOD_NET      3
#define METHOD_BAR      4
#define METHOD_SIFT     5
#define METHOD_CONCURRENT	6
#define METHOD_CONCURRENTDET	7
#define METHOD_NONE    8

#define SENSE_MIN	1	/* these correspond to cplex_objsense/2 */
#define SENSE_MAX	(-1)

#define IsMIPProb(t) ((t) == PROBLEM_MIP || (t) == PROBLEM_MIQP)
#define IsQPProb(t)  ((t) == PROBLEM_QP || (t) == PROBLEM_MIQP)

 
#ifdef SOLVE_MIP_COPY
# define XP_COPYOFF     0    /* do not make copy of problem */
# define XP_COPYINVALID 1    /* copy of problem not valid */
# define XP_COPYCURRENT 2    /* copy of problem is current */
# define Mark_Copy_As_Modified(lpd)   \
      if (lpd->copystatus != XP_COPYOFF) lpd->copystatus = XP_COPYINVALID
#else
# define Mark_Copy_As_Modified(lpd) {}
#endif

#if defined(NOECLIPSE) 
/* compiling for logged code */
 
# ifdef XPRESS
#  ifdef __STDC__
#   define __ANSIC_	/* used in xpresso.h */
#  endif
#  include "xprs.h"    
#  include <stdio.h>
#  include <string.h>
#  define CPXLPptr XPRSprob
# endif /* XPRESS */

# ifdef CPLEX
#  include "cplex.h"    
#  include <stdio.h>
#  include <string.h>
# endif /* CPLEX */

# ifdef C_TO_COIN
#  define COINprob void
# endif /* COIN */

#endif /* NOECLIPSE */


/* position of stream types in solver_stream[].
   (for XPRESS, = message type - 1 */
#define LogType		0   /* log */
#define ResType		1   /* result */
#define WrnType		2   /* warning */
#define ErrType		3   /* error */


/*
 * Our extended problem descriptor
 */


typedef struct {

    int		descr_state;	/* status DESCR_xxx */

    CPXLPptr	lp;		/* CPLEX/XPRESS handle */
    CPXLPptr    lpcopy;         /* copy of problem to allow MIP modifications */

#ifdef SOLVE_MIP_COPY
    int         copystatus;     /* status of copy of problem */
#endif
#ifdef USE_PROBLEM_ARRAY
    int		matno;		/* XPRESS matrix number if descriptor not
                                   current (or matrix number for logging) */
#endif

    /* here we collect the input parameters for cplex */
    int		prob_type;
    int		presolve;       /* presolve for problem */
    int		sense;		/* objective sense min=1, max=-1 */
    int		mac;		/* number of valid colunms */
    int		macadded;	/* columns already added to solver */
    int         start_mac;      /* initial number of valid columns */
    int		mar;		/* number of valid rows */
    int		nidc;		/* number of indicator constraints */

    int		matnz;		/* used nonzeros in column-wise nz arrays */
    int		macsz;          /* size of column buffer arrays */
    int		marsz;
    double	*rhsx;		/* rhs coeff. initial matrix + added rows */
    char	*senx;		/* constraint sense initial matrix + added rows */
    				/* CAUTION: uses external solver's encoding! */
    /* column-wise representation of matrix coefficients
       initial matrix + added columns
    */
    int		*matbeg;	/* index into matind for each column */
    int		*matcnt;	/* non-zeros count in each column */
    int		*matind;	/* non-zeros row indicies  */
    double	*matval;	/* non-zeros coefficient  */

    /* column information initial matrix + added columns */
    double	*bdl;		/* [macsz growable] */
    double	*bdu;		/* [macsz growable] */
    char        dirtybdflag;    /* bit-wise flag to indicate "dirty" bound arrays */
    char	*ctype;		/* column type */
    double	*objx;		/* objective coefficients */
    char	*qgtype;	/* [ngents] types of non-continuous vars */
    int		*mgcols;	/* [ngents] col-nos of non-continuous vars */
#ifdef XPRESS
    char        *probname;      /* problem name (for XPRESS files) */
    int		ngents;		/* number of non-continuous vars */
#endif

    int		nsos;		/* number of SOSs */
    int		nsos_added;	/* number of SOSs already in solver */
    int		sossz;		/* size of sos arrays */
    int		sosnzsz;	/* size of sos member arrays */
    int		nsosnz;		/* total number of sos members */
    sostype_t	*sostype;	/* [sosnzsz] sos type 1 or 2 */
    int		*sosbeg;	/* [sossz] index into sosind[] */
    int		*sosind;	/* [sosnzsz] col-no */
    double	*sosref;	/* [sosnzsz] */

    double	*rngval;
    char	**cname;
    char	*cstore;
    char	**rname;
    char	*rstore;
    unsigned	cstorsz;
    unsigned	rstorsz;

    /* A set of growable arrays, used as temporary storage for
     * - setting up the quadratic objective coefficients
     * - modifying linear objective coefficients
     * - modifying quadratic objective coefficients
     * These all use a sparse representation for columns
     */
    int		cb_sz;		/* size of cb_... arrays */
    int		cb_cnt;		/* next index to be used */
    int		*cb_index;	/* [cb_sz] */
    int		*cb_index2;	/* [cb_sz] for quad obj setup only (may be NULL) */
    double	*cb_value;	/* [cb_sz] coeff value */

    /* to add constraints (and retrieve constraints) */
    int		nr_sz;		/* size of new row arrays */
    int		nnz_sz;		/* size of new nonzero arrays */
    int		nr;		/* next new row index */
    int		nnz;		/* next new nonzero index */
    int		*rmatbeg;	/* [nr_sz]  nr_sz > nr! */
    int		*rmatind;	/* [nnz_sz] */
    double	*rmatval;	/* [nnz_sz] */
    char	*rcompl;	/* [nr_sz] indicator constraints only */
    int		*rindind;	/* [nr_sz] indicator constraints only */

    int		numsz;		/* auxiliary array for delrows/delcols */
    int		*numbers;	/* [numsz], contains numbers 0..numsz-1 */
    int		*zeroes;	/* [numsz], contains int zeroes */
    double      *dzeroes;       /* [numsz], contains double zeroes */

    /* cutpools - *2 variants are conditional */
/*    int		cp_nr_sz;	/* size of cutpool row arrays */
    int		cp_nr_sz2;	/* size of cond cutpool row arrays */
/*    int		cp_nz_sz;	/* size of cutpool nonzero arrays */
    int		cp_nz_sz2;	/* size of cond cutpool nonzero arrays */
/*    int		cp_nr;		/* next cutpool row index */
    int		cp_nr2;		/* next cond cutpool row index */
/*    int		cp_nnz;		/* next cuypool nonzero index */
    int		cp_nnz2;	/* next cond cutpool nonzero index */
    int         cp_nact2;       /* no. of active constraints in last solve */
/*    int		*cp_rmatbeg;	/* [cp_nr_sz] */
    int		*cp_rmatbeg2;	/* [cp_nr_sz2] */
/*    int		*cp_rmatind;	/* [cp_nz_sz] */
    int		*cp_rmatind2;	/* [cp_nz_sz2] */
/*    double	*cp_rmatval;	/* [cp_nz_sz] */
    double	*cp_rmatval2;	/* [cp_nz_sz2] */
/*    double	*cp_rhsx;	/* [cp_nr_sz] cutpool rhs coeffs */
    double	*cp_rhsx2;	/* [cp_nr_sz2] cond  cutpool rhs coeffs */
/*    char	*cp_senx;	/* [cp_nr_sz] cutpool constraint */
    char	*cp_senx2;	/* [cp_nr_sz2] cond cutpool constraint */
    				/* CAUTION: uses external solver's encoding! */
    char        *cp_active2;    /* [cp_nr_sz2] default state */
    char        *cp_initial_add2;  /* [cp_nr_sz2] added initially or not */
    /* named conditional cutpools */
    int         cp_npools2;
    int         cp_npools_sz2;
    int         *cp_pools_max2; /* [cp_npools_sz2] max cstr in each pool */
    int         *cp_pools_sz2;  /* [cp_npools_sz2] size for each pool */
    int         **cp_pools2;    /* [cp_npools_sz2] of int arrays of idxs */
    char        **cp_names2;    /* [cp_npools_sz2] of cutpool names */

    double      objval;         /* the most recent objective value */
    int		sol_state;	/* the most recent return status */

    int		mipstart_dirty;	/* we did set mipstart values */

    /* statistics for most recent solver call */
    int         sol_itcnt;
    int         sol_nodnum;

    /* (global) statistics */
    int		optimum_ctr;
    int		infeas_ctr;
    int		abort_ctr;
} lp_desc;


/*
 * Parameter descriptor array type
 */

struct param_desc {
    char *	name;
    param_id_t	num;
    int		type;
};


/*
 * Solver method information
 */

struct lp_meth {
    int meth;
    int auxmeth;
    int node_meth;
    int node_auxmeth;
#ifdef XPRESS
    char *meth_string;
    char *node_meth_string;
#endif
    int option_mipstart;
};


/*
 * Container for problem solutions
 */

struct lp_sol {
    double *oldsols;	/* [oldmac] */
    double *sols;	/* [mac] */
    double *slacks;
    double *pis;
    double *djs;	/* [mac] */
    int    *cbase;	/* [mac] */
    int    *rbase;
    int    oldmac;
    int    mac;
};

 
/* NOECLIPSE is defined if compiling the logged calls for bug reporting. 
   The calls are compiled without using ECLiPSe
*/

#ifdef NOECLIPSE
lp_desc *lpd;
lp_desc *lpdmat[400];  /* change size if more lpd used */

double objval, bestbound, worstbound;
int res,err;

# ifdef XPRESS
struct lp_sol sol;
 
void XPRS_CC
_get_xpress_sol(lp, solution)
XPRSprob lp;
void *solution;
{
    struct lp_sol *sol = (struct lp_sol *) solution;

    printf("Getting solution....\n");
    XPRSgetsol(lp, sol->sols, sol->slacks, sol->pis, sol->djs);
    printf("Gotten solution....\n");
/*    if (sol->base != NULL)
	XPRSgetpresolvebasis(lp, sol->base, sol->base+sol->mac);
*/
}

XPRSprob cpx_env;

# endif /* XPRESS */
#endif /* NOECLIPSE */
