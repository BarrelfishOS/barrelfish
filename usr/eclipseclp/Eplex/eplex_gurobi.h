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

#include "gurobi_c.h"

#define CPXENVptr			GRBenv*
#define CPXLPptr			GRBmodel*

typedef char* param_id_t;
typedef int sostype_t;

#include "eplex.h"	/* needs declarations above! */



/*
 * Capabilities
 */
#undef HAS_QUADRATIC
#undef HAS_MIQP
#define HAS_LIMITED_MIP_RESULTS
#define SOLVER_HAS_LOCAL_PARAMETERS
#define SOLVER_HAS_STR_PARAMS

/*
 * Constants
 */
#define SOLVER_SHORT_NAME	GRB
#define SOLVER_ATOMIC_NAME	"gurobi"
#define SOLVER_VERSION_INT	(100*GRB_VERSION_MAJOR+GRB_VERSION_MINOR)

#define CPXPUBLIC

#define CPX_INFBOUND		GRB_INFINITY

#define SOLVER_MAT_BASE		0
#define SOLVER_MAT_OFFSET	1

#define SOLVER_SENSE_LE		'<'
#define SOLVER_SENSE_GE		'>'
#define SOLVER_SENSE_EQ		'='
	
#define SOLVER_SOS_TYPE1	1
#define SOLVER_SOS_TYPE2	2

#define CPX_COL_AT_LOWER	GRB_NONBASIC_LOWER
#define CPX_COL_AT_UPPER	GRB_NONBASIC_UPPER
#define CPX_COL_BASIC		GBB_BASIC
#define CPX_COL_FREE_SUPER	GRB_SUPERBASIC

#define STRBUFFERSIZE		GRB_MAX_STRLEN

/*
 * Mappings for CPLEX-style functions
 */
#define CPXgetrhs(E,A1,A2,A3,A4)	GRBgetdblattrarray(A1,GRB_DBL_ATTR_RHS,A3,(A4)-(A3)+1,A2)
#define CPXgetsense(E,A1,A2,A3,A4)	GRBgetcharattrarray(A1,GRB_CHAR_ATTR_SENSE,A3,(A4)-(A3)+1,A2)
#define CPXgetlb(E,A1,A2,A3,A4)		GRBgetdblattrarray(A1,GRB_DBL_ATTR_LB,A3,(A4)-(A3)+1,A2)
#define CPXgetub(E,A1,A2,A3,A4)		GRBgetdblattrarray(A1,GRB_DBL_ATTR_UB,A3,(A4)-(A3)+1,A2)
#define CPXgetctype(E,A1,A2,A3,A4)	GRBgetcharattrarray(A1,GRB_CHAR_ATTR_VTYPE,A3,(A4)-(A3)+1,A2)
#define CPXchgctype(E,A1,A2,A3,A4)	GRBsetcharattrlist(A1,GRB_CHAR_ATTR_VTYPE,A2,A3,A4)
#define CPXchgbds(E,A1,A2,A3,A4,A5)	grb_chgbds(A1,A2,A3,A4,A5)
#define CPXcopybase(E,A1,A2,A3)		grb_loadbasis(A1,A2,A3)
#define CPXgetbase(E,A1,A2,A3)		grb_getbasis(A1,A2,A3)
#define CPXgetmipobjval(E,A1,A2)	GRBgetdblattr(A1,GRB_DBL_ATTR_OBJVAL,A2)
#define CPXgetobj(E,A1,A2,A3,A4)	GRBgetdblattrarray(A1,GRB_DBL_ATTR_OBJ,A3,(A4)-(A3)+1,A2)
#define CPXchgobj(E,A1,A2,A3,A4)	GRBsetdblattrlist(A1,GRB_DBL_ATTR_OBJ,A2,A3,A4)
/* branching directions ignored! */
#define CPXcopyorder(E,A1,A2,A3,A4,A5)	GRBsetintattrlist(A1,GRB_INT_ATTR_BRANCHPRIORITY,A2,A3,A4)
#define CPXchgqpcoef(E,A1,A2,A3,A4)	(-1)
#define CPXchgrhs(E,A1,A2,A3,A4)	GRBsetdblattrlist(A1,GRB_DBL_ATTR_RHS,A2,A3,A4)
#define CPXcloseCPLEX(E)		GRBfreeenv(*(E))
/* CAUTION: gurobi could set types directly here */
#define CPXaddcols(E,LP,NC,NNZ,OC,MB,MI,MV,LB,UB,CN) \
	GRBaddvars(LP,NC,NNZ,MB,MI,MV,OC,LB,UB,NULL,CN)
/* CAUTION: can only add rows, not columns, i.e. NC==0! */
#define CPXaddrows(E,LP,NC,NR,NNZ,RHS,SEN,MB,MI,MV,CN,RN) \
	GRBaddconstrs(LP,NR,NNZ,MB,MI,MV,SEN,RHS,CN)
/* CAUTION: arrays must be large enough */
#define CPXgetrows(E,A1,A2,A3,A4,A5,A6,A7,A8,A9) \
	GRBgetconstrs(A1,A2,A3,A4,A5,A8,(A9)-(A8)+1)
#define CPXchgobjsen(E,A1,A2) 		GRBsetintattr(A1,GRB_INT_ATTR_MODELSENSE,A2)
#define CPXchgprobtype(A1, A2, A3)	0 /* 0 for success return code */
#define CPXaddsos(E,LP,NS,NE,ST,SB,SI,SW,SN) GRBaddsos(LP,NS,NE,ST,SB,SI,SW)
#define XPRSdelrows(A1,A2,A3)		GRBdelconstrs(A1,A2,A3)
#define XPRSdelcols(A1,A2,A3)		GRBdelvars(A1,A2,A3)
#define CPXchgname(E,A1,A2,A3,A4)	grb_setname(A1,A2,A3,A4)

#define CPXisminimize(E,A1) 		(grb_getintattr(A1, GRB_INT_ATTR_MODELSENSE) == GRB_MINIMIZE)
#define CPXgetnumnz(E,A1)		grb_getintattr(A1, GRB_INT_ATTR_NUMNZS)
#define CPXgetnumint(E,A1)		grb_getintattr(A1, GRB_INT_ATTR_NUMINTVARS)
#define CPXgetnumbin(E,A1)		grb_getintattr(A1, GRB_INT_ATTR_NUMBINVARS)
#define CPXgetnumqpnz(E,A1)		grb_getintattr(A1, GRB_INT_ATTR_NUMQNZS)

#define CPXupdatemodel(LP)		GRBupdatemodel(LP)

/*
 * Other operations
 */
#define SetPreSolve(state)

#define Get_Int_Param(E,L,A1,A2) 	GRBgetintparam((L)?GRBgetenv((L)->lp):(E),A1,A2)
#define Get_Dbl_Param(E,L,A1,A2)	GRBgetdblparam((L)?GRBgetenv((L)->lp):(E),A1,A2)
#define Get_Str_Param(E,L,A1,A2)	GRBgetstrparam((L)?GRBgetenv((L)->lp):(E),A1,A2)
#define Set_Int_Param(E,L,A1,A2) 	GRBsetintparam((L)?GRBgetenv((L)->lp):(E),A1,A2)
#define Set_Dbl_Param(E,L,A1,A2)	GRBsetdblparam((L)?GRBgetenv((L)->lp):(E),A1,A2)
#define Set_Str_Param(E,L,A1,A2)	GRBsetstrparam((L)?GRBgetenv((L)->lp):(E),A1,A2)

#define Get_LP_Objval(A1,A2)		GRBgetdblattr((A1)->lp,GRB_DBL_ATTR_OBJVAL,A2)
#define Get_Best_Objbound(A1, A2)	GRBgetdblattr(A1,GRB_DBL_ATTR_OBJBOUND,A2)
#define Get_Feasibility_Tolerance(E,L,T) GRBgetdblparam(E,GRB_DBL_PAR_FEASIBILITYTOL,T)

#define Report_Error(Msg) \
	(void) ec_outfs(solver_streams[ErrType], "Gurobi error: "); \
	(void) ec_outfs(solver_streams[ErrType], Msg); \
	(void) ec_newline(solver_streams[ErrType]);

#define Report_Solver_Error(E)		Report_Error(GRBgeterrormsg(E))
