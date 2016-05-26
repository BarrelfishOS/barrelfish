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
 * Copyright (C) 2001 - 2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): ___________________________________. 
 * 
 * END LICENSE BLOCK
 *
 * ECLiPSe LIBRARY MODULE
 *
 * $Id: bip_pt.c,v 1.1 2006/09/23 01:53:18 snovello Exp $
 *
 *
 * IDENTIFICATION:	bip_pt.c
 *
 * AUTHOR:		Joachim Schimpf, IC-Parc
 *
 * CONTENTS:
 *
 *  licence_checkout(+Feature, +Policy, +Version, +Path, -Message, -Status)
 *  licence_heartbeat(+Feature, +Minutes, -Reconnects, -FailedReconnects)
 *  licence_checkin(+Feature)
 *  licence_held(+Feature)
 *
 * DESCRIPTION:
 *
 *  Interface to FLEXlm licence manager software
 *
 */


#define EC_EXTERNAL

#ifdef _WIN32
#include <windows.h>
#define Winapi WINAPI
#else
#define Winapi
#endif

#ifdef _WIN32
#  include 	<stdlib.h>
#  define MAX_PATH_LEN	_MAX_PATH
#else
#ifdef PATH_IN_LIMITS
#  include 	<limits.h>
#  define MAX_PATH_LEN	PATH_MAX
#else
#  include <sys/param.h>
#  define MAX_PATH_LEN	MAXPATHLEN
#endif
#endif


#include "external.h"

int DLLEXP pteclipse_init(int flags);

#ifdef HAVE_FLEXLM

#undef PERROR	/* lmpolicy.h contains a redefinition  of this name */

#include "lmpolicy.h"


/* The handle for the checked out 'eclipse' feature */

static LP_HANDLE *eclipse_lp_handle = 0;


/* A list of all features checked out with licence_checkout/6 */

struct checked_out_feature {
    dident	feature;
    dident	version;
    int		policy;
    LP_HANDLE	*lp_handle;
    struct checked_out_feature *next;
};

static struct checked_out_feature *
		checked_out_features = 0;


#define MAX_FLEXLM_POLICIES	7

static dident	lm_policy_name[MAX_FLEXLM_POLICIES];
static int	lm_policy_flag[MAX_FLEXLM_POLICIES];

static dident	d_ok, d_warning;


#define Get_Name_Did(v, t, d) \
    if (IsRef(t)) { Bip_Error(INSTANTIATION_FAULT); } \
    else if (IsAtom(t)) { d = (v).did; } \
    else if (IsString(t)) { d = ec_did(StringStart(v),0); } \
    else { Bip_Error(TYPE_ERROR); }



static int
p_licence_checkout(value vfeature, type tfeature, value vpol, type tpol, value vversion, type tversion, value vlicloc, type tlicloc, value vmsg, type tmsg, value vstat, type tstat)
{
    LP_HANDLE *lp_handle;
    char lic_path[MAX_PATH_LEN];
    char *licloc;
    dident feature, version;
    struct checked_out_feature *cof;
    int policy = LM_MANUAL_HEARTBEAT;
    Prepare_Requests;

    Get_Name_Did(vfeature, tfeature, feature);
    Get_Name_Did(vversion, tversion, version);
    Get_Name(vlicloc, tlicloc, licloc);
    Check_List(tpol);
    Check_Ref(tmsg);
    Check_Ref(tstat);

#ifdef REQUIRE_LICENCE
    /* do a heartbeat for the 'eclipse' feature */
    if (!eclipse_lp_handle || lp_heartbeat(eclipse_lp_handle, 0, 0))
    {
	pword pw;
	Make_String(&pw, "ECLiPSe licence check failed\n");
	Request_Unify_Pw(vmsg, tmsg, pw.val, pw.tag);
	Request_Unify_Atom(vstat, tstat, d_.err);
	Return_Unify;
    }
#endif

    /* decode the policy option list */
    if (IsList(tpol))
    {
	pword *car;
	for(car=vpol.ptr;;)
	{
	    int i;
	    pword *cdr = car+1;
	    Dereference_(car);
	    Check_Atom(car->tag);
	    for(i=0; i<MAX_FLEXLM_POLICIES; ++i)
	    {
		if (lm_policy_name[i] == car->val.did)
		    policy |= lm_policy_flag[i];
	    }
	    Dereference_(cdr);
	    if (IsList(cdr->tag))
		car = cdr->val.ptr;
	    else if IsNil(cdr->tag)
		break;
	    else
		{ Bip_Error(TYPE_ERROR); }
	}
    }
    else
    {
    	policy |= LM_RESTRICTIVE;
    }

    /* We allow checking out multiple licences for the same feature, but only
     * if they have the same version and policy. Because all checked out
     * licences for one feature are then equivalent, we can use the feature
     * name instead of a licence handle to identify it. When checking in,
     * we just check in an arbitrary one.
     */
    for(cof=checked_out_features; cof; cof = cof->next)
    {
	if (cof->feature == feature && 
	    ! (cof->version == version && cof->policy == policy))
	{
	    pword pw;
	    Make_String(&pw, "Implementation restriction: Multiple checkout of the same\nfeature only allowed when version and policy are the same.");
	    Request_Unify_Pw(vmsg, tmsg, pw.val, pw.tag);
	    Request_Unify_Atom(vstat, tstat, d_.err);
	    Return_Unify;
	}
    }

    /* fill in the default licence path, if necessary */
    if (*licloc == '\0')
    {
	char lic_path_canonical[MAX_PATH_LEN];
	strcpy(lic_path_canonical, DidName(d_.eclipse_home));
	strcat(lic_path_canonical, "/licence.dat");
	licloc = os_filename(lic_path_canonical, lic_path);
    }

    /* check out and return the results */
    if (lp_checkout(LPCODE, policy, DidName(feature), DidName(version),
    			1, licloc, &lp_handle))
    {
	pword pw;
	char *message = lp_errstring(lp_handle);
	if (!message)		/* shouldn't happen */
	    message = "";
	Make_String(&pw, message);
	Request_Unify_Pw(vmsg, tmsg, pw.val, pw.tag);
	Request_Unify_Atom(vstat, tstat, d_.err);
    }
    else
    {
	char *message = lp_warning(lp_handle);

	/* store the feature name and the checked out handle */
	cof = (struct checked_out_feature *)
		malloc(sizeof(struct checked_out_feature));
	cof->feature = feature;
	cof->version = version;
	cof->policy = policy;
	cof->lp_handle = lp_handle;
	cof->next = checked_out_features;
	checked_out_features = cof;

	if (message)
	{
	    pword pw;
	    Make_String(&pw, message);
	    Request_Unify_Pw(vmsg, tmsg, pw.val, pw.tag);
	    Request_Unify_Atom(vstat, tstat, d_warning);
	}
	else
	{
	    Request_Unify_Atom(vstat, tstat, d_ok);
	}
    }
    Return_Unify;
}


static int
p_licence_held(value vfeature, type tfeature)
{
    dident feature;
    struct checked_out_feature *cof;
    Get_Name_Did(vfeature, tfeature, feature);

    for(cof=checked_out_features; cof; cof = cof->next)
    {
	if (cof->feature == feature)
	    { Succeed_; }
    }
    Fail_;
}


static int
p_licence_checkin(value vfeature, type tfeature)
{
    dident feature;
    struct checked_out_feature *cof, **pcof;
    Get_Name_Did(vfeature, tfeature, feature);

    pcof = &checked_out_features;
    for(cof=checked_out_features; cof; cof = cof->next)
    {
	if (cof->feature == feature)
	{
	    lp_checkin(cof->lp_handle);
	    *pcof = cof->next;
	    free(cof);
	    break;
	}
	pcof = &cof->next;
    }
    Succeed_;
}


static int
p_licence_heartbeat(value vfeature, type tfeature, value vminutes, type tminutes, value vrec, type trec, value vfrec, type tfrec)
{
    struct checked_out_feature *cof;
    dident feature;
    int rec_total = 0, frec_total = 0, found = 0;
    Prepare_Requests;

    Get_Name_Did(vfeature, tfeature, feature);
    Check_Integer(tminutes);
    for(cof=checked_out_features; cof; cof = cof->next)
    {
	if (cof->feature == feature)
	{
	    int rec;
	    found = 1;
	    frec_total += lp_heartbeat(cof->lp_handle, &rec, (int) vminutes.nint);
	    rec_total += rec;
	}
    }
    if (found)
    {
	Request_Unify_Integer(vrec, trec, rec_total);
	Request_Unify_Integer(vfrec, tfrec, frec_total);
	Return_Unify;
    }
    Fail_;
}


/*ARGSUSED*/
int
pteclipse_init(int flags)
{
#ifdef REQUIRE_LICENCE
    extern char	ec_version[];
    char lic_path[MAX_PATH_LEN];

    strcpy(lic_path, DidName(d_.eclipse_home));
    strcpy(lic_path+strlen(lic_path), "/licence.dat");
    if (lp_checkout(LPCODE, LM_RESTRICTIVE|LM_MANUAL_HEARTBEAT,
    	"eclipse", ec_version, 1, lic_path, &eclipse_lp_handle))
    {
	lp_perror(eclipse_lp_handle, "ECLiPSe");
	eclipse_lp_handle = 0;
	Fail_;
    }
#endif

    (void) ec_external(ec_did("licence_checkout", 6), p_licence_checkout, d_.kernel_sepia);
    (void) ec_external(ec_did("licence_checkin", 1), p_licence_checkin, d_.kernel_sepia);
    (void) ec_external(ec_did("licence_heartbeat", 4), p_licence_heartbeat, d_.kernel_sepia);
    (void) ec_external(ec_did("licence_held", 1), p_licence_held, d_.kernel_sepia);
    lm_policy_name[0] = ec_did("restrictive",0); lm_policy_flag[0] = LM_RESTRICTIVE;
    lm_policy_name[1] = ec_did("queue",0); lm_policy_flag[1] = LM_QUEUE;
    lm_policy_name[2] = ec_did("failsafe",0); lm_policy_flag[2] = LM_FAILSAFE;
    lm_policy_name[3] = ec_did("lenient",0); lm_policy_flag[3] = LM_LENIENT;
    lm_policy_name[4] = ec_did("retry_restrictive",0); lm_policy_flag[4] = LM_RETRY_RESTRICTIVE;
    lm_policy_name[5] = ec_did("check_baddate",0); lm_policy_flag[5] = LM_CHECK_BADDATE;
    lm_policy_name[6] = ec_did("flexlock",0); lm_policy_flag[6] = LM_FLEXLOCK;
    d_warning = ec_did("warning", 0);
    d_ok = ec_did("ok", 0);
    Succeed_;
}

#else

/*ARGSUSED*/
int
pteclipse_init(int flags)
{
    Bip_Error(UNIMPLEMENTED);
}

#endif

