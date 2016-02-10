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
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
 * Examples for ECLiPSe C++ externals, from the User Manual
 *
 * $Id: eg_cc_external.cc,v 1.1 2008/06/30 17:43:54 jschimpf Exp $
 *
 */

#include "eclipseclass.h"

extern "C" int
p_isvar()
{
    return EC_arg(1).is_var();
}

extern "C" int
p_comp()
{
    return unify(EC_arg(1),
	/*
    	compare(EC_arg(2),EC_arg(3)) < 0 ? EC_atom("<") :
    	compare(EC_arg(2),EC_arg(3)) > 0 ? EC_atom(">") :
	EC_atom("="));
	*/
    	EC_arg(2) == EC_arg(3) ? EC_atom("==") :
	EC_atom("\\="));
}

extern "C" int
p_eq()
{
    return EC_arg(1) == EC_arg(2) ? EC_succeed : EC_fail;
}


extern "C" int
p_string_to_list()
{
    EC_word  the_string(EC_arg(1));
    EC_word  the_list(nil());
    char *s;
    long len;
    int res;

    res = the_string.is_string( &s );
    if (res != EC_succeed) return res;
    len = strlen(s);

    /* the list is built backwards */
    while (len--)
    {
	the_list = list(EC_word(s[len]), the_list);
    }
    return unify(EC_word(EC_arg(2)), the_list);
}


extern "C" int
p_sumlist()
{
    int res;
    long x, sum = 0;
    EC_word list(EC_arg(1));
    EC_word car,cdr;

    for ( ; list.is_list(car,cdr) == EC_succeed; list = cdr)
    {
	res = car.is_long( &x);
	if (res != EC_succeed) return res;
	sum += x;
    }
    res = list.is_nil();
    if (res != EC_succeed) return res;
    return unify(EC_arg(2), EC_word(sum));
}

