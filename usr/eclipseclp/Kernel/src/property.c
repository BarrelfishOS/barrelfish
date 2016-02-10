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
 * Copyright (C) 1989-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
 * VERSION	$Id: property.c,v 1.9 2013/09/28 00:25:39 jschimpf Exp $
 *
 * IDENTIFICATION:	property.c
 *
 * DESCRIPTION:		property list handling
 *
 * CONTENTS:
 *			set_property()
 *			get_property()
 *			get_simple_property()
 *			erase_property()
 *			set_modular_property()
 *			get_modular_property()
 *			erase_modular_property()
 *			erase_module_props()
 *			mark_dids_from_properties()
 *
 * AUTHOR:		bruno, joachim
 *
 * This version implements the following semantics of property lists:
 * -	There is no difference between module independent properties
 *	and module dependent global properties.
 *	Therefore the same routines can be used for both.
 * -	Independent/global properties can be created, accessed, modified
 *	and erased from everywhere. We always work on the visible property
 *	(except when a local is created it may hide a global one).
 * -	When a module is erased, its local properties are erased as well.
 */


#include "config.h"
#include "sepia.h"
#include "types.h"
#include "embed.h"
#include "error.h"
#include "mem.h"
#include "dict.h"
#include "module.h"
#include "property.h"


static void	free_prop_value(int, pword*);

extern void	mark_dids_from_array(pword *prop_value),
		mark_dids_from_pwords(pword *from, register pword *to),
		mark_dids_from_heapterm(pword *root);


#define Property_Error(err_ptr, err_no)	\
    *err_ptr = err_no;			\
    return 0;

static void
_rem_from_module_entry(property *m, module_item *pm)
{
    register property *p, **prev;
    prev = &(pm->properties);
    p = *prev;
    while (p != m)
    {
	if (!p) return;	/* should not happen, but ... */
	prev = &p->next_prop;
	p = *prev;
    }
    *prev = p->next_prop;
}

/*
 * create a new module-independent property descriptor
 */

pword *
set_property(dident functor, int property_name)
{
    int	err;
    /* the module is not used */
    return set_modular_property(functor, property_name,
			    d_.default_module, tdict, GLOBAL_PROP, &err);
}


/*
 * create a new property descriptor
 *
 * flag is one of {GLOBAL_PROP, LOCAL_PROP}.
 * the module is not important, but must de different from D_UNKNOWN.
 * If a descriptor already exists, NULL is returned, else
 * the return value is a pointer to the property value of the new descriptor.
 * A local definition hides an existing global one.
 *
 * A global descriptor is always created, even when only local properties
 * exist. It is the one in the property chain. If no global property
 * exists, its module field contains D_UNKNOWN, otherwise it holds the
 * definition module (which is not further used for globals).
 * The global descriptor is the head of a circular list of local properties.
 * The property_value field of any descriptor is initialised with a TEND tag.
 *
 * If an error occurs, nil is returned and the integer referenced by
 * err_ref is set to the error number. If the value returned is non nil,
 * it points to a valid property and *err_ref is not changed.
 * It is guaranty that err_ref will not be accessed if there is no
 * error (i.e. 0 can be passed if it shure there is no property and
 * that the module access is ok)
 *
 * Since this function returns a pointer into a property descriptor,
 * it must only be called inside an interrupt protected area !!!
 */

pword *
set_modular_property(dident functor, int property_name, dident module, type mod_tag, int flag, int *err_ref)
{
    register property	*p, *head;
    module_item		*pm;

    if (flag == LOCAL_PROP && IsLocked(module)
	&& !IsModuleTag(module, mod_tag))
    {
	Property_Error(err_ref, LOCKED);
    }

    /* get pointer to property list from atom */
    a_mutex_lock(&PropListLock);
    head = p = DidProperties(functor);

    while (p && p->name != property_name)	/* find the right one	*/
    {
	head = p;
	p = p->next_prop;
    }

    if (!p)					/* no such property yet	*/
    {
	p = (property *) hg_alloc_size(sizeof(property));
	p->name = property_name;
	p->next_prop = (property *) NULL;
	p->next_mod = p;
	p->module = D_UNKNOWN;
	if (head)
	    head->next_prop = p;
	else
	    DidProperties(functor) = p;
    }

    if (flag == GLOBAL_PROP)
    {
	if (p->module == D_UNKNOWN)
	{
	    p->module = module;			/* fill unused descriptor */
	    p->property_value.tag.kernel = TEND;
	    a_mutex_unlock(&PropListLock);
	    return &p->property_value;
	}
	else
	{
	    a_mutex_unlock(&PropListLock);
	    Property_Error(err_ref, PERROR)/* global exists already */
	}
    }

    /* else if (flag == LOCAL_PROP) */
    head = p;	
    for(p = head->next_mod; p != head; p = p->next_mod)
    {
	if (p->module == module)
	{
	    a_mutex_unlock(&PropListLock);
	    Property_Error(err_ref, PERROR); /* a local exists	*/
	}
    }

    /* insert a new descriptor at the beginning	*/
    p = (property *) hg_alloc_size(sizeof(property));
    p->name = property_name;
    p->module = module;
    p->property_value.tag.kernel = TEND;
    p->next_mod = head->next_mod;
    head->next_mod = p;
    a_mutex_unlock(&PropListLock);
    
    a_mutex_lock(&ModuleLock);
    pm = (module_item *) (get_property(module, MODULE_PROP))->val.ptr;
    p->next_prop = pm->properties;
    pm->properties = p;
    a_mutex_unlock(&ModuleLock);

    return &p->property_value;
}


/*
 * get a module independent or the global property
 */

pword * 
get_property(dident functor, int property_name)
{
    int	err;
    
    return get_modular_property(functor, property_name,
				D_UNKNOWN, tdict, GLOBAL_PROP, &err);
}


/*
 * get a property
 * flag is one of {VISIBLE_PROP, GLOBAL_PROP, LOCAL_PROP}.
 *
 * If an error occurs, nil is returned and the integer referenced by
 * err_ref is set to the error number. If the value returned is non nil,
 * it points to a valid property and *err_ref indicates which property
 * was returned (GLOBAL_PROP or LOCAL_PROP).
 *
 * Since this function returns a pointer into a property descriptor,
 * it must only be called inside an interrupt protected area !!!
 */

pword *
get_modular_property(dident functor, int property_name, dident module, type mod_tag, int which, int *res)
{
	register property	*p, *m;

	if (which != GLOBAL_PROP && IsLocked(module)
	    && !IsModuleTag(module, mod_tag))
	{
	    Property_Error(res, LOCKED);
	}

	/* scan property list until an entry for property is found or end */
	a_mutex_lock(&PropListLock);
	for (p = DidProperties(functor); p; p = p->next_prop)
	{
	    if (p->name == property_name)
	    {
		if (which != GLOBAL_PROP)
		    for (m = p->next_mod; m != p; m = m->next_mod)
		    {
			if (m->module == module) {
			    *res = LOCAL_PROP;
			    a_mutex_unlock(&PropListLock);
			    return(&m->property_value);	/* return the local */
			}
		    }

		a_mutex_unlock(&PropListLock);
		if (which != LOCAL_PROP  &&  p->module != D_UNKNOWN) {
		    *res = GLOBAL_PROP;
		    return(&p->property_value);	/* return the global */
		}
		else
		{
		    Property_Error(res, PERROR); /* no global */
		}
	    }
	}
	a_mutex_unlock(&PropListLock);
	Property_Error(res, PERROR);
}


/*
 * Quick routine to get a module-independent property.
 * Does not return a pointer into the property, therefore no lock
 * necessary around call.
 */
int
get_simple_property(dident functor, int property_name, pword *result)
{
    property	*p;

    a_mutex_lock(&PropListLock);
    for (p = DidProperties(functor); p; p = p->next_prop)
    {
	if (p->name == property_name)
	{
	    a_mutex_unlock(&PropListLock);
	    *result = p->property_value;
	    return PSUCCEED;
	}
    }
    a_mutex_unlock(&PropListLock);
    return PFAIL;
}


/*
 * erase a module independent or the global property
 */

int
erase_property(dident functor, int property_name)
{
	return erase_modular_property(functor, property_name,
				      D_UNKNOWN, tdict, GLOBAL_PROP);
}


/*
 * erase a property
 * flag is one of {VISIBLE_PROP, GLOBAL_PROP, LOCAL_PROP}.
 * This function can return a valid Prolog error code.
 * a successful erase may return PSUCCEED or PFAIL. The later
 * is return if the property has been completely removed for functor
 * i.e the global and all locals.
 */

int
erase_modular_property(dident functor, int property_name, dident module, type mod_tag, int which)
{
	register property	*p, **prev_p;
	int			res;
	module_item		*pm;

	if (which != GLOBAL_PROP && IsLocked(module)
	    && !IsModuleTag(module, mod_tag))
	{
	    return LOCKED;
	}

	/* this lookup must be before the lock */
	if (which != GLOBAL_PROP)
	    pm = (module_item *) (get_property(module, MODULE_PROP))->val.ptr;

	a_mutex_lock(&PropListLock);
	/* get pointer to property list from atom */
	prev_p = &(DidProperties(functor));
	p = *prev_p;

	/* scan property list until an entry for property is found or end */
	while (p)
	{
	    if (p->name == property_name)
	    {
		if (which != GLOBAL_PROP)
		{
		    register property	 *m, **prev_m;

		    prev_m = &(p->next_mod);
		    m = *prev_m;

		    while (m != p)	/* scan module list */
		    {
			if (m->module == module)
			{			/* erase the local	*/
			    *prev_m = m->next_mod;

			    _rem_from_module_entry(m, pm);
			    free_prop_value(property_name, &m->property_value);
			    hg_free_size((generic_ptr) m, sizeof(property));

			    if (p->next_mod == p && p->module == D_UNKNOWN)
			    {	/* all erased, remove head descriptor	*/
				*prev_p = p->next_prop;
				hg_free_size((generic_ptr) p, sizeof(property));
                              /* this is not an error, it is a message
                                 to notify that the property is erased
                                 completely */
                              res = PFAIL;
			      goto _unlock_return_;
			    }
			    res = PSUCCEED;
			    goto _unlock_return_;
			}
			prev_m = &(m->next_mod);
			m = *prev_m;
		    }
		}
		if (which != LOCAL_PROP  &&  p->module != D_UNKNOWN)
		{				/* erase the global	*/
		    free_prop_value(property_name, &p->property_value);
		    if (p->next_mod == p)
		    {		/* no locals: remove global descriptor	*/
			*prev_p = p->next_prop;
			hg_free_size((generic_ptr) p, sizeof(property));
                      /* this is not an error, it is a message to notify
                         that the property is erased completely       */
			res = PFAIL;
			goto _unlock_return_;
		    }
		    else
			p->module = D_UNKNOWN;	/* just mark it unused	*/
		    res = PSUCCEED;
		    goto _unlock_return_;
		}
		res = PERROR;
		goto _unlock_return_;		/* should give a warning */
	    }
	    prev_p = &(p->next_prop);
	    p = *prev_p;
	}
	res = PERROR;
_unlock_return_:
	a_mutex_unlock(&PropListLock);
        return(res);
}


/*
 * this is to be called from erase_module
 * prop_list is a list of module dependent (local) property descriptors
 * linked with the next_prop field
 */

void
erase_module_props(property *prop_list)
{
    register property *p;

    while(prop_list)
    {
	p = prop_list->next_mod;

	while (p->next_mod != prop_list)
	    p = p->next_mod;
	p->next_mod = prop_list->next_mod;

	p = prop_list;
	prop_list = prop_list->next_prop;
	free_prop_value((int) p->name, &p->property_value);
	hg_free_size((generic_ptr) p, sizeof(property));
    }
}


/*
 * free all space associated to the property value
 */

static void
free_prop_value(int prop_name, pword *prop_value)
{
    switch(prop_name)
    {
    case GLOBVAR_PROP:
	if (IsGlobalPrologRef(prop_value)) {
	    ec_ref_destroy((ec_ref) prop_value->val.wptr);
	    prop_value->val.wptr = NULL;
	}
	/* If we are erasing the last global ref, decrement the global index */
	else if (IsGlobalPrologRefIndex(prop_value) &&
		prop_value->val.nint == (GlobalVarIndex - 1))
	{
	    GlobalVarIndex--;
	}
	else
	{
	    free_heapterm(prop_value);
	}
	break;

    case ARRAY_PROP:
	free_array(prop_value);
	break;

    case IDB_PROP:
    {
	extern t_ext_type heap_rec_header_tid;
	heap_rec_header_tid.free((t_ext_ptr)prop_value->val.wptr);
	break;
    }

    case HTABLE_PROP:
    {
	extern t_ext_type heap_htable_tid;
	heap_htable_tid.free((t_ext_ptr)prop_value->val.wptr);
	break;
    }

    case SHELF_PROP:
    {
	extern t_ext_type heap_array_tid;
	heap_array_tid.free((t_ext_ptr)prop_value->val.wptr);
	break;
    }

    case MODULE_PROP:
    case TRANS_PROP:
    case WRITE_TRANS_PROP:
    case GOAL_TRANS_PROP:
    case WRITE_GOAL_TRANS_PROP:
    case CLAUSE_TRANS_PROP:
    case WRITE_CLAUSE_TRANS_PROP:
	hg_free((generic_ptr)prop_value->val.ptr);
	break;

    case EVENT_PROP:
    case STREAM_PROP:
    case PREFIX_PROP:
    case INFIX_PROP:
    case POSTFIX_PROP:
    case SYSCALL_PROP:
	break;

    default:
	p_fprintf(current_err_, "Unknown property type %d in free_prop_value()\n", prop_name);
	ec_flush(current_err_);
	break;
    }
}


/*
 * Support function for the dictionary garbage collector.
 * Mark all DIDs that occur in the given property list
 * (ie. treat all the properties a single functor).
 */

void
mark_dids_from_properties(property *prop_list)
{
    for (; prop_list; prop_list = prop_list->next_prop)
    {
	register property *p = prop_list;
	do
	{
	    if (p->module != D_UNKNOWN)
	    {
		switch (p->name)
		{
		case ARRAY_PROP:
		    mark_dids_from_array(&p->property_value);
		    break;

		case GLOBVAR_PROP:
		    mark_dids_from_heapterm(&p->property_value);
		    break;

		case HTABLE_PROP:
		    {
			extern t_ext_type heap_htable_tid;
			heap_htable_tid.mark_dids((t_ext_ptr)p->property_value.val.wptr);
		    }
		    break;

		case SHELF_PROP:
		    {
			extern t_ext_type heap_array_tid;
			heap_array_tid.mark_dids((t_ext_ptr)p->property_value.val.wptr);
		    }
		    break;

		case IDB_PROP:
		    {
			extern t_ext_type heap_rec_header_tid;
			heap_rec_header_tid.mark_dids((t_ext_ptr)p->property_value.val.wptr);
		    }
		    break;

		case TRANS_PROP:
		case WRITE_TRANS_PROP:
		case GOAL_TRANS_PROP:
		case WRITE_GOAL_TRANS_PROP:
		case CLAUSE_TRANS_PROP:
		case WRITE_CLAUSE_TRANS_PROP:
		    {
			macro_desc *md = (macro_desc *) p->property_value.val.ptr;
			Mark_Did(md->trans_function);
			Mark_Did(md->module);
		    }
		    break;

		case MODULE_PROP:
		    {
			module_item *m = (module_item *) p->property_value.val.ptr;
			register didlist *scan;
			for (scan = m->imports; scan; scan = scan->next)
			{
			    Mark_Did(scan->name);
			}
		    }
		    break;

		case STREAM_PROP:	/* just an integer */
		    break;

		case PREFIX_PROP:	/* did */
		case INFIX_PROP:	/* did */
		case POSTFIX_PROP:	/* did */
		case SYSCALL_PROP:	/* did or integer */
		case EVENT_PROP:	/* pri */
		    mark_dids_from_pwords(&p->property_value, &p->property_value + 1);
		    break;

		default:
		    p_fprintf(current_err_, "Unknown property type %d in mark_dids_from_properties()\n", p->name);
		    ec_flush(current_err_);
		    break;
		}
	    }
	    p = p->next_mod;
	} while (p != prop_list);
    }
}

