/**
 * \file
 * \brief Adds information from CPUID for the core it is running on to the SKB
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <stdio.h>
#include <stdlib.h>
#include <mm/mm.h>
#include <cpuid/cpuid.h>
#include <skb/skb.h>


/* XXX: removed references to old sockeye generated code undil skate has feature parity */

//#include <schema/cpuid.h>

#include "datagatherer.h"


/******************************************************************************/
//main function to be called
/******************************************************************************/
errval_t gather_cpuid_data(coreid_t core_id)
{
    errval_t err;
    err = cpuid_init();

    char buf[CPUID_PROC_NAME_LENGTH+1];
    err = cpuid_proc_name(buf, CPUID_PROC_NAME_LENGTH + 1);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "cpuid_proc_name");
        return err_push(err, SKB_DATAGATHERER_ERR_CPUID);
    }

    struct cpuid_proc_family family;

    err = cpuid_proc_family(&family);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "");
        return err_push(err, SKB_DATAGATHERER_ERR_CPUID);
    }

//    char *vendor_string = cpuid_vendor_string();

//    cpuid__vendor_t vendor;
//    vendor.Core_ID = core_id;
//    vendor.vendor = vendor_string;
    

    USER_PANIC("err = cpuid__vendor__add(&vendor);");
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "cpuid__vendor__add: %s", skb_get_error_output());
        return err_push(err, SKB_DATAGATHERER_ERR_CPUID);
    }

//    cpuid__family_t f_family;
//    f_family.Core_ID = core_id;
//    f_family.Vendor_String = vendor_string;
//    f_family.Family = family.family;
//    f_family.Model = family.model;
//    f_family.Stepping = family.stepping;

    USER_PANIC("err = cpuid__family__add(&f_family);");
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "");
        return err_push(err, SKB_DATAGATHERER_ERR_CPUID);
    }

    struct cpuid_threadinfo ti;
    err = cpuid_thread_info(&ti);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "");
        return err_push(err, SKB_DATAGATHERER_ERR_CPUID);
    }

//    cpuid__thread_t thread;
//    thread.Core_ID = core_id;
//    thread.Package = ti.package;
//    thread.Core = ti.core;
//    thread.HyperThread = ti.hyperthread;

    USER_PANIC("err = cpuid__thread__add(&thread);");
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "");
        return err_push(err, SKB_DATAGATHERER_ERR_CPUID);
    }

    uint8_t i = 0;
    struct cpuid_cacheinfo ci;
    while((err = cpuid_cache_info(&ci, i)) == SYS_ERR_OK) {

//        cpuid__cache_t cache;
//        cache.Core_ID = core_id;
//        cache.Name = ci.name;
//        cache.Level = ci.level;
//        cache.type = cpuid_cache_type_string(ci.type);
//        cache.Size = ci.size;
//        cache.Associativity = ci.associativity;
//        cache.LineSize = ci.linesize;
//        cache.Shared = ci.shared;
//        cache.Inclusive = ci.inclusive;

        USER_PANIC("err = cpuid__cache__add(&cache);");
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "");
            return err_push(err, SKB_DATAGATHERER_ERR_CPUID);
        }

        i++;
    }
    if (err_is_fail(err) && err != CPUID_ERR_INVALID_INDEX) {
        DEBUG_ERR(err, "");
        return err_push(err, SKB_DATAGATHERER_ERR_CPUID);
    }


    i = 0;
    struct cpuid_topologyinfo topo;
    while((err = cpuid_topology_info(&topo, i)) == SYS_ERR_OK) {

        // TODO Store topology in SKB

        i++;
    }
    if (err_is_fail(err) && err != CPUID_ERR_INVALID_INDEX) {
        DEBUG_ERR(err, "");
        return err_push(err, SKB_DATAGATHERER_ERR_CPUID);
    }

    i = 0;
    struct cpuid_tlbinfo tlbi;
    while((err = cpuid_tlb_info(&tlbi, i)) == SYS_ERR_OK) {

//        cpuid__tlb_t tlb;
//        tlb.Core_ID = core_id;
//        tlb.type = cpuid_cache_type_string(tlbi.type);
//        tlb.level = tlbi.level;
//        tlb.PageSize = tlbi.pagesize;
//        tlb.Entries = tlbi.entries;
//        tlb.Associativity = tlbi.associativity;

        USER_PANIC("err = cpuid__tlb__add(&tlb);");
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "");
            return err_push(err, SKB_DATAGATHERER_ERR_CPUID);
        }

        i++;
    }
    if (err_is_fail(err)) {
        switch (err) {
        case CPUID_ERR_INVALID_INDEX:
            // fall-through
        case CPUID_ERR_UNSUPPORTED_FUNCTION:
            break;
        default:
            DEBUG_ERR(err, "");
            return err_push(err, SKB_DATAGATHERER_ERR_CPUID);
        }
    }

    struct cpuid_adressspaceinfo ai;
    err = cpuid_address_space_info(&ai);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "");
        return err_push(err, SKB_DATAGATHERER_ERR_CPUID);
    }

//    cpuid__addrspace_t addrspace;
//    addrspace.Core_ID = core_id;
//    addrspace.BitsPhys = ai.physical;
//    addrspace.BitsVirt = ai.virtual;
//    addrspace.BitsGuest = ai.guest_physical;

    USER_PANIC("err = cpuid__addrspace__add(&addrspace);");
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "");
        return err_push(err, SKB_DATAGATHERER_ERR_CPUID);
    }

    return err;
}
