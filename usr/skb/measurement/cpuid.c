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
#include "datagatherer.h"


/******************************************************************************/
//main function to be called
/******************************************************************************/
void gather_cpuid_data(coreid_t core_id)
{

    cpuid_init();

    cpuid_vendor();

    char buf[CPUID_PROC_NAME_LENGTH+1];
    cpuid_proc_name(buf, CPUID_PROC_NAME_LENGTH + 1);

    struct cpuid_proc_family family;

    cpuid_proc_family(&family);
    char *vendor_string = cpuid_vendor_string();

    skb_add_fact("vendor(%hhu,%s).", core_id, vendor_string);
    skb_add_fact("cpu_family(%"PRIuCOREID", %s, %"PRIu16", %"PRIu16", %"PRIu8").",
                 core_id, vendor_string, family.family, family.model, family.stepping);

    struct cpuid_threadinfo ti;
    cpuid_thread_info(&ti);

    //cpu_thread(BF_CORE, packageID, coreID, threadID).
    skb_add_fact("cpu_thread(%u, %u, %u, %u).", core_id,
                 ti.package, ti.core, ti.hyperthread);

    uint8_t i = 0;
    struct cpuid_cacheinfo ci;
    while(cpuid_cache_info(&ci, i) == SYS_ERR_OK) {
        skb_add_fact("cpu_cache(%"PRIuCOREID", %s, %"PRIu8", %s, %"PRIu64
                     ", %"PRIu16", %"PRIu16", %"PRIu8", %"PRIu8").",
                     core_id, ci.name, ci.level, cpuid_cache_type_string(ci.type),
                     ci.size, ci.associativity, ci.linesize, ci.shared,
                     ci.inclusive);
        i++;
    }

    i = 0;
    struct cpuid_topologyinfo topo;
    while(cpuid_topology_info(&topo, i) == SYS_ERR_OK) {
        i++;
    }


    i = 0;
    struct cpuid_tlbinfo tlbi;
    while(cpuid_tlb_info(&tlbi, i) == SYS_ERR_OK) {
        skb_add_fact("cpu_tlb(%"PRIuCOREID", %s, %"PRIu8 ", %"PRIu32 ", %"PRIu32
                     ", %"PRIu32 ").",
                     core_id, cpuid_cache_type_string(tlbi.type), tlbi.level,
                     tlbi.pagesize, tlbi.entries,tlbi.associativity);
        i++;
    }

    struct cpuid_adressspaceinfo ai;
    cpuid_address_space_info(&ai);
    skb_add_fact("cpu_addrspace(%" PRIuCOREID ", %" PRIu8 ", %"PRIu32 ", %"PRIu32").",
                    core_id, ai.physical, ai.virtual, ai.guest_physical);


}
