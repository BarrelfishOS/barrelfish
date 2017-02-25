/**
 * \file
 * \brief Boot driver arch specific parts for ARM CPUs
 */
/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */



#include <barrelfish/barrelfish.h>
#include <barrelfish_kpi/paging_arch.h>
#include <barrelfish_kpi/platform.h>
#include <barrelfish/syscall_arch.h>
#include <target/arm/barrelfish_kpi/arm_core_data.h>

#include <hw_records_arch.h>

#include <skb/skb.h>


#include "../../coreboot.h"

#define ARMV8_MONITOR_NAME "/" BF_BINARY_PREFIX "armv8/sbin/monitor"


errval_t get_architecture_config(enum cpu_type type,
                                 genpaddr_t *arch_page_size,
                                 const char **monitor_binary,
                                 const char **cpu_binary)
{
    errval_t err;

    struct monitor_blocking_rpc_client *m = get_monitor_blocking_rpc_client();
    assert(m != NULL);

    uint32_t arch, platform;
    err = m->vtbl.get_platform(m, &arch, &platform);
    if (err_is_fail(err)) {
        return err;
    }
    assert(arch == PI_ARCH_ARMV8A);

    switch(platform) {
    case PI_PLATFORM_FVP:
        *cpu_binary = "/" BF_BINARY_PREFIX "armv8/sbin/cpu_a53v";
        break;
    case PI_PLATFORM_APM88XXXX:
        *cpu_binary = "/" BF_BINARY_PREFIX "armv8/sbin/cpu_apm88xxxx";
        break;
    case PI_PLATFORM_CN88XX:
        *cpu_binary = "/" BF_BINARY_PREFIX "armv8/sbin/cpu_cn88xx";
        break;
    default:
        return SPAWN_ERR_UNKNOWN_TARGET_ARCH;
    }

    *monitor_binary = ARMV8_MONITOR_NAME;
    *arch_page_size = BASE_PAGE_SIZE;

    return SYS_ERR_OK;
}


errval_t spawn_xcore_monitor(coreid_t coreid, int hwid, 
                             enum cpu_type cpu_type,
                             const char *cmdline,
                             struct frame_identity urpc_frame_id,
                             struct capref kcb)
{
    const char *monitorname = NULL, *cpuname = NULL;
    genpaddr_t arch_page_size;
    errval_t err;

    err = get_architecture_config(cpu_type, &arch_page_size,
                                  &monitorname, &cpuname);

    DEBUG("loading kernel: %s\n", cpuname);
    DEBUG("loading 1st app: %s\n", monitorname);

    DEBUG("%s:%s:%d: urpc_frame_id.base=%"PRIxGENPADDR"\n",
           __FILE__, __FUNCTION__, __LINE__, urpc_frame_id.base);
    DEBUG("%s:%s:%d: urpc_frame_id.size=0x%" PRIuGENSIZE "\n",
           __FILE__, __FUNCTION__, __LINE__, urpc_frame_id.bytes);

    static size_t cpu_binary_size;
    static lvaddr_t cpu_binary = 0;
    static genpaddr_t cpu_binary_phys;
    static const char* cached_cpuname = NULL;
    if (cpu_binary == 0) {
        cached_cpuname = cpuname;
        // XXX: Caching these for now, until we have unmap
        err = lookup_module(cpuname, &cpu_binary, &cpu_binary_phys,
                            &cpu_binary_size);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Can not lookup module");
            return err;
        }
    }
    // Ensure caching actually works and we're
    // always loading same binary. If this starts to fail, get rid of caching.
    assert (strcmp(cached_cpuname, cpuname) == 0);

    static size_t monitor_binary_size;
    static lvaddr_t monitor_binary = 0;
    static genpaddr_t monitor_binary_phys;
    static const char* cached_monitorname = NULL;
    if (monitor_binary == 0) {
        cached_monitorname = monitorname;
        // XXX: Caching these for now, until we have unmap
        err = lookup_module(monitorname, &monitor_binary,
                            &monitor_binary_phys, &monitor_binary_size);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Can not lookup module");
            return err;
        }
    }

    debug_printf("WARNING: spawn_xcore_monitor currently not implemented!");
    return LIB_ERR_NOT_IMPLEMENTED;
}

errval_t get_core_info(coreid_t core_id, archid_t* hw_id, enum cpu_type* cpu_type)
{
    char* record = NULL;
    errval_t err = oct_get(&record, "hw.processor.%"PRIuCOREID"", core_id);
    if (err_is_fail(err)) {
        goto out;
    }

    /* XXX: figure out which fields are required */
    uint64_t apic, enabled, type;
    err = oct_read(record, "_ { apic_id: %d, enabled: %d, type: %d}",
                   &apic, &enabled, &type);
    assert (enabled);
    if (err_is_fail(err)) {
        goto out;
    }

    *hw_id = (archid_t) apic;
    *cpu_type = (enum cpu_type) type;
out:
    return err;
}
