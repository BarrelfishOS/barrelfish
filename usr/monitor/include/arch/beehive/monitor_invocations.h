/**
 * \file
 * \brief Capability invocations specific to the monitors
 */

/*
 * Copyright (c) 2007 - 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_BEEHICE_MONITOR_INVOCATIONS_H
#define ARCH_BEEHIVE_MONITOR_INVOCATIONS_H

#include <barrelfish/syscall_arch.h> // for cap_invoke
#include <barrelfish/caddr.h>

/**
 * \brief Spawn a new core.
 *
 * \param cur_kern   Cap of the current kernel
 * \param core_id    APIC ID of the core to try booting
 * \param sp_mem     Cap to Ram type memory to relocate the new kernel
 * \param dcb        Cap to the dcb of the user program to run on the new kernel
 * \param root_vbits Number of valid bits in root_cptr
 * \param root_cptr  Cap to the root of cspace of the new user program
 * \param vtree      Cap to the vtree root of the new user program
 * \param dispatcher Cap to the dispatcher of the new user program
 * \param entry      Kernel entry point in physical memory
 */
static inline errval_t
invoke_monitor_spawn_core(coreid_t core_id, enum cpu_type cpu_type,
                          forvaddr_t entry)
{
    struct idc_send_msg msg;

    idc_msg_init(&msg);
    idc_msg_encode_word(&msg, KernelCmd_Spawn_core);
    idc_msg_encode_word(&msg, core_id);
    idc_msg_encode_word(&msg, cpu_type);
    idc_msg_encode_word(&msg, entry);

    return cap_invoke(cap_kernel, &msg).error;
}

static inline errval_t
invoke_monitor_identify_cap(capaddr_t cap, int bits, struct capability *out)
{
    struct idc_send_msg msg;

    idc_msg_init(&msg);
    idc_msg_encode_word(&msg, KernelCmd_Identify_cap);
    idc_msg_encode_word(&msg, cap);
    idc_msg_encode_word(&msg, bits);
    idc_msg_encode_word(&msg, (uintptr_t)out);

    return cap_invoke(cap_kernel, &msg).error;
}

static inline errval_t
invoke_monitor_nullify_cap(capaddr_t cap, int bits)
{
    struct idc_send_msg msg;

    idc_msg_init(&msg);
    idc_msg_encode_word(&msg, KernelCmd_Nullify_cap);
    idc_msg_encode_word(&msg, cap);
    idc_msg_encode_word(&msg, bits);

    return cap_invoke(cap_kernel, &msg).error;
}

static inline errval_t
invoke_monitor_create_cap(uint64_t *raw, capaddr_t caddr, int bits, capaddr_t slot)
{
    struct idc_send_msg msg;

    idc_msg_init(&msg);
    idc_msg_encode_word(&msg, KernelCmd_Create_cap);
    idc_msg_encode_word(&msg, (uintptr_t)raw);
    idc_msg_encode_word(&msg, caddr);
    idc_msg_encode_word(&msg, bits);
    idc_msg_encode_word(&msg, slot);

    return cap_invoke(cap_kernel, &msg).error;
}

static inline errval_t
invoke_monitor_cap_remote(capaddr_t cap, int bits, bool is_remote, 
                          bool * has_descendents)
{
    struct idc_send_msg msg;

    idc_msg_init(&msg);
    idc_msg_encode_word(&msg, KernelCmd_Remote_cap);
    idc_msg_encode_word(&msg, cap);
    idc_msg_encode_word(&msg, bits);
    idc_msg_encode_word(&msg, is_remote);
    idc_msg_encode_word(&msg, (uintptr_t)has_descendents);
    
    return cap_invoke(cap_kernel, &msg).error;
}

static inline errval_t
invoke_monitor_register(struct capref ep)
{
    struct idc_send_msg msg;

    idc_msg_init(&msg);
    idc_msg_encode_word(&msg, KernelCmd_Register);
    idc_msg_encode_word(&msg, get_cap_addr(ep));

    return cap_invoke(cap_kernel, &msg).error;
}

static inline errval_t
invoke_monitor_identify_cnode_get_cap(uint64_t *cnode_raw, capaddr_t slot,
                                      struct capability *out)
{
    USER_PANIC("NYI");
    assert(cnode_raw != NULL);
    assert(out != NULL);
    return LIB_ERR_NOT_IMPLEMENTED;

#if 0
    struct idc_send_msg msg;
    idc_msg_init(&msg);
    idc_msg_encode_word(&msg, KernelCmd_Iden_cnode_get_cap);
    for (int i = 0; i < sizeof(struct capability) / sizeof(uint64_t); i++) {
        idc_msg_encode_word(&msg, cnode_raw[i]);
    }
    idc_msg_encode_word(&msg, slot);
    idc_msg_encode_word(&msg, (uintptr_t)out);

    return cap_invoke(cap_kernel, &msg);
#endif
}

/**
 * \brief Set up tracing in the kernel
 *
 */
static inline errval_t
invoke_trace_setup(struct capref cap)
{
    struct idc_send_msg msg;
    idc_msg_init(&msg);
    idc_msg_encode_word(&msg, KernelCmd_Setup_trace);
    idc_msg_encode_word(&msg, get_cap_addr(cap));
    return cap_invoke(cap_kernel, &msg).error;
}

static inline errval_t
invoke_domain_id(struct capref cap, uint64_t domain_id)
{
    USER_PANIC("NYI");
    return LIB_ERR_NOT_IMPLEMENTED;
#if 0
    struct idc_send_msg msg;
    idc_msg_init(&msg);
    idc_msg_encode_word(&msg, KernelCmd_Domain_Id);
    idc_msg_encode_word(&msg, get_cap_addr(cap));
    idc_msg_encode_word(&msg, domain_id);
    return cap_invoke(cap_kernel, &msg);
#endif
}
static inline errval_t 
invoke_monitor_identify_domains_cap(capaddr_t root_cap, int root_bits,
                                    capaddr_t cap, int bits,
                                    struct capability *out)
{
    struct idc_send_msg msg;

    idc_msg_init(&msg);
    idc_msg_encode_word(&msg, KernelCmd_Identify_domains_cap);
    idc_msg_encode_word(&msg, root_cap);
    idc_msg_encode_word(&msg, root_bits);
    idc_msg_encode_word(&msg, cap);
    idc_msg_encode_word(&msg, bits);
    idc_msg_encode_word(&msg, (uintptr_t)out);

    return cap_invoke(cap_kernel, &msg).error;
}

static inline errval_t
invoke_monitor_remote_cap_retype(capaddr_t rootcap_addr, uint8_t rootcap_vbits,
                                 capaddr_t src, enum objtype newtype,
                                 int objbits, capaddr_t to, capaddr_t slot,
                                 int bits)
{
    struct idc_send_msg msg;

    idc_msg_init(&msg);
    idc_msg_encode_word(&msg, MonitorCmd_Retype);
    idc_msg_encode_word(&msg, rootcap_addr);
    idc_msg_encode_word(&msg, rootcap_vbits);
    idc_msg_encode_word(&msg, src);
    idc_msg_encode_word(&msg, newtype);
    idc_msg_encode_word(&msg, objbits);
    idc_msg_encode_word(&msg, to);
    idc_msg_encode_word(&msg, slot);
    idc_msg_encode_word(&msg, bits);

    return cap_invoke(cap_kernel, &msg).error;
}



static inline errval_t
invoke_monitor_remote_cap_delete(capaddr_t rootcap_addr, uint8_t rootcap_vbits,
                                 capaddr_t src, int bits) 
{
    struct idc_send_msg msg;
    
    idc_msg_init(&msg);
    idc_msg_encode_word(&msg, MonitorCmd_Delete);
    idc_msg_encode_word(&msg, rootcap_addr);
    idc_msg_encode_word(&msg, rootcap_vbits);
    idc_msg_encode_word(&msg, src);
    idc_msg_encode_word(&msg, bits);

    return cap_invoke(cap_kernel, &msg).error;
}

static inline errval_t
invoke_monitor_remote_cap_revoke(capaddr_t rootcap_addr, uint8_t rootcap_vbits,
                                 capaddr_t src, int bits) 
{
    struct idc_send_msg msg;

    idc_msg_init(&msg);
    idc_msg_encode_word(&msg, MonitorCmd_Revoke);
    idc_msg_encode_word(&msg, rootcap_addr);
    idc_msg_encode_word(&msg, rootcap_vbits);
    idc_msg_encode_word(&msg, src);
    idc_msg_encode_word(&msg, bits);

    return cap_invoke(cap_kernel, &msg).error;
}

static inline errval_t invoke_monitor_bmptable_set(
    struct capref bmpcap, unsigned int associd, struct capref ep)
{
    struct idc_send_msg msg;

    idc_msg_init(&msg);
    idc_msg_encode_word(&msg, BMPTableCmd_Set);
    idc_msg_encode_word(&msg, associd);
    idc_msg_encode_word(&msg, get_cap_addr(ep));
	
    return cap_invoke(bmpcap, &msg).error;
}

static inline errval_t invoke_monitor_bmptable_delete(
    struct capref bmpcap, unsigned int associd)
{
    struct idc_send_msg msg;

    idc_msg_init(&msg);
    idc_msg_encode_word(&msg, BMPTableCmd_Delete);
    idc_msg_encode_word(&msg, associd);
    
    return cap_invoke(bmpcap, &msg).error;
}

static inline errval_t invoke_monitor_sync_timer(uint64_t synctime)
{
    return ERR_NOTIMP;
}

static inline errval_t
invoke_monitor_get_arch_id(uintptr_t *arch_id)
{
    // core id == arch id on Beehive
    coreid_t coreid;
    errval_t err = invoke_kernel_get_core_id(cap_kernel, &coreid);
    if(err_is_ok(err)) {
        *arch_id = coreid;
    }
    return err;
}

#endif // ARCH_BEEHIVE_MONITOR_INVOCATIONS_H
