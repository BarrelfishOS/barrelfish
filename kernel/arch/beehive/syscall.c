/**
 * \file
 * \brief System calls implementation.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <hyper.h>
#include <stdio.h>
#include <errno.h>
#include <barrelfish_kpi/syscalls.h> // for struct sysret
#include <barrelfish_kpi/paging_arch.h>
#include <dispatch.h> // for dcb_current
#include <syscall.h> // kernel generic syscalls
#include <mdb.h> // for remove_mapping
#include <barrelfish_kpi/legacy_idc_buffer.h>
#include <barrelfish_kpi/lmp.h>
#include <target/beehive/barrelfish_kpi/dispatcher_shared_target.h>
#include <target/beehive/barrelfish_kpi/coredata_target.h>
#include <corearea.h>
#include <dcache.h>
#include <trace/trace.h> // needed for trace_snapshot
#include "beekernel.h"
#include "bmp.h"

#define MIN(a,b)        ((a) < (b) ? (a) : (b))


static struct sysret handle_invocation(struct capability *to,
                                       struct idc_send_msg *msg);



static struct sysret handle_dispatcher_setup(struct capability *to,
                                             struct idc_recv_msg *msg)
{
    capaddr_t cptr = idc_msg_decode_word_or_zero(msg);
    int depth    = idc_msg_decode_word_or_zero(msg);
    capaddr_t vptr = idc_msg_decode_word_or_zero(msg); // CPTR_NULL
    capaddr_t dptr = idc_msg_decode_word_or_zero(msg);
    bool run = idc_msg_decode_word_or_zero(msg);
    capaddr_t odptr = idc_msg_decode_word_or_zero(msg);

    // The generic code checks that vspace is initialized
    struct dcb *dcb = to->u.dispatcher.dcb;
    dcb->vspace = 0xBAD23200;

    return sys_dispatcher_setup(to, cptr, depth, vptr, dptr, run, odptr);
}

static struct sysret handle_dispatcher_properties(struct capability *to,
                                                  struct idc_recv_msg *msg)
{
    enum task_type type = idc_msg_decode_word_or_zero(msg);
    unsigned long deadline = idc_msg_decode_word_or_zero(msg);
    unsigned long wcet = idc_msg_decode_word_or_zero(msg);
    unsigned long period = idc_msg_decode_word_or_zero(msg);
    unsigned long release = idc_msg_decode_word_or_zero(msg);
    unsigned short weight = idc_msg_decode_word_or_zero(msg);

    return sys_dispatcher_properties(to, type, deadline, wcet, period,
                                     release, weight);
}


static struct sysret handle_dispatcher_perfmon(struct capability *to,
                                               struct idc_recv_msg *msg)
{
    return SYSRET(SYS_ERR_ILLEGAL_SYSCALL);
}

static struct sysret handle_retype_common(struct capability *root,
                                          struct idc_recv_msg *msg,
                                          bool from_monitor)
{
    capaddr_t source_cptr     = idc_msg_decode_word_or_zero(msg);
    enum objtype type       = idc_msg_decode_word_or_zero(msg);
    uint8_t objbits         = idc_msg_decode_word_or_zero(msg);
    capaddr_t dest_cnode_cptr = idc_msg_decode_word_or_zero(msg);
    capaddr_t dest_slot       = idc_msg_decode_word_or_zero(msg);
    uint8_t dest_vbits      = idc_msg_decode_word_or_zero(msg);

    return sys_retype(root, source_cptr, type, objbits, dest_cnode_cptr,
                      dest_slot, dest_vbits, from_monitor);
}

static struct sysret handle_retype(struct capability *root,
                                   struct idc_recv_msg *msg)
{
    return handle_retype_common(root, msg, false);
}

/**
 * Common code for copying and minting except the mint flag and param passing
 */
static struct sysret copy_or_mint(struct capability *root,
                                  struct idc_recv_msg *msg, bool mint)
{
    /* Retrive arguments */
    capaddr_t  destcn_cptr   = idc_msg_decode_word_or_zero(msg);
    cslot_t dest_slot     = idc_msg_decode_word_or_zero(msg);
    capaddr_t  source_cptr   = idc_msg_decode_word_or_zero(msg);
    int      destcn_vbits  = idc_msg_decode_word_or_zero(msg);
    int      source_vbits  = idc_msg_decode_word_or_zero(msg);
    uintptr_t param1, param2;
    // params only sent if mint operation
    if (mint) {
        param1 = idc_msg_decode_word_or_zero(msg);
        param2 = idc_msg_decode_word_or_zero(msg);
    } else {
        param1 = param2 = 0;
    }

    return sys_copy_or_mint(root, destcn_cptr, dest_slot, source_cptr,
                            destcn_vbits, source_vbits, param1, param2, mint);
}

static struct sysret handle_mint(struct capability *root,
                                 struct idc_recv_msg *msg)
{
    return copy_or_mint(root, msg, true);
}

static struct sysret handle_copy(struct capability *root,
                                 struct idc_recv_msg *msg)
{
    return copy_or_mint(root, msg, false);
}

static struct sysret handle_delete_common(struct capability *root,
                                          struct idc_recv_msg *msg,
                                          bool from_monitor)
{
    capaddr_t cptr = idc_msg_decode_word_or_zero(msg);
    int bits     = idc_msg_decode_word_or_zero(msg);
    return sys_delete(root, cptr, bits, from_monitor);
}

static struct sysret handle_delete(struct capability *root,
                                   struct idc_recv_msg *msg)
{
    return  handle_delete_common(root, msg, false);
}

static struct sysret handle_revoke_common(struct capability *root,
                                          struct idc_recv_msg *msg,
                                          bool from_monitor)
{
    capaddr_t cptr = idc_msg_decode_word_or_zero(msg);
    int bits     = idc_msg_decode_word_or_zero(msg);
    return sys_revoke(root, cptr, bits, from_monitor);
}

static struct sysret handle_revoke(struct capability *root,
                                   struct idc_recv_msg *msg)
{
    return  handle_revoke_common(root, msg, false);
}

static struct sysret monitor_handle_retype(struct capability *kernel_cap,
                                           struct idc_recv_msg *msg)
{
    errval_t err;

    capaddr_t root_caddr = idc_msg_decode_word_or_zero(msg);
    capaddr_t root_vbits = idc_msg_decode_word_or_zero(msg);

    struct capability *root;
    err = caps_lookup_cap(&dcb_current->cspace.cap, root_caddr, root_vbits,
                          &root, CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_ROOT_CAP_LOOKUP));
    }

    return handle_retype_common(root, msg, true);
}

/// Different handler for cap operations performed by the monitor
static struct sysret monitor_handle_delete(struct capability *kernel_cap,
                                           struct idc_recv_msg *msg)
{
    errval_t err;

    capaddr_t root_caddr = idc_msg_decode_word_or_zero(msg);
    capaddr_t root_vbits = idc_msg_decode_word_or_zero(msg);

    struct capability *root;
    err = caps_lookup_cap(&dcb_current->cspace.cap, root_caddr, root_vbits,
                          &root, CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_ROOT_CAP_LOOKUP));
    }

    return handle_delete_common(root, msg, true);
}

/// Different handler for cap operations performed by the monitor
static struct sysret monitor_handle_revoke(struct capability *kernel_cap,
                                           struct idc_recv_msg *msg)
{
    errval_t err;

    capaddr_t root_caddr = idc_msg_decode_word_or_zero(msg);
    capaddr_t root_vbits = idc_msg_decode_word_or_zero(msg);

    struct capability *root;
    err = caps_lookup_cap(&dcb_current->cspace.cap, root_caddr, root_vbits,
                          &root, CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_ROOT_CAP_LOOKUP));
    }

    return handle_revoke_common(root, msg, true);
}

static struct sysret monitor_handle_register(struct capability *kernel_cap,
                                             struct idc_recv_msg *msg)
{
    capaddr_t ep_caddr = idc_msg_decode_word_or_zero(msg);
    return sys_monitor_register(ep_caddr);
}

//
// Inter-core messaging
//
typedef unsigned int msg_t[63];

// Send a message to core number "dest", using "len" words at "buf".
//
// Note that message lengths are measured in words, not bytes.
void message_send(unsigned int dest, unsigned int type,
                  msg_t *buf, unsigned int len);

// If there's a message available to receive, place its details and
// contents in (srce, type, buf) and return its length.
// Otherwise return 0.
unsigned int message_recv(unsigned int *srce, unsigned int *type,
                          msg_t * buf);

/// Different handler for cap operations performed by the monitor
static struct sysret
sys_monitor_spawn_core(coreid_t core_id, enum cpu_type cpu_type,
                       genvaddr_t core_data)
{
    struct beehive_core_data *bcd =
        (struct beehive_core_data*)(lvaddr_t)core_data;
    if (core_id < 2 || core_id >= arch_get_max_core_id())
	panic("core_id out of range");
    printf("spawning core %d data=%p cpu=%p monitor=%p\n",
	   core_id, bcd, bcd->cpu_module, bcd->monitor_module);

    uint32_t entry = bcd->cpu_module->btorg;

    // Setup registers in save area
    struct corearea *savearea = COREAREA_FOR_CORE(core_id);
    for (int i = 1; i < 32; i++) {
        savearea->regs[i] = 0xBAD000 | i;
    }
    // Count
    savearea->regs[0] = 0;
    
    /* Pass argument through to target _bmain via s1 */
    savearea->regs[9] = core_data;
    savearea->regs[31] = entry;

    // Mark kernel covering whole memory so timer interrupts are deferred
    savearea->kernel_begins = 0;
    savearea->kernel_ends = 0x7fffffff;

    //bee_dcache_flush_rgn(savearea, 512);
    bee_dcache_flush_all(); // paranoid!

    if (arch_get_core_id() != 1) {
        msg_t msg;
        msg[0] = HYPER_START_CORE;
        msg[1] = core_id;
        message_send(1, HYPER_MSG_TYPE, &msg, 2);
    } else {
        // Send a START message
        message_send(core_id, 0, NULL, 0);
    }

    return SYSRET(SYS_ERR_OK);
}

/**
 * \brief Spawn a new core and create a kernel cap for it.
 */
static struct sysret monitor_spawn_core(struct capability *kernel_cap,
                                        struct idc_recv_msg *msg)
{
    coreid_t core_id       = idc_msg_decode_word_or_zero(msg);
    enum cpu_type cpu_type = idc_msg_decode_word_or_zero(msg);
    genvaddr_t entry       = idc_msg_decode_word_or_zero(msg);

    return sys_monitor_spawn_core(core_id, cpu_type, entry);
}

static struct sysret monitor_get_core_id(struct capability *kernel_cap,
                                         struct idc_recv_msg *msg)
{
    return (struct sysret){.error = SYS_ERR_OK, .value = arch_get_core_id()};
}


static struct sysret monitor_identify_cap_common(struct capability *kernel_cap,
                                                 struct capability *root,
                                                 struct idc_recv_msg *msg)
{
    capaddr_t cptr = idc_msg_decode_word_or_zero(msg);
    uint8_t bits = idc_msg_decode_word_or_zero(msg);
    struct capability *retbuf = (void *)idc_msg_decode_word_or_zero(msg);

    return sys_monitor_identify_cap(root, cptr, bits, retbuf);
}

static struct sysret monitor_identify_cap(struct capability *kernel_cap,
                                          struct idc_recv_msg *msg)
{
    return monitor_identify_cap_common(kernel_cap, &dcb_current->cspace.cap, msg);
}

static struct sysret monitor_identify_domains_cap(struct capability *kernel_cap,
                                                  struct idc_recv_msg *msg)
{
    errval_t err;

    capaddr_t root_caddr = idc_msg_decode_word_or_zero(msg);
    capaddr_t root_vbits = idc_msg_decode_word_or_zero(msg);

    struct capability *root;
    err = caps_lookup_cap(&dcb_current->cspace.cap, root_caddr, root_vbits,
                          &root, CAPRIGHTS_READ);

    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_ROOT_CAP_LOOKUP));
    }
    return monitor_identify_cap_common(kernel_cap, root, msg);
}

static struct sysret monitor_remote_cap(struct capability *kernel_cap,
                                        struct idc_recv_msg *msg)
{
    struct capability *root = &dcb_current->cspace.cap;
    capaddr_t cptr = idc_msg_decode_word_or_zero(msg);
    int bits = idc_msg_decode_word_or_zero(msg);
    bool remote = (bool)idc_msg_decode_word_or_zero(msg);
    bool * has_desc = (bool *)idc_msg_decode_word_or_zero(msg);

    struct cte *cte;
    errval_t err = caps_lookup_slot(root, cptr, bits, &cte, CAPRIGHTS_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_IDENTIFY_LOOKUP));
    }

    set_cap_remote(cte, remote);
    *has_desc = has_descendants(cte);

    return SYSRET(SYS_ERR_OK);
}


static struct sysret monitor_create_cap(struct capability *kernel_cap,
                                        struct idc_recv_msg *msg)
{
    /* Get the raw metadata of the capability to create */
    /* XXX: trusting user pointer! */
    struct capability *src = (struct capability*)idc_msg_decode_word_or_zero(msg);

    /* Certain types cannot be created here */
    if ((src->type == ObjType_Null) || (src->type == ObjType_EndPoint)
        || (src->type == ObjType_Dispatcher) || (src->type == ObjType_Kernel)
        || (src->type == ObjType_IRQTable)) {
        return SYSRET(SYS_ERR_ILLEGAL_DEST_TYPE);
    }

    /* Create the cap in the destination */
    capaddr_t cnode_cptr = idc_msg_decode_word_or_zero(msg);
    int cnode_vbits    = idc_msg_decode_word_or_zero(msg);
    size_t slot        = idc_msg_decode_word_or_zero(msg);
    return SYSRET(caps_create_from_existing(&dcb_current->cspace.cap,
                                            cnode_cptr, cnode_vbits,
                                            slot, src));
}

static struct sysret monitor_nullify_cap(struct capability *kernel_cap,
                                         struct idc_recv_msg *msg)
{
    capaddr_t cptr = idc_msg_decode_word_or_zero(msg);
    uint8_t bits = idc_msg_decode_word_or_zero(msg);

    return sys_monitor_nullify_cap(cptr, bits);
}

static struct sysret monitor_iden_cnode_get_cap(struct capability *kern_cap,
                                                struct idc_recv_msg *msg)
{
    errval_t err;

    /* Get the raw metadata of the cnode */
    uintptr_t raw[sizeof(struct capability) / sizeof(uintptr_t)];
    for(int i = 0; i < sizeof(struct capability) / sizeof(uintptr_t); i++) {
        raw[i] = idc_msg_decode_word_or_zero(msg);
    }

    struct capability *cnode = (struct capability*)raw;
    assert(cnode->type == ObjType_CNode);

    struct capability *cnode_copy;
    err = mdb_get_copy(cnode, &cnode_copy);
    if (err_is_fail(err)) {
        return SYSRET(err);
    }

    capaddr_t slot = idc_msg_decode_word_or_zero(msg);
    struct cte* cte = caps_locate_slot(cnode_copy->u.cnode.cnode, slot);

    // XXX: Write cap data directly back to user-space
    // FIXME: this should involve a pointer/range check for reliability,
    // but because the monitor is inherently trusted it's not a security hole
    struct capability *retbuf = (void *)idc_msg_decode_word_or_zero(msg);
    *retbuf = cte->cap;

    return SYSRET(SYS_ERR_OK);
}

static struct sysret handle_frame_identify(struct capability *to,
                                           struct idc_recv_msg *msg)
{
    // Return with physical base address of frame
    // XXX: pack size into bottom bits of base address
    assert(to->type == ObjType_Frame || to->type == ObjType_DevFrame);
    assert((to->u.frame.base & BASE_PAGE_MASK) == 0);
    return (struct sysret) {
        .error = SYS_ERR_OK,
        .value = to->u.frame.base | to->u.frame.bits,
    };
}


static struct sysret monitor_handle_domain_id(struct capability *monitor_cap,
                                              struct idc_recv_msg *msg)
{
    capaddr_t cptr = idc_msg_decode_word_or_zero(msg);
    domainid_t domain_id = idc_msg_decode_word_or_zero(msg);

    return sys_monitor_domain_id(cptr, domain_id);
}

/**
 * \brief Set up tracing in the kernel
 */
static struct sysret handle_trace_setup(struct capability *cap,
                                        struct idc_recv_msg *msg)
{
    struct capability *frame;
    errval_t err;

    /* lookup passed cap */
    capaddr_t cptr = idc_msg_decode_word_or_zero(msg);
    err = caps_lookup_cap(&dcb_current->cspace.cap, cptr, CPTR_BITS, &frame,
                          CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err);
    }

    //panic("kernel_trace_buf\n");
    kernel_trace_buf = /*phys_to_mem*/(frame->u.frame.base);
    //printf("kernel.%u: handle_trace_setup at %lx\n", apic_id, kernel_trace_buf);
    return SYSRET(SYS_ERR_OK);
}

static struct sysret irq_table_set(struct capability *to, struct idc_recv_msg *msg)
{
    return SYSRET(SYS_ERR_ILLEGAL_SYSCALL);
}

static struct sysret irq_table_delete(struct capability *to, struct idc_recv_msg *msg)
{
    return SYSRET(SYS_ERR_ILLEGAL_SYSCALL);
}

static struct sysret handle_io(struct capability *to, struct idc_recv_msg *msg)
{
    return SYSRET(SYS_ERR_IO_PORT_INVALID);
}




typedef struct sysret (*invocation_handler_t)(struct capability *to,
                                              struct idc_recv_msg *msg);

static invocation_handler_t invocations[ObjType_Num][CAP_MAX_CMD] = {
    [ObjType_Dispatcher] = {
        [DispatcherCmd_Setup] = handle_dispatcher_setup,
        [DispatcherCmd_Properties] = handle_dispatcher_properties,
        [DispatcherCmd_PerfMon] = handle_dispatcher_perfmon,
    },
    [ObjType_Frame] = {
        [FrameCmd_Identify] = handle_frame_identify
    },
    [ObjType_DevFrame] = {
        [FrameCmd_Identify] = handle_frame_identify
    },
    [ObjType_CNode] = {
        [CNodeCmd_Copy]   = handle_copy,
        [CNodeCmd_Mint]   = handle_mint,
        [CNodeCmd_Retype] = handle_retype,
        [CNodeCmd_Delete] = handle_delete,
        [CNodeCmd_Revoke] = handle_revoke,
    },
    [ObjType_Kernel] = {
        [KernelCmd_Spawn_core]   = monitor_spawn_core,
        [KernelCmd_Get_core_id]  = monitor_get_core_id,
        [KernelCmd_Identify_cap] = monitor_identify_cap,
        [KernelCmd_Identify_domains_cap] = monitor_identify_domains_cap,
        [KernelCmd_Remote_cap]   = monitor_remote_cap,
        [KernelCmd_Iden_cnode_get_cap] = monitor_iden_cnode_get_cap,
        [KernelCmd_Create_cap]   = monitor_create_cap,
        [KernelCmd_Nullify_cap]  = monitor_nullify_cap,
        [KernelCmd_Setup_trace]  = handle_trace_setup,
        [KernelCmd_Register]     = monitor_handle_register,
        [KernelCmd_Domain_Id]    = monitor_handle_domain_id,
        [MonitorCmd_Retype]      = monitor_handle_retype,
        [MonitorCmd_Delete]      = monitor_handle_delete,
        [MonitorCmd_Revoke]      = monitor_handle_revoke
    },
    [ObjType_IRQTable] = {
        [IRQTableCmd_Set] = irq_table_set,
        [IRQTableCmd_Delete] = irq_table_delete
    },
    [ObjType_BMPTable] = {
        [BMPTableCmd_Set] = bmp_table_set,
        [BMPTableCmd_Delete] = bmp_table_delete
    },
    [ObjType_IO] = {
        [IOCmd_Outb] = handle_io,
        [IOCmd_Outw] = handle_io,
        [IOCmd_Outd] = handle_io,
        [IOCmd_Inb] = handle_io,
        [IOCmd_Inw] = handle_io,
        [IOCmd_Ind] = handle_io
    }
};

static struct sysret handle_invocation(struct capability *to,
                                       struct idc_send_msg *msg)
{
    assert(to->type < ObjType_Num);

    // Endpoint cap, do IDC
    if (to->type == ObjType_EndPoint) {
        struct dcb *listener = to->u.endpoint.listener;
        assert(listener != NULL);

        if (listener->disp == 0) {
            return SYSRET(SYS_ERR_LMP_NO_TARGET);
        }

        // does the sender want to yield their timeslice on success?
        bool sync = msg->u.x.header.x.flags.sync;
        // does the sender want to yield to the target if undeliverable?
        bool yield = msg->u.x.header.x.flags.yield;

        /* limit length of message from buggy/malicious sender */
        size_t len = MIN(msg->u.x.header.x.length, LMP_MSG_LENGTH);

        // try to deliver message
        errval_t err = lmp_deliver(to, dcb_current, msg->u.x.words, len,
                                   msg->u.x.header.x.send_cptr,
                                   msg->u.x.header.x.send_bits);

        /* Switch to reciever upon successful delivery with sync flag,
         * or (some cases of) unsuccessful delivery with yield flag */
        enum err_code err_code = err_no(err);
        if ((sync && err_is_ok(err)) ||
            (yield && (err_code == SYS_ERR_LMP_BUF_OVERFLOW
                       || err_code == SYS_ERR_LMP_CAPTRANSFER_DST_CNODE_LOOKUP
                       || err_code == SYS_ERR_LMP_CAPTRANSFER_DST_CNODE_INVALID
                       || err_code == SYS_ERR_LMP_CAPTRANSFER_DST_SLOT_OCCUPIED))
                ) {
            if (err_is_fail(err)) {
                struct dispatcher_shared_generic *current_disp =
                    get_dispatcher_shared_generic(dcb_current->disp);
                struct dispatcher_shared_generic *listener_disp =
                    get_dispatcher_shared_generic(listener->disp);
                debug(SUBSYS_DISPATCH, "IDC failed; %.*s yields to %.*s: %u\n",
                      DISP_NAME_LEN, current_disp->name,
                      DISP_NAME_LEN, listener_disp->name, err_code);
            }
            union registers_beehive *save_area;
            dispatcher_handle_t handle = dcb_current->disp;
            struct dispatcher_shared_beehive *disp =
                get_dispatcher_shared_beehive(handle);
            //printf("dcb_current->disabled = %d\n", dcb_current->disabled);
            if (dcb_current->disabled) {
                save_area = &disp->disabled_save_area;
            } else {
                save_area = &disp->enabled_save_area;
            }
            // The syscall_invoke in syscall_arch.h returns errval_t
            save_area->named.return1 = err; // SET RETURN VALUE

            dispatch(to->u.endpoint.listener);
            panic("dispatch returned");
        }

        return SYSRET(err);
    }
    else if (to->type == ObjType_BMPEndPoint) {

        // TODO: XXX should this by done through the dispach table?
        // the dispatch code below looks bizzare and bad performance

        int core      = to->u.bmpendpoint.coreid;
        int assoc     = to->u.bmpendpoint.chanid;
        int len       = msg->u.x.words[0];
        uint32_t *ptr = (uintptr_t*)msg->u.x.words[1];
        bmp_send(core, assoc, len, ptr);
        return SYSRET(0);
    }


    // not endpoint cap, call kernel handler through dispatch table

    // construct receive message
    struct idc_recv_msg rcvmsg = {
        .pos = 0,
        .msg.header.raw = 0,
    };
    rcvmsg.msg.header.x.length = MIN(msg->u.x.header.x.length, IDC_MSG_LENGTH);

    /* XXX: copy payload */
    for (int i = 0; i < rcvmsg.msg.header.x.length; i++) {
        rcvmsg.msg.words[i] = msg->u.x.words[i];
    }

    uintptr_t cmd = idc_msg_decode_word_or_zero(&rcvmsg);
    if (cmd >= CAP_MAX_CMD) {
        return SYSRET(SYS_ERR_ILLEGAL_INVOCATION);
    }

    invocation_handler_t invocation = invocations[to->type][cmd];
    if(invocation == NULL) {
        return SYSRET(SYS_ERR_ILLEGAL_INVOCATION);
    }

    // Call the invocation
    struct sysret sysret = invocation(to, &rcvmsg);

    // If dcb_current got removed, dispatch someone else
    if (dcb_current == NULL) {
        assert(err_is_ok(sysret.error));
        dispatch(schedule());
    }

    return sysret;
} // handle_invocation


/*
 * The assembler trap handler assumes that these functions have a
 * maximum of 6 words of arguments.  Any more than this requries
 * modification in the assember.
 */
errval_t sys_invoke(uintptr_t *const pvalue,
                    uint8_t validbits, capaddr_t cptr,
                    struct idc_send_msg *msg);
errval_t sys_beehive_yield(capaddr_t target);
errval_t sys_lrpc(void);
errval_t sys_reboot(void);
errval_t sys_nop(void);
errval_t beehive_sys_print(const char *str, size_t length);
errval_t sys_rundown(void);
errval_t sys_illegal(unsigned int num);


/* Check that the table below is going to be correct */
STATIC_ASSERT(SYSCALL_INVOKE == 0, "0");
STATIC_ASSERT(SYSCALL_YIELD == 1, "1");
STATIC_ASSERT(SYSCALL_LRPC == 2, "2");
STATIC_ASSERT(SYSCALL_DEBUG == 3, "3");
STATIC_ASSERT(SYSCALL_REBOOT == 4, "4");
STATIC_ASSERT(SYSCALL_NOP == 5, "5");
STATIC_ASSERT(SYSCALL_PRINT == 6, "6");
STATIC_ASSERT(SYSCALL_BEEHIVE_TRACE_RUNDOWN == 7, "7");

const void* syscalls[] = {
    sys_invoke,
    sys_beehive_yield,
    sys_lrpc,
    sys_illegal,
    sys_reboot,
    sys_nop,
    sys_print,
    sys_rundown,
    sys_illegal,
    sys_illegal
};

// The sys_illegal must be at index SYSCALL_COUNT
STATIC_ASSERT_SIZEOF(syscalls, (sizeof(void*) * (SYSCALL_COUNT+1)));



// XXX TODO: Should be typedef for these, not uint8_t
errval_t sys_invoke(uintptr_t *const pvalue,
                    uint8_t validbits, capaddr_t cptr, struct idc_send_msg *msg)
{
    //printf("sys_invoke: called %u 0x%" PRIxCADDR "\n", validbits, cptr);

    struct capability *to = NULL;
    errval_t err = caps_lookup_cap(&dcb_current->cspace.cap,
                                   cptr, validbits,
                                   &to, CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        printf("sys_invoke: caps_lookup_cap: error %#x\n", err);
        //debug_print_backtrace();
        return err;
    }

    assert(to != NULL);

    // done in asm: dcb_current->disabled = disp_is_disabled_ip(dcb_current->disp, 0);

    struct sysret retval = handle_invocation(to, msg);
    if (err_is_fail(retval.error) &&
        retval.error != SYS_ERR_RETRY_THROUGH_MONITOR) {
        printf("sys_invoke: handle_invocation: error %#x\n", retval.error);
        debug_print_backtrace();
    }
    //printf("sys_invoke: return %#x,%#x\n", retval.error, retval.value);
    *pvalue = retval.value;
    return retval.error;
}


errval_t sys_lrpc(void)
{
    panic("sys_lrpc: called\n");
}

errval_t sys_beehive_yield(capaddr_t target)
{
    return sys_yield(target).error;
}

errval_t sys_reboot(void)
{
    panic("sys_reboot: called");
}


errval_t sys_nop(void)
{
    bmp_pump();
    return SYS_ERR_OK;
}

/**
 * Put rundown of current state in the trace buffer
 * This would typically be called at the start of tracing
 */
void trace_snapshot(void)
{
    // DCBS
    struct dcb *dcb = dcbs_list;
    struct trace_event ev;
    errval_t err;

    while (dcb != NULL) {
        struct dispatcher_shared_generic *disp =
            get_dispatcher_shared_generic(dcb->disp);
        //printf("%d DCB: %p %.*s\n", my_core_id, dcb, DISP_NAME_LEN, disp->name);
	// Top bit of timestamp is flag to indicate dcb rundown events
        ev.timestamp = (1ULL << 63) | (uintptr_t)dcb;
	assert(sizeof(ev.u.raw) <= sizeof(disp->name));
        memcpy(&ev.u.raw, disp->name, sizeof(ev.u.raw));
        err = trace_write_event(&ev);
        dcb = dcb->next_all;
    }

    // TO DO: currently running domain
}

errval_t sys_rundown(void)
{
    trace_snapshot();
    return SYS_ERR_OK;
}


errval_t sys_illegal(unsigned int num)
{
    printf("sys_illegal: %u\n", num);
    return SYS_ERR_ILLEGAL_SYSCALL;
}

