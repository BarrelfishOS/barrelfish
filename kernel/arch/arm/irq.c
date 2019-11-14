/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#include <kernel.h>
#include <stdio.h>
#include <string.h>
#include <arch/arm/arm.h>
#include <arch/arm/platform.h>
#include <barrelfish_kpi/lmp.h>
#include <barrelfish_kpi/syscalls.h>
#include <barrelfish_kpi/sys_debug.h>

#include <irq.h>

#include <paging_kernel_arch.h>
#include <dispatch.h>
#include <exec.h>
#include <stdio.h>
#include <syscall.h>
#include <arch/arm/syscall_arm.h>
#include <kcb.h>

/**
 * \brief User-space IRQ dispatch table.
 *
 * This is essentially a big CNode holding #NDISPATCH capability
 * entries to local endpoints of user-space applications listening to
 * the interrupts.
 */
static struct cte irq_dispatch[NDISPATCH];

errval_t irq_table_set(unsigned int nidt, capaddr_t endpoint)
{
    errval_t err;
    struct cte *recv;

    err = caps_lookup_slot(&dcb_current->cspace.cap, endpoint,
                           2, &recv, CAPRIGHTS_WRITE);
    if (err_is_fail(err)) {
        return err_push(err, SYS_ERR_IRQ_LOOKUP);
    }

    assert(recv != NULL);

    // Return w/error if cap is not an endpoint
    if (recv->cap.type != ObjType_EndPointLMP) {
        return SYS_ERR_IRQ_NOT_ENDPOINT;
    }

    // Return w/error if no listener on endpoint
    if (recv->cap.u.endpointlmp.listener == NULL) {
        return SYS_ERR_IRQ_NO_LISTENER;
    }

    if (nidt < NDISPATCH) {
        // check that we don't overwrite someone else's handler
        if (irq_dispatch[nidt].cap.type != ObjType_Null) {
            printf("kernel: replacing handler for IRQ %d\n", nidt);
        }
        err = caps_copy_to_cte(&irq_dispatch[nidt], recv, false, 0, 0);

        // Correct interrupt forwarding by the distributor must be ensured
        // in userspace.
        
        return err;
    }

    return SYS_ERR_IRQ_INVALID;
}

errval_t irq_connect(struct capability *irq_dest, capaddr_t endpoint) {
    assert(irq_dest->type == ObjType_IRQDest);
    return irq_table_set(irq_dest->u.irqdest.vector, endpoint);
}

errval_t irq_table_delete(unsigned int nidt)
{
    if (nidt < NDISPATCH) {
        irq_dispatch[nidt].cap.type = ObjType_Null;

        /* todo: gic disable irq */

        return SYS_ERR_OK;
    }
    return SYS_ERR_IRQ_INVALID;
}

errval_t irq_table_alloc_dest_cap(uint8_t dcn_level, capaddr_t dcn,
                                  capaddr_t out_cap_addr, int vec_hint)
{
    errval_t err;
    if(vec_hint < 0){
        printk(LOG_WARN, "irq: vec_hint must be provided on ARM\n", vec_hint);
        return SYS_ERR_IRQ_INVALID;
    }
    if(vec_hint >= NDISPATCH){
        printk(LOG_WARN, "irq: vec_hint (%d) invalid\n", vec_hint);
        return SYS_ERR_IRQ_INVALID;
    }

    // TODO: Keep track of allocations 
    struct cte * cn;
    err = caps_lookup_slot(&dcb_current->cspace.cap, dcn, dcn_level,
                           &cn, CAPRIGHTS_WRITE);
    if(err_is_fail(err)){
        return err;
    }

    struct cte out_cap;
    memset(&out_cap, 0, sizeof(struct cte));
    out_cap.cap.type = ObjType_IRQDest;
    out_cap.cap.u.irqdest.cpu = my_core_id;
    out_cap.cap.u.irqdest.vector = vec_hint;
    caps_copy_to_cnode(cn, out_cap_addr, &out_cap, 0, 0, 0);
    return  SYS_ERR_OK;
}

errval_t irq_table_notify_domains(struct kcb *kcb)
{
    uintptr_t msg[] = { 1 };
    for (int i = 0; i < NDISPATCH; i++) {
        if (kcb->irq_dispatch[i].cap.type == ObjType_EndPointLMP) {
            struct capability *cap = &kcb->irq_dispatch[i].cap;
            // 1 word message as notification
            errval_t err = lmp_deliver_payload(cap, NULL, msg, 1, false, false);
            if (err_is_fail(err)) {
                if (err_no(err) == SYS_ERR_LMP_BUF_OVERFLOW) {
                    struct dispatcher_shared_generic *disp =
                        get_dispatcher_shared_generic(cap->u.endpointlmp.listener->disp);
                    printk(LOG_DEBUG, "%.*s: IRQ message buffer overflow\n",
                            DISP_NAME_LEN, disp->name);
                } else {
                    printk(LOG_ERR, "Unexpected error delivering IRQ\n");
                }
            }
        }
        kcb->irq_dispatch[i].cap.type = ObjType_Null;
    }
    return SYS_ERR_OK;
}

/**
 * \brief Send interrupt notification to user-space listener.
 *
 * Sends an interrupt notification IDC to a local endpoint that
 * listens for IRQ notifications.
 *
 * \param irq   IRQ# to send in notification.
 */
void send_user_interrupt(int irq)
{
    assert(irq >= 0 && irq < NDISPATCH);
    struct capability *cap = &irq_dispatch[irq].cap;

    // Return on null cap (unhandled interrupt)
    if (cap->type == ObjType_Null) {
        printk(LOG_WARN, "unhandled IRQ %d\n", irq);
        return;
    }

    // Otherwise, cap needs to be an endpoint
    assert(cap->type == ObjType_EndPointLMP);
    errval_t err = lmp_deliver_notification(cap);
    if (err_is_fail(err)) {
        if (err_no(err) == SYS_ERR_LMP_BUF_OVERFLOW) {
            struct dispatcher_shared_generic *disp =
                    get_dispatcher_shared_generic(
                            cap->u.endpointlmp.listener->disp);
            printk(LOG_DEBUG, "%.*s: IRQ message buffer overflow\n",
                    DISP_NAME_LEN, disp->name);
        } else {
            printk(LOG_ERR, "Unexpected error delivering IRQ\n");
        }
    }

#ifdef SCHEDULER_RR
    /* XXX: run the handler dispatcher immediately
     * we shouldn't do this (we should let the scheduler decide), but because
     * our default scheduler is braindead, this is a quick hack to make sure
     * that mostly-sane things happen
     */
    dispatch(cap->u.endpointlmp.listener);
#else
    dispatch(schedule());
#endif
}
