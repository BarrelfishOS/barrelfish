/**
 * \file
 * \brief Kernel management of dispatchers (implementation).
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <barrelfish_kpi/cpu.h>
#include <exec.h> /* XXX wait_for_interrupt, resume, execute */
#include <paging_kernel_arch.h>
#include <dispatch.h>
#include <kcb.h>
#include <wakeup.h>
#include <systime.h>
#include <barrelfish_kpi/syscalls.h>
#include <barrelfish_kpi/lmp.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>
#include <barrelfish_kpi/dispatcher_shared_target.h>
#include <barrelfish_kpi/cpu_arch.h>
#include <barrelfish_kpi/registers_arch.h>

#include <bitmacros.h>

#if defined(__x86_64__) || defined(__i386__)
#  include <arch/x86/apic.h>
#endif

#if defined(__x86_64__) && !defined(__k1om__)
#  include <vmkit.h>
#endif


/**
 * \brief The kernel timeslice given in system ticks
 */
systime_t kernel_timeslice;

unsigned int config_timeslice = CONFIG_TIMESLICE;

/// Counter for number of context switches
uint64_t context_switch_counter = 0;

/// Current execution dispatcher (when in system call or exception)
struct dcb *dcb_current = NULL;

#if CONFIG_TRACE && NETWORK_STACK_BENCHMARK
#define TRACE_N_BM 1
#endif // CONFIG_TRACE && NETWORK_STACK_BENCHMARK


void __attribute__ ((noreturn)) dispatch(struct dcb *dcb)
{
    // XXX FIXME: Why is this null pointer check on the fast path ?
    // If we have nothing to do we should call something other than dispatch
    if (dcb == NULL) {
        dcb_current = NULL;
        wait_for_interrupt();
    }

    // Don't context switch if we are current already
    if (dcb_current != dcb) {

#ifdef TRACE_CSWITCH
        trace_event(TRACE_SUBSYS_KERNEL,
                    TRACE_EVENT_KERNEL_CSWITCH,
                    (uint32_t)(lvaddr_t)dcb & 0xFFFFFFFF);
#endif

        context_switch(dcb);
        dcb_current = dcb;
    }

    assert(dcb != NULL);

    dispatcher_handle_t handle = dcb->disp;
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(handle);
    arch_registers_state_t *disabled_area =
        dispatcher_get_disabled_save_area(handle);

    if (disp != NULL) {
        disp->systime = systime_now() + kcb_current->kernel_off;
    }
    TRACE(KERNEL, SC_YIELD, 1);

    if (dcb->disabled) {
        if (disp != NULL) {
            debug(SUBSYS_DISPATCH, "resume %.*s at 0x%" PRIx64 ", %s\n",
		  DISP_NAME_LEN, disp->name,
		  (uint64_t)registers_get_ip(disabled_area),
		  disp->disabled ? "disp->disabled" : "disp->enabled"
		);
        }

#if defined(__x86_64__) && !defined(__k1om__)
        if(!dcb->is_vm_guest) {
            resume(disabled_area);
        } else {
            vmkit_vmenter(dcb);
        }
#else
        resume(disabled_area);
#endif
    } else {
        if (disp != NULL) {
            debug(SUBSYS_DISPATCH, "dispatch %.*s\n", DISP_NAME_LEN, disp->name);
            assert(disp->dispatcher_run != 0);
            disp->disabled = true;
        }
#if defined(__x86_64__) && !defined(__k1om__)
        if(!dcb->is_vm_guest) {
            execute(disp->dispatcher_run);
        } else {
            vmkit_vmexec(dcb, (disp) ? disp->dispatcher_run : 0);
        }
#else
        execute(disp->dispatcher_run);
#endif
    }
} // end function: dispatch

/**
 * \brief Transfer cap from 'send' to 'ep', according to 'msg'.
 *
 * Reads the cap transfer spec in the LMP message 'msg' and transfers
 * the cap from CSpace in DCB 'send' accordingly.
 *
 * \param ep    Endpoint capability of destination
 * \param send  Pointer to sending DCB.
 * \param send_cptr Address of capability in sender's cspace
 * \param send_level Depth/level of capability in sender's cspace
 *
 * \return      Error code
 */
static errval_t lmp_transfer_cap(struct capability *ep, struct dcb *send,
                                 capaddr_t send_cptr, uint8_t send_level,
                                 bool give_away)
{
    errval_t err;
    /* Parameter checking */
    assert(send_cptr != CPTR_NULL);
    assert(send != NULL);
    assert(ep != NULL);
    assert(ep->type == ObjType_EndPointLMP);
    struct dcb *recv = ep->u.endpointlmp.listener;
    assert(recv != NULL);
    assert(ep->u.endpointlmp.epoffset != 0);

    // printk(LOG_NOTE, "%s: ep->u.endpointlmp.epoffset = %"PRIuLVADDR"\n", __FUNCTION__, ep->u.endpointlmp.epoffset);
    /* Look up the slot receiver can receive caps in */
    struct lmp_endpoint_kern *recv_ep
        = (void *)((uint8_t *)recv->disp + ep->u.endpointlmp.epoffset);

    // Lookup cspace root for receiving
    struct capability *recv_cspace_cap;
    // XXX: do we want a level into receiver's cspace here?
    // printk(LOG_NOTE, "recv_cspace_ptr = %"PRIxCADDR"\n", recv_ep->recv_cspc);
    err = caps_lookup_cap(&recv->cspace.cap, recv_ep->recv_cspc, 2,
                          &recv_cspace_cap, CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err) || recv_cspace_cap->type != ObjType_L1CNode) {
        return SYS_ERR_LMP_CAPTRANSFER_DST_CNODE_INVALID;
    }
    // Check index into L1 cnode
    capaddr_t l1index = recv_ep->recv_cptr >> L2_CNODE_BITS;
    if (l1index >= cnode_get_slots(recv_cspace_cap)) {
        return SYS_ERR_LMP_CAPTRANSFER_DST_CNODE_INVALID;
    }
    // Get the cnode
    struct cte *recv_cnode_cte = caps_locate_slot(get_address(recv_cspace_cap),
                                                  l1index);
    struct capability *recv_cnode_cap = &recv_cnode_cte->cap;
    // Check for cnode type
    if (recv_cnode_cap->type != ObjType_L2CNode) {
        return SYS_ERR_LMP_CAPTRANSFER_DST_CNODE_INVALID;
    }
    // The slot within the cnode
    struct cte *recv_cte;
    recv_cte = caps_locate_slot(get_address(recv_cnode_cap),
                                recv_ep->recv_cptr & MASK(L2_CNODE_BITS));

    /* Look up source slot in sender */
    struct cte *send_cte;
    err = caps_lookup_slot(&send->cspace.cap, send_cptr, send_level,
                           &send_cte, CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return err_push(err, SYS_ERR_LMP_CAPTRANSFER_SRC_LOOKUP);
    }

    /* Is destination empty */
    if (recv_cte->cap.type != ObjType_Null) {
        debug(SUBSYS_DISPATCH, "%s: dest slot occupied\n", __FUNCTION__);
        return SYS_ERR_LMP_CAPTRANSFER_DST_SLOT_OCCUPIED;
    }

    //caps_trace(__func__, __LINE__, send_cte, "transferring");
    //TRACE_CAP_MSG("transferring", send_cte);

    /* Insert send cap into recv cap */
    err = caps_copy_to_cte(recv_cte, send_cte, false, 0, 0);
    assert(err_is_ok(err)); // Cannot fail after checking that slot is empty

    if (give_away) {
        err = caps_delete(send_cte);
        if (err_is_fail(err)) {
            printk(LOG_NOTE, "deleting source of lmp captransfer failed: %"PRIuERRV"\n", err);
        }
        assert(err_is_ok(err)); // A copy now exists in the recv slot, so this
                                // should not fail
    }

    return SYS_ERR_OK;
}

/**
 * \brief Check if it would be possible to deliver LMP payload, but do not deliver it
 *
 * \param ep     Endpoint capability to send to
 * \param payload_len Length (in number of words) of payload
 */
errval_t lmp_can_deliver_payload(struct capability *ep,
                                 size_t payload_len)
{
    assert(ep != NULL);
    assert(ep->type == ObjType_EndPointLMP);
    struct dcb *recv = ep->u.endpointlmp.listener;
    assert(recv != NULL);

    /* check that receiver exists and has specified an endpoint buffer */
    if (recv->disp == 0 || ep->u.endpointlmp.epoffset == 0) {
        return SYS_ERR_LMP_NO_TARGET;
    }

    /* locate receiver's endpoint buffer */
    struct lmp_endpoint_kern *recv_ep
        = (void *)((uint8_t *)recv->disp + ep->u.endpointlmp.epoffset);

    /* check delivered/consumed state */
    uint32_t epbuflen = ep->u.endpointlmp.epbuflen;
    uint32_t pos = recv_ep->delivered;
    uint32_t consumed = recv_ep->consumed;
    if (pos >= epbuflen || consumed >= epbuflen) {
        return SYS_ERR_LMP_EP_STATE_INVALID;
    }

    /* compute space available in endpoint */
    uint32_t epspace;
    if (pos >= consumed) {
        epspace = epbuflen - (pos - consumed);
    } else {
        epspace = consumed - pos;
    }

    /* Check if there's enough space for another msg.
     * We always keep one word free, to avoid having the special case where
     * delivered == consumed may mean the buffer is both completely full and
     * completely empty */
    if (epspace <= payload_len + LMP_RECV_HEADER_LENGTH) {
        return SYS_ERR_LMP_BUF_OVERFLOW;
    }

    return SYS_ERR_OK;
}

/**
 * \brief Deliver the payload of an LMP message to a dispatcher.
 *
 * \param ep     Endpoint capability to send to
 * \param send   DCB of the sender. Can be NULL for kernel-originated messages
 * \param payload     Message payload
 * \param payload_len Length (in number of words) of payload
 * \param captransfer True iff a cap has also been delivered
 *
 * \return Error code
 */
errval_t lmp_deliver_payload(struct capability *ep, struct dcb *send,
                             uintptr_t *payload, size_t payload_len,
                             bool captransfer, bool now)
{
    assert(ep != NULL);
    assert(ep->type == ObjType_EndPointLMP);
    struct dcb *recv = ep->u.endpointlmp.listener;
    assert(recv != NULL);
    assert(payload != NULL || payload_len == 0);

    errval_t err;

    err = lmp_can_deliver_payload(ep, payload_len);
    if (err_is_fail(err)) {
        return err;
    }

    /* locate receiver's endpoint buffer */
    struct lmp_endpoint_kern *recv_ep
        = (void *)((uint8_t *)recv->disp + ep->u.endpointlmp.epoffset);

    /* read current pos and buflen */
    uint32_t epbuflen = ep->u.endpointlmp.epbuflen;
    uint32_t pos = recv_ep->delivered;

    struct dispatcher_shared_generic *send_disp =
        send ? get_dispatcher_shared_generic(send->disp) : NULL;
    struct dispatcher_shared_generic *recv_disp =
        get_dispatcher_shared_generic(recv->disp);
    debug(SUBSYS_DISPATCH, "LMP %.*s -> %.*s\n",
          DISP_NAME_LEN, send ? send_disp->name : "kernel",
          DISP_NAME_LEN, recv_disp->name);

    // Setup receiver's message flags
    union lmp_recv_header recvheader = { .raw = 0 };
    recvheader.x.flags.captransfer = captransfer;
    recvheader.x.length = payload_len;

    /* Deliver header */
    recv_ep->buf[pos] = recvheader.raw;
    if (++pos == epbuflen) {
        pos = 0;
    }

    /* Transfer the msg */
    for(int i = 0; i < payload_len; i++) {
        recv_ep->buf[pos] = payload[i];
        if (++pos == epbuflen) {
            pos = 0;
        }
    }

    // update the delivered pos
    recv_ep->delivered = pos;

    // tell the dispatcher that it has an outstanding message in one of its EPs
    recv_disp->lmp_delivered += payload_len + LMP_RECV_HEADER_LENGTH;

    // ... and give it a hint which one to look at
    recv_disp->lmp_hint = ep->u.endpointlmp.epoffset;

    // Make target runnable
    make_runnable(recv);
    if (now)
        schedule_now(recv);

    return SYS_ERR_OK;
}

/**
 * \brief Deliver an LMP message to a dispatcher.
 *
 * \param ep     Endpoint capability to send to
 * \param send   DCB of the sender. Can be NULL for kernel-originated messages
 * \param payload Buffer containing message payload
 * \param len    Length of message payload, as number of words
 * \param send_cptr Capability to be transferred with LMP
 * \param send_level CSpace level of cptr
 */
errval_t lmp_deliver(struct capability *ep, struct dcb *send,
                     uintptr_t *payload, size_t len,
                     capaddr_t send_cptr, uint8_t send_level, bool give_away)
{
    bool captransfer;
    assert(ep != NULL);
    assert(ep->type == ObjType_EndPointLMP);
    struct dcb *recv = ep->u.endpointlmp.listener;
    assert(recv != NULL);
    assert(payload != NULL);

    errval_t err;

    /* Is the sender trying to send a cap? */
    if (send_cptr != CPTR_NULL) {
        /* Don't attempt to transfer the cap if we can't send the payload */
        err = lmp_can_deliver_payload(ep, len);
        if (err_is_fail(err)) {
            return err;
        }

        err = lmp_transfer_cap(ep, send, send_cptr, send_level, give_away);
        if (err_is_fail(err)) {
            return err;
        }

        captransfer = true;
    } else {
        captransfer = false;
    }

    /* Send msg */
    err = lmp_deliver_payload(ep, send, payload, len, captransfer, false);
    // shouldn't fail, if we delivered the cap successfully
    assert(!(captransfer && err_is_fail(err)));
    return err;
}
