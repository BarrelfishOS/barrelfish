/**
 * \file
 * \brief User and kernel code definitions for system-wide tracing
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_TRACE_H
#define LIBBARRELFISH_TRACE_H

#if defined(__x86_64__) || defined(__BEEHIVE__)
#define TRACING_EXISTS 1
#endif


#ifdef __x86_64__
#ifndef IN_KERNEL
/* XXX: private includes from libbarrelfish */
#include <barrelfish/dispatcher_arch.h>
#include <barrelfish/curdispatcher_arch.h>
#else // IN_KERNEL
#include <arch/x86/apic.h> // XXX!
#endif // IN_KERNEL
#endif // __x86_64__

#ifdef __BEEHIVE__
#include <bench/bench_arch.h>
#ifndef IN_KERNEL
/* XXX: private includes from libbarrelfish */
#include <barrelfish/dispatcher_arch.h>
#include <barrelfish/curdispatcher_arch.h>
#include <barrelfish/syscall_arch.h>
#endif // IN_KERNEL
#endif // __BEEHIVE__

#include <barrelfish/sys_debug.h>

/*
 * turn some tracing on or off
 */
#define TRACE_THREADS
#define TRACE_CSWITCH

/* Trace only network related events
 * This will reduce the amount of events recorded, and hence allows
 * recording for longer time. */
#if CONFIG_TRACE && NETWORK_STACK_TRACE
//#define TRACE_ONLY_SUB_NET 1
#endif // CONFIG_TRACE && NETWORK_STACK_TRACE

#if CONFIG_TRACE && NETWORK_STACK_BENCHMARK
#define TRACE_ONLY_SUB_BNET 1
#endif // CONFIG_TRACE && NETWORK_STACK_BENCHMARK


/**
 * \brief Constants for trace subsystems and events.
 *
 * Please try and keep this under control.  Each subsystem should be
 * preceeded by two blank lines and followed by its events.  If events
 * within a subsystem need to be grouped with whitespace then use a
 * single blank line.
 *
 */
#define TRACE_SUBSYS_KERNEL             0xFFFF
#define TRACE_EVENT_CSWITCH             0xCCCC
#define TRACE_EVENT_BZERO               0xB0
#define TRACE_EVENT_TIMER               0x1
#define TRACE_EVENT_TIMER_SYNC          0x2

#define TRACE_EVENT_SCHED_MAKE_RUNNABLE 0xED00
#define TRACE_EVENT_SCHED_REMOVE        0xED01
#define TRACE_EVENT_SCHED_YIELD         0xED02
#define TRACE_EVENT_SCHED_SCHEDULE      0xED03
#define TRACE_EVENT_SCHED_CURRENT       0xED04

#define TRACE_EVENT_BMP_RX              0xBEE1
#define TRACE_EVENT_BMP_PRE_DELIVER     0xBEE2
#define TRACE_EVENT_BMP_POST_DELIVER    0xBEE3
#define TRACE_EVENT_BMP_PUMP            0xBEE4
#define TRACE_EVENT_BMP_SEND            0xBEE5


#define TRACE_SUBSYS_THREADS            0xEEEE

#define TRACE_EVENT_BARRIER_ENTER       0x0100
#define TRACE_EVENT_BARRIER_LEAVE       0x0101

#define TRACE_EVENT_MUTEX_LOCK_ENTER    0x0200
#define TRACE_EVENT_MUTEX_LOCK_LEAVE    0x0201
#define TRACE_EVENT_MUTEX_LOCK_NESTED_ENTER    0x0202
#define TRACE_EVENT_MUTEX_LOCK_NESTED_LEAVE    0x0203
#define TRACE_EVENT_MUTEX_TRYLOCK       0x0204
#define TRACE_EVENT_MUTEX_UNLOCK        0x0205

#define TRACE_EVENT_COND_WAIT_ENTER     0x0300
#define TRACE_EVENT_COND_WAIT_LEAVE     0x0301
#define TRACE_EVENT_COND_SIGNAL         0x0302
#define TRACE_EVENT_COND_BROADCAST      0x0303

#define TRACE_EVENT_SEM_WAIT_ENTER      0x0400
#define TRACE_EVENT_SEM_WAIT_LEAVE      0x0401
#define TRACE_EVENT_SEM_TRYWAIT         0x0402
#define TRACE_EVENT_SEM_POST            0x0403


#define TRACE_SUBSYS_MEMSERV 0xA000
#define TRACE_EVENT_ALLOC    0x0001


#define TRACE_SUBSYS_MONITOR 0xB000
#define TRACE_EVENT_SPAN0    0x0000
#define TRACE_EVENT_SPAN1    0x0001
#define TRACE_EVENT_SPAN     0x0002
#define TRACE_EVENT_PCREQ    0x0003
#define TRACE_EVENT_PCREPLY  0x0004
#define TRACE_EVENT_PCREQ_INTER   0x0005
#define TRACE_EVENT_PCREPLY_INTER 0x0006
#define TRACE_EVENT_URPC_BLOCK   0x0007
#define TRACE_EVENT_URPC_UNBLOCK 0x0008
#define TRACE_EVENT_REMOTE_CAP_RETYPE 0x0009
#define TRACE_EVENT_REMOTE_CAP_RETYPE_RETRY 0x0010
#define TRACE_EVENT_REMOTE_CAP_RETYPE_MSG   0x0011
#define TRACE_EVENT_REMOTE_CAP_RETYPE_END   0x0012
#define TRACE_EVENT_POLLING  0xBBBB


#define TRACE_SUBSYS_CHIPS   0xC000
#define TRACE_EVENT_CHIPS_LISTENCB 0x0001


#define TRACE_SUBSYS_BFLIB   0xBFBF


#define TRACE_SUBSYS_TWEED        0x2000
#define TRACE_EVENT_TWEED_START   0x0000
#define TRACE_EVENT_TWEED_END     0x0001
#define TRACE_EVENT_STEAL         0x0002
#define TRACE_EVENT_STEAL_END     0x0003
#define TRACE_EVENT_WAIT          0x0004
#define TRACE_EVENT_WAIT_END      0x0005
#define TRACE_EVENT_LOCKING       0x0006
#define TRACE_EVENT_LOCKING_END   0x0007


#define TRACE_SUBSYS_ROUTE                0x3000
#define TRACE_EVENT_BCAST_WITH_CCAST_SEND 0x0001
#define TRACE_EVENT_BCAST_WITH_CCAST      0x0002
#define TRACE_EVENT_RECV_BCAST_WITH_CCAST 0x0003
#define TRACE_EVENT_RECV_CCAST            0x0004
#define TRACE_EVENT_ROUTE_BENCH_START     0x0005
#define TRACE_EVENT_ROUTE_BENCH_STOP      0x0006
#define TRACE_EVENT_ROUTE_SEND_PING       0x0007
#define TRACE_EVENT_ROUTE_SEND_PONG       0x0008
#define TRACE_EVENT_ROUTE_RECV_PING       0x0009
#define TRACE_EVENT_ROUTE_RECV_PONG       0x000A
#define TRACE_EVENT_ROUTE_POLL            0x000B


#define TRACE_SUBSYS_BENCH        0x1234
#define TRACE_EVENT_PCBENCH       0x0000
#define TRACE_EVENT_RXPING        0x0001
#define TRACE_EVENT_RXPONG        0x0002


#define TRACE_SUBSYS_BOMP               0x4000
#define TRACE_EVENT_BOMP_START          0x0001
#define TRACE_EVENT_BOMP_STOP           0x0002
#define TRACE_EVENT_BOMP_ITER           0x0003


#define TRACE_SUBSYS_BARRIERS                0x5000
#define TRACE_EVENT_BARRIERS_START           0X0001
#define TRACE_EVENT_BARRIERS_STOP            0X0002
#define TRACE_EVENT_BARRIERS_BARRIER_WAIT    0X0003
#define TRACE_EVENT_BARRIERS_CENTRAL_REQ     0X0004
#define TRACE_EVENT_BARRIERS_CENTRAL_REP     0X0005
#define TRACE_EVENT_BARRIERS_TREE_REQ        0X0006
#define TRACE_EVENT_BARRIERS_TREE_REP        0X0007
#define TRACE_EVENT_BARRIERS_DIST            0X0008
#define TRACE_EVENT_BARRIERS_SEND            0X0009
#define TRACE_EVENT_BARRIERS_POLL1           0X000A
#define TRACE_EVENT_BARRIERS_POLL2           0X000B
#define TRACE_EVENT_BARRIERS_HEAP_REQ        0X000C
#define TRACE_EVENT_BARRIERS_HEAP_REP        0X000D
#define TRACE_EVENT_BARRIERS_SEQ_BCAST_REQ   0X000E
#define TRACE_EVENT_BARRIERS_SEQ_BCAST_RECV  0X000F
#define TRACE_EVENT_BARRIERS_TREE_BCAST_REQ  0X0010
#define TRACE_EVENT_BARRIERS_TREE_BCAST_RECV 0X0011

/* Following constants are used in network subsystem. */
#define TRACE_SUBSYS_NET                    0x6000
#define TRACE_EVENT_NET_START               0X0001
#define TRACE_EVENT_NET_STOP                0X0002
#define TRACE_EVENT_NET_NI_AI               0X0012 /* added, 0 */
#define TRACE_EVENT_NET_NI_I                0X0010 /* added, 0 */
#define TRACE_EVENT_NET_NI_A                0X0003 /* added, pkt data location */
#define TRACE_EVENT_NET_NI_FILTER_FRAG      0X0018 /* added, pkt data location */
#define TRACE_EVENT_NET_NI_FILTER_EX_1      0X0015 /* added, pkt data location */
#define TRACE_EVENT_NET_NI_ARP              0X0011 /* added, pkt data location */
#define TRACE_EVENT_NET_NI_FILTER_EX_2      0X0016 /* added, pkt data location */
#define TRACE_EVENT_NET_NI_PKT_CPY_1          0X0019 /* added, pkt data location */
#define TRACE_EVENT_NET_NI_PKT_CPY_2          0X001A /* added, pkt data location */
#define TRACE_EVENT_NET_NI_PKT_CPY_3          0X001B /* added, pkt data location */
#define TRACE_EVENT_NET_NI_PKT_CPY_4          0X001C /* added, pkt data location */

#define TRACE_EVENT_NET_NI_PKT_CPY          0X0017 /* added, pkt data location */
#define TRACE_EVENT_NET_NI_P                0X0004 /* added, pbuf_id */
#define TRACE_EVENT_NET_NI_S                0X0005 /* added, pbuf_id */
#define TRACE_EVENT_NET_AI_A                0X0006 /* added, pbuf_id */
#define TRACE_EVENT_NET_AI_P                0X0007 /* added, pbuf_addr */
#define TRACE_EVENT_NET_AO_C                0X0008 /* added, pbuf_addr */
#define TRACE_EVENT_NET_AO_Q                0X0009 /* added, pbuf_addr */
#define TRACE_EVENT_NET_AO_S                0X000A /* added, pbuf_addr (as client_data ) */
#define TRACE_EVENT_NET_NO_A                0X000B /* added, client_data (pbuf_address in lwip) */
#define TRACE_EVENT_NET_NO_S                0X000C /* added, e1000n.c client_data (pbuf_address in lwip) */

/* FIXME: Add the timings of when does NIC gets TX_done */
#define TRACE_EVENT_NET_NO_TXD              0X0013 /* yet to add */
#define TRACE_EVENT_NET_AIR_R               0x000E /* added, pbuf_addr (TX DONE in app) */

/* Response flow */
#define TRACE_EVENT_NET_AOR_S               0x000D /* added, pbuf_id ( register_pbuf from APP)*/
#define TRACE_EVENT_NET_NIR_REG_PBUF        0x0014 /* commented pbuf_id ( register_pbuf in NIC)*/

#define TRACE_SUBSYS_MULTIHOP		     0x7000
#define TRACE_EVENT_MULTIHOP_BENCH_START     0x0001
#define TRACE_EVENT_MULTIHOP_BENCH_STOP      0x0002
#define TRACE_EVENT_MULTIHOP_MESSAGE_SEND    0x0003
#define TRACE_EVENT_MULTIHOP_MESSAGE_RECEIVE 0x0004

/* Following constants are used in network benchmark. */
#define TRACE_SUBSYS_BNET                    0x8000
#define TRACE_EVENT_BNET_START               0x0001
#define TRACE_EVENT_BNET_STOP                0x0002
#define TRACE_EVENT_BNET_DRV_SEE             0x0003
#define TRACE_EVENT_BNET_APP_SEE             0x0004
#define TRACE_EVENT_BNET_DRV_INT             0x0005
#define TRACE_EVENT_BNET_DRV_POLL            0x0006
#define TRACE_EVENT_BNET_YIELD               0x0007
#define TRACE_EVENT_BNET_I                   0x0008

#define TRACE_EVENT(s,e,a) ((uint64_t)(s)<<48|(uint64_t)(e)<<32|(a))

#define TRACE_EVENT_SIZE          16
#define TRACE_MAX_EVENTS          8000        // max number of events
#define TRACE_PERCORE_BUF_SIZE    0x1ff00
// (TRACE_EVENT_SIZE * TRACE_MAX_EVENTS + (sizeof (struct trace_buffer flags)))

#define TRACE_BUF_SIZE (MAX_CPUS*TRACE_PERCORE_BUF_SIZE)


#if defined(__x86_64__)
#define TRACE_TIMESTAMP() rdtsc()

#define xapic_none 0x00
#define xapic_self 0x01
#define xapic_all_inc 0x02
#define xapic_all_exc 0x03
#define IPI_TRACE_COMPLETE 62
#define IPI_TRACE_START 63
#define TRACE_COMPLETE_IPI_IRQ    (62-32)

/*
 * \brief compare and set. If the value at address
 *        equals old, set it to new and return true,
 *        otherwise don't write to address and return false
 *
 * NOTE: This is only used by threads on the same core so no lock prefix
 */
static inline bool trace_cas(volatile uintptr_t *address, uintptr_t old,
        uintptr_t nw)
{
    register bool res;
    __asm volatile("cmpxchgq %2,%0     \n\t"
                   "setz %1                  \n\t"
                   : "+m" (*address), "=q" (res)
                   : "r" (nw), "a" (old)
                   : "memory");
    return res;
}


#elif defined(__i386__)


static inline bool trace_cas(volatile uintptr_t *address, uintptr_t old,
        uintptr_t nw)
{
    return false;
}

#define TRACE_TIMESTAMP() rdtsc()


#elif defined(__arm__)


static inline bool trace_cas(volatile uintptr_t *address, uintptr_t old,
        uintptr_t nw)
{
    return false;
}

#define TRACE_TIMESTAMP() 0


#elif defined(__BEEHIVE__)

static inline bool trace_cas(volatile uintptr_t *address, uintptr_t old,
        uintptr_t nw)
{
    uintptr_t discardable;
    uintptr_t difference; // 0 if old was still old
    /* We write the new value somewhere.  Possibly to *address and
     * possibly to discardable.  Putting the new value in wq while we
     * do the test causes this to be (single core) atomic. */
    __asm volatile("ld wq, %[nw]\n\t"
		   "aqr_ld zero, %[addr]\n\t"
		   "sub %[diff], rq, %[old]\n\t"
		   "jnz . + 3\n\t"
		   "aqw_ld zero, %[addr]\n\t"
		   "j . + 2\n\t"
		   "aqw_ld zero, %[discard]\n"
		   : [diff] "=&r" (difference)
		   : [addr] "r" (address)
		   , [nw] "r" (nw)
		   , [old] "r" (old)
		   , [discard] "r" (&discardable)
		   : "memory", "cc");
    return (difference == 0);
}

#define TRACE_TIMESTAMP() bench_tsc()

static inline int arch_is_simulator(void)
{
    volatile unsigned int *ptr = (void*)0x02;
    unsigned int val = *ptr;

    val >>= 18;
    val &= 0x7f;
    return val == 2 ? 1 : 0;
}

extern void simtrace(uint32_t word1, uint32_t word2);


__asm("_simtrace: simctrl 11; j link");



#else

#warning You need to supply CAS and a timestamp function for this architecture.

#endif


/// Trace event
struct trace_event {
    uint64_t timestamp;
    union {
        uint64_t raw;
        // ... stuff that is embedded in the event
	struct {
	    uint32_t word1;
	    uint32_t word2;
	} w32;
        struct {
            uint32_t arg;
            uint16_t event;
            uint16_t subsystem;
        } ev;
    } u;
};

/// Trace buffer
struct trace_buffer {
    volatile uintptr_t head_index;
    volatile uintptr_t tail_index;

    // ... flags...
    struct trace_buffer *master;       // Pointer to the trace master
    volatile bool     running;
    volatile bool     done_rundown;    // Core specific
    volatile uint64_t start_trigger;
    volatile uint64_t stop_trigger;
    volatile uint64_t stop_time;
    uint64_t          t0;              // Start time of trace
    uint64_t          duration;        // Max trace duration
    uint8_t           ipi_dest;        // Core for completion IPI

    // ... events ...
    struct trace_event events[TRACE_MAX_EVENTS];
};

#ifndef IN_KERNEL

extern lvaddr_t trace_buffer_master;
extern lvaddr_t trace_buffer_va;
struct cnoderef;

errval_t trace_init(void);
errval_t trace_disable_domain(void);
void trace_reset_buffer(void);
void trace_reset_all(void);
errval_t trace_setup_on_core(struct capref *retcap);
errval_t trace_setup_child(struct cnoderef taskcn,
                           dispatcher_handle_t handle);
errval_t trace_control(uint64_t start_trigger,
                       uint64_t stop_trigger,
                       uint64_t duration);
errval_t trace_wait(void);
size_t trace_dump(char *buf, size_t buflen);
errval_t trace_my_setup(void);

/**
 * \brief Compute fixed trace buffer address according to
 * given core_id
 *
 * Each core gets its own top-level page table entry, so to use a
 * fixed address need to compute it as an offset from core id.
 *
 * Once we've computed this for a domain, we store it in
 * the dispatcher.
 *
 */
static inline lvaddr_t compute_trace_buf_addr(uint8_t core_id)
{
    lvaddr_t addr = trace_buffer_master + (core_id * TRACE_PERCORE_BUF_SIZE);

    return addr;
}

#endif

void trace_init_disp(void);

/**
 * \brief Reserve a slot in the trace buffer and write the event.
 *
 * Returns the slot index that was written.
 * Lock-free implementation.
 *
 */
static inline uintptr_t trace_reserve_and_fill_slot(struct trace_event *ev,
                                                   struct trace_buffer *buf)
{
#ifdef __BEEHIVE__
    /* Top bit is rundown event which must be reprocessed later */
    if (arch_is_simulator() && ((ev->timestamp >> 63) == 0)) {
	simtrace(ev->u.w32.word1, ev->u.w32.word2);
	return SYS_ERR_OK;
    }
#endif
    uintptr_t i, nw;
    struct trace_event *slot;

    do {
        i = buf->head_index;

        /* XXX For now we're going to stop tracing if the buffer fills up
           This will give us a useful one-shot tracing mode, rather than
           a continuous circular buffer */
        if (i == TRACE_MAX_EVENTS) return i;
        nw = i + 1;
        //nw = (i + 1) % TRACE_MAX_EVENTS;

    } while (!trace_cas(&buf->head_index, i, nw));

    // Write the event
    slot = &buf->events[i];
    *slot = *ev;

    return i;
}

/**
 * \brief Write a trace event to the buffer for the current core.
 *
 * Tracing must have been set up by parent of the current domain
 * (by calling trace_setup_child).
 *
 * The per-core buffer must have already been initialized by
 * the monitor (by calling trace_setup_on_core).
 */

#ifdef IN_KERNEL

static inline coreid_t get_my_core_id(void)
{
    return my_core_id;
}

// Kernel-version: uses the global trace buffer variable
static inline errval_t trace_write_event(struct trace_event *ev)
{
#ifdef TRACING_EXISTS
    struct trace_buffer *master = (struct trace_buffer *)kernel_trace_buf;

    if (kernel_trace_buf == 0) {
        return TRACE_ERR_NO_BUFFER;
    }

    if (!master->running) {
        if (ev->u.raw == master->start_trigger) {
            master->start_trigger = 0;
            master->t0 = ev->timestamp;
            if (master->duration)
                master->stop_time = ev->timestamp + master->duration;
            master->running = true;
        } else {
            return SYS_ERR_OK;
        }
    }
    struct trace_buffer *trace_buf = (struct trace_buffer*) (kernel_trace_buf
            + my_core_id * TRACE_PERCORE_BUF_SIZE);
    if (!trace_buf->done_rundown) {
	/* We set it before we trace_snapshot since we only want one,
	 * and in particular because trace_snapshot uses
	 * trace_write_event... */
	trace_buf->done_rundown = true;
#ifdef __BEEHIVE__
	trace_snapshot();
#endif
    }
    (void) trace_reserve_and_fill_slot(ev, trace_buf);

    if (ev->u.raw == master->stop_trigger ||
        (ev->timestamp>>63 == 0 &&  // Not a DCB event
         ev->timestamp > master->stop_time)) {
        master->stop_trigger = 0;
        master->running = false;
#ifdef __x86_64__
        apic_send_std_ipi(master->ipi_dest, xapic_none, IPI_TRACE_COMPLETE);
#endif
    }
#endif // __x86_64__
    return SYS_ERR_OK;
}
#else

static inline coreid_t get_my_core_id(void)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic *disp = get_dispatcher_generic(handle);
    return disp->core_id;
}

// User-space version: gets trace buffer pointer out of the current dispatcher
static inline errval_t trace_write_event(struct trace_event *ev)
{
#ifdef TRACING_EXISTS
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic *disp = get_dispatcher_generic(handle);
    struct trace_buffer *trace_buf = disp->trace_buf;

    if (trace_buf == NULL) {
        return TRACE_ERR_NO_BUFFER;
    }

    struct trace_buffer *master = (struct trace_buffer*)trace_buffer_master;
    //struct trace_buffer *master = trace_buf->master;
    if (master == NULL) {
        return TRACE_ERR_NO_BUFFER;
    }

    if (!master->running) {
        if (ev->u.raw == master->start_trigger) {
            master->start_trigger = 0;
            master->t0 = ev->timestamp;
            if (master->duration != 0) {
                master->stop_time = master->t0 + master->duration;
            }
            master->running = true;

            // Make sure the trigger event is first in the buffer
            (void) trace_reserve_and_fill_slot(ev, trace_buf);
#ifdef __x86_64__
            sys_debug_send_ipi(xapic_none, xapic_all_inc, IPI_TRACE_START);
#endif
            return SYS_ERR_OK;

        } else {
            return SYS_ERR_OK;
        }
    }
    if (!trace_buf->done_rundown) {
	/* We set it before we trace_snapshot since we only want one,
	 * and in particular because sys_rundown uses
	 * trace_write_event... */
	trace_buf->done_rundown = true;
#ifdef __BEEHIVE__
	(void) sys_rundown();
#endif
    }
    (void) trace_reserve_and_fill_slot(ev, trace_buf);

    if (ev->u.raw == master->stop_trigger ||
        ev->timestamp > master->stop_time) {
        master->stop_trigger = 0;
        master->running = false;
#ifdef __x86_64__
        sys_debug_send_ipi(master->ipi_dest, xapic_none, IPI_TRACE_COMPLETE);
#endif
    }
#endif // __x86_64__

    return SYS_ERR_OK;
}



#endif



/**
 * \brief Convenience wrapper to Write a trace event
 *
 */
static inline errval_t trace_event_raw(uint64_t raw)
{
#ifdef CONFIG_TRACE

#if TRACE_ONLY_SUB_NET
	/* we do not want the stats about actual messages sent */
	return SYS_ERR_OK;
#endif // TRACE_ONLY_SUB_NET


    struct trace_event ev;
    ev.timestamp = TRACE_TIMESTAMP();
    ev.u.raw = raw;
    return trace_write_event(&ev);
#else
    return SYS_ERR_OK;
#endif
}

static inline errval_t trace_event(uint16_t subsys, uint16_t event, uint32_t arg)
{
#ifdef CONFIG_TRACE
    struct trace_event ev;
    ev.timestamp = TRACE_TIMESTAMP();
#ifdef __BEEHIVE__
    ev.u.w32.word1 = arg;
    ev.u.w32.word2 = (subsys << 16) | event;
#else
    ev.u.ev.subsystem = subsys;
    ev.u.ev.event     = event;
    ev.u.ev.arg       = arg;
#endif

#if TRACE_ONLY_SUB_NET
    /* NOTE: This will ensure that only network related messages are logged. PS */
    if (subsys != TRACE_SUBSYS_NET) {
    	return SYS_ERR_OK;
    }
#endif // TRACE_ONLY_SUB_NET

#if TRACE_ONLY_SUB_BNET
    if (get_my_core_id() != 1) {
    	return SYS_ERR_OK;
    }
#endif // TRACE_ONLY_SUB_NET

    return trace_write_event(&ev);
#else
    return SYS_ERR_OK;
#endif
}

#endif // LIBBARRELFISH_TRACE_H
