/* Copyright (c) 2007-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include <net_queue_manager/net_queue_manager.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/spawn_client.h>
#include <barrelfish/debug.h>
#include <barrelfish/deferred.h>
#include <pci/pci.h>


#include <if/sfn5122f_defs.h>
#include <dev/sfn5122f_dev.h>
#include <dev/sfn5122f_q_dev.h>

#include "helper.h"
#include "sfn5122f.h"
#include "sfn5122f_queue.h"
#include "sfn5122f_debug.h"
#include "buffer_tbl.h"

/******************************************************************************/
/* Prototypes */

static void idc_register_queue_memory(uint8_t queue,
                                      struct capref tx_frame,
                                      struct capref ev_frame,
                                      struct capref rx_frame,
                                      uint32_t rxbufsz,
                                      uint8_t vector,
                                      coreid_t core);

static void idc_terminate_queue(void);

void qd_queue_init_data(struct sfn5122f_binding *b, struct capref registers,
        uint64_t macaddr);
void qd_queue_memory_registered(struct sfn5122f_binding *b);
void qd_write_queue_tails(struct sfn5122f_binding *b);

void qd_argument(const char *arg);
void qd_main(void);
int main(int argc, char **argv) __attribute__((weak));

/* Global state */
static const char* service_name = "sfn5122f";

/** Binding to the internal sfn5122f management service */
static struct sfn5122f_binding *binding = NULL;

/** Queue index for this manager instance */
static int qi = -1;

/** Mackerel handle for device */
static sfn5122f_t *d = NULL;

/** Queue handle for queue management library */
static sfn5122f_queue_t *q;

/** MAC address to be used */
static uint64_t mac_address = 0;

/** Indicates if the initialization is done */
static int initialized = 0;

/**
 * Indicates whether we should rely on cache coherence for the descriptor
 * rings.
 */
static bool cache_coherence = true;

/** Indicates whether Interrupts should be used */
static bool use_interrupts = false;
static bool use_msix = false;
static coreid_t core = 0;
static uint8_t vector = 0;

/** Capability for hardware TX ring */
static struct capref tx_frame;

/** Capability for hardware RX ring */
static struct capref rx_frame;

/** Capability for hardware EV ring */
static struct capref ev_frame;

//static void* mac_virt;
uint64_t mac_stats_array[NUM_MAC_STATS];

/**  Userspace networking enable  */
static bool userspace = 0;

/******************************************************************************/
/* Transmit path */
static uint64_t find_tx_free_slot_count_fn(void)
{
    return sfn5122f_queue_free_txslots(q);
}

static errval_t transmit_pbuf_list_fn(struct driver_buffer *buffers,
                                      size_t                count)
{
    size_t i;
    
    if (find_tx_free_slot_count_fn() < count) {
        return ETHERSRV_ERR_CANT_TRANSMIT;
    }

    for (i = 0; i < count; i++) {
        sfn5122f_queue_add_txbuf(q, buffers[i].pa,
                                 buffers[i].len, buffers[i].opaque, 
                                 (i == count - 1)); 
    }

    sfn5122f_queue_bump_txtail(q);

    return SYS_ERR_OK;
}

static bool handle_free_tx_slot_fn(void)
{
    return false;
}



/******************************************************************************/
/* Receive path */

static uint64_t find_rx_free_slot_count_fn(void)
{
   return sfn5122f_queue_free_rxslots(q);
}

static errval_t register_rx_buffer_fn(uint64_t paddr, void *vaddr, void *opaque)
{
    if (find_rx_free_slot_count_fn() == 0) {
        printf("SFN5122F_%d: Not enough space in RX ring, not adding buffer\n", 
                qi);
        return ETHERSRV_ERR_TOO_MANY_BUFFERS;
    }

    sfn5122f_queue_add_rxbuf(q, paddr, opaque);
    sfn5122f_queue_bump_rxtail(q);
    
    return SYS_ERR_OK;
}


/*  polling event queue for new events         */
static size_t check_for_new_events(void)
{
    size_t len = 0;
    size_t count = 0;
    uint8_t ev_code;
    // TODO add constant
    struct driver_rx_buffer buf[16];

    // need to block until initalized
    if (!initialized) {
        return 0;
    }
 
    ev_code = sfn5122f_get_event_code(q);
    while (ev_code != 15 && count < 100){
          void *op = NULL;
          ev_code = sfn5122f_get_event_code(q);
          switch(ev_code){
   
              case EV_CODE_RX:
                   // TODO multiple packets
                   if (sfn5122f_queue_handle_rx_ev(q, &op, &len) == SYS_ERR_OK) {
                        buf[0].len = len;
                        buf[0].opaque = op;
                        process_received_packet(buf, 1, 0);
                   } else {
                        // TODO how to tell the the upper layer that it can reuse
                        // the rx buffer
                   }

                   DEBUG_QUEUE(" RX_EV Q_ID: %d len %ld \n", qi, len);
                   sfn5122f_queue_bump_evhead(q);
                   break;
              case EV_CODE_TX:
                   if (sfn5122f_queue_handle_tx_ev(q, &op) == SYS_ERR_OK) {
                        DEBUG_QUEUE("TX EVENT OK %d \n", qi);
                        handle_tx_done(op); 
                   } else {
                        DEBUG_QUEUE("TX EVENT ERR %d \n", qi);
                   }
                   sfn5122f_queue_bump_evhead(q);
                   break;
              case EV_CODE_DRV:
                   //DEBUG_QUEUE("DRIVER EVENT %d\n", qi);
                   sfn5122f_handle_drv_ev(q, qi);
                   sfn5122f_queue_bump_evhead(q);
                   break;
              case EV_CODE_DRV_GEN:
                   DEBUG_QUEUE("DRIVER GENERATED EVENT \n");
                   sfn5122f_queue_bump_evhead(q);
                   break;
              case EV_CODE_USER:
                   DEBUG_QUEUE("USER EVENT \n");
                   sfn5122f_queue_bump_evhead(q);
                   break;
              case EV_CODE_MCDI:
                   //DEBUG_QUEUE("MCDI EVENT \n");
                   sfn5122f_queue_handle_mcdi_event(q);
                   sfn5122f_queue_bump_evhead(q);
                   break;
              case EV_CODE_GLOBAL:
                   DEBUG_QUEUE("GLOBAL EVENT \n");
                   sfn5122f_queue_bump_evhead(q);
                   break;
          }
          count++;
    }
    /* update event queue tail */
    //if (count > 0) {
        sfn5122f_evq_rptr_reg_wr(d, qi, q->ev_head);
    //}

    return count-1;
}

/**  Misc             */
static errval_t update_rxtail(void *opaque, size_t tail)
{
    assert(d != NULL);
    uint64_t reg = 0;

    reg = sfn5122f_rx_desc_upd_reg_hi_rx_desc_wptr_insert(reg, tail);
    /* don't want to push an additional rx descriptor with the write pointer */
    reg = sfn5122f_rx_desc_upd_reg_hi_rx_desc_push_cmd_insert(reg, 0);
    /* the lower register will be ignored   */
    sfn5122f_rx_desc_upd_reg_lo_wr(d, qi, 0);
    sfn5122f_rx_desc_upd_reg_hi_wr(d, qi, reg);

    return SYS_ERR_OK;
}

static errval_t update_txtail(void *opaque, size_t tail)
{
    assert(d != NULL);
    uint64_t reg = 0;
    reg = sfn5122f_tx_desc_upd_reg_hi_tx_desc_wptr_insert(reg, tail);
    /* don't want to push an additional tx descriptor with the write pointer */
    reg = sfn5122f_tx_desc_upd_reg_hi_tx_desc_push_cmd_insert(reg, 0);
    reg = sfn5122f_tx_desc_upd_reg_hi_tx_desc_insert(reg, 0);

    /*  the lower register will be ignored  */
    sfn5122f_tx_desc_upd_reg_lo_wr(d, qi, 0);
    sfn5122f_tx_desc_upd_reg_hi_wr(d, qi, reg);
    return SYS_ERR_OK;
}

/** Callback to get card's MAC address */
static void get_mac_address_fn(uint8_t* mac)
{
    memcpy(mac, &mac_address, 6);
}
/******************************************************************************/
/* Interrupts */

static void qd_interrupt(void)
{
    size_t count;
    
    count = check_for_new_events();
    if (count <= 0) {
        DEBUG_QUEUE("qd_int_%d: qid=%d no events \n", disp_get_core_id(), qi);
    } else {
        DEBUG_QUEUE("qd_int_%d: qid=%d events processed=%ld \n", disp_get_core_id(), 
                    qi, count);
    }

}

static void interrupt_handler(void *data)
{
    qd_interrupt();
}

/******************************************************************************/
/* Device/queue initialization */

/** Allocate queue n and return handle for queue manager */

static void setup_queue(void)
{
    size_t tx_size, rx_size, ev_size;
    void *tx_virt, *rx_virt, *ev_virt;
    vregion_flags_t flags_vreg;
    
    struct sfn5122f_queue_ops ops = {
        .update_txtail = update_txtail,
        .update_rxtail = update_rxtail
     };

    // Decide on which flags to use for the mappings
    flags_vreg = (cache_coherence ? VREGION_FLAGS_READ_WRITE :
                               VREGION_FLAGS_READ_WRITE_NOCACHE);

   
    /* Allocate memory for descriptor rings  
       No difference for userspace networking*/
    tx_size = sfn5122f_q_tx_ker_desc_size * TX_ENTRIES;
    tx_virt = alloc_map_frame(flags_vreg, tx_size, &tx_frame);

    assert(tx_virt != NULL);

    if (!userspace) {
         rx_size = sfn5122f_q_rx_ker_desc_size * RX_ENTRIES;
    } else {
         rx_size = sfn5122f_q_rx_user_desc_size * RX_ENTRIES;
    }

    rx_virt = alloc_map_frame(flags_vreg, rx_size, &rx_frame);
    assert(rx_virt != NULL);

    ev_size = sfn5122f_q_event_entry_size * EV_ENTRIES;
    ev_virt = alloc_map_frame(flags_vreg, ev_size, &ev_frame);
    assert(ev_virt != NULL);

    if (use_interrupts && use_msix) {
        DEBUG_QUEUE("Enabling MSI-X interrupts\n");
        errval_t err = pci_setup_inthandler(interrupt_handler, NULL, &vector);
        assert(err_is_ok(err));
        core = disp_get_core_id();
    } else {
        if (use_interrupts) {
            DEBUG_QUEUE("Enabling legacy interrupts\n");
        }
        vector = 0;
        core = 0;
    }


    // Initialize queue manager
    q = sfn5122f_queue_init(tx_virt, TX_ENTRIES, rx_virt, RX_ENTRIES,
                            ev_virt, EV_ENTRIES, &ops,  NULL, userspace);
    idc_register_queue_memory(qi, tx_frame, rx_frame,
                              ev_frame, MTU_MAX, vector, core);

}

/** Terminate this queue driver */
static void terminate_queue_fn(void)
{
    idc_terminate_queue();
}

/******************************************************************************/
/* Management interface implemetation */

/** Request device register cap from card driver */

static void idc_request_device_info(void)
{

    errval_t r;
    DEBUG_QUEUE("idc_request_device_info()\n");

    r = sfn5122f_request_device_info__tx(binding, NOP_CONT);
    // TODO: handle busy
    assert(err_is_ok(r));
}

/** Send memory caps to card driver */
static void idc_register_queue_memory(uint8_t queue,
                                      struct capref tx,
                                      struct capref rx,
                                      struct capref ev,
                                      uint32_t rxbufsz,
                                      uint8_t vec,
                                      coreid_t cid)
{

    errval_t r;
    DEBUG_QUEUE("idc_register_queue_memory()\n");

    r = sfn5122f_register_queue_memory__tx(binding, NOP_CONT, queue,
                                           tx, rx, ev, rxbufsz, 
                                           use_interrupts, userspace,
                                           vec, cid);
    // TODO: handle busy
    assert(err_is_ok(r));
}

// Callback from device manager
void qd_queue_init_data(struct sfn5122f_binding *b, struct capref registers,
                        uint64_t macaddr)
{
    struct frame_identity frameid = { .base = 0, .bytes = 0 };
    errval_t err;
    void *virt;

    DEBUG_QUEUE("idc_queue_init_data\n");

    mac_address = macaddr;

    // Map device registers
    invoke_frame_identify(registers, &frameid);
    err = vspace_map_one_frame_attr(&virt, frameid.bytes, registers,
            VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);

    assert(err_is_ok(err));

    // Initialize mackerel device
    d = malloc(sizeof(*d));
    sfn5122f_initialize(d, virt);
    // Initialize queue
    setup_queue();
}

/** Tell card driver to stop this queue. */
static void idc_terminate_queue(void)
{
    errval_t r;
    DEBUG_QUEUE("idc_terminate_queue()\n");

    r = sfn5122f_terminate_queue__tx(binding, NOP_CONT, qi);
    // TODO: handle busy
    assert(err_is_ok(r));
}

// Callback from device manager
void qd_queue_memory_registered(struct sfn5122f_binding *b)
{
    initialized = 1;
    // Register queue with queue_mgr library
    DEBUG_QUEUE("Called ethersrv_init() \n");
    ethersrv_init((char*) service_name, qi, 
                  get_mac_address_fn, 
                  terminate_queue_fn,
                  transmit_pbuf_list_fn, 
                  find_tx_free_slot_count_fn,
                  handle_free_tx_slot_fn, 
                  MTU_MAX, 
                  register_rx_buffer_fn,
                  find_rx_free_slot_count_fn);
}

// Callback from device manager
void qd_write_queue_tails(struct sfn5122f_binding *b)
{
    DEBUG_QUEUE("idc_write_queue_tails()\n");

    sfn5122f_queue_bump_rxtail(q);
    sfn5122f_queue_bump_txtail(q);
}


// Callback from device manager
static void idc_queue_terminated(struct sfn5122f_binding *b)
{
    errval_t err;

    DEBUG_QUEUE("idc_queue_terminated()\n");

    // Free memory for hardware ring buffers
    if (q->userspace) {
        err = vspace_unmap(q->tx_ring.user);
        assert(err_is_ok(err));
        err = vspace_unmap(q->rx_ring.user);
        assert(err_is_ok(err));
    } else {
        err = vspace_unmap(q->tx_ring.ker);
        assert(err_is_ok(err));
        err = vspace_unmap(q->rx_ring.ker);
        assert(err_is_ok(err));
    }

    err = vspace_unmap(q->ev_ring);
    assert(err_is_ok(err));
    err = cap_delete(tx_frame);
    assert(err_is_ok(err));
    err = cap_delete(rx_frame);
    assert(err_is_ok(err));
    err = cap_delete(ev_frame);
    assert(err_is_ok(err));

    exit(0);
}

static struct sfn5122f_rx_vtbl rx_vtbl = {
    .queue_init_data = qd_queue_init_data,
    .queue_memory_registered = qd_queue_memory_registered,
    .write_queue_tails = qd_write_queue_tails,
    .queue_terminated = idc_queue_terminated,
};

static void bind_cb(void *st, errval_t err, struct sfn5122f_binding *b)
{
    assert(err_is_ok(err));

    DEBUG_QUEUE("Sucessfully connected to management interface\n");

    b->rx_vtbl = rx_vtbl;
    binding = b;

    idc_request_device_info();
}

/** Connect to the management interface */
static void connect_to_mngif(void)
{
    errval_t r;
    iref_t iref;
    const char *suffix = "_sfn5122fmng";
    char name[strlen(service_name) + strlen(suffix) + 1];

    // Build label for interal management service
    sprintf(name, "%s%s", service_name, suffix);

    // Connect to service
    DEBUG_QUEUE("Looking up management interface (%s)\n", name);
    r = nameservice_blocking_lookup(name, &iref);
    assert(err_is_ok(r));

    DEBUG_QUEUE("Binding to management interface\n");
    r = sfn5122f_bind(iref, bind_cb, NULL, get_default_waitset(),
            IDC_BIND_FLAGS_DEFAULT);
    assert(err_is_ok(r));
}

void qd_argument(const char *arg)
{
    if (strncmp(arg, "cardname=", strlen("cardname=") - 1) == 0) {
        service_name = arg + strlen("cardname=");
        ethersrv_argument(arg);

    } else if (strncmp(arg, "queue=", strlen("queue=") - 1) == 0) {
        qi = atol(arg + strlen("queue="));
        ethersrv_argument(arg);

    } else if (strncmp(arg, "cache_coherence=", 
                       strlen("cache_coherence=") - 1) == 0) {
        cache_coherence = !!atol(arg + strlen("cache_coherence="));

    } else if (strncmp(arg, "interrupts=", strlen("interrupts=") - 1) == 0) {
        use_interrupts = !!atol(arg + strlen("interrupts="));
        DEBUG_QUEUE("Interrupts enabled: legacy interrupts for fatal device errors\n");
    } else if (strncmp(arg, "msix=", strlen("msix=") - 1) == 0) {
        USER_PANIC("MSI-X not fully implemented yet!");
        use_msix = !!atol(arg + strlen("msix="));
        DEBUG_QUEUE("Using msix \n");
    } else if (strncmp(arg, "userspace=", strlen("userspace=") - 1) == 0) {
       USER_PANIC("Userspace networking for SFN5122F not implemented!");
       /*
        userspace = atol(arg + strlen("userspace="));
        ethersrv_argument(arg);
       */
    } else {
        ethersrv_argument(arg);
    }
}

static void parse_cmdline(int argc, char **argv)
{
    int i;
    for (i = 1; i < argc; i++) {
        qd_argument(argv[i]);
    }
}

static void eventloop(void)
{
    struct waitset *ws;
    errval_t err;

    DEBUG_QUEUE("eventloop()\n");

    ws = get_default_waitset();
    while (1) {
        err = event_dispatch_non_block(ws);
        do_pending_work_for_all();
        check_for_new_events();
    }
}

static void eventloop_ints(void)
{
    struct waitset *ws;
    DEBUG_QUEUE("eventloop_ints()\n");

    ws = get_default_waitset();
    while (1) {
        event_dispatch(ws);
        do_pending_work_for_all();
    }
}

void qd_main(void)
{
    // Validate some settings
    if (qi == -1) {
        USER_PANIC("For queue driver the queue= parameter has to be specified "
                   "on the command line!");
    }

    connect_to_mngif();
 
    if (use_interrupts) {
        eventloop_ints();
    } else {
        eventloop();
    }
}

int main(int argc, char **argv)
{
    DEBUG_QUEUE("Started\n");
    parse_cmdline(argc, argv);
    qd_main();
}
