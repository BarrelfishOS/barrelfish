/*
 * Copyright (c) 2013, 2014, University of Washington, 2017 ETH Zürich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include <net_device_manager/net_device_manager.h>
#include <pci/pci.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/debug.h>
#include <ipv4/lwip/inet.h>
#include <acpi_client/acpi_client.h>


#include <if/e10k_defs.h>
#include <if/e10k_vf_defs.h>
#include <if/e10k_vf_rpcclient_defs.h>
#include <dev/e10k_vf_dev.h>
#include <dev/e10k_q_dev.h>

#include "sleep.h"
#include "helper.h"
#include "e10k_devif_vf.h"
#include "e10k_queue.h"
#include "debug.h"


#define POOL_SIZE 2


#define E10K_PCI_DEVID 0x10ED


#define QUEUE_INTRX 0
#define QUEUE_INTTX 1


struct vf_state {
    uint8_t vf_num;
    // if we have 64 vfs then pool
    // size is only 2 but the maximum is 8
    struct e10k_queue* qs[POOL_SIZE];
    bool q_enabled[8];

    uint64_t vf_mac;
    
    // device info
    int initialized;
    e10k_vf_t *d;
    struct capref *regframe;
    uint64_t d_mac;
    uint32_t pci_function;


    // interrupt related
    bool msix;
    struct bmallocator msix_alloc;
    size_t vdriver_msix;
    uint8_t vdriver_vector;
    bool use_interrupts;

    //
    struct e10k_vf_binding *binding;
    bool bound;
};

static struct vf_state* vf;


#define prnonz(x)                                               \
    uint32_t x = e10k_vf_##x##_rd(vf->d);                           \
    snprintf(str[cnt++], 32, #x "=%x \n", x);                      \

static void stats_dump(void)
{
  char str[256][32];
  int cnt = 0;
  memset(str, 0, 256 * 32);

    prnonz(vfctrl);
    prnonz(vfstatus);
    prnonz(vflinks);
    prnonz(vfrxmemwrap);
    prnonz(vfeicr);
    prnonz(vfeics);
    prnonz(vfeims);
    prnonz(vfpsrtype);

    if(cnt > 0) {
      for(int i = 0; i < cnt; i++) {
	    printf("%s ", str[i]);
      }
      printf("\n");
    }
}

static void setup_interrupt(size_t *msix_index, uint8_t core, uint8_t vector)
{
    bool res;
    errval_t err;
    uint8_t dest;

    res = bmallocator_alloc(&vf->msix_alloc, msix_index);
    assert(res);

    err = get_apicid_from_core(core, &dest);
    assert(err_is_ok(err));

    err = pci_msix_vector_init(*msix_index, dest, vector);
    assert(err_is_ok(err));

    DEBUG_VF("e10k: MSI-X vector setup index=%"PRIx64", core=%d apic=%d swvec=%x\n",
            *msix_index, core, dest, vector);
}


static void interrupt_handler_msix(void* arg)
{
    struct e10k_queue* q = (struct e10k_queue*)arg;
    DEBUG_VF("MSI-X management interrupt\n");
    e10k_vf_vfeicr_t eicr = e10k_vf_vfeicr_rd(vf->d);

    eicr &= ~(1 << q->msix_index);

    // Ensure management MSI-X vector is cleared
    e10k_vf_vfeicr_wr(vf->d, 1 << q->msix_index);

    // Reenable interrupt
    e10k_vf_vfeimc_msix_wrf(vf->d, 1 << (q->msix_index % 32));
    // TODO check for packets?
}

/** Stop whole device. */
static void stop_device(struct vf_state* v)
{
    DEBUG_VF("Stopping device\n");

    // Disable interrupts
    e10k_vf_vfeimc_msix_wrf(v->d, 7);
    e10k_vf_vfeicr_rd(v->d);

    // Disable each RX and TX queue
    for(int i = 0; i < 4; i++) {
        e10k_vf_vftxdctl_wr(v->d, i, e10k_vf_vftxdctl_swflsh_insert(0x0, 1));
    }
    for(int i = 0; i < 8; i++) {
        e10k_vf_vfrxdctl_wr(v->d, i, 0x0);
    }

    // From BSD driver (not in spec)
    milli_sleep(2);
}

/**
 * Initialize hardware registers.
 * Is also called after a reset of the device.
 */
static void device_init(void)
{
    
    int i;
    errval_t err;
    bool initialized_before = vf->initialized;

    vf->initialized = 0;

    stats_dump();

    stop_device(vf);

    assert(!initialized_before);

    // Issue Global reset
    e10k_vf_vfctrl_rst_wrf(vf->d, 1);
    // Spec says 10, fbsd driver 50
    milli_sleep(5000);
    DEBUG_VF("Global reset done\n");

    // Disable interrupts
    e10k_vf_vfeimc_msix_wrf(vf->d, 7);
    e10k_vf_vfeicr_rd(vf->d);

    // Wait for link to come up
    DEBUG_VF("Waiting for Link\n");
    stats_dump();
    //while (e10k_vf_vflinks_lnk_up_rdf(vf->d) == 0); // TODO: Timeout and Uncomment
    DEBUG_VF("Link Up\n");
    milli_sleep(50);

    // Initialize interrupts
    e10k_vf_vfeicr_wr(vf->d, 7);
    e10k_vf_vfeitr_wr(vf->d, 0, 0x0);
    e10k_vf_vfeitr_wr(vf->d, 1, 0x0);

    if (vf->msix) {
        // Allocate msix vector for cdriver and set up handler
        if (vf->vdriver_msix == -1) {
            err = pci_setup_inthandler(interrupt_handler_msix, NULL, 
                                       &vf->vdriver_vector);
            assert(err_is_ok(err));

            setup_interrupt(&vf->vdriver_msix, disp_get_core_id(), 
                            vf->vdriver_vector);
        }

        // Map management interrupts to our vector
        e10k_vf_vfivar_misc_i_alloc0_wrf(vf->d, vf->vdriver_msix);
        e10k_vf_vfivar_misc_i_allocval0_wrf(vf->d, 1);

        // Enable interrupt
        e10k_vf_vfeitr_wr(vf->d, vf->vdriver_msix / 32, (1 << (vf->vdriver_msix % 32)));
    } else {
        // Enable all interrupts
        e10k_vf_vfeimc_wr(vf->d, e10k_vf_vfeims_rd(vf->d));
        e10k_vf_vfeims_msix_wrf(vf->d, 7);
    }

    // Other stuff
    e10k_vf_vfpsrtype_wr(vf->d, 0);

    // disable relaxed ordering
    for (i = 0; i < 8; i++) {
        e10k_vf_vfdca_txctrl_txdesc_wbro_wrf(vf->d, i, 0);
        e10k_vf_vfdca_rxctrl_rxhdr_ro_wrf(vf->d, i, 0);
        e10k_vf_vfdca_rxctrl_rxdata_wrro_wrf(vf->d, i, 0);
    }

    // enable all queues
    for (i = 0; i < POOL_SIZE; i++) {
        e10k_vf_vftxdctl_enable_wrf(vf->d, i, 1);
    }

    vf->initialized = 1;
}

/** Initialize hardware queue n. */
static void queue_hw_init(struct e10k_queue* q)
{
    errval_t r;
    struct frame_identity frameid = { .base = 0, .bytes = 0 };
    uint64_t tx_phys, txhwb_phys, rx_phys;
    size_t tx_size, rx_size;
    uint8_t n = q->id;
    e10k_vf_t* d = q->d;

    // Get physical addresses for rx/tx rings
    r = invoke_frame_identify(q->tx_frame, &frameid);
    assert(err_is_ok(r));
    tx_phys = frameid.base;
    tx_size = frameid.bytes;

    r = invoke_frame_identify(q->rx_frame, &frameid);
    assert(err_is_ok(r));
    rx_phys = frameid.base;
    rx_size = frameid.bytes;


    DEBUG_VF("tx.phys=%"PRIx64" tx.size=%"PRIu64"\n", tx_phys, tx_size);
    DEBUG_VF("rx.phys=%"PRIx64" rx.size=%"PRIu64"\n", rx_phys, rx_size);


    // Initialize RX queue in HW
    e10k_vf_vfrdbal_wr(d, n, rx_phys);
    e10k_vf_vfrdbah_wr(d, n, rx_phys >> 32);
    e10k_vf_vfrdlen_wr(d, n, rx_size);

    e10k_vf_vfsrrctl_bsz_pkt_wrf(d, n, q->rxbufsz / 1024);
    e10k_vf_vfsrrctl_bsz_hdr_wrf(d, n, 128 / 64); // TODO: Do 128 bytes suffice in
                                               //       all cases?
    e10k_vf_vfsrrctl_desctype_wrf(d, n, e10k_vf_adv_1buf);
    e10k_vf_vfsrrctl_drop_en_wrf(d, n, 1);

    // Initialize queue pointers (empty)
    e10k_vf_vfrdt_wr(d, n, q->rx_head);
    e10k_vf_vfrdh_wr(d, n, q->rx_head);

    e10k_vf_vfrxdctl_enable_wrf(d, n, 1);
    while (e10k_vf_vfrxdctl_enable_rdf(d, n) == 0); // TODO: Timeout
    DEBUG_VF("[%x] RX queue enabled\n", n);

    // Setup Interrupts for this queue
    if (q->use_irq) {
        DEBUG_VF("[%x] Setting up interrupts\n", n);
        uint8_t rxv, txv;
        // Look for interrupt vector
        if (q->msix_intvec != 0) {
            if (q->msix_index == -1) {
                setup_interrupt(&q->msix_index, q->msix_intdest,
                                q->msix_intvec);
            }
            rxv = txv = q->msix_index;
        }

        // XXX. Please double check, this is against the original intention
        // Map rxv/txv to eicr bits that we can recognize
        rxv = QUEUE_INTRX;
        txv = QUEUE_INTTX;

        // DEBUG_VF("rxv=%d txv=%d\n", rxv, txv);

        // Setup mapping queue Rx/Tx -> interrupt
        uint8_t i = n / 2;
        if ((n % 2) == 0) {
            e10k_vf_vfivar_i_alloc0_wrf(d, i, rxv);
            e10k_vf_vfivar_i_allocval0_wrf(d, i, 1);
            e10k_vf_vfivar_i_alloc1_wrf(d, i, txv);
            e10k_vf_vfivar_i_allocval1_wrf(d, i, 1);
        } else {
            e10k_vf_vfivar_i_alloc2_wrf(d, i, rxv);
            e10k_vf_vfivar_i_allocval2_wrf(d, i, 1);
            e10k_vf_vfivar_i_alloc3_wrf(d, i, txv);
            e10k_vf_vfivar_i_allocval3_wrf(d, i, 1);
        }
        if (q->msix_intvec != 0) {
            // Enable interrupt
            e10k_vf_vfeitr_wr(d, rxv / 32, (1 << (rxv % 32)));
        }
        if (rxv < 16) {
            // Make sure interrupt is cleared
            e10k_vf_vfeicr_wr(d, 1 << rxv);
        }
    }

    // Initialize TX queue in HW
    if (q->use_vtd) {
        e10k_vf_vftdbal_wr(d, n, (lvaddr_t) q->tx_ring);
        e10k_vf_vftdbah_wr(d, n, ((lvaddr_t)q->tx_ring) >> 32);
    } else {
        e10k_vf_vftdbal_wr(d, n, tx_phys);
        e10k_vf_vftdbah_wr(d, n, tx_phys >> 32);
    }

    e10k_vf_vftdlen_wr(d, n, tx_size);

    // Initialize TX head index write back
    if (!capref_is_null(q->txhwb_frame)) {
        r = invoke_frame_identify(q->txhwb_frame, &frameid);
        assert(err_is_ok(r));
        txhwb_phys = frameid.base;

	if (q->use_vtd) {
	    e10k_vf_vftdwbal_headwb_low_wrf(d, n, ((lvaddr_t)q->tx_hwb) >> 2);
	    e10k_vf_vftdwbah_headwb_high_wrf(d, n, ((lvaddr_t)q->tx_hwb) >> 32);
        } else {
            e10k_vf_vftdwbal_headwb_low_wrf(d, n, txhwb_phys >> 2);
            e10k_vf_vftdwbah_headwb_high_wrf(d, n, txhwb_phys >> 32);
        }
        e10k_vf_vftdwbal_headwb_en_wrf(d, n, 1);
    }

    // Initialized by queue driver to avoid race conditions
    // Initialize queue pointers
    assert(q->tx_head == 0);
    e10k_vf_vftdh_wr(d, n, q->tx_head);
    e10k_vf_vftdt_wr(d, n, q->tx_head);

    // Configure prefetch and writeback threshhold
    e10k_vf_vftxdctl_pthresh_wrf(d, n, 8); // FIXME: Figure out what the right number
                                      //        is here.
    e10k_vf_vftxdctl_hthresh_wrf(d, n, 0);
    e10k_vf_vftxdctl_wthresh_wrf(d, n, 0);      // Needs to be 0 for TXHWB

    e10k_vf_vftxdctl_enable_wrf(d, n, 1);

    while (e10k_vf_vftxdctl_enable_rdf(d, n) == 0); // TODO: Timeout
    DEBUG_VF("[%x] TX queue enabled\n", n);

    // Some initialization stuff from BSD driver
    e10k_vf_vfdca_txctrl_txdesc_wbro_wrf(d, n, 0);
}

#if 0
/** Stop queue. */
static void queue_hw_stop(uint8_t n)
{
#if 0
    // This process is described in 4.6.7.1.2

    // Disable TX for this queue
    e10k_txdctl_enable_wrf(d, n, 0);

    // TODO: Flush packet buffers
    // TODO: Remove all filters
    // TODO: With RSC we have to wait here (see spec), not used atm

    // Disable RX for this queue
    e10k_rxdctl_1_enable_wrf(d, n, 0);
    while (e10k_rxdctl_1_enable_rdf(d, n) != 0); // TODO: Timeout

    // A bit too much, but make sure memory is not used anymore
    milli_sleep(1);
#else
    assert(!"NYI");
#endif
}
#endif

/** Here are the global interrupts handled. */
static void interrupt_handler(void* arg)
{
    e10k_vf_vfeicr_t eicr = e10k_vf_vfeicr_rd(vf->d);

    DEBUG_VF("interrupt vf (eicr=%x)\n", eicr);
    
    if (eicr & ((1 << QUEUE_INTRX) | (1 << QUEUE_INTTX))) {
        e10k_vf_vfeicr_wr(vf->d, eicr);
        /*qd_interrupt(!!(eicr & (1 << QUEUE_INTRX)),
                     !!(eicr & (1 << QUEUE_INTTX)));*/
    }
}

/******************************************************************************/
/* Initialization code for driver */

/** Callback from pci to initialize a specific PCI device. */
static void pci_init_card(struct device_mem* bar_info, int bar_count)
{

    DEBUG_VF("pci init card\n");
    errval_t err;
    bool res;

    assert(!vf->initialized);

    vf->d = malloc(sizeof(*(vf->d)));

    // Map first BAR for register access
    assert(bar_count >= 1);
    map_device(&bar_info[0]);
    vf->regframe = bar_info[0].frame_cap;
    DEBUG_VF("BAR[0] mapped (v=%llx p=%llx l=%llx)\n",
            (unsigned long long) bar_info[0].vaddr,
            (unsigned long long) bar_info[0].paddr,
            (unsigned long long) bar_info[0].bytes);

    // Initialize Mackerel binding
    e10k_vf_initialize(vf->d, (void*) bar_info[0].vaddr);

    // Initialize manager for MSI-X vectors
    if (vf->msix) {
        DEBUG_VF("Enabling MSI-X interrupts\n");
        uint16_t msix_count = 0;
        err = pci_msix_enable(&msix_count);
        assert(err_is_ok(err));
        assert(msix_count > 0);
        DEBUG_VF("MSI-X #vecs=%d\n", msix_count);

        res = bmallocator_init(&vf->msix_alloc, msix_count);
        assert(res);
    } else {
        DEBUG_VF("Using legacy interrupts\n");
    }

    DEBUG_VF("STATUS = %x\n", e10k_vf_vfstatus_rd(vf->d));

    // Initialize hardware registers etc.
    DEBUG_VF("Initializing hardware\n");
    device_init();

    assert(vf->initialized);

    // Tell PF driver
    err = vf->binding->rpc_tx_vtbl.init_done(vf->binding, vf->vf_num);
    assert(err_is_ok(err));

}


/** Register with PCI */
static errval_t pci_register(void)
{
    errval_t r;

    r = pci_client_connect();
    if (err_is_fail(r)) {
        return r;
    }

    interrupt_handler_fn inthandler;

    if(vf->use_interrupts) {
        inthandler = interrupt_handler;
    } else {
        inthandler = NULL;
    }

    r = pci_register_driver_noirq(pci_init_card, PCI_CLASS_ETHERNET,
                                PCI_DONT_CARE, PCI_DONT_CARE,
                                PCI_VENDOR_INTEL, E10K_PCI_DEVID,
                                7, 16, vf->pci_function);

    DEBUG_VF("pci registered\n");
    if (err_is_fail(r)) {
        DEBUG_VF("err\n");
        return r;
    }
    return SYS_ERR_OK;
}


static void vf_bind_cont(void *st, errval_t err, struct e10k_vf_binding *b)
{
    assert(err_is_ok(err));

    vf->binding = b;
}

static errval_t e10k_vf_client_connect(int pci_function)
{
    iref_t iref;
    errval_t err, err2 = SYS_ERR_OK;
    char name[256];

    snprintf(name, 256, "e10k_vf%u", pci_function);

    /* Connect to the pci server */
    err = nameservice_blocking_lookup(name, &iref);
    if (err_is_fail(err)) {
        return err;
    }

    assert(iref != 0);

    /* Setup flounder connection with pci server */
    err = e10k_vf_bind(iref, vf_bind_cont, &err, get_default_waitset(),
                   IDC_BIND_FLAG_RPC_CAP_TRANSFER);
    if (err_is_fail(err)) {
        return err;
    }

    /* XXX: Wait for connection establishment */
    while (vf->binding == NULL && err2 == SYS_ERR_OK) {
        messages_wait_and_handle_next();
    }

    return err2;
}

errval_t e10k_vf_init_queue_hw(struct e10k_queue* q)
{
    DEBUG_VF("VF queue init\n");
    assert(vf->initialized);
    assert(e10k_vf_can_create_queue());
    assert(q != NULL);

    uint8_t q_idx = 0;
    // check which queue to initalize
    for (int i = 0; i < 2; i++) {   
        if (!vf->q_enabled[i]) {
            q_idx = i;
            vf->q_enabled[i] = true;
            break;
        }
    }

    q->d = vf->d;
    DEBUG_VF("Enabled queue %d of VF in hardware\n", q_idx);
    // initialize queue in hardware
    queue_hw_init(q);

    return SYS_ERR_OK;
}

errval_t e10k_init_vf_driver(uint8_t pci_function, 
                             bool interrupts)
{
    
    errval_t err, err2;
    // crate vtd domain for VF driver
    // XXX: might not be the best idea to do it here
    err = connect_to_acpi();
    assert(err_is_ok(err));
    err = vtd_create_domain(cap_vroot);
    assert(err_is_ok(err));
    err = vtd_domain_add_device(0, 7, 16, 0, cap_vroot);
    assert(err_is_ok(err));

    DEBUG_VF("VF driver started\n");
    vf = malloc(sizeof(struct vf_state));
    vf->use_interrupts = interrupts;
   
    DEBUG_VF("Connecting to PF driver...\n");
    err = e10k_vf_client_connect(pci_function);
    if (err_is_fail(err)) {
        return err;
    }  

    DEBUG_VF("Requesting VF number from PF...\n");
    err = vf->binding->rpc_tx_vtbl.request_vf_number(vf->binding, 
                                                     (uint8_t*) &vf->vf_num, 
                                                     &err2); 
    if (err_is_fail(err) || err_is_fail(err2)) {
        return err_is_fail(err) ? err: err2;
    }
 
    DEBUG_VF("Requesting MAC from PF...\n");
    err = vf->binding->rpc_tx_vtbl.get_mac_address(vf->binding, vf->vf_num, 
                                                   &vf->d_mac);
    assert(err_is_ok(err));

    DEBUG_VF("VF num %d initalize...\n", vf->vf_num);
    err = pci_register();
    if (err_is_fail(err)) {
        return err;
    }

    while (!vf->initialized) {
        event_dispatch(get_default_waitset());
    }

    DEBUG_VF("VF init done\n");
    return SYS_ERR_OK;
}

bool e10k_vf_started(void) 
{
    return !(vf == NULL);
}

// Check if VF queue pool has still queues that it can enable
bool e10k_vf_can_create_queue(void)
{
    for (int i = 0; i < POOL_SIZE; i++) {
        if (!vf->q_enabled[i]) {
            return true;
        }            
    }
    return false;
}


