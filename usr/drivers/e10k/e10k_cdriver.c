/*
 * Copyright (c) 2007-2011, 2013, 2014, ETH Zurich.
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
#include <net/net.h>

#include <net_device_manager/net_device_manager.h>
#include <pci/pci.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/debug.h>
#include <barrelfish/deferred.h>
#include <driverkit/iommu.h>
//#include <lwip/ip.h>
#include <arpa/inet.h>
#include <acpi_client/acpi_client.h>
#include <driverkit/driverkit.h>
#include <int_route/int_route_client.h>

#include <if/e10k_defs.h>
#include <if/e10k_vf_defs.h>
#include <if/net_filter_defs.h>
#include <dev/e10k_dev.h>

#include "e10k.h"
#include "sleep.h"
#include "helper.h"
#include "debug.h"
#include "e10k_vf_resources.h"

#define QUEUE_INTRX 0
#define QUEUE_INTTX 1

struct queue_state {
    bool enabled;
    struct e10k_binding *binding;
    struct e10k_vf_binding *devif;

    struct capref tx_frame;
    struct capref txhwb_frame;
    struct capref rx_frame;
    uint32_t rxbufsz;
    uint32_t rxhdrsz;

    size_t msix_index;
    int16_t msix_intvec;
    uint8_t msix_intdest;
    bool use_irq;
    bool use_rsc;

    uint64_t rx_head;
    uint64_t tx_head;
    lvaddr_t tx_va;
    lvaddr_t rx_va;
    lvaddr_t txhwb_va;
};

enum filter_l4type {
    L4_OTHER,
    L4_UDP,
    L4_TCP,
    L4_SCTP
};

enum filter_mask {
    MASK_L4PROTO    = (1 << 0),
    MASK_SRCIP      = (1 << 1),
    MASK_DSTIP      = (1 << 2),
    MASK_SRCPORT    = (1 << 3),
    MASK_DSTPORT    = (1 << 4),
};

struct e10k_filter {
    bool enabled;
    uint8_t priority;
    uint8_t queue;

    uint32_t src_ip;
    uint32_t dst_ip;
    uint16_t src_port;
    uint16_t dst_port;

    uint16_t mask;
    uint16_t l4_type;
};

union macentry {
    uint8_t as8[6];
    uint64_t as64;
};


struct e10k_driver_state {

    // gneral info
    const char *service_name;
    e10k_t *d;
    struct capref regframe;
    bool msix ;
    bool vtdon_dcboff;
    bool dca;
    struct capref* caps;
    struct bfdriver_instance *bfi;
    // Management of MSI-X vectors
    struct bmallocator msix_alloc;

    /** MSI-X vector used by cdriver */
    size_t cdriver_msix;
    uint8_t cdriver_vector;

    int next_interrupt;
    // queue state + filter states
    struct queue_state queues[128];
    struct e10k_filter filters[128];
    int ftqf_index;
    // driver init state
    int initialized;
    bool exported;
    /** Specifies if RX/TX is currently enabled on the device. */
    bool rxtx_enabled;


    char buf[4096];

    /* PCI device address passed on command line */
    uint32_t pci_bus;
    uint32_t pci_device;
    uint32_t pci_function;
    uint32_t pci_deviceid;

    /* VFs alloacation data*/
    bool vf_used[64];
    struct iommu_client* iommu;

    // transmittion rate limiting
    uint16_t credit_refill[128];
    uint32_t tx_rate[128];

    // VF mac?
    union macentry mactable[128];
};

/*
#define prnonz(x, st)                                               \
    uint32_t x = e10k_##x##_rd(st->d);                           \
    snprintf(str[cnt++], 32, #x "=%x \n", x);                      \

static void stats_dump(struct e10k_driver_state* st)
{
  char str[256][32];
  int cnt = 0;
  memset(str, 0, 256 * 32);

    prnonz(ctrl, st);
    prnonz(status, st);
    prnonz(links, st);
    prnonz(rxmemwrap, st);
    prnonz(eicr, st);
    prnonz(eics, st);
    prnonz(eims, st);
    prnonz(ssvpc, st);
    prnonz(txdgpc, st);
    prnonz(gptc, st);

    if(cnt > 0) {
      for(int i = 0; i < cnt; i++) {
	    printf("PF: %s ", str[i]);
      }
      printf("\n");
    }
}
*/
// some prototypes because of dependencies
static void queue_hw_init(struct e10k_driver_state* st, uint8_t n, bool set_tail);
static void device_init(struct e10k_driver_state* st);

static void e10k_flt_ftqf_setup(struct e10k_driver_state* st, int idx, struct e10k_filter* filter)
{
    uint16_t m = filter->mask;
    e10k_l4_proto_t p;
    e10k_ftqf_t ftqf = 0;
    e10k_l34timir_t timir = 0;
    e10k_sdpqf_t sdpqf = 0;


    // Write filter data
    if (!(m & MASK_SRCIP)) {
        DEBUG("src_ip=%"PRIx32" ", filter->src_ip);
        e10k_saqf_wr(st->d, idx, htonl(filter->src_ip));
    }

    if (!(m & MASK_DSTIP)) {
        DEBUG("dst_ip=%"PRIx32" ", filter->dst_ip);
        e10k_daqf_wr(st->d, idx, htonl(filter->dst_ip));
    }

    if (!(m & MASK_SRCPORT)) {
        DEBUG("src_port=%d ", filter->src_port);
        sdpqf = e10k_sdpqf_src_port_insert(sdpqf, htons(filter->src_port));
    }

    if (!(m & MASK_DSTPORT)) {
        DEBUG("dst_port=%d ", filter->dst_port);
        sdpqf = e10k_sdpqf_dst_port_insert(sdpqf, htons(filter->dst_port));
    }
    e10k_sdpqf_wr(st->d, idx, sdpqf);
    DEBUG("queue_id=%d \n", filter->queue);

    if (!(m & MASK_L4PROTO)) {
        switch (filter->l4_type) {
            case L4_OTHER:  p = e10k_l4other; break;
            case L4_UDP:    p = e10k_l4udp; break;
            case L4_TCP:    p = e10k_l4tcp; break;
            case L4_SCTP:   p = e10k_l4sctp; break;
            default: assert(0); return;
        }
        ftqf = e10k_ftqf_protocol_insert(ftqf, p);
    }

    // Write mask bits
    ftqf = e10k_ftqf_m_srcaddr_insert(ftqf, !!(m & MASK_SRCIP));
    ftqf = e10k_ftqf_m_dstaddr_insert(ftqf, !!(m & MASK_DSTIP));
    ftqf = e10k_ftqf_m_srcport_insert(ftqf, !!(m & MASK_SRCPORT));
    ftqf = e10k_ftqf_m_dstport_insert(ftqf, !!(m & MASK_DSTPORT));
    ftqf = e10k_ftqf_m_protocol_insert(ftqf, !!(m & MASK_L4PROTO));


    // Configure destination queue and enable filter
    timir = e10k_l34timir_rx_queue_insert(timir, filter->queue);
    e10k_l34timir_wr(st->d, idx, timir);

    ftqf = e10k_ftqf_priority_insert(ftqf, filter->priority);
    ftqf = e10k_ftqf_pool_mask_insert(ftqf, 1);
    ftqf = e10k_ftqf_queue_en_insert(ftqf, 1);
    e10k_ftqf_wr(st->d, idx, ftqf);
}

static int ftqf_alloc(struct e10k_driver_state* st)
{
    // FIXME: Do this reasonably
    return st->ftqf_index++;
}

static errval_t reg_ftfq_filter(struct e10k_driver_state* st, struct e10k_filter* f, uint64_t* fid)
{
    int i;

    DEBUG("reg_ftfq_filter: called\n");

    if ((i = ftqf_alloc(st)) < 0) {
        return FILTER_ERR_NOT_ENOUGH_MEMORY;
    }


    st->filters[i] = *f;
    st->filters[i].enabled = true;

    e10k_flt_ftqf_setup(st, i, f);

    *fid = i + 1;

    return SYS_ERR_OK;
}


/****************************************************************************/
/* Net filter interface implementation                                      */
/****************************************************************************/


static errval_t cb_install_filter(struct net_filter_binding *b,
                                  net_filter_filter_type_t type,
                                  uint64_t qid,
                                  uint32_t src_ip,
                                  uint32_t dst_ip,
                                  uint16_t src_port,
                                  uint16_t dst_port,
                                  uint64_t* fid)
{
    struct e10k_driver_state* st = (struct e10k_driver_state*) b->st;
    errval_t err;

    struct e10k_filter f = {
        .dst_port = dst_port,
        .src_port = src_port,
        .dst_ip = dst_ip,
        .src_ip = src_ip,
        .l4_type = (type == net_filter_PORT_TCP ? L4_TCP : L4_UDP),
        .priority = 1,
        .queue = qid,
    };

    if (src_ip == 0) {
        f.mask = f.mask | MASK_SRCIP;
    }

    // ignore dst ip
    f.mask = f.mask | MASK_DSTIP;

    if (dst_port == 0) {
        f.mask = f.mask | MASK_DSTPORT;
    }

    if (src_port == 0) {
        f.mask = f.mask | MASK_SRCPORT;
    }

    *fid = -1ULL;

    err = reg_ftfq_filter(st, &f, fid);
    DEBUG("filter registered: err=%s, fid=%"PRIu64"\n", err_getstring(err), *fid);
    return err;
}


static errval_t cb_remove_filter(struct net_filter_binding *b,
                                 net_filter_filter_type_t type,
                                 uint64_t filter_id,
                                 errval_t* err)
{
    if ((type == net_filter_PORT_UDP || type == net_filter_PORT_TCP)){
        USER_PANIC("NYI");
        *err = SYS_ERR_OK;
    } else {
        *err = NET_FILTER_ERR_NOT_FOUND;
    }

    DEBUG("unregister_filter: called (%"PRIx64")\n", filter_id);
    return SYS_ERR_OK;
}

static struct net_filter_rpc_rx_vtbl net_filter_rpc_rx_vtbl = {
    .install_filter_ip_call = cb_install_filter,
    .remove_filter_call = cb_remove_filter,
    .install_filter_mac_call = NULL,
};

static void net_filter_export_cb(void *st, errval_t err, iref_t iref)
{

    printf("exported net filter interface\n");
    err = nameservice_register("net_filter_e10k", iref);
    assert(err_is_ok(err));
    DEBUG("Net filter interface exported\n");
}


static errval_t net_filter_connect_cb(void *st, struct net_filter_binding *b)
{
    printf("New connection on net filter interface\n");
    b->rpc_rx_vtbl = net_filter_rpc_rx_vtbl;
    b->st = st;
    return SYS_ERR_OK;
}

/** Enable RX operation for whole card. */
static void rx_enable(struct e10k_driver_state* st)
{
    e10k_secrxctrl_rx_dis_wrf(st->d, 1);
    while (e10k_secrxstat_sr_rdy_rdf(st->d) == 0); // TODO: Timeout
    e10k_rxctrl_rxen_wrf(st->d, 1);
    e10k_secrxctrl_rx_dis_wrf(st->d, 0);
}

/** Disable RX operation for whole card. */
static void rx_disable(struct e10k_driver_state* st)
{
    e10k_secrxctrl_rx_dis_wrf(st->d, 1);
    while (e10k_secrxstat_sr_rdy_rdf(st->d) == 0); // TODO: Timeout
    e10k_rxctrl_rxen_wrf(st->d, 0);
    e10k_secrxctrl_rx_dis_wrf(st->d, 0);
}

/** Enable TX operation for whole card. */
static void tx_enable(struct e10k_driver_state* st)
{
    e10k_dmatxctl_txen_wrf(st->d, 1);
}

/** Disable TX operation for whole card. */
static void tx_disable(struct e10k_driver_state* st)
{
    e10k_dmatxctl_txen_wrf(st->d, 0);
    while (e10k_dmatxctl_txen_rdf(st->d) != 0); // TODO: timeout
}

/* ************************************************
 *  Interrupt setup and handling
 */
static void setup_interrupt(struct e10k_driver_state* st, size_t *msix_index, 
                            uint8_t core, uint8_t vector)
{
    bool res;
    errval_t err;
    uint8_t dest;

    res = bmallocator_alloc(&(st->msix_alloc), msix_index);
    assert(res);

    err = get_apicid_from_core(core, &dest);
    assert(err_is_ok(err));

    err = pci_msix_vector_init(*msix_index, dest, vector);
    assert(err_is_ok(err));

    DEBUG("e10k: MSI-X vector setup index=%"PRIx64", core=%d apic=%d swvec=%x\n",
            *msix_index, core, dest, vector);
}



static void management_interrupt(struct e10k_driver_state* st, e10k_eicr_t eicr)
{
    if (e10k_eicr_ecc_extract(eicr)) {
        DEBUG("##########################################\n");
        DEBUG("ECC Error, resetting device :-/\n");
        DEBUG("##########################################\n");
        device_init(st);
    } else if (eicr >> 16) {
        DEBUG("Interrupt: %x\n", eicr);
        e10k_eicr_prtval(st->buf, sizeof(st->buf), eicr);
        puts(st->buf);
    } else if (st->msix) {
        DEBUG("Weird management interrupt without cause: eicr=%x\n", eicr);
    }
}

static void interrupt_handler_msix(void* arg)
{
    struct e10k_driver_state* st = (struct e10k_driver_state*) arg;

    DEBUG("e10k: MSI-X management interrupt\n");
    e10k_eicr_t eicr = e10k_eicr_rd(st->d);

    eicr &= ~(1 << st->cdriver_msix);
    management_interrupt(st, eicr);

    // Ensure management MSI-X vector is cleared
    e10k_eicr_wr(st->d, (1 << st->cdriver_msix));

    // Reenable interrupt
    e10k_eimsn_cause_wrf(st->d, st->cdriver_msix / 32, (1 << (st->cdriver_msix % 32)));
}


/*
static void resend_interrupt(void* arg)
{
    errval_t err;
    struct e10k_vf_binding* b = (struct e10k_vf_binding*) arg;
    err = b->tx_vtbl.interrupt(b, NOP_CONT, i);
    // If the queue is busy, there is already an oustanding message
    if (err_is_fail(err) && err != FLOUNDER_ERR_TX_BUSY) {
        USER_PANIC("Error when sending interrupt %s \n", err_getstring(err));
    }
}
*/

/** Here are the global interrupts handled. */
static void interrupt_handler(void* arg)
{
    struct e10k_driver_state* st = (struct e10k_driver_state*) arg;

    errval_t err;
    e10k_eicr_t eicr = e10k_eicr_rd(st->d);

    if (eicr >> 16) {
        management_interrupt(st, eicr);
    }
    e10k_eicr_wr(st->d, eicr);

    DEBUG("@@@@@@@@@@@@@ Interrupt @@@@@@@@@@@@@@@@@@ \n");
    for (uint64_t i = 0; i < 16; i++) {
        if ((eicr >> i) & 0x1) {
            DEBUG("Interrupt eicr=%"PRIx32" \n", eicr);
            if (st->queues[i].use_irq && st->queues[i].devif != NULL) {
                err = st->queues[i].devif->tx_vtbl.interrupt(st->queues[i].devif, NOP_CONT, i);
                if (err_is_fail(err)) {
                    /*
                    err = st->queues[i].devif->register_send(st->queues[i].devif,
                                                            get_default_waitset(),
                                                            MKCONT(resend_interrupt,
                                                                  st->queues[i].devif));
                    */
                    // Do nothing since the interrupt is still outstanding
                }
            }
        }
    }
}

/** Stop whole device. */
static void stop_device(struct e10k_driver_state* st)
{
    int i = 0;

    DEBUG("Stopping device\n");

    // Disable RX and TX
    rx_disable(st);
    tx_disable(st);
    st->rxtx_enabled = false;

    // Disable interrupts
    e10k_eimc_cause_wrf(st->d, 0x7FFFFFFF);
    e10k_eicr_rd(st->d);

    // Disable each RX and TX queue
    for (i = 0; i < 128; i++) {
        e10k_txdctl_wr(st->d, i, e10k_txdctl_swflsh_insert(0x0, 1));

        if (i < 64) {
            e10k_rxdctl_1_wr(st->d, i, 0x0);
        } else {
            e10k_rxdctl_2_wr(st->d, i - 64, 0x0);
        }

    }

    // From BSD driver (not in spec)
    milli_sleep(2);

    // Master disable procedure
    e10k_ctrl_pcie_md_wrf(st->d, 1);
    while (e10k_status_pcie_mes_rdf(st->d) != 0); // TODO: Timeout
    DEBUG("Stopping device done\n");
}

/**
 * Initialize hardware registers.
 * Is also called after a reset of the device.
 */
static void device_init(struct e10k_driver_state* st)
{
    int i;
    e10k_ctrl_t ctrl;
    e10k_pfqde_t pfqde;
    errval_t err;
    bool initialized_before = st->initialized;

    st->initialized = 0;

    stop_device(st);

    if (initialized_before) {
        // Save queue heads and tails
        for (i = 0; i < 128; i++) {
            if (st->queues[i].enabled) {
                st->queues[i].tx_head = e10k_tdh_rd(st->d, i);
                if (i < 64) {
                    st->queues[i].rx_head = e10k_rdh_1_rd(st->d, i);
                } else {
                    st->queues[i].rx_head = e10k_rdh_2_rd(st->d, i - 64);
                }
            }
        }
    }

    // Make a double reset to be sure
    for (i = 0; i < 2; i++) {
        // Issue Global reset
        ctrl = e10k_ctrl_rd(st->d);
        ctrl = e10k_ctrl_lrst_insert(ctrl, 1);
        ctrl = e10k_ctrl_rst_insert(ctrl, 1);
        e10k_ctrl_wr(st->d, ctrl);
        while ((e10k_ctrl_rst_rdf(st->d) != 0) ||
               (e10k_ctrl_lrst_rdf(st->d) != 0)); // TODO: Timeout

        // Spec says 10, fbsd driver 50
        milli_sleep(50);
    }
    DEBUG("Global reset done\n");

    // Disable interrupts
    e10k_eimc_cause_wrf(st->d, 0x7FFFFFFF);
    e10k_eicr_rd(st->d);

    // Let firmware know that we have taken over
    e10k_ctrl_ext_drv_load_wrf(st->d, 1);

    // NO Snoop disable (from FBSD)
    // Without this, the driver only works on sbrinz1 if the receive buffers are
    // mapped non cacheable. If the buffers are mapped cacheable, sometimes we
    // seem to read old buffer contents, not sure exactly why, as far as
    // understood this, No snoop should only be enabled by the device if it is
    // save...
    // TODO: Also check performance implications of this on gottardo and other
    // machnies where it works without this.
    e10k_ctrl_ext_ns_dis_wrf(st->d, 1);

    // Initialize flow-control registers
    for (i = 0; i < 8; i++) {
        if (i < 4) e10k_fcttv_wr(st->d, i, 0x0);
        e10k_fcrtl_wr(st->d, i, 0x0);
        e10k_fcrth_wr(st->d, i, 0x0);
    }
    e10k_fcrtv_wr(st->d, 0x0);
    e10k_fccfg_wr(st->d, 0x0);

    // Initialize Phy
    e10k_phy_init(st->d);

    // Wait for EEPROM auto read
    while (e10k_eec_auto_rd_rdf(st->d) == 0); // TODO: Timeout
    DEBUG("EEPROM auto read done\n");

    // Wait for DMA initialization
    while (e10k_rdrxctl_dma_initok_rdf(st->d) == 0); // TODO: Timeout

    DEBUG("DMA UP\n");
    // Wait for link to come up
    while(e10k_links_lnk_up_rdf(st->d) == 0);

    DEBUG("Link Up\n");
    milli_sleep(50);

    // Initialize interrupts
    e10k_eicr_wr(st->d, 0xffffffff);
    if (st->msix) {
        // Switch to MSI-X mode
        e10k_gpie_msix_wrf(st->d, 1);
        e10k_gpie_pba_sup_wrf(st->d, 1);
        e10k_gpie_ocd_wrf(st->d, 1);

        // Allocate msix vector for cdriver and set up handler
        if (st->cdriver_msix == -1) {
            err = pci_setup_inthandler(interrupt_handler_msix, NULL, &(st->cdriver_vector));
            assert(err_is_ok(err));

            setup_interrupt(st, &(st->cdriver_msix), disp_get_core_id(), st->cdriver_vector);
        }

        // Map management interrupts to our vector
        e10k_ivar_misc_i_alloc0_wrf(st->d, st->cdriver_msix);
        e10k_ivar_misc_i_alloc1_wrf(st->d, st->cdriver_msix);
        e10k_ivar_misc_i_allocval0_wrf(st->d, 1);
        e10k_ivar_misc_i_allocval1_wrf(st->d, 1);

        // Enable auto masking of interrupt
        e10k_gpie_eiame_wrf(st->d, 1);
        e10k_eiamn_wr(st->d, st->cdriver_msix / 32, (1 << (st->cdriver_msix % 32)));

        // Set no interrupt delay
        e10k_eitr_l_wr(st->d, st->cdriver_msix, 0);
        e10k_gpie_eimen_wrf(st->d, 1);

        // Enable interrupt
        e10k_eimsn_wr(st->d, st->cdriver_msix / 32, (1 << (st->cdriver_msix % 32)));
    } else {
        e10k_gpie_msix_wrf(st->d, 0);
        // Set no Interrupt delay
        e10k_eitr_l_wr(st->d, 0, 0);
        e10k_gpie_eimen_wrf(st->d, 1);

        // Enable all interrupts
        e10k_eimc_wr(st->d, e10k_eims_rd(st->d));
        e10k_eims_cause_wrf(st->d, 0x7fffffff);
    }

    // Just a guess for RSC delay
    e10k_gpie_rsc_delay_wrf(st->d, 2);

    // Initialize multiple register tables (MAC 0 and 127 are not set)
    for (i = 0; i < 128; i++) {
        /* uint64_t mac = e10k_ral_ral_rdf(d, i) | ((uint64_t) e10k_rah_rah_rdf(d, i) << 32); */
        /* uint8_t *m = (uint8_t *)&mac; */
        /* DEBUG("Old MAC %d: %02hhx:%02hhx:%02hhx:%02hhx:%02hhx:%02hhx ... mac valid = %x\n", */
        /*       i, m[0], m[1], m[2], m[3], m[4], m[5], e10k_rah_av_rdf(d, 0)); */

        if(i > 0 && i < 127) {
            e10k_ral_wr(st->d, i, st->mactable[i].as64 & 0xffffffff);
            e10k_rah_wr(st->d, i, st->mactable[i].as64 >> 32);
            e10k_rah_av_wrf(st->d, i, 1);

            /* mac = e10k_ral_ral_rdf(d, i) | ((uint64_t) e10k_rah_rah_rdf(d, i) << 32); */
            /* m = (uint8_t *)&mac; */
            /* DEBUG("New MAC %d: %02hhx:%02hhx:%02hhx:%02hhx:%02hhx:%02hhx ... mac valid = %x\n", */
            /*       i, m[0], m[1], m[2], m[3], m[4], m[5], e10k_rah_av_rdf(d, 0)); */
        }
    }
    for (i = 0; i < 128; i++)
        e10k_mta_bit_vec_wrf(st->d, i, 0);
    for (i = 0; i < 128; i++)
        e10k_vfta_vlan_flt_wrf(st->d, i, 0);
    for (i = 0; i < 128; i++)
        e10k_pfvlvfb_wr(st->d, i, 0);

    for (i = 0; i < 64; i++) {
        if (st->vtdon_dcboff) {
            e10k_pfvlvf_vi_en_wrf(st->d, i, 1);
        } else {
            e10k_pfvlvf_vi_en_wrf(st->d, i, 0);
        }

        e10k_psrtype_wr(st->d, i, 0);    
    }

    for (i = 0; i < 128; i++)
        e10k_pfuta_wr(st->d, i, 0);

    for (i = 0; i < 256; i++)
        e10k_mpsar_pool_ena_wrf(st->d, i, 0);

    // Program direct match MAC forwarding rules
    // This setup will assign the first 64 MAC addresses each to a different
    // RX pool. This assumes we have 64 VFs. The rest is set to filtered.
    for(i = 0; i < 128; i++) {
        if(i < 32) {
            // Pools < 32 (low bits)
            e10k_mpsar_pool_ena_wrf(st->d, 2 * i, 1 << i);
            e10k_mpsar_pool_ena_wrf(st->d, 2 * i + 1, 0);
        } else if(i < 64) {
            // Pools >= 32 and < 64 (high bits)
            e10k_mpsar_pool_ena_wrf(st->d, 2 * i, 0);
            e10k_mpsar_pool_ena_wrf(st->d, 2 * i + 1, 1 << (i - 32));
        } else {
            // Pools >= 64 -> DROP
            e10k_mpsar_pool_ena_wrf(st->d, 2 * i, 0);
            e10k_mpsar_pool_ena_wrf(st->d, 2 * i + 1, 0);
        }
    }

    for (i = 0; i < 128; i++) {
        e10k_fhft_1_wr(st->d, i, 0);
        if (i < 64) {
            e10k_fhft_2_wr(st->d, i, 0);
        }
    }

    if (st->vtdon_dcboff) {
        // Disallow per-queue RSC (not supported in SR-IOV mode)
        e10k_rfctl_rsc_dis_wrf(st->d, 1);
    } else {
        // Allow for per-queue RSC
        e10k_rfctl_rsc_dis_wrf(st->d, 0);
    }

    // Initialize RX filters
    for (i = 0; i < 128; i++) {
        e10k_ftqf_wr(st->d, i, 0);
        e10k_saqf_wr(st->d, i, 0);
        e10k_daqf_wr(st->d, i, 0);
        e10k_sdpqf_wr(st->d, i, 0);
    }
    for (i = 0; i < 32; i++)
        e10k_reta_wr(st->d, i, 0);
    e10k_mcstctrl_mfe_wrf(st->d, 0);

    // Accept broadcasts
    e10k_fctrl_bam_wrf(st->d, 1);

    // Enable Jumbo frames
    e10k_hlreg0_jumboen_wrf(st->d, 1);
    e10k_maxfrs_mfs_wrf(st->d, 15872);

    // Make sure Rx CRC strip is consistently enabled in HLREG0 and RDRXCTL
    e10k_hlreg0_rxcrcstrp_wrf(st->d, 1);
    // Note: rscfrstsz has to be set to 0 (is mbz)
    e10k_rdrxctl_t rdrxctl = e10k_rdrxctl_rd(st->d);
    rdrxctl = e10k_rdrxctl_crcstrip_insert(rdrxctl, 1);
    e10k_rdrxctl_wr(st->d, rdrxctl);


    // Configure buffers etc. according to specification
    // Section 4.6.11.3.4 (DCB, virtualization, no RSS)
    // 1:1 from spec, though not sure if everything is necessary, but since
    // initialization is still buggy, I'd rather be conservative and set some
    // additional flags, even if they aren't strictly necessary.
    e10k_rttdcs_arbdis_wrf(st->d, 1);

    if (st->vtdon_dcboff) {
        e10k_rxpbsize_size_wrf(st->d, 0, 0x200);
        e10k_txpbsize_size_wrf(st->d, 0, 0xA0);
        e10k_txpbthresh_thresh_wrf(st->d, 0, 0xA0);
        for (i = 1; i < 8; i++) {
            e10k_rxpbsize_size_wrf(st->d, i, 0x0);
            e10k_txpbsize_size_wrf(st->d, i, 0x0);
            e10k_txpbthresh_thresh_wrf(st->d, i, 0x0);
        }

        e10k_mrqc_mrque_wrf(st->d, e10k_vrt_only);
        e10k_mtqc_rt_en_wrf(st->d, 0);
        e10k_mtqc_vt_en_wrf(st->d, 1);
        e10k_mtqc_num_tc_wrf(st->d, 2);
        e10k_pfvtctl_vt_en_wrf(st->d, 1);
    } else {
        e10k_rxpbsize_size_wrf(st->d, 0, 0x200);
        e10k_txpbsize_size_wrf(st->d, 0, 0xA0);
        e10k_txpbthresh_thresh_wrf(st->d, 0, 0xA0);
        for (i = 1; i < 8; i++) {
            e10k_rxpbsize_size_wrf(st->d, i, 0x0);
            e10k_txpbsize_size_wrf(st->d, i, 0x0);
            e10k_txpbthresh_thresh_wrf(st->d, i, 0x0);
        }

        e10k_mrqc_mrque_wrf(st->d, e10k_no_rss);
        e10k_mtqc_rt_en_wrf(st->d, 0);
        e10k_mtqc_vt_en_wrf(st->d, 0);
        e10k_mtqc_num_tc_wrf(st->d, 0);
        e10k_pfvtctl_vt_en_wrf(st->d, 0);
    }

    e10k_rtrup2tc_wr(st->d, 0);
    e10k_rttup2tc_wr(st->d, 0);


    if (st->vtdon_dcboff) {
        e10k_dtxmxszrq_max_bytes_wrf(st->d, 0xFFF);
    } else {
       e10k_dtxmxszrq_max_bytes_wrf(st->d, 0xFFF);
    }

    e10k_rttdcs_arbdis_wrf(st->d, 0);

    for (i = 0; i < 128; i++) {
        pfqde = e10k_pfqde_queue_idx_insert(0x0, i);
        pfqde = e10k_pfqde_we_insert(pfqde, 1);
        // XXX: Might want to set drop enable here
        /* pfqde = e10k_pfqde_qde_insert(pfqde, 1); */
        e10k_pfqde_wr(st->d, pfqde);
    }

    if (st->vtdon_dcboff) {
        e10k_mflcn_rpfce_wrf(st->d, 0);
        e10k_mflcn_rfce_wrf(st->d, 1);
        e10k_fccfg_tfce_wrf(st->d, e10k_lfc_en);
    } else {
        e10k_mflcn_rpfce_wrf(st->d, 0);
        e10k_mflcn_rfce_wrf(st->d, 1);
        e10k_fccfg_tfce_wrf(st->d, e10k_lfc_en);
    }

    if (st->vtdon_dcboff) {
        /* Causes ECC error (could be same problem as with l34timir (see e10k.dev) */
        e10k_rttdqsel_txdq_idx_wrf(st->d, 0);
        e10k_rttdt1c_wr(st->d, 0x3FFF);
        e10k_rttbcnrc_wr(st->d, 0);

        for (i = 1; i < 64; i++) {
            e10k_rttdqsel_txdq_idx_wrf(st->d, i);
            e10k_rttdt1c_wr(st->d, st->credit_refill[i]);   // Credit refill x 64 bytes
            e10k_rttbcnrc_wr(st->d, 0);
        }
    } else {
        /* Causes ECC error (could be same problem as with l34timir (see e10k.dev) */
        for (i = 0; i < 128; i++) {
            e10k_rttdqsel_txdq_idx_wrf(st->d, i);
            e10k_rttdt1c_wr(st->d, st->credit_refill[i]);   // Credit refill x 64 bytes
            e10k_rttbcnrc_wr(st->d, 0);
            if(st->tx_rate[i] != 0) {
                // Turn on rate scheduler for this queue and set rate factor
                e10k_rttbcnrc_t rttbcnrc = 0;
                // XXX: Assuming 10Gb/s link speed. Change if that's not correct.
                uint32_t tx_factor = (10000 << 14) / st->tx_rate[i];

                rttbcnrc = e10k_rttbcnrc_rf_dec_insert(rttbcnrc, tx_factor & 0x3fff);
                rttbcnrc = e10k_rttbcnrc_rf_int_insert(rttbcnrc, tx_factor >> 14);
                rttbcnrc = e10k_rttbcnrc_rs_ena_insert(rttbcnrc, 1);
                e10k_rttbcnrc_wr(st->d, rttbcnrc);

                printf("Setting rate for queue %d to %u\n", i, st->tx_rate[i]);
            }
        }
    }

    for (i = 0; i < 8; i++) {
        e10k_rttdt2c_wr(st->d, i, 0);
        e10k_rttpt2c_wr(st->d, i, 0);
        e10k_rtrpt4c_wr(st->d, i, 0);
    }

    if (st->vtdon_dcboff) {
        e10k_rttdcs_tdpac_wrf(st->d, 0);
        e10k_rttdcs_vmpac_wrf(st->d, 1);        // Remember to set RTTDT1C >= MTU when this is 1

        e10k_rttdcs_tdrm_wrf(st->d, 0);
        e10k_rttdcs_bdpm_wrf(st->d, 1);
        e10k_rttdcs_bpbfsm_wrf(st->d, 0);
        e10k_rttpcs_tppac_wrf(st->d, 0);
        e10k_rttpcs_tprm_wrf(st->d, 0);
        e10k_rttpcs_arbd_wrf(st->d, 0x224);
        e10k_rtrpcs_rac_wrf(st->d, 0);
        e10k_rtrpcs_rrm_wrf(st->d, 0);
    } else {
        e10k_rttdcs_tdpac_wrf(st->d, 0);
        e10k_rttdcs_vmpac_wrf(st->d, 0);
        e10k_rttdcs_tdrm_wrf(st->d, 0);
        e10k_rttdcs_bdpm_wrf(st->d, 1);
        e10k_rttdcs_bpbfsm_wrf(st->d, 1);
        e10k_rttpcs_tppac_wrf(st->d, 0);
        e10k_rttpcs_tprm_wrf(st->d, 0);
        e10k_rttpcs_arbd_wrf(st->d, 0x224);
        e10k_rtrpcs_rac_wrf(st->d, 0);
        e10k_rtrpcs_rrm_wrf(st->d, 0);
    }

    // disable relaxed ordering
    for (i = 0; i < 128; i++) {
        e10k_dca_txctrl_txdesc_wbro_wrf(st->d, i, 0);
        if (i < 64) {
            e10k_dca_rxctrl_1_rxhdr_ro_wrf(st->d, i, 0);
            e10k_dca_rxctrl_1_rxdata_wrro_wrf(st->d, i, 0);
        } else {
            e10k_dca_rxctrl_2_rxhdr_ro_wrf(st->d, i - 64, 0);
            e10k_dca_rxctrl_2_rxdata_wrro_wrf(st->d, i - 64, 0);
        }
    }

    // disable all queues
    for (i = 0; i < 128; i++) {
        e10k_txdctl_enable_wrf(st->d, i, 0);
        if (i < 64) {
            e10k_rxdctl_1_enable_wrf(st->d, i, 0);
        } else {
            e10k_rxdctl_2_enable_wrf(st->d, i - 64, 0);
        }
    }

    for(i = 0; i < 64; i++) {
        e10k_pfvml2flt_mpe_wrf(st->d, i, 1);
        e10k_pfvml2flt_bam_wrf(st->d, i, 1);
        e10k_pfvml2flt_aupe_wrf(st->d, i, 1);
    }

    if (st->dca) {
        e10k_dca_ctrl_t dca_ctrl = 0;
        dca_ctrl = e10k_dca_ctrl_dca_mode_insert(dca_ctrl, e10k_dca10);
        e10k_dca_ctrl_wr(st->d, dca_ctrl);
        printf("DCA globally enabled\n");
    }


    DEBUG("Card initialized (%d)\n", initialized_before);

    // Restore configuration
    if (initialized_before) {
        // Restoring filters
        for (i = 0; i < 128; i++) {
            if (st->filters[i].enabled) {
                e10k_flt_ftqf_setup(st, i, (st->filters) + i);
            }
        }

        // Restoring queues
        for (i = 0; i < 128; i++) {
            if (st->queues[i].enabled) {
                queue_hw_init(st, i, true);
            }
        }

        DEBUG("Configuration restored\n");
    }

    st->initialized = 1;
}

/** Initialize hardware queue n. */
static void queue_hw_init(struct e10k_driver_state* st, uint8_t n, bool set_tail)
{
    errval_t r;
    struct frame_identity frameid = { .base = 0, .bytes = 0 };
    uint64_t tx_phys, txhwb_phys, rx_phys;
    size_t tx_size, rx_size;
    bool enable_global = !st->rxtx_enabled;

    // Get physical addresses for rx/tx rings
    r = invoke_frame_identify(st->queues[n].tx_frame, &frameid);
    assert(err_is_ok(r));
    tx_phys = frameid.base;
    tx_size = frameid.bytes;

    r = invoke_frame_identify(st->queues[n].rx_frame, &frameid);
    assert(err_is_ok(r));
    rx_phys = frameid.base;
    rx_size = frameid.bytes;

    DEBUG("tx.phys=%"PRIx64" tx.size=%"PRIu64"\n", tx_phys, tx_size);
    DEBUG("rx.phys=%"PRIx64" rx.size=%"PRIu64"\n", rx_phys, rx_size);


    // Initialize RX queue in HW
    if (st->queues[n].rx_va) {
        e10k_rdbal_1_wr(st->d, n, st->queues[n].rx_va);
        e10k_rdbah_1_wr(st->d, n, (st->queues[n].rx_va) >> 32);
    } else {
        e10k_rdbal_1_wr(st->d, n, rx_phys);
        e10k_rdbah_1_wr(st->d, n, rx_phys >> 32);
    }
    e10k_rdlen_1_wr(st->d, n, rx_size);

    e10k_srrctl_1_bsz_pkt_wrf(st->d, n, st->queues[n].rxbufsz / 1024);
    uint32_t hdrsz = st->queues[n].rxhdrsz;
    if (hdrsz == 0) {
        hdrsz = 128;
    }
    assert(hdrsz % 64 == 0);
    assert(hdrsz >= 128 && hdrsz <= 1024);

    e10k_srrctl_1_bsz_hdr_wrf(st->d, n, hdrsz / 64);
    // Enable header split if desired
    if (st->queues[n].rxhdrsz != 0) {
        e10k_srrctl_1_desctype_wrf(st->d, n, e10k_adv_hdrsp);
        // Split packets after TCP, UDP, IP4, IP6 and L2 headers if we enable
        // header split
        e10k_psrtype_split_tcp_wrf(st->d, n, 1);
        e10k_psrtype_split_udp_wrf(st->d, n, 1);
        e10k_psrtype_split_ip4_wrf(st->d, n, 1);
        e10k_psrtype_split_ip6_wrf(st->d, n, 1);
        e10k_psrtype_split_l2_wrf(st->d, n, 1);
    } else {
        e10k_srrctl_1_desctype_wrf(st->d, n, e10k_adv_1buf);
        //e10k_srrctl_1_desctype_wrf(st->d, n, e10k_legacy);
    }
    e10k_srrctl_1_bsz_hdr_wrf(st->d, n, 128 / 64); // TODO: Do 128 bytes suffice in
                                               //       all cases?
    e10k_srrctl_1_drop_en_wrf(st->d, n, 1);

    // Set RSC status
    if (st->queues[n].use_rsc) {
        USER_PANIC("RSC not supported in SR-IOV mode!\n");
        e10k_rscctl_1_maxdesc_wrf(st->d, n, 3);
        e10k_rscctl_1_rsc_en_wrf(st->d, n, 1);
        // TODO: (how) does this work for queues >=64?
        e10k_psrtype_split_tcp_wrf(st->d, n, 1); // needed for RSC
    } else {
        e10k_rscctl_1_maxdesc_wrf(st->d, n, 0);
        e10k_rscctl_1_rsc_en_wrf(st->d, n, 0);
    }

    // Initialize queue pointers (empty)
    e10k_rdt_1_wr(st->d, n, st->queues[n].rx_head);
    e10k_rdh_1_wr(st->d, n, st->queues[n].rx_head);

    if (st->vtdon_dcboff) {
        // Open virtualization pool gate (assumes 64 VF mapping)
        e10k_pfvfre_wr(st->d, n / 64, e10k_pfvfre_rd(st->d, n / 64) | (1 << ((n / 2) % 32)));
    }

    e10k_rxdctl_1_enable_wrf(st->d, n, 1);
    while (e10k_rxdctl_1_enable_rdf(st->d, n) == 0); // TODO: Timeout
    DEBUG("[%x] RX queue enabled\n", n);

    // Setup Interrupts for this queue
    if (st->queues[n].use_irq) {
        uint8_t rxv, txv;
        // Look for interrupt vector
        if (st->queues[n].msix_intvec != 0) {
            if (st->queues[n].msix_index == -1) {
                setup_interrupt(st, &(st->queues[n].msix_index), st->queues[n].msix_intdest,
                                st->queues[n].msix_intvec);
            }
            rxv = txv = st->queues[n].msix_index;
        } else {
            //rxv = QUEUE_INTRX;
            //txv = QUEUE_INTTX;
            rxv = n % 16;
            txv = n % 16;
        }
        DEBUG("rxv=%d txv=%d\n", rxv, txv);

        // Setup mapping queue Rx/Tx -> interrupt
        uint8_t i = n / 2;
        if ((n % 2) == 0) {
            e10k_ivar_i_alloc0_wrf(st->d, i, rxv);
            e10k_ivar_i_allocval0_wrf(st->d, i, 1);
            e10k_ivar_i_alloc1_wrf(st->d, i, txv);
            e10k_ivar_i_allocval1_wrf(st->d, i, 1);
        } else {
            e10k_ivar_i_alloc2_wrf(st->d, i, rxv);
            e10k_ivar_i_allocval2_wrf(st->d, i, 1);
            e10k_ivar_i_alloc3_wrf(st->d, i, txv);
            e10k_ivar_i_allocval3_wrf(st->d, i, 1);
        }
        if (st->queues[n].msix_intvec != 0) {
            e10k_eitr_l_wr(st->d, rxv, 0);

            // Enable autoclear (higher ones are always auto cleared)
            if (rxv < 16) {
                e10k_eiac_rtxq_wrf(st->d, e10k_eiac_rtxq_rdf(st->d) | (1 << rxv));
            }

        }
        if (rxv < 16) {
            // Make sure interrupt is cleared
            e10k_eicr_wr(st->d, 1 << rxv);
        }

        // Enable interrupt
        e10k_eimsn_wr(st->d, rxv / 32, (1 << (rxv % 32)));
    }

    // Enable RX
    if (enable_global) {
        DEBUG("[%x] Enabling RX globally...\n", n);
        rx_enable(st);
        DEBUG("[%x] RX globally enabled\n", n);
    }

    if (st->dca){
        // Enable DCA for this queue
        e10k_dca_rxctrl_t dca_rxctrl = 0;

        dca_rxctrl = e10k_dca_rxctrl_rxdca_desc_insert(dca_rxctrl, 1);
        dca_rxctrl = e10k_dca_rxctrl_rxdca_hdr_insert(dca_rxctrl, 1);
        dca_rxctrl = e10k_dca_rxctrl_rxdca_payl_insert(dca_rxctrl, 1);

        uint8_t my_apic_id;
        errval_t err = sys_debug_get_apic_id(&my_apic_id);
        assert(err_is_ok(err));

        dca_rxctrl = e10k_dca_rxctrl_cpuid_insert(dca_rxctrl, my_apic_id);

        if(n < 64) {
            e10k_dca_rxctrl_1_wr(st->d, n, dca_rxctrl);
        } else {
            e10k_dca_rxctrl_2_wr(st->d, n - 64, dca_rxctrl);
        }

        printf("DCA enabled on queue %d with APIC ID %d\n", n, my_apic_id);
    }

    // Initialize TX queue in HW
    if (st->queues[n].rx_va) {
        e10k_tdbal_wr(st->d, n, st->queues[n].tx_va);
        e10k_tdbah_wr(st->d, n, (st->queues[n].tx_va) >> 32);
    } else {
        e10k_tdbal_wr(st->d, n, tx_phys);
        e10k_tdbah_wr(st->d, n, tx_phys >> 32);
    }
    e10k_tdlen_wr(st->d, n, tx_size);

    // Initialize TX head index write back
    if (!capref_is_null(st->queues[n].txhwb_frame)) {
        DEBUG("[%x] TX hwb enabled ...\n", n);
        r = invoke_frame_identify(st->queues[n].txhwb_frame, &frameid);
        assert(err_is_ok(r));
        txhwb_phys = frameid.base;
        if (st->queues[n].rx_va) {
            e10k_tdwbal_headwb_low_wrf(st->d, n, (st->queues[n].txhwb_va) >> 2);
            e10k_tdwbah_headwb_high_wrf(st->d, n, (st->queues[n].txhwb_va) >> 32);
        } else {
            e10k_tdwbal_headwb_low_wrf(st->d, n, txhwb_phys >> 2);
            e10k_tdwbah_headwb_high_wrf(st->d, n, txhwb_phys >> 32);
        }
        e10k_tdwbal_headwb_en_wrf(st->d, n, 1);
    }

    // Initialized by queue driver to avoid race conditions
    // Initialize queue pointers
    e10k_tdh_wr(st->d, n, st->queues[n].tx_head);
    e10k_tdt_wr(st->d, n, st->queues[n].tx_head);

    // Configure prefetch and writeback threshhold
    e10k_txdctl_pthresh_wrf(st->d, n, 8); // FIXME: Figure out what the right number
                                      //        is here.
    e10k_txdctl_hthresh_wrf(st->d, n, 0);
    e10k_txdctl_wthresh_wrf(st->d, n, 0);

    if (enable_global) {
        DEBUG("[%x] Enabling TX globally...\n", n);
        tx_enable(st);
        st->rxtx_enabled = true;
        DEBUG("[%x] TX globally enabled\n", n);
    }

    if (st->vtdon_dcboff) {
        // Open virtualization pool gate (assumes 64 VF mapping)
        e10k_pfvfte_wr(st->d, n / 64, e10k_pfvfte_rd(st->d, n / 64) | (1 << ((n / 2) % 32)));
    }

    e10k_txdctl_enable_wrf(st->d, n, 1);
    while (e10k_txdctl_enable_rdf(st->d, n) == 0); // TODO: Timeout
    DEBUG("[%x] TX queue enabled\n", n);

    // Some initialization stuff from BSD driver
    e10k_dca_txctrl_txdesc_wbro_wrf(st->d, n, 0);
}

/** Stop queue. */
static void queue_hw_stop(struct e10k_driver_state* st, uint8_t n)
{
    // This process is described in 4.6.7.1.2

    // Disable TX for this queue
    e10k_txdctl_enable_wrf(st->d, n, 0);

    // TODO: Flush packet buffers
    // TODO: Remove all filters
    // TODO: With RSC we have to wait here (see spec), not used atm

    // Disable RX for this queue
    e10k_rxdctl_1_enable_wrf(st->d, n, 0);
    while (e10k_rxdctl_1_enable_rdf(st->d, n) != 0); // TODO: Timeout

    // A bit too much, but make sure memory is not used anymore
    milli_sleep(1);
}

/******************************************************************************/
/* Management interface implemetation */

/** Request from queue driver to initialize hardware queue. */
/*
void cd_set_interrupt_rate(struct e10k_binding *b,
                           uint8_t n,
                           uint16_t rate)
{
    DEBUG("set_interrupt_rate(%"PRIu8")\n", n);

    uint8_t i;
    e10k_eitrn_t eitr = 0;
    eitr = e10k_eitrn_itr_int_insert(eitr, rate);

    i = (queues[n].msix_index == -1 ? 0 : queues[n].msix_index);
    if (i < 24) {
        e10k_eitr_l_wr(d, i, eitr);
    } else {
        e10k_eitr_h_wr(d, i - 24, eitr);
    }
}
*/
/****** VF/PF/Queue interface server interface *******/

static void init_done_vf(struct e10k_vf_binding *b, uint8_t vfn)
{
    struct e10k_driver_state* st = (struct e10k_driver_state*) b->st;

    if (!st->rxtx_enabled) {
        rx_enable(st);
        tx_enable(st);
        st->rxtx_enabled = true;
    } 

    assert(vfn < 64);

    DEBUG("VF %d init done\n", vfn);

    // Enable correct pool for VF
    e10k_pfvfre_wr(st->d, vfn / 32, e10k_pfvfre_rd(st->d, vfn / 32) | (1 << (vfn % 32)));
    e10k_pfvfte_wr(st->d, vfn / 32, e10k_pfvfte_rd(st->d, vfn / 32) | (1 << (vfn % 32)));

    if(vfn < 32) {
        e10k_pfvflrec_wr(st->d, 0, 1 << vfn);
    } else {
        e10k_pfvflrec_wr(st->d, 1, 1 << (vfn - 32));
    }

    errval_t err = b->tx_vtbl.init_done_response(b, NOP_CONT);
    assert(err_is_ok(err));
}

static void request_vf_number(struct e10k_vf_binding *b)
{
    struct e10k_driver_state* st = (struct e10k_driver_state*) b->st;
    assert(st->initialized);
    DEBUG("VF allocated\n");
    errval_t err;
    uint8_t vf_num = 255;
    uint64_t d_mac = 0;
    for (int i = 0; i < 64; i++) {
        if (!st->vf_used[i]) {
            vf_num = i;
            break;
        }
    }

    struct capref regs, irq, pci_ep, iommu_ep;
    if (vf_num == 255){
        //TODO better error
        err = NIC_ERR_ALLOC_QUEUE;
    } else {
        debug_printf("Enable VF \n");
        err = pci_sriov_enable_vf(vf_num);
        if (err_is_fail(err)) {
            st->vf_used[vf_num] = 0;
            goto out;
        }        

        debug_printf("PCI request VF bar \n");

        err = pci_sriov_get_vf_resources(vf_num, &regs, &irq, &iommu_ep, 
                                         &pci_ep);
        if (err_is_fail(err)) {
            st->vf_used[vf_num] = 0;
            goto out;
        }

        debug_printf("PCI Enabled VF \n");
        err = SYS_ERR_OK;
    }


    d_mac = e10k_ral_ral_rdf(st->d, vf_num) | ((uint64_t) e10k_rah_rah_rdf(st->d, vf_num) << 32);

out:
    err = b->tx_vtbl.request_vf_number_response(b, NOP_CONT, vf_num, d_mac, 
                                                regs, irq, iommu_ep, pci_ep, err);
    assert(err_is_ok(err));
}


static errval_t request_vf_number_rpc(struct e10k_vf_binding *b, uint8_t* vf_num, 
                                      uint64_t* mac, struct capref* regs, struct capref* irq,
                                      struct capref* iommu_ep, struct capref* pci_ep,
                                      errval_t* err)
{
    struct e10k_driver_state* st = (struct e10k_driver_state*) b->st;
    DEBUG("VF allocated RPC \n");

    for (int i = 0; i < 64; i++) {
        if (!st->vf_used[i]) {
            *vf_num = i;
            break;
        }
    }

    if (*vf_num == 255){
        //TODO better error
        *err = NIC_ERR_ALLOC_QUEUE;
    } else {
        debug_printf("Enable VF \n");
        *err = pci_sriov_enable_vf(*vf_num);
        if (err_is_fail(*err)) {
            st->vf_used[*vf_num] = 0;
            return *err;
        }        

        debug_printf("PCI request VF bar \n");

        *err = pci_sriov_get_vf_resources(*vf_num, regs, irq, iommu_ep, 
                                         pci_ep);
        if (err_is_fail(*err)) {
            st->vf_used[*vf_num] = 0;
            return *err;
        }

        debug_printf("PCI Enabled VF \n");
        *err = SYS_ERR_OK;
    }

    *mac = e10k_ral_ral_rdf(st->d, *vf_num) | ((uint64_t) e10k_rah_rah_rdf(st->d, *vf_num) << 32);
    return SYS_ERR_OK;
}


static errval_t cd_create_queue_rpc(struct e10k_vf_binding *b,
                                    struct capref tx_frame, struct capref txhwb_frame,
                                    struct capref rx_frame, uint32_t rxbufsz,
                                    int16_t msix_intvec, uint8_t msix_intdest,
                                    bool use_irq, bool use_rsc, bool default_q,
                                    uint64_t *mac, int32_t *qid, struct capref *regs,
                                    errval_t *ret_err)
{
    // TODO: Make sure that rxbufsz is a power of 2 >= 1024
    struct e10k_driver_state* st = (struct e10k_driver_state*) b->st;
    if (use_irq && msix_intvec != 0 && !st->msix) {
        printf("e10k: Queue requests MSI-X, but MSI-X is not enabled "
                " card driver. Ignoring queue\n");
        *ret_err = NIC_ERR_ALLOC_QUEUE;
        return NIC_ERR_ALLOC_QUEUE;
    }

    // allocate a queue
    int n = -1;
    for (int i = 1; i < 128; i++) {
        if (!st->queues[i].enabled) {
            n = i;
            break;
        }
    }

    if (default_q) {
        if (st->queues[0].enabled == false) {
            n = 0;
        } else {
            printf("Default queue already initalized \n");
            return NIC_ERR_ALLOC_QUEUE;
        }
    }

    DEBUG("create queue(%"PRIu8": interrupt %d )\n", n, use_irq);

    if (n == -1) {
        *ret_err = NIC_ERR_ALLOC_QUEUE;
        return NIC_ERR_ALLOC_QUEUE;
    }

    // Save state so we can restore the configuration in case we need to do a
    // reset
    
    st->queues[n].tx_frame = tx_frame;
    st->queues[n].txhwb_frame = txhwb_frame;
    st->queues[n].rx_frame = rx_frame;
    st->queues[n].tx_head = 0;
    st->queues[n].rx_head = 0;
    st->queues[n].devif = b;
    st->queues[n].rxbufsz = rxbufsz;
    st->queues[n].msix_index = -1;
    st->queues[n].msix_intvec = msix_intvec;
    st->queues[n].msix_intdest = msix_intdest;
    st->queues[n].use_irq = use_irq;
    st->queues[n].use_rsc = use_rsc;
    st->queues[n].enabled = true;
    

    queue_hw_init(st, n, false);

    // TODO for now vfn = 0
    uint64_t d_mac = e10k_ral_ral_rdf(st->d, 0) | ((uint64_t) e10k_rah_rah_rdf(st->d, 0) << 32);

    *regs = st->regframe;
    *qid = n;
    *mac = d_mac;

    DEBUG("[%d] Queue int done\n", n);
    *ret_err = SYS_ERR_OK;
    return SYS_ERR_OK;
}

static void cd_create_queue(struct e10k_vf_binding *b,
                            struct capref tx_frame, struct capref txhwb_frame,
                            struct capref rx_frame, uint32_t rxbufsz,
                            int16_t msix_intvec, uint8_t msix_intdest,
                            bool use_irq, bool use_rsc, bool default_q)
{

    uint64_t mac;
    int queueid;
    errval_t err;

    struct capref regs;

    err = cd_create_queue_rpc(b, tx_frame, txhwb_frame, rx_frame,
                              rxbufsz, msix_intvec, msix_intdest, use_irq, use_rsc,
                              default_q, &mac, &queueid, &regs, &err);

    err = b->tx_vtbl.create_queue_response(b, NOP_CONT, mac, queueid, regs, err);
    assert(err_is_ok(err));
    DEBUG("cd_create_queue end\n");
}

static void vf_export_cb(void *st, errval_t err, iref_t iref)
{
    struct e10k_driver_state* dev = (struct e10k_driver_state* ) st; 
    const char *suffix = "_vf";
    char name[strlen(dev->service_name) + strlen(suffix) + 100];

    assert(err_is_ok(err));

    // Build label for interal management service
    sprintf(name, "%s%s%u", dev->service_name, suffix, dev->pci_function);

    err = nameservice_register(name, iref);
    assert(err_is_ok(err));
    DEBUG("VF/PF interface [%s] exported\n", name);
    dev->exported = true;
}

static errval_t vf_connect_cb(void *st, struct e10k_vf_binding *b)
{
    DEBUG("New connection on VF/PF interface\n");

    b->rx_vtbl.create_queue_call = cd_create_queue;
    b->rx_vtbl.request_vf_number_call = request_vf_number;
    b->rx_vtbl.init_done_call = init_done_vf;

    b->rpc_rx_vtbl.create_queue_call = cd_create_queue_rpc;
    b->rpc_rx_vtbl.request_vf_number_call = request_vf_number_rpc;
    b->st = st;
    return SYS_ERR_OK;
}

/**
 * Initialize management interface for queue drivers.
 * This has to be done _after_ the hardware is initialized.
 */
static void initialize_vfif(struct e10k_driver_state* st)
{
    errval_t r;

    r = e10k_vf_export(st, vf_export_cb, vf_connect_cb, get_default_waitset(),
		       IDC_BIND_FLAGS_DEFAULT);
    assert(err_is_ok(r));

    r = net_filter_export(st, net_filter_export_cb, net_filter_connect_cb,
                          get_default_waitset(), IDC_BIND_FLAGS_DEFAULT);
    assert(err_is_ok(r));
}

/******************************************************************************/
/* Initialization code for driver */

/** Callback from pci to initialize a specific PCI device. */
static void init_card(struct e10k_driver_state* st)
{
    errval_t err;

    st->d = calloc(sizeof(e10k_t), 1);

    lvaddr_t vaddr;
    /* Map first BAR for register access */
    err = driverkit_get_bar_cap(st->bfi, 0, &st->regframe);
    if (err_is_fail(err)) {
        USER_PANIC("pcid_get_bar_cap failed \n");
    }

    err = map_device_cap(st->regframe, &vaddr);
    if (err_is_fail(err)) {
        USER_PANIC("map_device_cap failed \n");
    }

    e10k_initialize(st->d, (void *) vaddr);

    /* Initialize Mackerel binding */
    assert(st->d != NULL);

    // Initialize manager for MSI-X vectors
    if (st->msix) {
        DEBUG("Enabling MSI-X interrupts\n");
        uint16_t msix_count = 0;
        err = pci_msix_enable(&msix_count);
        assert(err_is_ok(err));
        assert(msix_count > 0);
        DEBUG("MSI-X #vecs=%d\n", msix_count);

        bool res;
        res = bmallocator_init(&(st->msix_alloc), msix_count);
        assert(res);
    } else {
        DEBUG("Using legacy interrupts\n");
    }

    // Initialize hardware registers etc.
    DEBUG("Initializing hardware\n");
    device_init(st);

    assert(st->initialized);

    if (st->vtdon_dcboff) {
        DEBUG("SR-IOV device up routine\n");

        // Setup support for 64 VFs
        e10k_gcr_ext_vtmode_wrf(st->d, e10k_vt_64);
        e10k_gpie_vtmode_wrf(st->d, e10k_vt_64);

        // Enable virtualization, disable default pool, replication enable
        e10k_pfvtctl_t pfvtctl = e10k_pfvtctl_rd(st->d);
        pfvtctl = e10k_pfvtctl_vt_en_insert(pfvtctl, 1);
        pfvtctl = e10k_pfvtctl_def_pl_insert(pfvtctl, 0);
        pfvtctl = e10k_pfvtctl_dis_def_pl_insert(pfvtctl, 1);
        pfvtctl = e10k_pfvtctl_rpl_en_insert(pfvtctl, 1);
        e10k_pfvtctl_wr(st->d, pfvtctl);

        // Enable L2 loopback
        // TODO set to 1 again, 
        // for now disabled advanced for descirptors without offloading functionality
        e10k_pfdtxgswc_lbe_wrf(st->d, 0); 

        // TODO: Accept untagged packets in all VMDQ pools
        // TODO: Broadcast accept mode
        // TODO: Accept packets matching PFUTA table
        // TODO: Accept packets matching MTA table
        // TODO: Accept untagged packets enable
        // TODO: Strip VLAN tag for incoming packets

        DEBUG("STATUS = %x\n", e10k_status_rd(st->d));

        e10k_ctrl_ext_pfrstd_wrf(st->d, 1);
    }

    DEBUG("Initializing VF/PF interface\n");
    initialize_vfif(st);
    DEBUG("Done with initialization\n");
}

static void parse_cmdline(struct e10k_driver_state* st, int argc, char **argv)
{
    int i;

    for (i = 1; i < argc; i++) {
        if (strncmp(argv[i], "cardname=", strlen("cardname=")) == 0) {
            st->service_name = argv[i] + strlen("cardname=");
        } else if (strncmp(argv[i], "bus=", strlen("bus=")) == 0) {
            st->pci_bus = atol(argv[i] + strlen("bus="));
        } else if (strncmp(argv[i], "device=", strlen("device=")) == 0) {
            st->pci_device = atol(argv[i] + strlen("device="));
        } else if (strncmp(argv[i], "function=", strlen("function=")) == 0) {
            st->pci_function = atol(argv[i] + strlen("function="));
        } else if (strncmp(argv[i], "deviceid=", strlen("deviceid=")) == 0) {
            st->pci_deviceid = strtoul(argv[i] + strlen("deviceid="), NULL, 0);
        } else if (strncmp(argv[i], "msix=", strlen("msix=")) == 0) {
            st->msix = !!atol(argv[i] + strlen("msix="));
	    } else if (strncmp(argv[i], "credit_refill[", strlen("credit_refill[") - 1) == 0) {
            // Controls the WRR (weighted round-robin) scheduler's credit refill rate
            // This seems to be per VM pool
            unsigned int entry, val;
            int r = sscanf(argv[i], "credit_refill[%u]=%u", &entry, &val);
            assert(r == 2);
            assert(entry < 128);
            assert(val < 0x3fff);
            st->credit_refill[entry] = val;
	    } else if (strncmp(argv[i], "tx_rate[", strlen("tx_rate[") - 1) == 0) {
            // This is specified in Mbits/s and must be >= 10 and <= link speed (typically 10,000)
            // This seems to be per Tx queue
            unsigned int entry, val;
            int r = sscanf(argv[i], "tx_rate[%u]=%u", &entry, &val);
            assert(r == 2);
            assert(entry < 128);
            assert(val >= 10 && val <= 10000);
            st->tx_rate[entry] = val;
        }
    }
}

static void init_default_values(struct e10k_driver_state* e10k)
{
    e10k->service_name = "e10k";
    e10k->d = NULL;
    e10k->msix = false;
    e10k->vtdon_dcboff = false;
    e10k->dca = false;
    e10k->cdriver_msix = -1;
    e10k->initialized = 0;
    e10k->exported = false;
    e10k->rxtx_enabled = false;
    e10k->pci_bus = PCI_DONT_CARE;
    e10k->pci_device = PCI_DONT_CARE;
    e10k->pci_function = 0;
    e10k->pci_deviceid = E10K_PCI_DEVID;

    union macentry mactable[128] = {
        { .as8 = "\x0\x0\x0\x0\x0\x0" },      // First MAC is never set (loaded from card EEPROM)
        { .as8 = "\x22\xc9\xfc\x96\x83\xfc" },
        { .as8 = "\xce\x43\x5b\xf7\x3e\x60" },
        { .as8 = "\x6a\xb0\x62\xf6\xa7\x21" },
        { .as8 = "\xb2\xdf\xf9\x39\xc6\x10" },
        { .as8 = "\x92\x77\xe7\x3f\x80\x30" },
        { .as8 = "\xd6\x88\xd6\x86\x4a\x22" },
        { .as8 = "\x7e\x64\xe9\x2e\xbe\x4b" },
        { .as8 = "\xba\xac\x49\xd6\x3c\x77" },

        // We set the rest to all zeroes

        // Last MAC (127) never set (loaded from card EEPROM ... at least, it's already there)
    };

    memcpy(e10k->mactable, mactable, sizeof(mactable));

}

/**
 * Driver initialization function. This function is called by the driver domain
 * (see also 'create_handler' in ddomain_service.c).
 * Typically through a request from the device manager.
 *
 * The init function is supposed to set `dev` to the exported service iref.
 * The init function may use `bfi->dstate` to store additional state about the device.
 *
 * \param[in]   bfi   The instance of this driver.
 * \param[in]   name  The name of this driver instance.
 * \param[in]   flags Additional flags (The exact flags supported is device/driver specific).
 * \param[in]   c     Capabilities (for registers etc.) as provided by the device manager.
 *                    The exact layout of the `c` is device specific.
 * \param[out]  dev   The service iref over which the device can be contacted.
 *
 * \retval SYS_ERR_OK Device initialized successfully.
 * \retval LIB_ERR_MALLOC_FAIL Unable to allocate memory for the driver.
 */
static errval_t init(struct bfdriver_instance *bfi, uint64_t flags, iref_t *dev)
{
    errval_t err;
    //barrelfish_usleep(10*1000*1000);
    DEBUG("PF driver started\n");

    bfi->dstate = calloc(sizeof(struct e10k_driver_state), 1);
    if (bfi->dstate == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }
    
    assert(bfi->dstate != NULL);
    struct e10k_driver_state* st = (struct e10k_driver_state*) bfi->dstate;
    st->caps = bfi->caps;

    init_default_values(st);
    st->bfi = bfi;

    // credit_refill value must be >= 1 for a queue to be able to send.
    // Set them all to 1 here. May be overridden via commandline.
    for(int i = 0; i < 128; i++) {
        st->credit_refill[i] = 0xFF;
    }

    memset(st->tx_rate, 0, sizeof(st->tx_rate));

    parse_cmdline(st, bfi->argc, bfi->argv);
 
    struct capref devcap = NULL_CAP;
    err = driverkit_get_iommu_cap(bfi, &devcap);
    

    if (!capref_is_null(devcap) && err_is_ok(err)) {
        DEBUG("VTD-Enabled initializing with VFs enabled \n");
        st->vtdon_dcboff = true;

        err = driverkit_iommu_client_init_cl(devcap, &st->iommu);
        if (err_is_fail(err)) {
            DEBUG("VTD-Enabled initializing with VFs disabled. Try turing on IOMMU! \n");
            st->vtdon_dcboff = false;
        }
        DEBUG("VTD-Enabled initializing with VFs enabled \n");
    }

    struct capref cap;
    // When started by Kaluga it handend off an endpoint cap to PCI
    err = driverkit_get_pci_cap(bfi, &cap);
    assert(err_is_ok(err));
    assert(!capref_is_null(cap));

    debug_printf("Connect to PCI\n");
    err = pci_client_connect_ep(cap);
    assert(err_is_ok(err));

    init_card(st);

    struct capref intcap = NULL_CAP;
    err = driverkit_get_interrupt_cap(bfi, &intcap);
    assert(err_is_ok(err));
    err = int_route_client_route_and_connect(intcap, 0,
                                             get_default_waitset(), 
                                             interrupt_handler, st);
    if (err_is_fail(err)) {
        USER_PANIC("Interrupt setup failed!\n");
    }

    while (!st->initialized || !st->exported) {
        event_dispatch(get_default_waitset());
    }
    
    debug_printf("PF driver init done\n");

    return SYS_ERR_OK;
}


/**
 * Instructs driver to attach to the device.
 * This function is only called if the driver has previously detached
 * from the device (see also detach).
 *
 * \note After detachment the driver can not assume anything about the
 * configuration of the device.
 *
 * \param[in]   bfi   The instance of this driver.
 * \retval SYS_ERR_OK Device initialized successfully.
 */
static errval_t attach(struct bfdriver_instance* bfi) {
    return SYS_ERR_OK;
}

/**
 * Instructs driver to detach from the device.
 * The driver must yield any control over to the device after this function returns.
 * The device may be left in any state.
 *
 * \param[in]   bfi   The instance of this driver.
 * \retval SYS_ERR_OK Device initialized successfully.
 */
static errval_t detach(struct bfdriver_instance* bfi) {
    return SYS_ERR_OK;
}

/**
 * Instructs the driver to go in a particular sleep state.
 * Supported states are platform/device specific.
 *
 * \param[in]   bfi   The instance of this driver.
 * \retval SYS_ERR_OK Device initialized successfully.
 */
static errval_t set_sleep_level(struct bfdriver_instance* bfi, uint32_t level) {
    return SYS_ERR_OK;
}

/**
 * Destroys this driver instance. The driver will yield any
 * control over the device and free any state allocated.
 *
 * \param[in]   bfi   The instance of this driver.
 * \retval SYS_ERR_OK Device initialized successfully.
 */
static errval_t destroy(struct bfdriver_instance* bfi) {
    struct e10k_driver_state* st = (struct e10k_driver_state*) bfi->dstate;
    for (int i = 0; i < 128; i++) {
        queue_hw_stop(st, i);
    }
    return SYS_ERR_OK;
}

struct e10k_vf_rx_vtbl vtbl = {
    .create_queue_call = cd_create_queue,
    .request_vf_number_call = request_vf_number,
    .init_done_call = init_done_vf
};

static errval_t get_ep(struct bfdriver_instance* bfi, bool lmp, struct capref* ret_cap)
{
    DEBUG("Endpoint was requested \n");
    errval_t err;
    struct e10k_vf_binding* b;
    err = e10k_vf_create_endpoint(lmp? IDC_ENDPOINT_LMP: IDC_ENDPOINT_UMP, 
                                  &vtbl, bfi->dstate,
                                  get_default_waitset(),
                                  IDC_ENDPOINT_FLAGS_DUMMY,
                                  &b, *ret_cap);
    
    return err;
}
/**
 * Registers the driver module with the system.
 *
 * To link this particular module in your driver domain,
 * add it to the addModules list in the Hakefile.
 */
DEFINE_MODULE(e10k_module, init, attach, detach, set_sleep_level, destroy, get_ep);
