/*
 * Copyright (c) 2007-2011, ETH Zurich.
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

#include <if/e10k_defs.h>
#include <dev/e10k_dev.h>

#include "e10k.h"
#include "sleep.h"

//#define DEBUG(x...) printf("e10k: " x)
#define DEBUG(x...) do {} while (0)


struct queue_state {
    bool enabled;
    struct e10k_binding *binding;

    struct capref tx_frame;
    struct capref txhwb_frame;
    struct capref rx_frame;
    uint32_t rxbufsz;

    uint64_t rx_head;
    uint64_t tx_head;
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


static void idc_write_queue_tails(struct e10k_binding *b);
static void stop_device(void);

static void device_init(void);
static void queue_hw_init(uint8_t n);
static void queue_hw_stop(uint8_t n);

static void e10k_flt_ftqf_setup(int index, struct e10k_filter *filter);
//static void e10k_flt_etype_setup(int filter, int queue, uint16_t etype);

static const char *service_name = "e10k";
uint64_t d_mac;
static int initialized = 0;
static e10k_t *d = NULL;
static struct capref *regframe;

/** Specifies if RX/TX is currently enabled on the device. */
static bool rxtx_enabled = false;

// State of queues and filters
static struct queue_state queues[128];
static struct e10k_filter filters[128];

static char buf[4096];

/* PCI device address passed on command line */
static uint32_t pci_bus = PCI_DONT_CARE;
static uint32_t pci_device = PCI_DONT_CARE;
static uint32_t pci_function = 0;




static void e10k_flt_ftqf_setup(int idx, struct e10k_filter* filter)
{
    uint16_t m = filter->mask;
    e10k_l4_proto_t p;
    e10k_ftqf_t ftqf = 0;
    e10k_l34timir_t timir = 0;
    e10k_sdpqf_t sdpqf = 0;


    // Write filter data
    if (!(m & MASK_SRCIP))
        e10k_saqf_wr(d, idx, htonl(filter->src_ip));
    if (!(m & MASK_DSTIP))
        e10k_daqf_wr(d, idx, htonl(filter->dst_ip));
    if (!(m & MASK_SRCPORT))
        sdpqf = e10k_sdpqf_src_port_insert(sdpqf, htons(filter->src_port));
    if (!(m & MASK_DSTPORT))
        sdpqf = e10k_sdpqf_dst_port_insert(sdpqf, htons(filter->dst_port));
    e10k_sdpqf_wr(d, idx, sdpqf);


    if (!(m & MASK_L4PROTO)) {
        switch (filter->l4_type) {
            case L4_OTHER:  p = e10k_l4other; break;
            case L4_UDP:    p = e10k_l4udp; break;
            case L4_TCP:    p = e10k_l4tcp; break;
            case L4_SCTP:   p = e10k_l4sctp; break;
            default: assert(0);
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
    e10k_l34timir_wr(d, idx, timir);

    ftqf = e10k_ftqf_priority_insert(ftqf, filter->priority);
    ftqf = e10k_ftqf_pool_mask_insert(ftqf, 1);
    ftqf = e10k_ftqf_queue_en_insert(ftqf, 1);
    e10k_ftqf_wr(d, idx, ftqf);
}

static int ftqf_index = 0;
static int ftqf_alloc(void)
{
    // FIXME: Do this reasonably
    return ftqf_index++;
}

static errval_t reg_ftfq_filter(struct e10k_filter* f, uint64_t* fid)
{
    int i;

    DEBUG("reg_ftfq_filter: called\n");

    if ((i = ftqf_alloc()) < 0) {
        return FILTER_ERR_NOT_ENOUGH_MEMORY;
    }


    filters[i] = *f;
    filters[i].enabled = true;

    e10k_flt_ftqf_setup(i, f);

    *fid = i + 1;

    return SYS_ERR_OK;
}



#if 0
static void e10k_flt_etype_setup(int filter, int queue, uint16_t etype)
{
    // Clear existing values
    e10k_etqf_wr(d, filter, 0x0);
    e10k_etqs_wr(d, filter, 0x0);

    e10k_etqs_rx_queue_wrf(d, filter, queue);
    e10k_etqs_queue_en_wrf(d, filter, 1);

    e10k_etqf_etype_wrf(d, filter, etype);
    e10k_etqf_filter_en_wrf(d, filter, 1);
}


static errval_t arp_filter(uint64_t qid, uint64_t* fid)
{
    e10k_flt_etype_setup(0, (int) qid, 0x0806);
    *fid = 0;
    DEBUG("reg_arp_filter: called\n");
    return SYS_ERR_OK;
}

static errval_t reg_ftfq_filter(struct e10k_filter* f, uint64_t* fid)
{
    int i;

    DEBUG("reg_ftfq_filter: called\n");

    if ((i = ftqf_alloc()) < 0) {
        return ETHERSRV_ERR_NOT_ENOUGH_MEM;
    }


    filters[i] = *f;
    filters[i].enabled = true;

    e10k_flt_ftqf_setup(i, f);

    *fid = i + 1;

    return SYS_ERR_OK;
}

static errval_t ipv4_tcp_port(uint64_t qid, uint16_t port, uint64_t* fid)
{
    struct e10k_filter f = {
        .dst_port = port,
        .mask = MASK_SRCIP | MASK_DSTIP | MASK_SRCPORT,
        .l4_type = L4_TCP,
        .priority = 1,
        .queue = qid,
    };

    DEBUG("ipv4_tcp_port: called\n");
    return reg_ftfq_filter(&f, fid);
}

static errval_t ipv4_udp_port(uint64_t qid, uint16_t port, uint64_t* fid)
{
    struct e10k_filter f = {
        .dst_port = port,
        .mask = MASK_SRCIP | MASK_DSTIP | MASK_SRCPORT,
        .l4_type = L4_UDP,
        .priority = 1,
        .queue = qid,
    };

    DEBUG("ipv4_udp_port: called\n");
    return reg_ftfq_filter( &f, fid);
}

static errval_t ipv4_tcp_conn(uint64_t qid,
                              uint32_t l_ip, uint16_t l_port,
                              uint32_t r_ip, uint16_t r_port,
                              uint64_t* fid)
{
    struct e10k_filter f = {
        .dst_ip = l_ip,
        .dst_port = l_port,
        .src_ip = r_ip,
        .src_port = r_port,
        .mask = 0,
        .l4_type = L4_TCP,
        .priority = 0,
        .queue = qid,
    };

    DEBUG("ipv4_tcp_conn: called\n");
    return reg_ftfq_filter(&f, fid);
}

static errval_t deregister_filter(uint64_t fid)
{
    DEBUG("deregister_filter: called\n");
    return LIB_ERR_NOT_IMPLEMENTED;
}

#endif


/** Enable RX operation for whole card. */
static void rx_enable(void)
{
    e10k_secrxctrl_rx_dis_wrf(d, 1);
    while (e10k_secrxstat_sr_rdy_rdf(d) == 0); // TODO: Timeout
    e10k_rxctrl_rxen_wrf(d, 1);
    e10k_secrxctrl_rx_dis_wrf(d, 0);
}

/** Disable RX operation for whole card. */
static void rx_disable(void)
{
    e10k_secrxctrl_rx_dis_wrf(d, 1);
    while (e10k_secrxstat_sr_rdy_rdf(d) == 0); // TODO: Timeout
    e10k_rxctrl_rxen_wrf(d, 0);
    e10k_secrxctrl_rx_dis_wrf(d, 0);
}

/** Enable TX operation for whole card. */
static void tx_enable(void)
{
    e10k_dmatxctl_txen_wrf(d, 1);
}

/** Disable TX operation for whole card. */
static void tx_disable(void)
{
    e10k_dmatxctl_txen_wrf(d, 0);
    while (e10k_dmatxctl_txen_rdf(d) != 0); // TODO: timeout
}



/**
 * Initialize hardware registers.
 * Is also called after a reset of the device.
 */
static void device_init(void)
{
    int i;
    e10k_ctrl_t ctrl;
    e10k_pfqde_t pfqde;
    bool initialized_before = initialized;

    initialized = 0;

    stop_device();

    if (initialized_before) {
        // Save queue heads and tails
        for (i = 0; i < 128; i++) {
            if (queues[i].enabled) {
                queues[i].tx_head = e10k_tdh_rd(d, i);
                if (i < 64) {
                    queues[i].rx_head = e10k_rdh_1_rd(d, i);
                } else {
                    queues[i].rx_head = e10k_rdh_2_rd(d, i - 64);
                }
            }
        }
    }

    // Make a double reset to be sure
    for (i = 0; i < 2; i++) {
        // Issue Global reset
        ctrl = e10k_ctrl_rd(d);
        ctrl = e10k_ctrl_lrst_insert(ctrl, 1);
        ctrl = e10k_ctrl_rst_insert(ctrl, 1);
        e10k_ctrl_wr(d, ctrl);
        while ((e10k_ctrl_rst_rdf(d) != 0) ||
               (e10k_ctrl_lrst_rdf(d) != 0)); // TODO: Timeout

        // Spec says 10, fbsd driver 50
        milli_sleep(50);
    }
    DEBUG("Global reset done\n");

    // Disable interrupts
    e10k_eimc_cause_wrf(d, 0x7FFFFFFF);
    e10k_eicr_rd(d);

    // Let firmware know that we have taken over
    e10k_ctrl_ext_drv_load_wrf(d, 1);

    // NO Snoop disable (from FBSD)
    // Without this, the driver only works on sbrinz1 if the receive buffers are
    // mapped non cacheable. If the buffers are mapped cacheable, sometimes we
    // seem to read old buffer contents, not sure exactly why, as far as
    // understood this, No snoop should only be enabled by the device if it is
    // save...
    // TODO: Also check performance implications of this on gottardo and other
    // machnies where it works without this.
    e10k_ctrl_ext_ns_dis_wrf(d, 1);

    // Initialize flow-control registers
    for (i = 0; i < 8; i++) {
        if (i < 4) e10k_fcttv_wr(d, i, 0x0);
        e10k_fcrtl_wr(d, i, 0x0);
        e10k_fcrth_wr(d, i, 0x0);
    }
    e10k_fcrtv_wr(d, 0x0);
    e10k_fccfg_wr(d, 0x0);

    // Initialize Phy
    e10k_phy_init(d);

    // Wait for EEPROM auto read
    while (e10k_eec_auto_rd_rdf(d) == 0); // TODO: Timeout
    DEBUG("EEPROM auto read done\n");


    // Wait for DMA initialization
    // Hangs, but should work according to spec :-/
    //while (e10k_rdrxctl_dma_initok_rdf(d) == 0); // TODO: Timeout

    d_mac = e10k_ral_ral_rdf(d, 0) | ((uint64_t) e10k_rah_rah_rdf(d, 0) << 32);
    DEBUG("mac valid = %x\n", e10k_rah_av_rdf(d, 0));

    // Wait for link to come up
    while (e10k_links_lnk_up_rdf(d) == 0); // TODO: Timeout
    DEBUG("Link Up\n");
    milli_sleep(50);

    // Initialize interrupts
    e10k_eicr_wr(d, 0xffffffff);
    e10k_gpie_eimen_wrf(d, 1);

    e10k_eimc_wr(d, e10k_eims_rd(d));
    e10k_eims_cause_wrf(d, 0x7fffffff);

    // Initialize multiple register tables
    for (i = 1; i < 128; i++) {
        e10k_ral_wr(d, i, 0);
        e10k_rah_wr(d, i, 0);
    }
    for (i = 0; i < 128; i++)
        e10k_mta_bit_vec_wrf(d, i, 0);
    for (i = 0; i < 128; i++)
        e10k_vfta_vlan_flt_wrf(d, i, 0);
    for (i = 0; i < 64; i++) {
        e10k_pfvlvf_vi_en_wrf(d, i, 0);
        e10k_psrtype_wr(d, i, 0);
    }
    for (i = 0; i < 128; i++)
        e10k_pfuta_wr(d, i, 0);
    for (i = 0; i < 256; i++)
        e10k_mpsar_pool_ena_wrf(d, i, 0);
    for (i = 0; i < 128; i++) {
        e10k_fhft_1_wr(d, i, 0);
        if (i < 64) {
            e10k_fhft_2_wr(d, i, 0);
        }
    }

    // Initialize RX filters
    for (i = 0; i < 128; i++) {
        e10k_ftqf_wr(d, i, 0);
        e10k_saqf_wr(d, i, 0);
        e10k_daqf_wr(d, i, 0);
        e10k_sdpqf_wr(d, i, 0);
    }
    for (i = 0; i < 32; i++)
        e10k_reta_wr(d, i, 0);
    e10k_mcstctrl_mfe_wrf(d, 0);

    // Accept broadcasts
    e10k_fctrl_bam_wrf(d, 1);

    // Spec says that rscfrstsz has to be set to 0 (is mbz)
    e10k_rdrxctl_wr(d, e10k_rdrxctl_rd(d));



    // Configure buffers etc. according to specification
    // Section 4.6.11.3.4 (No DCP, no virtualization, no RSS)
    // 1:1 from spec, though not sure if everything is necessary, but since
    // initialization is still buggy, I'd rather be conservative and set some
    // additional flags, even if they aren't strictly necessary.
    e10k_rxpbsize_size_wrf(d, 0, 0x200);
    e10k_txpbsize_size_wrf(d, 0, 0xA0);
    e10k_txpbthresh_thresh_wrf(d, 0, 0xA0);
    for (i = 1; i < 8; i++) {
        e10k_rxpbsize_size_wrf(d, i, 0x0);
        e10k_txpbsize_size_wrf(d, i, 0x0);
        e10k_txpbthresh_thresh_wrf(d, i, 0x0);
    }

    e10k_mrqc_mrque_wrf(d, e10k_no_rss);
    e10k_mtqc_rt_en_wrf(d, 0);
    e10k_mtqc_vt_en_wrf(d, 0);
    e10k_mtqc_num_tc_wrf(d, 0);
    e10k_pfvtctl_vt_en_wrf(d, 0);
    e10k_rtrup2tc_wr(d, 0);
    e10k_rttup2tc_wr(d, 0);

    e10k_dtxmxszrq_max_bytes_wrf(d, 0xFFF);

    for (i = 0; i < 128; i++) {
        pfqde = e10k_pfqde_queue_idx_insert(0x0, i);
        pfqde = e10k_pfqde_we_insert(pfqde, 1);
        e10k_pfqde_wr(d, pfqde);
    }

    e10k_mflcn_rpfce_wrf(d, 0);
    e10k_mflcn_rfce_wrf(d, 0);
    e10k_fccfg_tfce_wrf(d, e10k_lfc_en);

    /* Causes ECC error (could be same problem as with l34timir (see e10k.dev)
    for (i = 0; i < 128; i++) {
        e10k_rttdqsel_txdq_idx_wrf(d, i);
        e10k_rttdt1c_wr(d, 0);
    }*/
    for (i = 0; i < 8; i++) {
        e10k_rttdt2c_wr(d, i, 0);
        e10k_rttpt2c_wr(d, i, 0);
        e10k_rtrpt4c_wr(d, i, 0);
    }

    e10k_rttdcs_tdpac_wrf(d, 0);
    e10k_rttdcs_vmpac_wrf(d, 0);
    e10k_rttdcs_tdrm_wrf(d, 0);
    e10k_rttdcs_bdpm_wrf(d, 1);
    e10k_rttdcs_bpbfsm_wrf(d, 1);
    e10k_rttpcs_tppac_wrf(d, 0);
    e10k_rttpcs_tprm_wrf(d, 0);
    e10k_rttpcs_arbd_wrf(d, 0x224);
    e10k_rtrpcs_rac_wrf(d, 0);
    e10k_rtrpcs_rrm_wrf(d, 0);



    // disable relaxed ordering
    for (i = 0; i < 128; i++) {
        e10k_dca_txctrl_txdesc_wbro_wrf(d, i, 0);
        if (i < 64) {
            e10k_dca_rxctrl_1_rxhdr_ro_wrf(d, i, 0);
            e10k_dca_rxctrl_1_rxdata_wrro_wrf(d, i, 0);
        } else {
            e10k_dca_rxctrl_2_rxhdr_ro_wrf(d, i - 64, 0);
            e10k_dca_rxctrl_2_rxdata_wrro_wrf(d, i - 64, 0);
        }
    }

    // disable all queues
    for (i = 0; i < 128; i++) {
        e10k_txdctl_enable_wrf(d, i, 0);
        if (i < 64) {
            e10k_rxdctl_1_enable_wrf(d, i, 0);
        } else {
            e10k_rxdctl_2_enable_wrf(d, i - 64, 0);
        }
    }

    DEBUG("Card initialized (%d)\n", initialized_before);


    // Restore configuration
    if (initialized_before) {
        // Restoring filters
        for (i = 0; i < 128; i++) {
            if (filters[i].enabled) {
                e10k_flt_ftqf_setup(i, filters + i);
            }
        }

        // Restoring queues
        for (i = 0; i < 128; i++) {
            if (queues[i].enabled) {
                queue_hw_init(i);
            }
        }

        DEBUG("Configuration restored\n");
    }

    initialized = 1;
}

/** Initialize hardware queue n. */
static void queue_hw_init(uint8_t n)
{
    errval_t r;
    struct frame_identity frameid = { .base = 0, .bits = 0 };
    uint64_t tx_phys, txhwb_phys, rx_phys;
    size_t tx_size, rx_size;
    bool enable_global = !rxtx_enabled;

    // Get physical addresses for rx/tx rings
    r = invoke_frame_identify(queues[n].tx_frame, &frameid);
    assert(err_is_ok(r));
    tx_phys = frameid.base;
    tx_size = 1 << frameid.bits;

    r = invoke_frame_identify(queues[n].rx_frame, &frameid);
    assert(err_is_ok(r));
    rx_phys = frameid.base;
    rx_size = 1 << frameid.bits;


    DEBUG("tx.phys=%"PRIx64" tx.size=%"PRIu64"\n", tx_phys, tx_size);
    DEBUG("rx.phys=%"PRIx64" rx.size=%"PRIu64"\n", rx_phys, rx_size);


    // Initialize RX queue in HW
    e10k_rdbal_1_wr(d, n, rx_phys);
    e10k_rdbah_1_wr(d, n, rx_phys >> 32);
    e10k_rdlen_1_wr(d, n, rx_size);

    e10k_srrctl_1_bsz_pkt_wrf(d, n, queues[n].rxbufsz / 1024);
    e10k_srrctl_1_desctype_wrf(d, n, e10k_legacy);
    e10k_srrctl_1_drop_en_wrf(d, n, 1);

    // Initialize queue pointers (empty)
    e10k_rdt_1_wr(d, n, queues[n].rx_head);
    e10k_rdh_1_wr(d, n, queues[n].rx_head);

    e10k_rxdctl_1_enable_wrf(d, n, 1);
    while (e10k_rxdctl_1_enable_rdf(d, n) == 0); // TODO: Timeout
    DEBUG("[%x] RX queue enabled\n", n);

    // Setup Interrupts for this queue
    /*i = n / 2;
    if ((n % 2) == 0) {
        e10k_ivar_i_alloc0_wrf(d, i, n);
        e10k_ivar_i_allocval0_wrf(d, i, 1);
        e10k_ivar_i_alloc1_wrf(d, i, n);
        e10k_ivar_i_allocval1_wrf(d, i, 1);
    } else {
        e10k_ivar_i_alloc2_wrf(d, i, n);
        e10k_ivar_i_allocval2_wrf(d, i, 1);
        e10k_ivar_i_alloc3_wrf(d, i, n);
        e10k_ivar_i_allocval3_wrf(d, i, 1);
    }*/


    // Enable RX
    if (enable_global) {
        DEBUG("[%x] Enabling RX globally...\n", n);
        rx_enable();
        DEBUG("[%x] RX globally enabled\n", n);
    }


    // Initialize TX queue in HW
    e10k_tdbal_wr(d, n, tx_phys);
    e10k_tdbah_wr(d, n, tx_phys >> 32);
    e10k_tdlen_wr(d, n, tx_size);

    // Initialize TX head index write back
    if (!capref_is_null(queues[n].txhwb_frame)) {
        r = invoke_frame_identify(queues[n].txhwb_frame, &frameid);
        assert(err_is_ok(r));
        txhwb_phys = frameid.base;

        e10k_tdwbal_headwb_low_wrf(d, n, txhwb_phys >> 2);
        e10k_tdwbah_headwb_high_wrf(d, n, txhwb_phys >> 32);
        e10k_tdwbal_headwb_en_wrf(d, n, 1);
    }

    // Initialized by queue driver to avoid race conditions
    // Initialize queue pointers
    e10k_tdh_wr(d, n, queues[n].tx_head);
    e10k_tdt_wr(d, n, queues[n].tx_head);

    // Configure prefetch and writeback threshhold
    e10k_txdctl_pthresh_wrf(d, n, 8); // FIXME: Figure out what the right number
                                      //        is here.
    e10k_txdctl_hthresh_wrf(d, n, 0);
    e10k_txdctl_wthresh_wrf(d, n, 0);

    if (enable_global) {
        DEBUG("[%x] Enabling TX globally...\n", n);
        tx_enable();
        rxtx_enabled = true;
        DEBUG("[%x] TX globally enabled\n", n);
    }

    e10k_txdctl_enable_wrf(d, n, 1);
    while (e10k_txdctl_enable_rdf(d, n) == 0); // TODO: Timeout
    DEBUG("[%x] TX queue enabled\n", n);


    // Some initialization stuff from BSD driver
    e10k_dca_txctrl_txdesc_wbro_wrf(d, n, 0);

    idc_write_queue_tails(queues[n].binding);

}

/** Stop queue. */
static void queue_hw_stop(uint8_t n)
{
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
}


/** Stop whole device. */
static void stop_device(void)
{
    int i = 0;

    DEBUG("Stopping device\n");

    // Disable RX and TX
    rx_disable();
    tx_disable();
    rxtx_enabled = false;

    // Disable interrupts
    e10k_eimc_cause_wrf(d, 0x7FFFFFFF);
    e10k_eicr_rd(d);

    // Disable each RX and TX queue
    for (i = 0; i < 128; i++) {
        e10k_txdctl_wr(d, i, e10k_txdctl_swflsh_insert(0x0, 1));

        if (i < 64) {
            e10k_rxdctl_1_wr(d, i, 0x0);
        } else {
            e10k_rxdctl_2_wr(d, i - 64, 0x0);
        }

    }

    // From BSD driver (not in spec)
    milli_sleep(2);

    // Master disable procedure
    e10k_ctrl_pcie_md_wrf(d, 1);
    while (e10k_status_pcie_mes_rdf(d) != 0); // TODO: Timeout
    DEBUG("Stopping device done\n");
}

/** Here are the global interrupts handled. */
static void interrupt_handler(void* arg)
{
    e10k_eicr_t eicr = e10k_eicr_rd(d);

    e10k_eicr_wr(d, eicr);

    if (e10k_eicr_ecc_extract(eicr)) {
        DEBUG("##########################################\n");
        DEBUG("ECC Error, resetting device :-/\n");
        DEBUG("##########################################\n");
        device_init();
    } else if (eicr >> 16) {
        DEBUG("Interrupt: %x\n", eicr);
        e10k_eicr_prtval(buf, sizeof(buf), eicr);
        puts(buf);
    }
}





/******************************************************************************/
/* Management interface implemetation */

/** Send register cap and mac address to queue driver. */
static void idc_queue_init_data(struct e10k_binding *b,
                                struct capref registers,
                                uint64_t macaddr)
{
    errval_t r;
    r = e10k_queue_init_data__tx(b, NOP_CONT, registers, macaddr);
    // TODO: handle busy
    assert(err_is_ok(r));
}

/** Tell queue driver that we are done initializing the queue. */
static void idc_queue_memory_registered(struct e10k_binding *b)
{
    errval_t r;
    r = e10k_queue_memory_registered__tx(b, NOP_CONT);
    // TODO: handle busy
    assert(err_is_ok(r));
}

/** Send request to queue driver to rewrite the tail pointers of its queues. */
static void idc_write_queue_tails(struct e10k_binding *b)
{
    errval_t r;
    r = e10k_write_queue_tails__tx(b, NOP_CONT);
    // TODO: handle busy
    assert(err_is_ok(r));
}

/** Signal queue driver that the queue is stopped. */
static void idc_queue_terminated(struct e10k_binding *b)
{
    errval_t r;
    r = e10k_queue_terminated__tx(b, NOP_CONT);
    // TODO: handle busy
    assert(err_is_ok(r));
}

/** Send response about filter registration to device manager */
static void idc_filter_registered(struct e10k_binding *b,
                                  uint64_t buf_id_rx,
                                  uint64_t buf_id_tx,
                                  errval_t err,
                                  uint64_t filter)
{
    errval_t r;
    r = e10k_filter_registered__tx(b, NOP_CONT, buf_id_rx, buf_id_tx, err,
                                   filter);
    // TODO: handle busy
    assert(err_is_ok(r));
}

/** Send response about filter deregistration to device manager */
static void idc_filter_unregistered(struct e10k_binding *b,
                                    uint64_t filter,
                                    errval_t err)
{
    errval_t r;
    r = e10k_filter_unregistered__tx(b, NOP_CONT, filter, err);
    // TODO: handle busy
    assert(err_is_ok(r));
}

/** Request from queue driver for register memory cap */
static void idc_request_device_info(struct e10k_binding *b)
{
    idc_queue_init_data(b, *regframe, d_mac);
}

/** Request from queue driver to initialize hardware queue. */
static void idc_register_queue_memory(struct e10k_binding *b,
                                      uint8_t n,
                                      struct capref tx_frame,
                                      struct capref txhwb_frame,
                                      struct capref rx_frame,
                                      uint32_t rxbufsz)
{
    DEBUG("register_queue_memory(%"PRIu8")\n", n);
    // TODO: Make sure that rxbufsz is a power of 2 >= 1024

    // Save state so we can restore the configuration in case we need to do a
    // reset
    queues[n].enabled = true;
    queues[n].tx_frame = tx_frame;
    queues[n].txhwb_frame = txhwb_frame;
    queues[n].rx_frame = rx_frame;
    queues[n].tx_head = 0;
    queues[n].rx_head = 0;
    queues[n].rxbufsz = rxbufsz;
    queues[n].binding = b;

    queue_hw_init(n);

    idc_queue_memory_registered(b);
}

/**
 * Request from queue driver to stop hardware queue and free everything
 * associated with that queue.
 */
static void idc_terminate_queue(struct e10k_binding *b, uint8_t n)
{
    DEBUG("idc_terminate_queue(q=%d)\n", n);

    queue_hw_stop(n);

    queues[n].enabled = false;
    queues[n].binding = NULL;

    // TODO: Do we have to free the frame caps, or destroy the binding?
    idc_queue_terminated(b);
}

static void idc_register_port_filter(struct e10k_binding *b,
                                     uint64_t buf_id_rx,
                                     uint64_t buf_id_tx,
                                     uint8_t queue,
                                     e10k_port_type_t type,
                                     uint16_t port)
{
    struct e10k_filter f = {
        .dst_port = port,
        .mask = MASK_SRCIP | MASK_DSTIP | MASK_SRCPORT,
        .l4_type = (type == e10k_PORT_TCP ? L4_TCP : L4_UDP),
        .priority = 1,
        .queue = queue,
    };
    errval_t err;
    uint64_t fid = -1ULL;

    DEBUG("idc_register_port_filter: called (q=%d t=%d p=%d)\n",
            queue, type, port);

    err = reg_ftfq_filter(&f, &fid);
    DEBUG("filter registered: err=%"PRIu64", fid=%"PRIu64"\n", err, fid);

    idc_filter_registered(b, buf_id_rx, buf_id_tx, err, fid);
}

static void idc_unregister_filter(struct e10k_binding *b,
                                  uint64_t filter)
{
    DEBUG("unregister_filter: called (%"PRIx64")\n", filter);
    idc_filter_unregistered(b, filter, LIB_ERR_NOT_IMPLEMENTED);
}

static struct e10k_rx_vtbl rx_vtbl = {
    .request_device_info = idc_request_device_info,
    .register_queue_memory = idc_register_queue_memory,
    .terminate_queue = idc_terminate_queue,

    .register_port_filter = idc_register_port_filter,
    .unregister_filter = idc_unregister_filter,
};


static void export_cb(void *st, errval_t err, iref_t iref)
{
    const char *suffix = "_e10kmng";
    char name[strlen(service_name) + strlen(suffix) + 1];

    assert(err_is_ok(err));

    // Build label for interal management service
    sprintf(name, "%s%s", service_name, suffix);

    err = nameservice_register(name, iref);
    assert(err_is_ok(err));
    DEBUG("Management interface exported\n");
}

static errval_t connect_cb(void *st, struct e10k_binding *b)
{
    DEBUG("New connection on management interface\n");
    b->rx_vtbl = rx_vtbl;
    return SYS_ERR_OK;
}

/**
 * Initialize management interface for queue drivers.
 * This has to be done _after_ the hardware is initialized.
 */
static void initialize_mngif(void)
{
    errval_t r;

    r = e10k_export(NULL, export_cb, connect_cb, get_default_waitset(),
                    IDC_BIND_FLAGS_DEFAULT);
    assert(err_is_ok(r));
}




/******************************************************************************/
/* Initialization code for driver */

/** Callback from pci to initialize a specific PCI device. */
static void pci_init_card(struct device_mem* bar_info, int bar_count)
{
    assert(!initialized);

    d = malloc(sizeof(*d));

    // Map first BAR for register access
    assert(bar_count >= 1);
    map_device(&bar_info[0]);
    regframe = bar_info[0].frame_cap;
    DEBUG("BAR[0] mapped (v=%llx p=%llx l=%llx)\n",
            (unsigned long long) bar_info[0].vaddr,
            (unsigned long long) bar_info[0].paddr,
            (unsigned long long) bar_info[0].bytes);

    // Initialize Mackerel binding
    e10k_initialize(d, (void*) bar_info[0].vaddr);

    // Initialize hardware registers etc.
    DEBUG("Initializing hardware\n");
    device_init();

    assert(initialized);

    // Now we initialize the management interface
    DEBUG("Initializing management interface\n");
    initialize_mngif();
}


/** Register with PCI */
static void pci_register(void)
{
    errval_t r;

    r = pci_client_connect();
    assert(err_is_ok(r));
    DEBUG("connected to pci\n");

    r = pci_register_driver_irq(pci_init_card, PCI_CLASS_ETHERNET,
                                PCI_DONT_CARE, PCI_DONT_CARE,
                                PCI_VENDOR_INTEL, E10K_PCI_DEVID,
                                pci_bus, pci_device, pci_function,
                                interrupt_handler, NULL);
    assert(err_is_ok(r));
}

static void parse_cmdline(int argc, char **argv)
{
    int i;

    for (i = 1; i < argc; i++) {
        if (strncmp(argv[i], "cardname=", strlen("cardname=") - 1) == 0) {
            service_name = argv[i] + strlen("cardname=");
        }
        if (strncmp(argv[i], "bus=", strlen("bus=") - 1) == 0) {
            pci_bus = atol(argv[i] + strlen("bus="));
        }
        if (strncmp(argv[i], "device=", strlen("device=") - 1) == 0) {
            pci_device = atol(argv[i] + strlen("device="));
        }
        if (strncmp(argv[i], "function=", strlen("function=") - 1) == 0) {
            pci_function = atol(argv[i] + strlen("function="));
        }
    }
}

static void eventloop(void)
{
    struct waitset *ws;

    ws = get_default_waitset();
    while (1) {
        event_dispatch(ws);
    }
}

int main(int argc, char **argv)
{
    DEBUG("Started\n");
    parse_cmdline(argc, argv);
    pci_register();
    eventloop();
}

