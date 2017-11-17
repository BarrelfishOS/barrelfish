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

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/deferred.h>
#include <barrelfish/debug.h>
#include <pci/pci.h>

// TODO only required for htonl
#include <lwip/ip.h>
#include <net/net.h>

#include <if/sfn5122f_defs.h>
#include <if/sfn5122f_devif_defs.h>
#include <if/net_filter_defs.h>

#include "sfn5122f.h"
#include "sfn5122f_debug.h"
#include "buffer_tbl.h"

struct queue_state {
    uint64_t qid;
    bool enabled;
    bool userspace;
    bool use_irq;

    struct sfn5122f_devif_binding *devif;
    struct capref tx_frame;
    struct capref rx_frame;
    struct capref ev_frame;
    uint32_t rxbufsz;

    //bool userspace;
    uint64_t rx_head;
    uint64_t tx_head;
    uint64_t ev_head;

    // first entries of the buffer table to make up queue
    uint32_t rx_buf_tbl;
    uint32_t tx_buf_tbl;
    uint32_t ev_buf_tbl;

    // MSI-X information
    size_t msix_index;
    int16_t msix_intvec;
    uint8_t msix_intdest;
};

static bool use_msix = false;
static const char *service_name = "sfn5122f";
static sfn5122f_t *d = NULL;
static void* d_virt;
//static sfn5122f_msix_t *d_msix = NULL;
static uint64_t d_mac[2];
static int initialized = 0;
static struct capref *regframe;
/* Interrupt state  */
static struct capref int_ker;
static void* int_ker_virt;
/*  MAC stats  */
static struct capref mac_stats;
static void* mac_virt;
static uint64_t mac_phys;
/*        */

// Port  info
static uint32_t cap[2];
static uint32_t speed[2];
static uint32_t flags[2];
static uint32_t fcntl [2];

// Phy info
static uint32_t phy_caps[2];
static uint32_t phy_flags[2];
static uint32_t phy_media[2];
/* Loopback mode none and speed */
static uint32_t phy_loopback_mode = 0;
//static uint32_t phy_loopback_speed = 0;
//WoL Filter id
static uint32_t wol_filter_id = 0;

static bool csum_offload = 1;
// TX / RX
static uint32_t rx_indir_tbl[128];

// Queues
static struct queue_state queues[1024];
/* PCI device address passed on command line */
static uint32_t pci_bus = PCI_DONT_CARE;
static uint32_t pci_device = PCI_DONT_CARE;
static uint32_t pci_vendor = PCI_DONT_CARE;
static uint32_t pci_devid = PCI_DONT_CARE;
static uint32_t pci_function = 0;

static struct bmallocator msix_alloc;
static size_t cdriver_msix = -1;
static uint8_t cdriver_vector;

static bool use_interrupt = true;

// first to start everything
static bool first = 1;

/* Hash key */
uint8_t rx_hash_key[40];
uint8_t mc_hash[32];

// Filters

enum filter_type_ip {
    OTHER,
    UDP_FULL,
    TCP_FULL,
    TCP_WILDCARD,
    UDP_WILDCARD
};

/*
enum filter_type_mac {
    OTHER,
    MAC_FULL,
    MAC_WILDCARD
};
*/

struct sfn5122f_filter_ip {
    bool enabled;
    bool scatter;
    bool rss;

    uint8_t queue;

    uint32_t src_ip;
    uint32_t dst_ip;
    uint16_t src_port;
    uint16_t dst_port;

    uint16_t type_ip;
    uint16_t hash;
};

/*
struct sfn5122f_filter_mac {
    bool enabled;
    bool wildcard_match;
    bool scatter;
    bool rss;
    bool ip_override;

    uint8_t queue;

    uint64_t dst_mac;
    uint16_t vlan_id;

    uint16_t type_mac;
    uint16_t hash;
};
*/

/* scatter and rss enable */
static bool rss_en = 0;
static bool scatter_en = 0;
static struct sfn5122f_filter_ip filters_rx_ip[NUM_FILTERS_IP];
//static struct sfn5122f_filter_ip filters_tx_ip[NUM_FILTERS_IP];

/*
static struct sfn5122f_filter_mac filters_rx_ip[NUM_FILTERS_MAC];
static struct sfn5122f_filter_mac filters_tx_ip[NUM_FILTERS_MAC];
*/


/******************************************************************************/
/* Prototypes */

static void device_init(void);
static void start_all(void);
static void probe_all(void);
static uint32_t init_txq(uint16_t n, lpaddr_t phys, bool csum, bool userspace);
static uint32_t init_rxq(uint16_t n, lpaddr_t phys, bool userspace);
static uint32_t init_evq(uint16_t n, lpaddr_t phys, bool interrupt);
static void queue_hw_stop(uint16_t n);

static void setup_interrupt(size_t *msix_index, uint8_t core, uint8_t vector);
static void global_interrupt_handler(void* arg);

/***************************************************************************/
/* Filters */

static void sfn5122f_filter_port_setup(int idx, struct sfn5122f_filter_ip* filter)
{
    sfn5122f_rx_filter_tbl_lo_t filter_lo = 0;
    sfn5122f_rx_filter_tbl_hi_t filter_hi = 0;

    if (filter->type_ip == net_filter_PORT_UDP) {

        // Add destination IP
        filter_hi = sfn5122f_rx_filter_tbl_hi_dest_ip_insert(filter_hi,
                                                             filter->dst_ip);
        filter_lo = sfn5122f_rx_filter_tbl_lo_src_ip_insert(filter_lo,
                                                            0);
        filter_hi = sfn5122f_rx_filter_tbl_hi_tcp_udp_insert(filter_hi, 1);
        filter_lo = sfn5122f_rx_filter_tbl_lo_src_tcp_dest_udp_insert(
                                                filter_lo, filter->dst_port);

        filter_hi = sfn5122f_rx_filter_tbl_hi_rss_en_insert(filter_hi, 0);
        filter_hi = sfn5122f_rx_filter_tbl_hi_scatter_en_insert(filter_hi, 0);
        DEBUG("UPD filter index %d: ip_dst %x port_dst %d ip_src %x port_src %d"
               " queue %d \n",
               idx, filter->dst_ip, filter->dst_port,
               filter->src_ip, filter->src_port, filter->queue);
    }

    if (filter->type_ip == net_filter_PORT_TCP) {
        // Add dst IP and port
        filter_hi = sfn5122f_rx_filter_tbl_hi_dest_ip_insert(filter_hi,
                                                             filter->dst_ip);
        filter_lo = sfn5122f_rx_filter_tbl_lo_src_ip_insert(filter_lo,
                                                            filter->src_ip);
        filter_lo = sfn5122f_rx_filter_tbl_lo_dest_port_tcp_insert(filter_lo,
                                                                   filter->dst_port);
        filter_hi = sfn5122f_rx_filter_tbl_hi_tcp_udp_insert(filter_hi, 0);
        filter_hi = sfn5122f_rx_filter_tbl_hi_rss_en_insert(filter_hi, 0);
        filter_hi = sfn5122f_rx_filter_tbl_hi_scatter_en_insert(filter_hi, 0);
        DEBUG("TCP filter index %d: ip_dst %x port_dst %d ip_src %x port_src %d"
               " queue %d \n",
               idx, filter->dst_ip, filter->dst_port,
               filter->src_ip, filter->src_port, filter->queue);
    }

    filter_hi = sfn5122f_rx_filter_tbl_hi_rxq_id_insert(filter_hi, filter->queue);
    filter_hi = sfn5122f_rx_filter_tbl_hi_rss_en_insert(filter_hi, rss_en);
    filter_hi = sfn5122f_rx_filter_tbl_hi_scatter_en_insert(filter_hi, scatter_en);

    sfn5122f_rx_filter_tbl_lo_wr(d, idx, filter_lo);
    sfn5122f_rx_filter_tbl_hi_wr(d, idx, filter_hi);
}

static uint32_t build_key(struct sfn5122f_filter_ip* f)
{
    uint32_t data[4] = {0,0,0,0};
    uint32_t host1;
    uint32_t host2;
    uint16_t port1;
    uint16_t port2;
    host1 = f->src_ip;
    host2 = f->dst_ip;
    
    if (f->type_ip == net_filter_PORT_UDP) {
       port1 = f->dst_port;
       port2 = f->src_port;
       data[3] = 1;
    } else {
       port1 = f->src_port;
       port2 = f->dst_port;
       data[3] = 0;
    }

    data[0] = host1 << 16 | port1;
    data[1] = port2 << 16 | host1 >> 16;
    data[2] = host2;

    return data[0] ^ data[1] ^ data[2] ^ data[3];
}

static uint16_t filter_hash(uint32_t key)
{
    uint16_t tmp;

    /* First 16 rounds */
    tmp = 0x1fff ^ key >> 16;
    tmp = tmp ^ tmp >> 3 ^ tmp >> 6;
    tmp = tmp ^ tmp >> 9;
    /* Last 16 rounds */
    tmp = tmp ^ tmp << 13 ^ key;
    tmp = tmp ^ tmp >> 3 ^ tmp >> 6;
    return tmp ^ tmp >> 9;
}

/*
static bool filter_equals(struct sfn5122f_filter_ip* f1,
                          struct sfn5122f_filter_ip* f2)
{
    if (f1->type_ip != f2->type_ip) {
        return false;
    } else if ((f1->src_ip != f2->src_ip) ||
               (f1->dst_ip != f2->dst_ip) ||
               (f1->queue != f2->queue)) {
        return false;
    } else if ((f1->src_port != f2->src_port) &&
               (f2->dst_port != f1->dst_port)) {
        return false;
    } else {
        return true;
    }
}
*/
static uint16_t filter_increment(uint32_t key)
{
    return key * 2 - 1;
}

static int ftqf_alloc(struct sfn5122f_filter_ip* f)
{
    // Documentation suggest hashing using a certain algorithm
    int key = 0;
    uint16_t hash = 0;
    unsigned int incr = 0;
    uint16_t depth = 0;
    key = build_key(f);
    hash = filter_hash(key);
    incr = filter_increment(key);
    
    key = hash & (NUM_FILTERS_IP - 1);

    while (true) {
        if (filters_rx_ip[key].enabled == false) {
            return key;
        }
        
        if (depth > 3) {
            return -1;
        }
            
        key = (key + incr) & (NUM_FILTERS_IP - 1);
        depth++;
    }

    return key;
}

static errval_t reg_port_filter(struct sfn5122f_filter_ip* f, uint64_t* fid)
{
    int filt_ind;

    DEBUG("reg_port_filter: called\n");

    if ((filt_ind=ftqf_alloc(f)) < 0) {
        return FILTER_ERR_NOT_ENOUGH_MEMORY;
    }

    filters_rx_ip[filt_ind] = *f;
    filters_rx_ip[filt_ind].enabled = true;

    sfn5122f_filter_port_setup(filt_ind, f);

    // TODO +1 needed?
    *fid = filt_ind;

    return SYS_ERR_OK;
}


/***************************************************************************/
/* Helper functions*/
static void decode_link(uint32_t fcntl1 , uint32_t flags1 , uint32_t speed1)
{
    switch(fcntl1){
    case 0x3:
        DEBUG("LINK MODE: AUTO \n");
        break;
    case 0x2:
        DEBUG("LINK MODE: RX/TX \n");
        break;
    case 0x1:
        DEBUG("LINK MODE: RESPOND \n");
        break;
    case 0x0:
        DEBUG("LINK MODE: NONE \n");
        break;
    }
    DEBUG("LINK SPEED: %"PRIu32" \n", speed1);
    DEBUG("LINK FLAGS: %8lX \n", (long unsigned int) flags1);
    if (!!(flags1 & 1)) {
       DEBUG("LINK IS UP \n");
    }

    if (!!(flags1 & 1 << 0x1)) {
       DEBUG("LINK IS FULL DUPLEX \n");
    }

}

static void handle_assertions(void)
{
    uint8_t in[4];
    uint8_t out[140];
    uint32_t outlen = 0;
    errval_t err;

    memset(in, 0, sizeof(in));
    in[CMD_GET_ASSERTS_IN_CLEAR_OFFSET] = 0;

    err = mcdi_rpc(CMD_GET_ASSERTS, in , CMD_GET_ASSERTS_IN_LEN, out,
                   CMD_GET_ASSERTS_OUT_LEN, &outlen, pci_function, d);
    assert(err_is_ok(err));

    if(out[0] != 0x1){
          /* TODO handle assertions */
         printf("THERE WERE ASSERTIONS: %"PRIu8" \n ", out[0]);
         /* exit assertions -> special reboot*/
         in[0] = 0x1;
         err = mcdi_rpc(CMD_REBOOT, in, CMD_REBOOT_IN_LEN ,
                        NULL, 0, NULL, pci_function, d);
         assert(err_is_ok(err));
    }

}

/* Get Link and write settings into global variables  */
static void get_link(uint8_t port)
{
    uint8_t out[CMD_GET_LINK_OUT_LEN];
    errval_t err;

    err = mcdi_rpc(CMD_GET_LINK, NULL, 0 , out, CMD_GET_LINK_OUT_LEN, NULL, port,d);
    assert(err_is_ok(err));

    memcpy(&cap[port], out, 4);
    memcpy(&speed[port], out+CMD_GET_LINK_OUT_SPEED_OFFSET, 4);
    memcpy(&fcntl[port], out+CMD_GET_LINK_OUT_FCNTL_OFFSET, 4);
    memcpy(&flags[port], out+CMD_GET_LINK_OUT_FLAGS_OFFSET, 4);
   
    decode_link(fcntl[port], flags[port], speed[port]);

}


/* Init port */
static void init_port(uint8_t port)
{
    uint8_t in[CMD_SET_MAC_IN_LEN];
    uint32_t reg;
    errval_t err;

    memcpy(in + CMD_SET_MAC_IN_ADR_OFFSET, &d_mac[port], 6 );
    /* linux driver sets these bits */
    in[14] = 0xFF;
    in[15] = 0xFF;
    /* set MTU */
    reg = MTU_MAX;
    memcpy(in + CMD_SET_MAC_IN_MTU_OFFSET , &reg, 4);
    
    in[CMD_SET_MAC_IN_DRAIN_OFFSET] = 0;
    /* Reject unicast packets?   */
    in[CMD_SET_MAC_IN_REJECT_OFFSET] = 1;
    /* Set wanted flow control of the card 2 -> bidirectional*/
    in[CMD_SET_MAC_IN_FCTNL_OFFSET] = 2;
    err = mcdi_rpc(CMD_SET_MAC, in, CMD_SET_MAC_IN_LEN, NULL, 0, NULL, port, d);
    assert(err_is_ok(err));

    memset(mc_hash, 0, sizeof(mc_hash));
    err = mcdi_rpc(CMD_SET_MCAST_HASH, mc_hash , CMD_SET_MCAST_HASH_IN_LEN,
                   NULL, 0 , NULL, port, d);
    assert(err_is_ok(err));

    memset(in, 0 , sizeof(in));
    memcpy(in + CMD_SET_LINK_IN_CAP_OFFSET, &cap[pci_function], 4);
    
    err = mcdi_rpc(CMD_SET_LINK, in, CMD_SET_LINK_IN_LEN, NULL, 0, NULL, 0, d);
    assert(err_is_ok(err));
}
/*  start port        */
static void start_port(uint8_t port)
{
    uint8_t in[CMD_SET_MAC_IN_LEN];
    uint64_t reg;
    errval_t err;

    memset(&in, 0, sizeof(in));

    err = mcdi_rpc(CMD_SET_MCAST_HASH, mc_hash , CMD_SET_MCAST_HASH_IN_LEN,
                   NULL, 0 , NULL, port, d);
    assert(err_is_ok(err));

    /* mac address */
    memcpy(in + CMD_SET_MAC_IN_ADR_OFFSET, &d_mac[port], 6 );
    /* seems like the linux driver sets all bits not set
       from the MAC address to 1*/
    in[14] = 0xFF;
    in[15] = 0xFF;
    /* set MTU*/
    reg = MTU_MAX;
    memcpy(in + CMD_SET_MAC_IN_MTU_OFFSET , &reg, 4);
    in[CMD_SET_MAC_IN_DRAIN_OFFSET] = 0;
    /* Reject unicast packets ?  */
    in[CMD_SET_MAC_IN_REJECT_OFFSET] = 1;
    /* Set wanted functionality (flow control) of card -> set to 2 for RX/TX
       And on*/
    in[CMD_SET_MAC_IN_FCTNL_OFFSET] = 2;
    err = mcdi_rpc(CMD_SET_MAC, in, CMD_SET_MAC_IN_LEN, NULL, 0, NULL, port, d);
    assert(err_is_ok(err));

    err = mcdi_rpc(CMD_SET_MCAST_HASH, mc_hash , CMD_SET_MCAST_HASH_IN_LEN,
                   NULL, 0 , NULL, port, d);

    assert(err_is_ok(err));
}

/******************************************************************************
 * Device init
 *****************************************************************************/

static void probe_all(void)
{
    uint32_t offset = 0;
    uint32_t outlen = 0;
    
    uint64_t reg = 0;
    
    uint8_t in[16];
    uint8_t out[252];
     
    struct frame_identity frameid = { .base = 0, .bytes = 0 };
    errval_t r;

    // init MCDI
    init_mcdi_mutex();
    // Test and clear MC-reboot flag for port/function
    offset = MCDI_REBOOT_OFFSET(pci_function);
    reg =  sfn5122f_mc_treg_smem_rd(d,offset);
    if (reg != 0) {
        sfn5122f_mc_treg_smem_wr(d,offset,0);
    }

    /*print out any assertions */
    handle_assertions();
    // Let BMC know that driver is in charg of filter/link setttings
    // before we can restet NIC
    memset(&in, 0, sizeof(in));
    memset(&out, 0 , sizeof(out));
    
    r = mcdi_rpc(CMD_GET_VERSION, NULL, 0, out, CMD_GET_VERSION_OUT_LEN,
                 &outlen, pci_function, d);
    assert(err_is_ok(r));


    memset(&out, 0 , sizeof(out));

    // driver is operating / + update
    in[0] = 0x1;
    in[4] = 0x1;
    r = mcdi_rpc(CMD_DRV_ATTACH, in, CMD_DRV_ATTACH_IN_LEN, out,
                 CMD_DRV_ATTACH_OUT_LEN, &outlen, pci_function, d);
    assert(err_is_ok(r));

    /* reset card */
    r = mcdi_rpc(CMD_PORT_RESET, NULL, 0, NULL, 0, NULL, pci_function, d);
    assert(err_is_ok(r));

    // init WoL Filter
    if(mcdi_rpc(CMD_WOL_FILTER_GET, NULL, 0, out, CMD_WOL_FILTER_GET_OUT_LEN,
       &outlen, pci_function, d) == SYS_ERR_OK) {
        memcpy(&wol_filter_id, out , 4);
    } else {
      // Reset filter of card
      mcdi_rpc(CMD_WOL_FILTER_RESET, NULL, 0, NULL, 0, NULL, pci_function, d);
    }
 
    //  memory for INT_KER
    int_ker_virt = alloc_map_frame(VREGION_FLAGS_READ_WRITE,
                                   2*sizeof(uint64_t), &int_ker);
    memset(int_ker_virt, 0, 2*sizeof(uint64_t));
    // Read in non volatile configuration
    memset(&out, 0, sizeof(out));
    r = mcdi_rpc(CMD_GET_BOARD_CONFIG, NULL, 0, out,
                 CMD_GET_BOARD_CONFIG_OUT_LEN, &outlen, pci_function, d);
    assert(err_is_ok(r));

    memcpy(&d_mac[0], out+MCDI_MAC_PORT_OFFSET(0) ,6);
    memcpy(&d_mac[1], out+MCDI_MAC_PORT_OFFSET(1) ,6);
    
    // read phy configuration
    r = mcdi_rpc(CMD_GET_PHY_CFG, NULL, 0, out, CMD_GET_PHY_CFG_OUT_LEN, &outlen,
                 pci_function, d);
    assert(err_is_ok(r));

    memcpy(&phy_caps[pci_function], out+CMD_GET_PHY_CFG_OUT_CAP_OFFSET, 4);
    memcpy(&phy_flags[pci_function], out+CMD_GET_PHY_CFG_OUT_FLAGS_OFFSET, 4);
    memcpy(&phy_media[pci_function], out+CMD_GET_PHY_CFG_OUT_MEDIA_OFFSET, 4);

    // get loopback modes
    r = mcdi_rpc(CMD_GET_LOOPBACK_MODES, NULL, 0, out,
                 CMD_GET_LOOPBACK_MODES_OUT_LEN, &outlen, pci_function, d);
    assert(err_is_ok(r));
    memcpy(&phy_loopback_mode, out+CMD_GET_LOOPBACK_MODES_SUGGESTED_OFFSET,4);
    // loopback mode NONE is no valid condition
    phy_loopback_mode &= ~(1);
   

    // MAC STATS INIT
    mac_virt = alloc_map_frame(VREGION_FLAGS_READ_WRITE,
                               NUM_MAC_STATS*sizeof(uint64_t),
                               &mac_stats);

    assert(mac_virt != NULL);
    r = invoke_frame_identify(mac_stats, &frameid);
    assert(err_is_ok(r));
    mac_phys = frameid.base;
    memset(mac_virt, 0, NUM_MAC_STATS*sizeof(uint64_t));


    memset(&in, 0, sizeof(in));
    memcpy(in, &mac_phys, 8);

     // Settings for DMA of MAC stats
    in[CMD_MAC_STATS_IN_CMD_OFFSET] = 0x6;
    in[CMD_MAC_STATS_IN_DMA_LEN_OFFSET] = 8;
    in[CMD_MAC_STATS_IN_DMA_LEN_OFFSET+1] = 3;
    r = mcdi_rpc(CMD_MAC_STATS, in, CMD_MAC_STATS_IN_LEN, NULL, 0, NULL,
                pci_function, d);
    assert(err_is_ok(r));

}



// Init card IP filters
static void init_rx_filter_config(void)
{
    uint64_t reg_hi, reg_lo;

    for (int i = 0; i < NUM_FILTERS_IP; i++) {
        sfn5122f_rx_filter_tbl_lo_wr(d, i, 0);
        sfn5122f_rx_filter_tbl_hi_wr(d, i, 0);
    }

    reg_lo = sfn5122f_rx_filter_ctl_reg_lo_rd(d);
    reg_hi = sfn5122f_rx_filter_ctl_reg_hi_rd(d);

    reg_hi = sfn5122f_rx_filter_ctl_reg_hi_ethernet_full_search_limit_insert(reg_hi, 1);
    reg_hi = sfn5122f_rx_filter_ctl_reg_hi_ethernet_wildcard_search_limit_insert(reg_hi, 3);

    reg_lo = sfn5122f_rx_filter_ctl_reg_lo_multicast_nomatch_q_id_lo_insert(reg_lo, 0);
    reg_lo = sfn5122f_rx_filter_ctl_reg_lo_unicast_nomatch_q_id_insert(reg_lo, 0);
    reg_lo = sfn5122f_rx_filter_ctl_reg_lo_unicast_nomatch_rss_enabled_insert(reg_lo, 0);
    reg_lo = sfn5122f_rx_filter_ctl_reg_lo_multicast_nomatch_rss_enabled_insert(reg_lo, 0);

    reg_lo = sfn5122f_rx_filter_ctl_reg_lo_udp_full_srch_limit_insert(reg_lo, 1);
    reg_lo = sfn5122f_rx_filter_ctl_reg_lo_udp_wild_srch_limit_insert(reg_lo, 3);
    reg_lo = sfn5122f_rx_filter_ctl_reg_lo_tcp_full_srch_limit_insert(reg_lo, 1);
    reg_lo = sfn5122f_rx_filter_ctl_reg_lo_tcp_wild_srch_limit_insert(reg_lo, 3);


    sfn5122f_rx_filter_ctl_reg_lo_wr(d,reg_lo);
    sfn5122f_rx_filter_ctl_reg_hi_wr(d,reg_hi);

}

static void device_init(void)
{
    errval_t r;
    struct frame_identity frameid = { .base = 0, .bytes = 0 };
    uint64_t reg, reg2; // tmp_key = 0;
    uint8_t in[24]; // set length to biggest in length needed

    memset(&in, 0, sizeof(in));

    // recover from failed assertion post-reset
    handle_assertions();

    /* ignore TX of packets 16 bytes and less */
    reg = sfn5122f_tx_reserved_reg_lo_rd(d);
    reg = sfn5122f_tx_reserved_reg_lo_tx_flush_min_len_en_insert(reg, 1);
    sfn5122f_tx_reserved_reg_lo_wr(d, reg);
    sfn5122f_tx_reserved_reg_hi_wr(d, sfn5122f_tx_reserved_reg_hi_rd(d));
    //Disable TX_NO_EOP_DISC_EN because else would limit packets to 16
    reg = sfn5122f_tx_cfg_reg_lo_rd(d);
    reg = sfn5122f_tx_cfg_reg_lo_tx_no_eop_disc_en_insert(reg, 0);
    reg = sfn5122f_tx_cfg_reg_lo_tx_ownerr_ctl_insert(reg, 1);
    reg = sfn5122f_tx_cfg_reg_lo_tx_filter_en_bit_insert(reg, 1);
    sfn5122f_tx_cfg_reg_lo_wr(d, reg);
    sfn5122f_tx_cfg_reg_hi_wr(d, sfn5122f_tx_cfg_reg_hi_rd(d));

    reg = sfn5122f_rx_cfg_reg_lo_rd(d);
    // unset bit and set other bit which are not in documentation (43 and 47)
    reg = sfn5122f_rx_cfg_reg_lo_rx_desc_push_en_insert(reg, 0) ;
    reg = sfn5122f_rx_cfg_reg_lo_rx_ingr_en_insert(reg, 1);
    //reg = sfn5122f_rx_cfg_reg_lo_rx_usr_buf_size_insert(reg, (MTU_MAX-256) >> 5);
    reg = sfn5122f_rx_cfg_reg_lo_rx_usr_buf_size_insert(reg, 4096 >> 5);
    //reg = sfn5122f_rx_cfg_reg_lo_rx_ownerr_ctl_insert(reg, 1);
    reg = sfn5122f_rx_cfg_reg_lo_rx_ip_hash_insert(reg, 1);
    //reg = sfn5122f_rx_cfg_reg_lo_rx_hash_insrt_hdr_insert(reg, 1);
    reg = sfn5122f_rx_cfg_reg_lo_rx_hash_alg_insert(reg, 1);
    sfn5122f_rx_cfg_reg_lo_wr(d, reg);
    sfn5122f_rx_cfg_reg_hi_wr(d, sfn5122f_rx_cfg_reg_hi_rd(d));
    /* enable event logging, no UART
      Event destination is queue 0 */
    in[0] = 0x2;
    r = mcdi_rpc(CMD_LOG_CTRL, in, CMD_LOG_CTRL_IN_LEN,
                 NULL, 0, NULL, pci_function, d);
    assert(err_is_ok(r));

    /* Set destination of TX/RX flush event */
    
    sfn5122f_dp_ctrl_reg_lo_fls_evq_id_wrf(d, 0);
    sfn5122f_dp_ctrl_reg_hi_wr(d, sfn5122f_dp_ctrl_reg_hi_rd(d));
  
    /* Disalbe user events for now     */
    sfn5122f_usr_ev_cfg_lo_usrev_dis_wrf(d , 1);
    sfn5122f_usr_ev_cfg_hi_wr(d, sfn5122f_usr_ev_cfg_hi_rd(d));


    // This seems to be not device specific i.e. works for other
    // Solarflare cards
    /* Set position of descriptor caches in SRAM */
    sfn5122f_srm_tx_dc_cfg_reg_lo_wr(d, TX_DC_BASE);
    sfn5122f_srm_tx_dc_cfg_reg_hi_wr(d, sfn5122f_srm_tx_dc_cfg_reg_hi_rd(d));
    sfn5122f_srm_rx_dc_cfg_reg_lo_srm_rx_dc_base_adr_wrf(d, RX_DC_BASE);
    sfn5122f_srm_rx_dc_cfg_reg_hi_wr(d, sfn5122f_srm_rx_dc_cfg_reg_hi_rd(d));

    /* Set TX descriptor cache size to 16 */
    sfn5122f_tx_dc_cfg_reg_lo_tx_dc_size_wrf(d, 1);
    sfn5122f_tx_dc_cfg_reg_hi_wr(d, sfn5122f_tx_dc_cfg_reg_hi_rd(d));

    /* Set RX descriptor cache size to 64 and low watermark */
    sfn5122f_rx_dc_cfg_reg_lo_rx_dc_size_wrf(d, 3);
    sfn5122f_rx_dc_cfg_reg_hi_wr(d, sfn5122f_rx_dc_cfg_reg_hi_rd(d));

    reg = 0;
    reg = sfn5122f_rx_dc_pf_wm_reg_lo_rx_dc_pf_lwm_insert(reg, RX_DESC_CACHE_SIZE -8);
    sfn5122f_rx_dc_pf_wm_reg_lo_wr(d, reg);
    sfn5122f_rx_dc_pf_wm_reg_hi_wr(d, sfn5122f_rx_dc_pf_wm_reg_hi_rd(d));
   
   /*programm init ker address for interrupts */
    r = invoke_frame_identify(int_ker, &frameid);
    assert(err_is_ok(r));

    sfn5122f_int_adr_reg_ker_lo_wr(d, frameid.base);
    reg = sfn5122f_int_adr_reg_ker_hi_rd(d);

    // disable vector write if we use MSI-X
    if (use_msix) {
        reg = sfn5122f_int_adr_reg_ker_hi_norm_int_vec_dis_ker_insert(reg, 1);
        if (cdriver_msix == -1) {
            r = pci_setup_inthandler(global_interrupt_handler, NULL, &cdriver_vector);
            assert(err_is_ok(r));
            setup_interrupt(&cdriver_msix, disp_get_core_id(), cdriver_vector);
        }
    } else {
        reg = sfn5122f_int_adr_reg_ker_hi_norm_int_vec_dis_ker_insert(reg, 0);
    }
    sfn5122f_int_adr_reg_ker_hi_wr(d, reg);
   
    /* Enable all the genuinley fatal interrupts */
    reg = sfn5122f_fatal_intr_reg_ker_lo_ill_adr_int_ker_en_insert(reg, 1);
    /* Enable rxbuf/txbuf interrupt  fields not documented.
       Set bits 39 and 38*/
    reg = sfn5122f_fatal_intr_reg_ker_lo_rxbuf_own_int_ker_en_insert(reg, 1);
    reg = sfn5122f_fatal_intr_reg_ker_lo_txbuf_own_int_ker_en_insert(reg, 1);
    
    //reg = sfn5122f_fatal_intr_reg_ker_lo_sram_perr_int_p_ker_en_insert(reg, 1);
    sfn5122f_fatal_intr_reg_ker_lo_wr(d, ~reg);
    sfn5122f_fatal_intr_reg_ker_hi_wr(d, 0XFFFFFFFFFFFFFFFF);

    /* Setup RSS indirection table (maps from hash value to packet to RXQ) */
    for (int i = 0; i < 128; i++) {
        rx_indir_tbl[i] = 0;
        sfn5122f_rx_indirection_tbl_wr( d, i, rx_indir_tbl[i]);
    }

    /* Disable the ugly timer-based TX DMA backoff and allow TX DMA to be
     * controlled by the RX FIFO fill level. Set arbitration to one pkt/Q.
      (from linux driver) */
    reg = sfn5122f_tx_reserved_reg_lo_rd(d);
    reg = sfn5122f_tx_reserved_reg_lo_tx_rx_spacer_en_insert(reg, 1);
    reg = sfn5122f_tx_reserved_reg_lo_tx_one_pkt_per_q_insert(reg, 0);
    reg = sfn5122f_tx_reserved_reg_lo_tx_dis_non_ip_ev_insert(reg, 1);

    /* Enable software events */
    reg = sfn5122f_tx_reserved_reg_lo_tx_soft_evt_en_insert(reg, 1);
    /* Prefetch threshold 2 => fetch when descriptor cache half empty */
    reg = sfn5122f_tx_reserved_reg_lo_tx_pref_threshold_insert(reg, 2);
    /* Disable hardware watchdog which can misfire */
    reg = sfn5122f_tx_reserved_reg_lo_tx_pref_wd_tmr_insert(reg, 0x3fffff);
    /* Squash TX of packets of 16 bytes or less */
    reg = sfn5122f_tx_reserved_reg_lo_tx_flush_min_len_en_insert(reg, 1);
 
    reg2 = sfn5122f_tx_reserved_reg_hi_rd(d);
    reg2 = sfn5122f_tx_reserved_reg_hi_tx_push_en_insert(reg2, 0);
    reg2 = sfn5122f_tx_reserved_reg_hi_tx_push_chk_dis_insert(reg2, 0);
    //reg2 = sfn5122f_tx_reserved_reg_hi_tx_rx_spacer_insert(reg2, 0xfe);
    reg2 = sfn5122f_tx_reserved_reg_hi_tx_rx_spacer_insert(reg2, 0x1);
    sfn5122f_tx_reserved_reg_lo_wr(d, reg);
    sfn5122f_tx_reserved_reg_hi_wr(d, reg2);

    init_port(pci_function);
    get_link(pci_function);
    DEBUG("BASIC CARD INIT DONE  \n");
}

static void start_all(void)
{
    uint64_t reg;
 
    start_port(pci_function);

    memset(int_ker_virt, 0, 2*sizeof(uint64_t));
    /*  Enable interrupts   */
    /* Use an interrupt level unused by event queues */
    reg = sfn5122f_int_en_reg_ker_lo_rd(d);
    if (use_msix) {
        reg = sfn5122f_int_en_reg_ker_lo_ker_int_leve_sel_insert(reg, 0);
    } else {
        // legacy
        reg = sfn5122f_int_en_reg_ker_lo_ker_int_leve_sel_insert(reg, 0x1f);
    }
    reg = sfn5122f_int_en_reg_ker_lo_drv_int_en_ker_insert(reg, 1);

    /*   undocumented field   */
    reg = sfn5122f_int_en_reg_ker_lo_ker_int_ker_insert(reg, 0);
    sfn5122f_int_en_reg_ker_lo_wr(d, reg);
    sfn5122f_int_en_reg_ker_hi_wr(d, sfn5122f_int_en_reg_ker_hi_rd(d));

    /* Start MAC stats            */
    /*
    uint8_t in[CMD_MAC_STATS_IN_LEN];
    unsigned long long* stats = (unsigned long long *) mac_virt;
    uint8_t* pointer;

    memset(in, 0, sizeof(in));
    stats[0x60] = (unsigned long long) (-1);
    memcpy(in, &mac_phys, 8);
    pointer = (uint8_t *) &mac_phys;
    in[CMD_MAC_STATS_IN_CMD_OFFSET] = 0xD;
    in[10] = 0xE8;
    in[11] = 3;
    in[CMD_MAC_STATS_IN_DMA_LEN_OFFSET] = 8;
    in[CMD_MAC_STATS_IN_DMA_LEN_OFFSET+1] = 3;
    errval_t err = mcdi_rpc(CMD_MAC_STATS, in, CMD_MAC_STATS_IN_LEN,
                            NULL, 0, NULL, pci_function, d);
    assert(err_is_ok(err));
    */
}

/**************************************************************************
 Queue init and stop
***************************************************************************/


static void queue_hw_stop(uint16_t n)
{

    uint64_t reg = 0;
    /* flush TX queue */
    reg = sfn5122f_tx_flush_descq_reg_lo_rd(d);
    reg = sfn5122f_tx_flush_descq_reg_lo_tx_flush_descq_insert(reg, n);
    reg = sfn5122f_tx_flush_descq_reg_lo_tx_flush_descq_cmd_insert(reg, 1);
    sfn5122f_tx_flush_descq_reg_lo_wr(d, reg);
    sfn5122f_tx_flush_descq_reg_hi_wr(d, sfn5122f_tx_flush_descq_reg_hi_rd(d));
    /* flush RX queue */
    reg = sfn5122f_rx_flush_descq_reg_lo_rd(d);
    reg = sfn5122f_rx_flush_descq_reg_lo_rx_flush_descq_insert(reg, n);
    reg = sfn5122f_rx_flush_descq_reg_lo_rx_flush_descq_cmd_insert(reg, 1);
    sfn5122f_rx_flush_descq_reg_lo_wr(d, reg);
    sfn5122f_rx_flush_descq_reg_hi_wr(d, sfn5122f_rx_flush_descq_reg_hi_rd(d));

    /*   TODO Wait for DRIVER_EVENT    */
    /* clear pointer table entries */
    sfn5122f_tx_desc_ptr_tbl_lo_wr(d, n, 0);
    sfn5122f_tx_desc_ptr_tbl_hi_wr(d, n, 0);
    sfn5122f_rx_desc_ptr_tbl_lo_wr(d, n, 0);
    sfn5122f_rx_desc_ptr_tbl_hi_wr(d, n, 0);

    /*Free RX queue tbl entries*/
    reg = 0;
    reg = sfn5122f_buf_tbl_upd_reg_lo_buf_clr_cmd_insert(reg, 1);
    reg = sfn5122f_buf_tbl_upd_reg_lo_buf_clr_start_id_insert(reg,
                                              queues[n].rx_buf_tbl);

    if (queues[n].userspace) {
       reg = sfn5122f_buf_tbl_upd_reg_lo_buf_clr_end_id_insert(reg,
                                  queues[n].rx_buf_tbl + NUM_ENT_RX_USR);
    } else {
        reg = sfn5122f_buf_tbl_upd_reg_lo_buf_clr_end_id_insert(reg,
                                       queues[n].rx_buf_tbl + NUM_ENT_RX);
    }

    /*Free TX queue tbl entries*/
    reg = 0;
    reg = sfn5122f_buf_tbl_upd_reg_lo_buf_clr_cmd_insert(reg, 1);
    reg = sfn5122f_buf_tbl_upd_reg_lo_buf_clr_end_id_insert(reg,
                              queues[n].tx_buf_tbl + NUM_ENT_TX );
    reg = sfn5122f_buf_tbl_upd_reg_lo_buf_clr_start_id_insert(reg,
                                              queues[n].tx_buf_tbl);

    /*Free EV queue tbl entries*/
    reg = 0;
    reg = sfn5122f_buf_tbl_upd_reg_lo_buf_clr_cmd_insert(reg, 1);
    reg = sfn5122f_buf_tbl_upd_reg_lo_buf_clr_end_id_insert(reg,
                              queues[n].ev_buf_tbl + NUM_ENT_EVQ );
    reg = sfn5122f_buf_tbl_upd_reg_lo_buf_clr_start_id_insert(reg,
                                             queues[n].ev_buf_tbl);
}



static uint32_t init_evq(uint16_t n, lpaddr_t phys, bool interrupt)
{

    //errval_t r;
    //struct frame_identity frameid = { .base = 0, .bytes = 0 };
    uint64_t reg, buffer_offset;
    reg = 0;

    reg = sfn5122f_timer_tbl_lo_timer_q_en_insert(reg, 1);
    // set to 0 if interrupts for receives/sends should be generated
    if (use_msix) {
        reg = sfn5122f_timer_tbl_lo_host_notify_mode_insert(reg, 0);
    } else {
        reg = sfn5122f_timer_tbl_lo_int_pend_insert(reg, 0);
        reg = sfn5122f_timer_tbl_lo_int_armd_insert(reg, 0);
        if (use_interrupt && interrupt) {
            reg = sfn5122f_timer_tbl_lo_host_notify_mode_insert(reg, 0);
        } else {
            reg = sfn5122f_timer_tbl_lo_host_notify_mode_insert(reg, 1);
        }
    }
    // timer mode disabled
    reg = sfn5122f_timer_tbl_lo_timer_mode_insert(reg, 0);
    sfn5122f_timer_tbl_lo_wr(d, n, reg);
    sfn5122f_timer_tbl_hi_wr(d, n, sfn5122f_timer_tbl_hi_rd(d, n));

    /*
    r = invoke_frame_identify(queues[n].ev_frame, &frameid);
    assert(err_is_ok(r));
    ev_phys = frameid.base;
    */

    buffer_offset = alloc_buf_tbl_entries(phys, NUM_ENT_EVQ, 0, 0, d);
    if (buffer_offset == -1) {
        return -1;
    }

    DEBUG("EV_QUEUE_%d: buf_off %ld, phys 0x%lx\n",n , buffer_offset, phys);
    //  setup EV queue
    reg = sfn5122f_evq_ptr_tbl_lo_rd(d, n);
    reg = sfn5122f_evq_ptr_tbl_lo_evq_en_insert(reg, 1);
    reg = sfn5122f_evq_ptr_tbl_lo_evq_size_insert(reg, 6);
    reg = sfn5122f_evq_ptr_tbl_lo_evq_buf_base_id_insert(reg,
           buffer_offset);

    sfn5122f_evq_ptr_tbl_lo_wr(d, n, reg);
    sfn5122f_evq_ptr_tbl_hi_wr(d, n, sfn5122f_evq_ptr_tbl_hi_rd(d, n));

    /* No write collection for this register   */
    reg = sfn5122f_timer_command_reg_lo_rd(d,n);
    reg = sfn5122f_timer_command_reg_lo_tc_timer_val_insert(reg, 0);
    if (use_msix) {
        reg = sfn5122f_timer_command_reg_lo_tc_timer_mode_insert(reg, 0);
    } else {
        reg = sfn5122f_timer_command_reg_lo_tc_timer_mode_insert(reg, 0);
    }

    sfn5122f_timer_command_reg_lo_wr(d, n, reg);

    sfn5122f_evq_rptr_reg_wr(d, n, queues[n].ev_head);

    return buffer_offset;
}

static uint32_t init_rxq(uint16_t n, lpaddr_t phys, bool userspace)
{
    uint64_t reg_lo, reg_hi,  buffer_offset;
   /*
    * This will define a buffer in the buffer table, allowing
    * it to be used for event queues, descriptor rings etc.
    */
    /* Get physical addresses for rx/tx rings and event queue */

    /* RX   */
    if (userspace) {
        buffer_offset = alloc_buf_tbl_entries(phys, NUM_ENT_RX_USR, 0, false, d);
    } else {
        buffer_offset = alloc_buf_tbl_entries(phys, NUM_ENT_RX, 0, false, d);
    }

    if (buffer_offset == -1) {
       return -1;
    }

    DEBUG("RX_QUEUE_%d: buf_off %ld, phys %lx\n", n,
          buffer_offset, phys);
    /* setup RX queue */
    reg_lo = sfn5122f_rx_desc_ptr_tbl_lo_rd(d, n);
    reg_hi = sfn5122f_rx_desc_ptr_tbl_hi_rd(d, n);
    /*  Which buffer table entries are used (which is the first entry) */
    reg_lo = sfn5122f_rx_desc_ptr_tbl_lo_rx_descq_buf_base_id_insert(reg_lo, buffer_offset);
    /*  Which event queue is associated with this queue*/
    reg_lo = sfn5122f_rx_desc_ptr_tbl_lo_rx_descq_evq_id_insert(reg_lo, n);
    
    if (!userspace) {
        reg_lo = sfn5122f_rx_desc_ptr_tbl_lo_rx_descq_owner_id_insert(reg_lo, 0);
    } else {
        reg_lo = sfn5122f_rx_desc_ptr_tbl_lo_rx_descq_owner_id_insert(reg_lo, n+1);
    }

    reg_lo = sfn5122f_rx_desc_ptr_tbl_lo_rx_descq_label_insert(reg_lo, n);

    /*  1024 entries = 1   (512 = 0; 2048 = 2 ; 4096 = 3)   */
    reg_lo = sfn5122f_rx_desc_ptr_tbl_lo_rx_descq_size_insert(reg_lo, 3);

    if (!userspace) {
        reg_lo = sfn5122f_rx_desc_ptr_tbl_lo_rx_descq_type_insert(reg_lo, 0);
    } else {
        reg_lo = sfn5122f_rx_desc_ptr_tbl_lo_rx_descq_type_insert(reg_lo, 1);
    }
    /*   No scatter */
    reg_lo = sfn5122f_rx_desc_ptr_tbl_lo_rx_descq_jumbo_insert(reg_lo, 0);
    /*  Enable queue */
    reg_lo = sfn5122f_rx_desc_ptr_tbl_lo_rx_descq_en_insert(reg_lo, 1);
   
    /*   Hardware verifies data digest  */
    reg_hi = sfn5122f_rx_desc_ptr_tbl_hi_rx_iscsi_ddig_en_insert(reg_hi, 0);
    reg_hi = sfn5122f_rx_desc_ptr_tbl_hi_rx_iscsi_hdig_en_insert(reg_hi, 0);

    sfn5122f_rx_desc_ptr_tbl_lo_wr(d, n, reg_lo);
    sfn5122f_rx_desc_ptr_tbl_hi_wr(d, n, reg_hi);

    return buffer_offset;
}


static uint32_t init_txq(uint16_t n, uint64_t phys,
                         bool csum, bool userspace)
{

    uint64_t reg, reg1, buffer_offset;
  
    buffer_offset = alloc_buf_tbl_entries(phys, NUM_ENT_TX, 0, 0, d);
    
    if (buffer_offset == -1) {
       return -1;
    }

    DEBUG("TX_QUEUE_%d: buf_off %ld, phys %lx\n",n , buffer_offset, phys);
    /* setup TX queue */
    reg = sfn5122f_tx_desc_ptr_tbl_lo_rd(d, n);
    reg1 = sfn5122f_tx_desc_ptr_tbl_hi_rd(d, n);
    /*  Which buffer table entries are used (which is the first entry) */
    reg = sfn5122f_tx_desc_ptr_tbl_lo_tx_descq_buf_base_id_insert(reg,
                        buffer_offset);
    /*  Which event queue is associated with this queue */
    reg = sfn5122f_tx_desc_ptr_tbl_lo_tx_descq_evq_id_insert(reg , n);
    if (!userspace) {
        reg = sfn5122f_tx_desc_ptr_tbl_lo_tx_descq_owner_id_insert(reg, 0);
    } else {
        reg = sfn5122f_tx_desc_ptr_tbl_lo_tx_descq_owner_id_insert(reg, n+1);
    }
    reg = sfn5122f_tx_desc_ptr_tbl_lo_tx_descq_label_insert(reg , n);
    /*  1024 entries = 1   (512 = 0; 2048 = 2 ; 4096 = 3)   */
    reg = sfn5122f_tx_desc_ptr_tbl_lo_tx_descq_size_insert(reg , 3);

    /*  No user lvl networking   */
    if (!userspace) {
        reg = sfn5122f_tx_desc_ptr_tbl_lo_tx_descq_type_insert(reg, 0);
    } else {
        reg = sfn5122f_tx_desc_ptr_tbl_lo_tx_descq_type_insert(reg, 1);
    }

    reg1 = sfn5122f_tx_desc_ptr_tbl_hi_tx_iscsi_ddig_en_insert(reg1, 0);
    reg1 = sfn5122f_tx_desc_ptr_tbl_hi_tx_iscsi_hdig_en_insert(reg1, 0);
   
    reg1 = sfn5122f_tx_desc_ptr_tbl_hi_tx_non_ip_drop_dis_insert(reg1, 1);
 
    /*   Enable queue  */
    reg1 = sfn5122f_tx_desc_ptr_tbl_hi_tx_descq_en_insert(reg1 , 1);

    /* Enable offload of checksum */
    reg1 = sfn5122f_tx_desc_ptr_tbl_hi_tx_ip_chksm_dis_insert(reg1, !csum);
    reg1 = sfn5122f_tx_desc_ptr_tbl_hi_tx_tcp_chksm_dis_insert(reg1, !csum);
    sfn5122f_tx_desc_ptr_tbl_lo_wr(d, n, reg);
    sfn5122f_tx_desc_ptr_tbl_hi_wr(d, n, reg1);
 
    return buffer_offset;
}


static void setup_interrupt(size_t *msix_index, uint8_t core, uint8_t vector)
{
    bool res;
    errval_t err;
    uint8_t dest;

    res = bmallocator_alloc(&msix_alloc, msix_index);
    assert(res);

    err = get_apicid_from_core(core, &dest);
    assert(err_is_ok(err));

    err = pci_msix_vector_init(*msix_index, dest, vector);
    assert(err_is_ok(err));

    DEBUG("MSI-X vector setup index=%"PRIx64", core=%d apic=%d swvec=%x\n",
            *msix_index, core, dest, vector);
}

static void resend_interrupt(void* arg)
{
    errval_t err;
    uint64_t i = (uint64_t) arg;
    err = queues[i].devif->tx_vtbl.interrupt(queues[i].devif, NOP_CONT, i);
    // If the queue is busy, there is already an oustanding message
    if (err_is_fail(err) && err != FLOUNDER_ERR_TX_BUSY) {
        USER_PANIC("Error when sending interrupt %s \n", err_getstring(err));
    }
}

/** Here are the global interrupts handled. */
static void global_interrupt_handler(void* arg)
{
    //uint64_t reg;
    errval_t err;
    uint32_t q_to_check;
    errval_t syserr;
    uint8_t* net_ivec_fatal = (uint8_t *) int_ker_virt;

    // bit 64 is indicator for a fatal event
    syserr = (net_ivec_fatal[8] & 0x1);
    if (syserr) {
        // TODO handle fatal interrupt
        USER_PANIC("FATAL INTERRUPT");
    } else {

    }

    q_to_check = sfn5122f_int_isr0_reg_lo_rd(d);

    for (uint64_t i = 0; i < 32; i++) {
        if ((q_to_check >> i) & 0x1) {
            if (queues[i].use_irq && queues[i].devif != NULL) {
                DEBUG("Interrupt to queue %lu \n", i);
                err = queues[i].devif->tx_vtbl.interrupt(queues[i].devif, NOP_CONT, i);
                if (err_is_fail(err)) {
                    err = queues[i].devif->register_send(queues[i].devif,
                                                         get_default_waitset(),
                                                         MKCONT(resend_interrupt, (void*)i));
                }
            }
        }
    }

    // Don't need to start event queues because we're already polling

}
/******************************************************************************/
/* Management interface implemetation */

static errval_t cd_create_queue_rpc(struct sfn5122f_devif_binding *b, struct capref frame,
                    bool user, bool interrupt, bool qzero,
                    uint8_t core, uint8_t msix_vector,
                    uint64_t *mac, uint16_t *qid, struct capref *regs,
                    errval_t *ret_err)
{

    DEBUG("cd_create_queue \n");

    errval_t err;
    struct frame_identity id;

    int n = -1;
    for (int i = 1; i < NUM_QUEUES; i++) {
        if (queues[i].enabled == false) {
            n = i;
            break;
        }
    }

    if (n == -1) {
        *ret_err = NIC_ERR_ALLOC_QUEUE;
        *regs = NULL_CAP;
        return NIC_ERR_ALLOC_QUEUE;
    }
    
    if (qzero) {
        if (queues[0].enabled == false) {
            n = 0;
        } else {
            printf("Default queue already initalized \n");
            return NIC_ERR_ALLOC_QUEUE;
        }
    }

    b->st = &queues[n];

    queues[n].use_irq = interrupt;
    queues[n].enabled = false;
    queues[n].tx_frame = frame;
    queues[n].tx_head = 0;
    queues[n].rx_head = 0;
    queues[n].ev_head = 0;
    queues[n].rxbufsz = MTU_MAX;
    queues[n].devif = b;
    queues[n].userspace = user;
    queues[n].msix_index = -1;
    queues[n].msix_intdest = core;
    queues[n].msix_intvec = msix_vector;
    queues[n].qid = n;

    if (queues[n].use_irq && use_msix) {
        if (queues[n].msix_intvec != 0) {
            if (queues[n].msix_index == -1) {
                setup_interrupt(&queues[n].msix_index, queues[n].msix_intdest,
                        queues[n].msix_intvec);
            }
        }
    }

    err = invoke_frame_identify(frame, &id);
    assert(err_is_ok(err));
    // enable checksums
    queues[n].tx_buf_tbl = init_txq(n, id.base, csum_offload, user);
    queues[n].rx_buf_tbl = init_rxq(n, id.base+ sizeof(uint64_t)*TX_ENTRIES, user);

    queues[n].ev_buf_tbl = init_evq(n, id.base+sizeof(uint64_t)*(TX_ENTRIES+RX_ENTRIES),
            interrupt);
    if(queues[n].ev_buf_tbl == -1 ||
            queues[n].tx_buf_tbl == -1 ||
            queues[n].rx_buf_tbl == -1){
        *ret_err = NIC_ERR_ALLOC_QUEUE;
        *regs = NULL_CAP;
        return NIC_ERR_ALLOC_QUEUE;
    }

    queues[n].enabled = true;
    DEBUG("created queue %d \n", n);

    *mac = d_mac[pci_function];
    *qid = n;

    err = slot_alloc(regs);
    assert(err_is_ok(err));
    err = cap_copy(*regs, *regframe);
    assert(err_is_ok(err));

    *ret_err = SYS_ERR_OK;

    return SYS_ERR_OK;

}


static void cd_create_queue(struct sfn5122f_devif_binding *b, struct capref frame,
                            bool user, bool interrupt, bool qzero, uint8_t core,
                            uint8_t msix_vector)
{

    uint64_t mac;
    uint16_t queueid;
    errval_t err;

    struct capref regs;


    cd_create_queue_rpc(b, frame, user, interrupt, qzero, core,
                        msix_vector, &mac, &queueid, &regs, &err);

    err = b->tx_vtbl.create_queue_response(b, NOP_CONT, mac, queueid, regs, err);
    assert(err_is_ok(err));
    DEBUG("cd_create_queue end\n");
}


static errval_t cd_register_region_rpc(struct sfn5122f_devif_binding *b, uint16_t qid,
                                struct capref region, uint64_t *buftbl_id, errval_t *ret_err)
{
    errval_t err;

    struct frame_identity id;
    uint64_t buffer_offset = 0;

    err = invoke_frame_identify(region, &id);
    if (err_is_fail(err)) {
        err = b->tx_vtbl.register_region_response(b, NOP_CONT, 0, NIC_ERR_REGISTER_REGION);
        assert(err_is_ok(err));
    }

    size_t size = id.bytes;
    lpaddr_t addr = id.base;

    // TODO unsigned/signed
    buffer_offset = alloc_buf_tbl_entries(addr, size/BUF_SIZE, qid, true, d);
    if (buffer_offset == -1) {
        *buftbl_id = 0;
        return -1;
    }

    *buftbl_id = buffer_offset;
    *ret_err = SYS_ERR_OK;
    return SYS_ERR_OK;
}


static void cd_register_region(struct sfn5122f_devif_binding *b, uint16_t qid,
                               struct capref region)
{
    errval_t err, msgerr;
    uint64_t id;
    err = cd_register_region_rpc(b, qid, region, &id, &msgerr);

    err = b->tx_vtbl.register_region_response(b, NOP_CONT, id, msgerr);
}


static void cd_deregister_region(struct sfn5122f_devif_binding *b, uint64_t buftbl_id,
                                 uint64_t size)
{
    errval_t err;
    free_buf_tbl_entries(buftbl_id, size/BUF_SIZE, d);
   
    err = b->tx_vtbl.deregister_region_response(b, NOP_CONT, SYS_ERR_OK);
    assert(err_is_ok(err));
}

static void cd_destroy_queue(struct sfn5122f_devif_binding *b, uint16_t qid)
{
    errval_t err;
    queue_hw_stop(qid);

    queues[qid].enabled = false;
    queues[qid].devif = NULL;

    err = b->tx_vtbl.destroy_queue_response(b, NOP_CONT, SYS_ERR_OK);
    assert(err_is_ok(err));
}

static void cd_control(struct sfn5122f_devif_binding *b, uint64_t request,
                       uint64_t arg)
{
    errval_t err;

    struct queue_state *q = b->st;
    assert(q);

    DEBUG("control arg=0x%lx\n", arg);

    struct sfn5122f_filter_ip f = {
            .dst_port = ((uint32_t)arg >> 16),
            .dst_ip = htonl((uint32_t)(arg >> 32)),
            .src_ip = 0,
            .src_port = 0,
            .type_ip = arg & 0x1,
            .queue = q->qid,
    };

    uint64_t fid;
    err = reg_port_filter(&f, &fid);

    DEBUG("register filter: 0x%x:%u UDP=%u -> q=%u @ index=%lu %s\n",f.dst_ip,
          f.dst_port, f.type_ip, f.queue, fid, err_getstring(err));


    err = b->tx_vtbl.control_response(b, NOP_CONT, fid, err);
    assert(err_is_ok(err));
}

static void export_devif_cb(void *st, errval_t err, iref_t iref)
{
    const char *suffix = "_sfn5122fmng_devif";
    char name[strlen(service_name) + strlen(suffix) + 1];

    assert(err_is_ok(err));

    // Build label for interal management service
    sprintf(name, "%s%s", service_name, suffix);

    err = nameservice_register(name, iref);
    assert(err_is_ok(err));
    DEBUG("Devif Management interface exported\n");
    initialized = true;
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
    struct sfn5122f_filter_ip f = {
            .dst_port = dst_port,
            .src_port = src_port,
            .dst_ip = dst_ip,
            .src_ip = src_ip,
            .type_ip = type,
            .queue = qid,
    };

    if (type == net_filter_PORT_UDP) {
        f.src_ip = 0;
    }

    errval_t err = reg_port_filter(&f, fid);
    assert(err_is_ok(err));
    DEBUG("filter registered: err=%"PRIu64", fid=%"PRIu64"\n", err, *fid);
    return SYS_ERR_OK;
}


static errval_t cb_remove_filter(struct net_filter_binding *b,
                                 net_filter_filter_type_t type,
                                 uint64_t filter_id,
                                 errval_t* err)
{
    if ((type == net_filter_PORT_UDP || type == net_filter_PORT_TCP)
        && filters_rx_ip[filter_id].enabled == true) {
        filters_rx_ip[filter_id].enabled = false;

        sfn5122f_rx_filter_tbl_lo_wr(d, filter_id, 0);
        sfn5122f_rx_filter_tbl_hi_wr(d, filter_id, 0);
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
    err = nameservice_register("net_filter_sfn5122f", iref);
    assert(err_is_ok(err));
    DEBUG("Net filter interface exported\n");
}


static errval_t net_filter_connect_cb(void *st, struct net_filter_binding *b)
{
    printf("New connection on net filter interface\n");
    b->rpc_rx_vtbl = net_filter_rpc_rx_vtbl;
    return SYS_ERR_OK;
}


static errval_t connect_devif_cb(void *st, struct sfn5122f_devif_binding *b)
{
    DEBUG("New connection on devif management interface\n");

    //b->rx_vtbl = rx_vtbl_devif;

    b->rx_vtbl.create_queue_call = cd_create_queue;
    b->rx_vtbl.destroy_queue_call = cd_destroy_queue;
    b->rx_vtbl.register_region_call = cd_register_region;
    b->rx_vtbl.deregister_region_call = cd_deregister_region;
    b->rx_vtbl.control_call = cd_control;


    b->rpc_rx_vtbl.create_queue_call = cd_create_queue_rpc;
    b->rpc_rx_vtbl.register_region_call = cd_register_region_rpc;

    return SYS_ERR_OK;
}

/**
 * Initialize management interface for queue drivers.
 * This has to be done _after_ the hardware is initialized.
 */
static void initialize_mngif(void)
{
    errval_t r;

    r = sfn5122f_devif_export(NULL, export_devif_cb, connect_devif_cb,
                              get_default_waitset(), 1);
    assert(err_is_ok(r));

    r = net_filter_export(NULL, net_filter_export_cb, net_filter_connect_cb,
                          get_default_waitset(), 1);
    assert(err_is_ok(r));
}

/******************************************************************************/
/* Initialization code for driver */

/** Callback from pci to initialize a specific PCI device. */
static void pci_init_card(void *arg, struct device_mem* bar_info, int bar_count)
{
    errval_t err;
    bool res;

    d = malloc(sizeof(*d));
    /* Map first BAR for register access */
    assert(bar_count >= 1);
    DEBUG("BAR count %d \n", bar_count);
    map_device(&bar_info[0]);
    regframe = bar_info[0].frame_cap;
    DEBUG("BAR[0] mapped (v=%llx p=%llx l=%llx)\n",
            (unsigned long long) bar_info[0].vaddr,
            (unsigned long long) bar_info[0].paddr,
            (unsigned long long) bar_info[0].bytes);

    /* Initialize Mackerel binding */
    sfn5122f_initialize(d, (void*) bar_info[0].vaddr);
    d_virt = bar_info[0].vaddr;

    // Initialize manager for MSI-X vectors
    if (use_msix) {
        //d_msix = malloc(sizeof(*d_msix));
        //map_device(&bar_info[1]);
        //sfn5122f_msix_initialize(d_msix, (void*) bar_info[1].vaddr);
        DEBUG("Enabling MSI-X interrupts\n");
        uint16_t msix_count = 0;
        err = pci_msix_enable(&msix_count);
        assert(err_is_ok(err));
        assert(msix_count > 0);
        DEBUG("MSI-X #vecs=%d\n", msix_count);

        res = bmallocator_init(&msix_alloc, msix_count);
        assert(res);
    } else {
        DEBUG("Using legacy interrupts\n");
    }

    /* Get all information needed  */
    probe_all();
    /* Initialize hardware registers etc. */
    /* Start interrups / mac_stats etc.  */
    device_init();
    /* Init rx filters */
    init_rx_filter_config();
    /* initalize managemnt interface   */
    initialize_mngif();

    if (first){
       start_all();
       first = 0;
    }
}

static void parse_cmdline(int argc, char **argv)
{
    /*
     * XXX: the following contains a hack only to start the driver when
     *      the supplied bus/dev/funct matches the Kaluga start arguments.
     */
    int i;
    uint32_t tmp;
    for (i = 1; i < argc; i++) {
        if (strncmp(argv[i], "cardname=", strlen("cardname=") - 1) == 0) {
            service_name = argv[i] + strlen("cardname=");
        } else if (strncmp(argv[i], "bus=", strlen("bus=") - 1) == 0) {
            tmp = atol(argv[i] + strlen("bus="));
            if (pci_bus == PCI_DONT_CARE) {
                pci_bus = tmp;
            }

            if (pci_bus != tmp) {
                printf("DRIVER STARTED FOR BUS: 0x%x/0x%x\n", pci_bus, tmp);
                exit(1);
            }
            pci_bus = atol(argv[i] + strlen("bus="));
        } else if (strncmp(argv[i], "device=", strlen("device=") - 1) == 0) {
            tmp = atol(argv[i] + strlen("device="));
            if (pci_device == PCI_DONT_CARE) {
                pci_device = tmp;
            }

            if (pci_device != tmp) {
                printf("DRIVER STARTED FOR DEVICE: 0x%x/0x%x\n", pci_device, tmp);
                exit(1);
            }

        } else if (strncmp(argv[i], "function=", strlen("function=") - 1) == 0){
            tmp = atol(argv[i] + strlen("function="));
            if (pci_function == PCI_DONT_CARE) {
                pci_function = tmp;
            }

            if (pci_function != tmp) {
                printf("DRIVER STARTED FOR FUNCTION: 0x%x/0x%x\n", pci_bus, tmp);
                exit(1);
            }

            if (pci_function != 0) {
                USER_PANIC("Second port not implemented, please use function=0")
            }
        } else if (strncmp(argv[i], "msix=", strlen("msix=") - 1) == 0){
            USER_PANIC("MSI-X not fully supported yet");
            use_msix = !!atol(argv[i] + strlen("msix="));
            //qd_rgument(argv[i]);
        } else {
            printf("Unrecognized argument %s ignored \n", argv[i]);
            continue;
        }
    }
}

static void eventloop(void)
{
    struct waitset *ws;

    ws = get_default_waitset();
    DEBUG("SFN5122F enter event loop \n");
    while (1) {
        event_dispatch(ws);
    }
}

static void cd_main(void)
{
    eventloop();
}


int main(int argc, char** argv)
{
    
    //barrelfish_usleep(10*1000*1000);
    DEBUG("SFN5122F driver started \n");
    errval_t err;

    if (argc > 1) {
        uint32_t parsed = sscanf(argv[argc - 1], "%x:%x:%x:%x:%x", &pci_vendor,
                                 &pci_devid, &pci_bus, &pci_device, &pci_function);
        if (parsed != 5) {
            pci_vendor = PCI_DONT_CARE;
            pci_devid = PCI_DONT_CARE;
            pci_bus = PCI_DONT_CARE;
            pci_device = PCI_DONT_CARE;
            pci_function = 0;
        } else {
            if ((pci_vendor != PCI_VENDOR_SOLARFLARE) || (pci_devid != DEVICE_ID)) {
                printf("VENDOR/DEVICE ID MISMATCH: %x/%x %x/%x \n",
                        pci_vendor, PCI_VENDOR_SOLARFLARE, pci_devid, DEVICE_ID);
            }
            argc--;
        }
    }

    parse_cmdline(argc, argv);

    /* Register our device driver */
    err = pci_client_connect();
    assert(err_is_ok(err));
    err = pci_register_driver_irq(pci_init_card, NULL, PCI_CLASS_ETHERNET,
                                PCI_DONT_CARE, PCI_DONT_CARE,
                                pci_vendor, pci_devid,
                                pci_bus, pci_device, pci_function,
                                global_interrupt_handler, NULL);

    while (!initialized) {
        event_dispatch(get_default_waitset());
    }
    
    start_all();
    
    /* loop myself */
    cd_main();
}
