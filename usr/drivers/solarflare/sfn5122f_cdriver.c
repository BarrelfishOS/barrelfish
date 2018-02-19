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
#include <driverkit/driverkit.h>
#include <pci/pci.h>
#include <pci/pci_driver_client.h>

// TODO only required for htonl
//#include <lwip/ip.h>
#include <net/net.h>

#include <if/sfn5122f_defs.h>
#include <if/sfn5122f_devif_defs.h>
#include <if/net_filter_defs.h>

#include "sfn5122f.h"
#include "sfn5122f_debug.h"
#include "buffer_tbl.h"

#define SERVICE_NAME "sfn5122f"

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

    // copy from driver state
    sfn5122f_t *d;
};


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

struct sfn5122f_driver_state {

    /* Driver arguments */
    struct capref* caps;

    bool use_msix;
    char *service_name;
    sfn5122f_t *d;
    void* d_virt;
    //sfn5122f_msix_t *d_msix;
    uint64_t d_mac[2];
    bool initialized;    
    struct capref regframe;
    /* Interrupt state  */
    struct capref int_ker;
    void* int_ker_virt;
    /*  MAC stats  */
    struct capref mac_stats;
    void* mac_virt;
    uint64_t mac_phys;
    // Port  info
    uint32_t cap[2];
    uint32_t speed[2];
    uint32_t flags[2];  
    uint32_t fcntl [2];

    // Phy info
    uint32_t phy_caps[2];
    uint32_t phy_flags[2];
    uint32_t phy_media[2];
    /* Loopback mode none and speed */
    uint32_t phy_loopback_mode;
    //static uint32_t phy_loopback_speed = 0;
    //WoL Filter id
    uint32_t wol_filter_id;

    bool csum_offload;
    // TX / RX
    uint32_t rx_indir_tbl[128];

    // Queues
    struct queue_state queues[1024];
    /* PCI device address passed on command line */
    uint32_t pci_bus;
    uint32_t pci_device;
    uint32_t pci_vendor;
    uint32_t pci_devid;
    uint32_t pci_function;

    struct bmallocator msix_alloc;
    size_t cdriver_msix;
    uint8_t cdriver_vector;

    bool use_interrupt;

    // first to start everything
    bool first;

    /* Hash key */
    uint8_t rx_hash_key[40];
    uint8_t mc_hash[32];

    /* scatter and rss enable */
    bool rss_en;
    bool scatter_en;


    struct sfn5122f_filter_ip filters_rx_ip[NUM_FILTERS_IP];
    /*
    struct sfn5122f_filter_ip filters_tx_ip[NUM_FILTERS_IP];
    struct sfn5122f_filter_mac filters_rx_ip[NUM_FILTERS_MAC];
    struct sfn5122f_filter_mac filters_tx_ip[NUM_FILTERS_MAC];
    */

    struct pcid pdc;
};

/******************************************************************************/
/* Prototypes */

//static void probe_all(void);
//static uint32_t init_txq(uint16_t n, lpaddr_t phys, bool csum, bool userspace);
//static uint32_t init_rxq(uint16_t n, lpaddr_t phys, bool userspace);
//static uint32_t init_evq(uint16_t n, lpaddr_t phys, bool interrupt);
//static void queue_hw_stop(uint16_t n);

static void global_interrupt_handler(void* arg);
static void setup_interrupt(struct sfn5122f_driver_state* st, size_t *msix_index, 
                            uint8_t core, uint8_t vector);
/***************************************************************************/
/* Filters */

static void sfn5122f_filter_port_setup(struct sfn5122f_driver_state* st, int idx, 
                                       struct sfn5122f_filter_ip* filter)
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
    filter_hi = sfn5122f_rx_filter_tbl_hi_rss_en_insert(filter_hi, st->rss_en);
    filter_hi = sfn5122f_rx_filter_tbl_hi_scatter_en_insert(filter_hi, st->scatter_en);

    sfn5122f_rx_filter_tbl_lo_wr(st->d, idx, filter_lo);
    sfn5122f_rx_filter_tbl_hi_wr(st->d, idx, filter_hi);
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

static int ftqf_alloc(struct sfn5122f_driver_state* st, 
                      struct sfn5122f_filter_ip* f)
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
        if (st->filters_rx_ip[key].enabled == false) {
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

static errval_t reg_port_filter(struct sfn5122f_driver_state* st, 
                                struct sfn5122f_filter_ip* f, uint64_t* fid)
{
    int filt_ind;

    DEBUG("reg_port_filter: called\n");

    if ((filt_ind=ftqf_alloc(st, f)) < 0) {
        return FILTER_ERR_NOT_ENOUGH_MEMORY;
    }

    st->filters_rx_ip[filt_ind] = *f;
    st->filters_rx_ip[filt_ind].enabled = true;

    sfn5122f_filter_port_setup(st, filt_ind, f);

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

static void handle_assertions(sfn5122f_t* d, uint8_t pci_function)
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
static void get_link(struct sfn5122f_driver_state* st, uint8_t port)
{
    uint8_t out[CMD_GET_LINK_OUT_LEN];
    errval_t err;

    err = mcdi_rpc(CMD_GET_LINK, NULL, 0 , out, CMD_GET_LINK_OUT_LEN, NULL, 
                   port,st->d);
    assert(err_is_ok(err));

    memcpy(&(st->cap[port]), out, 4);
    memcpy(&(st->speed[port]), out+CMD_GET_LINK_OUT_SPEED_OFFSET, 4);
    memcpy(&(st->fcntl[port]), out+CMD_GET_LINK_OUT_FCNTL_OFFSET, 4);
    memcpy(&(st->flags[port]), out+CMD_GET_LINK_OUT_FLAGS_OFFSET, 4);
   
    decode_link(st->fcntl[port], st->flags[port], st->speed[port]);

}


/* Init port */
static void init_port(struct sfn5122f_driver_state* st, uint8_t port)
{
    uint8_t in[CMD_SET_MAC_IN_LEN];
    uint32_t reg;
    errval_t err;

    memcpy(in + CMD_SET_MAC_IN_ADR_OFFSET, &(st->d_mac[port]), 6 );
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
    err = mcdi_rpc(CMD_SET_MAC, in, CMD_SET_MAC_IN_LEN, NULL, 0, NULL, port, st->d);
    assert(err_is_ok(err));

    memset(st->mc_hash, 0, sizeof(st->mc_hash));
    err = mcdi_rpc(CMD_SET_MCAST_HASH, st->mc_hash , CMD_SET_MCAST_HASH_IN_LEN,
                   NULL, 0 , NULL, port, st->d);
    assert(err_is_ok(err));

    memset(in, 0 , sizeof(in));
    memcpy(in + CMD_SET_LINK_IN_CAP_OFFSET, &(st->cap[st->pci_function]), 4);
    
    err = mcdi_rpc(CMD_SET_LINK, in, CMD_SET_LINK_IN_LEN, NULL, 0, NULL, 0, st->d);
    assert(err_is_ok(err));
}
/*  start port        */
static void start_port(struct sfn5122f_driver_state* st, uint8_t port)
{
    uint8_t in[CMD_SET_MAC_IN_LEN];
    uint64_t reg;
    errval_t err;

    memset(&in, 0, sizeof(in));

    err = mcdi_rpc(CMD_SET_MCAST_HASH, st->mc_hash , CMD_SET_MCAST_HASH_IN_LEN,
                   NULL, 0 , NULL, port, st->d);
    assert(err_is_ok(err));

    /* mac address */
    memcpy(in + CMD_SET_MAC_IN_ADR_OFFSET, &(st->d_mac[port]), 6 );
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
    err = mcdi_rpc(CMD_SET_MAC, in, CMD_SET_MAC_IN_LEN, NULL, 0, NULL, port, st->d);
    assert(err_is_ok(err));

    err = mcdi_rpc(CMD_SET_MCAST_HASH, st->mc_hash , CMD_SET_MCAST_HASH_IN_LEN,
                   NULL, 0 , NULL, port, st->d);

    assert(err_is_ok(err));
}

/******************************************************************************
 * Device init
 *****************************************************************************/

static void probe_all(struct sfn5122f_driver_state* st)
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
    offset = MCDI_REBOOT_OFFSET(st->pci_function);

    reg =  sfn5122f_mc_treg_smem_rd(st->d,offset);
    if (reg != 0) {
        sfn5122f_mc_treg_smem_wr(st->d,offset,0);
    }

    /*print out any assertions */
    handle_assertions(st->d, st->pci_function);
    // Let BMC know that driver is in charg of filter/link setttings
    // before we can restet NIC
    memset(&in, 0, sizeof(in));
    memset(&out, 0 , sizeof(out));
    
    r = mcdi_rpc(CMD_GET_VERSION, NULL, 0, out, CMD_GET_VERSION_OUT_LEN,
                 &outlen, st->pci_function, st->d);
    assert(err_is_ok(r));


    memset(&out, 0 , sizeof(out));

    // driver is operating / + update
    in[0] = 0x1;
    in[4] = 0x1;
    r = mcdi_rpc(CMD_DRV_ATTACH, in, CMD_DRV_ATTACH_IN_LEN, out,
                 CMD_DRV_ATTACH_OUT_LEN, &outlen, st->pci_function, st->d);
    assert(err_is_ok(r));

    /* reset card */
    r = mcdi_rpc(CMD_PORT_RESET, NULL, 0, NULL, 0, NULL, st->pci_function, st->d);
    assert(err_is_ok(r));

    // init WoL Filter
    if(mcdi_rpc(CMD_WOL_FILTER_GET, NULL, 0, out, CMD_WOL_FILTER_GET_OUT_LEN,
       &outlen, st->pci_function, st->d) == SYS_ERR_OK) {
        memcpy(&(st->wol_filter_id), out , 4);
    } else {
      // Reset filter of card
      mcdi_rpc(CMD_WOL_FILTER_RESET, NULL, 0, NULL, 0, NULL, st->pci_function, st->d);
    }
 
    //  memory for INT_KER
    st->int_ker_virt = alloc_map_frame(VREGION_FLAGS_READ_WRITE,
                                   2*sizeof(uint64_t), &(st->int_ker));
    memset(st->int_ker_virt, 0, 2*sizeof(uint64_t));
    // Read in non volatile configuration
    memset(&out, 0, sizeof(out));
    r = mcdi_rpc(CMD_GET_BOARD_CONFIG, NULL, 0, out,
                 CMD_GET_BOARD_CONFIG_OUT_LEN, &outlen, st->pci_function, st->d);
    assert(err_is_ok(r));

    memcpy(&(st->d_mac[0]), out+MCDI_MAC_PORT_OFFSET(0) ,6);
    memcpy(&(st->d_mac[1]), out+MCDI_MAC_PORT_OFFSET(1) ,6);
    
    // read phy configuration
    r = mcdi_rpc(CMD_GET_PHY_CFG, NULL, 0, out, CMD_GET_PHY_CFG_OUT_LEN, &outlen,
                 st->pci_function, st->d);
    assert(err_is_ok(r));

    memcpy(&st->phy_caps[st->pci_function], out+CMD_GET_PHY_CFG_OUT_CAP_OFFSET, 4);
    memcpy(&st->phy_flags[st->pci_function], out+CMD_GET_PHY_CFG_OUT_FLAGS_OFFSET, 4);
    memcpy(&st->phy_media[st->pci_function], out+CMD_GET_PHY_CFG_OUT_MEDIA_OFFSET, 4);

    // get loopback modes
    r = mcdi_rpc(CMD_GET_LOOPBACK_MODES, NULL, 0, out,
                 CMD_GET_LOOPBACK_MODES_OUT_LEN, &outlen, st->pci_function, st->d);
    assert(err_is_ok(r));
    memcpy(&(st->phy_loopback_mode), out+CMD_GET_LOOPBACK_MODES_SUGGESTED_OFFSET,4);
    // loopback mode NONE is no valid condition
    st->phy_loopback_mode &= ~(1);
   

    // MAC STATS INIT
    st->mac_virt = alloc_map_frame(VREGION_FLAGS_READ_WRITE,
                                   NUM_MAC_STATS*sizeof(uint64_t),
                                   &st->mac_stats);

    assert(st->mac_virt != NULL);
    r = invoke_frame_identify(st->mac_stats, &frameid);
    assert(err_is_ok(r));
    st->mac_phys = frameid.base;
    memset(st->mac_virt, 0, NUM_MAC_STATS*sizeof(uint64_t));


    memset(&in, 0, sizeof(in));
    memcpy(in, &st->mac_phys, 8);

     // Settings for DMA of MAC stats
    in[CMD_MAC_STATS_IN_CMD_OFFSET] = 0x6;
    in[CMD_MAC_STATS_IN_DMA_LEN_OFFSET] = 8;
    in[CMD_MAC_STATS_IN_DMA_LEN_OFFSET+1] = 3;
    r = mcdi_rpc(CMD_MAC_STATS, in, CMD_MAC_STATS_IN_LEN, NULL, 0, NULL,
                st->pci_function, st->d);
    assert(err_is_ok(r));

}



// Init card IP filters
static void init_rx_filter_config(struct sfn5122f_driver_state* st)
{
    uint64_t reg_hi, reg_lo;

    for (int i = 0; i < NUM_FILTERS_IP; i++) {
        sfn5122f_rx_filter_tbl_lo_wr(st->d, i, 0);
        sfn5122f_rx_filter_tbl_hi_wr(st->d, i, 0);
    }

    reg_lo = sfn5122f_rx_filter_ctl_reg_lo_rd(st->d);
    reg_hi = sfn5122f_rx_filter_ctl_reg_hi_rd(st->d);

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


    sfn5122f_rx_filter_ctl_reg_lo_wr(st->d,reg_lo);
    sfn5122f_rx_filter_ctl_reg_hi_wr(st->d,reg_hi);

}

static void device_init(struct sfn5122f_driver_state* st)
{
    errval_t r;
    struct frame_identity frameid = { .base = 0, .bytes = 0 };
    uint64_t reg, reg2; // tmp_key = 0;
    uint8_t in[24]; // set length to biggest in length needed

    memset(&in, 0, sizeof(in));

    // recover from failed assertion post-reset
    handle_assertions(st->d, st->pci_function);

    /* ignore TX of packets 16 bytes and less */
    reg = sfn5122f_tx_reserved_reg_lo_rd(st->d);
    reg = sfn5122f_tx_reserved_reg_lo_tx_flush_min_len_en_insert(reg, 1);
    sfn5122f_tx_reserved_reg_lo_wr(st->d, reg);
    sfn5122f_tx_reserved_reg_hi_wr(st->d, sfn5122f_tx_reserved_reg_hi_rd(st->d));
    //Disable TX_NO_EOP_DISC_EN because else would limit packets to 16
    reg = sfn5122f_tx_cfg_reg_lo_rd(st->d);
    reg = sfn5122f_tx_cfg_reg_lo_tx_no_eop_disc_en_insert(reg, 0);
    reg = sfn5122f_tx_cfg_reg_lo_tx_ownerr_ctl_insert(reg, 1);
    reg = sfn5122f_tx_cfg_reg_lo_tx_filter_en_bit_insert(reg, 1);
    sfn5122f_tx_cfg_reg_lo_wr(st->d, reg);
    sfn5122f_tx_cfg_reg_hi_wr(st->d, sfn5122f_tx_cfg_reg_hi_rd(st->d));

    reg = sfn5122f_rx_cfg_reg_lo_rd(st->d);
    // unset bit and set other bit which are not in documentation (43 and 47)
    reg = sfn5122f_rx_cfg_reg_lo_rx_desc_push_en_insert(reg, 0) ;
    reg = sfn5122f_rx_cfg_reg_lo_rx_ingr_en_insert(reg, 1);
    //reg = sfn5122f_rx_cfg_reg_lo_rx_usr_buf_size_insert(reg, (MTU_MAX-256) >> 5);
    reg = sfn5122f_rx_cfg_reg_lo_rx_usr_buf_size_insert(reg, 4096 >> 5);
    //reg = sfn5122f_rx_cfg_reg_lo_rx_ownerr_ctl_insert(reg, 1);
    reg = sfn5122f_rx_cfg_reg_lo_rx_ip_hash_insert(reg, 1);
    //reg = sfn5122f_rx_cfg_reg_lo_rx_hash_insrt_hdr_insert(reg, 1);
    reg = sfn5122f_rx_cfg_reg_lo_rx_hash_alg_insert(reg, 1);
    sfn5122f_rx_cfg_reg_lo_wr(st->d, reg);
    sfn5122f_rx_cfg_reg_hi_wr(st->d, sfn5122f_rx_cfg_reg_hi_rd(st->d));
    /* enable event logging, no UART
      Event destination is queue 0 */
    in[0] = 0x2;
    r = mcdi_rpc(CMD_LOG_CTRL, in, CMD_LOG_CTRL_IN_LEN,
                 NULL, 0, NULL, st->pci_function, st->d);
    assert(err_is_ok(r));

    /* Set destination of TX/RX flush event */
    
    sfn5122f_dp_ctrl_reg_lo_fls_evq_id_wrf(st->d, 0);
    sfn5122f_dp_ctrl_reg_hi_wr(st->d, sfn5122f_dp_ctrl_reg_hi_rd(st->d));
  
    /* Disalbe user events for now     */
    sfn5122f_usr_ev_cfg_lo_usrev_dis_wrf(st->d , 1);
    sfn5122f_usr_ev_cfg_hi_wr(st->d, sfn5122f_usr_ev_cfg_hi_rd(st->d));


    // This seems to be not device specific i.e. works for other
    // Solarflare cards
    /* Set position of descriptor caches in SRAM */
    sfn5122f_srm_tx_dc_cfg_reg_lo_wr(st->d, TX_DC_BASE);
    sfn5122f_srm_tx_dc_cfg_reg_hi_wr(st->d, sfn5122f_srm_tx_dc_cfg_reg_hi_rd(st->d));
    sfn5122f_srm_rx_dc_cfg_reg_lo_srm_rx_dc_base_adr_wrf(st->d, RX_DC_BASE);
    sfn5122f_srm_rx_dc_cfg_reg_hi_wr(st->d, sfn5122f_srm_rx_dc_cfg_reg_hi_rd(st->d));

    /* Set TX descriptor cache size to 16 */
    sfn5122f_tx_dc_cfg_reg_lo_tx_dc_size_wrf(st->d, 1);
    sfn5122f_tx_dc_cfg_reg_hi_wr(st->d, sfn5122f_tx_dc_cfg_reg_hi_rd(st->d));

    /* Set RX descriptor cache size to 64 and low watermark */
    sfn5122f_rx_dc_cfg_reg_lo_rx_dc_size_wrf(st->d, 3);
    sfn5122f_rx_dc_cfg_reg_hi_wr(st->d, sfn5122f_rx_dc_cfg_reg_hi_rd(st->d));

    reg = 0;
    reg = sfn5122f_rx_dc_pf_wm_reg_lo_rx_dc_pf_lwm_insert(reg, RX_DESC_CACHE_SIZE -8);
    sfn5122f_rx_dc_pf_wm_reg_lo_wr(st->d, reg);
    sfn5122f_rx_dc_pf_wm_reg_hi_wr(st->d, sfn5122f_rx_dc_pf_wm_reg_hi_rd(st->d));
   
   /*programm init ker address for interrupts */
    r = invoke_frame_identify(st->int_ker, &frameid);
    assert(err_is_ok(r));

    sfn5122f_int_adr_reg_ker_lo_wr(st->d, frameid.base);
    reg = sfn5122f_int_adr_reg_ker_hi_rd(st->d);

    // disable vector write if we use MSI-X
    if (st->use_msix) {
        reg = sfn5122f_int_adr_reg_ker_hi_norm_int_vec_dis_ker_insert(reg, 1);
        if (st->cdriver_msix == -1) {
            r = pci_setup_inthandler(global_interrupt_handler, NULL, &(st->cdriver_vector));
            assert(err_is_ok(r));
            setup_interrupt(st, &(st->cdriver_msix), disp_get_core_id(), (st->cdriver_vector));
        }
    } else {
        reg = sfn5122f_int_adr_reg_ker_hi_norm_int_vec_dis_ker_insert(reg, 0);
    }
    sfn5122f_int_adr_reg_ker_hi_wr(st->d, reg);
   
    /* Enable all the genuinley fatal interrupts */
    reg = sfn5122f_fatal_intr_reg_ker_lo_ill_adr_int_ker_en_insert(reg, 1);
    /* Enable rxbuf/txbuf interrupt  fields not documented.
       Set bits 39 and 38*/
    reg = sfn5122f_fatal_intr_reg_ker_lo_rxbuf_own_int_ker_en_insert(reg, 1);
    reg = sfn5122f_fatal_intr_reg_ker_lo_txbuf_own_int_ker_en_insert(reg, 1);
    
    //reg = sfn5122f_fatal_intr_reg_ker_lo_sram_perr_int_p_ker_en_insert(reg, 1);
    sfn5122f_fatal_intr_reg_ker_lo_wr(st->d, ~reg);
    sfn5122f_fatal_intr_reg_ker_hi_wr(st->d, 0XFFFFFFFFFFFFFFFF);

    /* Setup RSS indirection table (maps from hash value to packet to RXQ) */
    for (int i = 0; i < 128; i++) {
        st->rx_indir_tbl[i] = 0;
        sfn5122f_rx_indirection_tbl_wr(st->d, i, st->rx_indir_tbl[i]);
    }

    /* Disable the ugly timer-based TX DMA backoff and allow TX DMA to be
     * controlled by the RX FIFO fill level. Set arbitration to one pkt/Q.
      (from linux driver) */
    reg = sfn5122f_tx_reserved_reg_lo_rd(st->d);
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
 
    reg2 = sfn5122f_tx_reserved_reg_hi_rd(st->d);
    reg2 = sfn5122f_tx_reserved_reg_hi_tx_push_en_insert(reg2, 0);
    reg2 = sfn5122f_tx_reserved_reg_hi_tx_push_chk_dis_insert(reg2, 0);
    //reg2 = sfn5122f_tx_reserved_reg_hi_tx_rx_spacer_insert(reg2, 0xfe);
    reg2 = sfn5122f_tx_reserved_reg_hi_tx_rx_spacer_insert(reg2, 0x1);
    sfn5122f_tx_reserved_reg_lo_wr(st->d, reg);
    sfn5122f_tx_reserved_reg_hi_wr(st->d, reg2);

    init_port(st, st->pci_function);
    get_link(st, st->pci_function);
    DEBUG("BASIC CARD INIT DONE \n");
}

static void start_all(struct sfn5122f_driver_state* st)
{
    uint64_t reg;
 
    start_port(st, st->pci_function);

    memset(st->int_ker_virt, 0, 2*sizeof(uint64_t));
    /*  Enable interrupts   */
    /* Use an interrupt level unused by event queues */
    reg = sfn5122f_int_en_reg_ker_lo_rd(st->d);
    if (st->use_msix) {
        reg = sfn5122f_int_en_reg_ker_lo_ker_int_leve_sel_insert(reg, 0);
    } else {
        // legacy
        reg = sfn5122f_int_en_reg_ker_lo_ker_int_leve_sel_insert(reg, 0x1f);
    }
    reg = sfn5122f_int_en_reg_ker_lo_drv_int_en_ker_insert(reg, 1);

    /*   undocumented field   */
    reg = sfn5122f_int_en_reg_ker_lo_ker_int_ker_insert(reg, 0);
    sfn5122f_int_en_reg_ker_lo_wr(st->d, reg);
    sfn5122f_int_en_reg_ker_hi_wr(st->d, sfn5122f_int_en_reg_ker_hi_rd(st->d));

    errval_t err;
    if (st->use_interrupt) {
        err = pcid_connect_int(&st->pdc, 0, global_interrupt_handler, st);
        if(err_is_fail(err)){
            USER_PANIC("Setting up interrupt failed \n");
        }
    }

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


static void queue_hw_stop(struct queue_state* st, uint16_t n)
{

    uint64_t reg = 0;
    /* flush TX queue */
    reg = sfn5122f_tx_flush_descq_reg_lo_rd(st->d);
    reg = sfn5122f_tx_flush_descq_reg_lo_tx_flush_descq_insert(reg, n);
    reg = sfn5122f_tx_flush_descq_reg_lo_tx_flush_descq_cmd_insert(reg, 1);
    sfn5122f_tx_flush_descq_reg_lo_wr(st->d, reg);
    sfn5122f_tx_flush_descq_reg_hi_wr(st->d, sfn5122f_tx_flush_descq_reg_hi_rd(st->d));
    /* flush RX queue */
    reg = sfn5122f_rx_flush_descq_reg_lo_rd(st->d);
    reg = sfn5122f_rx_flush_descq_reg_lo_rx_flush_descq_insert(reg, n);
    reg = sfn5122f_rx_flush_descq_reg_lo_rx_flush_descq_cmd_insert(reg, 1);
    sfn5122f_rx_flush_descq_reg_lo_wr(st->d, reg);
    sfn5122f_rx_flush_descq_reg_hi_wr(st->d, sfn5122f_rx_flush_descq_reg_hi_rd(st->d));

    /*   TODO Wait for DRIVER_EVENT    */
    /* clear pointer table entries */
    sfn5122f_tx_desc_ptr_tbl_lo_wr(st->d, n, 0);
    sfn5122f_tx_desc_ptr_tbl_hi_wr(st->d, n, 0);
    sfn5122f_rx_desc_ptr_tbl_lo_wr(st->d, n, 0);
    sfn5122f_rx_desc_ptr_tbl_hi_wr(st->d, n, 0);

    /*Free RX queue tbl entries*/
    reg = 0;
    reg = sfn5122f_buf_tbl_upd_reg_lo_buf_clr_cmd_insert(reg, 1);
    reg = sfn5122f_buf_tbl_upd_reg_lo_buf_clr_start_id_insert(reg,
                                              st->rx_buf_tbl);

    if (st->userspace) {
       reg = sfn5122f_buf_tbl_upd_reg_lo_buf_clr_end_id_insert(reg,
                                  st->rx_buf_tbl + NUM_ENT_RX_USR);
    } else {
        reg = sfn5122f_buf_tbl_upd_reg_lo_buf_clr_end_id_insert(reg,
                                       st->rx_buf_tbl + NUM_ENT_RX);
    }

    /*Free TX queue tbl entries*/
    reg = 0;
    reg = sfn5122f_buf_tbl_upd_reg_lo_buf_clr_cmd_insert(reg, 1);
    reg = sfn5122f_buf_tbl_upd_reg_lo_buf_clr_end_id_insert(reg,
                              st->tx_buf_tbl + NUM_ENT_TX );
    reg = sfn5122f_buf_tbl_upd_reg_lo_buf_clr_start_id_insert(reg,
                                              st->tx_buf_tbl);

    /*Free EV queue tbl entries*/
    reg = 0;
    reg = sfn5122f_buf_tbl_upd_reg_lo_buf_clr_cmd_insert(reg, 1);
    reg = sfn5122f_buf_tbl_upd_reg_lo_buf_clr_end_id_insert(reg,
                              st->ev_buf_tbl + NUM_ENT_EVQ );
    reg = sfn5122f_buf_tbl_upd_reg_lo_buf_clr_start_id_insert(reg,
                                             st->ev_buf_tbl);
}



static uint32_t init_evq(struct sfn5122f_driver_state* st, uint16_t n, 
                         lpaddr_t phys, bool interrupt)
{

    //errval_t r;
    //struct frame_identity frameid = { .base = 0, .bytes = 0 };
    uint64_t reg, buffer_offset;
    reg = 0;

    reg = sfn5122f_timer_tbl_lo_timer_q_en_insert(reg, 1);
    // set to 0 if interrupts for receives/sends should be generated
    if (st->use_msix) {
        reg = sfn5122f_timer_tbl_lo_host_notify_mode_insert(reg, 0);
    } else {
        reg = sfn5122f_timer_tbl_lo_int_pend_insert(reg, 0);
        reg = sfn5122f_timer_tbl_lo_int_armd_insert(reg, 0);
        if (st->use_interrupt && interrupt) {
            reg = sfn5122f_timer_tbl_lo_host_notify_mode_insert(reg, 0);
        } else {
            reg = sfn5122f_timer_tbl_lo_host_notify_mode_insert(reg, 1);
        }
    }
    // timer mode disabled
    reg = sfn5122f_timer_tbl_lo_timer_mode_insert(reg, 0);
    sfn5122f_timer_tbl_lo_wr(st->d, n, reg);
    sfn5122f_timer_tbl_hi_wr(st->d, n, sfn5122f_timer_tbl_hi_rd(st->d, n));

    /*
    r = invoke_frame_identify(queues[n].ev_frame, &frameid);
    assert(err_is_ok(r));
    ev_phys = frameid.base;
    */

    buffer_offset = alloc_buf_tbl_entries(phys, NUM_ENT_EVQ, 0, 0, st->d);
    if (buffer_offset == -1) {
        return -1;
    }

    DEBUG("EV_QUEUE_%d: buf_off %ld, phys 0x%lx\n",n , buffer_offset, phys);
    //  setup EV queue
    reg = sfn5122f_evq_ptr_tbl_lo_rd(st->d, n);
    reg = sfn5122f_evq_ptr_tbl_lo_evq_en_insert(reg, 1);
    reg = sfn5122f_evq_ptr_tbl_lo_evq_size_insert(reg, 6);
    reg = sfn5122f_evq_ptr_tbl_lo_evq_buf_base_id_insert(reg,
           buffer_offset);

    sfn5122f_evq_ptr_tbl_lo_wr(st->d, n, reg);
    sfn5122f_evq_ptr_tbl_hi_wr(st->d, n, sfn5122f_evq_ptr_tbl_hi_rd(st->d, n));

    /* No write collection for this register   */
    reg = sfn5122f_timer_command_reg_lo_rd(st->d,n);
    reg = sfn5122f_timer_command_reg_lo_tc_timer_val_insert(reg, 0);
    if (st->use_msix) {
        reg = sfn5122f_timer_command_reg_lo_tc_timer_mode_insert(reg, 0);
    } else {
        reg = sfn5122f_timer_command_reg_lo_tc_timer_mode_insert(reg, 0);
    }

    sfn5122f_timer_command_reg_lo_wr(st->d, n, reg);

    sfn5122f_evq_rptr_reg_wr(st->d, n, st->queues[n].ev_head);

    return buffer_offset;
}

static uint32_t init_rxq(struct sfn5122f_driver_state* st, uint16_t n, 
                         lpaddr_t phys, bool userspace)
{
    uint64_t reg_lo, reg_hi,  buffer_offset;
   /*
    * This will define a buffer in the buffer table, allowing
    * it to be used for event queues, descriptor rings etc.
    */
    /* Get physical addresses for rx/tx rings and event queue */

    /* RX   */
    if (userspace) {
        buffer_offset = alloc_buf_tbl_entries(phys, NUM_ENT_RX_USR, 0, false, st->d);
    } else {
        buffer_offset = alloc_buf_tbl_entries(phys, NUM_ENT_RX, 0, false, st->d);
    }

    if (buffer_offset == -1) {
       return -1;
    }

    DEBUG("RX_QUEUE_%d: buf_off %ld, phys %lx\n", n,
          buffer_offset, phys);
    /* setup RX queue */
    reg_lo = sfn5122f_rx_desc_ptr_tbl_lo_rd(st->d, n);
    reg_hi = sfn5122f_rx_desc_ptr_tbl_hi_rd(st->d, n);
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

    sfn5122f_rx_desc_ptr_tbl_lo_wr(st->d, n, reg_lo);
    sfn5122f_rx_desc_ptr_tbl_hi_wr(st->d, n, reg_hi);

    return buffer_offset;
}


static uint32_t init_txq(struct sfn5122f_driver_state* st, uint16_t n, 
                         uint64_t phys, bool csum, bool userspace)
{

    uint64_t reg, reg1, buffer_offset;
  
    buffer_offset = alloc_buf_tbl_entries(phys, NUM_ENT_TX, 0, 0, st->d);
    
    if (buffer_offset == -1) {
       return -1;
    }

    DEBUG("TX_QUEUE_%d: buf_off %ld, phys %lx\n",n , buffer_offset, phys);
    /* setup TX queue */
    reg = sfn5122f_tx_desc_ptr_tbl_lo_rd(st->d, n);
    reg1 = sfn5122f_tx_desc_ptr_tbl_hi_rd(st->d, n);
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
        DEBUG("TX_QUEUE_%d: user lvl networking disabled \n", n);
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
    sfn5122f_tx_desc_ptr_tbl_lo_wr(st->d, n, reg);
    sfn5122f_tx_desc_ptr_tbl_hi_wr(st->d, n, reg1);
 
    return buffer_offset;
}


static void setup_interrupt(struct sfn5122f_driver_state* st, size_t *msix_index, 
                            uint8_t core, uint8_t vector)
{
    bool res;
    errval_t err;
    uint8_t dest;

    res = bmallocator_alloc(&st->msix_alloc, msix_index);
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
    struct queue_state* st = (struct queue_state*) arg;
    err = st->devif->tx_vtbl.interrupt(st->devif, NOP_CONT, st->qid);
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
    struct sfn5122f_driver_state* st = (struct sfn5122f_driver_state*) arg;

    uint8_t* net_ivec_fatal = (uint8_t *) st->int_ker_virt;

    // bit 64 is indicator for a fatal event
    syserr = (net_ivec_fatal[8] & 0x1);
    if (syserr) {
        // TODO handle fatal interrupt
        USER_PANIC("FATAL INTERRUPT");
    } else {

    }

    q_to_check = sfn5122f_int_isr0_reg_lo_rd(st->d);

    for (uint64_t i = 0; i < 32; i++) {
        if ((q_to_check >> i) & 0x1) {
            if (st->queues[i].use_irq && st->queues[i].devif != NULL) {
                DEBUG("Interrupt to queue %lu \n", i);
                err = st->queues[i].devif->tx_vtbl.interrupt(st->queues[i].devif, NOP_CONT, i);
                if (err_is_fail(err)) {
                    err = st->queues[i].devif->register_send(st->queues[i].devif,
                                                            get_default_waitset(),
                                                            MKCONT(resend_interrupt, (void*) &st->queues[i]));
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
    struct sfn5122f_driver_state* st = (struct sfn5122f_driver_state*) b->st;
    assert(st != NULL);

    errval_t err;
    struct frame_identity id;

    int n = -1;
    for (int i = 1; i < NUM_QUEUES; i++) {
        if (st->queues[i].enabled == false) {
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
        if (st->queues[0].enabled == false) {
            n = 0;
        } else {
            printf("Default queue already initalized \n");
            return NIC_ERR_ALLOC_QUEUE;
        }
    }

    // TODO HOW TO HANDLE THIS STATE
    //b->st = &st->queues[n];

    st->queues[n].use_irq = interrupt;
    st->queues[n].enabled = false;
    st->queues[n].tx_frame = frame;
    st->queues[n].tx_head = 0;
    st->queues[n].rx_head = 0;
    st->queues[n].ev_head = 0;
    st->queues[n].rxbufsz = MTU_MAX;
    st->queues[n].devif = b;
    st->queues[n].userspace = user;
    st->queues[n].msix_index = -1;
    st->queues[n].msix_intdest = core;
    st->queues[n].msix_intvec = msix_vector;
    st->queues[n].qid = n;
    st->queues[n].d = st->d;

    if (st->queues[n].use_irq && st->use_msix) {
        if (st->queues[n].msix_intvec != 0) {
            if (st->queues[n].msix_index == -1) {
                setup_interrupt(st, &st->queues[n].msix_index, st->queues[n].msix_intdest,
                        st->queues[n].msix_intvec);
            }
        }
    }

    DEBUG("setup queue %d \n", n);

    err = invoke_frame_identify(frame, &id);
    assert(err_is_ok(err));
    // enable checksums
    st->queues[n].tx_buf_tbl = init_txq(st, n, id.base, st->csum_offload, user);
    st->queues[n].rx_buf_tbl = init_rxq(st, n, id.base+ sizeof(uint64_t)*TX_ENTRIES, user);

    st->queues[n].ev_buf_tbl = init_evq(st, n, id.base+sizeof(uint64_t)*(TX_ENTRIES+RX_ENTRIES),
            interrupt);
    if(st->queues[n].ev_buf_tbl == -1 ||
       st->queues[n].tx_buf_tbl == -1 ||
       st->queues[n].rx_buf_tbl == -1){
        *ret_err = NIC_ERR_ALLOC_QUEUE;
        *regs = NULL_CAP;
        return NIC_ERR_ALLOC_QUEUE;
    }

    st->queues[n].enabled = true;
    DEBUG("created queue %d \n", n);

    *mac = st->d_mac[st->pci_function];
    *qid = n;
    
    b->st = &(st->queues[n]);

    err = slot_alloc(regs);
    assert(err_is_ok(err));
    err = cap_copy(*regs, st->regframe);
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
    struct queue_state* st = (struct queue_state*) b->st;

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
    buffer_offset = alloc_buf_tbl_entries(addr, size/BUF_SIZE, qid, true, st->d);
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
    struct sfn5122f_driver_state* st = (struct sfn5122f_driver_state*) b->st;
    errval_t err;
    free_buf_tbl_entries(buftbl_id, size/BUF_SIZE, st->d);
   
    err = b->tx_vtbl.deregister_region_response(b, NOP_CONT, SYS_ERR_OK);
    assert(err_is_ok(err));
}

static void cd_destroy_queue(struct sfn5122f_devif_binding *b, uint16_t qid)
{
    struct queue_state* st = (struct queue_state*) b->st;
    errval_t err;
    queue_hw_stop(st, qid);

    st->enabled = false;
    st->devif = NULL;

    err = b->tx_vtbl.destroy_queue_response(b, NOP_CONT, SYS_ERR_OK);
    assert(err_is_ok(err));
}

static void cd_control(struct sfn5122f_devif_binding *b, uint64_t request,
                       uint64_t arg)
{
    /*
    errval_t err;

    struct queue_state *st = (struct queue_state*) b->st;
    assert(st);

    DEBUG("control arg=0x%lx\n", arg);

    struct sfn5122f_filter_ip f = {
            .dst_port = ((uint32_t)arg >> 16),
            .dst_ip = htonl((uint32_t)(arg >> 32)),
            .src_ip = 0,
            .src_port = 0,
            .type_ip = arg & 0x1,
            .queue = st->qid, // TODO need a way to get right queue ID
    };

    uint64_t fid = 0;
    err = reg_port_filter(st, &f, &fid);

    DEBUG("register filter: 0x%x:%u UDP=%u -> q=%u @ index=%lu %s\n",f.dst_ip,
          f.dst_port, f.type_ip, f.queue, fid, err_getstring(err));


    err = b->tx_vtbl.control_response(b, NOP_CONT, fid, err);
    assert(err_is_ok(err));
    */
    USER_PANIC("NIY\n");
}

static void export_devif_cb(void *st, errval_t err, iref_t iref)
{
    struct sfn5122f_driver_state* s = (struct sfn5122f_driver_state*) st;
    const char *suffix = "_sfn5122fmng_devif";
    char name[strlen(s->service_name) + strlen(suffix) + 1];

    assert(err_is_ok(err));

    // Build label for interal management service
    sprintf(name, "%s%s", s->service_name, suffix);

    err = nameservice_register(name, iref);
    assert(err_is_ok(err));
    DEBUG("Devif Management interface exported\n");
    s->initialized = true;
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
    struct sfn5122f_driver_state* st = (struct sfn5122f_driver_state*) b->st;
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

    errval_t err = reg_port_filter(st, &f, fid);
    assert(err_is_ok(err));
    DEBUG("filter registered: err=%"PRIu64", fid=%"PRIu64"\n", err, *fid);
    return SYS_ERR_OK;
}


static errval_t cb_remove_filter(struct net_filter_binding *b,
                                 net_filter_filter_type_t type,
                                 uint64_t filter_id,
                                 errval_t* err)
{
    struct sfn5122f_driver_state* st = (struct sfn5122f_driver_state*) b->st;
    if ((type == net_filter_PORT_UDP || type == net_filter_PORT_TCP)
        && st->filters_rx_ip[filter_id].enabled == true) {
        st->filters_rx_ip[filter_id].enabled = false;

        sfn5122f_rx_filter_tbl_lo_wr(st->d, filter_id, 0);
        sfn5122f_rx_filter_tbl_hi_wr(st->d, filter_id, 0);
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
    b->st = st;

    return SYS_ERR_OK;
}

/**
 * Initialize management interface for queue drivers.
 * This has to be done _after_ the hardware is initialized.
 */
static void initialize_mngif(struct sfn5122f_driver_state* st)
{
    errval_t r;

    r = sfn5122f_devif_export(st, export_devif_cb, connect_devif_cb,
                              get_default_waitset(), 1);
    assert(err_is_ok(r));

    r = net_filter_export(st, net_filter_export_cb, net_filter_connect_cb,
                          get_default_waitset(), 1);
    assert(err_is_ok(r));
}

/******************************************************************************/
/* Initialization code for driver */

/** Callback from pci to initialize a specific PCI device. */
static void init_card(struct sfn5122f_driver_state* st)
{
    errval_t err;
    bool res;

    st->d = calloc(sizeof(sfn5122f_t), 1);

    int num_bars = pcid_get_bar_num(&st->pdc);

    DEBUG("BAR count %d \n", num_bars);

    DEBUG("Initializing network device.\n");

    if (num_bars < 1) {
        USER_PANIC("Error: Not enough PCI bars allocated. Can not initialize network device.\n");
    }
    lvaddr_t vaddr;
    /* Map first BAR for register access */
    err = pcid_get_bar_cap(&st->pdc, 0, &st->regframe);
    if (err_is_fail(err)) {
        USER_PANIC("pcid_get_bar_cap failed \n");
    }

    err = map_device_cap(st->regframe, &vaddr);
    if (err_is_fail(err)) {
        USER_PANIC("map_device_cap failed \n");
    }

    sfn5122f_initialize(st->d, (void *) vaddr);

    /* Initialize Mackerel binding */
    st->d_virt = (void*) vaddr;
    assert(st->d != NULL);

    // Initialize manager for MSI-X vectors
    if (st->use_msix) {
        //d_msix = malloc(sizeof(*d_msix));
        //map_device(&bar_info[1]);
        //sfn5122f_msix_initialize(d_msix, (void*) bar_info[1].vaddr);
        DEBUG("Enabling MSI-X interrupts\n");
        uint16_t msix_count = 0;
        err = pci_msix_enable(&msix_count);
        assert(err_is_ok(err));
        assert(msix_count > 0);
        DEBUG("MSI-X #vecs=%d\n", msix_count);

        res = bmallocator_init(&st->msix_alloc, msix_count);
        assert(res);
    } else {
        DEBUG("Using legacy interrupts\n");
    }

    /* Get all information needed  */
    DEBUG("Starting probe\n");
    probe_all(st);
    /* Initialize hardware registers etc. */
    /* Start interrups / mac_stats etc.  */
    DEBUG("Init device\n");
    device_init(st);
    /* Init rx filters */
    DEBUG("Init filters\n");
    init_rx_filter_config(st);
    /* initalize managemnt interface   */
    DEBUG("Export management\n");
    initialize_mngif(st);

    if (st->first){
       start_all(st);
       st->first = 0;
    }
}

static void parse_cmdline(struct sfn5122f_driver_state* st, int argc, char **argv)
{
    /*
     * XXX: the following contains a hack only to start the driver when
     *      the supplied bus/dev/funct matches the Kaluga start arguments.
     */
    int i;
    uint32_t tmp;
    for (i = 1; i < argc; i++) {
        if (strncmp(argv[i], "cardname=", strlen("cardname=") - 1) == 0) {
            free(st->service_name);
            st->service_name = argv[i] + strlen("cardname=");
        } else if (strncmp(argv[i], "bus=", strlen("bus=") - 1) == 0) {
            tmp = atol(argv[i] + strlen("bus="));
            if (st->pci_bus == PCI_DONT_CARE) {
                st->pci_bus = tmp;
            }

            if (st->pci_bus != tmp) {
                printf("DRIVER STARTED FOR BUS: 0x%x/0x%x\n", st->pci_bus, tmp);
                exit(1);
            }
            st->pci_bus = atol(argv[i] + strlen("bus="));
        } else if (strncmp(argv[i], "device=", strlen("device=") - 1) == 0) {
            tmp = atol(argv[i] + strlen("device="));
            if (st->pci_device == PCI_DONT_CARE) {
                st->pci_device = tmp;
            }

            if (st->pci_device != tmp) {
                printf("DRIVER STARTED FOR DEVICE: 0x%x/0x%x\n", st->pci_device, tmp);
                exit(1);
            }

        } else if (strncmp(argv[i], "function=", strlen("function=") - 1) == 0){
            tmp = atol(argv[i] + strlen("function="));
            if (st->pci_function == PCI_DONT_CARE) {
                st->pci_function = tmp;
            }

            if (st->pci_function != tmp) {
                printf("DRIVER STARTED FOR FUNCTION: 0x%x/0x%x\n", st->pci_bus, tmp);
                exit(1);
            }

            if (st->pci_function != 0) {
                USER_PANIC("Second port not implemented, please use function=0")
            }
        } else if (strncmp(argv[i], "msix=", strlen("msix=") - 1) == 0){
            USER_PANIC("MSI-X not fully supported yet");
            st->use_msix = !!atol(argv[i] + strlen("msix="));
            //qd_rgument(argv[i]);
        } else {
            printf("Unrecognized argument %s ignored \n", argv[i]);
            continue;
        }
    }
}


// Initalized all default values
static void init_default_values(struct sfn5122f_driver_state* st)
{
    st->use_msix = false;
    st->service_name = calloc(strlen(SERVICE_NAME)+1, 1);
    strcpy(st->service_name, SERVICE_NAME);
    st->phy_loopback_mode = 0;
    st->wol_filter_id = 0;
    st->csum_offload = 1;
    st->pci_function = 0;
    st->pci_bus = PCI_DONT_CARE;
    st->pci_device = PCI_DONT_CARE;
    st->pci_vendor = PCI_DONT_CARE;
    st->pci_devid = PCI_DONT_CARE;
    st->cdriver_msix = -1;
    st->use_interrupt = true;
    st->first = 1;
    st->rss_en = 0;
    st->scatter_en = 0;
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
static errval_t init(struct bfdriver_instance* bfi, const char* name, uint64_t flags,
                     struct capref* caps, size_t caps_len, char** args, size_t args_len, iref_t* dev) {

    //barrelfish_usleep(10*1000*1000);
    DEBUG("SFN5122F driver started \n");
    errval_t err;

    bfi->dstate = calloc(sizeof(struct sfn5122f_driver_state), 1);
    if (bfi->dstate == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }
    
    assert(bfi->dstate != NULL);
    struct sfn5122f_driver_state* st = (struct sfn5122f_driver_state*) bfi->dstate;
    st->caps = caps;

    init_default_values(st);
 
    err = pcid_init(&st->pdc, caps, caps_len, args, args_len, get_default_waitset());
    if(err_is_fail(err)){
        USER_PANIC("pcid_init failed \n");
    }
   
    int argc = args_len;

    for(int i = 0; i < argc; i++) {
        printf("argv[%d] = %s \n", i, args[i]);
    }

    if (argc > 1) {
        uint32_t parsed = sscanf(args[argc - 1], "%x:%x:%x:%x:%x", &st->pci_vendor,
                                 &st->pci_devid, &st->pci_bus, &st->pci_device, &st->pci_function);
        if (parsed != 5) {
            st->pci_vendor = PCI_DONT_CARE;
            st->pci_devid = PCI_DONT_CARE;
            st->pci_bus = PCI_DONT_CARE;
            st->pci_device = PCI_DONT_CARE;
            st->pci_function = 0;
        } else {
            if ((st->pci_vendor != PCI_VENDOR_SOLARFLARE) || (st->pci_devid != DEVICE_ID)) {
                printf("VENDOR/DEVICE ID MISMATCH: %x/%x %x/%x \n",
                        st->pci_vendor, PCI_VENDOR_SOLARFLARE, st->pci_devid, DEVICE_ID);
            }
            argc--;
        }
    }

    parse_cmdline(st, args_len, args);

    init_card(st);

    while (!st->initialized) {
        event_dispatch(get_default_waitset());
    }
    /* loop myself */
    //cd_main();
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
    return SYS_ERR_OK;
}

/**
 * Registers the driver module with the system.
 *
 * To link this particular module in your driver domain,
 * add it to the addModules list in the Hakefile.
 */
DEFINE_MODULE(sfn5122f_module, init, attach, detach, set_sleep_level, destroy);
