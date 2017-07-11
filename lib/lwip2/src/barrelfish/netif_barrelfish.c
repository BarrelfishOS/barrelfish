/**
 * \file netif_barrelfish.c
 * \brief 
 */


/*
 * Copyright (c) 2017 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */


#include "lwip/opt.h"
#include "lwip/def.h"
#include "lwip/init.h"
#include "lwip/mem.h"
#include "lwip/pbuf.h"
#include "lwip/sys.h"
#include <lwip/stats.h>
#include <lwip/snmp.h>
#include "netif/etharp.h"
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <devif/queue_interface.h>

#define BFNETIF_POLL_MAX 128

#define BFNETIF_MTU 1500

#define BFNETIF_IFNAME0 'e'
#define BFNETIF_IFNAME1 'n'

struct bfnetif_state
{
    struct devq *q;
};

struct bf_pbuf
{
    struct pbuf_custom pb;
    regionid_t region_id;
};

LWIP_MEMPOOL_DECLARE(RX_POOL, 10, sizeof(struct bf_pbuf), "Zero-copy RX PBUF pool");

void bfnetif_pbuf_free(void* p)
{
    struct bf_pbuf *bf_p = (struct bf_pbuf *)p;

  LOCK_INTERRUPTS();
  free_rx_dma_descriptor(my_pbuf->dma_descriptor);
  LWIP_MEMPOOL_FREE(RX_POOL, my_pbuf);
  UNLOCK_INTERRUPTS();
}

void eth_rx_irq()
{


}

static void *defq_get_region_base(regionid_t reg)
{
    return NULL;
}


static err_t bfnetif_output(struct netif *netif, struct pbuf *p)
{
    LINK_STATS_INC(link.xmit);

    /* Update SNMP stats (only if you use SNMP) */
    MIB2_STATS_NETIF_ADD(netif, ifoutoctets, p->tot_len);
    int unicast = ((p->payload[0] & 0x01) == 0);
    if (unicast) {
      MIB2_STATS_NETIF_INC(netif, ifoutucastpkts);
    } else {
      MIB2_STATS_NETIF_INC(netif, ifoutnucastpkts);
    }

    struct bfnetif_state *st = netif->state;
    struct bf_pbuf *bf_p = (struct bf_pbuf *)p;

    errval_t err;

    gensize_t offset = p->payload - defq_get_region_base(bf_p->region_id);


    err = devq_enqueue(st->q, bf_p->region_id, offset, p->len, 0, p->len, 0);
    if (err_is_fail(err)) {
        return ERR_IF;
    }

    return ERR_OK;
}

static void bfnetif_status_cb(struct netif *netif)
{
    printf("netif status changed %s\n", ip4addr_ntoa(netif_ip4_addr(netif)));
}


#include <net_interfaces/flags.h>

/**
 * @brief   polls a netif by polling the associated device queue
 *
 * @param netif the NETIF to be polled
 *
 * @return  ERR_OK on success, ERR_IF on interface error, ERR_MEM if it could
 *          not be allocated
 */
err_t bfnetif_poll(struct netif *netif)
{
    errval_t err;

    struct bfnetif_state *st = netif->state;

    struct devq_buf dbuf;

    for (int i = 0; i < BFNETIF_POLL_MAX; i++) {
        err = devq_dequeue(st->q, &dbuf->rid, &dbuf->offset, &dbuf->length,
                           &dbuf->valid_data, &dbuf->valid_length, &dbuf->flags);
        if (err_is_fail(err)) {
            return (err_no(err) == DEVQ_ERR_QUEUE_EMPTY) ? ERR_OK : ERR_IF;
        }

        if (dbuf->flags & NETIF_TXFLAG) {
            /* TODO: FREE THE BUFFER */
        }

        struct bf_pbuf *bf_p  = (struct bf_pbuf *)LWIP_MEMPOOL_ALLOC(RX_POOL);
        if (!bf_p) {
            return ERR_MEM;
        }

        bf_p->pb.custom_free_function = bfnetif_pbuf_free;
        bf_p->region_id = dbuf->rid;

        uint8_t *region_base = defq_get_region_base(dbuf->rid);

        /*
         * TODO: invalidate_cpu_cache(dma_desc->rx_data, dma_desc->rx_length);
         */

        struct pbuf* p;
        p = pbuf_alloced_custom(PBUF_RAW, dbuf->valid_length, PBUF_REF, &bf_p->pb,
                                region_base + dbuf->offset + dbuf->valid_data,
                                dbuf->length);

        if(netif->input(p, netif) != ERR_OK) {
            pbuf_free(p);
        }
    }

    return ERR_OK;
}


err_t bfnetif_init(struct netif *netif)
{
    assert(netif);

    netif->output     = etharp_output;
    netif->linkoutput = bfnetif_output;
    netif->mtu        = BFNETIF_MTU;
    netif->flags      = NETIF_FLAG_BROADCAST | NETIF_FLAG_ETHARP | NETIF_FLAG_ETHERNET | NETIF_FLAG_IGMP;

    MIB2_INIT_NETIF(netif, snmp_ifType_ethernet_csmacd, 100000000);

    /* TODO: get mac address */
    SMEMCPY(netif->hwaddr, your_mac_address_goes_here, sizeof(netif->hwaddr));
    netif->hwaddr_len = sizeof(netif->hwaddr);

    netif->name[0] = BFNETIF_IFNAME0;
    netif->name[1] = BFNETIF_IFNAME1;

    netif_set_status_callback(&netif, bfnetif_status_cb);
    //netif_set_default(&netif);

    netif_set_up(&netif);

    return ERR_OK;

    return ERR_MEM;
}

struct netif *bfnetif_create()
{
    return NULL;
}



void main(void)
{
  struct netif netif;

  lwip_init();

  netif_add(&netif, IP4_ADDR_ANY, IP4_ADDR_ANY, IP4_ADDR_ANY, NULL, netif_init, netif_input);

  /* Start DHCP and HTTPD */
  dhcp_init();
  httpd_init();

  while(1) {
    /* Check link state, e.g. via MDIO communication with PHY */
    if(link_state_changed()) {
      if(link_is_up()) {
        netif_set_link_up(&netif);
      } else {
        netif_set_link_down(&netif);
      }
    }

    /* Check for received frames, feed them to lwIP */
    lock_interrupts();
    struct pbuf* p = queue_try_get(&queue);
    unlock_interrupts();

    if(p != NULL) {
      LINK_STATS_INC(link.recv);

      /* Update SNMP stats (only if you use SNMP) */
      MIB2_STATS_NETIF_ADD(netif, ifinoctets, p->tot_len);
      int unicast = ((p->payload[0] & 0x01) == 0);
      if (unicast) {
        MIB2_STATS_NETIF_INC(netif, ifinucastpkts);
      } else {
        MIB2_STATS_NETIF_INC(netif, ifinnucastpkts);
      }

      if(netif.input(p, &netif) != ERR_OK) {
        pbuf_free(p);
      }
    }

    /* Cyclic lwIP timers check */
    sys_check_timeouts();

    /* your application goes here */
  }
}



