/**
 * \file main.c
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


/*
 * Copyright (c) 2001-2003 Swedish Institute of Computer Science.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT
 * SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
 * OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY
 * OF SUCH DAMAGE.
 *
 * This file is part of the lwIP TCP/IP stack.
 *
 * Author: Adam Dunkels <adam@sics.se>
 * RT timer modifications by Christiaan Simons
 */


#include <getopt.h>
#include <string.h>

#include "lwip/init.h"

#include "lwip/debug.h"

#include "lwip/mem.h"
#include "lwip/memp.h"
#include "lwip/sys.h"
#include "lwip/timeouts.h"

#include "lwip/stats.h"

#include "lwip/ip.h"
#include "lwip/ip4_frag.h"
#include "lwip/udp.h"
#include "lwip/tcp.h"
#include "netif/etharp.h"
#include "lwip/snmp.h"
#include "lwip/inet_chksum.h"
#include "lwip/apps/snmp.h"
#include "lwip/apps/snmp_mib2.h"

#include "../../src/apps/snmp_private_mib/private_mib.h"
#include "../../src/apps/udpecho_raw/udpecho_raw.h"
#include "../../src/apps/tcpecho_raw/tcpecho_raw.h"

/* (manual) host IP configuration */
static ip4_addr_t ipaddr, netmask, gw;

#if 0
/* SNMP trap destination cmd option */
static unsigned char trap_flag;
static ip_addr_t trap_addr;

static const struct snmp_mib *mibs[] = {
  &mib2,
  &mib_private
};
#endif

/* nonstatic debug cmd option, exported in lwipopts.h */
unsigned char debug_flags;

#if LWIP_SNMP
/* enable == 1, disable == 2 */
u8_t snmpauthentraps_set = 2;
#endif

static struct option longopts[] = {
  /* turn on debugging output (if build with LWIP_DEBUG) */
  {"debug", no_argument,        NULL, 'd'},
  /* help */
  {"help", no_argument, NULL, 'h'},
  /* gateway address */
  {"gateway", required_argument, NULL, 'g'},
  /* ip address */
  {"ipaddr", required_argument, NULL, 'i'},
  /* netmask */
  {"netmask", required_argument, NULL, 'm'},
  /* ping destination */
  {"trap_destination", required_argument, NULL, 't'},
  /* new command line options go here! */
  {NULL,   0,                 NULL,  0}
};
#define NUM_OPTS ((sizeof(longopts) / sizeof(struct option)) - 1)

static void
usage(void)
{
  unsigned char i;

  printf("options:\n");
  for (i = 0; i < NUM_OPTS; i++) {
    printf("-%c --%s\n",longopts[i].val, longopts[i].name);
  }
}

#include <lwip/pbuf.h>
#include <lwip/prot/ethernet.h>
#include <lwip/prot/ip4.h>
#include <lwip/prot/udp.h>

static void devq_poll(struct netif *netif)
{

    /* if has foobar packet */

    printf("%s:%u\n", __FUNCTION__, __LINE__);

    /* get the pbuf of packet */
    struct pbuf *p = NULL;

    p = pbuf_alloc(PBUF_RAW, 2048, PBUF_POOL);

    struct eth_hdr* ethhdr = p->payload;
    struct ip_hdr *iphdr   = p->payload + SIZEOF_ETH_HDR;
    struct udp_hdr *udphdr = p->payload  + SIZEOF_ETH_HDR + IP_HLEN;

    memset(ethhdr->dest.addr, 0xaa, sizeof(ethhdr->dest.addr));
    memset(ethhdr->src.addr, 0xbb, sizeof(ethhdr->src.addr));
    ethhdr->type = PP_HTONS(ETHTYPE_IP);

    iphdr->_len = lwip_htons(1024);
    iphdr->_v_hl = (4 << 4) | 5;


    IP4_ADDR(&(iphdr->dest), 192,168,0,2);
    IP4_ADDR(&(iphdr->src), 192,168,0,1);

    iphdr->dest.addr = 0xdeadbeef;
    iphdr->_proto = IP_PROTO_UDP;
    iphdr->_chksum = inet_chksum(iphdr, 1024);

    udphdr->dest = PP_HTONS(7);
    udphdr->src = PP_HTONS(11);
    udphdr->len = PP_HTONS(1004);

    printf("%s:%u pbuf=%p payload=%p, len=%u, totlen=%u\n", __FUNCTION__, __LINE__,
            p, p->payload, p->len, p->tot_len);

    /* inet input */
    netif_input(p, netif);
}

// 10MBit interface
#define BFETH_NETSPEED  10000000

/* Define those to better describe your network interface. */
#define IFNAME0 'e'
#define IFNAME1 'n'

static err_t devq_send(struct netif *netif, struct pbuf *p)
{
    printf("%s:%u pbuf=%p payload=%p, len=%u, totlen=%u\n", __FUNCTION__, __LINE__,
            p, p->payload, p->len, p->tot_len);

    return ERR_OK;
}

static err_t devq_netif_init_fn(struct netif *netif)
{
    /* maximum transfer unit */
    netif->mtu = 1500;

    /* device capabilities */
    netif->flags = // NETIF_FLAG_ETHERNET |
      NETIF_FLAG_BROADCAST | NETIF_FLAG_ETHARP | NETIF_FLAG_LINK_UP;

    /* set MAC hardware address length */
    netif->hwaddr_len = ETHARP_HWADDR_LEN;

#if LWIP_NETIF_HOSTNAME
    /* Initialize interface hostname */
    netif->hostname = "lwip";
#endif

    netif->name[0] = IFNAME0;
    netif->name[1] = IFNAME1;

    /*
     * Initialize the snmp variables and counters inside the struct netif.
     * The last argument should be replaced with your link speed, in units
     * of bits per second.
     */
    NETIF_INIT_SNMP(netif, snmp_ifType_ethernet_csmacd, BFETH_NETSPEED);

    /* We directly use etharp_output() here to save a function call.
     * You can instead declare your own function an call etharp_output()
     * from it if you have to do some checks before sending (e.g. if link
     * is available...) */
    netif->output = etharp_output;

    netif->linkoutput = devq_send;

    return ERR_OK;
}



int
main(int argc, char **argv)
{

    printf("lwip min started\n");

    printf("lwipmin:%s:%u\n", __FUNCTION__, __LINE__);


  struct netif netif;
  int ch;
  char ip_str[16] = {0}, nm_str[16] = {0}, gw_str[16] = {0};

  /* startup defaults (may be overridden by one or more opts) */
  IP4_ADDR(&gw, 192,168,0,1);
  IP4_ADDR(&ipaddr, 192,168,0,2);
  IP4_ADDR(&netmask, 255,255,255,0);

#if 0
  trap_flag = 0;
#endif
  /* use debug flags defined by debug.h */
  debug_flags = LWIP_DBG_OFF;

  printf("lwipmin:%s:%u\n", __FUNCTION__, __LINE__);

  while ((ch = getopt_long(argc, argv, "dhg:i:m:t:", longopts, NULL)) != -1) {
    switch (ch) {
      case 'd':
        debug_flags |= (LWIP_DBG_ON|LWIP_DBG_TRACE|LWIP_DBG_STATE|LWIP_DBG_FRESH|LWIP_DBG_HALT);
        break;
      case 'h':
        usage();
        exit(0);
        break;
      case 'g':
        ip4addr_aton(optarg, &gw);
        break;
      case 'i':
        ip4addr_aton(optarg, &ipaddr);
        break;
      case 'm':
        ip4addr_aton(optarg, &netmask);
        break;
      case 't':
#if 0
        trap_flag = !0;
        /* @todo: remove this authentraps tweak
          when we have proper SET & non-volatile mem */
        snmpauthentraps_set = 1;
        ipaddr_aton(optarg, &trap_addr);
        strncpy(ip_str, ipaddr_ntoa(&trap_addr),sizeof(ip_str));
        printf("SNMP trap destination %s\n", ip_str);
#endif
        break;
      default:
        usage();
        break;
    }
  }

  printf("lwipmin:%s:%u\n", __FUNCTION__, __LINE__);

  argc -= optind;
  argv += optind;

  printf("lwipmin:%s:%u\n", __FUNCTION__, __LINE__);

  strncpy(ip_str, ip4addr_ntoa(&ipaddr), sizeof(ip_str));
  strncpy(nm_str, ip4addr_ntoa(&netmask), sizeof(nm_str));
  strncpy(gw_str, ip4addr_ntoa(&gw), sizeof(gw_str));
  printf("Host at %s mask %s gateway %s\n", ip_str, nm_str, gw_str);


#ifdef PERF
  perf_init("/tmp/minimal.perf");
#endif /* PERF */

  printf("lwipmin:%s:%u\n", __FUNCTION__, __LINE__);

  lwip_init();

  printf("TCP/IP initialized.\n");

  netif_add(&netif, &ipaddr, &netmask, &gw, NULL, devq_netif_init_fn, ethernet_input);
  netif_set_default(&netif);
  netif_set_up(&netif);

  printf("lwipmin:%s:%u\n", __FUNCTION__, __LINE__);

#if LWIP_IPV6
  netif_create_ip6_linklocal_address(&netif, 1);
#endif

  printf("lwipmin:%s:%u\n", __FUNCTION__, __LINE__);

#if 0
  /* initialize our private example MIB */
  lwip_privmib_init();

  /* snmp_trap_dst_ip_set(0,&trap_addr); */
  /* snmp_trap_dst_enable(0,trap_flag); */

#if SNMP_LWIP_MIB2
#if SNMP_USE_NETCONN
  snmp_threadsync_init(&snmp_mib2_lwip_locks, snmp_mib2_lwip_synchronizer);
#endif
  snmp_mib2_set_syscontact_readonly((const u8_t*)"root", NULL);
  snmp_mib2_set_syslocation_readonly((const u8_t*)"lwIP development PC", NULL);
  snmp_mib2_set_sysdescr((const u8_t*)"minimal example", NULL);
#endif /* SNMP_LWIP_MIB2 */

  /* snmp_set_snmpenableauthentraps(&snmpauthentraps_set); */
  snmp_set_mibs(mibs, LWIP_ARRAYSIZE(mibs));
  snmp_init();
#endif /* LWIP_SNMP */

  printf("lwipmin:%s:%u\n", __FUNCTION__, __LINE__);

  udpecho_raw_init();

  printf("lwipmin:%s:%u\n", __FUNCTION__, __LINE__);

  tcpecho_raw_init();



  printf("Applications started.\n");


  for(int i = 0; i < 10; i++) {
    /* poll netif, pass packet to lwIP */
    devq_poll(&netif);

    sys_check_timeouts();
  }

  return 0;
}
