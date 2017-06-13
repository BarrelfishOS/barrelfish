/**
 * \file ping.c
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


#include <barrelfish/barrelfish.h>
#include <barrelfish/deferred.h>

#include <lwip/ip.h>
#include <lwip/udp.h>
#include <lwip/pbuf.h>

#include <lwip/mem.h>
#include <lwip/raw.h>
#include <lwip/icmp.h>
#include <lwip/netif.h>
#include <lwip/sys.h>
#include <lwip/timeouts.h>
#include <lwip/inet_chksum.h>
#include <lwip/prot/ip4.h>

#include <net/net.h>

#define UDP_ECHOSERVER_PORT 7

static ip_addr_t ping_addr;
static uint16_t ping_port = UDP_ECHOSERVER_PORT;

static struct udp_pcb *ping_pcb;
static struct periodic_event ev;

#define PING_TIMEOUT   1000
#define PING_DELAY     1000
#define PING_ID        0xBFBF

uint16_t ping_data_size = 64;

static uint16_t ping_seq_num = 0;


struct result
{
    cycles_t t_start;
    cycles_t t_end;
    uint8_t received;
};

#define PING_RESULT_MAX 512

struct result results[PING_RESULT_MAX];




#include <barrelfish/sys_debug.h>
static cycles_t tsc_per_us = 0;
static inline uint64_t cycles_to_us(struct result *res)
{
    if (tsc_per_us == 0) {
        sys_debug_get_tsc_per_ms(&tsc_per_us);
        tsc_per_us /= 1000;
    }

    return (res->t_end - res->t_start) / (tsc_per_us);
}



static void
ping_recv(void *arg, struct udp_pcb *pcb, struct pbuf *p, const ip_addr_t *addr, uint16_t port)
{
    struct icmp_echo_hdr *iecho;

    if ((p->tot_len >= sizeof(struct icmp_echo_hdr) + ping_data_size)) {
        iecho = (struct icmp_echo_hdr *)p->payload;

        uint16_t seq = lwip_ntohs(iecho->seqno);

        if ((iecho->id == PING_ID) && (iecho->seqno == lwip_htons(ping_seq_num))) {
            if (results[seq & (PING_RESULT_MAX - 1)].t_end != 0) {
                debug_printf("t_end is non null???");
            }
            results[seq & (PING_RESULT_MAX - 1)].t_end = rdtsc();
            results[seq & (PING_RESULT_MAX - 1)].received = 1;

            char ip_str[16] = {0};

            strncpy(ip_str, ip4addr_ntoa(addr), sizeof(ip_str));

            printf("%u bytes from %s: icmp_seq=%u, ttl=%u time=%lu us (%lu)\n",
                    ping_data_size, ip_str, seq,
                    42, cycles_to_us(&results[seq & (PING_RESULT_MAX - 1)]),
                    results[seq & (PING_RESULT_MAX - 1)].t_end - results[seq & (PING_RESULT_MAX - 1)].t_start);
        }
    }

    pbuf_free(p);
}

static void
ping_send(struct udp_pcb *raw, ip_addr_t *addr)
{
  struct pbuf *p;
  struct icmp_echo_hdr *iecho;
  size_t ping_size = sizeof(struct icmp_echo_hdr) + ping_data_size;

  p = pbuf_alloc(PBUF_TRANSPORT, (u16_t)ping_size, PBUF_RAM);
  if (!p) {
    return;
  }
  if ((p->len == p->tot_len) && (p->next == NULL)) {
    iecho = (struct icmp_echo_hdr *)p->payload;

    ICMPH_TYPE_SET(iecho, ICMP_ECHO);
    ICMPH_CODE_SET(iecho, 0);
    iecho->chksum = 0;
    iecho->id     = PING_ID;
    iecho->seqno  = lwip_htons(++ping_seq_num);

    /* fill the additional data buffer with some data */
    for(uint16_t i = 0; i < ping_data_size; i++) {
      ((char*)iecho)[sizeof(struct icmp_echo_hdr) + i] = (char)i;
    }

    iecho->chksum = inet_chksum(iecho, ping_size);

    results[ping_seq_num & (PING_RESULT_MAX - 1)].t_end = 0;
    results[ping_seq_num & (PING_RESULT_MAX - 1)].received = 0;
    results[ping_seq_num & (PING_RESULT_MAX - 1)].t_start = rdtsc();

    udp_sendto(raw, p, addr, ping_port);
  }
  pbuf_free(p);
}

static void
ping_timeout(void *arg)
{
  struct udp_pcb *pcb = (struct udp_pcb*)arg;

  LWIP_ASSERT("ping_timeout: no pcb given!", pcb != NULL);

  if (!results[ping_seq_num & (PING_RESULT_MAX - 1)].received) {
      printf("Timeout.\n");
  }

  ping_send(pcb, &ping_addr);

  //sys_timeout(PING_DELAY, ping_timeout, pcb);
}


int main(int argc, char *argv[])
{
    errval_t err;

    debug_printf("PING started.\n");

    /* parse ip */

    if (argc < 2 || !ip4addr_aton(argv[1], &ping_addr)) {
        USER_PANIC("Invalid address supplied: %s\n", argv[1]);
    }

    if (argc == 3) {
        ping_port = atoi(argv[2]);
    }


    debug_printf("PING with IP %s:%u.\n", argv[1], ping_port);

    /* connect to the network */
    err = networking_init_default();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to initialize the network");
    }

    debug_printf("PING network initialized.\n");

    ping_pcb = udp_new();
    if (ping_pcb == NULL) {
        USER_PANIC("could not get a new raw pcb");
    }

    debug_printf("PING pcb created.\n");

    udp_bind(ping_pcb, IP_ADDR_ANY, UDP_ECHOSERVER_PORT);

    udp_recv(ping_pcb, ping_recv, NULL);

    err = networking_install_ip_filter(false, (ip_addr_t*) IP_ADDR_ANY, 
                                       0, UDP_ECHOSERVER_PORT);    
    if (err_is_fail(err)) {
        USER_PANIC("Adding filter failed %s \n", err_getstring(err));
    }

    err = periodic_event_create(&ev, get_default_waitset(),
                                (PING_DELAY * 1000),
                                MKCLOSURE(ping_timeout, (void*) ping_pcb));
    if (err_is_fail(err)) {
        USER_PANIC("Creating perodic event failed %s \n", err_getstring(err));
    }

    while(1) {
        networking_poll();
    }

    debug_printf("UDP ECHO termiated.\n");

    return 0;
}


