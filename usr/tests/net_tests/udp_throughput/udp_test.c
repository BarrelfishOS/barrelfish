/**
 * \file
 * \brief simple udp benchmark
 */

/*
 * Copyright (c) 2007-11 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/nameservice_client.h>
#include <stdio.h>
#include <lwip/pbuf.h>
#include <lwip/udp.h>
#include <lwip/init.h>
#include <netif/etharp.h>
//#define TOTAL_DATA_SIZE  629188608
#define MAX_DATA   1330
#define MULTIPLIER 10

static uint64_t pkt_count = 0;
static uint64_t data_size = 0;
static uint64_t recv_start_c = 0;
static uint64_t recv_stop_c = 0;

static uint64_t iterations = 2;

static struct waitset *ws = NULL;

static void
refresh_cache(struct ip_addr *dst_ip)
{
    struct netif *netif;
    netif = ip_route(dst_ip);

    errval_t r = etharp_request(netif, dst_ip);
    assert(err_is_ok(r));

   while (is_ip_present_in_arp_cache(dst_ip) == false) {
        r = event_dispatch(ws);
        if (err_is_fail(r)) {
            DEBUG_ERR(r, "in event_dispatch");
            abort();
        }
   } // end while: till arp not present
}

static void wait_for_lwip(void)
{
   errval_t r;

   while (is_lwip_loaded()) {
        r = event_dispatch(ws);
        if (err_is_fail(r)) {
            DEBUG_ERR(r, "in event_dispatch");
            abort();
        }
   } // end while: lwip_loaded
} // end function: wait_for_lwip


static void
udp_sender(struct udp_pcb *upcb, struct ip_addr recv_ip,
        uint16_t recv_port)
{
    printf("Going in UDP_SENDER mode\n");

    // connect with peer
    errval_t r = udp_connect(upcb, &recv_ip, recv_port);
    if (err_is_fail(r)) {
        DEBUG_ERR(r, "udp_connect:");
    }

    // create a pbuf
    struct pbuf *p = pbuf_alloc(PBUF_TRANSPORT, MAX_DATA, PBUF_POOL);
    assert(p != NULL);
    printf("pbuf len %"PRIu16", tot_len %"PRIu16"\n",
            p->len, p->tot_len);
    assert(p->len == p->tot_len);
    assert(p->len == MAX_DATA);
    assert(p->payload != NULL);
    void *payload_ptr = p->payload;

    // Set the data to zero
    memset(p->payload, 'd', p->len);

    refresh_cache(&recv_ip);
    // take a time-stamp
    uint64_t start = rdtsc();

    printf("Trying to send %"PRIu64" packets\n", iterations);
    uint64_t i = 0; // Iteration counter
    uint64_t failed = 0; // Failure counter
    // send data
    for (i = 0; i < iterations; ++i) {
        wait_for_lwip();
        /* resetting the values as they will be changed by
         * pbuf_header function */
        p->len = MAX_DATA;
        p->tot_len = MAX_DATA;
        p->payload = payload_ptr;
        r = udp_send(upcb, p);
        if (err_is_fail(r)) {
            ++failed;
            DEBUG_ERR(r, "udp_send:");
/*
            printf("udp_send failed: for %"PRIu64" packet\n",
                    i);
*/
        } // end if: failed
//        printf("Sent packet no. %"PRIu64"\n", i);
    } // end for :

    // FIXME: Make sure that all data is gone
    uint64_t stop = rdtsc();

    printf("Cycles taken %"PRIu64" to send %"PRIu64" packets"
            "(%"PRIu64" failed)\n",
            (stop - start), i, failed);
} // end function: udp_sender


static void
udp_recv_handler(void *arg, struct udp_pcb *pcb, struct pbuf *pbuf,
                    struct ip_addr *addr, u16_t port)
{
    assert(pbuf != NULL);
    assert(pbuf->payload != NULL);
    assert(pbuf->tot_len > 0);
    data_size = data_size + pbuf->tot_len;
    if(pkt_count == 0){
        // record starting time
        recv_start_c = rdtsc();
    }
    ++pkt_count;
} // end function: udp_recv_handler


static bool
condition_not_meet(void)
{
    if (data_size < (iterations * MAX_DATA) ) {
        return true;
    } else {
        return false;
    }
} // end function: condition_not_meet

static void
udp_receiver(struct udp_pcb *upcb, struct ip_addr *listen_ip,
        uint16_t listen_port)
{
    printf("Going in UDP_RECEIVER mode\n");
    // Bind to specified port
    errval_t r = udp_bind(upcb, listen_ip, listen_port);
    if (err_is_fail(r)) {
        DEBUG_ERR(r, "udp_bind:");
    }

    udp_recv(upcb, udp_recv_handler, 0 /*client data, arg in callback*/);

    while (condition_not_meet()) {
        r = event_dispatch(ws);
        if (err_is_fail(r)) {
            DEBUG_ERR(r, "in event_dispatch");
            break;
        }
    }
    // Record the stop timer
    recv_stop_c = rdtsc();

    // print the statistics
    printf("Time taken %"PRIu64" to recv %"PRIu64" data"
            "(%"PRIu64" packets)\n", (recv_stop_c - recv_start_c),
            data_size, pkt_count);

}// end function: udp_receiver


int main(int argc, char *argv[])
{

    struct ip_addr peer_ip;  // IP address of peer
    uint16_t port = 0;  // Port number of the peer
    int as_sender = 1; // Flag to choose between sender and receiver

    ws = get_default_waitset();

     // Parse args
    if (argc != 4) {
        printf("Usage: %s IP Port packets\n", argv[0]);
        return 1;
    }

    struct in_addr peer_ip_gen;
    int ret = inet_aton(argv[1], &peer_ip_gen);
    if (ret == 0) {
        printf("Invalid IP addr: %s\n", argv[1]);
        return 1;
    } // end if : ip validation
    peer_ip.addr = peer_ip_gen.s_addr;

    port = atoi(argv[2]);
    if (port <= 0) {
        printf("Invalid port given [%s] == [%"PRIu16"]\n",
                argv[2], port);
        return 1;
    } // end if : port validation

    iterations = atoi(argv[3]);
    if (iterations <= 0) {
        printf("Invalid no. of iterations [%s] == [%"PRIu64"]\n",
                argv[3], iterations);
        return 1;
    } // end if : port validation
    iterations = iterations * MULTIPLIER;

    printf("Performing [%"PRIu64"] iterations\n",
                iterations);

    if (lwip_init_auto() == false) {
        printf("ERROR: lwip_init_auto failed!\n");
        return 1;
    }

    // create pcb for connection
    struct udp_pcb *upcb;
    upcb = udp_new();

    assert(upcb != NULL);

    if(as_sender == 1) {
        udp_sender(upcb, peer_ip, port);
    } else {
        udp_receiver(upcb, IP_ADDR_ANY, port);
    } // end else:


    printf("Init finished.\n");

    while (1) {
        errval_t r = event_dispatch(ws);
        if (err_is_fail(r)) {
            DEBUG_ERR(r, "in event_dispatch");
            break;
        }
    }

    udp_remove(upcb);
} // end function: main

