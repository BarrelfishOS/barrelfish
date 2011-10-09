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

#define TEST_BUFFER_MANAGEMENT      1

#ifdef TEST_BUFFER_MANAGEMENT
#define TEST_TYPE   "With BUFF Mng"
#else
#define TEST_TYPE   "Without BUFF Mng"
#endif // TEST_BUFFER_MANAGEMENT


static int connection_type = 0;  // 0 for using PBUF_POOL
//static int connection_type = 1;  // 1 for using PBUF_RAM

static uint64_t pkt_count = 0;
static uint64_t rx_data_size = 0;
static uint64_t recv_start_c = 0;
static uint64_t recv_stop_c = 0;

static uint64_t iterations = 2;

static struct waitset *ws = NULL;

static void
loop_forever(void)
{
    errval_t r;
    // Loop forever
    while (1) {
        r = event_dispatch(ws);
        if (err_is_fail(r)) {
            DEBUG_ERR(r, "in event_dispatch");
            break;
        }
    }
}

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

static uint64_t stats[10] = {0, 0, 0, 0};
static    uint64_t start = 0;
static    uint64_t iter = 0; // Iteration counter
static    uint64_t failed = 0; // Failure counter
static void stop_benchmark(void)
{
    // FIXME: Make sure that all data is gone
    uint64_t stop = rdtsc();
    uint64_t delta = stop - start;

    // sending debug message marking the stop of benchmark
    lwip_start_net_debug(connection_type, 2, 0);


    printf("Test [%s], PBUF type %s\n", TEST_TYPE,
            connection_type?"PBUF_POOL":"PBUF_RAM");
    printf("Time taken %"PU" to send %"PRIu64" packets"
            "(%"PRIu64" failed)\n",
            in_seconds(delta), iter, failed);
    uint64_t data_size = iter * MAX_DATA;
    printf("TX speed = data(%"PRIu64") / time(%"PU") = [%f] KB \n",
            data_size, in_seconds(delta), ((data_size/in_seconds(delta))/1024));
    for (int j = 0; j < 4; ++j) {
        printf("Stats  %d: [%"PRIu64"] \n", j, stats[j]);
    }

    loop_forever();
}

static void wait_for_lwip(void)
{
   errval_t r;
    int ans;
   while ( (ans = is_lwip_loaded()) > 0 ) {
        ++stats[ans];
/*        if(ans == 1) {
            printf("stopping the benchmark as no more pbufs\n");
            stop_benchmark();
        }
*/
        r = event_dispatch(ws);
        if (err_is_fail(r)) {
            DEBUG_ERR(r, "in event_dispatch");
            abort();
        }
   } // end while: lwip_loaded
   ++stats[ans];
} // end function: wait_for_lwip

static struct pbuf *
get_pbuf_wrapper(void)
{
    struct pbuf *p = NULL;
    if (connection_type == 1) {
        p = pbuf_alloc(PBUF_TRANSPORT, MAX_DATA, PBUF_RAM);
    } else {
        p = pbuf_alloc(PBUF_TRANSPORT, MAX_DATA, PBUF_POOL);
        // setting ref to zero as we are using it for sending and
        // not receiving
//        p->ref = 0;
//        pbuf_free(p);
    }
    if (p == NULL){
        printf("pbuf_alloc failed while counter %"PRIu16" \n",
                free_pbuf_pool_count());
    }
    assert(p != NULL);
    assert(p->payload != NULL);
    assert(p->len == p->tot_len);
    assert(p->len == MAX_DATA);
//    memset(p->payload, 'd', p->len);
    return p;

} // end function: get_pbuf_wrapper

static void
udp_sender(struct udp_pcb *upcb, struct ip_addr recv_ip,
        uint16_t recv_port)
{
    struct pbuf *p = NULL;
    printf("Going in UDP_SENDER mode\n");

    // connect with peer
    errval_t r = udp_connect(upcb, &recv_ip, recv_port);
    if (err_is_fail(r)) {
        DEBUG_ERR(r, "udp_connect:");
    }

#ifndef TEST_BUFFER_MANAGEMENT
    // create a pbuf
    printf("Testing without buffer manager\n");
    p = get_pbuf_wrapper();
    printf("pbuf len %"PRIu16", tot_len %"PRIu16"\n",
            p->len, p->tot_len);
    void *payload_ptr = p->payload;

    // Set the data to zero
    memset(p->payload, 'd', p->len);
#else
    printf("Testing *with* buffer manager!\n");
#endif // TEST_BUFFER_MANAGEMENT

    refresh_cache(&recv_ip);

    printf("Trying to send %"PRIu64" packets\n", iterations);
    // send data
    for (iter = 0; iter < iterations; ++iter) {
        wait_for_lwip();
        if (iter == 0) {
            // sending debug message marking the start of benchmark
            lwip_start_net_debug(connection_type, 1, 0);
            // sending first packet
            start = rdtsc();
        }

#ifdef TEST_BUFFER_MANAGEMENT
        p = get_pbuf_wrapper();
#else
        /* resetting the values as they will be changed by
         * pbuf_header function */
        p->len = MAX_DATA;
        p->tot_len = MAX_DATA;
        p->payload = payload_ptr;
#endif // TEST_BUFFER_MANAGEMENT

        r = udp_send(upcb, p);
        if (err_is_fail(r)) {
            ++failed;
            DEBUG_ERR(r, "udp_send:");
        } // end if: failed
//        printf("Sent packet no. %"PRIu64"\n", i);

#ifdef TEST_BUFFER_MANAGEMENT
        pbuf_free(p);
#endif // TEST_BUFFER_MANAGEMENT

    } // end for :

    stop_benchmark();
} // end function: udp_sender

static void
udp_recv_handler(void *arg, struct udp_pcb *pcb, struct pbuf *pbuf,
                    struct ip_addr *addr, u16_t port)
{
    assert(pbuf != NULL);
    assert(pbuf->payload != NULL);
    assert(pbuf->tot_len > 0);
    rx_data_size = rx_data_size + pbuf->tot_len;
    if(pkt_count == 0){
        // record starting time
        recv_start_c = rdtsc();
    }
    ++pkt_count;
    pbuf_free(pbuf);
} // end function: udp_recv_handler


static bool
condition_not_meet(void)
{
    if (rx_data_size < (iterations * MAX_DATA) ) {
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

    lwip_start_net_debug(connection_type, 1, iterations);
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
    uint64_t delta = recv_stop_c - recv_start_c;
    lwip_print_interesting_stats();
    // print the statistics
    printf("Time taken %"PU" to recv %"PRIu64" data"
            "(%"PRIu64" packets)\n", in_seconds(delta),
            rx_data_size, pkt_count);
    printf("RX speed = data(%"PRIu64") / time(%"PU") = [%f] KB \n",
           rx_data_size, in_seconds(delta),
           ((rx_data_size/in_seconds(delta))/1024));
} // end function: udp_receiver


int main(int argc, char *argv[])
{

    struct ip_addr peer_ip;  // IP address of peer
    uint16_t port = 0;  // Port number of the peer
    int as_sender = 1; // Flag to choose between sender(1) and receiver(0)

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

    if (lwip_init_auto() == false) {
        printf("ERROR: lwip_init_auto failed!\n");
        return 1;
    }

//    lwip_init("e1000");
    // create pcb for connection
    struct udp_pcb *upcb;
    upcb = udp_new();

    assert(upcb != NULL);

    printf("###############----------#######################\n");
    printf("%d.%d: Performing [%"PRIu64"] iterations\n",
                disp_get_core_id(), disp_get_domain_id(),
                iterations);


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

