/**
 * @brief 
 *  udp_echo.c
 */

/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>

#include <lwip/ip.h>
#include <lwip/udp.h>
#include <lwip/pbuf.h>
#include <net/net.h>

#define UDP_ECHOSERVER_PORT 7

static void echo_recv_handler(void *arg, struct udp_pcb *upcb, struct pbuf *p,
                              const ip_addr_t *addr, uint16_t port)
{
    debug_printf("UDP ECHO received packet\n");
    udp_sendto(upcb, p, addr, port);
    pbuf_free(p);
}

int main(int argc, char *argv[])
{
    errval_t err;

    debug_printf("UDP ECHO started.\n");

    /* connect to the network */
    err = networking_init_default();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to initialize the network");
    }

    debug_printf("UDP ECHO network initialized.\n");

    //create a new UDP PCB
    struct udp_pcb *pcb = udp_new(); //UDP connection data
    if (pcb == NULL) {
        return ERR_MEM;
    }

    debug_printf("UDP ECHO pcb created.\n");

    err_t r = udp_bind(pcb, IP_ADDR_ANY, UDP_ECHOSERVER_PORT);
    if(r != ERR_OK) {
        udp_remove(pcb);
        return(r);
    }

    debug_printf("UDP ECHO bound to UDP port %u.\n", UDP_ECHOSERVER_PORT);

    udp_recv(pcb, echo_recv_handler,  0);


    debug_printf("UDP ECHO start receiving messages\n");


    for (int i = 0; i < 100; i++) {
        networking_poll();
    }

    debug_printf("UDP ECHO termiated.\n");

    return 0;
}


