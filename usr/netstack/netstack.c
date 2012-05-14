/** \file
 *  \brief Example lwip socket application
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/waitset.h>

#include "netg/netg.h"


static char test_string[] = "sbfbfbfbfbfbfbfbfbfbfbfbfbfbfbfbfe";

static void my_udp_recv(void *arg, struct netg_endpoint* uendpoint, struct netg_buffer* p, ip_addr_t addr, uint16_t port) {
	printf("UDP1: got something from the stack! header length %hu data length %hu\n", p->header_length, p->length);
	printf("[%s]\n", (char*)p->payload);
	netg_free_buffer(p);

	struct netg_buffer* buf = netg_get_buffer(uendpoint);
	memcpy(buf->payload, test_string, strlen(test_string));
	buf->length = strlen(test_string);
	printf("UDP: replying to unconnected endpoint\n");
	udp_sendto(uendpoint, buf, addr, port);
}

static void my_udp_recv2(void *arg, struct netg_endpoint* uendpoint, struct netg_buffer* p, ip_addr_t addr, uint16_t port) {
	printf("UDP2: got something from the stack comes from %x %hu!\n", addr, port);
	printf("[%s]\n", (char*)p->payload);
	netg_free_buffer(p);

	struct netg_buffer* buf = netg_get_buffer(uendpoint);
	memcpy(buf->payload, test_string, strlen(test_string));
	buf->length = strlen(test_string);
	printf("UDP: replying to unconnected endpoint\n");
	udp_sendto(uendpoint, buf, addr, port);

	/*

	if(uendpoint->status == NETG_ENDPOINT_CLOSED) {
		printf("UDP: connect to it and send a packet\n");
		udp_connect(uendpoint, addr, port);

		struct netg_buffer* buf = netg_get_buffer(uendpoint);
		memcpy(buf->payload, test_string, strlen(test_string));
		buf->length = strlen(test_string);
		printf("UDP: replying to connected endpoint\n");
		udp_send(uendpoint, buf);
	}*/
}
/*
int rcvd = 0;
int sent = 0;

static errval_t my_recv_server (void *arg, struct netg_endpoint* endpoint, struct netg_buffer * p, errval_t err) {
	printf("TCP: got %hu bytes of data from client, %i\n", p->length, ++rcvd);
	//printf("[%s]\n", (char*)p->payload);

	netg_free_buffer(p);

	if(rcvd == sent) {
		tcp_close(endpoint);
	}

	//tcp_write(endpoint, test_string, strlen(test_string), 0);
	//tcp_close(endpoint);

	return SYS_ERR_OK;
}*/

static errval_t my_recv_client (void *arg, struct netg_endpoint* endpoint, struct netg_buffer * p, errval_t err) {
	printf("TCP: got %hu bytes of data from server\n", p->length);
	printf("[%s]\n", (char*)p->payload);

	netg_free_buffer(p);

	tcp_write(endpoint, test_string, strlen(test_string), 0);
	//tcp_close(endpoint);

	return SYS_ERR_OK;
}
/*
static errval_t my_accept(void *arg, struct netg_endpoint* endpoint, errval_t err) {
	printf("TCP: got a new connection request\n");

	tcp_recv(endpoint, my_recv_server);

	return SYS_ERR_OK;
}*/

static errval_t my_connected(void *arg, struct netg_endpoint* endpoint, errval_t err) {
	printf("TCP: connected to server\n");

	tcp_recv(endpoint, my_recv_client);

	tcp_write(endpoint, test_string, strlen(test_string), 0);

	return SYS_ERR_OK;
}

int main(int argc, char *argv[])
{
    coreid_t mycore = disp_get_core_id();
	errval_t err;
	struct waitset *ws = get_default_waitset();

    printf("[%d]main(): This is %s on core %d with %d args\n",
            disp_get_domain_id(), argv[0], mycore, argc);

    netg_init("lo");

    for(int i=0;i<1024*1024;i++) {
		err = event_dispatch_non_block(ws);
		if (err_is_fail(err) && err != LIB_ERR_NO_EVENT) {
			DEBUG_ERR(err, "in event_dispatch");
			break;
		}
    }

    struct netg_endpoint* nept = udp_new();
	udp_bind(nept, IP4_ADDR_LOOPBACK, 12345);
	udp_recv(nept, my_udp_recv, NULL);


	struct netg_endpoint* nept2 = udp_new();
	udp_bind(nept2, IP4_ADDR_LOOPBACK, 1212);
	udp_recv(nept2, my_udp_recv2, NULL);


	printf("finished setting up endpoint\n");
	struct netg_buffer* buf = netg_get_buffer(nept);
	memcpy(buf->payload, test_string, strlen(test_string));
	buf->length = strlen(test_string);
	printf("start sending buffers to loopback\n");
	udp_sendto(nept, buf, IP4_ADDR_LOOPBACK, 1212);
	netg_free_buffer(buf);



	printf("\nTCP start\n");
	/*struct netg_endpoint* tcp_server = tcp_new();
	tcp_bind(tcp_server, 0, 5050);
	tcp_listen(tcp_server);
	tcp_accept(tcp_server, my_accept);*/
/*
	struct netg_endpoint* tcp_client = tcp_new();
	tcp_bind(tcp_client, 0x0a00020f, 7070);
	tcp_connect(tcp_client, 0xc0a8488F, 5050, my_connected);
*/

	if(1==0) {
	struct netg_endpoint* tcp_client = tcp_new();
	tcp_bind(tcp_client, IP4_ADDR_LOOPBACK, 7070);
	tcp_connect(tcp_client, IP4_ADDR_LOOPBACK, 5050, my_connected);
	}
	// loop forever

	while (1) {
		netg_tick();
		//print_tcp_status(tcp_server);
		//print_tcp_status(tcp_client);

		err = event_dispatch(ws);
		if (err_is_fail(err) && err != LIB_ERR_NO_EVENT) {
			DEBUG_ERR(err, "in event_dispatch");
			break;
		}
	}

    return EXIT_SUCCESS;
} // end function : main
