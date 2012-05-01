/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef TCP_SERVER_BM_H_
#define TCP_SERVER_BM_H_

// Initializes the tcp server benchmark
// essentially it starts listening on given port
int tcp_server_bm_init(uint16_t bind_port);


// initialize tcp connection for the client
int tcp_client_bm_init(char *ip_addr_str,  uint16_t server_port);

// send single message over TCP connection
int send_message_client(void *msg, size_t len);


// to be called from tcp_server.c code
// Informs the benchmarking code about arrival of data
void handle_data_arrived(char *payload, size_t data_len);
#endif // TCP_SERVER_BM_H_

