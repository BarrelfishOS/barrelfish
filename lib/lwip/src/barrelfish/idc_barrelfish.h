/**
 * \file
 * \brief Header file for the interfaceing part to the network driver
 *
 *
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef IDC_BARRELFISH_H_
#define IDC_BARRELFISH_H_

#include <barrelfish/barrelfish.h>
#include <lwip/ip_addr.h>

/**
 * \brief Receive and transmit sides
 *
 * The link to the network driver is composed by two distinct
 * channel. We identify these channels thanks to the following
 * constants.
 *
 */
#define RECEIVE_CONNECTION 0
#define TRANSMIT_CONNECTION 1


struct buffer_desc {
    struct capref cap;
    struct ether_binding *con;
    lpaddr_t pa;
    void *va;
    size_t size;
    uint64_t buffer_id; //as assigned by the network driver on registering
    struct buffer_desc *next;
};


void idc_connect_to_netd(char *server_name);
void idc_connect_to_driver(char *card_name);

uint64_t idc_send_packet_to_network_driver(struct pbuf *p);
void idc_register_buffer(struct buffer_desc *buff_ptr, uint8_t binding_index);
void idc_get_mac_address(uint8_t *mac);
int idc_check_capacity(int direction);
uint64_t idc_check_driver_load(void);
uint64_t idc_get_packet_drop_count(void);

void idc_register_receive_callback(void (*f)
                                        (void*, uint64_t, uint64_t,
                                        uint64_t, uint64_t),
                                        void *data);
uint64_t idc_get_next_packet(uint8_t *packet);
void idc_register_pbuf(uint64_t pbuf_id, uint64_t paddr, uint64_t len);
void idc_register_freeing_callback(void (*f)(struct pbuf*));
void idc_print_statistics(void);
void idc_print_cardinfo(void);
void network_polling_loop(void);
void idc_debug_status(uint8_t state);

/* netd services */
void idc_connect_netd(void);

void idc_just_to_test(void);

void idc_get_ip(void);
err_t idc_tcp_new_port(uint16_t *port_no);
err_t idc_udp_new_port(uint16_t *port_no);
err_t idc_redirect_tcp(struct ip_addr *local_ip,
                   uint16_t local_port, struct ip_addr *remote_ip,
                   uint16_t remote_port);
err_t idc_close_udp_port(uint16_t port);
err_t idc_close_tcp_port(uint16_t port);
err_t idc_bind_udp_port(uint16_t port);
err_t idc_bind_tcp_port(uint16_t port);
err_t idc_pause_tcp(struct ip_addr *local_ip, u16_t local_port,
                    struct ip_addr *remote_ip, u16_t remote_port);

#endif // IDC_BARRELFISH_H_
