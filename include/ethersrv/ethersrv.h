/**
 * \file
 * \brief Header file for ethersrv.h
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ETHERSRV_H
#define ETHERSRV_H

#include <barrelfish/barrelfish.h>
#include <barrelfish/bulk_transfer.h>
#include <contmng/contmng.h>
#include <if/ether_defs.h>
#include <barrelfish/net_constants.h>

/*****************************************************************
 * Constants:
 *****************************************************************/
#define RECEIVE_BUFFER_SIZE 2048 // user provided memory



#define MAX_NR_TRANSMIT_PBUFS 80

#define RECEIVE_CONNECTION 0
#define TRANSMIT_CONNECTION 1

/*****************************************************************
 * common data-structures
 *****************************************************************/

#define MAX_PAUSE_BUFFER        10

struct bufdesc {
    char pkt_data[1600];
    size_t pkt_len;
};

struct filter {
    uint64_t filter_id;
    uint64_t filter_type;
    uint8_t *data;
    int32_t len;
    bool paused;
    struct bufdesc pause_buffer[MAX_PAUSE_BUFFER];
    int pause_bufpos;
    struct buffer_descriptor *buffer;
    struct filter *next;
};

struct buffer_descriptor {
    uint64_t buffer_id;
    struct ether_binding *con;
    struct capref cap;
    lpaddr_t pa;
    uint64_t bits;
    void *va;

    uint64_t type;
    void *pbuf_metadata_ds; // FIXME: why is it void *
    void *pbuf_metadata_ds_tx;
    uint32_t pbuf_head;
    uint32_t pbuf_head_msg;
    uint32_t pbuf_tail;
    uint32_t pbuf_head_tx;
    uint32_t pbuf_tail_tx;
    struct buffer_descriptor *next;
    struct filter* tx_filters;
};

struct pbuf {
    uint64_t buffer_id;
    uint64_t len;
    uint64_t offset;
    uint64_t client_data;
};


struct pbuf_desc {
    uint64_t buffer_id; // k: we are gonna reference this from now on
    uint64_t pbuf_id;
    uint64_t vaddr; // k: we need virtual memory from now on :)
    uint64_t paddr; // k: we also need to provide this for card for DMA
    uint64_t client_data;
    uint64_t len;
    uint64_t packet_size;
    struct ether_binding *sr;
    bool event_sent; //the interrupt handler has to know wheter the client was
    //already notified about new data in this buffer.
    bool last;
};

struct client_closure;

struct tx_pbuf {
    struct buffer_descriptor *buffer;
    struct ether_binding *sr;
    struct client_closure *cc;
    bool is_priv;
    bool can_send;
    bool last;
    uint64_t len;
    uint64_t offset;
    uint64_t client_data;
    uint64_t buffer_id;
};

/* This is client_closure for network service */
struct client_closure {
    int cl_no;
    struct buffer_descriptor *buffer_ptr;

    /* Following two are used by packet transmit logic */
    uintptr_t nr_transmit_pbufs; /*< how many pbufs belong to the packet to
                                        transmit now? */
    /* FIXME: following should be a number */
//    uintptr_t rtpbuf; ///< how many pbufs have we received so far?
    uint16_t rtpbuf; ///< how many pbufs have we received so far?
    struct tx_pbuf pbuf[MAX_NR_TRANSMIT_PBUFS];
    uint64_t tx_private_mem_v;
    uint64_t tx_private_mem_p;
    uint64_t head;
    uint64_t tail;
    uint64_t len;

    struct ether_binding *app_connection; /* FIXME: Do I need this? */
    struct cont_queue *q;
    uint8_t debug_state;
}; /* holds info about how much data is transferred to NIC. */


/*****************************************************************************
 * Driver states
 * **************************************************************************/


/***************************************************************************
 *  Following functions must be implemented by the driver which is using
 * the library.
 **************************************************************************/
typedef void (*ether_get_mac_address_t)(uint8_t *mac);

typedef errval_t (*ether_transmit_pbuf_list_t)(struct client_closure *closure);


/***************************************************************************/
void ethersrv_init(char *service_name,
		ether_get_mac_address_t get_mac_ptr,
		ether_transmit_pbuf_list_t transmit_ptr);

bool waiting_for_netd(void);


bool notify_client_free_tx(struct ether_binding *b, uint64_t client_data);


void process_received_packet(void *pkt_data, size_t pkt_len);

/* for frag.c */
bool handle_fragmented_packet(void* packet, size_t len);


struct filter *execute_filters(void *data, size_t len);

/* FIXME: put this into the local include file.  */
bool copy_packet_to_user(struct buffer_descriptor* buffer,
				void *data, uint64_t len);


//debug
void print_statistics(void);


#endif // ETHERSRV_H
