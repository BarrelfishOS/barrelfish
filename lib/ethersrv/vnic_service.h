/**
 * \file
 * \brief Header file for vnic_service.c
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef VNIC_SERVICE_H
#define VNIC_SERVICE_H

#include <contmng/contmng.h>

#define CLINE_SIZE      (1 << 6)
#define VNIC_MAGIC_COOKI            0x564DC12

typedef uint32_t uoffset_t;
typedef uint32_t upbuf_len_t;

struct CACHELINE {
    uint8_t bytes[CLINE_SIZE];
};

/* Single slot in ring buffer.
 * Each slot will hold single pbuf entry
 * It should fit into single cacheline. */
struct ring_slot {
    uint16_t buf_id;            // buf_id tells which buffer this pbuf belongs
    uoffset_t offset;           // offset within above buffer where pbuf starts
    upbuf_len_t len;            // length of pbuf starting from offset
    uint8_t flags;              // flags telling if there are more pbufs coming
};

struct vreg {
    union {
        uint32_t value;         // The register content
        struct CACHELINE place_holder;  // to make it cacheline aligned
    } cline;
};


struct buf_ring {
    struct ring_slot *slots;    // Array of ring slots
    struct vreg *read;          // The read index
    struct vreg *write;         // The write index
    size_t ring_size;           // Size of the ring
};


enum v_nic_states {
    VNIC_INVALID = 0,
    VNIC_ERROR = 1,
    VNIC_INITIALIZED = 2,
    VNIC_READY = 3,
    VNIC_STOPPED = 4,
    VNIC_PAUSED = 5,
    VNIC_RUNNING = 6,
};


// Virtual NIC device that will be exclusively used by the application
struct v_nic {
    uint64_t id;                // NIC id
    uint64_t magic_cooki;       // magic no. to ensure that this is indeed vnic
    enum v_nic_states state;    // State of the NIC
    struct cont_queue *q;       // Queue to manage continuation callbacks
    struct buffer_desc *buffer_list;    // List of all buffers registered with this vNIC
    struct capref mem_cap;      // shared mem where vregs and buffer rings are kept
    struct buf_ring *RX_ring;
    struct buf_ring *TX_ring;
    uint32_t ip_addr;           // IP address for this V_NIC
    uint64_t mac_addr;          // MAC address of this V_NIC
    uint8_t debug_state;
};


void VNIC_service_init(char *service_name);


#endif                          // VNIC_SERVICE_H
