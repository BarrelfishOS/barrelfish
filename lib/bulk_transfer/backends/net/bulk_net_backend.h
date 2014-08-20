/**
 * \file
 * \brief Unidirectional bulk data transfer via shared memory
 */

/*
 * Copyright (c) 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BULK_NET_BACKEND_H
#define BULK_NET_BACKEND_H

#include <if/e10k_defs.h>
#include <dev/e10k_dev.h>

#include <bulk_transfer/bulk_net_proxy.h>

#include "e10k_queue.h"


/*
 * DEFINES FOR DEBUG OUTPUT
 */
/// enables/disables the entire debug outputs of the bulk net backend
#define BULK_NET_ENABLE_DEBUG 1

/// enables/disables status messages
#define BULK_NET_ENABLE_STATUS 1

/// enables/disables the tracing debug output
#define BULK_NET_ENABLE_TRACE 0

/// enables/disables the debug output for the e10k module
#define BULK_NET_ENABLE_DEBUG_E10K 0
#define BULK_NET_ENABLE_STATUS_E10K 0

/// enables/disables the debug output for the transfer module
#define BULK_NET_ENABLE_DEBUG_TRANSF 0
#define BULK_NET_ENABLE_STATUS_TRANSF 0

/// enables/disables the debug output for the backend module
#define BULK_NET_ENABLE_DEBUG_BACKEND 1
#define BULK_NET_ENABLE_STATUS_BACKEND 1

#if BULK_NET_ENABLE_DEBUG
#define BULK_NET_DEBUG(fmt, msg...) debug_printf("%s(): "fmt"\n", __func__,  msg);
#else
#define BULK_NET_DEBUG(x...) do {} while(0);
#endif

#if BULK_NET_ENABLE_STATUS
#define BULK_NET_STATUS(fmt, msg...) debug_printf("%s(): "fmt"\n", __func__,  msg);
#else
#define BULK_NET_STATUS(x...) do {} while(0);
#endif

#if BULK_NET_ENABLE_DEBUG && BULK_NET_ENABLE_TRACE
#define BULK_NET_TRACE debug_printf("%s\n", __func__);
#else
#define BULK_NET_TRACE do{} while(0);
#endif


/*
 * the following values are used in the endpoint creation
 */
#define BULK_NET_DEFAULT_BUFFER_SIZE 0x1000
#define BULK_NET_DEFAULT_BUFFER_COUNT 0xFF
#define BULK_NET_NOCOPY_SPARE_BUFFERS 1.5
#define BULK_NET_DEFAULT_QUEUES 2

#define BULK_NET_NOCOPY_META_BUFFER_SIZE 512

#define BULK_NET_TRANSFER_NUM_DESCS 1024

#define BULK_NET_TRANSFER_DESCLEN 4

#define BULK_NET_INTERNAL_BUFER_SIZE 512

struct bulk_implementation *bulk_net_get_impl(void);
struct bulk_implementation *bulk_net_get_impl_no_copy(void);



/// switch to turn on message dumping
#define DO_MSG_DUMP 0


#define BULK_NET_DESCLEN 4

struct receive_buffer {
    void     *hdr_virt;
    uintptr_t hdr_phys;

    void     *virt;
    uintptr_t phys;

    struct bulk_buffer *buffer;
    bool                is_meta;
};

#define INT_BUFSZ 512
struct transmit_buffer {
    void     *hdr_virt;
    uintptr_t hdr_phys;

    bool is_copy;
    struct bulk_buffer *buffer;
    struct bulk_continuation cont;

    void *int_virt;
    uintptr_t int_phys;
};


struct packet_header {
    struct {
        uint8_t dmac[6];
        uint8_t smac[6];
        uint16_t type;
    } __attribute__((packed)) l2;
    struct {
        uint8_t ver_ihl;
        uint8_t dscp;
        uint16_t len;
        uint16_t id;
        uint16_t offset;
        uint8_t ttl;
        uint8_t proto;
        uint16_t checksum;
        uint32_t s_ip;
        uint32_t d_ip;
    } __attribute__((packed)) l3;
    struct {
        uint16_t s_port;
        uint16_t d_port;
        uint16_t len;
        uint16_t checksum;
    } __attribute__((packed)) l4;
} __attribute__((packed));


/**
 * Descriptor for passing around buffer chains with a reasonable length. Note
 * that only the parts up to the first one with size == 0 are considered.
 */
struct bulk_net_msgdesc {
    struct {
        uint64_t phys;
        size_t   size;
        void    *opaque;
    } parts[BULK_NET_DESCLEN];
};


#define E10K_HDRSZ 128
#define E10K_DESCSZ (sizeof(e10k_q_tdesc_adv_wb_array_t))


void stack_alloc_init(struct stack_allocator *alloc, size_t size);
bool stack_alloc_free(struct stack_allocator *alloc, void *el);
void *stack_alloc_alloc(struct stack_allocator *alloc);



/******************************************************************************/
/* e10k direct access channel */

/**
 * Initialize directly mapped RX/TX queue pair with e10k NIC.
 *
 * @param bu          Channel struct
 * @param ws          Waitset
 * @param card        Card name
 * @param queue       Queue ID to use
 * @param buffer_size Size of receive buffers in bytes
 * @param ring_size   Number of descriptors in the RX/TX rings
 * @param received    Callback for a received packet
 * @param transmitted Callback for a transmitted packet
 */
errval_t bulk_e10k_init(struct bulk_e10k *bu,
                        struct waitset *ws,
                        const char *card,
                        uint8_t queue,
                        size_t buffer_size,
                        size_t ring_size,
                        void (*received)(struct bulk_e10k *,
                                         struct bulk_net_msgdesc *),
                        void (*transmitted)(struct bulk_e10k *, void *));

/**
 * Add a buffer to the receive queue.
 *
 * @param bu     Channel struct
 * @param phys   Physical address of buffer
 * @param header Physical address of header buffer (needs E10K_HDRSZ bytes)
 * @param opaque User-Data for this buffer, will be returned when it is used in
 *               a received packet.
 */
errval_t bulk_e10k_rx_add(struct bulk_e10k *bu, uint64_t phys, uint64_t header,
                          void *opaque);

/**
 * Send out a packet.
 *
 * @param bu   Channel struct
 * @param decs Descriptor for buffer chain to transmit
 */
errval_t bulk_e10k_send(struct bulk_e10k *bu, struct bulk_net_msgdesc *desc);


/**
 * Steer a specific UDP port to this queue.
 *
 * @param bu   Channel struct
 * @param port Port to allocate (in host byte order)
 */
errval_t bulk_e10k_port_add(struct bulk_e10k *bu, uint16_t port);

/**
 * Allocate an unused UDP port and steer it to this queue.
 *
 * @param bu   Channel struct
 * @param port Pointer to variable where port number will be stored (host byte
 *             order)
 */
errval_t bulk_e10k_port_alloc(struct bulk_e10k *bu, uint16_t *port);

/**
 * Get IP address configured for this interface.
 *
 * @param bu Channel struct
 * @param ip Pointer to variable where IP will be stored (host byte order)
 */
errval_t bulk_e10k_ip_info(struct bulk_e10k *bu, uint32_t *ip);

/**
 * Do an ARP lookup on this interface
 *
 * @param bu  Channnel struct
 * @param ip  IP address to resolve (in host byte order)
 * @param mac Pointer to variable where MAC address will be stored
 */
errval_t bulk_e10k_arp_lookup(struct bulk_e10k *bu, uint32_t ip, uint64_t *mac);

/******************************************************************************/


/** Allocate and map a frame */
static inline errval_t allocmap_frame(size_t size, void **virt, uintptr_t *phys,
                                      struct capref *cap)
{
    errval_t err;
    struct frame_identity fid = { 0, 0 };
    struct capref c;


    err = frame_alloc(&c, size, NULL);
    assert(err_is_ok(err));

    if (phys) {
        invoke_frame_identify(c, &fid);
        *phys = fid.base;
    }

    err = vspace_map_one_frame_attr(virt, size, c,
            VREGION_FLAGS_READ_WRITE, NULL, NULL);

    if (cap != NULL) {
        *cap = c;
    }
    return err;

}


#endif /* BULK_NET_BACKEND_H */
