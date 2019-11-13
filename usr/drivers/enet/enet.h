/*
 * Copyright (c) 2019, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ENET_H_
#define ENET_H_

#define ENET_DEBUG(x...) debug_printf("[enet] " x);

//#define ENET_PROMISC

#define TX_RING_SIZE 512
#define ENET_RX_FRSIZE 2048
#define ENET_RX_PAGES 256

#define RX_RING_SIZE (BASE_PAGE_SIZE / ENET_RX_FRSIZE) * ENET_RX_PAGES


#define ENET_RX_EMPTY ((ushort) 0x8000)
#define ENET_SC_WRAP ((ushort)0x2000)


struct region_entry {
    uint32_t rid;
    struct dmem mem;
    struct region_entry* next;
};

struct enet_queue {
    struct devq q;
    size_t size;

    // stop and wake threashold
    uint16_t stop_th; 
    uint16_t wake_th;
    char* tso_hdr;

    struct dmem desc_mem;

    // hd + tail
    size_t head;
    size_t tail;

    // Descriptor + Cleanq
    enet_bufdesc_array_t *ring;
    struct devq_buf *ring_bufs;


    struct region_entry* regions;
};

struct enet_driver_state {
    struct bfdriver_instance *bfi;
    struct capref regs;
    lvaddr_t d_vaddr;

    struct enet_queue* rxq;
    struct enet_queue* txq;
    enet_t* d;
    uint64_t mac;
};

#define ENET_HASH_BITS 6
#define ENET_CRC32_POLY 0xEDB88320

#endif // ndef ENET_H_
