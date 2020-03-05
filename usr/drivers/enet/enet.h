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

#define ENET_PROMISC

#define TX_RING_SIZE 512
#define ENET_RX_FRSIZE 2048
#define ENET_RX_PAGES 256

#define ENET_MAX_PKT_SIZE 1536

#define RX_RING_SIZE (BASE_PAGE_SIZE / ENET_RX_FRSIZE) * ENET_RX_PAGES


#define ENET_RX_EMPTY 0x8000
#define ENET_SC_WRAP ((ushort)0x2000)
#define ENET_RX_intr ((ushort)0x1000)
#define ENET_RX_LAST ((ushort) 0x0800)
#define ENET_RX_FIRST ((ushort) 0x0400)
#define ENET_RX_MISS ((ushort) 0x0100)
#define ENET_RX_LG ((ushort) 0x0020)
#define ENET_RX_NO ((ushort) 0x0010)
#define ENET_RX_SH ((ushort) 0x0008)
#define ENET_RX_CR ((ushort) 0x0004)
#define ENET_RX_OV ((ushort) 0x0002)
#define ENET_RX_CL ((ushort) 0x0001)
#define ENET_RX_STATS ((ushort) 0x013f)

#define ENET_TX_READY 0x8000
#define ENET_TX_WRAP 0x2000
#define ENET_TX_LAST 0x0800
#define ENET_TX_CRC 0x0400

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
    enet_t* d;

    // hd + tail
    size_t head;
    size_t tail;

    // alignment
    size_t align;

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

    uint32_t phy_id;

    struct capref rx_mem;
    struct capref tx_mem;
};

#define ENET_HASH_BITS 6
#define ENET_CRC32_POLY 0xEDB88320

#endif // ndef ENET_H_
