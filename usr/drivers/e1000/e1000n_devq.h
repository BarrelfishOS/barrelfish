/*
 * Copyright (c) 2008, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _E1000_DEVQ_H__
#define _E1000_DEVQ_H__

typedef struct e1000_queue {
    struct devq q;

    e1000_mac_type_t mac_type;
    e1000_t hw_device;
    uint32_t pci_vendor, pci_deviceid, pci_bus, pci_device, pci_function;
    char *name;
    uint64_t mac_address;
    struct capref regs;
    struct capref irq;

    volatile union rx_desc *receive_ring;
    volatile struct tx_desc *transmit_ring;
    struct capref rx;
    struct capref tx;

    int receive_buffers;
    int transmit_buffers;

    regionid_t region_id;
    genpaddr_t region_base;
    gensize_t  region_size;

    unsigned receive_head, receive_tail;
    unsigned transmit_head, transmit_tail;

    unsigned interrupt_mode;
    void (*isr)(void *);

    bool extended_interrupts;
    unsigned advanced_descriptors; // 0 - none, 1 - 82572/4, 3 - 82576/i210/i350

    // binding
    bool bound;
    struct e1000_devif_binding* b;
} e1000_queue_t;

static inline size_t e1000_queue_free_rxslots(e1000_queue_t* q)
{
    size_t head = q->receive_head;
    size_t tail = q->receive_tail;
    size_t size = q->receive_buffers;

    if (tail >= head) {
        return size - (tail - head) -1; 
    } else {
        return size - (tail + size - head) -1; 
    }
}


/*   TX       */
static inline size_t e1000_queue_free_txslots(e1000_queue_t* q)
{

    size_t head = q->transmit_head;
    size_t tail = q->transmit_tail;
    size_t size = q->transmit_buffers;

    if (tail >= head) {
        return size - (tail - head) - 1; 
    } else {
        return size - (tail + size - head) - 1; 
    }

}


#endif
