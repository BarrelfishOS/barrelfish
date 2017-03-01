
/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _BLK_AHCI_
#define _BLK_AHCI_

#include <barrelfish/barrelfish.h>
#include <pci/mem.h>
#include <blk/ahci.h>
//#include <devif/queue.h>

#include <dev/ahci_hba_dev.h>
#include <dev/ahci_port_dev.h>
#include <dev/ata_identify_dev.h>

#include "../dma_mem/dma_mem.h"

#define MAX_AHCI_PORTS 32
#define MAX_CTBA_SLOTS 32

#define MAX_HBA 32
#define MAX_BUFFERS 256
#define MAX_REQUESTS MAX_CTBA_SLOTS

struct __attribute__((__packed__)) region_descriptor {
    /// Data base address
    uint64_t dba;
    uint32_t reserved;
    /// Byte count and Interrupt bit (31)
    uint32_t dbc;
};

struct __attribute__((__packed__)) command_table {
    /// Command FIS
    uint8_t cfis[64];
    /// ATAPI command
    uint8_t acmd[16];
    uint8_t reserved[48];
    /// Physical Descriptior Region Table entries
    /// Can be up to 65536 entries, we limit this to 248 for now
    /// (which limits the size of CommandTable to a single page).
    struct region_descriptor prdt[248];
};


struct ahci_port;
struct dev_queue;


enum RequestStatus {
    RequestStatus_Unused = 0,
    RequestStatus_InProgress = 1,
    RequestStatus_Done = 2
};

struct dev_queue_request {
    // TODO change back to regionid_t
    //regionid_t region_id;
    uint32_t region_id;
    struct dma_mem region;
    uint64_t command_slot;

    genpaddr_t offset;
    genpaddr_t length;
    genpaddr_t valid_data;
    genpaddr_t valid_length;

    errval_t error;
    enum RequestStatus status;
};


typedef void (*ahci_port_interrupt_handler_fn)(struct ahci_port*, struct dev_queue_request* reqs, size_t slots);

struct ahci_port {
    bool is_initialized; //< Port is up and running, ready to read/write.
    ahci_port_t port;
    struct dma_mem fb;
    struct dma_mem clb;
    struct dma_mem ctba_mem[MAX_CTBA_SLOTS];
    struct command_table* command_table[MAX_CTBA_SLOTS]; //< Points to ctba_mem[i].vaddr
    size_t ncs; //< Number of command slots actually implemented
    ahci_port_interrupt_handler_fn interrupt;
    struct ahci_mgmt_binding *binding;
    struct dma_mem identify_mem;
    ata_identify_t identify; //< Points to identify_mem.vaddr, valid after port_identify() is done.
};

struct ahci_disk {
    struct device_mem* bar5;
    ahci_hba_t controller;
    struct ahci_port ports[MAX_AHCI_PORTS];
};



struct dev_queue {
    struct ahci_port* port;
    struct dma_mem buffers[MAX_BUFFERS];
    struct dev_queue_request requests[MAX_REQUESTS];
};

/* ahci_port_init.h */
errval_t blk_ahci_ports_init(struct ahci_disk *ad);
errval_t blk_ahci_port_dma(struct ahci_port *port, uint64_t block, struct dma_mem *buffer, bool write);
errval_t blk_ahci_port_dma_async(struct ahci_port *port, size_t slot, uint64_t block, lpaddr_t base, size_t length, bool write);

lvaddr_t blk_ahci_get_bar5_vaddr(struct ahci_disk* ad);

#endif // _BLK_AHCI_
