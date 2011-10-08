/**
 * \file
 * \brief producer consumer library
 *
 * This file provides a producer consumer protocol
 */

/*
 * Copyright (c) 2007-11 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


#ifndef PROCON_H_
#define PROCON_H_
#include <stdio.h>




#ifdef __scc__
#define CACHESIZE   32
#else
#define CACHESIZE    64
#endif

#define SLOT_PADDING ((CACHESIZE) - ((sizeof(struct slot_data))%(CACHESIZE)))

#define SLOT_SIZE ((sizeof(struct slot_data)) + (SLOT_PADDING))

#define TMP_SLOTS 2


// information inside the slot
struct slot_data {
    uint64_t buffer_id;
    uint64_t pbuf_id;
    uint64_t paddr;  // FIXME: replace it with offset!
    uint64_t len;
    uint64_t no_pbufs;
    uint64_t ts;
};


// slot container
union slot {
    struct slot_data d;
    uint8_t raw[SLOT_SIZE];
};


// register to hold the indexes
union vreg {
    uint64_t value;
    uint8_t raw[CACHESIZE];
};


// Container for shared space
struct shared_pool {
    union vreg write_reg; // slot-index that producer will produce next
    union vreg read_reg;  // slot-index that Consumer will consume next
    union vreg size_reg;
    union slot slot_list[TMP_SLOTS];
};

struct shared_pool_private {
    struct shared_pool *sp;
    struct capref cap;
//    struct ether_binding *con;
    lpaddr_t pa;
    void *va;
    uint64_t mem_size;
    uint64_t sp_id;         //as assigned by the network driver on registering
    uint64_t alloted_slots;
//    uint64_t read_id_cache;
    uint64_t peek_id;
//    uint64_t write_id_cache;
//    uint64_t size_reg_cache;
    bool    is_creator;
    uint8_t role;
    bool    producer;
    bool    consumer;

};

// initialization function prototypes
struct shared_pool_private *sp_create_shared_pool(uint64_t slot_no,
                                uint8_t role);
errval_t sp_map_shared_pool(struct shared_pool_private *spp, struct capref cap,
        uint64_t slot_no, uint8_t role);
void sp_reset_pool(struct shared_pool_private *spp, uint64_t slot_count);

// State checking function prototypes
bool sp_queue_empty(struct shared_pool_private *spp);
bool sp_queue_full(struct shared_pool_private *spp);
bool sp_peekable_index(struct shared_pool_private *spp, uint64_t index);
uint64_t sp_queue_elements_count(struct shared_pool_private *spp);
uint64_t sp_queue_free_slots_count(struct shared_pool_private *spp);

// State modifying function prototypes
bool sp_produce_slot(struct shared_pool_private *spp, struct slot_data *d);
bool sp_replace_slot(struct shared_pool_private *spp,
            struct slot_data *new_slot);
bool sp_peek_slot(struct shared_pool_private *spp, struct slot_data *dst,
        uint64_t index);

// Debugging functions
void sp_print_slot(struct slot_data *d);
void sp_print_metadata(struct shared_pool_private *spp);
void sp_print_pool(struct shared_pool_private *spp);

#endif // PROCON_H_
