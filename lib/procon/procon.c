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

#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/bulk_transfer.h>
#include <procon/procon.h>

uint64_t sp_get_read_index(struct shared_pool_private *spp)
{
#if !defined(__scc__) && !defined(__i386__)
        cache_flush_range(&spp->sp->read_reg, CACHESIZE);
#endif // !defined(__scc__) && !defined(__i386__)
    return (spp->sp->read_reg.value);
}

uint64_t sp_get_write_index(struct shared_pool_private *spp)
{
#if !defined(__scc__) && !defined(__i386__)
        cache_flush_range(&spp->sp->write_reg, CACHESIZE);
#endif // !defined(__scc__) && !defined(__i386__)
    return (spp->sp->write_reg.value);
}

uint64_t sp_get_queue_size(struct shared_pool_private *spp)
{
#if !defined(__scc__) && !defined(__i386__)
        cache_flush_range(&spp->sp->size_reg, CACHESIZE);
#endif // !defined(__scc__) && !defined(__i386__)
    return (spp->sp->size_reg.value);
}

// Checks for queue empty condition
bool sp_queue_empty(struct shared_pool_private *spp)
{
    assert(spp != NULL);
    struct shared_pool *sp = spp->sp;
    assert(sp != NULL);

    return (sp->read_reg.value == sp->write_reg.value);
}


// Check for queue full condition
bool sp_queue_full(struct shared_pool_private *spp)
{
    assert(spp != NULL);
    struct shared_pool *sp = spp->sp;
    assert(sp != NULL);

    uint64_t queue_size = sp->size_reg.value;
    return (((sp->write_reg.value + 1) % queue_size ) == sp->read_reg.value);
}

// Checks if given index is peekable or not
bool sp_read_peekable_index(struct shared_pool_private *spp, uint64_t index)
{
    assert(spp != NULL);
    struct shared_pool *sp = spp->sp;
    assert(sp != NULL);

    uint64_t queue_size = sp->size_reg.value;
    // Trivial case: index bigger than queue size
    if (index >= queue_size){
        return false;
    }


    // Trivial case: queue empty
    if (sp_queue_empty(spp)) {
        return false;
    }

    // Trivial case: queue full
    if (sp_queue_full(spp)) {
        if (index == sp_get_write_index(spp)) {
            return false;
        }
        return true;
    }


    if (sp->read_reg.value < sp->write_reg.value) {
        // simple, non-wrapped state of queue
        if ((sp->read_reg.value <= index) && (index < sp->write_reg.value)) {
            return true;
        }
        return false;
    }

    // wrapped queue, so more complicated!
    if ((index < sp->write_reg.value) || (sp->read_reg.value <= index)) {
        return true;
    }
    return false;
} // end function: sp_read_peekable_index

// Checks if given index is settable for not for read_reg
bool sp_read_setable_index(struct shared_pool_private *spp, uint64_t index)
{
    assert(spp != NULL);
    struct shared_pool *sp = spp->sp;
    assert(sp != NULL);

    uint64_t queue_size = sp->size_reg.value;
    // Trivial case: index bigger than queue size
    if (index >= queue_size){
        return false;
    }


    // Trivial case: queue empty
    if (sp_queue_empty(spp)) {
        return false;
    }

    // Trivial case: queue full
    if (sp_queue_full(spp)) {
        if (index == sp_get_write_index(spp)) {
            return false;
        }
        return true;
    }


    if (sp->read_reg.value < sp->write_reg.value) {
        // simple, non-wrapped state of queue
        if ((sp->read_reg.value <= index) && (index <= sp->write_reg.value)) {
            return true;
        }
        return false;
    }

    // wrapped queue, so more complicated!
    if ((index <= sp->write_reg.value) || (sp->read_reg.value <= index)) {
        return true;
    }
    return false;
} // end function: sp_read_setable_index


// Returns no. of elements available for consumption
uint64_t sp_queue_elements_count(struct shared_pool_private *spp)
{
    assert(spp != NULL);
    struct shared_pool *sp = spp->sp;
    assert(sp != NULL);

    uint64_t queue_size = sp->size_reg.value;
    if (sp_queue_empty(spp)) {
        return 0;
    }

    if (sp->write_reg.value > sp->read_reg.value) {
        // simple, non-wrapped state of queue
        return (sp->write_reg.value - sp->read_reg.value);
    }

    // wrapped queue, so more complicated!
    return ((queue_size - sp->read_reg.value) + sp->write_reg.value);
} // end function: sp_queue_element_count

// Checks if given index is write peekable or not
bool sp_write_peekable_index(struct shared_pool_private *spp, uint64_t index)
{
    assert(spp != NULL);
    struct shared_pool *sp = spp->sp;
    assert(sp != NULL);

    uint64_t queue_size = sp->size_reg.value;
    // Trivial case: index bigger than queue size
    if (index >= queue_size){
        return false;
    }

    // Trivial case: queue full
  if (sp_queue_full(spp)) {
        return false;
    }


    // Trivial case: queue empty
    if (sp_queue_empty(spp)) {
        return true;
    }


    if (sp->read_reg.value < sp->write_reg.value) {
        // simple, non-wrapped state of queue
        if ((index < sp->read_reg.value) || (sp->write_reg.value <= index)) {
            return true;
        }
        return false;
    }

    // wrapped queue, so more complicated!
    if ((sp->write_reg.value <= index) && ( index < sp->read_reg.value)) {
        return true;
    }
    return false;
} // end function: sp_write_peekable_index



// Returns no. of free slots available for production
uint64_t sp_queue_free_slots_count(struct shared_pool_private *spp)
{
    assert(spp != NULL);
    struct shared_pool *sp = spp->sp;
    assert(sp != NULL);

    uint64_t queue_size = sp->size_reg.value;
    if (sp_queue_empty(spp)) {
        return queue_size;
    }

    if (sp->write_reg.value > sp->read_reg.value) {
        // simple, non-wrapped state of queue
        return ((queue_size - sp->write_reg.value) + sp->read_reg.value);
    }

    // wrapped queue, so more complicated!
    return (sp->read_reg.value - sp->write_reg.value);
} // end function: sp_queue_free_slots_count





// ************* Initialization functions ***********************


static size_t calculate_shared_pool_size(uint64_t slot_no)
{
    return (sizeof(struct shared_pool) +
                ((sizeof(union slot)) * (slot_no - TMP_SLOTS)));
}

void sp_reset_pool(struct shared_pool_private *spp, uint64_t slot_count)
{
    assert(spp != NULL);
    struct shared_pool *sp = spp->sp;
    assert(sp != NULL);
    assert(slot_count > TMP_SLOTS);

    int i = 0;

    // Esure that slot_count is <= alloted_slots
    assert(slot_count <= spp->alloted_slots);

    sp->size_reg.value = slot_count;
    sp->read_reg.value = 0;
    sp->write_reg.value = 0;
    for(i = 0; i < slot_count; ++i)  {
       memset(&sp->slot_list[i], 0, sizeof(union slot));
    } // end for:

    spp->notify_other_side = 0;
    spp->ghost_read_id = sp->read_reg.value;
    spp->ghost_write_id = sp->write_reg.value;
    spp->produce_counter = 0;
    spp->consume_counter = 0;
} // sp_reset_pool


// Creates a new shared_pool area and initializes it as creator
struct shared_pool_private *sp_create_shared_pool(uint64_t slot_no,
        uint8_t role)
{


    struct shared_pool_private *spp = (struct shared_pool_private *)
                malloc(sizeof(struct shared_pool_private));
    assert(spp != NULL);

    errval_t err;
    assert(slot_no > 2);
    size_t mem_size = calculate_shared_pool_size(slot_no);

    // NOTE: using bulk create here because bulk_create code has
    // been modified to suit the shared buffer allocation
    // FIXME: code repetation with mem_barrelfish_alloc_and_register
    struct bulk_transfer bt_sp;
#if defined(__scc__) && !defined(RCK_EMU)
    err = bulk_create(mem_size, sizeof(union slot), &(spp->cap), &bt_sp, true);
#else
    err = bulk_create(mem_size, sizeof(union slot), &(spp->cap), &bt_sp, false);
#endif // defined(__scc__) && !defined(RCK_EMU)
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "bulk_create failed.");
        return NULL;
    }
    spp->va = bt_sp.mem;
    spp->sp = (struct shared_pool *)spp->va;

    struct frame_identity f;

    err = invoke_frame_identify(spp->cap, &f);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "frame_identify failed");
        return NULL;
    }
    spp->pa = f.base;
    spp->mem_size = (1 << f.bits);
    spp->alloted_slots = slot_no;
    spp->ghost_read_id = 0;
    spp->ghost_write_id = 0;
    spp->is_creator = true;
    spp->role = role;
    sp_reset_pool(spp, slot_no);
    printf("Created shared_pool of size(R %"PRIu64", A %"PRIu64") "
            "with role [%"PRIu8"] and slots [%"PRIu64"]\n",
            (uint64_t)mem_size, spp->mem_size, spp->role,
            spp->alloted_slots);
    return spp;
} // end function: sp_create_shared_pool


// Loads shared_pool area which is already created by some other creator
errval_t sp_map_shared_pool(struct shared_pool_private *spp, struct capref cap,
        uint64_t slot_no, uint8_t role)
{
    errval_t err = SYS_ERR_OK;
    assert(spp != NULL);
    assert(spp->sp == NULL);
    assert(slot_no > 2);
    spp->cap = cap;
    spp->alloted_slots = slot_no;
    spp->role = role;
    spp->is_creator = 0;

    struct frame_identity f;

    err = invoke_frame_identify(cap, &f);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "invoke_frame_identify failed");
        return err;
    }
    spp->pa = f.base;
    spp->mem_size = (1 << f.bits);
    size_t mem_size = calculate_shared_pool_size(slot_no);
    assert(mem_size >= spp->mem_size);

    err = vspace_map_one_frame_attr(&spp->va, (1L << f.bits), cap,
                  VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "vspace_map_one_frame failed");
        return err;
    }

    spp->sp = (struct shared_pool *)spp->va;
    assert(spp->sp->size_reg.value == spp->alloted_slots);
    spp->ghost_read_id = spp->sp->read_reg.value;
    spp->ghost_write_id = spp->sp->write_reg.value;
    spp->notify_other_side = 0;
    spp->produce_counter = 0;
    spp->consume_counter = 0;

    printf("Mapped shared_pool of size(R %"PRIu64", A %"PRIu64") "
            "with role [%"PRIu8"], slots[%"PRIu64"] and pool len[%"PRIu64"]\n",
            (uint64_t)mem_size, spp->mem_size, spp->role, spp->alloted_slots,
            spp->sp->size_reg.value);
    return SYS_ERR_OK;

} // end function: sp_map_shared_pool


// *************************** State modifying functions *************
/*
void sp_increment_write_index(struct shared_pool_private *spp)
{
    assert(spp != NULL);
    struct shared_pool *sp = spp->sp;
    assert(sp != NULL);

    uint64_t queue_size = sp->size_reg.value;
    // Incrementing write pointer
    sp->write_index = (sp->write_index + 1) % queue_size;
}
*/

static bool validate_slot(struct slot_data *d)
{
    if (d == NULL) {
        return false;
    }

    // FIXME: check if the buffer_id, pbuff_id, len and all are sensible!
    return true;
} // end function: validate_slot

static void sp_copy_slot_data(struct slot_data *d, struct slot_data *s)
{
    assert(d != NULL);
    assert(s != NULL);
    d->buffer_id = s->buffer_id;
    d->pbuf_id = s->pbuf_id;
    d->offset = s->offset;
    d->len = s->len;
    d->no_pbufs = s->no_pbufs;
    d->client_data = s->client_data;
    d->ts = s->ts;
}

// Set the value of read index
// To be used with sp_read_peek_slot
bool sp_set_read_index(struct shared_pool_private *spp, uint64_t index)
{
    assert(spp != NULL);
    struct shared_pool *sp = spp->sp;
    assert(sp != NULL);

    // Trivial case:
    if (sp_get_read_index(spp) == index) {
        return true;
    }

    if (!sp_read_setable_index(spp, index)) {
        // The value in index is invalid!
        return false;
    }

    if (sp_queue_full(spp)) {
        // Producer is assuming that there is no free space in this pool.
        // As we have created some free space by reading, we should inform
        // the producer to produce more!
        // Typically means, I am slow!
        ++spp->notify_other_side;
    }

    sp->read_reg.value = index;
    spp->ghost_read_id = sp->read_reg.value;
#if !defined(__scc__) && !defined(__i386__)
        cache_flush_range(&sp->read_reg, CACHESIZE);
#endif // !defined(__scc__) && !defined(__i386__)
//    printf("changing read_index!\n");
    if (sp_queue_empty(spp)) {
        // There is nothing more to consume,
        // We should inform producer to produce quickly
        // Typically means, Producer is slow!
        ++spp->notify_other_side;
    }

    ++spp->consume_counter;

    return true;
} // end function: sp_set_read_index


// Set the value of write index
// To be used with sp_ghost_produce_slot
bool sp_set_write_index(struct shared_pool_private *spp, uint64_t index)
{
    assert(spp != NULL);
    struct shared_pool *sp = spp->sp;
    assert(sp != NULL);

    // Trivial case:
    if (sp_get_write_index(spp) == index) {
        return true;
    }

    if (!sp_write_peekable_index(spp, index)) {
        // The value in index is invalid!
        return false;
    }

    if (sp_queue_empty(spp)) {
        // Consumer is assuming that there is no data in the pool
        // As we have created new data, we should inform
        // the consumer to consume more!
        // Typically means, I am slow!
        ++spp->notify_other_side;
    }

    sp->write_reg.value = index;
    spp->ghost_write_id = sp->write_reg.value;
#if !defined(__scc__) && !defined(__i386__)
        cache_flush_range(&sp->write_reg, CACHESIZE);
#endif // !defined(__scc__) && !defined(__i386__)

    if (sp_queue_full(spp)) {
        // There no free space left to create new items.
        // We should inform the consumer that it is slow!
        // Typically means, consumer is slow!
        ++spp->notify_other_side;
    }

    ++spp->produce_counter;
    return true;
} // end function: sp_set_write_index


// Adds the data from parameter d into appropriate slot of shared pool queue
bool sp_produce_slot(struct shared_pool_private *spp, struct slot_data *d)
{
    assert(spp != NULL);
    struct shared_pool *sp = spp->sp;
    assert(sp != NULL);

    uint64_t queue_size = sp->size_reg.value;
    if (sp_queue_full(spp)) {
        return false;
    }

    uint64_t wi = sp->write_reg.value;
    sp_copy_slot_data(&sp->slot_list[wi].d, d);

#if !defined(__scc__) && !defined(__i386__)
        cache_flush_range(&sp->slot_list[wi].d, SLOT_SIZE);
#endif // !defined(__scc__) && !defined(__i386__)

    // Incrementing write pointer
    assert(sp_set_write_index(spp, ((wi + 1) % queue_size)));
    return true;
} // end function: sp_produce_slot


// Gost-add data into shared_pool
// Add data into free slots, but don't increment write index
// This allows adding multiple slots and then atomically increment write index
bool sp_ghost_produce_slot(struct shared_pool_private *spp,
        struct slot_data *d, uint64_t index)
{
    assert(spp != NULL);
    struct shared_pool *sp = spp->sp;
    assert(sp != NULL);

    // Make sure that slot provided is proper
    assert(d != NULL);

    uint64_t queue_size = sp->size_reg.value;
    if (sp_queue_full(spp)) {
//        printf("sp_ghost_produce_slot: queue full\n");
        return false;
    }

    // Check if the requested peak is valid or not
    if (!sp_write_peekable_index(spp, index))
    {
        return false;
    }

    sp_copy_slot_data(&sp->slot_list[index].d, d);
#if !defined(__scc__) && !defined(__i386__)
        cache_flush_range(&sp->slot_list[index].d, SLOT_SIZE);
#endif // !defined(__scc__) && !defined(__i386__)
    // Incrementing write pointer
    spp->ghost_write_id = (index + 1) % queue_size;
    return true;
} // end function: sp_produce_slot

// Reads the slot without changing the read pointer, instead changes the local
// ghost_read_id to know how much is read.
// To bu used by driver when it adds the packet in hardware queue for sending
// but the packet is not yet sent.
// when packet is actually done, then read pointer can be changed.
bool sp_ghost_read_slot(struct shared_pool_private *spp, struct slot_data *dst)
{
    assert(spp != NULL);
    struct shared_pool *sp = spp->sp;
    assert(sp != NULL);

    uint64_t queue_size = sp->size_reg.value;

    // Make sure that slot provided is proper
    assert(dst != NULL);

    // Make sure that there is slot available for consumption
    if (sp_queue_empty(spp)) {
        return false;
    }

    // Check if the requested peak is valid or not
    if (!sp_read_peekable_index(spp, spp->ghost_read_id))
    {
        return false;
    }

    //  Copying the slot data contents into provided slot
#if !defined(__scc__) && !defined(__i386__)
        cache_flush_range(&sp->slot_list[spp->ghost_read_id].d, SLOT_SIZE);
#endif // !defined(__scc__) && !defined(__i386__)
    sp_copy_slot_data(dst, &sp->slot_list[spp->ghost_read_id].d);
    spp->ghost_read_id = (spp->ghost_read_id + 1) % queue_size;
    return true;
} // end function: sp_read_peak_slot


// swaps the slot provided in parameter d with next available slot for
// consumption.
// TO be used by application to receive packet and register new pbuf
// at same time.
bool sp_replace_slot(struct shared_pool_private *spp, struct slot_data *new_slot)
{
    assert(spp != NULL);
    struct shared_pool *sp = spp->sp;
    assert(sp != NULL);

    uint64_t queue_size = sp->size_reg.value;

    // Make sure that slot provided is proper
    if (!validate_slot(new_slot)) {
        return false;
    }

    // Make sure that there is slot available for consumption
    if (sp_queue_empty(spp)) {
        return false;
    }

    uint64_t ri = sp->read_reg.value;
    // swapping the slot_data contents between ri and new_slot
    struct slot_data tmp;
#if !defined(__scc__) && !defined(__i386__)
        cache_flush_range(&sp->slot_list[ri].d, SLOT_SIZE);
#endif // !defined(__scc__) && !defined(__i386__)
    sp_copy_slot_data(&tmp, &sp->slot_list[ri].d);
    sp_copy_slot_data(&sp->slot_list[ri].d, new_slot);
    sp_copy_slot_data(new_slot, &tmp);
#if !defined(__scc__) && !defined(__i386__)
        cache_flush_range(&sp->slot_list[ri].d, SLOT_SIZE);
#endif // !defined(__scc__) && !defined(__i386__)

    // Incrementing read index
    sp->read_reg.value = (ri + 1) % queue_size;
    sp_set_read_index(spp, ((ri + 1) % queue_size));
    return true;
} // end function: sp_consume_slot


// ****************** For debugging purposes **************
void sp_print_metadata(struct shared_pool_private *spp)
{
    assert(spp != NULL);
    printf("SPP Q C[%"PRIu8"], R[%"PRIu8"], GRI[%"PRIu64"], GWI[%"PRIu64"]\n",
            spp->is_creator?1:0, spp->role,
            spp->ghost_read_id, spp->ghost_write_id);
    printf("SPP S PRO[%"PRIu64"],  CON[%"PRIu64"]\n",
            spp->produce_counter, spp->consume_counter);

    struct shared_pool *sp = spp->sp;
    assert(sp != NULL);

    printf("SP Q len[%"PRIu64"], RI[%"PRIu64"], WI[%"PRIu64"], elem[%"PRIu64"]"
            " free[%"PRIu64"]\n",
            sp->size_reg.value, sp->read_reg.value, sp->write_reg.value,
            sp_queue_elements_count(spp),
            sp_queue_free_slots_count(spp));
}


void sp_print_slot(struct slot_data *d)
{
    printf("buf[%"PRIu64"], pbuf_id[%"PRIu64"], offset[%"PRIu64"], "
            "len[%"PRIu64"], n_p[%"PRIu64"], CL[%"PRIu64"], ts[%"PRIu64"]\n",
            d->buffer_id, d->pbuf_id, d->offset, d->len, d->no_pbufs,
            d->client_data, d->ts);
}

// Code for testing and debugging the library
void sp_print_pool(struct shared_pool_private *spp)
{
    assert(spp != NULL);
    struct shared_pool *sp = spp->sp;
    assert(sp != NULL);

    uint64_t queue_size = sp->size_reg.value;
    sp_print_metadata(spp);
    int i = 0;
    for(i = 0; i < queue_size; ++i)  {
        sp_print_slot(&sp->slot_list[i].d);
    }
}

// for debug purposes only
#if 0
int main()
{
    printf("slot_data = %d, slot = %d, slot_size = %d, padding = %d\n",
            sizeof(struct slot_data), sizeof(union slot),
            SLOT_SIZE, SLOT_PADDING);

    int start_id = 11;
    uint64_t test_id = 0;
    struct shared_pool mysp;
    struct slot_data d1;
    sp_reset_pool(&mysp, 10);
    sp_print_pool(&mysp);
    int i = 0;
    d1.buffer_id = 1;
    d1.pbuf_id = start_id + i;
    d1.offset = 0;
    d1.len = 1500;
    d1.no_pbufs = 3;
    d1.ts = 0;

    while(sp_produce_slot(&mysp, &d1)) {
        ++i;
        ++d1.pbuf_id;
    };
    printf("%d slots produced\n", i);
    sp_print_pool(&mysp);


    test_id = 0;
    if (sp_read_peekable_index(&mysp, test_id)) {
        printf("id %"PRIu64" is peekable\n", test_id);
    } else {
        printf("id %"PRIu64" is not peekable\n", test_id);
    }

    test_id = 9;
    if (sp_read_peekable_index(&mysp, test_id)) {
        printf("id %"PRIu64" is peekable\n", test_id);
    } else {
        printf("id %"PRIu64" is not peekable\n", test_id);
    }
    test_id = 5;
    if (sp_read_peekable_index(&mysp, test_id)) {
        printf("id %"PRIu64" is peekable\n", test_id);
    } else {
        printf("id %"PRIu64" is not peekable\n", test_id);
    }
    test_id = 3;
    if (sp_read_peekable_index(&mysp, test_id)) {
        printf("id %"PRIu64" is peekable\n", test_id);
    } else {
        printf("id %"PRIu64" is not peekable\n", test_id);
    }
    test_id = 8;
    if (sp_read_peekable_index(&mysp, test_id)) {
        printf("id %"PRIu64" is peekable\n", test_id);
    } else {
        printf("id %"PRIu64" is not peekable\n", test_id);
    }


    d1.buffer_id = 2;
    d1.pbuf_id = 100;
    i = 0;
    while(sp_replace_slot(&mysp, &d1)) {
        printf("Replaced %"PRIu64" with %d\n", d1.pbuf_id, i);
        ++i;
        ++d1.pbuf_id;
        if(i == 4) {
            break;
        }

    };
    printf("%d slots consumed\n", i);
    sp_print_pool(&mysp);

    test_id = 0;
    if (sp_read_peekable_index(&mysp, test_id)) {
        printf("id %"PRIu64" is peekable\n", test_id);
    } else {
        printf("id %"PRIu64" is not peekable\n", test_id);
    }

    test_id = 9;
    if (sp_read_peekable_index(&mysp, test_id)) {
        printf("id %"PRIu64" is peekable\n", test_id);
    } else {
        printf("id %"PRIu64" is not peekable\n", test_id);
    }
    test_id = 5;
    if (sp_read_peekable_index(&mysp, test_id)) {
        printf("id %"PRIu64" is peekable\n", test_id);
    } else {
        printf("id %"PRIu64" is not peekable\n", test_id);
    }
    test_id = 3;
    if (sp_read_peekable_index(&mysp, test_id)) {
        printf("id %"PRIu64" is peekable\n", test_id);
    } else {
        printf("id %"PRIu64" is not peekable\n", test_id);
    }

    test_id = 8;
    if (sp_read_peekable_index(&mysp, test_id)) {
        printf("id %"PRIu64" is peekable\n", test_id);
    } else {
        printf("id %"PRIu64" is not peekable\n", test_id);
    }

    d1.buffer_id = 2;
    d1.pbuf_id = 100;
    i = 0;
    while(sp_produce_slot(&mysp, &d1)) {
        ++i;
        ++d1.pbuf_id;
    };
    printf("%d slots produced\n", i);
    sp_print_pool(&mysp);


    test_id = 0;
    if (sp_read_peekable_index(&mysp, test_id)) {
        printf("id %"PRIu64" is peekable\n", test_id);
    } else {
        printf("id %"PRIu64" is not peekable\n", test_id);
    }

    test_id = 9;
    if (sp_read_peekable_index(&mysp, test_id)) {
        printf("id %"PRIu64" is peekable\n", test_id);
    } else {
        printf("id %"PRIu64" is not peekable\n", test_id);
    }
    test_id = 5;
    if (sp_read_peekable_index(&mysp, test_id)) {
        printf("id %"PRIu64" is peekable\n", test_id);
    } else {
        printf("id %"PRIu64" is not peekable\n", test_id);
    }
    test_id = 3;
    if (sp_read_peekable_index(&mysp, test_id)) {
        printf("id %"PRIu64" is peekable\n", test_id);
    } else {
        printf("id %"PRIu64" is not peekable\n", test_id);
    }

    test_id = 8;
    if (sp_read_peekable_index(&mysp, test_id)) {
        printf("id %"PRIu64" is peekable\n", test_id);
    } else {
        printf("id %"PRIu64" is not peekable\n", test_id);
    }
}

#endif // 0
