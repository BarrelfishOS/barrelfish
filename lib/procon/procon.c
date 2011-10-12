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


// *******************  cache coherency specific code
#define MAX_CACHE_READ_TRIES  3

static uint64_t sp_atomic_read_reg(union vreg *reg)
{

    volatile uint64_t v1 = 0;
    volatile uint64_t v2 = 0;
    uint8_t tries = 0;

    for (tries = 0; tries < MAX_CACHE_READ_TRIES; ++tries) {
/*
#if !defined(__scc__) && !defined(__i386__)
        cache_flush_range(reg, CACHESIZE);
#endif // !defined(__scc__) && !defined(__i386__)
*/
        v1 = reg->value;
/*
#if !defined(__scc__) && !defined(__i386__)
        cache_flush_range(reg, CACHESIZE);
#endif // !defined(__scc__) && !defined(__i386__)
*/
        v2 = reg->value;

        if (v1 == v2) {
            return v1;
        }
    } // end for : retrying
    assert (!"atomic read of read index failed");
    return v1;
} // end function: sp_atomic_read_reg

static void sp_atomic_set_reg(union vreg *reg, uint64_t value)
{
    reg->value = value;

#if !defined(__scc__) && !defined(__i386__)
        cache_flush_range(reg, CACHESIZE);
#endif // !defined(__scc__) && !defined(__i386__)
}

void sp_reload_regs(struct shared_pool_private *spp)
{
    assert(spp != NULL);
    struct shared_pool *sp = spp->sp;
    assert(sp != NULL);
    spp->c_read_id = sp_atomic_read_reg(&spp->sp->read_reg);
    spp->c_write_id = sp_atomic_read_reg(&spp->sp->write_reg);
    spp->c_size = sp_atomic_read_reg(&spp->sp->size_reg);
}



// **************************** generic queue based code
bool sp_gen_queue_empty(uint64_t read, uint64_t write)
{
    return (read == write);
}

bool sp_gen_queue_full(uint64_t read, uint64_t write, uint64_t size)
{
    return (((write + 1) % size ) == read);
}

uint64_t sp_c_range_size(uint64_t start, uint64_t end, uint64_t size)
{

    // simple, non-wrapped space
    if (start <= end) {
        return (end - start);
    }

    // wrapped queue, so more complicated!
    return ((size - start) + end);
}


// checks for (start <= value < end ) in circular queue of size "size"
bool sp_c_between(uint64_t start, uint64_t value, uint64_t end, uint64_t size)
{

    // sanity check: value must be less than size
    if (value >= size) {
        return false;
    }

    // Logical queue empty state
    if (start == end) {
        if (start == value) {
            return true;
        }
        return false;
    }

    // simple, non-wrapped space
    if (start < end) {
        if ((start <= value) && (value < end)) {
            return true;
        }
        return false;
    }

    // wrapped space, more complicated
    if ((value < end) || (start <= value)) {
        return true;
    }
    return false;
}

// ******************* spp queue code for condition checking

uint64_t sp_get_read_index(struct shared_pool_private *spp)
{
    sp_reload_regs(spp);
    return spp->c_read_id;
}

uint64_t sp_get_write_index(struct shared_pool_private *spp)
{
    sp_reload_regs(spp);
    return spp->c_write_id;
}

uint64_t sp_get_queue_size(struct shared_pool_private *spp)
{
    sp_reload_regs(spp);
    return spp->c_size;
}


// Checks for queue empty condition
bool sp_queue_empty(struct shared_pool_private *spp)
{
//    sp_reload_regs(spp);
    return sp_gen_queue_empty(spp->c_read_id, spp->c_write_id);
}


// Check for queue full condition
bool sp_queue_full(struct shared_pool_private *spp)
{
    return sp_gen_queue_full(spp->c_read_id, spp->c_write_id,
            spp->c_size);
}


// Checks if given index is peekable or not
bool sp_read_peekable_index(struct shared_pool_private *spp, uint64_t index)
{
    sp_reload_regs(spp);
    return sp_c_between(spp->c_read_id, index, spp->c_write_id, spp->c_size);
} // end function: sp_read_peekable_index


// Checks if given index is settable for not for read_reg
bool sp_validate_read_index(struct shared_pool_private *spp, uint64_t index)
{
    sp_reload_regs(spp);
    uint64_t upper_limit = (spp->c_write_id + 1) % spp->c_size;
    return sp_c_between(spp->c_read_id, index, upper_limit, spp->c_size);
}


// Returns no. of elements available for consumption
uint64_t sp_queue_elements_count(struct shared_pool_private *spp)
{
    sp_reload_regs(spp);
    return sp_c_range_size(spp->c_read_id, spp->c_write_id, spp->c_size);
} // end function: sp_queue_elements_count

// Checks if given index is write peekable or not
bool sp_write_peekable_index(struct shared_pool_private *spp, uint64_t index)
{
    sp_reload_regs(spp);

    // Trivial case: index bigger than queue size
    if (index >= spp->c_size){
        return false;
    }

    // Trivial case: queue empty
    if (sp_queue_empty(spp)) {
        return true;
    }

    return sp_c_between(spp->c_write_id, index, spp->c_read_id, spp->c_size);
} // end function: sp_write_peekable_index


// Checks if given index is valid for write or not
bool sp_validate_write_index(struct shared_pool_private *spp, uint64_t index)
{
    return sp_write_peekable_index(spp, index);
} // end function: sp_validate_write_index


// Returns no. of free slots available for production
uint64_t sp_queue_free_slots_count(struct shared_pool_private *spp)
{
    sp_reload_regs(spp);
    if (sp_queue_empty(spp)) {
        return spp->c_size;
    }
    return sp_c_range_size(spp->c_write_id, spp->c_read_id, spp->c_size);
} // end function: sp_queue_free_slots_count


// ************* Initialization functions ***********************

static size_t calculate_shared_pool_size(uint64_t slot_no)
{
    return (sizeof(struct shared_pool) +
                ((sizeof(union slot)) * (slot_no - TMP_SLOTS)));
}

static void sp_reset_pool(struct shared_pool_private *spp, uint64_t slot_count)
{
    assert(spp != NULL);
    struct shared_pool *sp = spp->sp;
    assert(sp != NULL);
    assert(slot_count > TMP_SLOTS);

    int i = 0;

    // Esure that slot_count is <= alloted_slots
    assert(slot_count <= spp->alloted_slots);

    sp_atomic_set_reg(&sp->read_reg, 0);
    sp_atomic_set_reg(&sp->write_reg, 0);
    sp_atomic_set_reg(&sp->size_reg, slot_count);
    for(i = 0; i < slot_count; ++i)  {
       memset(&sp->slot_list[i], 0, sizeof(union slot));
    } // end for:

    sp_reload_regs(spp);
    spp->notify_other_side = 0;
    spp->ghost_read_id = spp->c_read_id;
    spp->ghost_write_id = spp->c_write_id;
    spp->pre_write_id = spp->c_read_id;
    spp->produce_counter = 0;
    spp->consume_counter = 0;
    spp->clear_counter = 0;
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

    sp_reload_regs(spp);
    assert(spp->c_size == spp->alloted_slots);

    spp->ghost_read_id = spp->c_read_id;
    spp->ghost_write_id = spp->c_write_id;
    spp->pre_write_id = spp->c_read_id;
    spp->notify_other_side = 0;
    spp->produce_counter = 0;
    spp->consume_counter = 0;
    spp->clear_counter = 0;

    printf("Mapped shared_pool of size(R %"PRIu64", A %"PRIu64") "
            "with role [%"PRIu8"], slots[%"PRIu64"] and pool len[%"PRIu64"]\n",
            (uint64_t)mem_size, spp->mem_size, spp->role, spp->alloted_slots,
            spp->c_size);
    return SYS_ERR_OK;

} // end function: sp_map_shared_pool


// *************************** State modifying functions *************
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

    sp_reload_regs(spp);
    // Trivial case:
    if (spp->c_read_id == index) {
        return true;
    }

    if (!sp_validate_read_index(spp, index)) {
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

    sp_atomic_set_reg(&spp->sp->read_reg, index);
    sp_reload_regs(spp);

    spp->ghost_read_id = spp->c_read_id;
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
    sp_reload_regs(spp);

    // Trivial case:
    if (spp->c_write_id  == index) {
        return true;
    }

    if (!sp_validate_write_index(spp, index)) {
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

    sp_atomic_set_reg(&spp->sp->write_reg, index);
    sp_reload_regs(spp);
    spp->ghost_write_id = spp->c_write_id;

    if (sp_queue_full(spp)) {
        // There no free space left to create new items.
        // We should inform the consumer that it is slow!
        // Typically means, consumer is slow!
        ++spp->notify_other_side;
    }

    ++spp->produce_counter;
    return true;
} // end function: sp_set_write_index



uint64_t sp_is_slot_clear(struct shared_pool_private *spp, uint64_t id)
{
    sp_reload_regs(spp);
    if (!sp_queue_empty(spp)) {
        if(!sp_c_between(spp->c_write_id, id, spp->c_read_id, spp->c_size)) {
            sp_print_metadata(spp);
            printf("failed for id %"PRIu64"\n", id);
            printf("callstack: %p %p %p %p\n",
	         __builtin_return_address(0),
	         __builtin_return_address(1),
	         __builtin_return_address(2),
	         __builtin_return_address(3));
        }
        assert(sp_c_between(spp->c_write_id, id, spp->c_read_id, spp->c_size));
    }
    /*
    else {
        // queue empty!
        if (id == spp->c_write_id) {
            sp_print_metadata(spp);
            printf("failed for id %"PRIu64"\n", id);
            printf("callstack: %p %p %p %p\n",
	         __builtin_return_address(0),
	         __builtin_return_address(1),
	         __builtin_return_address(2),
	         __builtin_return_address(3));
        }
        assert(id != spp->c_write_id);
    }
    */
    return spp->sp->slot_list[id].d.client_data;
}

bool sp_clear_slot(struct shared_pool_private *spp, struct slot_data *d,
        uint64_t id)
{
    sp_reload_regs(spp);

    if (!sp_c_between(spp->c_write_id, id, spp->c_read_id, spp->c_size)) {
        return false;
    }

    if (sp_queue_full(spp)) {
        return false;
    }

    sp_copy_slot_data(d, &spp->sp->slot_list[id].d);
    spp->pre_write_id = id;
/*    printf("%s Slot %p with id %"PRIu64" is cleared and had %"PRIu64", %"PRIu64"\n",
            disp_name(), &spp->sp->slot_list[id].d,
            id, spp->sp->slot_list[id].d.client_data, d->client_data);
*/
    spp->sp->slot_list[id].d.client_data = 0;
    ++spp->clear_counter;

    return true;
} // end function: sp_clear_slot


// Adds the data from parameter d into appropriate slot of shared pool queue
bool sp_produce_slot(struct shared_pool_private *spp, struct slot_data *d)
{

    sp_reload_regs(spp);

    if (sp_queue_full(spp)) {
        return false;
    }

    uint64_t wi = spp->c_write_id;
    sp_copy_slot_data(&spp->sp->slot_list[wi].d, d);

#if !defined(__scc__) && !defined(__i386__)
        cache_flush_range(&spp->sp->slot_list[wi], SLOT_SIZE);
#endif // !defined(__scc__) && !defined(__i386__)

    // Incrementing write pointer
    assert(sp_set_write_index(spp, ((wi + 1) % spp->c_size)));
    return true;
} // end function: sp_produce_slot


// Gost-add data into shared_pool
// Add data into free slots, but don't increment write index
// This allows adding multiple slots and then atomically increment write index
bool sp_ghost_produce_slot(struct shared_pool_private *spp,
        struct slot_data *d, uint64_t index)
{
    sp_reload_regs(spp);

    // Make sure that slot provided is proper
    assert(d != NULL);

    if (sp_queue_full(spp)) {
//        printf("sp_ghost_produce_slot: queue full\n");
        return false;
    }

    // Check if the requested peak is valid or not
    if (!sp_write_peekable_index(spp, index))
    {
        return false;
    }

    sp_copy_slot_data(&spp->sp->slot_list[index].d, d);
#if !defined(__scc__) && !defined(__i386__)
        cache_flush_range(&spp->sp->slot_list[index], SLOT_SIZE);
#endif // !defined(__scc__) && !defined(__i386__)
    // Incrementing write pointer
    spp->ghost_write_id = (index + 1) % spp->c_size;
    /*
    printf("ghost produce slot, producing for %"PRIu64", val %"PRIu64"\n",
            index, d->client_data);
   sp_print_slot(&spp->sp->slot_list[index].d);
   */
    return true;
} // end function: sp_produce_slot

// Reads the slot without changing the read pointer, instead changes the local
// ghost_read_id to know how much is read.
// To bu used by driver when it adds the packet in hardware queue for sending
// but the packet is not yet sent.
// when packet is actually done, then read pointer can be changed.
bool sp_ghost_read_slot(struct shared_pool_private *spp, struct slot_data *dst)
{
    sp_reload_regs(spp);

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
/*
#if !defined(__scc__) && !defined(__i386__)
        cache_flush_range(&spp->sp->slot_list[spp->ghost_read_id], SLOT_SIZE);
#endif // !defined(__scc__) && !defined(__i386__)
*/
    sp_copy_slot_data(dst, &spp->sp->slot_list[spp->ghost_read_id].d);
/*    printf("After copying data from id %"PRIu64"\n", spp->ghost_read_id);
    sp_print_slot(&spp->sp->slot_list[spp->ghost_read_id].d);
*/
    spp->ghost_read_id = (spp->ghost_read_id + 1) % spp->c_size;
    return true;
} // end function: sp_read_peak_slot



// FIXME: not used, may be it should be removed
bool sp_ghost_read_confirm(struct shared_pool_private *spp)
{
    return (sp_set_read_index(spp, spp->ghost_read_id));
}

// swaps the slot provided in parameter d with next available slot for
// consumption.
// TO be used by application to receive packet and register new pbuf
// at same time.
bool sp_replace_slot(struct shared_pool_private *spp, struct slot_data *new_slot)
{
    sp_reload_regs(spp);

    // Make sure that slot provided is proper
    if (!validate_slot(new_slot)) {
        return false;
    }

    // Make sure that there is slot available for consumption
    if (sp_queue_empty(spp)) {
        return false;
    }

    uint64_t ri = spp->c_read_id;
    // swapping the slot_data contents between ri and new_slot
    struct slot_data tmp;
#if !defined(__scc__) && !defined(__i386__)
        cache_flush_range(&spp->sp->slot_list[ri], SLOT_SIZE);
#endif // !defined(__scc__) && !defined(__i386__)
    sp_copy_slot_data(&tmp, &spp->sp->slot_list[ri].d);
    sp_copy_slot_data(&spp->sp->slot_list[ri].d, new_slot);
    sp_copy_slot_data(new_slot, &tmp);
#if !defined(__scc__) && !defined(__i386__)
        cache_flush_range(&spp->sp->slot_list[ri], SLOT_SIZE);
#endif // !defined(__scc__) && !defined(__i386__)

    // Incrementing read index
    assert(sp_set_read_index(spp, ((ri + 1) % spp->c_size)));
    return true;
} // end function: sp_consume_slot


// ****************** For debugging purposes **************
void sp_print_metadata(struct shared_pool_private *spp)
{
    assert(spp != NULL);
//    sp_reload_regs(spp);
    printf("SPP Q C[%"PRIu8"], R[%"PRIu8"], GRI[%"PRIu64"], GWI[%"PRIu64"] "
            "pre_write_id[%"PRIu64"]\n",
            spp->is_creator?1:0, spp->role,
            spp->ghost_read_id, spp->ghost_write_id, spp->pre_write_id);
    printf("SPP S PRO[%"PRIu64"],  CON[%"PRIu64"], CLEAR[%"PRIu64"]\n",
            spp->produce_counter, spp->consume_counter, spp->clear_counter);
    printf("SPP S C C-R[%"PRIu64"],  C-W[%"PRIu64"]\n",
            spp->c_read_id, spp->c_write_id);

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
    printf("%s @%p, buf[%"PRIu64"], pbuf_id[%"PRIu64"], offset[%"PRIu64"], "
            "len[%"PRIu64"], n_p[%"PRIu64"], CL[%"PRIu64"], ts[%"PRIu64"]\n",
            disp_name(), d, d->buffer_id, d->pbuf_id, d->offset, d->len,
            d->no_pbufs, d->client_data, d->ts);
}

// Code for testing and debugging the library
void sp_print_pool(struct shared_pool_private *spp)
{
    sp_reload_regs(spp);
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

