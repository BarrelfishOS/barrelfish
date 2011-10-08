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
#include <procon/procon.h>

// Checks for queue empty condition
bool sp_queue_empty(struct shared_pool *sp)
{
    return (sp->read_reg.value == sp->write_reg.value);
}


// Check for queue full condition
bool sp_queue_full(struct shared_pool *sp)
{
    uint64_t queue_size = sp->size_reg.value;
//    uint64_t queue_size = MAX_SLOTS;
    return (((sp->write_reg.value + 1) % queue_size ) == sp->read_reg.value);
}

// Checks if given index is peekable or not
bool sp_peekable_index(struct shared_pool *sp, uint64_t index)
{
    uint64_t queue_size = sp->size_reg.value;
//    uint64_t queue_size = MAX_SLOTS;
    // Trivial case: index bigger than queue size
    if (index >= queue_size){
        return false;
    }

    // Trivial case: queue empty
    if (sp_queue_empty(sp)) {
        return false;
    }

    if (sp->write_reg.value > sp->read_reg.value) {
        // simple, non-wrapped state of queue
        if ((sp->read_reg.value <= index) && (index < sp->write_reg.value)) {
            return true;
        }
        return false;
    }

    // wrapped queue, so more complicated!
    if (index < sp->write_reg.value || sp->read_reg.value <= index) {
        return true;
    }
    return false;
} // end function: sp_peekable_index


// Returns no. of elements available for consumption
uint64_t sp_queue_elements_count(struct shared_pool *sp)
{
    uint64_t queue_size = sp->size_reg.value;
//    uint64_t queue_size = MAX_SLOTS;
    if (sp_queue_empty(sp)) {
        return 0;
    }

    if (sp->write_reg.value > sp->read_reg.value) {
        // simple, non-wrapped state of queue
        return (sp->write_reg.value - sp->read_reg.value);
    }

    // wrapped queue, so more complicated!
    return ((queue_size - sp->read_reg.value) + sp->write_reg.value);
} // end function: sp_queue_element_count



// Returns no. of free slots available for production
uint64_t sp_queue_free_slots_count(struct shared_pool *sp)
{
    uint64_t queue_size = sp->size_reg.value;
//    uint64_t queue_size = MAX_SLOTS;
    if (sp_queue_empty(sp)) {
        return queue_size;
    }

    if (sp->write_reg.value > sp->read_reg.value) {
        // simple, non-wrapped state of queue
        return ((queue_size - sp->write_reg.value) + sp->read_reg.value);
    }

    // wrapped queue, so more complicated!
    return (sp->read_reg.value - sp->write_reg.value);
} // end function: sp_queue_free_slots_count


void sp_reset_pool(struct shared_pool *sp, uint64_t slot_count)
{
    int i = 0;
    if (slot_count == 0) {
        slot_count = MAX_SLOTS;
    }
    sp->size_reg.value = slot_count;
    sp->read_reg.value = 0;
    sp->write_reg.value = 0;
    for(i = 0; i < slot_count; ++i)  {
       memset(&sp->slot_list[i], 0, sizeof(union slot));
    } // end for:
} // sp_reset_pool

/*
void sp_increment_write_index(struct shared_pool *sp)
{
    uint64_t queue_size = sp->size_reg.value;
//    uint64_t queue_size = MAX_SLOTS;
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
    d->paddr = s->paddr;
    d->len = s->len;
    d->no_pbufs = s->no_pbufs;
    d->ts = s->ts;
}

// Adds the data from parameter d into appropriate slot of shared pool queue
bool sp_produce_slot(struct shared_pool *sp, struct slot_data *d)
{
    uint64_t queue_size = sp->size_reg.value;
//    uint64_t queue_size = MAX_SLOTS;
    if (sp_queue_full(sp)) {
        return false;
    }

    uint64_t wi = sp->write_reg.value;
    sp_copy_slot_data(&sp->slot_list[wi].d, d);
    // Incrementing write pointer
    sp->write_reg.value = (wi + 1) % queue_size;
    return true;
} // end function: sp_produce_slot

// Peeks the requested slot without changing the read pointer
// To bu used by driver when it adds the packet in hardware queue for sending
// but the packet is not yet sent.
// when packet is actually done, then read pointer can be changed.
bool sp_peek_slot(struct shared_pool *sp, struct slot_data *dst,
        uint64_t index)
{
    // Make sure that slot provided is proper
    assert(dst != NULL);

    // Make sure that there is slot available for consumption
    if (sp_queue_empty(sp)) {
        return false;
    }

    // Check if the requested peak is valid or not
    if (!sp_peekable_index(sp, index))
    {
        return false;
    }

    // swapping the slot_data contents between ri and new_slot
    sp_copy_slot_data(dst, &sp->slot_list[index].d);
    return true;
} // end function: sp_consume_slot


// swaps the slot provided in parameter d with next available slot for
// consumption.
// TO be used by application to receive packet and register new pbuf
// at same time.
bool sp_replace_slot(struct shared_pool *sp, struct slot_data *new_slot)
{
    uint64_t queue_size = sp->size_reg.value;
//    uint64_t queue_size = MAX_SLOTS;

    // Make sure that slot provided is proper
    if (!validate_slot(new_slot)) {
        return false;
    }

    // Make sure that there is slot available for consumption
    if (sp_queue_empty(sp)) {
        return false;
    }

    uint64_t ri = sp->read_reg.value;
    // swapping the slot_data contents between ri and new_slot
    struct slot_data tmp;
    sp_copy_slot_data(&tmp, &sp->slot_list[ri].d);
    sp_copy_slot_data(&sp->slot_list[ri].d, new_slot);
    sp_copy_slot_data(new_slot, &tmp);

    // Incrementing read index
    sp->read_reg.value = (ri + 1) % queue_size;
    return true;
} // end function: sp_consume_slot

void sp_print_metadata(struct shared_pool *sp)
{
    printf("SP Q len[%"PRIu64"], RI[%"PRIu64"], WI[%"PRIu64"], elem[%"PRIu64"]"
            "free[%"PRIu64"]\n",
            sp->size_reg.value, sp->read_reg.value, sp->write_reg.value,
            sp_queue_elements_count(sp),
            sp_queue_free_slots_count(sp)
            );
}


void sp_print_slot(struct slot_data *d)
{
    printf("buf[%"PRIu64"], pbuf_id[%"PRIu64"], paddr[%"PRIu64"], "
            "len[%"PRIu64"], n_p[%"PRIu64"], ts[%"PRIu64"]\n",
            d->buffer_id, d->pbuf_id, d->paddr, d->len, d->no_pbufs,
            d->ts);
}

// Code for testing and debugging the library
void sp_print_pool(struct shared_pool *sp)
{
    uint64_t queue_size = sp->size_reg.value;
//    uint64_t queue_size = MAX_SLOTS;
    sp_print_metadata(sp);
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
    d1.paddr = 0;
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
    if (sp_peekable_index(&mysp, test_id)) {
        printf("id %"PRIu64" is peekable\n", test_id);
    } else {
        printf("id %"PRIu64" is not peekable\n", test_id);
    }

    test_id = 9;
    if (sp_peekable_index(&mysp, test_id)) {
        printf("id %"PRIu64" is peekable\n", test_id);
    } else {
        printf("id %"PRIu64" is not peekable\n", test_id);
    }
    test_id = 5;
    if (sp_peekable_index(&mysp, test_id)) {
        printf("id %"PRIu64" is peekable\n", test_id);
    } else {
        printf("id %"PRIu64" is not peekable\n", test_id);
    }
    test_id = 3;
    if (sp_peekable_index(&mysp, test_id)) {
        printf("id %"PRIu64" is peekable\n", test_id);
    } else {
        printf("id %"PRIu64" is not peekable\n", test_id);
    }
    test_id = 8;
    if (sp_peekable_index(&mysp, test_id)) {
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
    if (sp_peekable_index(&mysp, test_id)) {
        printf("id %"PRIu64" is peekable\n", test_id);
    } else {
        printf("id %"PRIu64" is not peekable\n", test_id);
    }

    test_id = 9;
    if (sp_peekable_index(&mysp, test_id)) {
        printf("id %"PRIu64" is peekable\n", test_id);
    } else {
        printf("id %"PRIu64" is not peekable\n", test_id);
    }
    test_id = 5;
    if (sp_peekable_index(&mysp, test_id)) {
        printf("id %"PRIu64" is peekable\n", test_id);
    } else {
        printf("id %"PRIu64" is not peekable\n", test_id);
    }
    test_id = 3;
    if (sp_peekable_index(&mysp, test_id)) {
        printf("id %"PRIu64" is peekable\n", test_id);
    } else {
        printf("id %"PRIu64" is not peekable\n", test_id);
    }

    test_id = 8;
    if (sp_peekable_index(&mysp, test_id)) {
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
    if (sp_peekable_index(&mysp, test_id)) {
        printf("id %"PRIu64" is peekable\n", test_id);
    } else {
        printf("id %"PRIu64" is not peekable\n", test_id);
    }

    test_id = 9;
    if (sp_peekable_index(&mysp, test_id)) {
        printf("id %"PRIu64" is peekable\n", test_id);
    } else {
        printf("id %"PRIu64" is not peekable\n", test_id);
    }
    test_id = 5;
    if (sp_peekable_index(&mysp, test_id)) {
        printf("id %"PRIu64" is peekable\n", test_id);
    } else {
        printf("id %"PRIu64" is not peekable\n", test_id);
    }
    test_id = 3;
    if (sp_peekable_index(&mysp, test_id)) {
        printf("id %"PRIu64" is peekable\n", test_id);
    } else {
        printf("id %"PRIu64" is not peekable\n", test_id);
    }

    test_id = 8;
    if (sp_peekable_index(&mysp, test_id)) {
        printf("id %"PRIu64" is peekable\n", test_id);
    } else {
        printf("id %"PRIu64" is not peekable\n", test_id);
    }
}

#endif // 0
