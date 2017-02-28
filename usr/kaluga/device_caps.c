#include <barrelfish/barrelfish.h>
#include <barrelfish/capabilities.h>
#include <barrelfish/nameservice_client.h>

#include <stdio.h>
#include <stdlib.h>
#include <mm/mm.h>
#include <if/monitor_blocking_defs.h>

#include "kaluga.h"

static struct mm register_manager;

/**
 * \brief Constructs a capability to a physical device range
 *
 * \param[in] address Physical address of register you want to map.
 * \param[in] size Size of register space to map
 * \param[out] devframe Capability to device range
 *
 * \retval SYS_ERR_OK devframe is a valid capability for the requested range.
 **/
errval_t get_device_cap(lpaddr_t address, size_t size, struct capref* devframe) 
{
    KALUGA_DEBUG("map_device_register: %"PRIxLPADDR" %zu %u\n", address, size, log2ceil(size));

    struct allocated_range {
        struct capref cr;
        struct frame_identity id;
        struct allocated_range* next;
    };

    static struct allocated_range* allocation_head = NULL;

    assert(address != 0);
    // TODO(gz) the paging in the kernel wants these two preconditions?
    assert(size % BASE_PAGE_SIZE == 0);
    assert(size >= BASE_PAGE_SIZE && "ARM paging breaks when smaller");

    errval_t err = mm_alloc_range(&register_manager, log2ceil(size),
                                  address, address+size,
                                  devframe, NULL);
    if (err_is_fail(err)) {
        // TODO(gz) Is there a better way to handle duplicated allocations?
        KALUGA_DEBUG("mm_alloc_range failed.\n");
        KALUGA_DEBUG("Do we already have an allocation that covers this range?\n");
        struct allocated_range* iter = allocation_head;
        while (iter != NULL) {
            if (address >= iter->id.base && 
                (address + size <= (iter->id.base + iter->id.bytes))) {
                KALUGA_DEBUG("Apparently, yes. We try to map that one.\n");
                *devframe = iter->cr;                
                return SYS_ERR_OK;
            }
            iter = iter->next;
        }
        // One way out of here might be to re-try with
        // a bigger maxlimit?
        DEBUG_ERR(err, "mm_alloc_range failed.\n");
        return err;
    }

    struct allocated_range* ar = calloc(sizeof(struct allocated_range), 1);
    assert(ar != NULL);
    ar->cr = *devframe;
    err = frame_identify(ar->cr, &ar->id);

    // Insert into the queue to track the allocation
    struct allocated_range** iter = &allocation_head;
    while(*iter != NULL) {
        iter = &(*iter)->next;
    }
    *iter = ar;

    return SYS_ERR_OK;
}

errval_t init_cap_manager(void)
{
    KALUGA_DEBUG("init_cap_manager\n");
    errval_t err, error_code;

    // Request I/O Cap
    struct monitor_blocking_binding *cl = get_monitor_blocking_binding();
    assert(cl != NULL);

    struct capref requested_cap;
    err = slot_alloc(&requested_cap);
    assert(err_is_ok(err));
    err = cl->rpc_tx_vtbl.get_io_cap(cl, &requested_cap, &error_code);
    assert(err_is_ok(err) && err_is_ok(error_code));

    // Initialize the memory allocator to handle PhysAddr caps
    static struct range_slot_allocator devframes_allocator;
    err = range_slot_alloc_init(&devframes_allocator, L2_CNODE_SLOTS, NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC_INIT);
    }

    struct frame_identity ret;
    err = frame_identify(requested_cap, &ret);
    size_t capbits= log2ceil(ret.bytes);
    assert (err_is_ok(err));
    assert((1ULL << capbits) == ret.bytes);

    err = mm_init(&register_manager, ObjType_DevFrame, ret.base, capbits, 1,
                  slab_default_refill, slot_alloc_dynamic, slot_refill_dynamic,
                  &devframes_allocator, false);
    if (err_is_fail(err)) {
        return err_push(err, MM_ERR_MM_INIT);
    }

    err= mm_add(&register_manager, requested_cap, capbits, ret.base);
    if (err_is_fail(err)) {
        return err_push(err, MM_ERR_MM_INIT);
    }
 
    KALUGA_DEBUG("init_cap_manager DONE\n");
    return SYS_ERR_OK;
}
