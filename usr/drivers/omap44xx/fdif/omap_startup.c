#include <barrelfish/barrelfish.h>
#include <barrelfish/capabilities.h>
#include <barrelfish/nameservice_client.h>

#include <stdio.h>
#include <stdlib.h>
#include <mm/mm.h>
#include <if/monitor_blocking_rpcclient_defs.h>

#include "fdif.h"

#define UNBITS_GENPA(bits) (((genpaddr_t)1) << (bits))

static struct mm register_manager;

/**
 * \brief Maps a physical register location into virtual address space
 *
 * \param[in] address Physical address of register you want to map.
 * \param[in] size Size of register space to map
 * \param[out] return_address Virtual address where the register is mapped at.
 *
 * \retval SYS_ERR_OK return_address is valid and mapped.
 **/
errval_t map_device_register(lpaddr_t address, size_t size, lvaddr_t** return_address) 
{
    struct allocated_range {
        struct capref cr;
        struct frame_identity id;
        struct allocated_range* next;
    };
    static struct allocated_range* allocation_head = NULL;

    FDIF_DEBUG("map_device_register: %"PRIxLPADDR" %zu %zu\n", address, size, log2ceil(size));

    assert(address != 0);
    assert(return_address != NULL);
    // TODO(gz) the paging in the kernel wants these two preconditions?
    assert(size % BASE_PAGE_SIZE == 0);
    assert(size >= BASE_PAGE_SIZE && "ARM paging breaks when smaller");

    bool insert = true;
    struct capref devframe = NULL_CAP;
    errval_t err = mm_alloc_range(&register_manager, log2ceil(size),
                                  address, address+size,
                                  &devframe, NULL);
    if (err_is_fail(err)) {
        // TODO(gz) Is there a better way to handle duplicated allocations?
        FDIF_DEBUG("mm_alloc_range failed.\n");
        FDIF_DEBUG("Do we already have an allocation that covers this range?\n");
        struct allocated_range* iter = allocation_head;
        while (iter != NULL) {
            if (address >= iter->id.base && 
                (address + size <= (iter->id.base + UNBITS_GENPA(iter->id.bits)))) {
                FDIF_DEBUG("Apparently, yes. We try to map that one.\n");
                devframe = iter->cr;
                insert = false;
                goto map_it; // yay, recovered!
            }
            iter = iter->next;
        }
        // One way out of here might be to re-try with
        // a bigger maxlimit?
        DEBUG_ERR(err, "mm_alloc_range failed.\n");
        return err;
    }

map_it:
    err = vspace_map_one_frame_attr((void**)return_address, size, 
                                    devframe, VREGION_FLAGS_READ_WRITE_NOCACHE,
                                    NULL, NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "vspace_map_one_frame_attr failed.\n");
        return err;
    }

    if (insert) {
        struct allocated_range* ar = calloc(sizeof(struct allocated_range), 1);
        ar->cr = devframe;
        err = invoke_frame_identify(ar->cr, &ar->id);
        if (err_is_fail(err)) {
            free(ar);
            DEBUG_ERR(err, "frame identity failed.\n");
            return err;
        }
        // Insert into the queue to track the allocation
        struct allocated_range** iter = &allocation_head;
        while(*iter != NULL) {
            iter = &(*iter)->next;
        }
        *iter = ar;
    }

    return SYS_ERR_OK;
}

errval_t init_memory_manager(void)
{
    FDIF_DEBUG("init_memory_manager\n");
    errval_t err, error_code;

    // Request I/O Cap
    struct monitor_blocking_rpc_client *cl = get_monitor_blocking_rpc_client();
    assert(cl != NULL);

    struct capref requested_cap;
    err = cl->vtbl.get_io_cap(cl, &requested_cap, &error_code);
    assert(err_is_ok(err) && err_is_ok(error_code));

    // Initialize the memory allocator to handle PhysAddr caps
    static struct range_slot_allocator devframes_allocator;
    err = range_slot_alloc_init(&devframes_allocator, 2048, NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC_INIT);
    }

    struct frame_identity ret;
    err = invoke_frame_identify(requested_cap, &ret);
    assert (err_is_ok(err));

    err = mm_init(&register_manager, ObjType_DevFrame, ret.base, ret.bits, 
                  1, slab_default_refill, slot_alloc_dynamic, 
                  &devframes_allocator, false);
    if (err_is_fail(err)) {
        return err_push(err, MM_ERR_MM_INIT);
    }

    err = mm_add(&register_manager, requested_cap,
                 30, 0x40000000);
    assert(err_is_ok(err));
 
    FDIF_DEBUG("init_memory_manager DONE\n");
    return SYS_ERR_OK;
}