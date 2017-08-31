/**
 * \brief Memory management helper functions for device drivers.
 */
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/capabilities.h>

#include <driverkit/driverkit.h>

#define UNBITS_GENPA(bits) (((genpaddr_t)1) << (bits))

/**
 * \brief Maps device register with the capabilities provided by the
 * argcn slot.
 *
 * The function is used mostly as a helper to map registers by programs
 * that were spawned by Kaluga.
 *
 * \param[in] address The address of the device region you want to map.
 * \param[in] size The size of the region.
 * \param[out] return_address The virtual memory address where the region
 * was mapped at.
 *
 * \retval SYS_ERR_OK Mapping was succesful.
 */
errval_t map_device_register(lpaddr_t address, size_t size, lvaddr_t *return_address)
{
    errval_t err;
    struct cnoderef argcn_cnref = {
        .croot = CPTR_ROOTCN,
        .cnode = ROOTCN_SLOT_ADDR(ROOTCN_SLOT_ARGCN),
        .level = CNODE_TYPE_OTHER,
    };

    struct capref device_cap_iter = {
        .cnode = argcn_cnref,
        .slot = 0
    };

    for (; device_cap_iter.slot < L2_CNODE_SLOTS; device_cap_iter.slot++) {
        // Get cap data
        struct capability cap;
        err = debug_cap_identify(device_cap_iter, &cap);
        // If cap type was Null, kernel returns error
        if (err_no(err) == SYS_ERR_IDENTIFY_LOOKUP ||
            err_no(err) == SYS_ERR_CAP_NOT_FOUND ||
            err_no(err) == SYS_ERR_LMP_CAPTRANSFER_SRC_LOOKUP) {
            continue;
        }

        struct frame_identity fid;
        err = frame_identify(device_cap_iter, &fid);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Failure in frame_identify");
            return err;
        }
        assert(err_is_ok(err));

        lpaddr_t address_base = address & ~(BASE_PAGE_SIZE-1);
        lpaddr_t offset = address & (BASE_PAGE_SIZE-1);
        // XXX: should be address+size <= ...
        // Need to add proper register size
        if (address_base >= fid.base &&
                (address_base + size) <= (fid.base + fid.bytes)) {
            void* frame_base;
            err = vspace_map_one_frame_attr(&frame_base, size,
                                            device_cap_iter, VREGION_FLAGS_READ_WRITE_NOCACHE,
                                            NULL, NULL);
            *return_address = (lvaddr_t)frame_base + offset;
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "Failure in vspace_map_one_frame_attr.\n");
            }
            return err;
        }
    }

    return DRIVERKIT_ERR_NO_CAP_FOUND;
}

errval_t map_device_cap(struct capref device_cap, lvaddr_t *return_address) {
    struct frame_identity fid;
    errval_t err = frame_identify(device_cap, &fid);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Failure in frame_identify");
        return err;
    }
    return vspace_map_one_frame_attr((void**)return_address, fid.bytes,
                                    device_cap, VREGION_FLAGS_READ_WRITE_NOCACHE,
                                    NULL, NULL);
}
