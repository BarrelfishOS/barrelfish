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
    //debug_printf("%s:%d: address=0x%x size=%u\n", __FUNCTION__, __LINE__, address, size);

    struct capref argcn = {
        .cnode = cnode_root,
        .slot = 10
    };
    //debug_cspace(argcn);

    size_t bits = 8; // TODO(gz): How do I figure this value out on the fly?
    struct capref device_cap_iter = {
        .cnode = build_cnoderef(argcn, bits),
        .slot = 0
    };
    //walk_cspace(build_cnoderef(argcn, bits), 0);


    for (size_t i = 0; i<12; i++) {
        struct frame_identity fid;
        errval_t err = invoke_frame_identify(device_cap_iter, &fid);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Failure in invoke_frame_identify");
            return err;
        }
        assert(err_is_ok(err));
        
        lpaddr_t address_base = address & ~(BASE_PAGE_SIZE-1);
        lpaddr_t offset = address & (BASE_PAGE_SIZE-1);
        // XXX: should be address+size <= ...
        // Need to add proper register size
        if (address_base >= fid.base &&
                (address_base + size) <= (fid.base + UNBITS_GENPA(fid.bits))) {
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

        device_cap_iter.slot += 1;
    }

    return DRIVERKIT_NO_CAP_FOUND;
}
