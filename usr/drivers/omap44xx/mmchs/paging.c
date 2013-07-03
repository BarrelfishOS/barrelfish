#include <barrelfish/barrelfish.h>

uintptr_t map_device_cap(void)
{
    errval_t err;

    struct capref dev_cap = {
        .cnode = cnode_root,
        .slot = ROOTCN_SLOT_ARGCN
    };

    struct frame_identity frameid;

    err = invoke_frame_identify(dev_cap, &frameid);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not identify the frame.\n");
        return (0);
    }

    void *ret_addr = NULL;
    size_t size = (1UL << frameid.bits); /* bytes */

    err = vspace_map_one_frame_attr(&ret_addr, size, dev_cap,
            VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to create a vspace mapping.\n");
        return (0);
    }

    return ((uintptr_t) ret_addr);
}