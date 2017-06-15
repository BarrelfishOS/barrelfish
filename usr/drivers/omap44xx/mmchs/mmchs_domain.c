#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <driverkit/driverkit.h>
#include <barrelfish/nameservice_client.h>

#include "i2c.h"
#include "twl6030.h"
#include <arch/arm/omap44xx/device_registers.h>
#include <maps/omap44xx_map.h>

#include "cap_slots.h"

/// XXX: TODO not needed once kaluga <-> driver domain communicate over dcontrol iface
static errval_t find_cap_for(lpaddr_t address, size_t size, struct capref* out)
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
        // XXX: should be address+size <= ...
        // Need to add proper register size
        if (address_base >= fid.base &&
                (address_base + size) <= (fid.base + fid.bytes)) {
            *out = device_cap_iter;
            return SYS_ERR_OK;
        }
    }

    return DRIVERKIT_ERR_NO_CAP_FOUND;
}


int main(int argc, char** argv)
{
    size_t drivers = 0;
    struct bfdriver* cur = NULL;
    driverkit_list(&cur, &drivers);
    for (size_t i=0; i<drivers; i++) {
        printf("%s:%s:%d: Found device driver class = %s\n", __FILE__, __FUNCTION__, __LINE__, cur->name);
        cur += 1;
    }

    struct capref* caps = calloc(5, sizeof(struct capref));


    errval_t err = find_cap_for(OMAP44XX_CM2, 0x1000, &caps[CM2_SLOT]);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "finding cap failed.");
    }
    err = find_cap_for(OMAP44XX_CLKGEN_CM2, 0x1000, &caps[CLKGEN_CM2_SLOT]);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "finding cap failed.");
    }
    err = find_cap_for(i2c_get_pbase(I2C_HC), 0x1000, &caps[IC2_SLOT]);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "finding cap failed.");
    }
#define SYSCTRL_PADCONF_CORE 0x4a100000u
    err = find_cap_for(SYSCTRL_PADCONF_CORE, 0x1000, &caps[PADCONF_CORE_SLOT]);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "finding cap failed.");
    }
    err = find_cap_for(OMAP44XX_MAP_L4_PER_HSMMC1, OMAP44XX_MAP_L4_PER_HSMMC1_SIZE, &caps[L4_PER_HSMMC1_SLOT]);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "finding cap failed.");
    }

    iref_t dev, ctrl;
    driverkit_create_driver("mmchs", "mmchs_inst", caps, 1, NULL, 0, 0, &dev, &ctrl);

    messages_handler_loop();
    return 0;
}
