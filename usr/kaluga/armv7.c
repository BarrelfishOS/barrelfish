/**
 * \file
 * \brief ARMv7 arch specfic code
 */

/*
 * Copyright (c) 2013, 2016 ETH Zurich.
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include "kaluga.h"

errval_t arch_startup(void)
{
    errval_t err = SYS_ERR_OK;
#if __pandaboard__
    debug_printf("Kaluga running on Pandaboard.\n");

    err = init_cap_manager();
    assert(err_is_ok(err));

    err = oct_set("all_spawnds_up { iref: 0 }");
    assert(err_is_ok(err));

    struct module_info* mi = find_module("fdif");
    if (mi != NULL) {
        err = mi->start_function(0, mi, "hw.arm.omap44xx.fdif {}");
        assert(err_is_ok(err));
    }
    mi = find_module("mmchs");
    if (mi != NULL) {
        err = mi->start_function(0, mi, "hw.arm.omap44xx.mmchs {}");
        assert(err_is_ok(err));
    }
    mi = find_module("mmchs2");
    if (mi != NULL) {
        err = mi->start_function(0, mi, "hw.arm.omap44xx.mmchs {}");
        assert(err_is_ok(err));
    }
    mi = find_module("prcm");
    if (mi != NULL) {
        err = mi->start_function(0, mi, "hw.arm.omap44xx.prcm {}");
        assert(err_is_ok(err));
    }
    mi = find_module("serial");
    if (mi != NULL) {
        err = mi->start_function(0, mi, "hw.arm.omap44xx.uart {}");
        assert(err_is_ok(err));
    }
    mi = find_module("sdma");
    if (mi != NULL) {
        err = mi->start_function(0, mi, "hw.arm.omap44xx.sdma {}");
        assert(err_is_ok(err));
    }

    mi = find_module("usb_manager");
    if (mi != NULL) {
#define USB_ARM_EHCI_IRQ 109
        char *buf = malloc(255);
        uint8_t offset = 0;
        mi->cmdargs = buf;
        mi->argc = 3;
        mi->argv[0] = mi->cmdargs + 0;

        snprintf(buf + offset, 255 - offset, "ehci\0");
        offset += strlen(mi->argv[0]) + 1;
        mi->argv[1] = mi->cmdargs + offset;
        snprintf(buf + offset, 255 - offset, "%u\0", 0xC00);
        offset += strlen(mi->argv[1]) + 1;
        mi->argv[2] = mi->cmdargs + offset;
        snprintf(buf+offset, 255-offset, "%u\0", USB_ARM_EHCI_IRQ);

        // XXX Use customized start function or add to module info
        err = mi->start_function(0, mi, "hw.arm.omap44xx.usb {}");
        assert(err_is_ok(err));
    }
#elif __gem5__
    printf("Kaluga running on GEM5 armv8.\n");

    err = init_cap_manager();
    assert(err_is_ok(err));

    err = oct_set("all_spawnds_up { iref: 0 }");
    assert(err_is_ok(err));

    struct module_info* mi = find_module("serial");
    if (mi != NULL) {
        err = mi->start_function(0, mi, "hw.arm.gem5.uart {}");
        assert(err_is_ok(err));
    }
#endif
    return err;
}
