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
#include <barrelfish_kpi/platform.h>
#include <if/monitor_blocking_rpcclient_defs.h>
#include "kaluga.h"

static errval_t omap44xx_startup(void)
{
    errval_t err;

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
    return SYS_ERR_OK;
}

static errval_t vexpress_startup(void)
{
    errval_t err;
    err = init_cap_manager();
    assert(err_is_ok(err));

    err = oct_set("all_spawnds_up { iref: 0 }");
    assert(err_is_ok(err));

    struct module_info* mi = find_module("serial_pl011");
    if (mi != NULL) {
        err = mi->start_function(0, mi, "hw.arm.vexpress.uart {}");
        assert(err_is_ok(err));
    }
    return SYS_ERR_OK;
}

static errval_t zynq7_startup(void)
{
    /* There's nothing special to do for Zynq (yet). */
    return SYS_ERR_OK;
}

errval_t arch_startup(char * add_device_db_file)
{
    errval_t err = SYS_ERR_OK;

    struct monitor_blocking_rpc_client *m = get_monitor_blocking_rpc_client();
    assert(m != NULL);

    uint32_t arch, platform;
    err = m->vtbl.get_platform(m, &arch, &platform);
    assert(err_is_ok(err));
    assert(arch == PI_ARCH_ARMV7A);

    switch(platform) {
        case PI_PLATFORM_OMAP44XX:
            debug_printf("Kaluga running on Pandaboard\n");
            return omap44xx_startup();
        case PI_PLATFORM_VEXPRESS:
            debug_printf("Kaluga running on VExpressEMM\n");
            return vexpress_startup();
        case PI_PLATFORM_ZYNQ7:
            debug_printf("Kaluga running on a Zynq7000\n");
            return zynq7_startup();
    }

    return KALUGA_ERR_UNKNOWN_PLATFORM;
}
