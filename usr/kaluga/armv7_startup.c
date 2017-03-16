/**
 * \file
 * \brief Provides a generic startup function for the ARM OMAP platform
 */
/*
 * Copyright (c) 2013, ETH Zurich.
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/spawn_client.h>
#include <barrelfish_kpi/platform.h>
#include <if/monitor_blocking_defs.h>

#include <arch/arm/omap44xx/device_registers.h>
#include <maps/omap44xx_map.h>
#include <maps/vexpress_map.h>

#include "kaluga.h"

struct allowed_registers
{
    char* binary;
    lpaddr_t registers[][2];
};

static struct allowed_registers usb = {
    .binary = "hw.arm.omap44xx.usb",
    .registers =
    {
        {OMAP44XX_MAP_L4_CFG_HSUSBHOST, OMAP44XX_MAP_L4_CFG_HSUSBHOST_SIZE},
        {OMAP44XX_MAP_L4_CFG_HSUSBTLL, OMAP44XX_MAP_L4_CFG_HSUSBTLL_SIZE},
        {OMAP44XX_MAP_L4_WKUP_SRCM, OMAP44XX_MAP_L4_WKUP_SRCM_SIZE},
        {OMAP44XX_MAP_L4_WKUP_SYSCTRL_PADCONF_WKUP, OMAP44XX_MAP_L4_WKUP_SYSCTRL_PADCONF_WKUP_SIZE},
        {OMAP44XX_MAP_L4_CFG_SYSCTRL_PADCONF_CORE, OMAP44XX_MAP_L4_CFG_SYSCTRL_PADCONF_CORE_SIZE},
        {OMAP44XX_MAP_L4_CFG_CM2, OMAP44XX_MAP_L4_CFG_CM2_SIZE},
        {OMAP44XX_MAP_L4_WKUP_GPIO1, OMAP44XX_MAP_L4_WKUP_GPIO1_SIZE},
        {OMAP44XX_MAP_L4_PER_GPIO2, OMAP44XX_MAP_L4_PER_GPIO2_SIZE},
        {OMAP44XX_MAP_L4_WKUP_PRM, OMAP44XX_MAP_L4_WKUP_PRM_SIZE},
        {0x0, 0x0}
    }
};

static struct allowed_registers fdif = {
    .binary = "hw.arm.omap44xx.fdif",
    .registers =
    {
        {OMAP44XX_CAM_CM2, 0x1000},
        {OMAP44XX_DEVICE_PRM, 0x1000},
        {OMAP44XX_CAM_PRM, 0x1000},
        {OMAP44XX_MAP_L4_CFG_FACE_DETECT,OMAP44XX_MAP_L4_CFG_FACE_DETECT_SIZE},
        {0x0, 0x0}
    }
};

static struct allowed_registers mmchs = {
    .binary = "hw.arm.omap44xx.mmchs",
    .registers =
    {
        {OMAP44XX_CM2,        0x1000},
        {OMAP44XX_CLKGEN_CM2, 0x1000},
        {OMAP44XX_L4PER_CM2,  0x1000},
        // i2c
        {OMAP44XX_MAP_L4_PER_I2C1, OMAP44XX_MAP_L4_PER_I2C1_SIZE},
        {OMAP44XX_MAP_L4_PER_I2C2, OMAP44XX_MAP_L4_PER_I2C2_SIZE},
        {OMAP44XX_MAP_L4_PER_I2C3, OMAP44XX_MAP_L4_PER_I2C3_SIZE},
        {OMAP44XX_MAP_L4_PER_I2C4, OMAP44XX_MAP_L4_PER_I2C4_SIZE},
        // ctrlmodules
        {OMAP44XX_MAP_L4_CFG_SYSCTRL_GENERAL_CORE, OMAP44XX_MAP_L4_CFG_SYSCTRL_GENERAL_CORE_SIZE},
        {OMAP44XX_MAP_L4_WKUP_SYSCTRL_GENERAL_WKUP, OMAP44XX_MAP_L4_WKUP_SYSCTRL_GENERAL_WKUP_SIZE},
        {OMAP44XX_MAP_L4_CFG_SYSCTRL_PADCONF_CORE, OMAP44XX_MAP_L4_CFG_SYSCTRL_PADCONF_CORE_SIZE},
        {OMAP44XX_MAP_L4_WKUP_SYSCTRL_PADCONF_WKUP, OMAP44XX_MAP_L4_WKUP_SYSCTRL_PADCONF_WKUP_SIZE},
        // MMCHS
        {OMAP44XX_MAP_L4_PER_HSMMC1, OMAP44XX_MAP_L4_PER_HSMMC1_SIZE},
        {OMAP44XX_MAP_L4_PER_HSMMC2, OMAP44XX_MAP_L4_PER_HSMMC2_SIZE},
        {OMAP44XX_MAP_L4_PER_MMC_SD3, OMAP44XX_MAP_L4_PER_MMC_SD3_SIZE},
        {OMAP44XX_MAP_L4_PER_MMC_SD4, OMAP44XX_MAP_L4_PER_MMC_SD4_SIZE},
        {OMAP44XX_MAP_L4_PER_MMC_SD5, OMAP44XX_MAP_L4_PER_MMC_SD5_SIZE},
        {0x0, 0x0}
    }
};

static struct allowed_registers prcm = {
    .binary = "hw.arm.omap44xx.prcm",
    .registers =
    {
        {OMAP44XX_MAP_L4_WKUP_PRM, OMAP44XX_MAP_L4_WKUP_PRM_SIZE},
        {OMAP44XX_DEVICE_PRM, 0x1000},
        {OMAP44XX_MAP_L4_PER_I2C1, OMAP44XX_MAP_L4_PER_I2C1_SIZE},
        {OMAP44XX_MAP_L4_PER_I2C2, OMAP44XX_MAP_L4_PER_I2C2_SIZE},
        {OMAP44XX_MAP_L4_PER_I2C3, OMAP44XX_MAP_L4_PER_I2C3_SIZE},
        {OMAP44XX_MAP_L4_PER_I2C4, OMAP44XX_MAP_L4_PER_I2C4_SIZE},
        {0x0, 0x0}
    }
};

static struct allowed_registers omap_uart = {
    .binary = "hw.arm.omap44xx.uart",
    .registers =
    {
        {OMAP44XX_MAP_L4_PER_UART1,OMAP44XX_MAP_L4_PER_UART1_SIZE},
        {OMAP44XX_MAP_L4_PER_UART2,OMAP44XX_MAP_L4_PER_UART2_SIZE},
        {OMAP44XX_MAP_L4_PER_UART3,OMAP44XX_MAP_L4_PER_UART3_SIZE},
        {OMAP44XX_MAP_L4_PER_UART4,OMAP44XX_MAP_L4_PER_UART4_SIZE},
        {0x0, 0x0}
    }
};

static struct allowed_registers sdma = {
    .binary = "hw.arm.omap44xx.sdma",
    .registers =
    {
        {OMAP44XX_MAP_L4_CFG_SDMA, OMAP44XX_MAP_L4_CFG_SDMA_SIZE},
        {0x0, 0x0}
    }
};

static struct allowed_registers* omap44xx[] = {
    &usb,
    &fdif,
    &mmchs,
    &prcm,
    &omap_uart,
    &sdma,
    NULL,
};

static struct allowed_registers vexpress_uart = {
    .binary = "hw.arm.vexpress.uart",
    .registers =
    {
        {VEXPRESS_MAP_UART0, VEXPRESS_MAP_UART0_SIZE},
        {VEXPRESS_MAP_UART1, VEXPRESS_MAP_UART1_SIZE},
        {VEXPRESS_MAP_UART2, VEXPRESS_MAP_UART2_SIZE},
        {VEXPRESS_MAP_UART3, VEXPRESS_MAP_UART3_SIZE},
        {0x0, 0x0}
    }
};

static struct allowed_registers* vexpress[] = {
    &vexpress_uart,
    NULL,
};

/**
 * \brief Startup function for ARMv7 drivers.
 *
 * Makes sure we get the device register capabilities.
 */
errval_t
default_start_function(coreid_t where, struct module_info* driver,
        char* record, struct driver_argument* int_arg)
{
    assert(driver != NULL);
    assert(record != NULL);

    errval_t err;

    struct monitor_blocking_binding *m=
        get_monitor_blocking_binding();
    assert(m != NULL);

    uint32_t arch, platform;
    err = m->rpc_tx_vtbl.get_platform(m, &arch, &platform);
    assert(err_is_ok(err));
    assert(arch == PI_ARCH_ARMV7A);

    struct allowed_registers **regs= NULL;
    switch(platform) {
        case PI_PLATFORM_OMAP44XX:
            regs= omap44xx;
            break;
        case PI_PLATFORM_VEXPRESS:
            regs= vexpress;
            break;
        default:
            printf("Unrecognised ARMv7 platform\n");
            abort();
    }

    // TODO Request the right set of caps and put in device_range_cap
    struct cnoderef dev_cnode;
    struct capref dev_cnode_cap;
    err = cnode_create_l2(&dev_cnode_cap, &dev_cnode);
    assert(err_is_ok(err));

    struct capref device_cap;
    device_cap.cnode = dev_cnode;
    device_cap.slot = 0;

    char* name;
    err = oct_read(record, "%s", &name);
    assert(err_is_ok(err));
    KALUGA_DEBUG("%s:%d: Starting driver for %s\n", __FUNCTION__, __LINE__, name);
    for (size_t i=0; regs[i] != NULL; i++) {

        if(strcmp(name, regs[i]->binary) != 0) {
            continue;
        }

        // Get the device cap from the managed capability tree
        // put them all in a single cnode
        for (size_t j=0; regs[i]->registers[j][0] != 0x0; j++) {
            struct capref device_frame;
            KALUGA_DEBUG("%s:%d: mapping 0x%"PRIxLPADDR" %"PRIuLPADDR"\n", __FUNCTION__, __LINE__,
                   regs[i]->registers[j][0], regs[i]->registers[j][1]);

            lpaddr_t base = regs[i]->registers[j][0] & ~(BASE_PAGE_SIZE-1);
            err = get_device_cap(base,
                                 regs[i]->registers[j][1],
                                 &device_frame);
            assert(err_is_ok(err));

            KALUGA_DEBUG("get_device_cap worked\n");

            err = cap_copy(device_cap, device_frame);
            assert(err_is_ok(err));
            device_cap.slot++;
        }
    }
    free(name);

    err = spawn_program_with_caps(0, driver->path, driver->argv, environ,
            NULL_CAP, dev_cnode_cap, 0, driver->did);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Spawning %s failed.", driver->path);
        return err;
    }

    return SYS_ERR_OK;
}
