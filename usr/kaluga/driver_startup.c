/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/spawn_client.h>

#include <pci/pci.h> // for pci_address

#ifdef __arm__
#include <if/monitor_blocking_defs.h>
#endif

#ifdef __aarch64__
#include <if/monitor_blocking_defs.h>
#endif

#include "kaluga.h"

#if defined(__x86__) || defined(__ARM_ARCH_8A__)

// Add an argument to argc/argv pair. argv must be mallocd!
static void argv_push(int * argc, char *** argv, char * new_arg){
    int new_size = *argc + 1;
    //char ** argv_old = *argv;
    //*argv = malloc((new_size+1) * sizeof(char*));
    //memcpy(*argv, argv_old, sizeof(char*) * (*argc + 1));
    *argv = realloc(*argv, (new_size+1) * sizeof(char*)); // +1 for last NULL entry.
    if(*argv == NULL){
        USER_PANIC("Could not allocate argv memory");
    }
    *argc = new_size;
    (*argv)[new_size-1] = new_arg;
    (*argv)[new_size] = NULL;
}

errval_t default_start_function(coreid_t where,
                                struct module_info* mi,
                                char* record, struct driver_argument * arg)
{
    assert(mi != NULL);
    errval_t err = SYS_ERR_OK;
    coreid_t core;

    /*
     *  XXX: there may be more device using this driver, so starting it a second time
     *       may be needed.
     */
    if (!can_start(mi)) {
        return KALUGA_ERR_DRIVER_ALREADY_STARTED;
    }

    core = where + get_core_id_offset(mi);

    if (!is_auto_driver(mi)) {
        return KALUGA_ERR_DRIVER_NOT_AUTO;
    }

    // Construct additional command line arguments containing pci-id.
    // We need one extra entry for the new argument.
    char **argv = NULL;
    int argc = mi->argc;
    argv = malloc((argc+1) * sizeof(char *)); // +1 for trailing NULL
    assert(argv != NULL);
    memcpy(argv, mi->argv, (argc+1) * sizeof(char *));
    assert(argv[argc] == NULL);

    uint64_t vendor_id, device_id, bus, dev, fun;
    err = oct_read(record, "_ { bus: %d, device: %d, function: %d, vendor: %d, device_id: %d }",
                    &bus, &dev, &fun, &vendor_id, &device_id);

    char * int_arg_str = NULL;
    if(arg != NULL && arg->int_arg.model != 0){
        // This mallocs int_arg_str
        int_startup_argument_to_string(&(arg->int_arg), &int_arg_str);
        KALUGA_DEBUG("Adding int_arg_str: %s\n", int_arg_str);
        argv_push(&argc, &argv, int_arg_str);
    }

    char * pci_arg_str = NULL;
    if (err_is_ok(err)) {
        // We assume that we're starting a device if the query above succeeds
        // and therefore append the pci vendor and device id to the argument
        // list.
        pci_arg_str = malloc(26);
        // Make sure pci vendor and device id fit into our argument
        assert(vendor_id < 0xFFFF && device_id < 0xFFFF);
        snprintf(pci_arg_str, 26, "%04"PRIx64":%04"PRIx64":%04"PRIx64":%04"
                        PRIx64":%04"PRIx64, vendor_id, device_id, bus, dev, fun);

        argv_push(&argc, &argv, pci_arg_str);
    }


    //err = spawn_program(core, mi->path, argv,
    //                environ, 0, get_did_ptr(mi));

    struct capref arg_cap = NULL_CAP;
    if(arg != NULL){
       arg_cap = arg->arg_caps;
    }
    err = spawn_program_with_caps(core, mi->path, argv,
                    environ, NULL_CAP, arg_cap, 0, get_did_ptr(mi));

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Spawning %s failed.", mi->path);
    }

    free(argv);
    free(pci_arg_str);
    free(int_arg_str);

    return err;
}
#endif

errval_t start_networking(coreid_t core,
                          struct module_info* driver,
                          char* record, struct driver_argument * arg)
{
    assert(driver != NULL);
    errval_t err = SYS_ERR_OK;


    /* check if we are using the supplied pci address of eth0 */
    /*
    if (eth0.bus != 0xff || eth0.device != 0xff || eth0.function != 0xff) {
        err = oct_read(record, "_ { bus: %d, device: %d, function: %d, vendor: %d, device_id: %d }",
                            &bus, &dev, &fun, &vendor_id, &device_id);
        assert(err_is_ok(err));

        if ((eth0.bus != (uint8_t)bus)
             | (eth0.device != (uint8_t)dev)
             | (eth0.function != (uint8_t)fun)) {
            printf("start_networking: skipping card %" PRIu64 ":%" PRIu64 ":%"
                    PRIu64"\n", bus, dev, fun);
            printf("eth0 %" PRIu8 ":%" PRIu8 ":%"
                    PRIu8"\n", eth0.bus, eth0.device, eth0.function);
            return KALUGA_ERR_DRIVER_NOT_AUTO;
        }
    }
    */


    if (is_started(driver)) {
        printf("Already started %s\n", driver->binary);
        return KALUGA_ERR_DRIVER_ALREADY_STARTED;
    }

    if (!is_auto_driver(driver)) {
        printf("Not auto %s\n", driver->binary);
        return KALUGA_ERR_DRIVER_NOT_AUTO;
    }


    if (!(strcmp(driver->binary, "net_sockets_server") == 0)) {
        
        err = default_start_function(core, driver, record, arg);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Spawning %s failed.", driver->path);
            return err;
        }

        // cards with driver in seperate process
        struct module_info* net_sockets = find_module("net_sockets_server");
        if (net_sockets == NULL) {
            printf("Net sockets server not found\n");
            return KALUGA_ERR_DRIVER_NOT_AUTO;
        }

        uint64_t vendor_id, device_id, bus, dev, fun;
        err = oct_read(record, "_ { bus: %d, device: %d, function: %d, vendor: %d, device_id: %d }",
                       &bus, &dev, &fun, &vendor_id, &device_id);

        char* pci_arg_str = malloc(26);
        snprintf(pci_arg_str, 26, "%04"PRIx64":%04"PRIx64":%04"PRIx64":%04"
                        PRIx64":%04"PRIx64, vendor_id, device_id, bus, dev, fun);

        // Spawn net_sockets_server
        net_sockets->argv[0] = "net_sockets_server";
        net_sockets->argv[1] = "auto";
        net_sockets->argv[2] = driver->binary;
        net_sockets->argv[3] = pci_arg_str;

        err = spawn_program(core, net_sockets->path, net_sockets->argv, environ, 0,
                            get_did_ptr(net_sockets));
        free (pci_arg_str);
    } else {
        // TODO currently only for e1000, might be other cards that 
        // start the driver by creating a queue
        for (int i = 0; i < driver->argc; i++) {
            printf("argv[%d]=%s \n", i, driver->argv[i]);
        }        

        if (!(driver->argc > 2)) {
            driver->argv[driver->argc] = "e1000";        
            driver->argc++;
        }

        // All cards that start the driver by creating a device queue
        err = default_start_function(core, driver, record, arg);
    }

    return err;
}

/* errval_t start_usb_manager(void) */
/* { */

/*     struct module_info* driver = find_module("usb_manager"); */
/*     if (driver == NULL || !is_auto_driver(driver)) { */
/*         KALUGA_DEBUG("NGD_mng not found or not declared as auto."); */
/*         return KALUGA_ERR_DRIVER_NOT_AUTO; */
/*     } */

/*     debug_printf("doing pandaboard related setup...\n"); */
/*     errval_t err; */

/*     struct monitor_binding *cl = get_monitor_blocking_binding(); */
/*     assert(cl != NULL); */

/*     // Request I/O Cap */
/*     struct capref requested_caps; */
/*     errval_t error_code; */

/*     err = cl->rpc_tx_vtbl.get_io_cap(cl, &requested_caps, &error_code); */
/*     assert(err_is_ok(err) && err_is_ok(error_code)); */

/*     // Copy into correct slot */

/*     struct capref device_range_cap = NULL_CAP; */

/*     err = slot_alloc(&device_range_cap); */
/*     if (err_is_fail(err)) { */
/*         printf("slot alloc failed. Step 1\n"); */
/*         return (err); */
/*     } */
/*     struct capref tiler_cap = NULL_CAP; */

/*     err = slot_alloc(&tiler_cap); */
/*     if (err_is_fail(err)) { */
/*         DEBUG_ERR(err, "slot_alloc failed. Step 1\n"); */
/*         return (err); */
/*     } */

/*     err = cap_retype(device_range_cap, requested_caps, ObjType_DevFrame, 29); */

/*     struct capref l3_ocm_ram = NULL_CAP; */

/*     err = slot_alloc(&l3_ocm_ram); */
/*     if (err_is_fail(err)) { */
/*         DEBUG_ERR(err, "slot_alloc failed. Step 2\n"); */
/*         return (err); */
/*     } */

/*     err = cap_retype(l3_ocm_ram, device_range_cap, ObjType_DevFrame, 26); */
/*     if (err_is_fail(err)) { */
/*         DEBUG_ERR(err, "failed to retype the dev cap. Step 3\n"); */
/*         return (err); */
/*     } */

/*     struct capref l3_config_registers_cap; */
/*     err = slot_alloc(&l3_config_registers_cap); */
/*     if (err_is_fail(err)) { */
/*         DEBUG_ERR(err, "slot alloc failed. Step 4\n"); */
/*         return (err); */
/*     } */

/*     struct capref l4_domains_cap; */
/*     err = slot_alloc(&l4_domains_cap); */
/*     if (err_is_fail(err)) { */
/*         DEBUG_ERR(err, "slot_alloc failed. Step 5\n"); */
/*         return (err); */
/*     } */

/*     struct capref emif_registers_cap; */
/*     err = slot_alloc(&emif_registers_cap); */
/*     if (err_is_fail(err)) { */
/*         DEBUG_ERR(err, "slot_alloc failed. Step 6\n"); */
/*         return (err); */
/*     } */

/*     struct capref gpmc_iss_cap; */
/*     err = slot_alloc(&gpmc_iss_cap); */
/*     if (err_is_fail(err)) { */
/*         DEBUG_ERR(err, "slot_alloc failed. Step 7\n"); */
/*         return (err); */
/*     } */

/*     struct capref l3_emu_m3_sgx_cap; */
/*     err = slot_alloc(&l3_emu_m3_sgx_cap); */
/*     if (err_is_fail(err)) { */
/*         DEBUG_ERR(err, "slot_alloc failed. Step 8\n"); */
/*         return (err); */
/*     } */

/*     struct capref display_iva_cap; */
/*     err = slot_alloc(&display_iva_cap); */
/*     if (err_is_fail(err)) { */
/*         DEBUG_ERR(err, "slot_alloc failed. Step 9\n"); */
/*         return (err); */
/*     } */
/*     struct capref tmp_cap = display_iva_cap; */
/*     tmp_cap.slot++; */
/*     cap_delete(tmp_cap); */

/*     struct capref l4_PER_domain_cap; */
/*     err = slot_alloc(&l4_PER_domain_cap); */
/*     if (err_is_fail(err)) { */
/*         DEBUG_ERR(err, "slot_alloc failed. Step 12\n"); */
/*         return (err); */
/*     } */

/*     struct capref l4_ABE_domain_cap; */
/*     err = slot_alloc(&l4_ABE_domain_cap); */
/*     if (err_is_fail(err)) { */
/*         DEBUG_ERR(err, "slot_alloc failed. Step 11\n"); */
/*         return (err); */
/*     } */

/*     struct capref l4_CFG_domain_cap; */
/*     err = slot_alloc(&l4_CFG_domain_cap); */
/*     if (err_is_fail(err)) { */
/*         DEBUG_ERR(err, "slot_alloc failed. Step 12\n"); */
/*         return (err); */
/*     } */

/*     err = cap_retype(l4_PER_domain_cap, l4_domains_cap, ObjType_DevFrame, 24); */
/*     if (err_is_fail(err)) { */
/*         DEBUG_ERR(err, "failed to retype the cap. Step 13\n"); */
/*         return (err); */
/*     } */
/*     tmp_cap = l4_CFG_domain_cap; */
/*     tmp_cap.slot++; */
/*     cap_delete(tmp_cap); */

/*     struct frame_identity frameid;  // = {        0,        0    }; */

/*     err = invoke_frame_identify(l4_CFG_domain_cap, &frameid); */
/*     if (err_is_fail(err)) { */
/*         DEBUG_ERR(err, "could not identify the frame. Step 14\n"); */
/*     } */

/*     // get the 32 bit */
/*     uint32_t last = (uint32_t) (0xFFFFFFFF & (frameid.base)); */
/*     uint32_t size2 = frameid.bits; */

/*     /\* the L4 CFG domain cap must have address 0x4A000000 *\/ */
/*     assert(last == 0x4a000000); */

/*     /\* the size of the L4 CFG domain is 16k *\/ */
/*     assert(((1 << size2) / 1024) == (16 * 1024)); */

/* #define USB_SUBSYSTEM_L4_OFFSET 0x00062000 */
/* //#define USB_OHCI_OFFSET         (0x000A9000-USB_SUBSYSTEM_L4_OFFSET) */
/* #define USB_OHCI_OFFSET         0x00002800 */
/* #define USB_EHCI_OFFSET         0x00002C00 */

/* #define USB_ARM_EHCI_IRQ 109 */

/*     uint32_t tmp = (uint32_t) USB_EHCI_OFFSET + USB_SUBSYSTEM_L4_OFFSET; */

/*     char buf[255]; */
/*     uint8_t offset = 0; */
/*     driver->cmdargs = buf; */
/*     driver->argc = 3; */
/*     driver->argv[0] = driver->cmdargs+0; */

/*     snprintf(buf+offset, 255-offset, "ehci\0"); */
/*     offset += strlen(driver->argv[0])+1; */
/*     driver->argv[1] = driver->cmdargs+offset; */
/*     snprintf(buf+offset, 255-offset, "%u\0", tmp); */
/*     offset += strlen(driver->argv[1])+1; */
/*             driver->argv[2] = driver->cmdargs+offset; */
/*     snprintf(buf+offset, 255-offset, "%u\0", USB_ARM_EHCI_IRQ); */

/*     err = SYS_ERR_OK; */
/*     coreid_t core = 0; */

/*     if (is_started(driver)) { */
/*         debug_printf("driver is already started..."); */
/*         return KALUGA_ERR_DRIVER_ALREADY_STARTED; */
/*     } */

/*     err = spawn_program_with_caps(core, driver->path, driver->argv, environ, */
/*             NULL_CAP, l4_CFG_domain_cap, 0, &driver->did); */
/*     if (err_is_fail(err)) { */
/*         DEBUG_ERR(err, "Spawning %s failed.", driver->path); */
/*         return err; */
/*     } */

/*     return err; */
/* } */
