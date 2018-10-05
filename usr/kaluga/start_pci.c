/**
 * \file
 * \brief Code responsible for booting application cores
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>

#include <if/octopus_defs.h>
#include <if/octopus_thc.h>

//Manual channel setup includes
#include <if/pci_defs.h>
#include <if/kaluga_defs.h>
#include <pci/pci.h>
#include <driverkit/driverkit.h>
#include <driverkit/control.h>

#include <octopus/octopus.h>
#include <octopus/trigger.h>

#include <skb/skb.h>

#include <hw_records.h>

#include "kaluga.h"
#include <acpi_client/acpi_client.h>

static struct capref pci_ep;
static struct kaluga_binding* pci_binding;
static coreid_t pci_core = 0;
static coreid_t iommu_core = 0;
static uint32_t num_iommu_started = 0;

// TODO might needs some methods
static struct kaluga_rx_vtbl kaluga_rx_vtbl;

static void pci_change_event(octopus_mode_t mode, const char* device_record,
                             void* st);

static void spawnd_up_event(octopus_mode_t mode, const char* spawnd_record,
                            void* st)
{
    assert(mode & OCT_ON_SET);
    uint64_t iref;
    errval_t err = oct_read(spawnd_record, "_ { iref: %d }", &iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to read iref from spawnd record?");
    }

    // Pass the iref as state, this tells pci_change_event that we
    // don't need to look again for the spawnd iref
    // XXX: Pointer
    pci_change_event(OCT_ON_SET, st, (void*)(uintptr_t)iref);
}

static errval_t wait_for_spawnd(coreid_t core, void* state)
{
    // Check if the core we're spawning on is already up...
    struct octopus_thc_client_binding_t* cl = oct_get_thc_client();
    errval_t error_code;
    octopus_trigger_t t = oct_mktrigger(OCT_ERR_NO_RECORD,
            octopus_BINDING_EVENT, OCT_ON_SET, spawnd_up_event, state);

    // Construct service name
    static char* format = "spawn.%"PRIuCOREID" { iref: _ }";
    int length = snprintf(NULL, 0, format, core);
    char* query = malloc(length+1);
    snprintf(query, length+1, format, core);

    errval_t err = cl->call_seq.get(cl, query, t, NULL, NULL, &error_code);
    free(query);

    if (err_is_fail(err)) {
        return err;
    }

    return error_code;
};

/**
 * For devices store the endpoints to PCI and Kaluga
 * into driver_arg.
 */
static errval_t add_ep_args(struct pci_addr addr, struct pci_id id, coreid_t core, 
                            struct driver_argument *driver_arg, char* binary_name, 
                            char* module_name)
{
    errval_t err, out_err;
    // Endpoint to PCI
    struct capref pci_ep = {
        .cnode = driver_arg->argnode_ref,
        .slot = DRIVERKIT_ARGCN_SLOT_PCI_EP
    };

    struct capref pci_cap;
    err = slot_alloc(&pci_cap);
    if (err_is_fail(err)) {
        return err;
    }

    err = pci_binding->rpc_tx_vtbl.request_endpoint_cap(pci_binding, 
                       (core == pci_core)? IDC_ENDPOINT_LMP: IDC_ENDPOINT_UMP,
                       addr.bus, addr.device, 
                       addr.function, id.vendor, id.device, &pci_cap, &out_err);

    if (err_is_fail(err) || err_is_fail(out_err)) {
        //slot_free(pci_cap);
        return KALUGA_ERR_CAP_ACQUIRE;
    }

    err = cap_copy(pci_ep, pci_cap);
    if (err_is_fail(err)) {
        cap_destroy(pci_cap);
        return KALUGA_ERR_CAP_ACQUIRE;
    }


    // Endpoint to IOMMU
    struct capref iommu_ep = {
        .cnode = driver_arg->argnode_ref,
        .slot = DRIVERKIT_ARGCN_SLOT_IOMMU
    };

    struct capref iommu_cap;
    err = slot_alloc(&iommu_cap);
    if (err_is_fail(err)) {
        cap_destroy(pci_cap);
        return err;
    }

    if (num_iommu_started > 0) {
        err = pci_binding->rpc_tx_vtbl.request_iommu_endpoint_cap(pci_binding, 
                           (core == iommu_core)? IDC_ENDPOINT_LMP: IDC_ENDPOINT_UMP,
                           0,
                           addr.bus, addr.device, 
                           addr.function, &iommu_cap, &out_err);

        if (err_is_fail(err) || err_is_fail(out_err)) {
            cap_destroy(pci_cap);
           // slot_free(iommu_cap);
            return KALUGA_ERR_CAP_ACQUIRE;
        }

        err = cap_copy(iommu_ep, iommu_cap);
        if (err_is_fail(err)) {
            cap_destroy(pci_cap);
            cap_destroy(iommu_cap);
            return KALUGA_ERR_CAP_ACQUIRE;
        }
    }

    return err;
}

/**
 * For device at addr, finds and stores the interrupt arguments and caps
 * into driver_arg.
 */
static errval_t add_mem_args(struct pci_addr addr, struct driver_argument
                             *driver_arg, char *debug)
{
    errval_t err;

    struct device_mem *bars;
    size_t bars_len;

    //NOT VERY BEAUTIFUL
    static bool pci_init=false;
    if(!pci_init){
        pci_client_connect();
        pci_init = true;
    }
    //

    err = pci_get_bar_caps_for_device(addr, &bars, &bars_len);
    if(err_is_fail(err)){
        DEBUG_ERR(err, "pci_get_caps_for_device");
        return err;
    }

    // Copy the caps into the argument cnode
    for(int i=0; i<bars_len; i++){
        struct capref cap = {
            .cnode = driver_arg->argnode_ref,
            .slot = DRIVERKIT_ARGCN_SLOT_BAR0 + i
        };

        if(bars[i].type == 0){
            cap_copy(cap, bars[i].frame_cap);
        } else {
            return KALUGA_ERR_CAP_ACQUIRE;
        }

    }

    KALUGA_DEBUG("Received %zu bars\n", bars_len);
    return SYS_ERR_OK;
};

/**
 * For device at addr, finds and stores the interrupt arguments and caps
 * into driver_arg.
 */
static errval_t add_int_args(struct pci_addr addr, struct driver_argument *driver_arg, char *debug){
    errval_t err = SYS_ERR_OK;
    char debug_msg[100];
    strcpy(debug_msg, "none");
    // TODO: every driver should specify the int_model in device_db
    // until then, we treat them like legacy, so they can use the standard
    // pci client functionality.
    if(driver_arg->int_arg.model == INT_MODEL_LEGACY ||
       driver_arg->int_arg.model == INT_MODEL_NONE) {
        KALUGA_DEBUG("Starting driver with legacy interrupts\n");
        // No controller has to instantiated, but we need to get caps for the int numbers
        //err = skb_execute_query("get_pci_legacy_int_range(addr(%"PRIu8",%"PRIu8",%"PRIu8"),Li),"
        //        "writeln(Li).", addr.bus, addr.device, addr.function);
        err = skb_execute_query(
                "add_pci_controller(Lbl, addr(%"PRIu8",%"PRIu8",%"PRIu8")),"
                "write('\n'), print_int_controller(Lbl).",
                addr.bus, addr.device, addr.function);
        KALUGA_DEBUG("After Query.\n");
        if(err_is_fail(err)) DEBUG_SKB_ERR(err, "add/print pci controller");

    } else if(driver_arg->int_arg.model == INT_MODEL_MSI){
        printf("Kaluga: Starting driver with MSI interrupts\n");
        printf("Kaluga: MSI interrupts are not supported.\n");
        // TODO instantiate controller
    } else if(driver_arg->int_arg.model == INT_MODEL_MSIX){
        KALUGA_DEBUG("Starting driver with MSI-x interrupts\n");

        // TODO need to determine number of MSIx interrupts 
        
        // Add the pci_msix and msix controller
        err = skb_execute_query(
                "add_pci_msix_controller(PciMsixLbl, MsixLbl, addr(%"PRIu8",%"PRIu8",%"PRIu8"))"
                ",write('\n'),"
                "print_int_controller(PciMsixLbl),"
                "write(MsixLbl).",
                addr.bus, addr.device, addr.function);
        if(err_is_fail(err)) DEBUG_SKB_ERR(err, "add/print msix controller");
        char * lines[8];
        lines[0] = skb_get_output();
        for(int i=0; i<7; i++){
            if(lines[i] == NULL) break;
            lines[i+1] = strstr(lines[i], "\n");
            if(lines[i+1] != NULL) lines[i+1]++;
        }
        strncpy(driver_arg->int_arg.msix_ctrl_name, lines[2],
                sizeof(driver_arg->int_arg.msix_ctrl_name));
        KALUGA_DEBUG("Set msix_ctrl_name for (%d,%d,%d) to %s",
                     addr.bus, addr.device, addr.function, driver_arg->int_arg.msix_ctrl_name);

    } else {
        KALUGA_DEBUG("No interrupt model specified. No interrupts"
                " for this driver.\n");
    }

    if(err_is_fail(err)) return err;

    KALUGA_DEBUG("Setting up arguments.\n");
    // For debugging
    strncpy(debug_msg, skb_get_output(), sizeof(debug_msg));
    char * nl = strchr(debug_msg, '\n');
    if(nl) *nl = '\0';
    debug_msg[sizeof(debug_msg)-1] = '\0';

    KALUGA_DEBUG("SKB readout.\n");
    uint64_t start=0, end=0;
    char ctrl_label[64];
    // Format is: Lbl,Class,InLo,InHi,....
    err = skb_read_output("%*[^\n]\n%64[^,],%*[^,],%"SCNu64",%"SCNu64,
            ctrl_label,
            &start, &end);
    if(err_is_fail(err)) DEBUG_SKB_ERR(err, "read response");

    driver_arg->int_arg.int_range_start = start;
    driver_arg->int_arg.int_range_end = end;
    
    KALUGA_DEBUG("Debug msg.\n");
    //Debug message
    snprintf(debug_msg, sizeof(debug_msg),
            "lbl=%s,lo=%"PRIu64",hi=%"PRIu64,
            driver_arg->int_arg.msix_ctrl_name,
            start, end);

    err = store_int_cap(start, end, driver_arg);
    if(err_is_fail(err)){
        USER_PANIC_ERR(err, "store_int_cap");
    }
    if(debug) strcpy(debug, debug_msg);
    return SYS_ERR_OK;
}

static void pci_change_event(octopus_mode_t mode, const char* device_record,
                             void* st)
{
    errval_t err;
    char *binary_name = NULL;
    char *module_name = NULL;


    if (mode & OCT_ON_SET) {
        KALUGA_DEBUG("pci_change_event: device_record: %s\n", device_record);
        struct pci_addr addr;
        struct pci_id id;
        {
            uint64_t vendor_id, device_id, bus, dev, fun;
            err = oct_read(device_record, "_ { vendor: %d, device_id: %d, bus: %d, device: %d,"
                    " function: %d }",
                    &vendor_id, &device_id, &bus, &dev, &fun);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "Got malformed device record?");
            }
            addr = (struct pci_addr) {
                .bus = bus,
                .device = dev,
                .function = fun
            };

            id = (struct pci_id) {
                .device = device_id,
                .vendor = vendor_id
            };
        }

        /* duplicate device record as we may need it for later
         *
         * XXX: need to check that we are not leaking memory here
         *      device_record might be allocated using strdup() in
         *      oct_trigger_existing_and_watch() -> oct_get()
         */
        device_record = strdup(device_record);
        assert(device_record);


        // Ask the SKB which binary and where to start it...
        static char* query = "find_pci_driver(pci_card(%"PRIu16", %"PRIu16", _, _, _), Driver),"
                             "writeln(Driver).";
        err = skb_execute_query(query, id.vendor, id.device);
        if (err_no(err) == SKB_ERR_EXECUTION) {
            KALUGA_DEBUG("No PCI driver found for: VendorId=0x%"PRIx16", "
                         "DeviceId=0x%"PRIx16"\n",
                    id.vendor, id.device);
            goto out;
        }
        else if (err_is_fail(err)) {
            DEBUG_SKB_ERR(err, "Failed to query SKB.\n");
            goto out;
        }

        // XXX: Find better way to parse binary name from SKB
        size_t len = strlen(skb_get_output());
        binary_name = malloc(len);
        module_name = malloc(len);
        coreid_t core;
        uint8_t multi;
        uint8_t int_model_in;


        coreid_t offset;
        err = skb_read_output("driver(%"SCNu8", %"SCNu8", %"SCNu8", %[^,], %[^,], "
                "%"SCNu8")", &core, &multi, &offset, binary_name, module_name, &int_model_in);
        if(err_is_fail(err)){
            USER_PANIC_SKB_ERR(err, "Could not parse SKB output.\n");
        }

        KALUGA_DEBUG("PCI driver (%s %s) found for: VendorId=0x%"PRIx16" DeviceId=0x%"PRIx16" \n", 
                     binary_name, module_name, id.vendor, id.device);

        static int irqtests_started = 0;
        if(strstr(binary_name, "irqtest") != NULL){
            if(irqtests_started++ > 0){
                debug_printf("Not starting multiple instances of irqtest\n");
                goto out;
            }
        }

        struct module_info* mi = find_module(binary_name);
        if (mi == NULL) {
            KALUGA_DEBUG("Driver %s not loaded. Ignore.\n", binary_name);
            goto out;
        }
        KALUGA_DEBUG("Domain %s Driver %s \n", binary_name, module_name);

        // Wait until the core where we start the driver
        // is ready
        if (st == NULL && core != my_core_id) {
            err = wait_for_spawnd(core, (CONST_CAST)device_record);
            if (err_no(err) == OCT_ERR_NO_RECORD) {
                KALUGA_DEBUG("Core where driver %s runs is not up yet.\n",
                             mi->binary);
                // Don't want to free device record yet...
                return;
            }
            else if (err_is_fail(err)) {
                DEBUG_ERR(err, "Waiting for core %d failed?\n", core);
                goto out;
            }
        }

        struct driver_argument driver_arg;
        err = init_driver_argument(&driver_arg);
        if(err_is_fail(err)){
            USER_PANIC_ERR(err, "Could not initialize driver argument.\n");
        }


        driver_arg.module_name = module_name;
        driver_arg.int_arg.model = int_model_in;


        set_multi_instance(mi, multi);
        set_core_id_offset(mi, offset);

        debug_printf("Adding int args.\n");
        char intcaps_debug_msg[100];

        err = add_int_args(addr, &driver_arg, intcaps_debug_msg);
        assert(err_is_ok(err));

        debug_printf("Adding mem args.\n");
        char memcaps_debug_msg[100];
        err = add_mem_args(addr, &driver_arg, memcaps_debug_msg);
        assert(err_is_ok(err));

        debug_printf("Adding endpoint args.\n");
        err = add_ep_args(addr, id, core, &driver_arg, binary_name, module_name);
        assert(err_is_ok(err));

        // If we've come here the core where we spawn the driver
        // is already up
        printf("Kaluga: Starting \"%s\" (\"%s\") for (bus=%"PRIu16",dev=%"PRIu16",fun=%"PRIu16")"
               ", int: %s, on core %"PRIuCOREID"\n",
               binary_name, module_name, addr.bus, addr.device, addr.function, intcaps_debug_msg, core);

        err = mi->start_function(core, mi, (CONST_CAST)device_record, &driver_arg);
        switch (err_no(err)) {
        case SYS_ERR_OK:
            KALUGA_DEBUG("Spawned PCI driver: %s\n", mi->binary);
            set_started(mi);
            break;

        case KALUGA_ERR_DRIVER_ALREADY_STARTED:
            printf("%s already running. \n", mi->binary);
            break;

        case KALUGA_ERR_DRIVER_NOT_AUTO:
            KALUGA_DEBUG("%s not declared as auto, ignore.\n", mi->binary);
            break;

        default:
            DEBUG_ERR(err, "Unhandled error while starting %s\n", mi->binary);
            break;
        }
    }

out:
    free(binary_name);
    free(module_name);
}

errval_t watch_for_pci_devices(void)
{
    static char* pci_device  = "r'hw\\.pci\\.device\\.[0-9]+' { "
                               " bus: _, device: _, function: _, vendor: _,"
                               " device_id: _, class: _, subclass: _, "
                               " prog_if: _ }";
    octopus_trigger_id_t tid;
    return oct_trigger_existing_and_watch(pci_device, pci_change_event, NULL, &tid);
}

static int32_t predicate(void* data, void* arg)
{
    struct driver_instance* drv = (struct driver_instance*) data;
    if (strncmp(drv->inst_name, arg, strlen(drv->inst_name)) == 0) {
        return 1;
    }
    return 0;
}


static errval_t bridge_add_iommu_endpoint_args(struct driver_argument* arg)
{
    errval_t err;
    char** names;
    size_t len;

    // Get number of iommus
    err = oct_get_names(&names, &len, HW_PCI_IOMMU_RECORD_REGEX);
    if (err_is_fail(err)) {
        if (err == OCT_ERR_NO_RECORD) {
            return SYS_ERR_OK;
        }
        return err;
    }
    oct_free_names(names, len);
    
    if (len == 0) {
        // there are no arguments to load
        return SYS_ERR_OK;
    }

    // Get ep to iommus
    struct module_info* driver = find_module("iommu");
    if (driver == NULL) {
        // Do not start IOMMU
        num_iommu_started = 0;
        return KALUGA_ERR_MODULE_NOT_FOUND;
    }

    // Iommu module is around, assume we stared all the iommus
    num_iommu_started = len;

    struct capref iommu;
    char name[128];
    for (int i = 0; i < len; i++) {

        errval_t err;
        sprintf(name, "hw.pci.iommu.%d", i);
    
        struct driver_instance* drv = (struct driver_instance*) 
                                      collections_list_find_if(driver->driverinstance->spawned, 
                                                               predicate, name);
        assert(drv);
        
        struct capref cap = {
            .cnode = arg->argnode_ref,
            .slot = DRIVERKIT_ARGCN_SLOT_BAR0 + i
        };

        err = slot_alloc(&iommu);
        if (err_is_fail(err)) {
            return err;
        }
     
        // TODO can we assume that kluaga + iommu driver are on core 0?
        err = driverkit_get_driver_ep_cap(drv, &iommu, true);
        if (err_is_fail(err)) {
            return err;
        }
   
        err = cap_copy(cap, iommu);
        if (err_is_fail(err)) {
            slot_free(iommu);
            return err;
        }
    }
    

    return SYS_ERR_OK;
}

static void bridge_change_event(octopus_mode_t mode, const char* bridge_record,
                                void* st)
{
    if (mode & OCT_ON_SET) {
        // No need to ask the SKB as we always start pci for
        // in case we find a root bridge
        struct module_info* mi = find_module("pci");
        if (mi == NULL) {
            KALUGA_DEBUG("PCI driver not found or not declared as auto.");
            return;
        }

        if (is_started(mi)) {
            printf("Already started %s\n", mi->binary);
            return;
        }
        // XXX: always spawn on my_core_id; otherwise we need to check that
        // the other core is already up
    
        // create endpoint cap for Kaluga to PCI connection and setup a connection
        // to PCI. 
        errval_t err;
        struct driver_argument driver_arg;

        err = init_driver_argument(&driver_arg);
        if(err_is_fail(err)){
            USER_PANIC_ERR(err, "Could not initialize driver argument.\n");
        }

        pci_ep.cnode = driver_arg.argnode_ref;
        pci_ep.slot = DRIVERKIT_ARGCN_SLOT_KALUGA_EP;

        err = kaluga_create_endpoint(IDC_ENDPOINT_LMP, &kaluga_rx_vtbl, NULL,
                                     get_default_waitset(),
                                     IDC_ENDPOINT_FLAGS_DUMMY,
                                     &pci_binding, pci_ep);
 
        kaluga_rpc_client_init(pci_binding);

        err = bridge_add_iommu_endpoint_args(&driver_arg);
        if (err_is_fail(err)) {
            if (err == KALUGA_ERR_MODULE_NOT_FOUND) {
                debug_printf("IOMMU module not found, continue withouth IOMMU \n");
            } else {
                DEBUG_ERR(err, "Unhandled error while starting %s\n", mi->binary);
                cap_destroy(pci_ep);           
                return;
            }
        }

        err = mi->start_function(my_core_id, mi, (CONST_CAST)bridge_record, &driver_arg);
        switch (err_no(err)) {
        case SYS_ERR_OK:
            KALUGA_DEBUG("Spawned PCI bus driver: %s\n", mi->binary);
            set_started(mi);
            break;

        case KALUGA_ERR_DRIVER_ALREADY_STARTED:
            KALUGA_DEBUG("%s already running.\n", mi->binary);
            break;

        case KALUGA_ERR_DRIVER_NOT_AUTO:
            KALUGA_DEBUG("%s not declared as auto, ignore.\n", mi->binary);
            break;

        default:
            DEBUG_ERR(err, "Unhandled error while starting %s\n", mi->binary);
            break;
        }
    }
}

errval_t watch_for_pci_root_bridge(void)
{
    static char* root_bridge = "r'hw\\.pci\\.rootbridge\\.[0-9]+' { "
                               " bus: _, device: _, function: _, maxbus: _,"
                               " acpi_node: _ }";
    octopus_trigger_id_t tid;
    return oct_trigger_existing_and_watch(root_bridge, bridge_change_event,
            NULL, &tid);
}
