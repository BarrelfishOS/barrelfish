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
#include <flounder/flounder_txqueue.h>
//END
#include <pci/pci.h>

#include <octopus/octopus.h>
#include <octopus/trigger.h>

#include <skb/skb.h>
#include <thc/thc.h>

#include "kaluga.h"
#include <acpi_client/acpi_client.h>


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



static void init_pci_device_handler(struct pci_binding *b,
                                    uint32_t class_code, uint32_t sub_class,
                                    uint32_t prog_if, uint32_t vendor_id,
                                    uint32_t device_id,
                                    uint32_t bus, uint32_t dev, uint32_t fun)
{

    KALUGA_DEBUG("init_pci_device_handler!!! %"PRIu32", %"PRIu32","
            "%"PRIu32"\n", bus, dev, fun);

}
struct pci_rx_vtbl pci_rx_vtbl = {
    .init_pci_device_call = init_pci_device_handler
};

/**
 * \brief callback when the PCI client connects 
 *
 * \param st    state pointer
 * \param err   status of the connect
 * \param _b    created PCI binding
 */
static void pci_accept_cb(void *st,
                                  errval_t err,
                                  struct pci_binding *_b)
{
    KALUGA_DEBUG("connection accepted.");
    _b->rx_vtbl = pci_rx_vtbl;
}

const int PCI_CHANNEL_SIZE = 2048;

static errval_t frame_to_pci_frameinfo(struct capref frame, struct pci_frameinfo *fi){
    struct frame_identity fid;
    errval_t err;
    err = invoke_frame_identify(frame, &fid);
    if(err_is_fail(err)){
        DEBUG_ERR(err, "invoke_frame_identify");        
        return err;
    }
    KALUGA_DEBUG("pci ep frame base=0x%lx, size=0x%lx\n", fid.base, fid.bytes);

    void *msg_buf;
    err = vspace_map_one_frame(&msg_buf, fid.bytes, frame,
                               NULL, NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "vspace_map_one_frame");
        return err;
    }

    *fi = (struct pci_frameinfo) {
        .sendbase = (lpaddr_t)msg_buf + PCI_CHANNEL_SIZE,
        .inbuf = msg_buf,
        .inbufsize = PCI_CHANNEL_SIZE,
        .outbuf = ((uint8_t *) msg_buf) + PCI_CHANNEL_SIZE,
        .outbufsize = PCI_CHANNEL_SIZE
    };

    return SYS_ERR_OK;
} 

static errval_t start_pci_ump_accept(struct capref out_frame){
    assert(!capref_is_null(out_frame));

    size_t msg_frame_size;
    errval_t err;
    //err = frame_alloc(out_frame, 2 * PCI_CHANNEL_SIZE, &msg_frame_size);
    err = frame_create(out_frame, 2 * PCI_CHANNEL_SIZE, &msg_frame_size);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "frame_create");
        return err;
    }

    struct pci_frameinfo fi;
    err = frame_to_pci_frameinfo(out_frame, &fi);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "frame_to_frameinfo");
        return err;
    }

    KALUGA_DEBUG("creating channel on %p\n", fi.inbuf);

    err = pci_accept(&fi, NULL, pci_accept_cb,
                      get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "pci_accept");
        return err;
    }
    return SYS_ERR_OK;
}

/**
 * For device at addr, finds and stores the interrupt arguments and caps
 * into driver_arg.
 */
static errval_t add_pci_ep(struct pci_addr addr, struct driver_argument
        *driver_arg)
{
    errval_t err;

    struct capref cap = {
        .cnode = driver_arg->argnode_ref,
        .slot = PCIARG_SLOT_PCI_EP
    };

    err = start_pci_ump_accept(cap);
    if(err_is_fail(err)){
        DEBUG_ERR(err, "start_pci_ump_accept");
        return err;
    }

    return err;
};

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
            .slot = PCIARG_SLOT_BAR0 + i
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
                add.bus, addr.dev, addr.function, driver_arg->int_arg.msix_ctrl_name);

    } else {
        KALUGA_DEBUG("No interrupt model specified. No interrupts"
                " for this driver.\n");
    }

    if(err_is_fail(err)) return err;

    // For debugging
    strncpy(debug_msg, skb_get_output(), sizeof(debug_msg));
    char * nl = strchr(debug_msg, '\n');
    if(nl) *nl = '\0';
    debug_msg[sizeof(debug_msg)-1] = '\0';

    uint64_t start=0, end=0;
    char ctrl_label[64];
    // Format is: Lbl,Class,InLo,InHi,....
    err = skb_read_output("%*[^\n]\n%64[^,],%*[^,],%"SCNu64",%"SCNu64,
            ctrl_label,
            &start, &end);
    if(err_is_fail(err)) DEBUG_SKB_ERR(err, "read response");

    driver_arg->int_arg.int_range_start = start;
    driver_arg->int_arg.int_range_end = end;
    
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

        /* duplicate device record as we may need it for later */
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
        binary_name = malloc(strlen(skb_get_output()));
        coreid_t core;
        uint8_t multi;
        uint8_t int_model_in;

        struct driver_argument driver_arg;
        err = init_driver_argument(&driver_arg);
        if(err_is_fail(err)){
            USER_PANIC_ERR(err, "Could not initialize driver argument.\n");
        }
        coreid_t offset;
        err = skb_read_output("driver(%"SCNu8", %"SCNu8", %"SCNu8", %[^,], "
                "%"SCNu8")", &core, &multi, &offset, binary_name, &int_model_in);
        if(err_is_fail(err)){
            USER_PANIC_SKB_ERR(err, "Could not parse SKB output.\n");
        }

        driver_arg.int_arg.model = int_model_in;

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

        set_multi_instance(mi, multi);
        set_core_id_offset(mi, offset);

        // Build up the driver argument
        err = add_pci_ep(addr, &driver_arg);
        assert(err_is_ok(err));

        char intcaps_debug_msg[100];
        err = add_int_args(addr, &driver_arg, intcaps_debug_msg);
        assert(err_is_ok(err));

        char memcaps_debug_msg[100];
        err = add_mem_args(addr, &driver_arg, memcaps_debug_msg);
        assert(err_is_ok(err));


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

        // If we've come here the core where we spawn the driver
        // is already up
        printf("Kaluga: Starting \"%s\" for (bus=%"PRIu16",dev=%"PRIu16",fun=%"PRIu16")"
               ", int: %s, on core %"PRIuCOREID"\n",
               binary_name, addr.bus, addr.device, addr.function, intcaps_debug_msg, core);

        err = mi->start_function(core, mi, (CONST_CAST)device_record, &driver_arg);
        switch (err_no(err)) {
        case SYS_ERR_OK:
            KALUGA_DEBUG("Spawned PCI driver: %s\n", mi->binary);
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

out:
    free(binary_name);
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

        // XXX: always spawn on my_core_id; otherwise we need to check that
        // the other core is already up
        errval_t err = mi->start_function(my_core_id, mi, (CONST_CAST)bridge_record, NULL);
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
