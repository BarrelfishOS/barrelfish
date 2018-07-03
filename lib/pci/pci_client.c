/**
 * \file
 * \brief PCI service client-side logic
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/dispatch.h>
#include <barrelfish/inthandler.h>
#include <pci/pci.h>
#include <pci/pci_client_debug.h>
#include <if/pci_defs.h>
#include <if/acpi_defs.h>
#include <acpi_client/acpi_client.h>
#include <int_route/int_model.h>
#include <int_route/int_route_client.h>

#define INVALID_VECTOR ((uint64_t)-1)
#define INVALID_VECTOR_32 ((uint32_t)-1)

static struct pci_binding *pci_client = NULL;


/*
 * Parse the int_model=
 */
static struct int_startup_argument int_arg;
static bool int_arg_found = false;

errval_t pci_parse_int_arg(int argc, char ** argv) {
    errval_t err;
    for(int i=0; i < argc; i++){
        err = int_startup_argument_parse(argv[i], &int_arg);
        if(err_is_ok(err)){
            int_arg_found = true;
            return err;
        }
    }
    return SYS_ERR_IRQ_INVALID;
}

errval_t pci_reregister_irq_for_device(uint32_t class, uint32_t subclass, uint32_t prog_if,
                                       uint32_t vendor, uint32_t device,
                                       uint32_t bus, uint32_t dev, uint32_t fun,
                                       interrupt_handler_fn handler,
                                       void *handler_arg,
                                       interrupt_handler_fn reloc_handler,
                                       void *reloc_handler_arg)
{
    uint64_t vector = INVALID_VECTOR;
    errval_t err, msgerr;

    if (handler != NULL && reloc_handler != NULL) {
        // register movable interrupt
        err = inthandler_setup_movable(handler, handler_arg, reloc_handler,
                reloc_handler_arg, &vector);
        if (err_is_fail(err)) {
            return err;
        }

        assert(vector != INVALID_VECTOR);
    } else if (handler != NULL) {
        // register non-movable interrupt
        err = inthandler_setup(handler, handler_arg, &vector);
        if (err_is_fail(err)) {
            return err;
        }

        assert(vector != INVALID_VECTOR);
    }

    err = pci_client->rpc_tx_vtbl.
        reregister_interrupt(pci_client, class, subclass, prog_if, vendor,
                device, bus, dev, fun, disp_get_current_core_id(),
                vector, &msgerr);
    if (err_is_fail(err)) {
        return err;
    } else if (err_is_fail(msgerr)) {
        return msgerr;
    }
    return SYS_ERR_OK;
}

static errval_t check_src_capability(struct capref irq_src_cap){
    struct capability irq_src_cap_data;
    errval_t err;
    err = debug_cap_identify(irq_src_cap, &irq_src_cap_data);
    if(err_is_fail(err)){
        DEBUG_ERR(err, "Could not identify cap?");
        return err;
    }
    if(irq_src_cap_data.type != ObjType_IRQSrc){
        PCI_CLIENT_DEBUG("First cap argument ist not of type IRQSrc (is=%d)."
                "Driver not started by kaluga?\n", irq_src_cap_data.type);
        return SYS_ERR_IRQ_NOT_IRQ_TYPE;
    }
    return SYS_ERR_OK;
}

/**
 * This function does all the interrupt routing setup. It uses the interrupt source
 * capability passed from kaluga out of the cspace.
 * The source capability contains a range of vectors, irq_idx is an offset into
 * this range, making it convenient for the clients, as they don't have 
 * to care about the absolute value, but get their local view on interrupt numbers.
 * It allocates an interrupt destination capability from the monitor.
 * It sets up a route between these two using the interrupt routing service
 * It registers the handler passed as an argument as handler for the int destination
 * capability.
 * Finally, it instructs the PCI service to activate interrupts for this card.
 */
errval_t pci_setup_int_routing(int irq_idx, interrupt_handler_fn handler,
                                         void *handler_arg,
                                         interrupt_handler_fn reloc_handler,
                                         void *reloc_handler_arg){
    struct capref irq_src_cap;
    irq_src_cap.cnode = build_cnoderef(cap_argcn, CNODE_TYPE_OTHER);
    irq_src_cap.slot = PCIARG_SLOT_INT;

    return pci_setup_int_routing_with_cap(irq_idx, irq_src_cap, handler,
            handler_arg, reloc_handler, reloc_handler_arg);
}


/**
 * This function does all the interrupt routing setup. It uses the interrupt source
 * capability passed from kaluga out of the cspace.
 * The source capability contains a range of vectors, irq_idx is an offset into
 * this range, making it convenient for the clients, as they don't have 
 * to care about the absolute value, but get their local view on interrupt numbers.
 * It allocates an interrupt destination capability from the monitor.
 * It sets up a route between these two using the interrupt routing service
 * It registers the handler passed as an argument as handler for the int destination
 * capability.
 * Finally, it instructs the PCI service to activate interrupts for this card.
 */
errval_t pci_setup_int_routing_with_cap(int irq_idx, 
                                        struct capref irq_src_cap,
                                        interrupt_handler_fn handler,
                                        void *handler_arg,
                                        interrupt_handler_fn reloc_handler,
                                        void *reloc_handler_arg){

    errval_t err;
    errval_t msgerr;
    err = check_src_capability(irq_src_cap);
    if(err_is_fail(err)){
        USER_PANIC_ERR(err, "No interrupt capability");
        return err;
    }

    // For now, we just use the first vector passed to the driver.
    uint64_t gsi = INVALID_VECTOR;
    err = invoke_irqsrc_get_vec_start(irq_src_cap, &gsi);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Could not lookup GSI vector");
        return err;
    }
    PCI_CLIENT_DEBUG("Got irqsrc cap, gsi: %"PRIu64"\n", gsi);

    // Get irq_dest_cap from monitor
    struct capref irq_dest_cap;
    err = alloc_dest_irq_cap(&irq_dest_cap);
    if(err_is_fail(err)){
        DEBUG_ERR(err, "Could not allocate dest irq cap");
        return err;
    }
    uint64_t irq_dest_vec = INVALID_VECTOR;
    err = invoke_irqdest_get_vector(irq_dest_cap, &irq_dest_vec);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Could not lookup irq vector");
        return err;
    }
    PCI_CLIENT_DEBUG("Got dest cap, vector: %"PRIu64"\n", irq_dest_vec);


    err = int_route_client_route(irq_src_cap, irq_idx, irq_dest_cap);
    if(err_is_fail(err)){
        DEBUG_ERR(err, "Could not set up route.");
        return err;
    } else {
        PCI_CLIENT_DEBUG("Int route set-up success.\n");
    }

    if(handler != NULL){
        err = inthandler_setup_movable_cap(irq_dest_cap, handler, handler_arg,
                reloc_handler, reloc_handler_arg);
        if (err_is_fail(err)) {
            return err;
        }
    }

    // Activate PCI interrupt
    err = pci_client->rpc_tx_vtbl.irq_enable(pci_client, &msgerr);
    assert(err_is_ok(err));
    if(err_is_fail(msgerr)){
        DEBUG_ERR(msgerr, "irq_enable");
        return msgerr;
    }
    return err;
}


/**
 * This function is used by kaluga to retrieve the caps for the specified
 * device. This function will malloc bars and store it in bars_out & bars_len.
 * addr is not allowed to contain dont cares
 */
errval_t pci_get_bar_caps_for_device(
        struct pci_addr addr,
        struct device_mem **bars_out,
        size_t *bars_len)
{
    assert(addr.bus != PCI_DONT_CARE);
    assert(addr.device != PCI_DONT_CARE);
    assert(addr.function != PCI_DONT_CARE);
    assert(bars_out);
    assert(bars_len);

    uint8_t nbars = 0;
    errval_t err, msgerr;

    err = pci_client->rpc_tx_vtbl.
        init_pci_device(pci_client,
                PCI_DONT_CARE, PCI_DONT_CARE, PCI_DONT_CARE,
                PCI_DONT_CARE, PCI_DONT_CARE,
                addr.bus, addr.device, addr.function, &msgerr,
                &nbars);

    if (err_is_fail(err)) {
        PCI_CLIENT_DEBUG("init pci device failed.\n");
        return err;
    } else if (err_is_fail(msgerr)) {
        PCI_CLIENT_DEBUG("init pci device failed.\n");
        return msgerr;
    }
    assert(nbars > 0); // otherwise we should have received an error!

    struct device_mem *bars = calloc(nbars, sizeof(struct device_mem));
    assert(bars != NULL);

    // request caps for all bars of device
    for (int nb = 0; nb < nbars; nb++) {
        struct device_mem *bar = &bars[nb];

        struct capref cap;
        uint8_t type;

        err = slot_alloc(&cap);
        assert(err_is_ok(err));
        err = pci_client->rpc_tx_vtbl.get_bar_cap(pci_client, nb, &msgerr, &cap,
                                       &type, &bar->bar_nr);
        if (err_is_fail(err) || err_is_fail(msgerr)) {
            if (err_is_ok(err)) {
                err = msgerr;
            }
            DEBUG_ERR(err, "requesting cap for BAR %d of device", nb);
            goto err_out;
        }

        if (type == 0) { // Frame cap BAR
            bar->frame_cap = cap;

            struct frame_identity fid = { .base = 0, .bytes = 0 };
            err = frame_identify(cap, &fid);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "frame identify failed.");
            }
            bar->paddr = fid.base;
            bar->bits = log2ceil(fid.bytes);
            bar->bytes = fid.bytes;
        } else { // IO BAR
            bar->io_cap = cap;
            err = cap_copy(cap_io, cap);
            if(err_is_fail(err) && err_no(err) != SYS_ERR_SLOT_IN_USE) {
                DEBUG_ERR(err, "cap_copy for IO cap");
                goto err_out;
            }
        }
    }

    // initialize the device. We have all the caps now
    PCI_CLIENT_DEBUG("Succesfully done with pci init.\n");
    *bars_out = bars;
    *bars_len = nbars;
    return SYS_ERR_OK;

 err_out:
    free(bars);
    return err;
}

errval_t pci_register_driver_movable_irq(pci_driver_init_fn init_func,
                                         void *user_state, uint32_t class,
                                         uint32_t subclass, uint32_t prog_if,
                                         uint32_t vendor, uint32_t device,
                                         uint32_t bus, uint32_t dev, uint32_t fun,
                                         interrupt_handler_fn handler,
                                         void *handler_arg,
                                         interrupt_handler_fn reloc_handler,
                                         void *reloc_handler_arg)
{
    uint8_t nbars;
    errval_t err, msgerr;

    err = pci_client->rpc_tx_vtbl.
        init_pci_device(pci_client, class, subclass, prog_if, vendor,
                        device, bus, dev, fun, &msgerr,
                        &nbars);

    if (err_is_fail(err)) {
        PCI_CLIENT_DEBUG("init pci device failed.\n");
        return err;
    } else if (err_is_fail(msgerr)) {
        return msgerr;
    }
    assert(nbars > 0); // otherwise we should have received an error!

    // Set-up int routing.
    // We use the first passed vector of the device,
    // for backward compatibility with function interface.
    if (handler != NULL) {
        PCI_CLIENT_DEBUG("Calling pci_setup_int_routing\n");
        err = pci_setup_int_routing(0, handler, handler_arg, reloc_handler, reloc_handler_arg);
        if(err_is_fail(err)){
           DEBUG_ERR(err, "Could not set up int routing. Continuing w/o interrupts");
        }
    }

    // FIXME: leak
    struct device_mem *bars = calloc(nbars, sizeof(struct device_mem));
    assert(bars != NULL);

    // request caps for all bars of device
    for (int nb = 0; nb < nbars; nb++) {
        struct device_mem *bar = &bars[nb];

        struct capref cap;
        uint8_t type;

        err = slot_alloc(&cap);
        assert(err_is_ok(err));
        err = pci_client->rpc_tx_vtbl.get_bar_cap(pci_client, nb, &msgerr, &cap,
                                       &type, &bar->bar_nr);
        if (err_is_fail(err) || err_is_fail(msgerr)) {
            if (err_is_ok(err)) {
                err = msgerr;
            }
            DEBUG_ERR(err, "requesting cap for BAR %d of device", nb);
            return err;
        }

        if (type == 0) { // Frame cap BAR
            bar->frame_cap = cap;
            struct frame_identity id = { .base = 0, .bytes = 0 };
            err = frame_identify(cap, &id);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "frame identify failed.");
            }
            bar->paddr = id.base;
            bar->bits = log2ceil(id.bytes);
            bar->bytes = id.bytes;
        } else { // IO BAR
            bar->io_cap = cap;
            err = cap_copy(cap_io, cap);
            if(err_is_fail(err) && err_no(err) != SYS_ERR_SLOT_IN_USE) {
                DEBUG_ERR(err, "cap_copy for IO cap");
                return err;
            }
        }
    }

    // initialize the device. We have all the caps now
    PCI_CLIENT_DEBUG("Succesfully done with pci init.\n");
    if(init_func) init_func(user_state, bars, nbars);

    return SYS_ERR_OK;
}

errval_t pci_register_driver_irq(pci_driver_init_fn init_func,
                                 void *user_state, uint32_t class,
                                 uint32_t subclass, uint32_t prog_if,
                                 uint32_t vendor, uint32_t device,
                                 uint32_t bus, uint32_t dev, uint32_t fun,
                                 interrupt_handler_fn handler,
                                 void *handler_arg)
{
     return pci_register_driver_movable_irq(init_func, user_state, class, subclass,
             prog_if, vendor, device, bus, dev, fun, handler, handler_arg,
             NULL, NULL);
}


errval_t pci_register_driver_noirq(pci_driver_init_fn init_func,
                                   void *user_state, uint32_t class,
                                   uint32_t subclass, uint32_t prog_if,
                                   uint32_t vendor, uint32_t device,
                                   uint32_t bus, uint32_t dev, uint32_t fun)
{
    return pci_register_driver_irq(init_func, user_state, class, subclass,
                                   prog_if, vendor, device, bus, dev, fun, NULL,
                                   NULL);
}

errval_t pci_register_legacy_driver_irq_cap(legacy_driver_init_fn init_func,
                                        uint16_t iomin, uint16_t iomax, int irq_cap_idx,
                                        interrupt_handler_fn handler,
                                        void *handler_arg) {
    errval_t err, msgerr;
    struct capref iocap;
    // Connect to PCI without interrupts
    err = slot_alloc(&iocap);
    assert(err_is_ok(err));
    err = pci_client->rpc_tx_vtbl.init_legacy_device(pci_client, iomin, iomax, 0,
                                              disp_get_core_id(), INVALID_VECTOR_32,
                                              &msgerr, &iocap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "pci_client->init_legacy_device()\n");
        return err;
    } else if (err_is_fail(msgerr)) {
        DEBUG_ERR(msgerr, "pci_client->init_legacy_device()\n");
        return msgerr;
    }

    /* copy IO cap to default location */
    err = cap_copy(cap_io, iocap);
    if (err_is_fail(err) && err_no(err) != SYS_ERR_SLOT_IN_USE) {
        DEBUG_ERR(err, "failed to copy legacy io cap to default slot\n");
        return err;
    }

    err = cap_destroy(iocap);
    assert(err_is_ok(err));

    // Setup int routing
    err = pci_setup_int_routing(irq_cap_idx, handler, handler_arg, NULL, NULL);
    if(err_is_fail(err)){
       DEBUG_ERR(err, "Could not set up int routing. Continuing w/o interrupts");
    } else {
        PCI_CLIENT_DEBUG("pci_setup_int_routing successful.\n");
    }

    // Run init function
    init_func();

    return SYS_ERR_OK;
}

errval_t pci_register_legacy_driver_irq(legacy_driver_init_fn init_func,
                                        uint16_t iomin, uint16_t iomax, int irq,
                                        interrupt_handler_fn handler,
                                        void *handler_arg)
{
    errval_t err, msgerr;
    struct capref iocap;

    debug_printf("WARNING: pci_register_legacy_driver_irq is deprecated."
                 "Make sure driver is started by Kaluga and use"
                 "pci_register_legacy_driver_irq_cap.\n");

    uint64_t vector = INVALID_VECTOR;
    err = inthandler_setup(handler, handler_arg, &vector);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "inthandler_setup()\n");
        return err;
    }

    err = slot_alloc(&iocap);
    assert(err_is_ok(err));
    err = pci_client->rpc_tx_vtbl.init_legacy_device(pci_client, iomin, iomax, irq,
                                              disp_get_core_id(), vector,
                                              &msgerr, &iocap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "pci_client->init_legacy_device()\n");
        return err;
    } else if (err_is_fail(msgerr)) {
        DEBUG_ERR(msgerr, "pci_client->init_legacy_device()\n");
        return msgerr;
    }

    /* copy IO cap to default location */
    err = cap_copy(cap_io, iocap);
    if (err_is_fail(err) && err_no(err) != SYS_ERR_SLOT_IN_USE) {
        DEBUG_ERR(err, "failed to copy legacy io cap to default slot\n");
    }

    err = cap_destroy(iocap);
    assert(err_is_ok(err));

    /* run init func */
    init_func();

    return msgerr;
}

errval_t pci_setup_inthandler(interrupt_handler_fn handler, void *handler_arg,
                                      uint8_t *ret_vector)
{
    errval_t err;
    uint64_t vector = INVALID_VECTOR;
    *ret_vector = 0;
    err = inthandler_setup(handler, handler_arg, &vector);
    if (err_is_ok(err)) {
        *ret_vector = vector + 32; // FIXME: HACK
    }
    return err;
}

errval_t pci_read_conf_header(uint32_t dword, uint32_t *val)
{
    errval_t err, msgerr;
    err = pci_client->rpc_tx_vtbl.read_conf_header(pci_client, dword, &msgerr, val);
    return err_is_fail(err) ? err : msgerr;
}

errval_t pci_write_conf_header(uint32_t dword, uint32_t val)
{
    errval_t err, msgerr;
    err = pci_client->rpc_tx_vtbl.write_conf_header(pci_client, dword, val, &msgerr);
    return err_is_fail(err) ? err : msgerr;
}

errval_t pci_msix_enable_addr(struct pci_addr *addr, uint16_t *count)
{
    errval_t err, msgerr;
    if (addr == NULL) {
        err = pci_client->rpc_tx_vtbl.msix_enable(pci_client, &msgerr, count);
    } else {
        err = pci_client->rpc_tx_vtbl.msix_enable_addr(pci_client, addr->bus, addr->device,
                                                addr->function, &msgerr, count);
    }
    return err_is_fail(err) ? err : msgerr;
}

errval_t pci_msix_enable(uint16_t *count)
{
    return pci_msix_enable_addr(NULL, count);
}

errval_t pci_msix_vector_init_addr(struct pci_addr *addr, uint16_t idx,
                                   uint8_t destination, uint8_t vector)
{
    errval_t err, msgerr;
    if (addr == NULL) {
        err = pci_client->rpc_tx_vtbl.msix_vector_init(pci_client, idx, destination,
                                                    vector, &msgerr);
    } else {
        err = pci_client->rpc_tx_vtbl.msix_vector_init_addr(pci_client, addr->bus,
                                                     addr->device, addr->function,
                                                     idx, destination,
                                                     vector, &msgerr);
    }

    return err_is_fail(err) ? err : msgerr;
}

errval_t pci_msix_vector_init(uint16_t idx, uint8_t destination,
                              uint8_t vector)
{
    return pci_msix_vector_init_addr(NULL, idx, destination, vector);
}

static void bind_cont(void *st, errval_t err, struct pci_binding *b)
{
    errval_t *reterr = st;
    if (err_is_ok(err)) {
        pci_rpc_client_init(b);
        pci_client = b;
    }
    *reterr = err;
}

errval_t pci_client_connect(void)
{
    iref_t iref;
    errval_t err, err2 = SYS_ERR_OK;

    PCI_CLIENT_DEBUG("Connecting to acpi\n");
    err = connect_to_acpi();
    if(err_is_fail(err)){
        return err;
    }
    PCI_CLIENT_DEBUG("Connected to ACPI\n");

    /* Connect to the pci server */
    PCI_CLIENT_DEBUG("Looking up pci iref\n");
    err = nameservice_blocking_lookup("pci", &iref);
    if (err_is_fail(err)) {
        return err;
    }

    assert(iref != 0);

    PCI_CLIENT_DEBUG("Connecting to pci\n");
    /* Setup flounder connection with pci server */
    err = pci_bind(iref, bind_cont, &err2, get_default_waitset(),
                   IDC_BIND_FLAG_RPC_CAP_TRANSFER);
    if (err_is_fail(err)) {
        return err;
    }

    /* XXX: Wait for connection establishment */
    while (pci_client == NULL && err2 == SYS_ERR_OK) {
        messages_wait_and_handle_next();
    }

    if(err_is_ok(err2)){
        PCI_CLIENT_DEBUG("PCI connection successful, connecting to int route service\n");
        err = int_route_client_connect();
        if(err_is_ok(err)){
            PCI_CLIENT_DEBUG("Int route service connected.\n");
        } else {
            DEBUG_ERR(err, "Could not connect to int route service\n");
        }
    }
    return err2;
}
