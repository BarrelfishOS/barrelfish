 /**
 * \file
 * \brief PCI service code
 *
 * This file exports the PCI service interface for drivers
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
#include <barrelfish/sys_debug.h>

#include <if/pci_defs.h>
#include <if/acpi_defs.h>

#include <acpi_client/acpi_client.h>
#include <mm/mm.h>
//#include "pci_confspace.h"

#include "pci.h"
#include "pci_debug.h"

/*****************************************************************
 * Data types:
 *****************************************************************/

/* Per-client state
 * XXX: this assumes only one driver per client */
struct client_state {
    uint8_t initialized;
    int nr_allocated_bars;
    uint32_t bus;
    uint32_t dev;
    uint32_t fun;
    bool pcie;
    void *cont_st;
};

/*****************************************************************
 * Event handlers:
 *****************************************************************/
/*
    cc->bus = cc->dev = cc->fun = 0;
*/

static void init_pci_device_handler(struct pci_binding *b,
                                    uint32_t class_code, uint32_t sub_class,
                                    uint32_t prog_if, uint32_t vendor_id,
                                    uint32_t device_id,
                                    uint32_t bus, uint32_t dev, uint32_t fun)
{
    struct client_state *cc = (struct client_state *) b->st;
    errval_t err;

    err = device_init(class_code, sub_class, prog_if, vendor_id, device_id,
                      &bus, &dev, &fun, &(cc->pcie), &(cc->nr_allocated_bars));

    cc->bus = bus;
    cc->dev = dev;
    cc->fun = fun;

    err = b->tx_vtbl.init_pci_device_response(b, NOP_CONT, err,
                                              cc->nr_allocated_bars);
    assert(err_is_ok(err));
}

static void irq_enable_handler(struct pci_binding *b)
{
    struct client_state *cc = (struct client_state *) b->st;
    pci_enable_interrupt_for_device(cc->bus, cc->dev, cc->fun, cc->pcie);
    b->tx_vtbl.irq_enable_response(b, NOP_CONT, SYS_ERR_OK);
}

static void init_legacy_device_handler(struct pci_binding *b,
                                       uint16_t iomin, uint16_t iomax,
                                       uint8_t irq, coreid_t coreid,
                                       uint32_t vector)
{
    struct capref iocap = NULL_CAP;
    errval_t e = SYS_ERR_OK;

    PCI_DEBUG("pci: init_legacy_device_handler: called. irq:%"PRIu8", coreid:%"PRIuCOREID", vector:%"PRIu32"\n", irq, coreid, vector);

    /* TODO: make sure nobody else has claimed iomin-iomax range */

    /* construct iocap for this region */
    if (iomin != 0 || iomax != 0) {
        e = slot_alloc(&iocap);
        if (err_is_fail(e)) {
            e = err_push(e, LIB_ERR_SLOT_ALLOC);
            goto reply;
        }

        e = cap_mint(iocap, cap_io, iomin, iomax);
        if (err_is_fail(e)) {
            e = err_push(e, PCI_ERR_MINT_IOCAP);
            goto reply;
        }
    }

    /* determine IOAPIC INTI for given GSI and map to core */
    if (vector != (uint32_t)-1) {

        struct acpi_binding* cl = get_acpi_binding();
        errval_t ret_error;
        e = cl->rpc_tx_vtbl.enable_and_route_interrupt(cl, irq, coreid, vector, &ret_error);
        assert(err_is_ok(e));
        if (err_is_fail(ret_error)) {
            DEBUG_ERR(e, "failed to route interrupt %d -> %d\n", irq, vector);
            e = err_push(e, PCI_ERR_ROUTING_IRQ);
            goto reply;
        }
    }

    /* send reply */
reply:
    e = b->tx_vtbl.init_legacy_device_response(b, NOP_CONT, e,
                                               err_is_ok(e) ? iocap : NULL_CAP);
    if (err_is_fail(e)) {
        DEBUG_ERR(e, "failed to send reply");
    }

    PCI_DEBUG("pci: init_legacy_device_handler: terminated.\n");
}

static void get_bar_cap_response_resend(void *arg);

static void get_bar_cap_response_cont(struct pci_binding *b, errval_t err,
                                  struct capref cap, uint8_t type, uint8_t bar_nr)
{
    errval_t e;
    e = b->tx_vtbl.get_bar_cap_response(b, NOP_CONT, err, cap, type, bar_nr);
    if(err_is_fail(e)) {
        if(err_no(e) == FLOUNDER_ERR_TX_BUSY) {
            struct client_state *st = b->st;
            struct pci_get_bar_cap_response__tx_args *me = malloc(sizeof(*me));
            assert(me != NULL);
            me->err = err;
            me->cap = cap;
            me->type = type;
            me->bar_nr = bar_nr;
            st->cont_st = me;

            e = b->register_send(b, get_default_waitset(),
                                 MKCONT(get_bar_cap_response_resend, b));
            assert(err_is_ok(e));
        } else {
            USER_PANIC_ERR(e, "get_bar_cap_response");
        }
    }
}

static void get_bar_cap_response_resend(void *arg)
{
    struct pci_binding *b = arg;
    struct client_state *st = b->st;
    struct pci_get_bar_cap_response__tx_args *a = st->cont_st;
    get_bar_cap_response_cont(b, a->err, a->cap, a->type, a->bar_nr);
    free(a);
}

static void get_irq_cap_handler(struct pci_binding *b, uint16_t idx){
    // TODO: This method works only for non legacy devices
    // and supports only one interrupt per device at the moment
    assert(idx == 0);
    errval_t err;
    struct capref cap;
    slot_alloc(&cap);

    struct client_state *st = b->st;

    // TODO: This should be part of the routing step
    int irq = pci_setup_interrupt(st->bus, st->dev, st->fun);
    PCI_DEBUG("pci: init_device_handler_irq: init interrupt.\n");

    pci_enable_interrupt_for_device(st->bus, st->dev, st->fun, st->pcie);

    PCI_DEBUG("pci: Interrupt enabled.\n");

    err = sys_debug_create_irq_src_cap(cap, irq, irq);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "create irq src cap failed.");
    }

    err = b->tx_vtbl.get_irq_cap_response(b, NOP_CONT, err, cap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "cap response failed.");
    }
}

static void get_bar_cap_handler(struct pci_binding *b, uint32_t idx)
{
    struct client_state *st = b->st;
    assert(st != NULL);
    errval_t e;

    if (idx >= st->nr_allocated_bars) {
        e = b->tx_vtbl.get_bar_cap_response(b, NOP_CONT, PCI_ERR_WRONG_INDEX,
                                        NULL_CAP, 0, 0);
        assert(err_is_ok(e));
    } else {
        uint8_t type = pci_get_bar_cap_type_for_device(st->bus, st->dev,
                                                   st->fun, idx);
        uint8_t bar_nr = pci_get_bar_nr_for_index(st->bus, st->dev,
                                                   st->fun, idx);

        struct capref cap;
        if(type == 0) {
            cap = pci_get_bar_cap_for_device(st->bus, st->dev, st->fun, idx);
        } else {
            cap = NULL_CAP; //TODO: Get this io_cap from somewhere...
        }

        get_bar_cap_response_cont(b, SYS_ERR_OK, cap, type, bar_nr);
    }
}
/*
static void get_vbe_bios_cap(struct pci_binding *b)
{
    errval_t err;
    err = b->tx_vtbl.get_vbe_bios_cap_response(b, NOP_CONT, SYS_ERR_OK, biosmem,
                                               1UL << BIOS_BITS);
    assert(err_is_ok(err));
}*/

static void read_conf_header_handler(struct pci_binding *b, uint32_t dword)
{

    struct client_state *cc = (struct client_state *) b->st;
    struct pci_address addr = {
        .bus= cc->bus,
        .device=cc->dev,
        .function=cc->fun,
    };
    PCI_DEBUG("Read config header from %u:%u:%u\n",addr.bus, addr.device, addr.function);
    uint32_t val = pci_read_conf_header(&addr, dword);

    errval_t err;
    err = b->tx_vtbl.read_conf_header_response(b, NOP_CONT, SYS_ERR_OK, val);
    assert(err_is_ok(err));
}

static void reregister_interrupt_handler(struct pci_binding *b,
                                    uint32_t class_code, uint32_t sub_class,
                                    uint32_t prog_if, uint32_t vendor_id,
                                    uint32_t device_id,
                                    uint32_t bus, uint32_t dev, uint32_t fun,
                                    coreid_t coreid, uint32_t vector)
{
    errval_t err;
    err = device_reregister_interrupt(coreid, vector,
                      class_code, sub_class, prog_if, vendor_id, device_id,
                      &bus, &dev, &fun);
    err = b->tx_vtbl.reregister_interrupt_response(b, NOP_CONT, err);
    assert(err_is_ok(err));
}

static void write_conf_header_handler(struct pci_binding *b, uint32_t dword, uint32_t val)
{
    struct client_state *cc = (struct client_state *) b->st;
    struct pci_address addr = {
        .bus= cc->bus,
        .device=cc->dev,
        .function=cc->fun,
    };
    PCI_DEBUG("Write config header from %u:%u:%u\n",addr.bus, addr.device, addr.function);
    pci_write_conf_header(&addr, dword, val);

    errval_t err;
    err = b->tx_vtbl.write_conf_header_response(b, NOP_CONT, SYS_ERR_OK);
    assert(err_is_ok(err));
}

static void msix_enable_addr_handler(struct pci_binding *b, uint8_t bus,
                                      uint8_t dev, uint8_t fun)
{
    struct client_state *cc = (struct client_state *) b->st;
    struct pci_address addr;

    /* XXX: find another way to do this */

    if (bus == cc->bus && dev == cc->dev) {
        addr.bus= bus;
        addr.device=dev;
        addr.function=fun;
    } else {
        addr.bus= cc->bus;
        addr.device=cc->dev;
        addr.function=fun;
    }

    errval_t err;
    uint16_t count;

    debug_printf("enabling MSI-X for device (%u, %u, %u)\n", addr.bus,
                 addr.device, addr.function);

    err = pci_msix_enable(&addr, &count);
    err = b->tx_vtbl.msix_enable_response(b, NOP_CONT, err, count);
    assert(err_is_ok(err));
}

static void msix_enable_handler(struct pci_binding *b)
{
    struct client_state *cc = (struct client_state *) b->st;
    msix_enable_addr_handler(b, cc->bus, cc->dev, cc->fun);
}

static void msix_vector_init_addr_handler(struct pci_binding *b, uint8_t bus,
                                          uint8_t dev, uint8_t fun, uint16_t idx,
                                          uint8_t destination, uint8_t vector)
{
    struct client_state *cc = (struct client_state *) b->st;
    struct pci_address addr;

    /* XXX: find another way to do this */

    if (bus == cc->bus && dev == cc->dev) {
        addr.bus= bus;
        addr.device=dev;
        addr.function=fun;
    } else {
        addr.bus= cc->bus;
        addr.device=cc->dev;
        addr.function=fun;
    }

    debug_printf("initialize MSI-X vector for device (%u, %u, %u)\n", addr.bus,
                     addr.device, addr.function);

    errval_t err;

    err = pci_msix_vector_init(&addr, idx, destination, vector);
    err = b->tx_vtbl.msix_vector_init_response(b, NOP_CONT, err);
    assert(err_is_ok(err));
}

static void msix_vector_init_handler(struct pci_binding *b, uint16_t idx,
                                     uint8_t destination, uint8_t vector)
{
    struct client_state *cc = (struct client_state *) b->st;

    msix_vector_init_addr_handler(b, cc->bus, cc->dev, cc->fun, idx, destination,
                                  vector);
}

struct pci_rx_vtbl pci_rx_vtbl = {
    .init_pci_device_call = init_pci_device_handler,
    .init_legacy_device_call = init_legacy_device_handler,
    .get_bar_cap_call = get_bar_cap_handler,
    .get_irq_cap_call = get_irq_cap_handler,
    .reregister_interrupt_call = reregister_interrupt_handler,
    //.get_vbe_bios_cap_call = get_vbe_bios_cap,
    .read_conf_header_call = read_conf_header_handler,
    .write_conf_header_call = write_conf_header_handler,
    .irq_enable_call = irq_enable_handler,

    .msix_enable_call = msix_enable_handler,
    .msix_enable_addr_call = msix_enable_addr_handler,
    .msix_vector_init_call = msix_vector_init_handler,
    .msix_vector_init_addr_call = msix_vector_init_addr_handler,
};

static void export_callback(void *st, errval_t err, iref_t iref)
{
    assert(err_is_ok(err));

    err = nameservice_register("pci", iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_register failed");
    }
}

static errval_t connect_callback(void *cst, struct pci_binding *b)
{
    struct client_state *st = malloc(sizeof(struct client_state));
    assert(st != NULL);

    b->rx_vtbl = pci_rx_vtbl;
    b->st = st;
    st->nr_allocated_bars = 0;

    return SYS_ERR_OK;
}

/*****************************************************************
 * Boots up the PCI server:
 *****************************************************************/

void pci_init(void)
{
    PCI_DEBUG("pci: pci_init: called\n");

    PCI_DEBUG("pci: pci_init: launch listening\n");
    errval_t r = pci_export(NULL, export_callback, connect_callback,
                            get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);
    assert(err_is_ok(r));

    PCI_DEBUG("pci: pci_init: terminated\n");
}
