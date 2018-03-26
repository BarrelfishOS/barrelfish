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
#include <driverkit/driverkit.h>
#include <collections/list.h>
#include <skb/skb.h>

#include <if/pci_defs.h>
#include <if/pci_iommu_defs.h>
#include <if/kaluga_defs.h>
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

struct iommu_client_state {
    uint32_t index;
    struct pci_iommu_binding* b;
};

// Assume one connection to kaluga over which
// kaluga can request PCI endpoints to had off to devices
static struct capref kaluga_ep;
static struct kaluga_binding* kaluga;
static bool bound;

static collections_listnode* iommu_list;


/*****************************************************************
 * Helper functions:
 *****************************************************************/

static errval_t device_lookup_iommu_by_pci(uint16_t seg, uint8_t bus, uint8_t dev, 
                                           uint8_t fun, uint32_t* index)
{
    /* find the device scope */
    errval_t err;
    uint32_t idx, type;

    err = skb_execute_query("iommu_device(T,I, _, _, addr(%" PRIu16 ", "
                            "%" PRIu8 ", %" PRIu8 ", %" PRIu8 "), _),"
                            "write(u(T,I)).", seg, bus, dev, fun);
    if (err_is_ok(err)) {
        err = skb_read_output("u(%d,%d)", &type, &idx);
        PCI_DEBUG("[pci] found device at iommu with idx %u\n", idx);

        assert(err_is_ok(err));
    }

    PCI_DEBUG("[pci] look-up iommu by PCI segment with all flags\n");
    err = skb_execute_query("iommu(T,I,1, %" PRIu16 "),write(u(T,I,1)).", seg);
    if (err_is_fail(err)) {
        PCI_DEBUG("[pci] look-up iommu by PCI segment\n");
        err = skb_execute_query("iommu(T,I,F, %" PRIu16 "),write(u(T,I,F)).", seg);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to obtain iommu for PCI segment");
            return IOMMU_ERR_IOMMU_NOT_FOUND;
        }
    }

    uint32_t flags;
    err = skb_read_output("u(%d,%d,%d)", &type, &idx, &flags);
    assert(err_is_ok(err));
    
    *index = idx;
    PCI_DEBUG("iommu_device_lookup_iommu_by_pci succeeded!\n");
    return SYS_ERR_OK;
}

static int32_t find_index(void* elem, void* idx) {
    assert(elem != NULL);
    assert(idx != NULL);

    struct iommu_client_state* st = (struct iommu_client_state*) elem;
    uint32_t index = *((uint32_t*) idx);
    if (index == st->index) {
        return 1;
    }
    return 0;
}


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

static void get_vf_bar_cap_handler(struct pci_binding *b, uint32_t vf_num, uint32_t idx)
{
    struct client_state *st = b->st;
    assert(st != NULL);
    errval_t err;
    
    struct pci_address pci = {
        .bus = st->bus,
        .device =  st->dev,
        .function = st->fun
    };
    
    struct pci_address vf_addr;

    err = pci_get_vf_addr_of_device(pci, vf_num, &vf_addr);
    if (err_is_fail(err)) {
        err = b->tx_vtbl.get_vf_bar_cap_response(b, NOP_CONT, NULL_CAP, err);
        assert(err_is_ok(err));
        return;
    }

    uint8_t type = pci_get_bar_cap_type_for_device(vf_addr.bus, vf_addr.device,
                                               vf_addr.function, idx);
    struct capref cap;
    if(type == 0) {
        cap = pci_get_bar_cap_for_device(vf_addr.bus, vf_addr.device, vf_addr.function, idx);
    } else {
        // TODO is this even possible?
        err = PCI_ERR_SRIOV_NOT_SUPPORTED;
        cap = NULL_CAP;
    }

    err = b->tx_vtbl.get_vf_bar_cap_response(b, NOP_CONT, cap, err);
    assert(err_is_ok(err));
}


static void get_vf_iommu_endpoint_cap_handler(struct pci_binding *b, uint32_t vf_num, 
                                              uint8_t type)

{
    struct client_state *st = b->st;
    assert(st != NULL);
    errval_t err, out_err;
    
    struct pci_address pci = {
        .bus = st->bus,
        .device =  st->dev,
        .function = st->fun
    };
    
    struct pci_address vf_addr;

    err = pci_get_vf_addr_of_device(pci, vf_num, &vf_addr);
    if (err_is_fail(err)) {
        err = b->tx_vtbl.get_vf_iommu_endpoint_cap_response(b, NOP_CONT, NULL_CAP, err);
        assert(err_is_ok(err));
        return;
    }

    uint32_t idx;
    // TODO get segemnt correct, works with 0.
    err = device_lookup_iommu_by_pci(0, vf_addr.bus, vf_addr.device, 
                                     vf_addr.function, &idx);
    if (err_is_fail(err)) {
        goto reply;
    }

    struct capref cap;    
    out_err = slot_alloc(&cap);
    if (err_is_fail(out_err)) {
        goto reply;
    }
    
    struct iommu_client_state* cl = collections_list_find_if(iommu_list, find_index, 
                                                             &idx);
    if (cl == NULL) {
        goto reply;
    }

    err = cl->b->rpc_tx_vtbl.request_iommu_endpoint(cl->b,type, 0, vf_addr.bus, vf_addr.device, 
                                                    vf_addr.function, &cap, &out_err);
    if (err_is_ok(err)) {
       goto reply;
    }

reply:
    err = b->tx_vtbl.get_vf_iommu_endpoint_cap_response(b, NOP_CONT, cap, out_err);
    assert(err_is_ok(err));
    slot_free(cap);
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

static void sriov_enable_vf_handler(struct pci_binding* b, uint32_t vf_num)
{
    errval_t err;

    struct client_state* state = (struct client_state* ) b->st;
    struct pci_address addr = {
        .bus = state->bus,
        .device = state->dev,
        .function = state->fun
    };

    debug_printf("Enabling Virtual Function for device (bus=%d, device=%d, function=%d)"
                 "binding %p state %p \n",
                 state->bus, state->dev, state->fun, b, state);

    // Add octopus record
    err = pci_start_virtual_function_for_device(&addr, vf_num);
    
    err = b->tx_vtbl.sriov_enable_vf_response(b, NOP_CONT, err);
    assert(err_is_ok(err));
}

static void get_vf_pci_endpoint_cap_handler(struct pci_binding *b, uint32_t vf_num, 
                                            uint8_t type);

struct pci_rx_vtbl pci_rx_vtbl = {
    .init_pci_device_call = init_pci_device_handler,
    .init_legacy_device_call = init_legacy_device_handler,
    .get_bar_cap_call = get_bar_cap_handler,
    .get_vf_bar_cap_call = get_vf_bar_cap_handler,
    .get_vf_iommu_endpoint_cap_call = get_vf_iommu_endpoint_cap_handler,
    .get_vf_pci_endpoint_cap_call = get_vf_pci_endpoint_cap_handler,
    .get_irq_cap_call = get_irq_cap_handler,
    .reregister_interrupt_call = reregister_interrupt_handler,
    //.get_vbe_bios_cap_call = get_vbe_bios_cap,
    .read_conf_header_call = read_conf_header_handler,
    .write_conf_header_call = write_conf_header_handler,
    .irq_enable_call = irq_enable_handler,
    .sriov_enable_vf_call = sriov_enable_vf_handler,
    .msix_enable_call = msix_enable_handler,
    .msix_enable_addr_call = msix_enable_addr_handler,
    .msix_vector_init_call = msix_vector_init_handler,
    .msix_vector_init_addr_call = msix_vector_init_addr_handler,
};

static void get_vf_pci_endpoint_cap_handler(struct pci_binding *b, uint32_t vf_num, 
                                            uint8_t type)

{
    struct client_state *st = b->st;
    assert(st != NULL);
    
    errval_t err, out_err;

    struct pci_address pci = {
        .bus = st->bus,
        .device =  st->dev,
        .function = st->fun
    };
   
    struct pci_address vf_addr;

    err = pci_get_vf_addr_of_device(pci, vf_num, &vf_addr);
    if (err_is_fail(err)) {
        err = b->tx_vtbl.get_vf_pci_endpoint_cap_response(b, NOP_CONT, NULL_CAP, err);
        assert(err_is_ok(err));
        return;
    }

 
    PCI_DEBUG("Requested pci endpoint for device (bus=%d, device=%d, function=%d)\n",
              vf_addr.bus, vf_addr.device, vf_addr.function);

    struct capref cap;    
    err = slot_alloc(&cap);
    assert(err_is_ok(err));

    struct pci_binding* pci_b;
    struct client_state* state = (struct client_state*) 
                                  calloc(1, sizeof(struct client_state));

    out_err = pci_create_endpoint(type, &pci_rx_vtbl, state,
                                  get_default_waitset(),
                                  IDC_ENDPOINT_FLAGS_DUMMY,
                                  &pci_b, cap);
    if (err_is_fail(out_err)) {
        goto reply;
    }
    pci_rpc_client_init(pci_b);

    state->bus = vf_addr.bus;
    state->dev = vf_addr.device;
    state->fun = vf_addr.function;
    pci_b->st = state;

reply:
    err = b->tx_vtbl.get_vf_pci_endpoint_cap_response(b, NOP_CONT, cap, out_err);
    assert(err_is_ok(err));
}

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
 * Iommu PCI connection interface
 *****************************************************************/

// TODO empty for now
struct pci_iommu_rx_vtbl pci_iommu_rx_vtbl;


/*****************************************************************
 * Connection to Kaluga to request PCI endpoints for devices
 *****************************************************************/

static void request_endpoint_cap_handler(struct kaluga_binding* b, uint8_t type, 
                                         uint32_t bus, uint32_t device, 
                                         uint32_t function)
{
    errval_t err, out_err;
    PCI_DEBUG("Kaluga requested pci endpoint for device (bus=%d, device=%d, function=%d)\n",
              bus, device, function);

    struct capref cap;    
    err = slot_alloc(&cap);
    assert(err_is_ok(err));

    struct pci_binding* pci;

    out_err = pci_create_endpoint(type, &pci_rx_vtbl, NULL,
                              get_default_waitset(),
                              IDC_ENDPOINT_FLAGS_DUMMY,
                              &pci, cap);
    if (err_is_fail(out_err)) {
        goto reply;
    }
    pci_rpc_client_init(pci);
    struct client_state* state = (struct client_state*) 
                                  calloc(1, sizeof(struct client_state));
    state->bus = bus;
    state->dev = device;
    state->fun = function;
    pci->st = state;

reply:
    err = b->tx_vtbl.request_endpoint_cap_response(b, NOP_CONT, cap, out_err);
    assert(err_is_ok(err));
}

static void request_endpoint_cap_for_iommu_handler(struct kaluga_binding* b, uint8_t type, 
                                                   uint32_t index)
{
    errval_t err;
    PCI_DEBUG("Kaluga requested pci to iommu endpoint for iommu %d\n", index);

    struct capref cap;    
    err = slot_alloc(&cap);
    assert(err_is_ok(err));

    struct iommu_client_state* state = (struct iommu_client_state*) 
                                        calloc(1, sizeof(struct iommu_client_state));
    err = pci_iommu_create_endpoint(type, &pci_iommu_rx_vtbl, NULL,
                                    get_default_waitset(),
                                    IDC_ENDPOINT_FLAGS_DUMMY,
                                    &state->b, cap);
    assert(err_is_ok(err));

    pci_iommu_rpc_client_init(state->b);
    state->index = index;
 
    if (iommu_list == NULL) {
        // TODO free function
        collections_list_create(&iommu_list, NULL);   
    }
  
    collections_list_insert_tail(iommu_list, state);
    
    err = b->tx_vtbl.request_endpoint_cap_for_iommu_response(b, NOP_CONT, cap, err);
    assert(err_is_ok(err));
}

static void request_iommu_endpoint_cap_handler(struct kaluga_binding* b, uint8_t type, 
                                               uint32_t segment,
                                               uint32_t bus, uint32_t device, 
                                               uint32_t function)
{
    errval_t err, out_err;
    PCI_DEBUG("Kaluga requested pci endpoint for device (bus=%d, device=%d, function=%d)\n",
              bus, device, function);


    uint32_t idx;
    err = device_lookup_iommu_by_pci(segment, bus, device, function, &idx);
    if (err_is_fail(err)) {
        goto reply;
    }

    struct capref cap;    
    out_err = slot_alloc(&cap);
    if (err_is_fail(out_err)) {
        goto reply;
    }
    
    struct iommu_client_state* cl = collections_list_find_if(iommu_list, find_index, 
                                                             &idx);
    if(cl == NULL) {
        goto reply;
    }

    err = cl->b->rpc_tx_vtbl.request_iommu_endpoint(cl->b,type, segment, bus, device, 
                                                    function, &cap, &out_err);
    if (err_is_ok(err)) {
       goto reply;
    }

reply:
    err = b->tx_vtbl.request_iommu_endpoint_cap_response(b, NOP_CONT, cap, out_err);
    assert(err_is_ok(err));
    slot_free(cap);
}

static struct kaluga_rx_vtbl rx_vtbl = {
    .request_endpoint_cap_call = request_endpoint_cap_handler,
    .request_iommu_endpoint_cap_call = request_iommu_endpoint_cap_handler,
    .request_endpoint_cap_for_iommu_call = request_endpoint_cap_for_iommu_handler
};

static void bind_cont(void *st, errval_t err, struct kaluga_binding *b)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "bind failed");
    }

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rx_vtbl;
    b->st = st;
    kaluga_rpc_client_init(b);

    kaluga = b;
    bound = true;
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


    PCI_DEBUG("pci: pci_init: connect to kaluga using endpoint cap\n");
    // When started by Kaluga it handend off an endpoint cap to Kaluga
    kaluga_ep.cnode = build_cnoderef(cap_argcn, CNODE_TYPE_OTHER);
    kaluga_ep.slot = DRIVERKIT_ARGCN_SLOT_KALUGA_EP;

    r = kaluga_bind_to_endpoint(kaluga_ep, bind_cont, NULL, get_default_waitset(), 
                                IDC_BIND_FLAGS_DEFAULT);
    while(!bound) {
        event_dispatch(get_default_waitset());
    }

    PCI_DEBUG("pci: pci_init: terminated\n");
}


