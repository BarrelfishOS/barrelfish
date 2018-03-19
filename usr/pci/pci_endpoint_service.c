#include <stdio.h>

#include <pci/pci_types.h>
#include <barrelfish/barrelfish.h>
#include <driverkit/driverkit.h>
#include <skb/skb.h>
#include <if/kaluga_defs.h>
#include <if/pci_defs.h>

#include "pci_int_ctrl.h"
#include "pci_debug.h"
#include "pci.h"

// Assume one connection
static struct capref kaluga_ep;
static struct kaluga_binding* kaluga;
static bool bound;


static void sriov_enable_vf_handler(struct pci_binding* b, uint32_t vf_num)
{
    errval_t err;

    struct pci_address* addr = (struct pci_address* ) b->st;

    // Add octopus record
    err = pci_start_virtual_function_for_device(addr, vf_num);
    
    err = b->tx_vtbl.sriov_enable_vf_response(b, NOP_CONT, err);
    assert(err_is_ok(err));
}

static struct pci_rx_vtbl pci_rx_vtbl = {
    .sriov_enable_vf2_call = sriov_enable_vf_handler
};


static void requst_vf_resources_handler(struct pci_binding* b, uint32_t vf_num)
{
    errval_t err;

    struct pci_address* addr = (struct pci_address* ) b->st;

    // Add octopus record
    err = pci_start_virtual_function_for_device(addr, vf_num);
    
    err = b->tx_vtbl.sriov_enable_vf_response(b, NOP_CONT, err);
    assert(err_is_ok(err));
}

static void request_pci_cap_handler(struct kaluga_binding* b, uint32_t bus, 
                                    uint32_t device, uint32_t function)
{
    errval_t err;
    debug_printf("#################### client requested resources!########################### \n");

    struct capref cap;    
    err = slot_alloc(&cap);
    assert(err_is_ok(err));

    struct pci_binding* pci;
    err = pci_create_endpoint(IDC_ENDPOINT_LMP, &pci_rx_vtbl, NULL,
                              get_default_waitset(),
                              IDC_ENDPOINT_FLAGS_DUMMY,
                              &pci, cap);

    pci_rpc_client_init(pci);
    struct pci_address* pci_addr = (struct pci_address*) malloc(sizeof(struct pci_address));
    pci_addr->bus = bus;
    pci_addr->device = device;
    pci_addr->function = function;
    pci->st = pci;
    
    err = b->tx_vtbl.request_pci_cap_response(b, NOP_CONT, cap);
    assert(err_is_ok(err));
}

static struct kaluga_rx_vtbl rx_vtbl = {
    .request_pci_cap_call = request_pci_cap_handler
};

static void bind_cont(void *st, errval_t err, struct kaluga_binding *b)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "bind failed");
    }

    debug_printf("client bound!\n");

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rx_vtbl;
    b->st = st;
    kaluga_rpc_client_init(b);

    kaluga = b;
    bound = true;
}

errval_t pci_setup_connection_to_kaluga(void) 
{
    errval_t err;

    // When started by Kaluga it handend off an endpoint cap to Kaluga
    kaluga_ep.cnode = build_cnoderef(cap_argcn, CNODE_TYPE_OTHER);
    kaluga_ep.slot = DRIVERKIT_ARGCN_SLOT_EP;
 
    err = kaluga_bind_to_endpoint(kaluga_ep, bind_cont, NULL, get_default_waitset(), 
                                  IDC_BIND_FLAGS_DEFAULT);
    while(!bound) {
        event_dispatch(get_default_waitset());
    }

    return SYS_ERR_OK;
}
