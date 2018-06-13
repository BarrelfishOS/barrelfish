#include <stdio.h>

#include <pci/pci_types.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/cpu_arch.h>
#include <barrelfish/nameservice_client.h>
#include <if/int_route_controller_defs.h>
#include <skb/skb.h>

#include "pci_int_ctrl.h"
#include "pci_debug.h"
#include "pci.h"

/*
 *  This file contains interrupt controller clients for the classes 'pci'
 *  and 'pci_msix'. Both enable interrupts in the PCI conf space. The
 *  first does so for legacy interrupts, while the later is doing if for MSI-x
 */


static void add_mapping(struct int_route_controller_binding *b,
        const char *label,
        const char *class,
        int_route_controller_int_message_t from,
        int_route_controller_int_message_t to) {

    errval_t err = SYS_ERR_OK;

    PCI_DEBUG("pci add_mapping: label:%s, class:%s (%"PRIu64", %"PRIu64") to "
            "(%"PRIu64", %"PRIu64")\n", label, class, from.addr, from.msg, to.addr, to.msg);

    assert(strcmp(class, "pci") == 0 || strcmp(class, "pci_msix") == 0);


    struct pci_addr addr;
    char s_pcie[5];
    bool pcie;
    // Get pcilnk acpiName from SKB
    err = skb_execute_query("pci_lbl_addr(%s, addr(Bus,Dev,Fun)),"
            "device(PCIE,addr(Bus,Dev,Fun), _, _, _, _, _, _),"
            "write((Bus,Dev,Fun,PCIE)).", label);
    if(err_is_fail(err)){
        DEBUG_SKB_ERR(err, "get pci addr");
        return;
    }


    err = skb_read_output("%d,%d,%d,%s", &addr.bus, &addr.device,
            &addr.function, s_pcie);
    if(err_is_fail(err)){
        DEBUG_SKB_ERR(err, "parse pci addr");
        return;
    }
    if(strncmp(s_pcie, "pcie", strlen("pcie")) == 0) {
        pcie = true;
    } else {
        pcie = false;
    }

    PCI_DEBUG("add_mapping: pcie=%d, bus=%"PRIu32", dev=%"PRIu32", fun=%"PRIu32"\n",
            pcie, addr.bus, addr.device, addr.function);

    if(strncmp(label, "pci_msix", strlen("pci_msix")) == 0){
        //MSI-X
        struct pci_address address = {
            .bus = addr.bus,
            .device = addr.device,
            .function = addr.function
        };
        err = pci_msix_enable_confspace(&address, 1);
        if(err_is_fail(err)){
            USER_PANIC_ERR(err, "msix enable");
        }
    } else {
        //Legacy
        pci_enable_interrupt_for_device(addr.bus, addr.device, addr.function, pcie);
    }

}

static void bind_cb(void *st, errval_t err, struct int_route_controller_binding *b) {
    if(!err_is_ok(err)){
        debug_err(__FILE__,__FUNCTION__,__LINE__, err, "Bind failure\n");
        return;
    }

    b->rx_vtbl.add_mapping = add_mapping;

    // Register this binding for all controllers with class pci
    const char * label = "";
    const char * ctrl_class = "pci";
    b->tx_vtbl.register_controller(b, NOP_CONT, label, ctrl_class);

    // Register this binding for all controllers with class pci_msix
    const char * ctrl_class_msix = "pci_msix";
    b->tx_vtbl.register_controller(b, NOP_CONT, label, ctrl_class_msix);

}

errval_t pci_int_ctrl_init(void) {
    // Connect to int route service
    iref_t int_route_service;
    errval_t err;
    err = nameservice_blocking_lookup("int_ctrl_service", &int_route_service);
    if(!err_is_ok(err)){
        debug_err(__FILE__,__FUNCTION__,__LINE__, err, "Could not lookup int_route_service\n");
        return err;
    }

    err = int_route_controller_bind(int_route_service,
            bind_cb, NULL, get_default_waitset(),
            IDC_BIND_FLAGS_DEFAULT);

    if(!err_is_ok(err)){
        debug_err(__FILE__,__FUNCTION__,__LINE__, err, "Could not bind int_route_service\n");
        return err;
    }

    return SYS_ERR_OK;
}
