#include <pci/pci_driver_client.h>
#include <pci/pci_driver_client_debug.h>
#include <if/pci_driver_client_defs.h>
#include <barrelfish/capabilities.h>
#include <barrelfish/debug.h>
#include <int_route/int_route_client.h>

// This is only here for PCIARG defines, remove me!
#include <pci/pci.h>
// End


static inline bool strbegins(char *str, char *start){
    return strncmp(str, start, strlen(start)) == 0;
}

errval_t pcid_init(
        struct pcid * pdc,
        struct capref* caps,
        size_t caps_len,
        char** args,
        size_t args_len,
        struct waitset * ws)
{
    errval_t err;
    pdc->ws = ws;

    // all caps that are not Interrupt or PCI_EP
    pdc->num_bars = caps_len - 2;

    {
        struct capref cnodecap;
        err = slot_alloc_root(&cnodecap);
        if(err_is_fail(err)){
            DEBUG_ERR(err, "slot_alloc_root");
            return err;
        }
        if(caps_len == 1) {
            debug_printf("Not enough caps received\n");
            return PCI_ERR_NO_CAP;
        }
        err = cap_copy(cnodecap, caps[0]);
        pdc->arg_cnode = build_cnoderef(cnodecap, CNODE_TYPE_OTHER); 
    }


    // Parse pci and int_model arguments
    bool arg_pci_found = false;
    bool arg_int_found = false;
    for(int i=0; i<args_len; i++) {
        if(strbegins(args[i], "pci=")){
            pci_deserialize_octet(args[i] + strlen("pci="), &pdc->addr,
                    &pdc->id, &pdc->cls);
            arg_pci_found = true;
        }
        if(strbegins(args[i], "int_model=") ){
            err = int_startup_argument_parse(args[i], &pdc->int_arg);
            if(err_is_fail(err)){
                DEBUG_ERR(err, "int_startup_argument_parse");
                return err;
            }
            arg_int_found = true;
        }
    }

    if(!arg_pci_found || !arg_int_found){
        debug_printf("PCI or INT arg not found");
        return PCI_ERR_ARG_PARSE;
    }

    // Connect to interrupt service
    err = int_route_client_connect();
    if(err_is_fail(err)){
        DEBUG_ERR(err, "int_route_client_connect");
        return err;

    }
    
    return SYS_ERR_OK;
}

errval_t pcid_get_interrupt_cap(struct pcid* pdc, struct capref *ret) {

    
    struct capref interrupt = {
        .cnode = pdc->arg_cnode,
        .slot = PCIARG_SLOT_INT,
    };
    
    *ret = interrupt;

    return SYS_ERR_OK;
}

errval_t pcid_get_bar_cap(struct pcid* pdc, int bar_index, struct capref *ret) {

    assert(bar_index <= pdc->num_bars);
    struct capref bar = {
        .cnode = pdc->arg_cnode,
        .slot = PCIARG_SLOT_BAR0 + bar_index,
    };

    *ret = bar;

    return SYS_ERR_OK;
}

size_t pcid_get_bar_num(struct pcid* pdc)
{
    return pdc->num_bars;
}

errval_t pcid_connect_int_with_cap(struct capref int_src, int int_index,
                                   interrupt_handler_fn handler, void *st)
{
    errval_t err;

    err = int_route_client_route_and_connect(int_src, int_index,
        get_default_waitset(), handler, st);

    if(err_is_fail(err)) {
        DEBUG_ERR(err, "set-up int routing");
    }

    return err;
}

errval_t pcid_connect_int(struct pcid* pdc, int int_index,
                          interrupt_handler_fn handler, void *st)
{
    errval_t err;
    struct capref irq_cap;
    err = pcid_get_interrupt_cap(pdc, &irq_cap); 
    if(err_is_fail(err)){
        return err;
    }

    return pcid_connect_int_with_cap(irq_cap, int_index, handler, st);
}

errval_t pcid_enable_msix(int *num_vectors) {
    //TODO RPC 
    return SYS_ERR_OK;
}
