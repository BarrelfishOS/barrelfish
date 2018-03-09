#include <pci/pci_driver_client.h>
#include <pci/pci_driver_client_debug.h>
#include <if/pci_driver_client_defs.h>
#include <barrelfish/capabilities.h>
#include <barrelfish/debug.h>
#include <int_route/int_route_client.h>

// This is only here for PCIARG defines, remove me!
#include <pci/pci.h>
// End

const int PCI_CHANNEL_SIZE = 2048;

/*
 * Helper for constructing the pci_driver_client_frameinfo from a frame cap
 * \param frame     an existing frame cap (>=4k)
 * \param bind      create for bind or accept?  
 * \param fi        output
 */
static errval_t frame_to_frameinfo(struct capref frame, bool bind, struct pci_driver_client_frameinfo *fi){
    struct frame_identity fid;
    errval_t err;
    err = invoke_frame_identify(frame, &fid);
    if(err_is_fail(err)){
        DEBUG_ERR(err, "invoke_frame_identify");        
        return err;
    }
    PDC_DEBUG("pci ep frame base=0x%lx, size=0x%lx\n", fid.base, fid.bytes);

    uint8_t *msg_buf;
    err = vspace_map_one_frame((void*)&msg_buf, fid.bytes, frame, NULL, NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "vspace_map_one_frame");
        return err;
    }

    fi->outbufsize = PCI_CHANNEL_SIZE;
    fi->inbufsize = PCI_CHANNEL_SIZE;
    if(bind){
        fi->sendbase = (lpaddr_t)msg_buf;
        fi->inbuf = msg_buf + PCI_CHANNEL_SIZE;
        fi->outbuf = msg_buf;
    } else {
        fi->sendbase = (lpaddr_t)msg_buf + PCI_CHANNEL_SIZE;
        fi->inbuf = msg_buf;
        fi->outbuf = msg_buf + PCI_CHANNEL_SIZE;
    }

    return SYS_ERR_OK;
} 

static void pci_bind_cont(void *st, errval_t err, struct pci_driver_client_binding *_binding) {
    //struct pcid * pcd = (struct pcid *) st;
}

static errval_t bind_to_pci(struct capref ep, struct pcid * pdc){
    errval_t err;
    struct pci_driver_client_frameinfo fi;
    err = frame_to_frameinfo(ep, true, &fi);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "frame_to_frameinfo");
        return err;
    }

    err = pci_driver_client_connect(&fi, pci_bind_cont, pdc, get_default_waitset(),
            IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "pci_driver_client_connect");
        return err;
    }
    return SYS_ERR_OK;
}

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

    // Connect to PCI endpoint
    {
        struct capref ep = {
            .cnode = pdc->arg_cnode,
            .slot = PCIARG_SLOT_PCI_EP
        };
        PDC_DEBUG("binding to pci ...\n");
        err = bind_to_pci(ep, pdc);
        assert(err_is_ok(err));
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
