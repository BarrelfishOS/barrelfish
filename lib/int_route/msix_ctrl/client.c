#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <int_route/msix_ctrl.h>
#include <if/int_route_controller_defs.h>

#include "debug.h"


static void add_mapping(struct int_route_controller_binding *b,
        char *label,
        char *class,
        int_route_controller_int_message_t from,
        int_route_controller_int_message_t to) {

    CTRL_DEBUG("msix add_mapping: label:%s, class:%s (%"PRIu64", %"PRIu64") to "
            "(%"PRIu64", %"PRIu64")\n", label, class, from.addr, from.msg, to.addr, to.msg);


    assert(!"NYI");
};

static void msix_ctrl_bind_cb(void *st, errval_t err, struct int_route_controller_binding *b) {
    if(!err_is_ok(err)){
        debug_err(__FILE__,__FUNCTION__,__LINE__, err, "Bind failure\n");
        return;
    }

    b->rx_vtbl.add_mapping = add_mapping;

    // Register this binding for all controllers with class pcilnk
    const char * label = ""; //TODO set me
    const char * ctrl_class = "msix";
    b->tx_vtbl.register_controller(b, NOP_CONT, label, ctrl_class);
}

errval_t msix_client_init_by_args(int argc, char **argv, void* msix_tab) {
    // TODO: implement me
    return msix_client_init(NULL, msix_tab);
};

errval_t msix_client_init(char *ctrl_name, void* msix_tab) {
    // Connect to int route service
    iref_t int_route_service;
    errval_t err;
    err = nameservice_blocking_lookup("int_ctrl_service", &int_route_service);
    if(!err_is_ok(err)){
        debug_err(__FILE__,__FUNCTION__,__LINE__, err,
                "Could not lookup int_route_service\n");
        return err;
    }

    err = int_route_controller_bind(int_route_service, msix_ctrl_bind_cb, NULL, get_default_waitset(),
            IDC_BIND_FLAGS_DEFAULT);

    if(!err_is_ok(err)){
        debug_err(__FILE__,__FUNCTION__,__LINE__, err, "Could not bind int_route_service\n");
        return err;
    }

    return SYS_ERR_OK;
};
