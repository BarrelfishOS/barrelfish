#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <int_route/msix_ctrl.h>
#include <int_route/int_model.h>
#include <if/int_route_controller_defs.h>
#include <dev/msix_dev.h>

#include "debug.h"

struct msix_ctrl_state {
    msix_t dev;     // Mackerel state
    char name[255]; // We use explicit name to identify on the service.
};


static void add_mapping(struct int_route_controller_binding *b,
        char *label,
        char *class,
        int_route_controller_int_message_t from,
        int_route_controller_int_message_t to) {

    struct msix_ctrl_state * st = b->st;
    CTRL_DEBUG("msix add_mapping: label:%s, class:%s (%"PRIu64", %"PRIu64") to "
            "(%"PRIu64", %"PRIu64")\n", label, class, from.addr, from.msg, to.addr, to.msg);


    CTRL_DEBUG("Setting MSIx entry %"PRIu64" to (addr=0x%"PRIx64",msg=0x%"PRIx64")\n",
            from.addr, to.addr, to.msg);

    int vec_num = from.addr; //TODO make sure this is correct
    msix_vec_control_wr(&st->dev, vec_num, 0);
    msix_msg_data_wr(&st->dev, vec_num, to.msg);
    msix_msg_addr_wr(&st->dev, vec_num, to.addr);
};

static void msix_ctrl_bind_cb(void *stin, errval_t err, struct int_route_controller_binding *b) {
    struct msix_ctrl_state * st = stin;
    if(!err_is_ok(err)){
        debug_err(__FILE__,__FUNCTION__,__LINE__, err, "Bind failure\n");
        return;
    }

    b->rx_vtbl.add_mapping = add_mapping;

    // Register this binding for all controllers with class pcilnk
    const char * label = ""; //TODO set me
    const char * ctrl_class = "msix";
    b->tx_vtbl.register_controller(b, NOP_CONT, label, ctrl_class);

    // Store state in binding
    b->st = st;
}

errval_t msix_client_init_by_args(int argc, char **argv, void* msix_tab) {
    struct int_startup_argument arg;
    errval_t err = SYS_ERR_IRQ_NO_ARG;
    for(int i=0; i<argc; i++){
        err = int_startup_argument_parse(argv[i], &arg);
        if(err_is_ok(err)) break;
    }
    if(!err_is_ok(err)) return err;
    return msix_client_init(arg.msix_ctrl_name, msix_tab);
};

errval_t msix_client_init(char *ctrl_name, void* msix_tab) {
    // Allocate state. Need to think about this.
    struct msix_ctrl_state * st = malloc(sizeof(struct msix_ctrl_state));

    // Connect to int route service
    iref_t int_route_service;
    errval_t err;
    err = nameservice_blocking_lookup("int_ctrl_service", &int_route_service);
    if(!err_is_ok(err)){
        debug_err(__FILE__,__FUNCTION__,__LINE__, err,
                "Could not lookup int_route_service\n");
        return err;
    }

    err = int_route_controller_bind(int_route_service, msix_ctrl_bind_cb, st,
            get_default_waitset(), IDC_BIND_FLAGS_DEFAULT);

    if(!err_is_ok(err)){
        debug_err(__FILE__,__FUNCTION__,__LINE__, err,
                "Could not bind int_route_service\n");
        return err;
    }

    msix_initialize(&st->dev, msix_tab);

    return SYS_ERR_OK;
};
