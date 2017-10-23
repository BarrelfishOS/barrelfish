#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <int_route/msix_ctrl.h>
#include <int_route/int_model.h>
#include <if/int_route_controller_defs.h>
#include <dev/msix_e1000_dev.h>

#include "debug.h"

struct msix_ctrl_state {
    int bound;
    msix_e1000_t dev;     // Mackerel state
    struct int_startup_argument * arg; // Contains name etc.
};


static void add_mapping(struct int_route_controller_binding *b,
        const char *label,
        const char *class,
        int_route_controller_int_message_t from,
        int_route_controller_int_message_t to) {

    struct msix_ctrl_state * st = b->st;
    CTRL_DEBUG("msix add_mapping: label:%s, class:%s (%"PRIu64", %"PRIu64") to "
            "(%"PRIu64", %"PRIu64")\n", label, class, from.addr, from.msg, to.addr, to.msg);


    CTRL_DEBUG("Setting MSIx port=%"PRIu64" to (addr=0x%"PRIx64",msg=0x%"PRIx64")\n",
            from.port, to.addr, to.msg);

    int vec_num = from.addr; //TODO make sure this is correct
    msix_e1000_vec_control_wr(&st->dev, vec_num, 0);
    msix_e1000_msg_data_wr(&st->dev, vec_num, to.msg);
    msix_e1000_msg_addr_wr(&st->dev, vec_num, to.addr);
};

static void msix_ctrl_bind_cb(void *stin, errval_t err, struct int_route_controller_binding *b) {
    struct msix_ctrl_state * st = stin;
    if(!err_is_ok(err)){
        debug_err(__FILE__,__FUNCTION__,__LINE__, err, "Bind failure\n");
        return;
    }

    b->rx_vtbl.add_mapping = add_mapping;

    // Register this binding for name=msix_ctrl_name and class msix
    b->tx_vtbl.register_controller(b, BLOCKING_CONT, st->arg->msix_ctrl_name, "msix");

    // Store state in binding
    b->st = st;

    st->bound = true;
}

errval_t msix_client_init_by_args(int argc, char **argv, void* msix_tab) {
    struct int_startup_argument *arg = malloc(sizeof(struct int_startup_argument));
    if(!arg) return LIB_ERR_MALLOC_FAIL;

    errval_t err = SYS_ERR_IRQ_NO_ARG;
    for(int i=0; i<argc; i++){
        err = int_startup_argument_parse(argv[i], arg);
        if(err_is_ok(err)) break;
    }
    if(!err_is_ok(err)) return err;
    if(arg->model != INT_MODEL_MSIX) {
        return SYS_ERR_IRQ_INVALID;
    }
    return msix_client_init(arg, msix_tab);
};

errval_t msix_client_init(struct int_startup_argument *arg, void* msix_tab) {
    // Allocate state. Need to think about this.
    struct msix_ctrl_state * st = malloc(sizeof(struct msix_ctrl_state));
    st->bound = false;
    st->arg = arg;
    CTRL_DEBUG("Instantiating MSIx ctrl driver (name=%s)\n",
            arg->msix_ctrl_name);

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

    while(!st->bound) event_dispatch(get_default_waitset());


    msix_e1000_initialize(&st->dev, msix_tab);

    return SYS_ERR_OK;
};
