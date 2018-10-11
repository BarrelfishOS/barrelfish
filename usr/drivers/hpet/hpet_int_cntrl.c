#include <stdio.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/cpu_arch.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/types.h>
//#include <if/int_route_controller_defs.h>
#include <hpet_debug.h>
#include <hpet_int_cntrl.h>
#include <if/int_route_controller_defs.h>
#include <int_route/int_model.h>

struct hpet_ctrl_state {
    int bound;

    hpet_t dev;                       // Mackerel state
    struct int_startup_argument *arg; // Contains name etc.
};

static void add_mapping(struct int_route_controller_binding *b,
                        const char *label, const char *class,
                        int_route_controller_int_message_t from,
                        int_route_controller_int_message_t to) {

    struct hpet_ctrl_state *st = b->st;
    HPET_DEBUG("(add_mapping) start \n");
    HPET_DEBUG("(add_mapping) label:%s, class:%s (%" PRIu64 ", %" PRIu64 ") to "
               "(%" PRIu64 ", %" PRIu64 ")\n",
               label, class, from.addr, from.msg, to.addr, to.msg);

    HPET_DEBUG("(adding_map)Setting MSIx port=%" PRIu64 " to (addr=0x%" PRIx64
               ",msg=0x%" PRIx64 ")\n",
               from.port, to.addr, to.msg);

    // timer number = from.port
    // activate fsb interrupt
    // hpet->timer[from.port].fsb_address = to.addr
    map_fsb_int(from.port, to.addr, to.msg, (void *)&st->dev);
};

// bind the call back (add_mapping) to be called from the server to client
// whenever it routes something
static void hpet_ctrl_bind_cb(void *stin, errval_t err,
                              struct int_route_controller_binding *b) {

    HPET_DEBUG("(hpet_ctrl_bind_cb) start \n ");
    struct hpet_ctrl_state *st = stin;
    if (!err_is_ok(err)) {
        debug_err(__FILE__, __FUNCTION__, __LINE__, err, "Bind failure\n");
        HPET_DEBUG("(hpet_ctrl_bind_cb) Binding Failed ");
        return;
    }

    b->rx_vtbl.add_mapping = add_mapping;
    // Register controller so that the IRS knows that the following interrupts
    // belong to the hpet_ctrl
    b->tx_vtbl.register_controller(b, BLOCKING_CONT, "", "hpet");
    HPET_DEBUG("(hpet_ctrl_bind_cb) controller has been registered \n ");

    // Store state in binding
    b->st = st;

    st->bound = true;
}

errval_t init_hpet_int_controller(struct hpet_driver_state *ds,
                                  lvaddr_t vaddr) {

    HPET_DEBUG("(init_hpet_int_controller) start \n");

    // Initializing int start up argument
    struct int_startup_argument *arg =
        malloc(sizeof(struct int_startup_argument));
    arg->int_range_start = ds->int_start_range;
    arg->int_range_end = ds->int_end_range;
    strcpy(arg->msix_ctrl_name, "hpet");
    arg->model = INT_MODEL_NONE;

    // initializng hpet controller state
    struct hpet_ctrl_state *st = malloc(sizeof(struct hpet_ctrl_state));
    st->bound = false;
    st->arg = arg;
    st->dev = ds->d;
    HPET_DEBUG("Instantiating Hpet ctrl driver (name=%s)\n",
               arg->msix_ctrl_name);

    // Connect to int route service
    iref_t int_route_service;
    errval_t err;
    err = nameservice_blocking_lookup("int_ctrl_service", &int_route_service);
    if (!err_is_ok(err)) {
        debug_err(__FILE__, __FUNCTION__, __LINE__, err,
                  "Could not lookup int_route_service\n");
        return err;
    }

    // bind the mapping function to the interrupt routing sservice

    err = int_route_controller_bind(int_route_service, hpet_ctrl_bind_cb, st,
                                    get_default_waitset(),
                                    IDC_BIND_FLAGS_DEFAULT);

    if (!err_is_ok(err)) {
        debug_err(__FILE__, __FUNCTION__, __LINE__, err,
                  "Could not bind int_route_service\n");
        return err;
    }

    while (!st->bound)
        event_dispatch(get_default_waitset());

    HPET_DEBUG("Interrupt route binding is done , Status bound = %d \n ",
               st->bound);

    return SYS_ERR_OK;
}
