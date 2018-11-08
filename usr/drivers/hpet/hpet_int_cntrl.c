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
    struct hpet_comp_st *comp_st;
    int bound;
    char label[128]; 
    struct int_route_controller_binding *bind;
};


static void add_mapping(struct int_route_controller_binding *b,
                        const char *label, const char *class,
                        int_route_controller_int_message_t from,
                        int_route_controller_int_message_t to) {

    struct hpet_ctrl_state *st = b->st;
    errval_t err;
    HPET_DEBUG("(add_mapping) label:%s, class:%s (%" PRIu64 ", %" PRIu64 ") to "
               "(%" PRIu64 ", %" PRIu64 ")\n",
               label, class, from.addr, from.msg, to.addr, to.msg);

    // activate fsb interrupt
    if(to.port == 0){
        err = hpet_comp_enable_fsb_int(st->comp_st, to.addr, to.msg);
    } else {
        err = hpet_comp_enable_ioapic_int(st->comp_st, to.msg);
    }
    // hpet->timer[from.port].fsb_address = to.addr
    if(err_is_fail(err)){
        DEBUG_ERR(err, "map_fsb_int");
    }
};

static void hpet_ctrl_bind_cb(void *stin, errval_t err,
                              struct int_route_controller_binding *bind) {

    HPET_DEBUG("(hpet_ctrl_bind_cb) bind_cb\n");
    struct hpet_ctrl_state *st = stin;
    bind->rx_vtbl.add_mapping = add_mapping;
    bind->tx_vtbl.register_controller(bind, BLOCKING_CONT, st->label, "");
    // Store state in binding
    st->bind = bind;
    st->bound = true;
}

errval_t init_hpet_int_controller(struct hpet_comp_st *comp_st, const char * lbl) {

    errval_t err;
    HPET_DEBUG("(init_hpet_int_controller) start, label=%s\n", lbl);

    // initializng hpet controller state
    struct hpet_ctrl_state *st = malloc(sizeof(struct hpet_ctrl_state));
    st->bound = false;
    st->comp_st = comp_st;
    strncpy(st->label, lbl, sizeof(st->label));

    // Connect to int route service
    iref_t int_route_service;
    err = nameservice_blocking_lookup("int_ctrl_service", &int_route_service);
    if (!err_is_ok(err)) {
        DEBUG_ERR(err, "Could not lookup int_route_service\n");
        return err;
    }

    err = int_route_controller_bind(int_route_service, hpet_ctrl_bind_cb, st,
                                    get_default_waitset(),
                                    IDC_BIND_FLAGS_DEFAULT);

    if (!err_is_ok(err)) {
        DEBUG_ERR(err, "int bind\n");
        return err;
    }

    while (!st->bound)
        event_dispatch(get_default_waitset());

    return SYS_ERR_OK;
}
