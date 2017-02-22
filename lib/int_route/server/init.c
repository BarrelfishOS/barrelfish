/**
 * \file
 * \brief Contains handler functions for server-side octopus interface RPC call.
 */

/*
 * Copyright (c) 2009, 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <inttypes.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <skb/skb.h> // read list
#include <int_route/int_route_server.h>
#include <int_route/int_route_debug.h>

#include <if/int_route_service_defs.h>
#include <if/int_route_controller_defs.h>

struct controller_driver {
   char * label; // Label used in the SKB
   char * class; // Label used in the SKB
   struct int_route_controller_binding * binding; //
   struct controller_driver * next; // Linked list next
};

/* This server exports two interfaces:
 * * RPC interface - This is RPC the interface that
 * * Controller interface - Interrupt controllers use a
 */

struct controller_driver * controller_head;
static int exported = 0;

//static struct controller_driver * find_controller(char * lbl, struct controller_driver *d){
//    if(d == NULL){
//        return NULL;
//    } else if(strcmp(d->lbl, lbl) == 0){
//        return d;
//    } else {
//        return find_controller(lbl, d->next);
//    }
//}

static struct controller_driver * add_controller(struct controller_driver * d){
    INT_DEBUG("%s: enter\n", __FUNCTION__);
    struct controller_driver * cur;
    if(controller_head == NULL){
        controller_head = malloc(sizeof(struct controller_driver));
        cur = controller_head;
    } else if(d != NULL && d->next != NULL){
        return add_controller(d->next);
    } else {
        assert(d != NULL);
        d->next = malloc(sizeof(struct controller_driver));
        cur = d->next;
    }

    // Initialize cur
    cur->next = NULL;
    cur->binding = NULL;
    cur->label = NULL;
    cur->class = NULL;
    return cur;
}

/*
 * Finds a controller for the given label/class combination.
 * First, if a label is passed, it tries to match the label exactly, if no controller is found, it
 * goes on to match on the class.
 */
static struct controller_driver * find_controller(char * label, char * class){
    if(label != NULL && strlen(label) > 0) {
        for(struct controller_driver * cur = controller_head; cur != NULL; cur = cur->next){
            if(strcmp(label, cur->label) == 0){
                return cur;
            }
        }
    }
    if(class != NULL && strlen(class) > 0) {
        for(struct controller_driver * cur = controller_head; cur != NULL; cur = cur->next){
            if(strcmp(class, cur->class) == 0){
                return cur;
            }
        }
    }
    return NULL;
}

static int_route_controller_int_message_t build_int_message(uint64_t port, char * in){
    int_route_controller_int_message_t ret = {0,0};

    if(strcmp(in,"nullMsg") == 0){
        ret.addr = port;
        ret.msg = 0;
        return ret;
    }

    if(strncmp(in, "mem_write", strlen("mem_write")) == 0){
        sscanf(in, "mem_write(%"SCNu64",%"SCNu64")", &ret.addr, &ret.msg);
        assert(port == 0);
        return ret;
    }

    if(sscanf(in,"%"SCNu64,&ret.msg) == 1){
        ret.addr = port;
        return ret;
    }

    INT_DEBUG("Could not parse int_message: %s\n", in);
    return ret;
}

static errval_t read_route_output_and_tell_controllers(void){
    char * out = skb_get_output();
    INT_DEBUG("skb output: %s\n", out);

    // Parse output and instruct controller
    int inport, outport;
    char class[256];
    char lbl[256];
    char inmsg[256], outmsg[256];

    for(char * pos = out; pos - 1 != NULL && *pos != 0; pos = strchr(pos,'\n')+1 ) {
        int res = sscanf(pos, "%255[^,\n],%255[^,\n],%d,%255[^,\n],%d,%255[^,\n]",
                lbl, class, &inport, inmsg, &outport, outmsg);
        if(res != 6) {
            debug_printf("WARNING: Invalid SKB response. (%d)\n", __LINE__);
            continue;
        }
        INT_DEBUG("Scanned %d args: %s %s %d %s %d %s\n", res, lbl, class,
                inport, inmsg, outport, outmsg );

        struct controller_driver * dest = find_controller(lbl, class);
        if(dest == NULL){
            INT_DEBUG("No controller driver found.");
        } else {
            errval_t err;
            err = int_route_controller_add_mapping__tx(dest->binding,
                    BLOCKING_CONT, lbl, class, build_int_message(inport, inmsg),
                    build_int_message(outport, outmsg));
            assert(err_is_ok(err));
        }
    }
    return SYS_ERR_OK;
}

#define INVALID_VECTOR ((uint64_t)-1)

static void driver_route_call(struct int_route_service_binding *b,
        struct capref intsource, struct capref intdest){
    INT_DEBUG("%s: enter\n", __FUNCTION__);
    errval_t err;

    uint64_t int_src_num = INVALID_VECTOR;
    err = invoke_irqsrc_get_vec_start(intsource, &int_src_num);
    uint64_t int_src_num_high = INVALID_VECTOR;
    err = invoke_irqsrc_get_vec_end(intsource, &int_src_num_high);

    // TODO: Maybe it would be better to pass a IRQ offset into
    // the capability to the route call. So that the client
    // doesnt have to do retype all the time.
    if(int_src_num != int_src_num_high){
        err = SYS_ERR_IRQ_INVALID;
        DEBUG_ERR(err, "IrqSrc cap must contain only one vec");
        b->tx_vtbl.route_response(b, NOP_CONT, err);
        return;
    }

    assert(err_is_ok(err));

    uint64_t dest_vec = INVALID_VECTOR;
    err = invoke_irqdest_get_vector(intdest, &dest_vec);
    assert(err_is_ok(err));

    //TODO fix this
    uint64_t dest_cpu = INVALID_VECTOR;
    err = invoke_irqdest_get_cpu(intdest, &dest_cpu);
    assert(err_is_ok(err));

    printf("Int route service: Routing request, (int=%"PRIu64") to "
            "(cpu=%"PRIu64",vec=%"PRIu64")\n",
            int_src_num, dest_cpu, dest_vec);

    const char * template = "find_and_add_irq_route(%"PRIu64",%"PRIu64",%"PRIu64").";
    int q_size = strlen(template) + 3 * 16;
    char * query = malloc(q_size);
    snprintf(query, q_size, template, int_src_num, dest_cpu, dest_vec);
    err = skb_execute(query);
    if(err_is_fail(err)){
        DEBUG_SKB_ERR(err, "%s failed", query);
        b->tx_vtbl.route_response(b, NOP_CONT, err);
        return;
    }

    err = read_route_output_and_tell_controllers();
    if(err_is_fail(err)){
        DEBUG_ERR(err, "Error read_route_and_tell_controllers.\n");
    }
    b->tx_vtbl.route_response(b, NOP_CONT, err);
}

static void ctrl_register_controller(struct int_route_controller_binding *_binding,
        const char *label, const char *class) {
    struct controller_driver * c = add_controller(controller_head);
    c->label = malloc(strlen(label)+1);
    assert(c->label != NULL);
    strcpy(c->label, label);

    c->class = malloc(strlen(class)+1);
    assert(c->class != NULL);
    strcpy(c->class, class);
    INT_DEBUG("ctrl_register_controller, label=%s, class=%s\n",c->label, c->class);

    c->binding = _binding;
}


static struct int_route_service_rx_vtbl driver_rx_vtbl = {
        .route_call = driver_route_call

};

static struct int_route_controller_rx_vtbl ctrl_rx_vtbl = {
        .register_controller = ctrl_register_controller
};

static errval_t driver_connect_cb(void *st, struct int_route_service_binding *b) {
    INT_DEBUG("%s: enter\n", __FUNCTION__);
    b->st = NULL;
    b->rx_vtbl = driver_rx_vtbl;
    return SYS_ERR_OK;

}

static errval_t ctrl_connect_cb(void *st, struct int_route_controller_binding *b) {
    INT_DEBUG("%s: enter\n", __FUNCTION__);
    b->st = NULL;
    b->rx_vtbl = ctrl_rx_vtbl;
    return SYS_ERR_OK;

}

static void driver_export_cb(void *st, errval_t err, iref_t iref){
    INT_DEBUG("%s: enter\n", __FUNCTION__);
    assert(err_is_ok(err));

    err = nameservice_register("int_route_service", iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_register 1 failed");
    };
    exported++;
}

static void ctrl_export_cb(void *st, errval_t err, iref_t iref){
    INT_DEBUG("%s: enter\n", __FUNCTION__);
    assert(err_is_ok(err));

    err = nameservice_register("int_ctrl_service", iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_register 2 failed");
    };
    exported++;
}


// The main function of this service
errval_t int_route_service_init(void)
{
    errval_t err;
    INT_DEBUG("int_route_service_init\n");
    // We need skb connection
    skb_client_connect();

    // Export route service for PCI device drivers
    err = int_route_service_export(NULL, driver_export_cb, driver_connect_cb, get_default_waitset(),
        IDC_EXPORT_FLAGS_DEFAULT);

    if(err_is_fail(err)){
        USER_PANIC_ERR(err, "int_route_service_export failed");
    }

    err = int_route_controller_export(NULL, ctrl_export_cb, ctrl_connect_cb, get_default_waitset(),
         IDC_EXPORT_FLAGS_DEFAULT);
    if(err_is_fail(err)){
        USER_PANIC_ERR(err, "int_route_controller_export failed");
    }


    // XXX: Due to cyclic dependency, we must make sure the service has been exported before
    // returning.

    while(exported != 2){
        event_dispatch(get_default_waitset());
    }
    return SYS_ERR_OK;
}
