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
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <errno.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <skb/skb.h> // read list
#include <int_route/int_route_server.h>
#include <int_route/int_route_debug.h>

#include <if/int_route_service_defs.h>
#include <if/int_route_controller_defs.h>

struct controller_driver {
   char * label; // Label used in the SKB
   char * class; // Class used in the SKB
   struct int_route_controller_binding * binding; //
   struct controller_driver * next; // Linked list next
};

/* This server exports two interfaces:
 * * RPC interface - This is RPC the interface that
 * * Controller interface - Interrupt controllers use a
 */

struct controller_driver * controller_head;
static int exported = 0;

static struct controller_driver * add_controller(struct controller_driver * d){
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
 * First, if a label is passed, it tries to match the label exactly, if no
 * controller is found, it goes on to match on the class.
 */
static struct controller_driver *find_controller(const char *label,
                                                 const char *class) {
    if (label != NULL && strlen(label) > 0) {
        for (struct controller_driver *cur = controller_head; cur != NULL;
             cur = cur->next) {
            if (strcmp(label, cur->label) == 0) {
                return cur;
            }
        }
    }
    if (class != NULL && strlen(class) > 0) {
        for (struct controller_driver *cur = controller_head; cur != NULL;
             cur = cur->next) {
            if (strcmp(class, cur->class) == 0) {
                return cur;
            }
        }
    }
    return NULL;
}

struct controller_add_mapping_data {
    char * lbl;
    char * class;
    int_route_controller_int_message_t in_msg;
    int_route_controller_int_message_t out_msg;
    struct int_route_controller_binding *binding;
};

#define INVALID_PORT -1
#define INVALID_VECTOR ((uint64_t)-1)

/**
 * Since on ARMv7 the topolgy is easy, we don't use the SKB.
 */
static void driver_route_call_armv7(struct int_route_service_binding *b,
        struct capref intsource, int irq_idx,
        struct capref intdest){
    INT_DEBUG("%s: enter\n", __FUNCTION__);

    errval_t err;
    uint64_t int_src_num = INVALID_VECTOR;
    err = invoke_irqsrc_get_vec_start(intsource, &int_src_num);
    uint64_t int_src_num_high = INVALID_VECTOR;
    err = invoke_irqsrc_get_vec_end(intsource, &int_src_num_high);
    if(int_src_num + irq_idx > int_src_num_high || irq_idx < 0){
        err = SYS_ERR_IRQ_INVALID;
        DEBUG_ERR(err, "irq_idx out of range");
        b->tx_vtbl.route_response(b, NOP_CONT, err);
        return;
    }

    assert(err_is_ok(err));
    int_src_num += irq_idx;

    uint64_t dest_vec = INVALID_VECTOR;
    err = invoke_irqdest_get_vector(intdest, &dest_vec);
    assert(err_is_ok(err));

    uint64_t dest_cpu = INVALID_VECTOR;
    err = invoke_irqdest_get_cpu(intdest, &dest_cpu);
    assert(err_is_ok(err));

    printf("Int route service: Routing request, (int=%"PRIu64") to "
            "(cpu=%"PRIu64",vec=%"PRIu64")\n",
            int_src_num, dest_cpu, dest_vec);


    const char * class = "gic_dist";
    struct controller_driver * gic_dist = find_controller(NULL, class);

    if(gic_dist == NULL){
        err = SYS_ERR_IRQ_INVALID;
        DEBUG_ERR(err, "gic_dist controller not found!\n");
    } else {
        int_route_controller_int_message_t in_msg;
        in_msg.port = int_src_num;
        int_route_controller_int_message_t out_msg;
        out_msg.port = dest_cpu;
        err = int_route_controller_add_mapping__tx(gic_dist->binding,
                BLOCKING_CONT, NULL, class, in_msg,
                out_msg);
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
        .route_call = driver_route_call_armv7

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
int main(void) {
    errval_t err;
    debug_printf("Start\n");

    // Export route service for PCI device drivers
    err = int_route_service_export(NULL, driver_export_cb, driver_connect_cb,
                                   get_default_waitset(),
                                   IDC_EXPORT_FLAGS_DEFAULT);

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "int_route_service_export failed");
    }

    err = int_route_controller_export(NULL, ctrl_export_cb, ctrl_connect_cb,
                                      get_default_waitset(),
                                      IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "int_route_controller_export failed");
    }

    while (true) {
        event_dispatch(get_default_waitset());
    }

    return 0;
}
