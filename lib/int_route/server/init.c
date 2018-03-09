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

struct controller_add_mapping_data {
    char * lbl;
    char * class;
    int_route_controller_int_message_t in_msg;
    int_route_controller_int_message_t out_msg;
    struct int_route_controller_binding *binding;
};

/*
 * Parse string at pos, write parsed message into out. If pos_after is 
 * not null, the pointer after the parsing will be written there.
 * Returns success value
 *
 * Example: 23,nullMsg        -> {23,0,0}
 *          23,52             -> {23,52,0}
 *          23,mem_write(1,2) -> {23,1,2}
 */
static int parse_int_message(char *pos, int_route_controller_int_message_t *out,
        char **pos_after){ 
    if(*pos == ',') return 1;

    out->port = atoll(pos);
    char * n_pos = strchr(pos, ',');
    if(n_pos == NULL){
        return 1;
    }
    n_pos++;
    // n_pos points to after first comma

    if(strncmp(n_pos, "mem_write", strlen("mem_write")) == 0){
        char * int_1 = strchr(n_pos, '(');
        if(int_1 == NULL){
            return 1;
        }
        int_1++;
        out->addr = atoll(int_1);

        char * int_2 = strchr(int_1, ',');
        if(int_2 == NULL){
            return 1;
        }
        int_2++;
        out->msg = atoll(int_2);

        char * after = strchr(int_2, ')');
        if(after == NULL){
            return 1;
        }
        if(pos_after != NULL) *pos_after = after + 1;
    } else if(strncmp(n_pos, "nullMsg", strlen("nullMsg"))==0) {
        if(pos_after != NULL) *pos_after = n_pos + strlen("nullMsg"); 
        out->msg = 0;
        out->addr = 0;
    } else {
        out->msg = strtoll(n_pos, pos_after, 0);
        out->addr = 0;
        if(errno == EINVAL) {
            return 1;
        }
    }
    return 0;
}

//static int test_read(char *in, int_route_controller_int_message_t expected){
//    int_route_controller_int_message_t actual;
//    char * after = NULL;
//    int err = parse_int_message(in, &actual, &after);
//    if(err){
//        printf("Returned error code %d\n", err);
//    }
//    if(memcmp(&actual, &expected, sizeof(actual)) != 0){
//        printf("Parsing %s failed!\n", in);
//    }
//    if(*after != '\n'){
//        printf("After pointer set wrong\n");
//    }
//    return 0;
//}
//}
//
//static void test_read_all(void){
//    test_read("23,nullMsg\n", (int_route_controller_int_message_t){23,0,0});
//    test_read("23,42\n", (int_route_controller_int_message_t){23,42,0});
//    test_read("22,mem_write(33,44)\n", (int_route_controller_int_message_t){22,33,44});
//};


#define INVALID_PORT -1
static errval_t read_route_output_and_tell_controllers(void){
    //test_read_all();
    char * out = malloc(strlen(skb_get_output())+1);
    strcpy(out, skb_get_output());

    INT_DEBUG("skb output: %s\n", out);
    errval_t err;

    // Parse output and instruct controller
    char class[256];
    char lbl[256];

    for(char * pos = out; pos-1 != NULL && *pos != 0; pos = strchr(pos,'\n')+1 ) {
        // Sample output line: msix_0,msix,0,nullMsg,0,mem_write(4276092928,34)
               
        int matches = 0;  
        // lbl and class must not contain spaces:
        matches = sscanf(pos, "%[^,\n],%[^,\n]", lbl, class); 
        
        if(matches != 2) {
            debug_printf("WARNING: Invalid SKB response. (%d)\n", __LINE__);
            continue;
        }

        char * pos_1 = strchr(pos,',');
        if(pos_1 == NULL){
            debug_printf("WARNING: Invalid SKB response. (%d)\n", __LINE__);
        }
        pos = strchr(pos_1 + 1,',');
        if(pos == NULL){
            debug_printf("WARNING: Invalid SKB response. (%d)\n", __LINE__);
        }
        pos += 1;

        int_route_controller_int_message_t in_msg;
        err = parse_int_message(pos, &in_msg, &pos);
        if(err_is_fail(err)){
            debug_printf("WARNING: Invalid SKB response. (%d)\n", __LINE__);
            continue;
        }
        pos += 1;

        int_route_controller_int_message_t out_msg;
        err = parse_int_message(pos, &out_msg, &pos);
        if(err_is_fail(err)){
            debug_printf("WARNING: Invalid SKB response. (%d)\n", __LINE__);
            continue;
        }

        INT_DEBUG("Scanned args: '%s' '%s': (%"PRIu64",%"PRIu64",%"PRIu64") -> "
                "(%"PRIu64",%"PRIu64",%"PRIu64")\n", lbl, class,
                in_msg.port, in_msg.msg, in_msg.addr,
                out_msg.port, out_msg.msg, out_msg.addr);

        struct controller_driver * dest = find_controller(lbl, class);
        if(dest == NULL){
            INT_DEBUG("No ctrl driver found (lbl=%s,class=%s). Ignoring.\n",lbl,
                    class);
        } else {
            err = int_route_controller_add_mapping__tx(dest->binding,
                    BLOCKING_CONT, lbl, class, in_msg,
                    out_msg);
            assert(err_is_ok(err));
        }
    }

    free(out);
    return SYS_ERR_OK;
}

#define INVALID_VECTOR ((uint64_t)-1)

static void driver_route_call(struct int_route_service_binding *b,
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
