/** \file
 *  \brief IDC system test code
 */

/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich.
 * Attn: Systems Group.
 */

#include <string.h>
#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/spawn_client.h>

#include <if/flounderbootstrap_defs.h>



static struct flounderbootstrap_binding *binding_ump;
static struct flounderbootstrap_binding *binding_lmp;
static struct flounderbootstrap_binding *binding;

static struct capref dom_lmp;
static struct capref dom_ump;

static struct capref ep_lmp;
static struct capref ep_ump;


struct state {
    const char *name;
    uint32_t counter;
    uint32_t acks;
};

/* */

static uint8_t phase = 0;


static void tx_init(struct flounderbootstrap_binding *b, struct capref cap)
{
    errval_t err;

    struct state *state = b->st;
    
    debug_printf("%s Sending tx_init\n", state->name);  


    if (state->counter == 2) {
        debug_printf("stopping with cap transfers. already sent two\n");
        err = flounderbootstrap_ack__tx(b, NOP_CONT);
        assert(err_is_ok(err));
        return;
    }

    state->counter++;

    err = flounderbootstrap_init__tx(b, NOP_CONT, cap);
    assert(err_is_ok(err));
}

static void tx_ack(struct flounderbootstrap_binding *b)
{
    errval_t err;

    struct state *state = b->st;

    debug_printf("%s Sending tx_ack\n", state->name);

    err = flounderbootstrap_ack__tx(b, NOP_CONT);
    assert(err_is_ok(err));
}

static void tx_test(struct flounderbootstrap_binding *b,
                    uint32_t arg)
{
    errval_t err;

    struct state *state = b->st;   

    debug_printf("%s Sending tx_test\n", state->name);

    err = flounderbootstrap_test__tx(b, NOP_CONT, arg);
    assert(err_is_ok(err));
}

/* ------------------------ COMMON MESSAGE HANDLERS ------------------------ */

static void rx_init(struct flounderbootstrap_binding *b,
                    struct capref cap)
{
    struct state *state = b->st;


    if (capref_is_null(cap)) {
        debug_printf("rx_init %s NULL_CAP\n", state->name);  
        tx_test(b, 0xdeadbeef);

    } else {
        errval_t err;

        struct frame_identity id;
        err = invoke_frame_identify(cap, &id);
        assert(err_is_ok(err));
        debug_printf("rx_init %s Frame: %lx\n", state->name, id.base);  
        tx_test(b, 0xb001b001);
    }
}

static void rx_ack(struct flounderbootstrap_binding *b)
{
    struct state *state = b->st;

    debug_printf("rx_ack %s\n", state->name);

    state->acks++;

    if (state->acks == 2) {
        phase++;
    } else {
        tx_test(b, 0xcafebabe);
    }

}

static void rx_test(struct flounderbootstrap_binding *b,
                    uint32_t arg)
{
    struct state *state = b->st;

    debug_printf("rx_ack %s, %u\n", state->name, arg);

    if (arg == 0xcafebabe) {
        tx_init(b, NULL_CAP);
    } else if (arg == 0xdeadbeef) {
        errval_t err;
        struct capref cap;
        err = frame_alloc(&cap, BASE_PAGE_SIZE, NULL);
        assert(err_is_ok(err));
        tx_init(b, cap);
    } else {
        tx_test(b, 0xcafebabe);
    }
}

static struct flounderbootstrap_rx_vtbl rx_vtbl = {
    .init = rx_init,
    .ack = rx_ack,
    .test = rx_test,
};

/* ------------------------------ CLIENT ------------------------------ */


static void bind_cont(void *st, errval_t err, struct flounderbootstrap_binding *b)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "bind failed");
    }

    debug_printf("client bound!\n");

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rx_vtbl;

    binding = b;

    tx_ack(b);
}


static void dddebug_cap(char *c, struct capref cap)
{

    char buf[512];
    debug_print_cap_at_capref(buf, 512, cap);

    debug_printf("%s CAP: %s\n", c, buf);
}


static void start_client(void)
{
    errval_t err;

    debug_printf("Starting Client...\n");

    struct capref ep =  {
        .cnode = build_cnoderef(cap_argcn, CNODE_TYPE_OTHER),
        .slot = 0
    };

    dddebug_cap("Client", ep);

    struct state *state = calloc(1, sizeof(*state));
    assert(state);

    state->name = "Client";

    debug_printf("Bind to endpoint...\n");

    err = flounderbootstrap_bind_to_endpoint(ep, bind_cont, state,
                                             get_default_waitset(), IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to bind...");
    }
}

/* ------------------------------ SERVER ------------------------------ */




static errval_t spawn_client(char *name, coreid_t core, void *st)
{
    errval_t err;

    struct capref *dom_cap;

    struct capref argcn;
    struct cnoderef argcnref;
    err = cnode_create_l2(&argcn, &argcnref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to create the argcn");
        return err;
    }

    struct state *state = calloc(1, sizeof(*state));
    assert(state);

    state->name = st;

    if (core == disp_get_core_id()) {
        debug_printf("Creating LMP Endpoint...\n");

        ep_lmp.cnode = argcnref;
        ep_lmp.slot  = 0;

        dom_cap = &dom_lmp;


        err = flounderbootstrap_create_endpoint(IDC_ENDPOINT_LMP, &rx_vtbl, state,
                                                get_default_waitset(),
                                                IDC_ENDPOINT_FLAGS_DUMMY,
                                                &binding_lmp, ep_lmp);

        dddebug_cap("server", ep_lmp);
    } else {

        debug_printf("Creating UMP Endpoint...\n");

        ep_ump.cnode = argcnref;
        ep_ump.slot  = 0;

        dom_cap = &dom_ump;


        err = flounderbootstrap_create_endpoint(IDC_ENDPOINT_UMP, &rx_vtbl, state,
                                                get_default_waitset(),
                                                IDC_ENDPOINT_FLAGS_DUMMY,
                                                &binding_ump, ep_ump);

        dddebug_cap("server", ep_ump);

    };

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to create the endpoint");
        return err;
    }

    char *argv[2];
    argv[0] = "client";
    argv[1] = NULL;

    return spawn_program_with_caps(core, name, argv, NULL, NULL_CAP, argcn,
                                   SPAWN_FLAGS_DEFAULT, dom_cap);
}


static void start_server(char *path)
{
    errval_t err;

    if (phase == 0) {
        debug_printf("=================================\n");
        debug_printf("LMP Test\n");
        debug_printf("=================================\n");
        err = spawn_client(path, disp_get_core_id(), "server LMP");
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "failed to start clients\n");
        }
    } else if (phase == 2) {
        debug_printf("=================================\n");
        debug_printf("UMP Test\n");
        debug_printf("=================================\n");
        coreid_t target = 1;
        if (disp_get_core_id() == 1) {
            target = 0;
        }
        err = spawn_client(path, target, "server UMP");
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "failed to start clients\n");
        }
    }
    phase++;
}

/* ------------------------------ MAIN ------------------------------ */

int main(int argc, char *argv[])
{
    errval_t err;

    debug_printf("%s: %u %s\n", argv[0], argc, "");

    uint8_t is_server =  0;
    if (argc == 1 && strcmp(argv[0], "client") == 0) {
        start_client();
    }  else {
        is_server = 1;
    }

    debug_printf("%s Going into message handler loop\n", is_server ? "SErver" : "Client");

    struct waitset *ws = get_default_waitset();
    while (1) {
        if (is_server && phase == 0) {
            start_server("tests/ep_basic");
        } else if (is_server && phase == 2) {
            start_server("tests/ep_basic");
        }
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
    }



    return EXIT_FAILURE;
}
