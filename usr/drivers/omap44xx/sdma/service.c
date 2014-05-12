/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr 6, CH-8092 Zurich.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <if/omap_sdma_defs.h>
#include <if/omap_sdma_thc.h>
#include <thc/thc.h>

#include "sdma.h"

static void run_service(struct omap_sdma_thc_service_binding_t *sv)
{
    omap_sdma_service_msg_t msg;
    bool loop = true;

    // this is the bitmap of messages we are interested in receiving
    struct omap_sdma_service_selector selector = {
        .mem_copy = 1, .mem_copy_2d = 1,
        .mem_fill = 1, .mem_fill_2d = 1,
    };

    while (loop) {
        // receive any message
        sv->recv_any(sv, &msg, selector);

        errval_t reterr = SYS_ERR_OK;

        // dispatch it
        switch(msg.msg) {
        case omap_sdma_mem_copy:
            reterr = mem_copy(
                msg.args.mem_copy.in.dst,
                msg.args.mem_copy.in.src);
            sv->send.mem_copy(sv, reterr);
            break;
        case omap_sdma_mem_fill:
            reterr = mem_fill(
                msg.args.mem_fill.in.dst,
                msg.args.mem_fill.in.color);
            sv->send.mem_fill(sv, reterr);
            break;
        case omap_sdma_mem_copy_2d:
            reterr = mem_copy_2d(
                msg.args.mem_copy_2d.in.dst,
                msg.args.mem_copy_2d.in.src,
                msg.args.mem_copy_2d.in.count,
                msg.args.mem_copy_2d.in.transparent,
                msg.args.mem_copy_2d.in.color);
            sv->send.mem_copy_2d(sv, reterr);
            break;
        case omap_sdma_mem_fill_2d:
            reterr = mem_fill_2d(
                msg.args.mem_fill_2d.in.dst,
                msg.args.mem_fill_2d.in.count,
                msg.args.mem_fill_2d.in.color);
            sv->send.mem_fill_2d(sv, reterr);
            break;
        default:
            debug_printf("unexpected message: %d\n", msg.msg);
            loop = false;
            break;
        }
    }

    free(sv);
}

void start_service(void)
{
    errval_t err;

    struct omap_sdma_thc_export_info e_info;
    struct omap_sdma_thc_service_binding_t *sv;
    struct omap_sdma_binding *b;
    iref_t iref;

    err = omap_sdma_thc_export(&e_info, "sdma",
                             get_default_waitset(),
                             IDC_EXPORT_FLAGS_DEFAULT,
                             &iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "thc export failed");
    }

    DO_FINISH({
        while(true) {
            err = omap_sdma_thc_accept(&e_info, &b);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "thc accept failed");
            }

            sv = malloc(sizeof(struct omap_sdma_thc_service_binding_t));
            assert(sv != NULL);

            err = omap_sdma_thc_init_service(sv, b, b);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "thc init failed");
            }

            ASYNC({run_service(sv);});
        }
    });
}
