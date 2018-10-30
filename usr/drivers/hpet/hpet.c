/*
 * Copyright (c) 2018, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <driverkit/driverkit.h>
#include <flounder/flounder_support.h>
#include <hpet.h>
#include <hpet_debug.h>
#include <if/hpet_defs.h>
#include <octopus/getset.h>
#include <octopus/init.h>
#include <hw_records.h>

static errval_t init(struct bfdriver_instance *bfi, const char *name,
                     uint64_t flags, struct capref *caps, size_t caps_len,
                     char **args, size_t args_len, iref_t *dev) {

    errval_t err;
    lvaddr_t vbase;
    int uid = atoi(args[0]);

    // connect to octopus
    err = oct_init();
    if(err_is_fail(err)){
        DEBUG_ERR(err, "oct_init");
        return err;
    }

    // connect to SKB
    err = skb_client_connect();
    if(err_is_fail(err)){
        DEBUG_ERR(err, "skb_client_connect");
        return err;
    }

    err = map_device_cap(caps[HPET_MEM_CAP], &vbase);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "unable to map HPET registers\n");
        return err;
    }

    // initialize the virtual base address to Mackarel
    hpet_t d;
    hpet_initialize(&d, (mackerel_addr_t)vbase);

    // configure general capabilities register
    uint64_t gcap_reg = hpet_gcap_id_rd(&d);
    int n_timers = hpet_gcap_id_num_tim_cap_extract(gcap_reg);


    for (int i = 0; i < n_timers; i++) {
        uint32_t int_cap = hpet_timers_config_reg_timer_int_rout_cap_rdf(&d,i);
        uint8_t fsb_cap = hpet_timers_config_reg_timer_fsb_int_delv_cap_rdf(&d, i);
        HPET_DEBUG("hpet_comp(%d, %d, %" PRIu32 ",%" PRIu8 ")\n", uid, i, int_cap, fsb_cap);
        err = skb_add_fact(
            "hpet_comp(%d, %d, %" PRIu32 ",%" PRIu8 ")", uid, i, int_cap, fsb_cap);
        if(err_is_fail(err)){
            DEBUG_ERR(err, "skb_add_fact");
            continue;
        }

        err = oct_mset(SET_SEQUENTIAL, HW_HPET_COMP_RECORD_FORMAT, uid, i);
        if(err_is_fail(err)){
            DEBUG_ERR(err, "oct_set");
            continue;
        }
    }

    return SYS_ERR_OK;
}

static errval_t attach(struct bfdriver_instance *bfi) { return SYS_ERR_OK; }

static errval_t detach(struct bfdriver_instance *bfi) { return SYS_ERR_OK; }

static errval_t set_sleep_level(struct bfdriver_instance *bfi, uint32_t level) {
    return SYS_ERR_OK;
}

static errval_t destroy(struct bfdriver_instance *bfi) { return SYS_ERR_OK; }

DEFINE_MODULE(hpet_module, init, attach, detach, set_sleep_level, destroy);
