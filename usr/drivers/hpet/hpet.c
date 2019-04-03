/*
 * Copyright (c) 2018, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <driverkit/driverkit.h>
#include <flounder/flounder_support.h>
#include <dev/hpet_dev.h>
#include <hpet.h>
#include <hpet_debug.h>
#include <if/hpet_defs.h>
#include <octopus/getset.h>
#include <octopus/init.h>
#include <hw_records.h>

lvaddr_t hpet_vbase = 0;

static errval_t init(struct bfdriver_instance *bfi, uint64_t flags, iref_t *dev) {

    errval_t err;
    int uid = atoi(bfi->argv[0]);

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

    struct capref cnodecap;
    err = slot_alloc_root(&cnodecap);
    if(err_is_fail(err)){
        DEBUG_ERR(err, "slot_alloc_root");
        return err;
    }
    err = cap_copy(cnodecap, bfi->caps[0]);
    struct cnoderef arg_cnode = build_cnoderef(cnodecap, CNODE_TYPE_OTHER); 
    struct capref mem_cap = {
        .cnode = arg_cnode,
        .slot = HPET_MEM_CAP
    };

    err = map_device_cap(mem_cap, &hpet_vbase);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "unable to map HPET registers\n");
        return err;
    }

    // initialize mackerel
    hpet_t d;
    hpet_initialize(&d, (mackerel_addr_t)hpet_vbase);

    // configure general capabilities register
    hpet_gcap_id_t gcap_reg = hpet_gcap_id_rd(&d);
    char gcap_debug[1024];
    hpet_gcap_id_pr(gcap_debug, sizeof(gcap_debug), &d);
    printf("HPET: gcap register: \n%s\n", gcap_debug);

    err = skb_add_fact("hpet_gcap(%d, %" PRIu64 ")", uid, gcap_reg);
    if(err_is_fail(err)){
        DEBUG_ERR(err, "skb_add_fact");
    }
    
    if(hpet_gcap_id_leg_rt_cap_extract(gcap_reg)) {
        // TODO: Figure out when use legacy replacement.
        hpet_gen_conf_leg_rt_cnf_wrf(&d, 1); 
    }

    // Enable timer!
    hpet_gen_conf_enable_cnf_wrf(&d, 1);

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

static errval_t get_ep(struct bfdriver_instance *bfi, bool lmp, struct capref *ret_cap){
    *ret_cap = NULL_CAP;
    return SYS_ERR_OK;
}

DEFINE_MODULE(hpet_module, init, attach, detach, set_sleep_level, destroy, get_ep);
