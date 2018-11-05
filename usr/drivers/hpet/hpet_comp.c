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
#include <int_route/int_route_client.h>
#include <hpet.h>
#include <hpet_debug.h>
#include <hpet_int_cntrl.h>


static void int_handler(void *args) {
    struct hpet_comp_st *st = (struct hpet_comp_st *)args;
    debug_printf("***** HPET interrupt for %d\n", st->index);
}


static void hpet_comp_disable_int(struct hpet_comp_st *st) {
    hpet_timers_config_reg_timer_fsb_enb_cnf_wrf(&st->hpet_dev, st->index, 0); 
    hpet_timers_config_reg_timer_int_enb_cnf_wrf(&st->hpet_dev, st->index, 0);
};


errval_t hpet_comp_enable_fsb_int(struct hpet_comp_st *st,
                                  uint32_t msg_addr, uint32_t msg_data) {
    HPET_DEBUG("MAP FSB NYI!\n");
    return SYS_ERR_OK;
    hpet_timers_config_reg_timer_fsb_enb_cnf_wrf(&st->hpet_dev, st->index, 1); 
    hpet_timers_config_reg_timer_int_enb_cnf_wrf(&st->hpet_dev, st->index, 1);
    return SYS_ERR_OK;
}

errval_t hpet_comp_enable_ioapic_int(struct hpet_comp_st *st,
                                     uint32_t gsi) {
    HPET_DEBUG("MAP IOAPIC NYI!\n");
    return SYS_ERR_OK;
    hpet_timers_config_reg_timer_fsb_enb_cnf_wrf(&st->hpet_dev, st->index, 0); 
    hpet_timers_config_reg_timer_int_enb_cnf_wrf(&st->hpet_dev, st->index, 1);
    return SYS_ERR_OK;
}




static errval_t init(struct bfdriver_instance *bfi, const char *name,
                     uint64_t flags, struct capref *caps, size_t caps_len,
                     char **args, size_t args_len, iref_t *dev) {

    errval_t err;
    assert((void*)hpet_vbase != NULL);
    struct hpet_comp_st * st = (struct hpet_comp_st*) malloc(sizeof(struct hpet_comp_st));

    debug_printf("hpet_comp: init enter! argv[0]=%s,argv[1]=%s, hpet_vbase=%p \n",
            args[0], args[1], (void*)hpet_vbase); 

    hpet_initialize(&st->hpet_dev, (mackerel_addr_t)hpet_vbase);

    st->index = atoi(args[1]);
   
    // Disable all interrupts
    hpet_comp_disable_int(st);
    // Edge trigger
    hpet_timers_config_reg_timer_int_type_cnf_wrf(&st->hpet_dev, st->index, 0); 

    // Now instantiage hpet_comp int controller 
    err = init_hpet_int_controller(st, args[0]);
    if(err_is_fail(err)){
        DEBUG_ERR(err, "init_hpet");
        return err;
    }

    err = int_route_client_connect();
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "init_route_client");
        return err;
    }

    struct capref cnodecap;
    err = slot_alloc_root(&cnodecap);
    if(err_is_fail(err)){
        DEBUG_ERR(err, "slot_alloc_root");
        return err;
    }
    err = cap_copy(cnodecap, caps[0]);
    struct cnoderef arg_cnode = build_cnoderef(cnodecap, CNODE_TYPE_OTHER); 
    struct capref int_src_cap = {
        .cnode = arg_cnode,
        .slot = HPET_COMP_INT_CAP
    };

    if(1){
        err = int_route_client_route_and_connect(
            int_src_cap, 0, get_default_waitset(), int_handler, st);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "route_and_connect");
            return err;
        }
    }

    return err;
}

static errval_t attach(struct bfdriver_instance *bfi) { return SYS_ERR_OK; }

static errval_t detach(struct bfdriver_instance *bfi) { return SYS_ERR_OK; }

static errval_t set_sleep_level(struct bfdriver_instance *bfi, uint32_t level) {
    return SYS_ERR_OK;
}

static errval_t destroy(struct bfdriver_instance *bfi) { return SYS_ERR_OK; }

DEFINE_MODULE(hpet_comp_module, init, attach, detach, set_sleep_level, destroy);
