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
#include <barrelfish/nameservice_client.h>

#include <driverkit/driverkit.h>
#include <int_route/int_model.h>
#include <int_route/int_route_client.h>
#include <int_route/msix_ctrl.h>

#include <flounder/flounder_support.h>
#include <hpet.h>
#include <hpet_debug.h>
#include <hpet_int_cntrl.h>
#include <if/hpet_defs.h>

static char buf[4096];
uint64_t count_val;
uint32_t clk_period;

static void hpet_int_handler(void *args) {
    HPET_DEBUG("\n ******************************************* \n");
    HPET_DEBUG("Interrupt Handler Function : an interrupt has been triggered ! "
               "\n \n ");
    HPET_DEBUG("\n ******************************************* \n");
    return;
}

errval_t map_fsb_int(uint64_t from_port, uint32_t msg_addr, uint32_t msg_data,
                     void *driver_state) {

    struct hpet_driver_state *hpetds = (struct hpet_driver_state *)driver_state;
    int timer_index = from_port;
    HPET_DEBUG("(map_fsb) From Port is %lu \n ", from_port);
    // 2- Set it to Edge-Triggered Mode
    hpet_timers_config_reg_timer_int_type_cnf_wrf(&hpetds->d, timer_index, 0);
    // 3- Set it FSB_ENB=1 & int enable =1
    hpet_timers_config_reg_timer_fsb_enb_cnf_wrf(&hpetds->d, timer_index, 1);
    hpet_timers_config_reg_timer_int_enb_cnf_wrf(&hpetds->d, timer_index, 1);

    // set the FSB reg values
    uint32_t value =
        msg_data; // unmask0-edge0-remote irr0-active high 0-0-fixed - 40
    uint32_t address = msg_addr;

    hpet_timers_fsb_int_route_reg_timer_fsb_int_val_wrf(&hpetds->d, timer_index,
                                                        value);
    hpet_timers_fsb_int_route_reg_timer_fsb_int_addr_wrf(&hpetds->d,
                                                         timer_index, address);

    HPET_DEBUG("(map_fsb_int)This FSB has val = %u and addr = %u \n",
               hpet_timers_fsb_int_route_reg_timer_fsb_int_val_rdf(&hpetds->d,
                                                                   timer_index),
               hpet_timers_fsb_int_route_reg_timer_fsb_int_addr_rdf(
                   &hpetds->d, timer_index));
    // set the comparator register
    hpet_timers_comparator_reg_wr(&hpetds->d, timer_index, DEFAULT_COMPARATOR);

    // Print its value
    hpet_timers_config_reg_pri(buf, sizeof(buf), &hpetds->d, timer_index);

    HPET_DEBUG("(map_fsb_int)Printing timer number %d configuration register "
               "value  with comp value %lu \n \n  %s \n",
               timer_index,
               hpet_timers_comparator_reg_rd(&hpetds->d, timer_index), buf);

    return SYS_ERR_OK;
}

static errval_t init(struct bfdriver_instance *bfi, const char *name,
                     uint64_t flags, struct capref *caps, size_t caps_len,
                     char **args, size_t args_len, iref_t *dev) {

    HPET_DEBUG("(init) enter \n");

    errval_t err;
    lvaddr_t vbase;

    // connect to SKB first
    err = skb_client_connect();
    // initilaize hpet driver state
    struct hpet_driver_state *hpetds;
    hpetds = calloc(sizeof(struct hpet_driver_state), 1);
    char *ptr;
    hpetds->int_start_range = strtoul(args[0], &ptr, 10);
    hpetds->int_end_range = strtoul(args[1], &ptr, 10);
    hpetds->msix_port = strtoul(args[2], &ptr, 10);
    bfi->dstate = hpetds;

    // use the caps to map physical address to virtual address
    err = map_device_cap(caps[HPET_MEM_CAP], &vbase);

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "(init) unable to map HPET registers \n");
        HPET_DEBUG("Unable to map HPET Registers \n ");
        return err;
    }

    // initialize the virtual base address to Mackarel
    hpet_initialize(&hpetds->d, (mackerel_addr_t)vbase);

    // configure general capabilities register
    uint64_t gcap_reg = hpet_gcap_id_rd(&hpetds->d);
    hpetds->nTimers = hpet_gcap_id_num_tim_cap_extract(gcap_reg);
    clk_period = hpet_gcap_id_counter_clk_per_cap_extract(gcap_reg);
    int dummy = hpet_gcap_id_prtval(buf, sizeof(buf), gcap_reg);

    HPET_DEBUG("(init) nTimers from general register:  %u \n", hpetds->nTimers);
    HPET_DEBUG("(init) Printing general register value \n  %s \n ", buf);
    for (int i = 0; i < hpetds->nTimers; i++) {
        uint8_t fsb = hpet_timers_config_reg_timer_fsb_int_delv_cap_rdf(&hpetds->d, i);
        skb_add_fact("%" PRIu8 " ", fsb);
    }
    // add Timers which support FSB to SKB and add supported interrupt vectors
    // for the others
    for (int i = 0; i < hpetds->nTimers; i++) {
        int port_number = hpetds->int_start_range + i;
        if (hpet_timers_config_reg_timer_fsb_int_delv_cap_rdf(&hpetds->d, i) ==
            1) // add it to SKB constraints
        {

            HPET_DEBUG("(init) Timer# %d supports FSB interrupt %u \n", i,
                       hpet_timers_config_reg_timer_fsb_int_delv_cap_rdf(
                           &hpetds->d, i));
            err = skb_add_fact("mapf_valid_class(hpet,hpet_0,%" PRIu32
                               ",_, %" PRIu64 ", mem_write(_,_))",
                               port_number, hpetds->msix_port);

            if (err_is_fail(err)) {
                HPET_DEBUG("Failed to add FSB fact for timer number %d \n", i);
                DEBUG_SKB_ERR(err, "FSB Fact error");
            }
        }

        // inserting valid i/o apic interrupts to SKB
        uint32_t int_route_cap =
            hpet_timers_config_reg_timer_int_rout_cap_rdf(&hpetds->d, i);
        HPET_DEBUG("int_route_cap for timer number %d is %d \n", i,
                   int_route_cap);
        err = skb_add_fact("hpet_ioapic(hpet_0,%" PRIu32 ",%" PRIu32 ")",
                           port_number, int_route_cap);
    }

    HPET_DEBUG("---------------------------------------------------------------"
               "-------------------- \n ");

    // initialize hpet interrupt controller
    err = init_hpet_int_controller(hpetds, vbase);
    err = int_route_client_connect();
    if (err_is_fail(err)) {
        HPET_DEBUG(" Error !! : Unable to connect to int routing servie  \n");
        USER_PANIC_ERR(err, "int-route-client error");
        return err;
    }

    // connect to Interrupt Routing Service
    err = int_route_client_route_and_connect(
        caps[HPET_INT_CAP], 0, get_default_waitset(), hpet_int_handler, NULL);
    if (err_is_fail(err)) {
        HPET_DEBUG("(init) Unable to connect to int routing servie \n");
        USER_PANIC_ERR(err, "int-route-client error");
        return err;
    }

    // To do: export service
    // err = hpet_export(NULL, export_cb, connect_cb, get_default_waitset(),
    // IDC_EXPORT_FLAGS_DEFAULT);

    // main counter register
    count_val = 1000; // this is the counter's start value
    hpet_main_cnt_wr(
        &hpetds->d,
        count_val); // have to set the counter when the timer is not working
    HPET_DEBUG("(init) Main Counter Value = %lu and clock time period = %u \n",
               hpet_main_cnt_rd(&hpetds->d), clk_period);
    // general configuration register
    uint64_t gconfig_reg =
        hpet_gen_conf_enable_cnf_insert(gconfig_reg, 1); // enable the timer
    HPET_DEBUG("(init) Counter just started  = %lu \n ",
               hpet_main_cnt_rd(&hpetds->d));
    gconfig_reg = hpet_gen_conf_leg_rt_cnf_insert(gconfig_reg, 0);
    hpet_gen_conf_wr(&hpetds->d, gconfig_reg);
    dummy = hpet_gen_conf_prtval(buf, sizeof(buf), gconfig_reg);
    HPET_DEBUG("(init) Printing Config register value : \n %s \n", buf);

    // interrupt status register
    uint64_t int_st_reg = hpet_gintr_sta_rd(&hpetds->d);
    dummy = hpet_gintr_sta_prtval(buf, sizeof(buf), int_st_reg);
    HPET_DEBUG(" (init) Printing interrupt register value \n  %s \n ", buf);

    // Test Routing
    // err=skb_execute_query("int_dest_port_list(Li).");
    err = skb_execute_query("print_controller_config(ioapic_0)");
    // err=skb_execute_query("print_controller_class_details(Lbl, ioapic)");
    if (err_is_fail(err)) {
        debug_printf("Trial for int_dest_port failed \n");
    } else {
        char *out = malloc(strlen(skb_get_output()) + 1);
        strcpy(out, skb_get_output());
        debug_printf("First Trial to print possible routings skb output: %s\n",
                     out);
    }

    // FSB Settings

    // // FSB Enable for timer 0
    // //1-Check if FSB_CAP =1
    // u_int64_t tim0= hpet_tim0_conf_rd(&hpetds->d);
    // uint8_t fsb_tim0 = hpet_tim_conf_timer_fsb_int_delv_cap_extract(tim0);

    // if(fsb_tim0==1){
    // //2- Set it to Edge-Triggered Mode
    //   hpet_tim0_conf_timer_int_type_cnf_wrf(&hpetds->d ,0);
    // //3- Set it FSB_ENB=1 & int enable =1
    //   hpet_tim0_conf_timer_fsb_enb_cnf_wrf(&hpetds->d , 1);
    //  hpet_tim0_conf_timer_int_enb_cnf_wrf(&hpetds->d , 1);

    // // set the FSB reg values
    //   uint32_t  value = 24; // unmask0-edge0-remote irr0-active high
    //   0-0-fixed - 40 uint32_t  address= 0xFEE04000;

    //   hpet_tim0_fsb_int_route_timer_fsb_int_val_wrf(&hpetds->d , value);
    //   hpet_tim0_fsb_int_route_timer_fsb_int_addr_wrf(&hpetds->d , address);

    //   debug_printf("\n \n \n \t \t t This FSB has val = %ui and addr = %u",
    //   hpet_tim0_fsb_int_route_timer_fsb_int_val_rdf(&hpetds->d),hpet_tim0_fsb_int_route_timer_fsb_int_addr_rdf(&hpetds->d));
    // // set the comparator register
    //   hpet_tim0_comp_wr( &hpetds->d, tim0_comp);

    //   //Print its value
    //   hpet_tim0_conf_pr(buf,sizeof(buf),&hpetds->d );
    //   printf("\n \n \t \t -*********** Printing timer0 register value  with
    //   comp value %lu \n \n \n \n \n  %s" ,hpet_tim0_comp_rd(&hpetds->d),
    //   buf);

    // }

    return SYS_ERR_OK;
}

static errval_t attach(struct bfdriver_instance *bfi) { return SYS_ERR_OK; }

static errval_t detach(struct bfdriver_instance *bfi) { return SYS_ERR_OK; }

static errval_t set_sleep_level(struct bfdriver_instance *bfi, uint32_t level) {
    return SYS_ERR_OK;
}

static errval_t destroy(struct bfdriver_instance *bfi) { return SYS_ERR_OK; }

DEFINE_MODULE(hpet_module, init, attach, detach, set_sleep_level, destroy);
