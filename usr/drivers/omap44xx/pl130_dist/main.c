/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr 6, CH-8092 Zurich.
 */

#include <stdio.h>
#include <stdlib.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <driverkit/driverkit.h>

//HACK
#include <barrelfish/sys_debug.h>
//ENDHACK

#include <dev/pl130_gic_dist_dev.h>
#include <if/int_route_controller_defs.h>

#include "debug.h"


struct pl130_dist_driver_state {
    pl130_gic_dist_t devgic;
    uint32_t it_num_lines;
    int cpu_index;
    int cpu_count;
};

enum IrqType {
    IrqType_SGI,
    IrqType_PPI,
    IrqType_SPI
};

/**
 * \brief Returns the IRQ type based on the interrupt ID
 *
 * We have three types of interrupts
 * 1) Software generated Interrupts (SGI): IDs 0-15
 * 2) Private Peripheral Interrupts (PPI): IDs 16-31
 * 3) Shared Peripheral Interrups (SPI): IDs 32-
 *
 * \return The type of the interrupt.
 */
static enum IrqType get_irq_type(uint32_t int_id)
{
    if (int_id < 16) {
        return IrqType_SGI;
    } else if (int_id < 32) {
        return IrqType_PPI;
    } else {
        return IrqType_SPI;
    }
}


__attribute__((unused))
static void gic_raise_softirq(struct pl130_dist_driver_state * ds, uint8_t cpumask, uint8_t irq)
{
    uint32_t regval = (cpumask << 16) | irq;
    pl130_gic_dist_ICDSGIR_wr(&ds->devgic, regval);
};


static errval_t pl130_dist_init(struct pl130_dist_driver_state * ds, mackerel_addr_t reg_base){
    pl130_gic_dist_initialize(&ds->devgic, reg_base);

    // read GIC configuration
    pl130_gic_dist_ICDICTR_t gic_config = pl130_gic_dist_ICDICTR_rd(&ds->devgic);

    // ARM GIC 2.0 TRM, Table 4-6
    // This is the number of ICDISERs, i.e. #SPIs
    // Number of SGIs (0-15) and PPIs (16-31) is fixed
    uint32_t it_num_lines_tmp =
        pl130_gic_dist_ICDICTR_it_lines_num_extract(gic_config);
    ds->it_num_lines = 32*(it_num_lines_tmp + 1);
    ds->cpu_count = pl130_gic_dist_ICDICTR_cpu_number_extract(gic_config) + 1;
    // TODO: determine cpu index
    ds->cpu_index = 0;

    PL130_DEBUG("interrupt lines = %d, cpu_count = %d\n", ds->it_num_lines,
            ds->cpu_count);

    // Distributor:
    // enable interrupt forwarding from distributor to cpu interface
    pl130_gic_dist_ICDDCR_enable_wrf(&ds->devgic, 0x1);

    return SYS_ERR_OK;
}


/**
 * \brief Enable an interrupt
 *
 * \see ARM Generic Interrupt Controller Architecture Specification v1.0
 *
 * \param int_id
 * \param cpu_targets 8 Bit mask. One bit for each core in the system.
 *    (chapter 4.3.11)
 * \param prio Priority of the interrupt (lower is higher). We allow 0..15.
 *    The number of priority bits is implementation specific, but at least 16
 *    (using bits [7:4] of the priority field, chapter 3.3)
 * \param 0 is level-sensitive, 1 is edge-triggered
 * \param 0 is N-to-N, 1 is 1-N
 */
static errval_t enable_interrupt(struct pl130_dist_driver_state *ds, int int_id,
    uint8_t cpu_targets, bool edge_triggered, bool one_to_n, uint16_t prio)
{

    uint32_t ind = int_id / 32;
    uint32_t bit_mask = (1U << (int_id % 32));
    enum IrqType irq_type = get_irq_type(int_id);

    // We allow PPI on any core, and SPI only on instance 0
    if(!(irq_type == IrqType_SPI && int_id <= ds->it_num_lines))
    {
        PL130_DEBUG("invalid int_id=%d on cpu=%d\n", int_id, ds->cpu_index);
        return SYS_ERR_IRQ_INVALID;
    }
    
    // Enable
    // 1 Bit per interrupt
    uint32_t regval = pl130_gic_dist_ICDISER_rd(&ds->devgic, ind);
    regval |= bit_mask;
    pl130_gic_dist_ICDISER_wr(&ds->devgic, ind, regval);

    // TODO: cleanup pl130 mackerel file so that we don't need bit magic
    // here.  -SG, 2012/12/13

    // Priority
    // 8 Bit per interrupt
    // chp 4.3.10
    ind = int_id/4;
    // XXX: check that priorities work properly, -SG, 2012/12/13
    prio = (prio & 0xF)<<4;
    switch(int_id % 4) {
    case 0:
        pl130_gic_dist_ICDIPR_prio_off0_wrf(&ds->devgic, ind, prio);
        break;
    case 1:
        pl130_gic_dist_ICDIPR_prio_off1_wrf(&ds->devgic, ind, prio);
        break;
    case 2:
        pl130_gic_dist_ICDIPR_prio_off2_wrf(&ds->devgic, ind, prio);
        break;
    case 3:
        pl130_gic_dist_ICDIPR_prio_off3_wrf(&ds->devgic, ind, prio);
        break;
    }

    // Target processors (only SPIs)
    // 8 Bit per interrupt
    ind = int_id/4;
    if (irq_type == IrqType_SPI) { // rest is ro
        switch (int_id % 4) {
        case 0:
            pl130_gic_dist_ICDIPTR_targets_off0_wrf(&ds->devgic, ind, cpu_targets);
            break;
        case 1:
            pl130_gic_dist_ICDIPTR_targets_off1_wrf(&ds->devgic, ind, cpu_targets);
            break;
        case 2:
            pl130_gic_dist_ICDIPTR_targets_off2_wrf(&ds->devgic, ind, cpu_targets);
            break;
        case 3:
            pl130_gic_dist_ICDIPTR_targets_off3_wrf(&ds->devgic, ind, cpu_targets);
            break;
        }
    }

    // Configuration registers
    // 2 Bit per IRQ
    ind = int_id/16;
    uint8_t val = ((edge_triggered&0x1) << 1) | (one_to_n&0x1);
    switch (int_id % 16) {
    case 0:
        pl130_gic_dist_ICDICR_conf0_wrf(&ds->devgic, ind, val);
        break;
    case 1:
        pl130_gic_dist_ICDICR_conf1_wrf(&ds->devgic, ind, val);
        break;
    case 2:
        pl130_gic_dist_ICDICR_conf2_wrf(&ds->devgic, ind, val);
        break;
    case 3:
        pl130_gic_dist_ICDICR_conf3_wrf(&ds->devgic, ind, val);
        break;
    case 4:
        pl130_gic_dist_ICDICR_conf4_wrf(&ds->devgic, ind, val);
        break;
    case 5:
        pl130_gic_dist_ICDICR_conf5_wrf(&ds->devgic, ind, val);
        break;
    case 6:
        pl130_gic_dist_ICDICR_conf6_wrf(&ds->devgic, ind, val);
        break;
    case 7:
        pl130_gic_dist_ICDICR_conf7_wrf(&ds->devgic, ind, val);
        break;
    case 8:
        pl130_gic_dist_ICDICR_conf8_wrf(&ds->devgic, ind, val);
        break;
    case 9:
        pl130_gic_dist_ICDICR_conf9_wrf(&ds->devgic, ind, val);
        break;
    case 10:
        pl130_gic_dist_ICDICR_conf10_wrf(&ds->devgic, ind, val);
        break;
    case 11:
        pl130_gic_dist_ICDICR_conf11_wrf(&ds->devgic, ind, val);
        break;
    case 12:
        pl130_gic_dist_ICDICR_conf12_wrf(&ds->devgic, ind, val);
        break;
    case 13:
        pl130_gic_dist_ICDICR_conf13_wrf(&ds->devgic, ind, val);
        break;
    case 14:
        pl130_gic_dist_ICDICR_conf14_wrf(&ds->devgic, ind, val);
        break;
    case 15:
        pl130_gic_dist_ICDICR_conf15_wrf(&ds->devgic, ind, val);
        break;
    }

    return SYS_ERR_OK;
}

static void add_mapping(struct int_route_controller_binding *b,
        const char *label,
        const char *class,
        int_route_controller_int_message_t from,
        int_route_controller_int_message_t to)
{
    errval_t err;
    PL130_DEBUG("add_mapping: label:%s, class:%s (%"PRIu64", %"PRIu64") to "
            "(%"PRIu64", %"PRIu64")\n", label, class, from.addr, from.msg, to.addr, to.msg);
    // TODO: CPU mask
    err = enable_interrupt(b->st, from.port, 0, 0, 0, 0);
    assert(err_is_ok(err));
}

static void bind_cb(void *st, errval_t err, struct int_route_controller_binding *b) {
    struct pl130_dist_driver_state * ds = st;

    if(!err_is_ok(err)){
        DEBUG_ERR(err, "Bind failure\n");
        return;
    }

    b->st = st;
    b->rx_vtbl.add_mapping = add_mapping;

    // Register this binding for all controllers with class pcilnk
    char label[128];
    snprintf(label, sizeof(label), "dist_%d", ds->cpu_index);
    const char * ctrl_class = "gic_dist";
    HERE;
    b->tx_vtbl.register_controller(b, NOP_CONT, label, ctrl_class);
}

static errval_t connect_to_irs(struct bfdriver_instance *bfi) {
    // Connect to int route service
    iref_t int_route_service;
    errval_t err;
    PL130_DEBUG("int_ctrl_service lookup...\n");
    err = nameservice_blocking_lookup("int_ctrl_service", &int_route_service);
    if (!err_is_ok(err)) {
        DEBUG_ERR(err, "Could not lookup int_route_service\n");
        return err;
    }

    HERE;
    err = int_route_controller_bind(
        int_route_service, bind_cb, bfi,
        get_default_waitset(), IDC_BIND_FLAGS_DEFAULT);

    if (!err_is_ok(err)) {
        DEBUG_ERR(err, "Could not bind int_route_service\n");
        return err;
    }
    HERE;

    return SYS_ERR_OK;
}


static errval_t init(struct bfdriver_instance* bfi, uint64_t flags, iref_t *dev) {
    errval_t err;
    PL130_DEBUG("Entering driver init. name=%s\n", bfi->name);

    bfi->dstate = malloc(sizeof(struct pl130_dist_driver_state));
    assert(bfi->dstate != NULL);

    // Map device registers
    struct capref mem_cap = {
        .cnode = bfi->argcn,
        .slot = 0
    };

    lvaddr_t dev_base;
    err = map_device_cap(mem_cap, &dev_base);
    USER_PANIC_ON_ERR(err, "map_device_cap");

    err = pl130_dist_init(bfi->dstate, (mackerel_addr_t)dev_base);
    USER_PANIC_ON_ERR(err, "pl130_dist_init");

    err = connect_to_irs(bfi->dstate);
    USER_PANIC_ON_ERR(err, "connect_to_irs");

    return SYS_ERR_OK;
}

static errval_t attach(struct bfdriver_instance* bfi) {
    return SYS_ERR_OK;
}

static errval_t detach(struct bfdriver_instance* bfi) {
    return SYS_ERR_OK;
}

static errval_t set_sleep_level(struct bfdriver_instance* bfi, uint32_t level) {
    return SYS_ERR_OK;
}

static errval_t destroy(struct bfdriver_instance* bfi) {
    struct pl130_dist_driver_state* uds = bfi->dstate;
    free(uds);
    bfi->dstate = NULL;
    // XXX: Tear-down the service
    bfi->device = 0x0;
    return SYS_ERR_OK;
}

static errval_t get_ep(struct bfdriver_instance* bfi, bool lmp, struct capref* ret_cap)
{   
    USER_PANIC("NIY \n");
    return SYS_ERR_OK;
}

DEFINE_MODULE(pl130_dist, init, attach, detach, set_sleep_level, destroy, get_ep);
