/**
 * \file
 * \brief Code for handling booting additional cores
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "monitor.h"
#include <bexec.h>
#include <target/beehive/barrelfish_kpi/coredata_target.h>
#include <barrelfish/dispatch.h>
#include <simctrl.h>

// XXX We can't relocate binaries on Beehive yet so need a 2nd copy of 
// everything!
#define MONITOR_NAME "beehive/sbin/monitor"
#define KERNEL_NAME "beehive/sbin/cpu"

#define APP_CORE_DATA_PAGES 600

static errval_t spawn_arch_load(lvaddr_t binary,
                        size_t binary_size, genvaddr_t *entry)
{
    // Look at the bexec header and pull out the entry point.
    // Don't need to relocate the binary - but we may want to record
    // that the module is in use to prevent two domains sharing the 
    // same data segment!
    bexec_t *bexec = (bexec_t *)binary;
    assert(bexec->bmagic == BEXEC_BMAGIC);
    assert(bexec->btorg != 0);

    *entry = bexec->btorg;

    return SYS_ERR_OK;
}

/* FIXME: find a header file for these! */
extern errval_t beehive_create_cap(coreid_t coreid, int chanid,
                                   struct capref *retcap);

extern errval_t beehive_chan_allocate(struct capref ep, int *chanid);

extern void intermon_bmp_setup(struct intermon_bmp_binding *b, struct capref bmp_cap,
                        struct lmp_endpoint *inep, struct capref inepcap);


// set up the monitor's side of a BMP binding given all the necessary state
void intermon_bmp_setup(struct intermon_bmp_binding *b, struct capref bmp_cap,
                        struct lmp_endpoint *inep, struct capref inepcap)
{

    // init our end of the binding and channel
    intermon_bmp_init(b, get_default_waitset());

    // XXX: stuff state into private parts of the binding to complete init
    b->bmp_state.chan.outepcap = bmp_cap;
    b->bmp_state.chan.outeplen = DEFAULT_BMP_BUF_WORDS;
    b->bmp_state.chan.inep = inep;
    b->bmp_state.chan.inepcap = inepcap;

    // XXX: run rx handler to cause it to register on waitset for recieve events
    intermon_bmp_rx_handler(b);
}

errval_t spawn_xcore_monitor(coreid_t coreid, int nopid, enum cpu_type cpu_type,
                             const char *cmdline /* XXX: currently ignored */,
                             struct intermon_binding **ret_binding)
{
    errval_t err;

    debug_printf("spawn_xcore_monitor %d\n", coreid);

    // XXX BUILD A DATASTRUCTURE TO PASS TO THE NEW KERNEL

    /* This will tell the new kernel where the monitor binary is and
       also pass it some initial memory for the kernel to use when
       creating the monitor process strutures. 
       
       We also allocate memory for use by the startup code of the new
       kernel, e.g. to create the monitor domain
    */

    assert(sizeof(struct beehive_core_data) <= BASE_PAGE_SIZE);
    size_t cpu_memory = BASE_PAGE_SIZE * APP_CORE_DATA_PAGES;
    struct capref cpu_memory_cap;
    err = frame_alloc(&cpu_memory_cap, cpu_memory, &cpu_memory);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_FRAME_ALLOC);
    }
    void *cpu_buf_memory;
    err = vspace_map_one_frame(&cpu_buf_memory, cpu_memory, 
                               cpu_memory_cap, NULL, NULL);
    if(err_is_fail(err)) {
        return err_push(err, LIB_ERR_VSPACE_MAP);
    }
    struct beehive_core_data *core_data = 
        (struct beehive_core_data*)cpu_buf_memory;

    debug_printf("BOOTINFO at %p\n", bi);
    core_data->bootinfo = bi;
    core_data->memory_base = (lvaddr_t)cpu_buf_memory + BASE_PAGE_SIZE;
    core_data->memory_limit = (lvaddr_t)cpu_buf_memory + cpu_memory;

    // construct name of kernel module for target core
    char namebuf[32];
    size_t namelen;
    namelen = snprintf(namebuf, sizeof(namebuf), "%s|%u", KERNEL_NAME, coreid);
    assert(namelen < sizeof(namebuf));

    /* Look up modules */
    struct mem_region *cpu_region = multiboot_find_module(bi, namebuf);
    if (cpu_region == NULL) {
        debug_printf("failed to find cpu driver module\n");
        return SPAWN_ERR_FIND_MODULE;
    }

    size_t cpu_binary_size;
    lvaddr_t cpu_binary;
    genpaddr_t cpu_binary_phys;

    err = spawn_map_module(cpu_region, &cpu_binary_size, &cpu_binary,
			   &cpu_binary_phys);
    if (err_is_fail(err)) {
	return err_push(err, SPAWN_ERR_MAP_MODULE);
    }
    debug_printf("CPU driver at %x\n", cpu_binary);

    /* Load the image */
    genvaddr_t cpu_entry;
    err = spawn_arch_load(cpu_binary, cpu_binary_size, &cpu_entry);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_LOAD);
    }

    // construct name of monitor module for target core
    namelen = snprintf(namebuf, sizeof(namebuf), "%s|%u", MONITOR_NAME, coreid);
    assert(namelen < sizeof(namebuf));
    struct mem_region *monitor_region = multiboot_find_module(bi, namebuf);
    if (monitor_region == NULL) {
        return SPAWN_ERR_FIND_MODULE;
    }

    core_data->cpu_module = (bexec_t *)cpu_binary;
    core_data->module_start = cpu_binary;
    core_data->module_end   = cpu_binary + cpu_binary_size;

    debug_printf("spawn_map_module..\n");

    size_t monitor_binary_size;
    lvaddr_t monitor_binary;
    genpaddr_t monitor_binary_phys;

    err = spawn_map_module(monitor_region, &monitor_binary_size, 
			   &monitor_binary, &monitor_binary_phys);
    if (err_is_fail(err)) {
	return err_push(err, SPAWN_ERR_MAP_MODULE);
    }
    debug_printf("monitor at %x\n", monitor_binary);

    core_data->monitor_module = (bexec_t *)monitor_binary;
    core_data->module_start = monitor_binary;
    core_data->module_end   = monitor_binary + monitor_binary_size;
    core_data->src_core_id = my_core_id;


    /*
     * Create the binding between this monitor and the monitor on the
     * newly spawned core. The other half of this connection setup is 
     * performed in boot_arch_app_core() below.
     */

    struct intermon_bmp_binding *bmp_binding = 
        malloc(sizeof(struct intermon_bmp_binding));
    assert(bmp_binding != NULL);

    // Create a normal IDC endpoint cap for incoming messages
    struct capref ep;
    struct lmp_endpoint *iep;
    err = endpoint_create(DEFAULT_BMP_BUF_WORDS, &ep, &iep);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "endpoint_create failed");
    }

    // XXX Need to allocate a kernel 'association' for incoming messages
    // And then set it up to deliver to the above endpoint
    int chanid;
    err = beehive_chan_allocate(ep, &chanid);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "beehive_chan_allocate failed");
    }

    // Record useful info in the core_data
    core_data->chanid = chanid;

    // Create a way to send beehive messages to new core
    // Need to cause the remote kernel to take these messages and 
    // deliver them to its monitor LMP endpoint.
    // Channel 0 on new core is always the monitor.
    struct capref beehive_cap;
    err = beehive_create_cap(coreid, 0, &beehive_cap);
    assert(err == SYS_ERR_OK);

    /* Invoke kernel capability to boot new core */
    debug_printf("invoke kernel cap...\n");
    err = invoke_monitor_spawn_core(coreid, cpu_type,
                                    (forvaddr_t)(lvaddr_t)core_data);
    if (err_is_fail(err)) {
        return err_push(err, MON_ERR_SPAWN_CORE);
    }

    {
        struct bmp_chan chan;
        struct bmp_recv_msg msg;
        uintptr_t sendmsg = 0xF00;

        /* Create a raw BMP chan */
        bmp_chan_init(&chan);
        chan.outepcap = beehive_cap;
        chan.outeplen = DEFAULT_BMP_BUF_WORDS;
        chan.inep = iep;
        chan.inepcap = ep;

        msg.buf.buflen= BMP_MSG_LENGTH;

        debug_printf("waiting for msgs...\n");
        for (int i = 0 ; i < 1000; i++) {
        retry:
            err = bmp_chan_recv(&chan, &msg);
            if (err_no(err) == LIB_ERR_NO_LMP_MSG) {
                sys_nop();
                goto retry;
            }
            bmp_chan_send(&chan, &sendmsg, 1);
        }
    }
    // init our end of the binding and channel
    intermon_bmp_setup(bmp_binding, beehive_cap, iep, ep);

    *ret_binding = &bmp_binding->b;

    return SYS_ERR_OK;
}

/**
 * \brief Initialize monitor running on app cores
 */
errval_t boot_arch_app_core(int argc, char *argv[],
                            coreid_t *ret_parent_coreid,
                            struct intermon_binding **ret_binding)
{
    errval_t err;

    assert(argc == 3);

    // First argument contains the bootinfo location
    //    bi = (struct bootinfo*)strtol(argv[1], NULL, 10);

    // core_id of the core that booted this core
    coreid_t core_id = strtol(argv[1], NULL, 10);
    *ret_parent_coreid = core_id;

    // other monitor's channel id
    assert(strncmp("chanid", argv[2], strlen("chanid")) == 0);
    int chanid = strtol(strchr(argv[2], '=') + 1, NULL, 10);

    // Create our own beehive cap
    struct capref beehive_cap;
    err = beehive_create_cap(core_id, chanid, &beehive_cap);
    assert(err == SYS_ERR_OK);

    struct capref ep;
    struct lmp_endpoint *iep;
    err = endpoint_create(DEFAULT_BMP_BUF_WORDS, &ep, &iep);
    assert(err_is_ok(err));

    int mychanid;
    err = beehive_chan_allocate(ep, &mychanid);
    assert(err_is_ok(err));
    assert(0 == mychanid);


    {
        struct bmp_chan chan;
        struct bmp_recv_msg msg;
        uintptr_t sendmsg = 0xF00;

        /* Create a raw BMP chan */
        bmp_chan_init(&chan);
        chan.outepcap = beehive_cap;
        chan.outeplen = DEFAULT_BMP_BUF_WORDS;
        chan.inep     = iep;
        chan.inepcap  = ep;

        /* Init message buf */
        msg.buf.buflen = BMP_MSG_LENGTH;
        
        /* Measure round-trip time */
        cycles_t before = bench_tsc();
        for (int i = 0 ; i < 1000; i++) {
	    BEE_SIMCTRL(BEE_SIMCTRL_CACHE_DELTA);
	    if (i == 555)
		BEE_SIMCTRL(BEE_SIMCTRL_TRACEON);
            bmp_chan_send(&chan, &sendmsg, 1);
        retry:
            err = bmp_chan_recv(&chan, &msg);
            if (err_no(err) == LIB_ERR_NO_LMP_MSG) {
                sys_nop();  // PUMP the BMP
                goto retry;
            }
	    if (i == 555)
		BEE_SIMCTRL(BEE_SIMCTRL_TRACEOFF);
        }
        cycles_t after = bench_tsc();
        debug_printf("Before %u after %u elapsed %u RTT %u\n",
                     before, after, (after-before), (after-before)/1000);
    }

    // setup our side of the binding
    struct intermon_bmp_binding *ibb;
    ibb = malloc(sizeof(struct intermon_bmp_binding));
    assert(ibb != NULL);

    intermon_bmp_setup(ibb, beehive_cap, iep, ep);

    *ret_binding = &ibb->b;

    return SYS_ERR_OK;
}
