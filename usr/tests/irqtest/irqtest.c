/**
 * \file
 * \brief A simple test for checking if lpc_timer works
 * It tests periodic timers implemented by timer library.
 * test_A: When no arguments are given, then timer_test will run test_A
 * which starts two periodic timers and stops them after 100 callbacks
 * from each of them.
 *
 * test_B: When there are command line arguments given, then timer_test
 * will run test_B.  This test registers three periodic timer and stops
 * when 100 callbacks are received from all three timers.
 *
 * It is advised to run both test_A and test_B at same time.  It can be
 * done by inserting following lines into the menu.lst
 * module	/x86_64/sbin/lpc_timer core=1
 * module	/x86_64/sbin/timer_test core=2
 * module	/x86_64/sbin/timer_test core=3 B
 *
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/waitset.h>
#include <pci/pci.h>
#include "irqtest_debug.h"
#include "e1000n.h"
#include "stdint.h"

/*****************************************************************
 * Local states:
 *****************************************************************/
static uint32_t class = PCI_CLASS_ETHERNET;
static uint32_t subclass = PCI_DONT_CARE;
static uint32_t bus = PCI_DONT_CARE;
static uint32_t device = PCI_DONT_CARE;
static uint32_t function = PCI_DONT_CARE;
//static uint32_t deviceid = 0x1079; //sbrinz1/2
static uint32_t deviceid = 0x107d; //appenzeller
static uint32_t vendor = PCI_VENDOR_INTEL;
static uint32_t program_interface = PCI_DONT_CARE;
static e1000_mac_type_t mac_type = e1000_undefined;

/*****************************************************************
 * Local states:
 *****************************************************************/
static uint64_t minbase = -1;
static uint64_t maxbase = -1;

/*****************************************************************
 * e1000 states:
 *****************************************************************/
static e1000_t e1000;
static e1000_device_t e1000_device;
static bool e1000_initialized = false;
static uint8_t mac_address[MAC_ADDRESS_LEN]; /* buffers the card's MAC address upon card reset */

/*****************************************************************
 * Receive and transmit
 *****************************************************************/
static e1000_rx_bsize_t receive_buffer_size = bsize_16384;
static volatile struct tx_desc *transmit_ring;

//receive
static volatile union rx_desc *receive_ring;


//static uint32_t receive_bufptr = 0;

static void **receive_opaque = NULL;


#define DRIVER_RECEIVE_BUFFERS      (1024 * 8)
#define DRIVER_TRANSMIT_BUFFERS     (1024 * 8)

#define PACKET_SIZE_LIMIT       1073741824      /* 1 Gigabyte */







static void setup_internal_memory(void)
{
    receive_opaque = calloc(sizeof(void *), DRIVER_RECEIVE_BUFFERS);
    assert(receive_opaque != NULL );
}




static void e1000_init_fn(struct device_mem *bar_info, int nr_allocated_bars)
{
    IRQ_DEBUG("Starting hardware initialization.\n");
    e1000_hwinit(&e1000_device, bar_info, nr_allocated_bars, &transmit_ring,
                 &receive_ring, DRIVER_RECEIVE_BUFFERS, DRIVER_TRANSMIT_BUFFERS,
                 mac_address, false, 1);

    // Disable interrupt throttling
    e1000_itr_interval_wrf(e1000_device.device, 0);
    e1000_eitr_interval_wrf(e1000_device.device, 0, 0);

    e1000_initialized = true;
    IRQ_DEBUG("Hardware initialization complete.\n");

    setup_internal_memory();

}

/*****************************************************************
 * e1000 interrupt handler
 *
 ****************************************************************/
static int64_t interrupt_counter = 0;
static int64_t int_trigger_counter = 0;
static void e1000_interrupt_handler_fn(void *arg)
{
    /* Read interrupt cause, this also acknowledges the interrupt */
    e1000_intreg_t icr = e1000_icr_rd(e1000_device.device);

    printf("#### interrupt handler called: %"PRIi64"\n", interrupt_counter);
    ++interrupt_counter;

    if (e1000_intreg_rxt0_extract(icr) == 0) {
        return;
    }
}

static void e1000_reregister_handler(void *arg)
{
    errval_t err;
    printf("%s:%s:%d:\n", __FILE__, __FUNCTION__, __LINE__);
    err = pci_reregister_irq_for_device(
            class, subclass, program_interface,
            vendor, deviceid, bus, device, function,
            e1000_interrupt_handler_fn, NULL,
            e1000_reregister_handler, NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "pci_reregister_irq_for_device");
    }

    return;
}


int main(int argc, char **argv)
{
    errval_t err;

    /** Parse command line arguments. */
    IRQ_DEBUG("irq test started.\n");

    IRQ_DEBUG("argc = %d\n", argc);



    for (int i = 1; i < argc; i++) {
        IRQ_DEBUG("arg %d = %s\n", i, argv[i]);
        if (strcmp(argv[i], "auto") == 0) {
            continue;
        }
        if (strncmp(argv[i], "affinitymin=", strlen("affinitymin=")) == 0) {
            minbase = atol(argv[i] + strlen("affinitymin="));
            IRQ_DEBUG("minbase = %lu\n", minbase);
        } else if (strncmp(argv[i], "affinitymax=", strlen("affinitymax="))
                 == 0) {
            maxbase = atol(argv[i] + strlen("affinitymax="));
            IRQ_DEBUG("maxbase = %lu\n", maxbase);
        } else if(strncmp(argv[i],"bus=",strlen("bus=")-1)==0) {
            bus = atol(argv[i] + strlen("bus="));
            IRQ_DEBUG("bus = %ul\n", bus);
        } else if (strncmp(argv[i], "device=", strlen("device=")) == 0) {
            device = atol(argv[i] + strlen("device="));
            IRQ_DEBUG("device = %ul\n", device);
        } else if (strncmp(argv[i], "function=", strlen("function=")) == 0) {
            function = atol(argv[i] + strlen("function="));
            IRQ_DEBUG("function = %u\n", function);
        } else if (strncmp(argv[i], "deviceid=", strlen("deviceid=")) == 0) {
            deviceid = strtoul(argv[i] + strlen("deviceid="), NULL, 0);
            IRQ_DEBUG("deviceid = %u\n", deviceid);
        } else if (strcmp(argv[i], "-h") == 0 ||
                 strcmp(argv[i], "--help") == 0) {
            //exit_help(argv[0]);
        } else {
            IRQ_DEBUG("Parsed Kaluga device address %s.\n", argv[i]);
        }
    } // end for :

    if ((minbase != -1) && (maxbase != -1)) {
        IRQ_DEBUG("set memory affinity [%lx, %lx]\n", minbase, maxbase);
        ram_set_affinity(minbase, maxbase);
    }


    IRQ_DEBUG("Starting standalone driver.\n");

    /* Check if forced device id and vendor is known to be supported. */
    mac_type = e1000_get_mac_type(vendor, deviceid);
    if(mac_type == e1000_undefined){
        IRQ_DEBUG("WARNING: Passed deviceid unknown.\n");
    }


    /* Setup known device info */
    e1000_device.device = &e1000;
    e1000_device.mac_type = mac_type;
    e1000_device.device_id = deviceid;
    if (e1000_device.mac_type == e1000_82575
        || e1000_device.mac_type == e1000_82576
        || e1000_device.mac_type == e1000_I210
        || e1000_device.mac_type == e1000_I350) {
        // These cards do not have a bsex reg entry
        // therefore, we can't use 16384 buffer size.
        // If we use smaller buffers than 2048 bytes the
        // eop bit on received packets might not be set in case the package
        // is biger than the receive buffer size and we don't handle these
        // cases currently.
        e1000_device.rx_bsize = bsize_2048;
    } else {
        e1000_device.rx_bsize = receive_buffer_size;
    }
    e1000_device.media_type = e1000_media_type_undefined;


    IRQ_DEBUG("Connecting to PCI.\n");

    err = pci_client_connect();
    assert(err_is_ok(err));

    err = pci_register_driver_movable_irq(e1000_init_fn, class, subclass, program_interface,
                                          vendor, deviceid, bus, device, function,
                                          e1000_interrupt_handler_fn, NULL,
                                          e1000_reregister_handler,
                                          NULL);
    IRQ_DEBUG("########### Driver with interrupts ###########\n");


    if (err_is_fail(err)) {
        E1000_PRINT_ERROR("Error: %u, pci_register_driver failed\n", (unsigned int)err);
        exit(err);
    }

    IRQ_DEBUG("Registered driver.\n");

    IRQ_DEBUG("#### starting dispatch loop.\n");
    uint64_t ticks_per_msec, current_tick;
    uint64_t last_int_trigger_ticks = 0;
    sys_debug_get_tsc_per_ms(&ticks_per_msec);
    IRQ_DEBUG("Ticks per msec: %"PRIu64".\n", ticks_per_msec);
    assert(err_is_ok(err));

    while(true){
        err = event_dispatch_non_block(get_default_waitset());
        if(!err_is_ok(err) && err != LIB_ERR_NO_EVENT) {
            IRQ_DEBUG("Error in event_dispatch_non_block, returned %s\n",
                    err_getstring(err));
        }

        if(int_trigger_counter >= 20){
            if(abs(int_trigger_counter - interrupt_counter) < 3){
                printf("triggerred: %"PRIi64" and received %"PRIi64" interrupts. (+-2 is okay).\n",
                        int_trigger_counter, interrupt_counter);
                printf("TEST SUCCESS\n");
            }
            else {
                printf("triggerred: %"PRIi64" and received %"PRIi64" interrupts. (+-2 is okay).\n",
                        int_trigger_counter, interrupt_counter);
                printf("TEST FAILURE\n");
            }
            exit(0);
        }
        if(e1000_initialized){
            current_tick = rdtsc();
            if(last_int_trigger_ticks + ticks_per_msec*100 < current_tick){
                last_int_trigger_ticks = current_tick;
                IRQ_DEBUG("Creating Link change interrupt...\n");

                // Cause an (artificial) interrupt
                e1000_intreg_t ics = 0;
                ics = e1000_intreg_lsc_insert(ics, 1);
                e1000_ics_wr(e1000_device.device, ics);
                int_trigger_counter++;
            }
        }
    }

    return 1;
}
