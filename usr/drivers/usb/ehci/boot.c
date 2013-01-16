/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <pci/pci.h>

#include <dev/ehci_dev.h>

#include "usb_manager_client.h"
#include "boot.h"

#include "ehci.h"      // Has device structures
#include "async_queue.h"       // In init function

#define GBYTES (1024 * 1024 * 1024)

#define ENABLE   0x1
#define DISABLE  0x0

//XXX: Enable this macro for EHCI debugging
//#define EHCI_LOCAL_DEBUG

#include "ehci_debug.h"

static struct device_mem *bar_info = NULL;
static bool ehci_initialized = false;
static bool enable_64 = false;
static uint64_t nr_allocated_bars = 0;
static uint64_t total_int = 0;

static void ehci_init(void);
static void ehci_post_init(void);
static void enable_global_routing(void);
static void print_ehci_device(void);
static void set_ehci_status(ehci_t *dev, uint8_t val);
static void port_scanner(void);

/*
 * Sanity check for Mackerel generated structs
 */
static void assert_ehci_reg(void)
{
    assert(sizeof (struct ehci_usb_cmd_t) == 4);
    assert(sizeof (struct ehci_usb_status_t) == 4);
    assert(sizeof (struct ehci_usb_int_t) == 4);
    assert(sizeof (struct ehci_frame_index_t) == 4);
    assert(sizeof (struct ehci_ctrl_dss_reg_t) == 4);
    assert(sizeof (struct ehci_flba_reg_t) == 4);
    assert(sizeof (struct ehci_asyn_list_reg_t) == 4);
    assert(sizeof (struct ehci_config_flag_t) == 4);
    assert(sizeof (struct ehci_portsc_t) == 4);
}

/**
 * \brief A sanity check function for the device. According to the
 *        documentation (Intel EHCI HC) after EHCI reset following
 *        registers must have these values.
 */
static void assert_reset(void)
{
    volatile uint32_t val;
    assert_ehci_reg();

    val = ehci_usb_cmd_rd_raw(&ehci_device);
    assert(val == 0x000080000 || val == 0x00080B00);

    val = ehci_usb_status_rd_raw(&ehci_device);
    assert(val == 0x00001000);

    val = ehci_usb_int_rd_raw(&ehci_device);
    assert(val == 0x00000000);

    val = ehci_frame_index_rd_raw(&ehci_device);
    assert(val == 0x00000000);

    val = ehci_ctrl_dss_reg_rd_raw(&ehci_device);
    assert(val == 0x00000000);

    val = ehci_config_flag_rd_raw(&ehci_device);
    assert(val == 0x00000000);
}


/**
 * \brief  Performs hardware reset for EHCI. There is another software
 * reset (light) which has different effect and preserves corresponding
 * registers & values.Hardware (hard) reset resets all registers.
 */
static void ehci_reset(void)
{
    volatile uint32_t i;
    dprintf("\n EHCI: Performing EHCI reset ...");
    // Host controller reset
    ehci_usb_cmd_hcr_wrf(&ehci_device, 1);

    // Give time to stablize
    for (i = 0; i < 50000; i++)
        thread_yield();
    assert_reset();
}


/**
 * \brief This function prepares and setup all required mapping before
 *        actually initializing the actual device. It maps bar info and
 *        locate the operational EHCI device.
 */
static void ehci_pre_init(void)
{
    uint8_t cap_length = 0x0;
    uint64_t op_base = 0x0;
    ehci_hcc_params_t ex_cap_64;
    uint64_t ram_upper = GBYTES;
    ram_upper *= 4;             // 4 GB RAM affinity, 32 bit HC
    char *buff = (char *)malloc(2000 * sizeof (char));
    if (ehci_initialized) {
        USER_PANIC("\nEHCI: double call ehci_pre_init\n");
    }

    dprintf("Preparing to map device memory bar\n");
    assert(nr_allocated_bars > 0);

    // Get the virtual address
    // XXX: Assuming that we just have one bar
    //bar_info[0].bytes = 1 << bar_info[0].bits;


    assert(bar_info[0].type == 0);
    dprintf("bar_info physical address %lx bits %lx bytes %lx\n",
            bar_info[0].paddr, bar_info[0].bits, bar_info[0].bytes);
    assert(bar_info[0].bytes > 0);

    map_device(&bar_info[0]);
    dprintf("Host Controller memory is mapped at vaddr: %lx\n",
            bar_info[0].vaddr);

    // init the device capability struct
    ehci_initialize(&ehci_device, (void *)(bar_info[0].vaddr), NULL);
    dprintf("Capability device init done\n");

    // Locating the operational device address
    cap_length = ehci_cap_length_rd(&ehci_device);
    assert(cap_length != 0);
    op_base = bar_info[0].vaddr + cap_length;

    dprintf("Operational base address found to be at %lx\n", op_base);
    dprintf("Initializing the operational device...\n");
    ehci_initialize(&ehci_device, 
		    (void *)(bar_info[0].vaddr), 
		    op_base );

    ex_cap_64 = ehci_hcc_params_rd(&ehci_device);
    dprintf("Checking 64 bit addressing capabilities\n");
    if (ex_cap_64.ex_ac == 0) {
        // Controller is not capable of
        // addressing 64 bit mode, switch to 32 bit
        dprintf("32 bit capable EHCI controller found,\
                         Setting memory affinity\n");
        ram_set_affinity(0, ram_upper);
        dprintf("RAM affinity set\n");
        enable_64 = false;
    } else {
        dprintf("64 bit capable EHCI controller found\n");
        enable_64 = true;
        // Make corresponding chages and setting everywhere
        //FIXME: Disable it and switch it to 32 bit
        assert(!"64 bit HC, NYI");
    }

    dprintf("Registers mapped successfully\n");
    ehci_hci_revision_pr(buff, 2000, &ehci_device);
    dprintf("%s\n", buff);
    free(buff);

    dprintf("ALL WORK DONE, passing on to reset \n");
    ehci_reset();
    ehci_init();
    ehci_initialized = true;

    // scan ports
    port_scanner();
}

/*
 * \brief Generic EHCI device priniting function which interacts with
 *        Mackerel device and prints current stats of the EHCI.
 */
static void print_ehci_device(void)
{
    dprintf("Printing EHCI status ... \n");
    char buff[1024];
    ehci_pr(buff, sizeof(buff), &ehci_device);
    dprintf("%.*s\n", (int)sizeof(buff), buff);
}

/**
 * \brief This function just detects and prints stats about companion
 *        controllers (CC). CC could be OHCI or UHCI. Though UHCI are rare.
 */
static void handle_cc(void)
{
    int no_of_cc, global_routing, port_routing_logic, ports_per_cc;
    ehci_hcs_params_t hc_capabilities;
    hc_capabilities = ehci_hcs_params_rd(&ehci_device);
    no_of_cc = hc_capabilities.n_cc;
    port_routing_logic = hc_capabilities.prr;
    ports_per_cc = hc_capabilities.n_pcc;

    dprintf("Number of companion controllers detected are %d\n", no_of_cc);
    dprintf("Number of ports per CC are %d\n", ports_per_cc);
    dprintf("Port routing logic is (Must be complementry to CF) %d\n",
            port_routing_logic);

    global_routing = (ehci_config_flag_rd(&ehci_device)).cf;

    // If routing not enabled then do it NOW.
    if (global_routing) {
        dprintf("Global routing already enabled ..\
                                hence no CC will be used\n");
    } else {
        dprintf("Enabling global routing\n");
        enable_global_routing();
    }
}


/**
 * \brief Function to enable global routing. After enabling global
 *        routing all port realted activities and notifications will
 *        be routed to root hub (EHCI).
 */
static void enable_global_routing(void)
{
    // Enabale global routing
    ehci_config_flag_cf_wrf(&ehci_device, 0x01);
}


/*
 * \brief Performs the hardware port reset on connected port
 *        After reset if HI-speed (USB 2.0) device is detected
 *        then enabled the power logic.
 */
static void reset_and_power(int port_num)
{
    int i;
    int port_enable;
    port_enable = (ehci_portsc_arr_rd(&ehci_device, port_num)).p_ed;
    assert(port_enable == 0);   // Port must be disabled before reset

    if ((ehci_portsc_arr_rd(&ehci_device, port_num)).line_status == 1) {
        dprintf("A LOW/FULL speed device is connected on port [%d]\n",
                port_num);
        dprintf("Not configuring the device, skipping ahead\n");
    } else {
        // Disable the port
        ehci_portsc_arr_p_ed_wrf(&ehci_device, port_num, 0x00);
        // Reset the port
        ehci_portsc_arr_p_reset_wrf(&ehci_device, port_num, 0x1);

        // Wait 2 mseconds. how ? dummy loop here

        // XXX: FIXME: How to set it precisely ?
        // Sometimes depending upon thread_yield
        // it waits for a very long time
        for (i = 0; i < 100000; i++)
            thread_yield();

        // Stop the reset process.
        ehci_portsc_arr_p_reset_wrf(&ehci_device, port_num, 0x00);

        // Stablize, same issue
        for (i = 0; i < 100000; i++)
            thread_yield();
        // Check status of the port, MUST be 1
        assert((ehci_portsc_arr_rd(&ehci_device, port_num)).p_ed == 0x01);
        // If this was zero then we have a
        // full speed device (strange ??)
        // Not supported. Hence device can not be
        // controlled with EHCI
    }
}

/*
 * EHCI status dumping function
 */
/*
static void print_ehci_status(void)
{
        return ;
        char *buff = (char *) malloc(10000 * sizeof(char));
        ehci_usb_status_t usb_status;
        usb_status = ehci_usb_status_rd(&ehci_device);
        ehci_usb_status_prtval(buff, 10000, usb_status);
        dprintf("%s \n", buff);
        free(buff);
}*/


/*
 * \brief Notifies the USB manager about a new USB deivce.
 */
static void config_device(int port_num)
{
    //print_ehci_status();
    dprintf("Sending port info to USB manager %llu\n",
            (unsigned long long int)rdtsc());
    notify_new_device(port_num);
}

/*
 * Notifies the USB manager about the disconnected device
 */
static void device_disconnect_logic(int port)
{
    dprintf("*** USB DEVICE REMOVED \n");
    notify_device_removal(port);
}

/*
 * \brief This function is invoked upon receiving an interrupt from
 *        the HC. It scans all USB ports and look for status change.
 *        Status change could be device connect or disconnect event.
 *        It then notifies USB manager to make proper changes in
 *        topology tree and scheduling. USB manager is responsible for
 *        driver notification and interaction.
 */
static void port_scanner(void)
{
    /*
     * Device enumeration is done one-by-one and there
     * SHALL only be one ZERO addressed device
     * XXX: This code should be fine because USB manager has
     * one by one IPC processing.
     */

    int total_no_ports, i, sz, no_activity_ports = 0;
    char *buff = NULL;
    sz = 2000;
    total_no_ports = (ehci_hcs_params_rd(&ehci_device)).n_ports;

    // Buffer to hold printing dump
    buff = (char *)malloc(sz * sizeof (char));

    dprintf("Number of REPORTED downstream ports are [%d]\n", total_no_ports);
    total_no_ports = (total_no_ports > 15) ? 15 : total_no_ports;
    dprintf("Number of NEW downstream ports are [%d]\n", total_no_ports);

    // Scan all ports
    for (i = 0; i < total_no_ports; i++) {
        // Device is connected && status changed
        if ((ehci_portsc_arr_rd(&ehci_device, i)).ccs == 1
            && ehci_portsc_arr_rd(&ehci_device, i).csc == 1) {
            ehci_portsc_arr_pri(buff, sz, &ehci_device, i);
            dprintf("%s\n", buff);
            dprintf("Device detected at [%d]\n", i);
            dprintf("Initializing the reset and power logic\n");
            reset_and_power(i);
            config_device(i);
        }
        // Device is disconnected and status changed
        else if ((ehci_portsc_arr_rd(&ehci_device, i)).ccs == 0
                 && ehci_portsc_arr_rd(&ehci_device, i).csc == 1) {
            dprintf("Device disconnect detected !!\n");
            device_disconnect_logic(i);

        } else {
            dprintf("\n EHCI: No activity on port no [%d]/[%d]\n",
                    i, total_no_ports);
            no_activity_ports++;

        }

    }
    dprintf("----- END OF PORT SCAN -----\n");

    free(buff);
}

/*
 * Scans and prints few details here and there.
 */
static void ehci_post_init(void)
{
    handle_cc();
}

static void set_ehci_status(ehci_t *dev, uint8_t val)
{
    volatile uint8_t new_val, status, timeout = 100;
    status = ehci_usb_status_rd(dev).hc_halt;       // Halt status
    new_val = ehci_usb_cmd_rd(dev).rs;

    dprintf("ehci status is R/S bit [%d] Halt [%d]\n", new_val, status);

    if (status == 0x0)          // EHCI is already running
        return;

    ehci_usb_cmd_rs_wrf(dev, val);  // Enable the ehci in RUN state
    dprintf("Wrote %d to USB_COMMAND register on R/S field\n", val);

    while (timeout > 0) {
        status = ehci_usb_status_rd(dev).hc_halt;
        new_val = ehci_usb_cmd_rd(dev).rs;

        dprintf("R/S status check status [%d] cmd [%d]\n", status, new_val);
        timeout--;

        if (status == 0x0)
            // Command to enable EHCI is
            // executed it is in RUN state
            break;
        else
            continue;
    }

    dprintf("EXIT >> HCI R/S status check status [%d] cmd [%d]\n",
            status, new_val);
    if (timeout == 0 && status != 0x0)
        assert(0);
}


/**
 * \brief This is EHCI initialization function, which writes different
 *        registers with a valid value. This is a 5 step process as
 *        documented in Intel EHCI documentation.
 */
static void ehci_init(void)
{
    // STEP 1. Write CTRLDSEGMENT to locate 4 G segment
    // For 32 bit HC this will be all zero.
    ehci_ctrl_dss_reg_wr_raw(&ehci_device, (0x0));

    //STEP 2. Setup Interrupt map
    dprintf("Enabling the interrupts\n");
    ehci_usb_int_usbi_e_wrf(&ehci_device, ENABLE);

    dprintf("Async Advanc          : [%d]\n", ENABLE);
    ehci_usb_int_iaa_e_wrf(&ehci_device, ENABLE);

    dprintf("Host system errors    : [%d]\n", ENABLE);
    ehci_usb_int_hye_e_wrf(&ehci_device, ENABLE);

    // XXX: Periodic schedule not enabled
    dprintf("Frame list roll over  : [%d]\n", DISABLE);
    ehci_usb_int_flr_e_wrf(&ehci_device, DISABLE);

    dprintf("Port change           : [%d]\n", ENABLE);
    ehci_usb_int_pci_e_wrf(&ehci_device, ENABLE);

    // Enabale USB transaction error interrupts
    dprintf("Error reporting       : [%d]\n", ENABLE);
    ehci_usb_int_usbei_e_wrf(&ehci_device, ENABLE);


    // XXX STEP 3. PERIODICLIST_BASE register with periodic frame list
    // FIXME; Setup the queue
    //ehci_flba_reg_wr_raw(&ehci_device,
    //periodic_address.frame_id.base); // Lower 32 bits, in either
    //mode 32 or 64 we need them


    // STEP 4. USBCMD to set interrupt rate and ON/RUN bit to start
    // controller in hardware.
    ehci_usb_cmd_itc_wrf(&ehci_device, ehci_uframe_1); // ehci_uframes_8
    //ehci_usb_cmd_itc_wrf(&ehci_device, ehci_uframes_64);

    set_ehci_status(&ehci_device, 1); // set EHCI ON

    // XXX 4.2 = Set up the async queue managment
    // Already taken care by async_init

    // STEP 5. Write 1 to CONFIGFLAG to route all ports to EHCI controller
    enable_global_routing();

    // 6. Call post init config
    ehci_post_init();
}

/**
 * \brief The interrupt handler code. Most of the times, upon receiving an
 *        interrupt it just post notification on semaphore and different
 *        thread takes care of it.
 */

static void handle_interrupt(void *args)
{
    dprintf("An interrupt recevied. Probing for the source \
                of interrupt [%lu] \n", total_int + 1);
    ehci_usb_status_t usb_status = ehci_usb_status_rd(&ehci_device);

    // acknowledge all pending interrupts
    // XXX: These bits are WC, hence writing 1 will ACK and clear the bit
    ehci_usb_status_wr(&ehci_device, usb_status);

    ehci_usb_int_t usb_int = ehci_usb_int_rd(&ehci_device);

    total_int++;
    bool handled = false;
    if (usb_status.hs_err == 0x1 && usb_int.hye_e == 0x1) {
        // Host system error
        dprintf("Interrupt due to HOST SYSTEM ERROR\n");
        assert(!"Host system hardware error: NYI soft reboot");
        // Call handler here
        handled = true;
    }

    if (usb_status.pcd == 0x1 && usb_int.pci_e == 0x1) {
        // Port change detected
        dprintf("Interrupt due to PORT STATUS CHANGE\n");
        port_scanner();
        dprintf("Port scanning done\n");
        handled = true;
    }

    if (usb_status.usb_ei == 0x1 && usb_int.usbei_e == 0x1) {
        // Transaction error
        dprintf("\n\n ******* EHCI: interrupt due to"
                "TRANSACTION ERROR ******** [%lu] \n\n", total_int);

        // Notify the core to update list
        notify_remove();
        handled = true;
    }

    if (usb_status.i_aa == 0x1 && usb_int.iaa_e == 0x1) {
        // On Async advancement
        dprintf("Interrupt due to ASYNC ADVANCMENT  [%lu]\n", total_int);
        notify_remove();
        handled = true;
    }

    if (usb_status.flr == 0x1 && usb_int.flr_e == 0x1) {
        // Frame list rollover, used in periodic schedule
        //ehci_usb_status_flr_wrf(&ehci_device, 0x1);       // ACK
        dprintf("Interrupt due to FRAME LIST ROLLOVER flr = %x\n",
                ehci_frame_index_rd_raw(&ehci_device));
        handled = true;
    }

    if (usb_status.usb_i == 0x1) {
        // ioc interrupt, also issued on short packets
        //ehci_usb_status_usb_i_wrf(&ehci_device, 0x1);     // ACK
        dprintf("Interrupt due to IOC ioc, ACKed and cleared [%lu]\n",
                total_int);

        notify_scan();
        handled = true;
    }

    if (!handled) {
        print_ehci_device();
        USER_PANIC(" \n\n EHCI: Unknown source of interrupt ?? \n\n");
    }
}


/**
 * \brief An initialization function which calls init functions of
 *        different modules.
 */
static void init_all_modules(void)
{
    static bool init = false;
    if (!init) {
        // XXX: There is no particular
        // inter-dependencies between these
        // modules but "memory manager" must be
        // the first one. Rest of the modules
        // use it for memory allocation.
        usb_mem_init();
        set_ehci_core_id(disp_get_core_id());
        async_queue_init();
        init = true;
    } else
        USER_PANIC("\n EHCI: HCD init blunder...init called TWICE\
                                , check calling code");
}

/**
 * \brief  PCI call back function, which upon discovery of the device
 *         (here EHCI) inititates other logics.
 */
static void pci_callback(struct device_mem *pci_bar_info,
                         uint64_t nr_allocated_regions)
{
    bar_info = pci_bar_info;
    nr_allocated_bars = nr_allocated_regions;
    dprintf("Number of allocated region = %lx  \
                bits %lx bytes %lx\n", nr_allocated_regions, bar_info[0].bits, bar_info[0].bytes);

    init_all_modules();
    ehci_pre_init();
}

/**
 * \brief ehci_boot takes care of hardware booting of EHCI host controller.
 *        It connects to the PCI server and asks for the device. If found
 *        then proceeds to init it.
 */

int ehci_boot(void *args)
{
    errval_t r;
    //XXX: Don't know what it is, but logic copied from e1000n
    uint32_t function = -1;

    dprintf("Starting the EHCI controller hardware.\n");
    dprintf("Trying to connect to Chip service server.\n");
    r = pci_client_connect();
    assert(err_is_ok(r));
    dprintf("Connected to Chips server.\n");

    for (r = 0; r < 0xFFFF; r++)
        thread_yield();

    r = pci_register_driver_irq(pci_callback, PCI_CLASS_SERIAL,
                                PCI_SUB_USB, PCI_IF_USB_EHCI, PCI_DONT_CARE,
                                PCI_DONT_CARE,
                                PCI_DONT_CARE, PCI_DONT_CARE, function,
                                handle_interrupt, NULL);

    assert(err_is_ok(r));
    dprintf("PCI call done ...waiting for the callback ....\n");
    return 0;
}
