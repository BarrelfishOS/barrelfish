/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "mass_storage_driver.h"

#include <usb/utility.h>
#include <usb/usb_device.h>
#include <usb/usb_pipe.h>
#include <usb/usb_services.h>

#include "../ehci/error_codes.h"
#include <usb/mem/usb_mem.h>
#include "scsi.h"
#include "../ehci/ehci_services.h"

//#define DRIVER_LOCAL_DEBUG
#include "driver_debug.h"
#include "usb_class_storage.h"

// How much data to read
#define KB100 200
#define MB1   (KB100 * 10)
#define MB10  (MB1 * 10)
#define MB30  (MB10 *3)
#define MB33  (0xFFFF)

#define DATA_SIZE MB33

usb_pipe_t pipe_in, pipe_out;
scsi_device_t *global_scsi_dev = NULL;


static inline void check(int r) __attribute__ ((always_inline));


static void driver_shutdown(void)
{
    dprintf("\n USB_DRIVER: Device disconnect detected ...");
    dprintf("\n Releasing driver resources ....");
    //XXX: right now I/O buffs are local to functions
}


/*
 * \brief checks the status of the transaction.
 */

static inline void check(int r)
{
    if (r > 0)                  // Good
        return;
    if (r == DEV_REMOVED)       // Device was removed
        driver_shutdown();
    else if (r < 0)             // Something is not right
        assert(0);
}


/*
 * Performs protocol level USB mass storage reset
 * on passed usb device dev.
 */

static int perform_mass_storage_reset(uint8_t dev)
{
    int r;
    usb_device_request req;

    /* As specified in USB mass stroge specification 1.0
     * at page number 7
     */

    req.bmRequestType = 0x21; // 0b00100001;
    req.bRequest      = 0xff; // 0b11111111;
    req.wValue        = 0x0000;

    // Current interface number, as saved in the device struct
    req.wIndex = get_curr_intf_num(dev);

    req.wLength = 0x0;

    r = usb_ctrl_exe(req, dev, EHCI_NO_DEBUG);

    if (r == 0) {
        dprintf("Reset done \n");
        // Give some time to stablize the device, 125 usec on Linux
        int i;
        for (i = 0; i < 0xFF; i++)
            thread_yield();

    } else {
        assert(!"USB_DRIVER: Reset failed\n");
    }

    return r;

}

/*
 * Getting logical unit can be done by 2 ways
 *    - Some devices report endpoint stall if they only support 1
 *      LUN and driver tried reading it. This is what is done in
 *      this function. If we encounter REQ_HALTED then we have only
 *      1 LUN device.
 *    - Few devcies also report this informtion in INQUIRY command
 *      and this information can be extracted from there.
 *
 *    In USB flash driver both works. So for now we implicitly
 *    assume that we have 1 LUN device.
 */

/*static int get_max_LUN(usb_device_t dev)
{
        int r;
        usb_device_request req;
        uint8_t data;

        //As specified in USB mass stroge specification 1.0
        //  at page number 7
        //

        req.bmRequestType = 0b10100001;
        req.bRequest     = 0b11111111;
        req.wValue = 0x0000;

        // Current interface number, as saved in the device struct
        req.wIndex = dev.config[dev.curr_config].curr_intf;

        // 1 byte of data is expected back
        req.wLength = 0x1;

        r = enqueue_usb_cmd_req(req, (void *)&data,
        sizeof(uint8_t), USB_DATA, dev.address,
        USB_DEFAULT_EP, EHCI_NO_DEBUG);

        if(r > 0)
        {
                dprintf("\n USB_DRIVER: Successfully read max LUN on"
               "device as %u...", data);
                return data;
        }else{
                if(r == REQ_HALTED)
                {
                        dprintf("\n USB_DRIVER: Mass storage device does"
                       "not support LUNs r = %d", r);
                        return 0;
                }
                else
                        dprintf("\n USB_DRIVER: Something went WRONG in "
                        "reading LUNs r = %d", r);
        }

        return r; // This is for last case

}*/

/*
 * \brief Packs and sends INQUIRY command to the USB device. See SCSI
 *        documentaion for more details.
 */

static void inquire_device(uint8_t dev, uint8_t LUN, scsi_device_t * scsi_dev)
{

    uint32_t tag = get_new_tag();
    uint32_t data_len = sizeof (inq_data_t);
    int r;

    usb_mem cmd_mem, data_mem, status_mem;
    cmd_mem = malloc_32();
    data_mem = malloc_64();
    status_mem = malloc_32();

    cbw_t cmd_wrapper;
    csw_t status_wrapper;

    inq_cmd_t inq;
    inq_data_t *data;

    // Set the command
    get_inquiry_command(data_len, &inq);

    init_new_cbw(tag, data_len, CBW_FLAG_IN,
                 sizeof (inq_cmd_t), (uint8_t *) & inq, &cmd_wrapper);

    init_new_csw(tag, &status_wrapper);

    // Assign them to usb_mem
    *((cbw_t *) (cmd_mem.va)) = cmd_wrapper;
    data = ((inq_data_t *) (data_mem.va));
    clear_arr((void *)data, sizeof (inq_data_t));

    print_inq_data(*data);

    *((csw_t *) (data_mem.va + 36)) = status_wrapper;
    dprintf("Size of command block wrapper %lu\n", sizeof (cbw_t));
    dprintf("Size of command status wrapper %lu\n", sizeof (csw_t));

    print_cbw(cmd_wrapper);

    // Stage 1, send COMMAND
    dprintf("------------- STAGE 1 --------------------\n");


    r = usb_bulk_exe(pipe_out, cmd_mem.pa, sizeof (cbw_t), EHCI_NO_DEBUG);
    check(r);


    dprintf("INQ command stauts = %d\n", r);
    dprintf("------------- END OF STAGE 1 --------------------\n");

    // Stage 2, get the IRQ data
    dprintf("------------- STAGE 2 --------------------\n");

    r = usb_bulk_exe(pipe_in, data_mem.pa, sizeof (inq_data_t), EHCI_NO_DEBUG);
    check(r);
    dprintf("INQ command DATA IN stauts = %x\n", r);
    dprintf("------------- END OF STAGE 2 ---------------\n");

    data = ((inq_data_t *) (data_mem.va));
    print_inq_data(*data);

    dprintf("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n");

    // Stage 3. Check the command status
    clear_arr(status_mem.va, sizeof (csw_t));

    r = usb_bulk_exe(pipe_in, status_mem.pa, sizeof (csw_t), EHCI_NO_DEBUG);
    check(r);

    dprintf("INQ command STATUS stauts = %d\n", r);
    status_wrapper = *((csw_t *) (status_mem.va));
    print_csw(status_wrapper);
    assert(!status_wrapper.bCSWStatus);
    assert(cmd_wrapper.dCBWTag == status_wrapper.dCSWTag);

    // Store the recorded information
    scsi_dev->inq_data = *data;

    free_32(cmd_mem);
    free_64(data_mem);
    free_32(status_mem);

}

/*
 * \brief Packs and sends command to read capacity of the storage device.
 *        See SCSI documentation for more details
 */

static void read_capacity(uint8_t dev, scsi_device_t * scsi_dev)
{
    uint32_t tag = get_new_tag();
    uint32_t data_len = sizeof (read_capacity_data_t);
    int r;

    usb_mem cmd_mem, data_mem, status_mem;
    cmd_mem = malloc_32();
    data_mem = malloc_64();
    status_mem = malloc_32();

    cbw_t cmd_wrapper;
    csw_t status_wrapper;

    read_capacity_t cap;
    read_capacity_data_t *cap_data;

    // Set the command
    get_read_capacity_command(&cap);

    init_new_cbw(tag, data_len, CBW_FLAG_IN,
                 sizeof (read_capacity_t), (uint8_t *) & cap, &cmd_wrapper);
    init_new_csw(tag, &status_wrapper);

    // Assign them to usb_mem
    *((cbw_t *) (cmd_mem.va)) = cmd_wrapper;
    cap_data = ((read_capacity_data_t *) (data_mem.va));
    clear_arr((void *)cap_data, sizeof (read_capacity_data_t));
    dprintf("Disk capacity dump ...\n");
    print_uint8(cap_data->data, 8);

    *((csw_t *) (status_mem.va)) = status_wrapper;
    dprintf("Size of command block wrapper %lu\n", sizeof (cbw_t));
    dprintf("Size of command status wrapper %lu\n", sizeof (csw_t));

    print_cbw(cmd_wrapper);

    // Stage 1
    dprintf(" ------------- STAGE 1 --------------------\n");
    r = usb_bulk_exe(pipe_out, cmd_mem.pa, sizeof (cbw_t), EHCI_NO_DEBUG);
    check(r);

    dprintf("READ_CAP command stauts = %d\n", r);
    dprintf("------------- END OF STAGE 1 --------------------\n");

    dprintf("\n\n ------------- STAGE 2 --------------------");
    r = usb_bulk_exe(pipe_in, data_mem.pa, sizeof (read_capacity_data_t),
                     EHCI_NO_DEBUG);
    check(r);

    dprintf("READ_CAP command DATA IN stauts = %x\n", r);
    dprintf("------------- END OF STAGE 2 ---------------\n");

    cap_data = ((read_capacity_data_t *) (data_mem.va));
    r = usb_bulk_exe(pipe_in, status_mem.pa, sizeof (csw_t), EHCI_NO_DEBUG);
    check(r);

    dprintf("READ_CAP command STATUS stauts = %d\n", r);
    status_wrapper = *((csw_t *) (status_mem.va));

    dprintf("READ_CAP comamnd stauts ========== %x\n",
            status_wrapper.bCSWStatus);
    print_csw(status_wrapper);
    dprintf("Ddisk capacity dump ...");
    print_uint8(cap_data->data, 8);
    assert(!status_wrapper.bCSWStatus);
    assert(cmd_wrapper.dCBWTag == status_wrapper.dCSWTag);

    init_scsi_dev(*cap_data, scsi_dev);

    free_32(cmd_mem);
    free_64(data_mem);
    free_32(status_mem);

}

/*
 * \brief Packs and sends READ10 command to the device to read a particular
 *        range of blocks.
 */

void read10(uint8_t dev, uint32_t block, uint16_t num,
            void *pbuff, uint8_t cache)
{

    uint32_t tag = get_new_tag();
    uint32_t data_len = sizeof (read10_data_t) * num;
    int r;

    usb_mem cmd_mem, status_mem;
    cmd_mem = malloc_32();
    status_mem = malloc_32();


    cbw_t cmd_wrapper;
    csw_t status_wrapper;

    read10_t cap;

    // Set the command
    get_read10_command(&cap, block, num, cache);

    init_new_cbw(tag, data_len, CBW_FLAG_IN, sizeof (read10_t),
                 (uint8_t *) & cap, &cmd_wrapper);
    dprintf("Read status total len = %u, %x, %u, %u, %x\n",
            data_len, data_len, num * 512, num, num);

    init_new_csw(tag, &status_wrapper);

    // Assign them to usb_mem
    *((cbw_t *) (cmd_mem.va)) = cmd_wrapper;
    dprintf("c1-%llu\n", (long long unsigned int)rdtsc());

    //XXX: Source of major overhead. Don't clear the array
    //for large requests here.
    //clear_arr((void *) cap_data, sizeof(read10_data_t) * num);

    dprintf("c2-%llu\n", (long long unsigned int)rdtsc());

    *((csw_t *) (status_mem.va)) = status_wrapper;

    //print_cbw(cmd_wrapper);

    // Stage 1
    // Stage one of the USB Mass storage protocol
    // Send the command
    dprintf("------------- STAGE 1 --------------------\n");
    dprintf("2. RDTSC before read command-- [%llu]\n",
            (long long unsigned int)rdtsc());
    dprintf("%llu\n", (long long unsigned int)rdtsc());

    r = usb_bulk_exe(pipe_out, cmd_mem.pa, sizeof (cbw_t), EHCI_NO_DEBUG);
    check(r);

    dprintf("5. RDTSC after command / before read data-- [%llu]\n",
            (long long unsigned int)rdtsc());
    printf("%llu\n", (long long unsigned int)rdtsc());

    dprintf("READ10 command stauts = %d\n", r);
    dprintf("------------- END OF STAGE 1 --------------------\n");

    // Stage 2, Read the expected data
    dprintf("------------- STAGE 2 --------------------\n");
    r = usb_bulk_exe(pipe_in, pbuff, num * sizeof (read10_data_t),
                     EHCI_NO_DEBUG);
    check(r);

    dprintf("6. RDTSC after command / before read data-- [%llu]\n",
            (long long unsigned int)rdtsc());
    printf("%llu\n", (long long unsigned int)rdtsc());
    dprintf("READ10 command DATA IN stauts = %x\n", r);
    dprintf("------------- END OF STAGE 2 ---------------\n");


    // Stage 3 Check the command status
    r = usb_bulk_exe(pipe_in, status_mem.pa, sizeof (csw_t), EHCI_NO_DEBUG);
    check(r);

    dprintf("7. RDTSC after read data-- [%llu]\n",
            (long long unsigned int)rdtsc());
    printf("%llu\n", (long long unsigned int)rdtsc());

    dprintf("READ10 command STATUS stauts = %d\n", r);
    status_wrapper = *((csw_t *) (status_mem.va));
    dprintf("READ10 comamnd stauts ========== %x\n", status_wrapper.bCSWStatus);
    //print_csw(status_wrapper);
    dprintf("\n USB_DRIVER: disk read10 dump ...");
    //Skip the large dumping of the data on console
    /*for(r=0; r<512*num;r++)
       {
       if(cap_data->data[r] >=65 && cap_data->data[r] <= 65+26)
       dprintf("\n [[%d] : %c]",
       r, (char) cap_data->data[r]);
       } */

    assert(!status_wrapper.bCSWStatus);
    assert(cmd_wrapper.dCBWTag == status_wrapper.dCSWTag);

    free_32(cmd_mem);
    free_32(status_mem);
    dprintf("8. RDTSC after complete function call -- [%llu]\n",
            (long long unsigned int)rdtsc());
    dprintf("%llu\n", (long long unsigned int)rdtsc());
}

/*
 * \brief Packs and sends WRITE10 command to the device to write a particular
 *        range of blocks.
 */

void write10(uint8_t dev, uint32_t block, uint16_t num, void *pbuff,
             uint8_t cache)
{

    uint32_t tag = get_new_tag();
    uint32_t data_len = sizeof (write10_data_t) * num;
    int r;

    usb_mem cmd_mem, status_mem;
    cmd_mem = malloc_32();
    status_mem = malloc_32();

    cbw_t cmd_wrapper;
    csw_t status_wrapper;

    write10_t cap;

    // Set the command
    get_write10_command(&cap, block, num, cache);

    init_new_cbw(tag, data_len, CBW_FLAG_IN, sizeof (write10_t),
                 (uint8_t *) & cap, &cmd_wrapper);
    init_new_csw(tag, &status_wrapper);

    // Assign them to usb_mem
    *((cbw_t *) (cmd_mem.va)) = cmd_wrapper;
    *((csw_t *) (status_mem.va)) = status_wrapper;

    print_cbw(cmd_wrapper);

    // Stage 1 of USB mass storage protocol
    // #1 Send command to the device
    dprintf("------------- STAGE 1 --------------------\n");
    r = usb_bulk_exe(pipe_out, cmd_mem.pa, sizeof (cbw_t), EHCI_NO_DEBUG);
    check(r);

    assert(r == 31);
    dprintf("WRITE command stauts = %d\n", r);
    dprintf("------------ END OF STAGE 1 --------------------\n");

    //Stage #2 Send the expected data to the device

    dprintf("------------- STAGE 2 --------------------\n");
    r = usb_bulk_exe(pipe_out, pbuff, num * sizeof (write10_data_t),
                     EHCI_NO_DEBUG);
    check(r);

    assert(r == data_len);
    dprintf("WRITE command DATA IN stauts = %x\n", r);
    dprintf("------------- END OF STAGE 2 ---------------\n");

    //Stage #3 Read the command status
    r = usb_bulk_exe(pipe_in, status_mem.pa, sizeof (csw_t), EHCI_NO_DEBUG);
    check(r);

    assert(r == 13);
    dprintf("WRITE command STATUS stauts = %d\n", r);
    status_wrapper = *((csw_t *) (status_mem.va));
    dprintf("WRITE comamnd stauts ========== %x\n", status_wrapper.bCSWStatus);
    print_csw(status_wrapper);

    assert(!status_wrapper.bCSWStatus);
    assert(cmd_wrapper.dCBWTag == status_wrapper.dCSWTag);
    dprintf("WRITE command sucessfull\n");

    free_32(cmd_mem);
    free_32(status_mem);
}

/*
 * \brief Sync the volatile cache to the persistent storage
 */

void sync_cache_on_dev(uint8_t dev, uint32_t block, uint16_t num)
{
    uint32_t tag = get_new_tag();
    int r;

    usb_mem cmd_mem, status_mem;
    cmd_mem = malloc_32();
    status_mem = malloc_32();


    cbw_t cmd_wrapper;
    csw_t status_wrapper;

    sync_cache_t cap;

    // Set the command
    get_sync_cache_command(&cap, block, num);

    init_new_cbw(tag, 0, CBW_FLAG_IN, sizeof (sync_cache_t),
                 (uint8_t *) & cap, &cmd_wrapper);
    init_new_csw(tag, &status_wrapper);

    // Assign them to usb_mem
    *((cbw_t *) (cmd_mem.va)) = cmd_wrapper;
    *((csw_t *) (status_mem.va)) = status_wrapper;

    print_cbw(cmd_wrapper);

    // Stage 1
    dprintf("\n\n ------------- STAGE 1 --------------------");

    r = usb_bulk_exe(pipe_out, cmd_mem.pa, sizeof (cbw_t), EHCI_NO_DEBUG);
    check(r);

    dprintf("SYNC command stauts = %d\n", r);
    dprintf("------------- END OF STAGE 1 --------------------\n");

    r = usb_bulk_exe(pipe_in, status_mem.pa, sizeof (csw_t), EHCI_NO_DEBUG);
    check(r);

    dprintf("SYNC command STATUS stauts = %d\n", r);
    status_wrapper = *((csw_t *) (status_mem.va));
    dprintf("SYNC comamnd stauts ========== %x\n", status_wrapper.bCSWStatus);
    print_csw(status_wrapper);

    assert(!status_wrapper.bCSWStatus);
    assert(cmd_wrapper.dCBWTag == status_wrapper.dCSWTag);

    free_32(cmd_mem);
    free_32(status_mem);
}


/*
 * \brief Test if the device is ready to accept new command
 */

static void test_device_ready(uint8_t dev, uint8_t LUN)
{
    uint32_t tag = get_new_tag();
    int r;

    usb_mem cmd_mem, status_mem;
    cmd_mem = malloc_32();
    status_mem = malloc_32();

    cbw_t cmd_wrapper;
    csw_t status_wrapper;

    inq_cmd_t test;             // Rename it to test ready

    test.op_code = 0x0;
    test.EVPD = 0x0;
    test.page_code = 0x0;
    test.MSB = 0x0;
    test.LSB = 0x0;
    test.ctrl = 0x0;

    init_new_cbw(tag, 0, CBW_FLAG_IN, sizeof (inq_cmd_t),
                 (uint8_t *) & test, &cmd_wrapper);
    init_new_csw(tag, &status_wrapper);

    // Stage 1. Send command
    // Assign it to wrapper
    *((cbw_t *) (cmd_mem.va)) = cmd_wrapper;
    print_cbw(*((cbw_t *) (cmd_mem.va)));
    r = usb_bulk_exe(pipe_out, cmd_mem.pa, sizeof (cbw_t), EHCI_NO_DEBUG);
    check(r);
    dprintf("%s command stauts = %d \n\n\n next setp 0\n", __func__, r);
    dprintf("%s command stauts = %d\n", __func__, r);
    dprintf("------------- END OF STAGE 1 --------------------\n");

    // Stage 2. Check the command status
    clear_arr(status_mem.va, sizeof (csw_t));
    print_csw(*((csw_t *) status_mem.va));
    r = usb_bulk_exe(pipe_in, status_mem.pa, sizeof (csw_t), EHCI_NO_DEBUG);
    check(r);

    dprintf("%s command STATUS stauts = %d\n", __func__, r);
    status_wrapper = *((csw_t *) (status_mem.va));
    print_csw(status_wrapper);
    assert(!status_wrapper.bCSWStatus);
    assert(cmd_wrapper.dCBWTag == status_wrapper.dCSWTag);

    free_32(cmd_mem);
    free_32(status_mem);
}

/*
 * \brief Exports the SCSI dev
 */

scsi_device_t *export_scsi_dev(void)
{
    return global_scsi_dev;
}


/*
 * Temp functionts to test FAT32 FS and flags.
 */

static int scan(char *buff, int i)
{
    return (buff[i] = 'F'
            && buff[i + 1] == 'A'
            && buff[i + 2] == 'T' && buff[i + 3] == '3' && buff[i + 4] == '2');

    return (buff[i] = 'A'
            && buff[i + 1] == 'N'
            && buff[i + 2] == 'I'
            && buff[i + 3] == 'M'
            && buff[i + 4] == 'E' && buff[i + 1] == 'S' && buff[i + 2] == 'H');

}

static void alter(char *buff, int i)
{
    //buff[i] = 'S';
    //buff[i+1] = 'O';
    //buff[i+2] = 'D';
    //buff[i+3] = 'S';
    //buff[i+4] = 'M';

    //buff[i] = 'M';
    //buff[i+1] = 'S';
    //buff[i+2] = 'D';
    //buff[i+3] = 'O';
    //buff[i+4] = 'S';
    for (i = 0; i < 512; i++)
        buff[i] = i;
}

volatile static uint8_t dev_args = 0;

/*
 * \brief Main entry point for the device driver
 */

static int msd_init(void *args)
{
    errval_t err = thread_detach(thread_self());
    assert(err_is_ok(err));
    assert(dev_args != 0);

    uint8_t dev = dev_args;

    dprintf("Device address ====[%u]\n", dev);
    assert(dev == 1);

    int r, i;
    uint8_t LUN;
    scsi_device_t scsi_dev;
    scsi_dev.block_size = 0;
    scsi_dev.block_last = 0;
    scsi_dev.usb_address = dev;
    usb_mem io_buff;


    dprintf("\n\n\n\n");
    printf("A mass storage device has been located\n");

    r = perform_mass_storage_reset(dev);
    if (r != 0) {
        assert(!"Could not reset the mass storage device");
    }

    /*
     * locate pipes for the transactions
     */

    init_pipe(dev, EP_BULK, EP_TYPE_IN, &pipe_in);
    assert(pipe_in.valid == 1);
    init_pipe(dev, EP_BULK, EP_TYPE_OUT, &pipe_out);
    assert(pipe_out.valid == 1);
    dprintf("\n DRIVER: pipes located and found OK");

    // This will cause ep to HALT so ....ignore for now
    // But this information is also available in INQ data, perhaps
    // not required to extract it this way.

    //XXX: LUN = get_max_LUN(dev);

    LUN = 0;

    // XXX: This might be different for different devices but
    // devices supporting > 1 LUNs are rare (Linux comment)
    // so for now assuming that we are working with LUN =0
    assert(LUN == 0);

    // Test command for PING state
    test_device_ready(dev, LUN);


    // INQ device about attributes
    // XXX: Actualy from here you will get more information
    // about the device attributes ...from experiments I know
    // that flash drive is 000 (SCSI SBC -2 complaint) but this
    // should be on demand basis.
    inquire_device(dev, LUN, &scsi_dev);

    // Read the capacity
    read_capacity(dev, &scsi_dev);

    // Print device information
    printf("\n USB_DRIVER: Device attributes ....");
    printf("\n             USB address :%u ", scsi_dev.usb_address);
    printf("\n             block size  :%u bytes", scsi_dev.block_size);
    printf("\n             last block  :%u", scsi_dev.block_last);
    printf("\n             capacity    :%lu bytes",
           (uint64_t) scsi_dev.block_size * scsi_dev.block_last);
    global_scsi_dev = &scsi_dev;

    dprintf("Initiating the long test read/write Animesh...\n");


    /*
     * Old test which is use to alter FAT32 flags to check write
     * */

#if 0
    io_buff = malloc_iobuff(scsi_dev.block_size, USB_NEAR_EHCI);


    char *buff = (char *)io_buff.va;
    uint32_t block = 0;

    while (block < scsi_dev.block_last) {

        dprintf("\n --------------SCAN TEST -------------------");
        dprintf("\n Reading block %u", block);
        read10(dev, block, 1, io_buff.pa, DONT_USE_CACHE);
        dprintf("\n Scanning ...");
        for (i = 0; i < 512 - 7; i++) {
            if (buff[i] == 'F') {
                dprintf(" \n Potential at %d", i);
                if (scan(buff, i)) {
                    dprintf(" Match found at %d", i);
                    //replace(buff, i);
                    //write10(dev, block, 1, io_buff.pa,
                    //DONT_USE_CACHE);
                    //dprintf(" Wrote block ...");
                    goto end;
                } else
                    dprintf(" ...no match");
            }
        }
        block++;
        clear_arr(io_buff.va, 4096);
        dprintf("..none found");
    }
#endif

    i = 0;
    uint64_t ehci_core = get_ehci_core_id();
    printf("\nUSB_DRIVER: Host controller driver is running on core %lx\n",
           ehci_core);


    io_buff = malloc_iobuff(DATA_SIZE * 512, USB_NEAR_SELF);
    dprintf("I/O buffer allocated successfully at va=[%p] pa=[%p]\n",
            io_buff.va, io_buff.pa);
    clear_arr(io_buff.va, DATA_SIZE * scsi_dev.block_size);
    dprintf("I/O buffer cleanned successfully \n");

    dprintf("1. RDTSC before read -- [%llu]\n",
            (long long unsigned int)rdtsc());
    printf("%llu\n", (long long unsigned int)rdtsc());

    read10(dev, 0, DATA_SIZE, io_buff.pa, DONT_USE_CACHE);

    printf("%llu\n", (long long unsigned int)rdtsc());

    goto end_driver_test;

    /* Code to test write behavior and sync */
    assert(scan((char *)io_buff.va, 82) == 1);
    dprintf("\n Changed ...stuff \n\n\n\n");
    alter((char *)io_buff.va, 82);
    print_uint8((uint8_t *) io_buff.va, 512);
    write10(dev, 63, 1, io_buff.pa, DONT_USE_CACHE);
    sync_cache_on_dev(dev, 63, 1);
    dprintf("\n Writing done ...\n");
    clear_arr(io_buff.va, DATA_SIZE * scsi_dev.block_size);
    read10(dev, 63, 1, io_buff.pa, DONT_USE_CACHE);

    goto end_driver_test;
    assert(scan(io_buff.va, 4067) == 1);
    dprintf("FS flag altereddd......\n");
    alter((char *)io_buff.va, 4067);
    dprintf("Going to write on devic ......\n");
    write10(dev, 56, 8, io_buff.pa, DONT_USE_CACHE);
    dprintf("DONE >>>>>> device unusable ......\n");


  end_driver_test:
    printf("\n Amount of data read/write [%lu] bytes",
           (unsigned long)DATA_SIZE * scsi_dev.block_size);
    print_memory_stats();
    printf("Freeing the allocated I/O buffer\n");
    free_iobuff(io_buff);
    print_memory_stats();
    printf("\n\nFURTHER DRIVER CODE IN DEVELOPMENT\n");
    printf("---- ++++ ---- ++++ -----\n");

    thread_exit();
    return 0;
}

void handle_device(uint8_t dev)
{
    dprintf("\n Handle device thread ...[%u]", dev);
    //FIXME: create a thread array
    assert(dev == 1);
    struct thread *dev_thread = NULL;
    dev_args = dev;
    dev_thread = thread_create(msd_init, 0);
    assert(dev_thread != NULL);
}
