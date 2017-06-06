/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


#include <inttypes.h>
#include <devif/backends/blk/ahci_devq.h>
#include "ahcid.h"
#include "test.h"

void* dq = NULL;
struct waitset disk_ws;

static struct ahci_disk* ad;
static volatile bool driver_initialized = false;
static struct device_mem hbabar;
static struct waitset_chanstate *chan = NULL;


struct device_id {
    uint16_t vendor;
    uint16_t device;
};

static void interrupt_handler(void *arg)
{
    ahci_interrupt_handler(dq);

#ifdef DISABLE_INTERRUPTS
    assert(chan != NULL);
    assert(dq != NULL);
    errval_t err = waitset_chan_register(&disk_ws, chan, MKCLOSURE(interrupt_handler, dq));
    if (err_is_fail(err) && err_no(err) == LIB_ERR_CHAN_ALREADY_REGISTERED) {
        printf("Got actual interrupt?\n");
    }
    else if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Can't register our dummy channel.");
    }
    err = waitset_chan_trigger(chan);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "trigger failed.");
    }
#endif
    
}

static void do_ahci_init(void *arg, struct device_mem* bar_info, int nr_allocated_bars)
{
    // Although the AHCI specification requires the AHCI memory region to be in
    // BAR 5 (BAR 0 to 4 are used for legacy IDE mode) the QEMU AHCI emulation
    // incorrectly uses BAR 0.  Because of this, ahcid consults both BAR 0 and
    // BAR 5 to find the HBA's memory mapped I/O region.
    // Since two BARs between 0-5 are I/O ports they are not passed to use by PCI.

    if (nr_allocated_bars == 1) {
        memcpy(&hbabar, &bar_info[0], sizeof(struct device_mem));
    } else if (nr_allocated_bars == 3) {
        memcpy(&hbabar, &bar_info[2], sizeof(struct device_mem));
    } else {
        printf("Strange device... not supported\n");
        abort();
    }

    errval_t err = blk_ahci_init(&hbabar, &ad);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "AHCI HBA init failed.");
    }

    struct ahci_queue* q;
    err = ahci_create(&q, ad, 0);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "ahci_queue create failed.");
    }

    dq = (struct devq*) q;

#ifdef DISABLE_INTERRUPTS
    waitset_init(&disk_ws);

    // Hack: Why don't interrupts work?
    chan = malloc(sizeof(struct waitset_chanstate));
    waitset_chanstate_init(chan, CHANTYPE_AHCI);

    err = waitset_chan_register(&disk_ws, chan, MKCLOSURE(interrupt_handler, dq));
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "waitset_chan_regster failed.");
    }
    err = waitset_chan_trigger(chan);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "trigger failed.");
    }
#endif

    driver_initialized = true;
}

static void ahci_reregister_handler(void *arg)
{
    errval_t err;
    struct device_id *dev_id = arg;
    err = pci_reregister_irq_for_device(PCI_CLASS_MASS_STORAGE, PCI_SUB_SATA,
            PCI_DONT_CARE, dev_id->vendor, dev_id->device, PCI_DONT_CARE, PCI_DONT_CARE,
            PCI_DONT_CARE, interrupt_handler, NULL,
            ahci_reregister_handler, dev_id);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "pci_reregister_irq_for_device");
    }

    return;
}

int main(int argc, char **argv)
{
    int r;
    r = skb_client_connect();
    assert(err_is_ok(r));
    r = pci_client_connect();
    assert(err_is_ok(r));

    if (argc >= 3) {
        printf("Got %s as vendor_id:device_id\n", argv[argc-1]);
        uint64_t vendor_id, device_id;
        vendor_id = strtol(argv[argc-1], NULL, 16);
        device_id = strtol(argv[argc-1]+5, NULL, 16);

        struct device_id *dev_id = malloc(sizeof(*dev_id));
        dev_id->vendor = vendor_id;
        dev_id->device = device_id;

        r = pci_register_driver_movable_irq(do_ahci_init, NULL, PCI_CLASS_MASS_STORAGE,
                PCI_SUB_SATA, PCI_DONT_CARE, vendor_id, device_id,
                PCI_DONT_CARE, PCI_DONT_CARE, PCI_DONT_CARE,
                interrupt_handler, NULL,
                ahci_reregister_handler, dev_id);
        if (err_is_fail(r)) {
            printf("Couldn't register device %04"PRIx64":%04"PRIx64": %s\n", vendor_id,
                    device_id, err_getstring(r));
            return 1;
        }
        printf("AHCID: registered device %04"PRIx64":%04"PRIx64"\n", vendor_id, device_id);
    }
    else {
        printf("usage: ahcid <vendor id>:<device id>\n");
        exit(1);
    }
    assert(driver_initialized);

#ifdef TESTING
    printf("Initialized driver, running tests:\n");
    if (argc < 4) {
        return 1;
    }
    if (strcmp(argv[2], "read|write") == 0) {
        test_runner(2, AhciTest_READ, AhciTest_WRITE);
    }
    if (strcmp(argv[2], "read") == 0) {
        test_runner(1, AhciTest_READ);
    }
    if (strcmp(argv[2], "write") == 0) {
        test_runner(1, AhciTest_WRITE);
    }
    if (strcmp(argv[2], "verify") == 0) {
        test_runner(1, AhciTest_VERIFY);
    }
#endif
}
