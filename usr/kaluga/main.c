/**
 * \file
 * \brief Device manager for Barrelfish.
 *
 * Interacts with the SKB / PCI to start cores, drivers etc.
 *
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <assert.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/cpu_arch.h>

#include <if/monitor_defs.h>

#include <dist2/dist2.h>
#include <barrelfish/nameservice_client.h>

#include <errors/errno.h>

#include "queue.h"

#define TRIGGER_ALWAYS (DIST_PERSIST | DIST_ON_SET | DIST_ON_DEL | DIST_ALWAYS_SET)
#define BSP_CORE_ID 0

// TODO: This is needed because of we have to send
// boot_initialize_request after all cores have booted
// This is a hack (see monitor boot.c:102)
static coreid_t cores_on_boot = 0;

static errval_t new_mon_msg(struct mon_msg_state** mms, send_handler_fn s)
{
    *mms = malloc(sizeof(struct mon_msg_state));
    if (*mms == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    (*mms)->send = s;
    (*mms)->next = NULL;

    return SYS_ERR_OK;
}


/*static void free_mon_send_state(void* arg) {
    debug_printf("free_mon_send_state");
    free(arg);
}*/

static void send_boot_initialize_request(struct monitor_binding* b,
        struct mon_msg_state* mm)
{
    errval_t err;

    err = b->tx_vtbl.boot_initialize_request(b,
            NOP_CONT);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            assert(!"false");
            enqueue_msg_state(b, mm);
            return;
        }
        USER_PANIC_ERR(err, "device_manager: sending %s failed!", __FUNCTION__);
    }
}


static void boot_core_reply(struct monitor_binding *st, errval_t msgerr)
{
    static coreid_t core_boot_replies = 0;
    if (err_is_fail(msgerr)) {
        USER_PANIC_ERR(msgerr, "msgerr in boot_core_reply, exiting\n");
    }

    if (++core_boot_replies == cores_on_boot) {
        struct monitor_binding *mb = get_monitor_binding();
        struct mon_msg_state *mms = NULL;
        errval_t err = new_mon_msg(&mms, send_boot_initialize_request);
        assert(err_is_ok(err));
        printf("before boot send...\n");
        mms->send(mb, mms);
    }
}

static void boot_initialize_reply(struct monitor_binding *st)
{
    // Ignore ?
}


static void send_boot_core_request(struct monitor_binding* b,
        struct mon_msg_state* mm)
{
    errval_t err;
    debug_printf("coreid: %d, arch_id: %lu, current cpu: %d, kernel_cmdline: %s\n",
            mm->core_id, mm->arch_id, CURRENT_CPU_TYPE, "/x86_64/sbin/cpu loglevel=4");


    err = b->tx_vtbl.boot_core_request(b, NOP_CONT,
            mm->core_id, mm->arch_id, CURRENT_CPU_TYPE, "/x86_64/sbin/cpu loglevel=4"); // TODO

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            assert(!"false");
            enqueue_msg_state(b, mm);
            return;
        }
        USER_PANIC_ERR(err, "device_manager: sending %s failed!", __FUNCTION__);
    }
}

static void cpu_change_event(dist2_mode_t mode, char* record, void* state)
{
    if (mode & DIST_ON_SET) {
        debug_printf("CPU found: %s\n", record);

        uint64_t core_id, arch_id = 0;
        errval_t err = dist_read(record, "_ { cpu_id: %d }", &core_id);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Cannot read record.");
            assert(!"Illformed core record received");
            goto out;
        }

        if (core_id != BSP_CORE_ID) { // Thats were we're running
            struct monitor_binding* mb = get_monitor_binding();

            struct mon_msg_state* mms = NULL;
            err = new_mon_msg(&mms, send_boot_core_request);
            assert(err_is_ok(err));

            mms->core_id = core_id;
            mms->arch_id = arch_id;
            mms->arch_id = 1; // TODO
            mms->send(mb, mms);
        }

    }
    if (mode & DIST_ON_DEL) {
        debug_printf("CPU removed: %s\n", record);
        assert(!"NYI");
    }

out:
    assert(!(mode & DIST_REMOVED));
    free(record);
}

static void watch_cores(void) {
    // Get current cores registered in system
    struct dist2_rpc_client* rpc = get_dist_rpc_client();
    dist2_trigger_t t = dist_mktrigger(SYS_ERR_OK, TRIGGER_ALWAYS,
            cpu_change_event, NULL);

    char* output;
    errval_t error_code;
    dist2_trigger_id_t tid;
    errval_t err = rpc->vtbl.get_names(rpc, "r'hw\\.cpu.*' { cpu_id: _ }", t,
            &output, &tid, &error_code);
    assert(err_is_ok(err));

    if (err_is_ok(error_code)) {
        char** names = NULL;
        size_t len = 0;
        err = dist_parse_names(output, &names, &len);
        cores_on_boot = (coreid_t) len;

        for (size_t i=0; i < cores_on_boot; i++) {
            char* core_record = NULL; // freed by core_change_event
            err = dist_get(&core_record, names[i]);

            switch (err_no(err)) {
            case SYS_ERR_OK:
                cpu_change_event(DIST_ON_SET, core_record, NULL);
                break;

            case DIST2_ERR_NO_RECORD:
                // Core was removed in the meantime - ignore
                assert(core_record == NULL);
                break;

            default:
                assert(core_record == NULL);
                DEBUG_ERR(err, "Unable to retrieve core record for %s", names[i]);
                break;
            }
        }
        dist_free_names(names, cores_on_boot);
    }
    else if (err_no(err) == DIST2_ERR_NO_RECORD) {
        // No cores found, do nothing for now
    }
    else {
        USER_PANIC_ERR(err, "Failed to check for CPU cores in SKB.");
    }
}

int main(void)
{
    // We need to run on core 0
    // (since we are responsible for booting all the other cores)
    assert(disp_get_core_id() == BSP_CORE_ID);
    printf("Device manager running.\n");

    struct monitor_binding* mb = get_monitor_binding();
    mb->rx_vtbl.boot_core_reply = boot_core_reply;
    mb->rx_vtbl.boot_initialize_reply = boot_initialize_reply;
    //mb->st = NULL;


    errval_t err;
    /*err = dist_init();
    assert(err_is_ok(err));*/


    iref_t iref;
    nameservice_blocking_lookup("pci_discovery_done", &iref);
    watch_cores();

    char** names = NULL;
    size_t len = 0;
    err = dist_get_names(&names, &len, "r'device.*' { class: _ }");

    for (size_t i=0; i<len; i++) {
        //debug_printf("found device: %s\n", names[i]);
        char* device_record = NULL;
        err = dist_get(&device_record, names[i]);
        assert(err_is_ok(err));

        uint64_t device_id, vendor_id;
        //debug_printf("device record is: %s\n", device_record);
        err = dist_read(device_record, "_ { device_id: %d, vendor: %d }", &device_id, &vendor_id);
        assert(err_is_ok(err));
        debug_printf("device_record: %s\n", device_record);
        char* db_record = NULL;
        err = dist_get(&db_record, "pci.db.devices.%lu.%lu", vendor_id, device_id);
        if (err_is_ok(err)) {
            char* vendor_name = NULL;
            char* device_name = NULL;
            //debug_printf("found db_record: %s\n", db_record);

            err = dist_read(db_record, "_ { device_name: %s, vendor_name: %s }", &device_name, &vendor_name);
            assert(err_is_ok(err));

            printf("** New device found: %s, %s\n", vendor_name, device_name);
            free(vendor_name);
            free(device_name);
        }
        else {
            printf("** New device not recognized: vendor_id:%lu, device_id:%lu\n", vendor_id, device_id);
        }

        free(device_record);
    }

    dist_free_names(names, len);

    messages_handler_loop();

    return EXIT_SUCCESS;
}
