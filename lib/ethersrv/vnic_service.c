/**
 * \file
 * \brief File describing the virtual NIC's
 */

/*
 * Copyright (c) 2007-2011 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <stdio.h>
#include <string.h>
#include <if/ether_defs.h>
#include <if/v_nic_defs.h>
#include "ethersrv_debug.h"
#include "vnic_service.h"



#define MAX_VNICS       100
//static v_nic_unicid_t vnic_id_counter = 0;
static uint64_t vnic_id_counter = 0;

// FIXME: Why not make the following linked list??
static struct v_nic *vnic_list[MAX_VNICS];

static struct v_nic *fetch_valid_vnic(struct v_nic_binding *b)
{
    struct v_nic *vnic = (struct v_nic *)b->st;
    assert(vnic != NULL);
    assert(vnic->magic_cooki == VNIC_MAGIC_COOKI);
    return vnic;
} // end function: fetch_valid_vnic

// This function handles the error conditions when msg passing channel is closed
// This function is responsible for all cleanup
static void vnic_error_handler(struct v_nic_binding *b, errval_t err)
{
    // mark this vnic as unusable
    struct v_nic *vnic = fetch_valid_vnic(b);
    vnic->state = VNIC_ERROR;
    // FIXME: free up all associated memory
} // end function: vnic_error_handler


static errval_t send_create_vnic_response(struct q_entry e) {
    struct v_nic_binding *b = (struct v_nic_binding *)e.binding_ptr;
    struct v_nic *vnic = fetch_valid_vnic(b);

    if (b->can_send(b)) {
        return b->tx_vtbl.create_vnic_response(b,
                MKCONT(cont_queue_callback, vnic->q),
                e.plist[0], e.plist[1]);
             /* e.err,      e.vnic_id */
    } else {
        ETHERSRV_DEBUG("send_create_vnic_response: Flounder busy, will retry\n");
        return FLOUNDER_ERR_TX_BUSY;
    }
}


static void enqueue_create_vnic_response(struct v_nic_binding *b, errval_t err,
        uint64_t vnic_id)
{
    struct q_entry entry;
    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_create_vnic_response;
    entry.binding_ptr = (void *)b;
    struct v_nic *vnic = fetch_valid_vnic(b);
    entry.plist[0] = err;
    entry.plist[1] = vnic_id;
    /*   entry.plist[0], entry.plist[1]);
         entry.error,    entry.vnic_id */

    enqueue_cont_q(vnic->q, &entry);
} // end function: enqueue_create_vnic_response

static void create_VNIC(struct v_nic_binding *b, struct capref cap,
        v_nic_ubufring_size_t rx_slots, v_nic_ubufring_size_t tx_slots)
{
    struct v_nic *vnic = fetch_valid_vnic(b);
    errval_t err = SYS_ERR_OK;
    uint64_t vnic_id = vnic->id;

    if (vnic->state != VNIC_INITIALIZED){
        ETHERSRV_DEBUG("create_VNIC: VNIC is in wrong state %d\n", vnic->state);
        err = ETHERSRV_ERR_INVALID_STATE;
        enqueue_create_vnic_response(b, err, vnic_id);
        return;
    }

    lpaddr_t physical_addr;
    uint64_t bits;

    // Map the provided memory cap in address space
    struct frame_identity pa;
    err = invoke_frame_identify(cap, &pa);
    assert(err_is_ok(err));
    physical_addr = pa.base;
    bits = pa.bits;
    void *va = NULL;

    err = vspace_map_one_frame_attr(&va, (1L << bits), cap,
            VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);

    if (err_is_fail(err)){
        ETHERSRV_DEBUG("create_VNIC: Could not map given frame cap\n");
        err = ETHERSRV_ERR_FRAME_CAP_MAP;
        enqueue_create_vnic_response(b, err, vnic_id);
        return;
    }

    // TODO: Make the data-structures point to mapped memory
} // end function: create_VNIC



static struct v_nic_rx_vtbl rx_v_nic_vtbl = {
        .create_vnic_request = create_VNIC,
};


static errval_t connect_vnic_cb(void *st, struct v_nic_binding *b)
{
    assert(st != NULL);
    ETHERSRV_DEBUG("%s service got a connection\n", (char *)st);

    b->rx_vtbl = rx_v_nic_vtbl;
    b->error_handler = vnic_error_handler;

    errval_t err = SYS_ERR_OK;

    if(vnic_id_counter >= MAX_VNICS){
        ETHERSRV_DEBUG("connect_vnic_cb: ERROR: Too many VNICS\n");
        err = ETHERSRV_ERR_TOO_MANY_VNICS;
        return err;
    }

    // Create new empty vnic
    struct v_nic *vnic = (struct v_nic *)
                            malloc(sizeof(struct v_nic *));
    if(vnic == NULL) {
        err = ETHERSRV_ERR_NOT_ENOUGH_MEM;
        ETHERSRV_DEBUG("connect_vnic_cb: out of memory\n");
        return err;
    }

    // Create a queue for continuation management
    char name[64];
    sprintf(name, "vnic_c_%"PRIu64"", vnic_id_counter);
    vnic->q = create_cont_q(name);
    if (vnic->q == NULL) {
        err = ETHERSRV_ERR_NOT_ENOUGH_MEM;
        ETHERSRV_DEBUG("connect_vnic_cb: [%s] queue allocation failed\n",
                name);
        free(vnic);
        return err;
    }

    // Initialize the vnic
    memset(vnic, 0, sizeof(struct v_nic));
    vnic->magic_cooki = VNIC_MAGIC_COOKI;
    vnic->state = VNIC_INITIALIZED;
    vnic_list[vnic_id_counter] = vnic;
    vnic->id = vnic_id_counter;
    ++vnic_id_counter;
    b->st = vnic;

    ETHERSRV_DEBUG("connect_vnic_cb: app connected for vid %"PRIu64"\n",
            vnic->id);
    return err;
} // end function: connect_vnic_cb


static void export_vnic_cb(void *st, errval_t err, iref_t iref)
{
    char *service_name = st;

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export_vnic_cb returned with error!");
    }

    assert(st != NULL);

    // register this iref with the name service
    err = nameservice_register(service_name, iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export_vnic_cb: nameservice_register failed"
                " to register name [%s]!", service_name);
    }
    ETHERSRV_DEBUG("service [%s] exported at iref %u\n", service_name, iref);
} // end function: export_vnic_cb


void VNIC_service_init(char *service_name)
{
    ETHERSRV_DEBUG("in the VNIC_service_init\n");
    assert(service_name != NULL);

    vnic_id_counter = 0;
    memset(vnic_list, 0, sizeof(vnic_list));

    errval_t err = v_nic_export(service_name, export_vnic_cb, connect_vnic_cb,
            get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "v_nic_export failed!");
    }
} // VNIC_service_init


