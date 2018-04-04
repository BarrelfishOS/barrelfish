/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/ump_chan.h>
#include <bench/bench.h>
#include <barrelfish/sys_debug.h>

#include <xeon_phi/xeon_phi.h>
#include <xeon_phi/xeon_phi_client.h>

#include <dma/xeon_phi/xeon_phi_dma.h>
#include <dma/dma_request.h>
#include <dma/client/dma_client_device.h>
#include <dma/dma_manager_client.h>


#include <driverkit/iommu.h>

#include "benchmark.h"


#define HLINE debug_printf("========================================================\n");
#define hline debug_printf("--------------------------------------------------------\n");
#define PRINTF(x...) debug_printf("[HW Models] " x)
#define TODO(x...) debug_printf("[HW Models] TODO: " x)


/**
 * callback is executed when
 */
static errval_t msg_open_cb(xphi_dom_id_t domain,
                            uint64_t usrdata,
                            struct capref msgframe,
                            uint8_t type)
{
    PRINTF("msg_open callback\n");
    return SYS_ERR_OK;
}


static struct xeon_phi_callbacks callbacks = {
        .open = msg_open_cb
};


static int32_t driverkit_get_my_node_id(void)
{
    return -1;
}

static int32_t driverkit_lookup_node_id(const char *path)
{
    return -1;
}


static int32_t node_id_dma = -1;
static int32_t node_id_offload_core = -1;
static int32_t node_id_self = -1;
static int32_t node_id_network = -1;

#define OFFLOAD_PATH "k1om/sbin/hwmodel/offload"
#define XEON_PHI_ID 0
#define XEON_PHI_CORE 1
#define DATA_SIZE (1UL << 30)
#define MSG_FRAME_SIZE (2UL << 20)

static void get_node_ids(void)
{
    PRINTF("Obtaining ");
    node_id_self = driverkit_get_my_node_id();
    PRINTF("node id self is %d\n", node_id_self);

    node_id_dma = xeon_phi_client_get_node_id(XEON_PHI_ID, "dma");
    PRINTF("node id dma is %d\n", node_id_dma);

    node_id_offload_core = xeon_phi_client_get_node_id(XEON_PHI_ID,
                                                      "core: 1");
    PRINTF("node id offload is %d\n", node_id_offload_core);

    node_id_network = driverkit_lookup_node_id("e1000");
    PRINTF("node id network is %d\n", node_id_offload_core);

    if (node_id_self == -1 || node_id_offload_core == -1
            || node_id_dma == -1 || node_id_network == -1) {
        USER_PANIC("Failed to obtain node id\n");
    }
}


static errval_t driverkit_frame_alloc(struct capref *dst,
                                      size_t bytes,
                                      int32_t *nodes, size_t len)
{
    return LIB_ERR_NOT_IMPLEMENTED;
}


static errval_t driverkit_vspace_map(int32_t nodeid, struct capref frame,
                                     vregion_flags_t flags, struct dmem *dmem)
{
    return LIB_ERR_NOT_IMPLEMENTED;
}


int main(int argc,  char **argv)
{
    errval_t err;

    HLINE
    PRINTF("Offload Scenario started.\n");
    HLINE

    // initialize the xeon phi client
    err = xeon_phi_client_init(XEON_PHI_ID);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to initialize the xeon phi client");
    }

    // set the callbacks
    xeon_phi_client_set_callbacks(&callbacks);

    // obtain the node id
    get_node_ids();

    PRINTF("Allocating memory for data processing\n");

    struct capref mem;
    int32_t nodes_data[3] = {
            node_id_dma, node_id_self, 0
    };
    err = driverkit_frame_alloc(&mem, DATA_SIZE, nodes_data, 2);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to allocate memory\n");
    }

    hline

    PRINTF("Mapping area of memory.\n");
    struct dmem dmem;
    err = driverkit_vspace_map(node_id_self, mem, VREGION_FLAGS_READ_WRITE,
                               &dmem);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to map the memory\n");
    }

    hline

    PRINTF("Populating memory region with data\n");
    for(size_t i = 0; i < DATA_SIZE; i += sizeof(uint64_t)) {
        uint64_t *p = (uint64_t *)(dmem.vbase + i);
        *p = i;
    }

    hline

    PRINTF("Allocating memory for message passing\n");

    struct capref msgframemem;
    int32_t nodes_msg[3] = {
            node_id_offload_core, node_id_self, 0
    };
    err = driverkit_frame_alloc(&msgframemem, MSG_FRAME_SIZE, nodes_msg, 2);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to allocate memory\n");
    }

    struct dmem msgmem;
    err = driverkit_vspace_map(node_id_self, msgframemem, VREGION_FLAGS_READ_WRITE,
                               &msgmem);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to map the memory\n");
    }

    hline

    PRINTF("Allocating memory on the co-processor\n");

    struct capref offloadmem;
    int32_t nodes_offload[3] = {
            node_id_offload_core, node_id_dma, 0
    };
    err = driverkit_frame_alloc(&offloadmem, DATA_SIZE, nodes_offload, 2);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to allocate memory\n");
    }

    hline

    PRINTF("Prepare DMA from system RAM to co-processor GDDR\n");

    uint64_t addr;
    err = xeon_phi_client_dma_register(mem, &addr);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to register memory\n");
    }
    dmem.devaddr = addr;

    err = xeon_phi_client_dma_register(offloadmem, &addr);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to register memory\n");
    }

    hline

    PRINTF("Spawning process on co-processor");
    xphi_dom_id_t  domid;
    err = xeon_phi_client_spawn(XEON_PHI_ID, XEON_PHI_CORE, OFFLOAD_PATH, NULL,
                                NULL_CAP, 0, &domid);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to start the programm\n");
    }

    hline

    PRINTF("Adding message passing frame");
    err = xeon_phi_client_chan_open(XEON_PHI_ID, domid, 0, msgframemem, 0);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to set the channel");
    }

    hline

    PRINTF("Adding DMA mem");
    err = xeon_phi_client_chan_open(XEON_PHI_ID, domid, 0, mem, 1);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to set the channel");
    }

    hline

    PRINTF("Perform DMA from system RAM to co-processor GDDR\n");

    err = xeon_phi_client_dma_memcpy(addr, dmem.devaddr, DATA_SIZE);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to do the dma mem cpy\n");
    }

    hline

    PRINTF("Sending command to the co-processor\n");
    TODO("SEND COMMAND\n");


    PRINTF("Wait for co-processor to finish\n");
    TODO("wait for message\n");

    hline

    PRINTF("Perform DMA from co-processor GDDR to system RAM\n");
    TODO("ask the DMA driver to do the copy\n");

    err = xeon_phi_client_dma_memcpy(dmem.devaddr, addr, DATA_SIZE);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to do the dma mem cpy\n");
    }
}

