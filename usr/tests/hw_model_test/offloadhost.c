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

#include <if/xomp_defs.h>


#define ENABLE_NETWORKING 1


#define HLINE debug_printf("#######################################################\n");
#define hline debug_printf("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
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

struct xomp_binding *coprocessor = NULL;
static bool work_is_done = false;

static int32_t node_id_dma = -1;
static int32_t node_id_offload_core = -1;
static int32_t node_id_self = -1;
static int32_t node_id_ram = -1;
#if ENABLE_NETWORKING
static int32_t node_id_network = -1;
#endif

#define OFFLOAD_PATH "k1om/sbin/hwmodel/offload"
#define XEON_PHI_ID 0
#define XEON_PHI_CORE 1
#define DATA_SIZE (1UL << 30)
#define MSG_CHANNEL_SIZE (1UL << 20)
#define MSG_FRAME_SIZE (2 * MSG_CHANNEL_SIZE)

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

    #if ENABLE_NETWORKING
    node_id_network = driverkit_lookup_node_id("e1000");
    PRINTF("node id network is %d\n", node_id_offload_core);
    #endif

    node_id_ram = driverkit_lookup_node_id("numanode:0");
    PRINTF("node id ram is %d\n", node_id_offload_core);

    if (node_id_self == -1 || node_id_offload_core == -1
            || node_id_dma == -1 || node_id_ram == -1
    #if ENABLE_NETWORKING
        || node_id_network == -1
    #endif
    ) {
        USER_PANIC("Failed to obtain node id\n");
    }
}


static errval_t driverkit_frame_alloc(struct capref *dst,
                                      size_t bytes, int32_t dstnode,
                                      int32_t *nodes)
{
    return LIB_ERR_NOT_IMPLEMENTED;
}


static errval_t driverkit_vspace_map(int32_t nodeid, struct capref frame,
                                     vregion_flags_t flags, struct dmem *dmem)
{
    return LIB_ERR_NOT_IMPLEMENTED;
}


static void notify_rx(struct xomp_binding *_binding, uint64_t arg, errval_t err)
{
    PRINTF("Work is done callback.\n");

    work_is_done = true;
}


//static void connect_cb(void *st, struct xomp_binding *binding)
static void connect_cb(void *st, errval_t err, struct xomp_binding *_binding)
{
    PRINTF("Client connected.\n");
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to accept the connection");
    }
    coprocessor = _binding;
    coprocessor->rx_vtbl.done_notify = notify_rx;
}

static void message_passing_init(struct dmem *msgmem)
{
    errval_t err;

    struct xomp_frameinfo fi = {
        .sendbase = msgmem->devaddr,
        .inbuf = (void *)(msgmem->vbase + MSG_CHANNEL_SIZE),
        .inbufsize = MSG_CHANNEL_SIZE,
        .outbuf = (void *)(msgmem->vbase),
        .outbufsize = MSG_CHANNEL_SIZE,
    };
    err = xomp_accept(&fi, NULL, connect_cb, get_default_waitset(),
                      IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to accept the connection");
    }
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
    int32_t nodes_data[] = {
        node_id_dma, node_id_self,
    #if ENABLE_NETWORKING
        node_id_network,
    #endif
         0};
    err = driverkit_frame_alloc(&mem, DATA_SIZE, node_id_ram, nodes_data);
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

    /* TODO: allocate vspace in client */
    uint64_t clientva = (512UL << 30);

    hline

    PRINTF("Populating memory region with data\n");
    for(size_t i = 0; i < DATA_SIZE; i += sizeof(uint64_t)) {
        uint64_t *p = (uint64_t *)(dmem.vbase + i);
        *p = i;
    }

    hline

    PRINTF("Allocating memory for message passing\n");

    struct capref msgframemem;
    int32_t nodes_msg[] = {
            node_id_offload_core, node_id_self, 0
    };
    err = driverkit_frame_alloc(&msgframemem, MSG_FRAME_SIZE, node_id_ram, nodes_msg);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to allocate memory\n");
    }

    struct dmem msgmem;
    err = driverkit_vspace_map(node_id_self, msgframemem, VREGION_FLAGS_READ_WRITE,
                               &msgmem);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to map the memory\n");
    }

    message_passing_init(&msgmem);

    hline

    PRINTF("Allocating memory on the co-processor\n");

    struct capref offloadmem;
    int32_t nodes_offload[] = {
            node_id_offload_core, node_id_dma, 0
    };
    err = driverkit_frame_alloc(&offloadmem, DATA_SIZE, node_id_ram,  nodes_offload);
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
    err = xeon_phi_client_chan_open(XEON_PHI_ID, domid, clientva, mem, 1);
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

    while(coprocessor == NULL) {
        messages_wait_and_handle_next();
    }

    err = coprocessor->tx_vtbl.do_work(coprocessor, NOP_CONT, clientva, DATA_SIZE,
                                       0, 0);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to send the message");
    }


    PRINTF("Wait for co-processor to finish\n");

    while(!work_is_done) {
        messages_wait_and_handle_next();
    }

    hline

    PRINTF("Perform DMA from co-processor GDDR to system RAM\n");
    TODO("ask the DMA driver to do the copy\n");

    err = xeon_phi_client_dma_memcpy(dmem.devaddr, addr, DATA_SIZE);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to do the dma mem cpy\n");
    }
}

