/**
 * \file
 * \brief Network server thread of the bulk server
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */



#ifndef BLOCK_LOCAL_SERVER_H
#define BLOCK_LOCAL_SERVER_H

#include <bulk_transfer/bulk_transfer.h>
#include <bulk_transfer/bulk_sm.h>

#include "network_common.h"

/* ---------------------------  Server State  ---------------------------- */
enum local_server_state {
    SERVICE_STATE_UNINITIALIZED = 0,
    SERVICE_STATE_EXPORTED = 1,
    SERVICE_STATE_RUNNING = 2,
    SERVICE_STATE_STOPPED = 3,
    SERVICE_STATE_FAILURE = 4,
};

#define SERVICE_FLAG_DEFAULT 0x0
#define SERVICE_FLAG_CLIENT  0x1


struct block_local_service {
    uint32_t                            bound;
    struct block_service_binding       *binding;
    struct bulk_channel                 tx_chan;
    struct bulk_channel                 rx_chan;
    struct bulk_sm_endpoint_descriptor  rx_ep;
    struct bulk_sm_endpoint_descriptor  tx_ep;

};



errval_t block_local_init(struct block_net_service *server, uint32_t flags);

errval_t block_local_start(void);

errval_t block_local_stop(void);

void block_local_data_ready(struct bulk_buffer *buffer,
                            void *meta);

errval_t block_local_return_buffer(struct bulk_channel *chan,
                                   struct bulk_buffer *buffer,
                                   void *meta);

errval_t block_local_release_copy(struct bulk_buffer *buffer);

errval_t block_local_send_status(enum block_net_msg_type req,
                                 uint32_t reqid,
                                 enum block_net_err stats);

#endif /* BLOCK_LOCAL_SERVER_H */
