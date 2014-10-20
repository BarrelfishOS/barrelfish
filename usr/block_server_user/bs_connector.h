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



#ifndef BS_USER_CONNECTOR_H
#define BS_USER_CONNECTOR_H

#include <if/block_service_defs.h>

#include <bulk_transfer/bulk_transfer.h>
#include <bulk_transfer/bulk_sm.h>

#define BLOCK_SERVICE_NAME "blockservice"

struct bs_meta_data
{
    struct bulk_continuation cont;
    size_t block_id;
    uint32_t req_id;
};

/* debug */
#define BS_CONN_DEBUG(fmt, msg...) debug_printf("%s: "fmt"\n", __func__, msg);
//#define BS_CONN_DEBUG(fmt, msg...)

enum block_err_code {
    BLOCK_ERR_OK,
    BLOCK_ERR_NOT_CONNECTED,
    BLOCK_ERR_BAD_REQUEST,
    BLOCK_ERR_BAD_BLOCK_ID,
    BLOCK_ERR_NO_BUFS,
};


enum bs_conn_state {
    BS_CONN_CLOSED,
    BS_CONN_SERVICE_BOUND,
    BS_CONN_BULK_BINDING,
    BS_CONN_CONNECTED,
    BS_CONN_ERR
};



struct bs_connection {
    struct block_service_binding *service;
    enum bs_conn_state  state;
    struct bulk_channel tx_channel;
    struct bulk_channel rx_channel;
    struct bulk_sm_endpoint_descriptor tx_ep;
    struct bulk_sm_endpoint_descriptor rx_ep;
};

errval_t bs_service_connect(struct bs_connection *conn,
                            struct bulk_channel_callbacks *rx_cb,
                            struct bulk_channel_callbacks *tx_cb);

errval_t bs_service_read(struct bs_connection *conn,
                         uint32_t block_id,
                         uint32_t block_count,
                         struct bulk_continuation cont);

#endif /* BS_USER_CONNECTOR_H */
