/**
 * \file
 * \brief Network client of the block service
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>

#include "network_common.h"
#include "network_client.h"

/**
 * \brief connects to a network block service
 *
 * TODO: Specification which block service to connect to
 */
errval_t block_net_connect(struct block_net_server *server)
{
    // initialize network connection to the block server

    // obtain basic information about the block server

    // potential initialize local cache

    // initialize the bulk channel to the block server
    assert( !"NYI: block_net_connect");
    return SYS_ERR_OK;
}

/**
 * \brief disconnects form the network block service
 */
errval_t block_net_disconnect(struct block_net_server *server)
{
    // tear down the bulk channel

    // tear down the network connection

    // free allocated cache
    assert( !"NYI: block_net_connect");
    return SYS_ERR_OK;
}

/**
 * \brief issues a new retrieval request to the network block server
 *
 * \param block_start   the id of the first block
 * \param count         the number of blocks to read
 * \param ret_data      the data returned
 *
 * TODO: appropriate data type for ret_data
 */
errval_t block_net_read(struct block_net_server *server, size_t block_start,
                        size_t count, void *ret_data)
{
    // issue a new read request

    // wait until the data arrives

    // return data and status code
    assert( !"NYI: block_net_connect");
    return SYS_ERR_OK;
}

/**
 * \brief forwards the write request to the network block server
 *
 * \param block_start   the id of the first block
 * \param count         the numbers of blocks to write
 * \param data          the data to write
 *
 * TODO: appropriate data type for ret_data
 */
errval_t block_net_write(struct block_net_server *server, size_t block_start,
                         size_t count, void *data)
{
    // issue a new write request

    // send data over the bulk channel

    // wait until completed

    // return status code
    assert( !"NYI: block_net_connect");
    return SYS_ERR_OK;
}
