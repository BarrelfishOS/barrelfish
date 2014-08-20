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

#ifndef BLOCK_NETWORK_CLIENT_H
#define BLOCK_NETWORK_CLIENT_H

struct block_net_server
{
    char *ip;
    uint16_t port;
/* other fields needed to represent the connection */
};

errval_t block_net_connect(struct block_net_server *server);

errval_t block_net_disconnect(struct block_net_server *server);

errval_t block_net_read(struct block_net_server *server, size_t block_start,
                        size_t count, void *ret_data);

errval_t block_net_write(struct block_net_server *server, size_t block_start,
                         size_t count, void *data);

struct block_net_server *block_net_server_lookup(size_t block_start);

#endif /* BLOCK_NETWORK_CLIENT_H */
