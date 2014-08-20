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

#include <bulk_transfer/bulk_transfer.h>

#include <lwip/tcp.h>

#include "network_common.h"
#include "block_server.h"

/**
 *
 */
errval_t block_net_connect(struct block_net_service *server,
                           struct ip_addr *ip,
                           uint16_t port);

/**
 *
 */
errval_t block_net_disconnect(struct block_net_service *server);

/**
 *
 */
errval_t block_net_read(struct block_net_service *server,
                        size_t block_start,
                        size_t count,
                        uint32_t seqn,
                        struct bulk_continuation cont);

/**
 *
 */
errval_t block_net_write(struct block_net_service *server,
                         size_t count,
                         struct bulk_buffer **buf,
                         struct bs_meta_data *meta,
                         struct bulk_continuation cont);

errval_t block_net_pass(struct block_net_service *server,
                        size_t count,
                        struct bulk_buffer **buf,
                        struct bs_meta_data *meta,
                        struct bulk_continuation cont);

errval_t block_net_release(struct block_net_service *server,
                           size_t count,
                           struct bulk_buffer **buf,
                           struct bs_meta_data *meta,
                           struct bulk_continuation cont);

#endif /* BLOCK_NETWORK_CLIENT_H */
