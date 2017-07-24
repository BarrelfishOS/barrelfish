/**
 * \file
 * \brief TFTP library
 */

/*
 * Copyright (c) 2015 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <net_sockets/net_sockets.h>

#include <tftp/tftp.h>
#include "tftp_internal.h"


/*
 * ------------------------------------------------------------------------------
 * Ack
 * ------------------------------------------------------------------------------
 */

errval_t tftp_send_ack(struct net_socket *socket, uint32_t blockno,
                       struct in_addr addr, uint16_t port,
                       void *payload)
{
    TFTP_DEBUG_PACKETS("sending ack(%u)\n", blockno);

    memset(payload, 0, sizeof(uint32_t) + sizeof(uint16_t));

    size_t length = set_opcode(payload, TFTP_OP_ACK);
    length += set_block_no(payload + length, blockno);

    errval_t err;
    err = net_send_to(socket, payload, length + 1, addr, port);
    assert(err_is_ok(err));

    return SYS_ERR_OK;
}
