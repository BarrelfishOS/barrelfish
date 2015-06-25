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
#include <lwip/udp.h>

#include <tftp/tftp.h>
#include "tftp_internal.h"


/*
 * ------------------------------------------------------------------------------
 * Ack
 * ------------------------------------------------------------------------------
 */

errval_t tftp_send_ack(struct udp_pcb *pcb, uint32_t blockno,
                       struct ip_addr *addr, u16_t port,
                       struct pbuf *p, void *payload)
{
    TFTP_DEBUG_PACKETS("sending ack(%u)\n", blockno);

    p->len = TFTP_MAX_MSGSIZE;
    p->tot_len = TFTP_MAX_MSGSIZE;
    p->payload = payload;

    memset(p->payload, 0, sizeof(uint32_t) + sizeof(uint16_t));

    size_t length = set_opcode(p->payload, TFTP_OP_ACK);
    length += set_block_no(p->payload + length, blockno);

    p->len = (uint16_t)length +1;
    p->tot_len = (uint16_t)length+1;

    int r = udp_sendto(pcb, p, addr, port);
    if (r != ERR_OK) {
        TFTP_DEBUG("send failed\n");
    }

    return SYS_ERR_OK;
}

