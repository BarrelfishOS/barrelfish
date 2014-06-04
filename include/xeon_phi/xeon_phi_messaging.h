/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef XEON_PHI_MESSAGING_H_
#define XEON_PHI_MESSAGING_H_

/*
 * XXX: All the data structures specified in this must have a size of maximum
 *      58 bytes to fill up a 64byte cacheline with the 8byte header
 */

#define XEON_PHI_CHAN_TYPE_INVAL  0x00
#define XEON_PHI_CHAN_TYPE_VIRTIO 0x01
#define XEON_PHI_CHAN_TYPE_UMP    0x02
#define XEON_PHI_CHAN_TYPE_OTHER  0xFF

struct xeon_phi_msg_open
{
    lpaddr_t base;     ///< physical address of the messaging frame
    uint64_t size :60; ///< size in bytes of the frame
    uint64_t type :4;  ///< type of the channel
    char name[40];     ///< interface name (exported by the application)
};


struct xeon_phi_msg_spawn
{
    coreid_t core;  ///< core on which to spawn the new domain
    uint8_t length; ///< length of the name in the name field
    char name[54];  ///< name of the domain to spawn
};

struct xeon_phi_msg_kill
{
    domainid_t domain;   ///< the domain id to kill
};

struct xeon_phi_msg_err
{
    uint64_t err;   ///< error code returned
    uint8_t cmd;    ///< command that issued the error code
    uint8_t data[40];
};

#endif // XEON_PHI_MESSAGING_H_
