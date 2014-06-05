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

#ifdef __k1om__
#define XEON_PHI_MESSAGING_NAME "xeon_phi_messaging_card"
#else
#define XEON_PHI_MESSAGING_NAME "xeon_phi_messaging_host"
#endif

#define XEON_PHI_MESSAGING_START_HANDLER 0x01
#define XEON_PHI_MESSAGING_NO_HANDLER   0x00

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

struct xeon_phi_msg_spawn_reply
{
    domainid_t id;
    coreid_t core;  ///< core on which to spawn the new domain
    char name[51];  ///< name of the domain to spawn
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


/**
 * this structure holds function pointers to functions which handle the special
 * type of messages over the xeon phi channel
 */
struct xeon_phi_messaging_cb
{
    errval_t (*open)(struct capref msgframe, uint8_t chantype);
    errval_t (*spawn)(coreid_t core, char *name);
};

/**
 * \brief initializes the messaging listener for messages over the Xeon Phi
 *        channel.
 *
 * \param fn    callback functions which are invoked when a message arrives
 *
 * \returns SYS_ERR_OK un success
 *          errorcode on failure
 */
errval_t xeon_phi_messaging_service_init(struct xeon_phi_messaging_cb *fn);

/**
 * \brief starts the service by registring the service with the name server
 *
 * \param start_hander flag to star the message handler loop
 *
 * \returns SYS_ERR_OK on success
 */
errval_t xeon_phi_messaging_service_start(uint8_t start_handler);

/**
 * \brief starts the service when running on the xeon phi driver
 *
 * \returns SYS_ERR_OK on success
 */
errval_t xeon_phi_messaging_service_start_phi(void);

#endif // XEON_PHI_MESSAGING_H_
