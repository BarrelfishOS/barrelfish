/**
 * \file
 * \brief Driver for booting the Xeon Phi Coprocessor card on a Barrelfish Host
 */

/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef XEON_PHI_MESSAGING_H
#define XEON_PHI_MESSAGING_H

#include <xeon_phi/xeon_phi_messaging.h>

#define XEON_PHI_MSG_INIT_BITS 12
#define XEON_PHI_MSG_INIT_SIZE (1UL << XEON_PHI_MSG_INIT_BITS)
#define XEON_PHI_MSG_CHANS  (XEON_PHI_MSG_INIT_SIZE/2/64)


#define XEON_PHI_MSG_CMD_OPEN      0x01
#define XEON_PHI_MSG_CMD_SPAWN     0x03
#define XEON_PHI_MSG_CMD_ERROR     0x04
#define XEON_PHI_MSG_CMD_OTHER     0x0F

#define XEON_PHI_MSG_STATE_UNSEEN  0x01
#define XEON_PHI_MSG_STATE_SEEN    0x80

#define XEON_PHI_MSG_STATE_VALID    0xFF00FF00
#define XEON_PHI_MSG_STATE_CLEAR    0x0

/**
 * Representation of a message header.
 */
struct xeon_phi_msg_hdr
{
    uint32_t valid; ///< indicates that the message is fully written
    uint16_t size;  ///< the length of the message in bytes (max 58)
    struct {
        uint16_t cmd : 4;  ///< the type of the payload
        uint16_t _res : 12;
    } flags;        ///< flags for this message

};

struct xeon_phi_msg_data
{
    struct xeon_phi_msg_hdr ctrl;
    union {
        struct xeon_phi_msg_open open;
        struct xeon_phi_msg_kill kill;
        struct xeon_phi_msg_spawn spawn;
        struct xeon_phi_msg_err err;
        uint64_t raw[7];
    } data;

};

/**
 * represents the information needed to create a new messaging channel
 * between the card and the host
 */
struct xeon_phi_msg_chan
{
    struct xeon_phi_msg_data data[XEON_PHI_MSG_CHANS];
};

struct xeon_phi_msg
{
    struct xeon_phi_msg_chan h2c;
    struct xeon_phi_msg_chan c2h;
};



/**
 * represents the information for the messaging channel between the host and
 * the card.
 */
struct msg_info
{
    struct capref frame;
    lpaddr_t base;
    size_t size;
    struct xeon_phi_msg_data *out;
    struct xeon_phi_msg_data *in;
    struct xeon_phi_msg *meta;
};

/**
 * \brief initializes the messaging boostrap infrastructure between the
 *        host and the card
 *
 * \param phi   the xeon phi to initialize the basic messaging bootstrap
 * \param frame the frame to use, or nullcap if new alloc
 *
 * \return SYS_ERR_OK on success
 */
errval_t messaging_init(struct xeon_phi *phi,
                        struct capref frame);

/**
 * \brief polls the shared messaging frame for a new message
 *
 * \param phi the xeon phi to poll
 *
 * \return SYS_ERR_OK on success
 */
errval_t messaging_poll(struct xeon_phi *phi);

/**
 * \brief registers a new frame for the shared messaging channel to be used
 *        for communication purposes
 *
 * \param frame capability representing the frame to be used
 * \param type  type identifier of the channel
 * \param iface the name of the exported interface
 *
 * \returns SYS_ERR_OK on success
 */
errval_t messaging_send_open(struct capref frame,
                             uint8_t type,
                             char *iface);

/**
 * \brief sends a spawn command over the Xeon Phi channel
 *
 * \param core id of the core on which to spawn the program
 * \param name the name of the program to spawn
 *
 * \returns SYS_ERR_OK on success
 */
errval_t messaging_send_spawn(coreid_t core,
                              char *name);

/**
 * \brief sends a new message over the host-card channel
 *
 * \param phi    pointer to the xeon phi data structure
 * \param hdr    message header
 * \param data   pointer to the data to send
 * \param length amount of data to be sent
 *
 * \return
 */
errval_t messaging_send(struct xeon_phi *phi,
                        struct xeon_phi_msg_hdr hdr,
                        void *data);


#endif /* XEON_PHI_MESSAGING_H_ */
