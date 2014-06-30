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
#define XEON_PHI_MSG_SIZE 64
#define XEON_PHI_MSG_CHAN_SIZE  (XEON_PHI_MSG_INIT_SIZE/2)
#define XEON_PHI_MSG_CHAN_SLOTS (XEON_PHI_MSG_INIT_SIZE/2/XEON_PHI_MSG_SIZE)

#define XEON_PHI_MSG_CMD_OPEN      0x01
#define XEON_PHI_MSG_CMD_BOOTSTRAP 0x02
#define XEON_PHI_MSG_CMD_SPAWN     0x03
#define XEON_PHI_MSG_CMD_ERROR     0x04
#define XEON_PHI_MSG_CMD_READY     0x05
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
    uint32_t valid;  ///< indicates that the message is fully written
    uint16_t size;  ///< the length of the message in bytes (max 58)
    struct
    {
        uint16_t cmd :4;  ///< the type of the payload
        uint16_t _res : 4;
        uint16_t xphi_id : 8;
    } flags;        ///< flags for this message
};

struct xeon_phi_msg_data
{
    struct xeon_phi_msg_hdr ctrl;
    union
    {
        struct xeon_phi_msg_bootstrap bootstrap;
        struct xeon_phi_msg_ready     ready;
        struct xeon_phi_msg_open open;
        struct xeon_phi_msg_kill kill;
        struct xeon_phi_msg_spawn spawn;
        struct xeon_phi_msg_err err;
        uint64_t raw[7];
    } data;
};

STATIC_ASSERT_SIZEOF(struct xeon_phi_msg_data, XEON_PHI_MSG_SIZE);

/**
 * represents the information needed to create a new messaging channel
 * between the card and the host
 */
struct xeon_phi_msg_chan
{
    struct xeon_phi_msg_data data[XEON_PHI_MSG_CHAN_SLOTS];
};
STATIC_ASSERT_SIZEOF(struct xeon_phi_msg_chan, XEON_PHI_MSG_CHAN_SIZE);


struct xeon_phi_msg
{
    struct xeon_phi_msg_chan h2c;
    struct xeon_phi_msg_chan c2h;
};

STATIC_ASSERT_SIZEOF(struct xeon_phi_msg, XEON_PHI_MSG_INIT_SIZE);

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
    uint8_t is_client;
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
 * \brief initializes the messaging boostrap infrastructure between the
 *        two Xeon Phi cards
 *
 * \param phi the xeon phi to initialize the basic messaging bootstrap
 *
 * \return SYS_ERR_OK on success
 */
errval_t messaging_init_xphi(uint8_t xphi,
                             struct xeon_phi *phi,
                             struct capref frame,
                             uint8_t is_client);

/**
 * \brief polls the shared messaging frame for a new message
 *
 * \param phi the xeon phi to poll
 *
 * \return SYS_ERR_OK on success
 */
errval_t messaging_poll(struct xeon_phi *phi);

/**
 * \brief sends an bootstrap message to the Xeon Phi driver in order to
 *        establish a new Intra-Phi communication channel
 *
 * \param base      physical base address of the messaging frame
 * \param bits      size of the messaging frame in bits
 * \param is_client flag indicating if this side is the client or the host
 * \param xphi_id   the id of the Xeon Phi we want to establish the connection to
 *
 * \return SYS_ERR_OK on success
 */
errval_t messaging_send_bootstrap(lpaddr_t base,
                                  lpaddr_t offset,
                                  uint8_t bits,
                                  uint8_t xphi_id,
                                  uint8_t is_client);

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
 * \brief registers a new frame for the shared messaging channel to be used
 *        for communication purposes with a specific Xeon Phi chard
 *
 * \param xphi  the id of the Xeon Phi Card to send the open command to
 * \param frame capability representing the frame to be used
 * \param type  type identifier of the channel
 * \param iface the name of the exported interface
 *
 * \returns SYS_ERR_OK on success
 */
errval_t messaging_send_open_to_xphi(uint8_t xphi,
                                     struct capref frame,
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
 * \brief sends a spawn command over the Xeon Phi channel to
 *        a specific card.
 *
 * \param core id of the core on which to spawn the program
 * \param name the name of the program to spawn
 *
 * \returns SYS_ERR_OK on success
 */
errval_t messaging_send_spawn_to_xphi(uint8_t xphi,
                                      coreid_t core,
                                      char *name);

/**
 * \brief sends a ready message to the other Xeon Phi card indicating
 *        that the card intra Phi bootstrap frame is ready
 *
 * \param xphi the id of the other card
 */
errval_t messaging_send_ready(uint8_t xphi);

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
