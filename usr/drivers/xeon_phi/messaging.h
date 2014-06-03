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

/**
 * represents the information for the messaging channel between the host and
 * the card.
 */
struct msg_info
{
    struct capref frame;
    lpaddr_t base;
    size_t size;
    void *addr;
};

#define XEON_PHI_MSG_CHAN 63
#define XEON_PHI_MSG_INIT_SIZE 4096

#define XEON_PHI_CHAN_TYPE_INVAL  0x00
#define XEON_PHI_CHAN_TYPE_VIRTIO 0x01
#define XEON_PHI_CHAN_TYPE_UMP    0x02
#define XEON_PHI_CHAN_TYPE_OTHER  0xFF

#define XEON_PHI_CHAN_OWNER_SELF  0x1
#define XEON_PHI_CHAN_OWNER_OTHER 0x0

#define XEON_PHI_CHAN_DIR_IN    0x1
#define XEON_PHI_CHAN_DIR_OUT   0x0

#define XEON_PHI_ADDR_LOCAL     0x01
#define XEON_PHI_ADDR_REMOTE    0x02


/**
 * represents the information needed to create a new messaging channel
 * between the card and the host
 */
struct xeon_phi_msg_chan
{
    lpaddr_t base;      ///< physical address of the buffer
    lvaddr_t vbase;     ///< virtual address of the messaging buffer (own VSPACE)
    uint64_t size;      ///< size of the virtual buffer in bytes
    uint64_t location;  ///< location information where the base refers to
    struct {
        uint64_t type   : 8; ///< the type of the channel
        uint64_t owner  : 1; ///< indicates if we are the owner
        uint64_t dir    : 1; ///< the direction of the channel
        uint64_t ack    : 1; ///< acknowledge of changes
        uint64_t _res   : 53;///< reserved
    } flags;            ///< channel flags
    uint64_t _pad[3];   ///< padding for full cache line
};

/**
 * meta information about the channel
 */
struct xeon_phi_msg_meta
{
    uint8_t meta_changed;
    uint8_t chan_changed[XEON_PHI_MSG_CHAN];
    struct xeon_phi_msg_chan chan[XEON_PHI_MSG_CHAN];
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
 * \param phi   the card to initialize the messaging for
 * \param frame capability representing the frame to be used
 */
errval_t messaging_channel_open(struct xeon_phi *phi,
                                struct capref frame);

/**
 * \brief closes an opened channel
 *
 * \param phi   the card to initialize the messaging for
 * \param frame capability representing the frame to be used
 */
errval_t messaging_channel_close(struct xeon_phi *phi,
                                 struct capref frame);

#endif /* XEON_PHI_MESSAGING_H_ */
