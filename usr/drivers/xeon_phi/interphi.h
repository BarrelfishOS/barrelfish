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

#ifndef XEON_PHI_INTERPHI_H
#define XEON_PHI_INTERPHI_H

#define XEON_PHI_INTERPHI_CHANNEL_SIZE BASE_PAGE_SIZE
#define XEON_PHI_INTERPHI_FRAME_SIZE (2* XEON_PHI_INTERPHI_CHANNEL_SIZE)


/**
 * \brief waits for the client driver to connect
 *
 * \param phi Xeon Phi
 *
 * \return SYS_ERR_OK when then client driver successfully connected
 */
errval_t interphi_wait_for_client(struct xeon_phi *phi);

/*
 * ----------------------------------------------------------------------------
 * Initialization
 * ----------------------------------------------------------------------------
 */

/**
 * \brief initializes the messaging boostrap infrastructure between the
 *        two Xeon Phi cards
 *
 * \param phi the xeon phi to initialize the basic messaging bootstrap
 *
 * \return SYS_ERR_OK on success
 *         errval on failure
 */
errval_t interphi_init_xphi(uint8_t xphi,
                            struct xeon_phi *phi,
                            struct capref frame,
                            uint8_t is_client);

/**
 * \brief initializes the communication between the host and the card Xeon Phi
 *        drivers using a bootstraped flounder channel
 *
 * \param phi   Xeon Phi to initialize
 *
 * \return SYS_ERR_OK on success
 *         errval on failure
 */
errval_t interphi_init(struct xeon_phi *phi,
                       struct capref frame);

/*
 * ----------------------------------------------------------------------------
 * Message Sending
 * ----------------------------------------------------------------------------
 */

/**
 * \brief sends a bootstrap request to the Xeon Phi client driver
 *
 * \param phi        Xeon Phi
 * \param frame_base base address of the messaging frame
 * \param frame_bits size of the messaging frame in bits
 * \param offset     offset into the SMPT
 * \param xid        ID of the other Xeon Phi
 * \param is_client  flag indicating if this is the client of the connection
 *
 * \returns SYS_ERR_OK on success
 *          errval on faiure
 */
errval_t interphi_bootstrap(struct xeon_phi *phi,
                            lpaddr_t frame_base,
                            uint8_t frame_bits,
                            lpaddr_t offset,
                            uint8_t xid,
                            uint8_t is_client);

/**
 * \brief sends a spawn request to the Xeon Phi driver
 *
 * \param node      Xeon Phi Node
 * \param core      which core to spawn the domain on
 * \param cmdline   Commandline of the domain to spawn
 * \param domain    Domain identifier returned
 *
 * \returns SYS_ERR_OK on success
 *          errval on faiure
 */
errval_t interphi_spawn(struct xnode *node,
                        uint8_t core,
                        char *cmdline,
                        uint64_t *domain);


/**
 * \brief sends a spawn request to the Xeon Phi driver
 *
 * \param node      Xeon Phi Node
 * \param core      which core to spawn the domain on
 * \param cmdline   Commandline of the domain to spawn
 * \param cap       Cap to hand over to the domain at boot
 * \param domain    Domain identifier returned
 *
 * \returns SYS_ERR_OK on success
 *          errval on faiure
 */
errval_t interphi_spawn_with_cap(struct xnode *node,
                                 uint8_t core,
                                 char *cmdline,
                                 struct capref cap,
                                 uint64_t *domain);

/**
 * \brief sends a kill request for a domain
 *
 * \param node      Target Xeon Phi node
 * \param domain    Domain identifier
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t interphi_kill(struct xnode *node,
                       xphi_dom_id_t domain);

/**
 * \brief sends a channel open messages to another Xeon Phi driver
 *
 * \param node      Xeon Phi Node to send the message to
 * \param target    target domain id
 * \param iface     target domain interface name
 * \param source    source domain id
 * \param msgframe  capability of the messaging frame
 * \param type      Channel type
 *
 * \returns SYS_ERR_OK on success
 */
errval_t interphi_chan_open(struct xnode *node,
                            xphi_dom_id_t target,
                            char *iface,
                            xphi_dom_id_t source,
                            struct capref msgframe,
                            xphi_chan_type_t type);

#endif /* XEON_PHI_INTERPHI_H */
