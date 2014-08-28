/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef XEON_PHI_SERVICE_CLIENT_H_
#define XEON_PHI_SERVICE_CLIENT_H_

#define XEON_PHI_SERVICE_NAME "xeon_phi_svc"

typedef errval_t (*xphi_chan_open_t)(xphi_dom_id_t domain,
                                     uint64_t usrdata,
                                     struct capref msgframe,
                                     uint8_t type);

struct xeon_phi_callbacks
{
    xphi_chan_open_t open;
};

/**
 * \brief sets the callbacks for incoming messages
 *
 * \param cb    Xeon Phi callbacks
 */
void xeon_phi_client_set_callbacks(struct xeon_phi_callbacks *cb);

/**
 * \brief initializes the Xeon Phi client
 *
 * \param xid   Xeon Phi ID of the card to initialize
 */
errval_t xeon_phi_client_init(xphi_id_t xid);

/**
 * \brief spawns a new domain on the Xeon Phi or on the host
 *
 * \param xid       Xeon Phi ID to start the domain
 * \param core      Core to start
 * \param path      Program to spawn
 * \param argv      Program arguments
 * \param cap       Capability to pass
 * \param flags     spawn flags
 * \param domid     returns the domain id of the spawned domain
 *
 * \return SYS_ERR_OK on success
 *         errval on failure
 */
errval_t xeon_phi_client_spawn(xphi_id_t xid,
                               coreid_t core,
                               char *path,
                               char *argv[],
                               struct capref cap,
                               uint8_t flags,
                               xphi_dom_id_t *domid);

/**
 * \brief sends an channel open request to the domain
 *
 * \param xid       Xeon Phi ID
 * \param domid     Domain ID
 * \param usrdata   Supplied data for the other side
 * \param iface     Interface name of the domain
 * \param msgframe  Message frame
 * \param chantype  Type of the channel
 *
 * \returns SYS_ERR_OK on success
 *          XEON_PHI_ERR_CLIENT_OPEN_REJCT if the client rejected
 *          errval on error
 *
 * The function expectes to be either the domain or the interface specified.
 * If both are non-null then the domain ID is taken
 */
errval_t xeon_phi_client_chan_open(xphi_id_t xid,
                                   xphi_dom_id_t domid,
                                   uint64_t usrdata,
                                   struct capref msgframe,
                                   xphi_chan_type_t chantype);

/**
 * \brief sends a kill request to the Xeon Phi
 *
 * \param xid   Xeon Phi ID
 * \param domid ID of the domain to kill
 *
 * \returns SYS_ERR_OK on success,
 *          XEON_PHI_ERR_CLIENT_DOMAIN_VOID,
 *          errval on error
 */
errval_t xeon_phi_client_kill(xphi_id_t xid,
                              xphi_dom_id_t domid);

#endif // XEON_PHI_SERVICE_CLIENT_H_
