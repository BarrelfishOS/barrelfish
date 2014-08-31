/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_XOMP_WORKER_H_
#define LIB_XOMP_WORKER_H_

/// flag indicating that the worker is acting as a gatway
#define XOMP_WID_GATEWAY_FLAG (1UL << 63)

/**
 * \brief parses the command line arguments to extract
 *
 * \param argc  argument count
 * \param argv  argument values
 * \param wid   returns the XOMP worker ID
 *
 * \returns SYS_ERR_OK iff the command line arguments were parsed succecssfully
 *          XOMP_ERR_INVALID_WORKER_ARGS if there were no XOMP worker argumetnts
 *          errval on error
 *
 */
errval_t xomp_worker_parse_cmdline(uint8_t argc,
                                   char *argv[],
                                   xomp_wid_t *wid);

/**
 * \brief initializes the XOMP worker library
 *
 * \param wid   Xomp worker id
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t xomp_worker_init(xomp_wid_t wid);

#endif // LIB_XOMP_WORKER_H_
