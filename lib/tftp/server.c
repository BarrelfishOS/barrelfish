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

#include <stdlib.h>
#include <stdio.h>

#include <barrelfish/barrelfish.h>

#include <tftp/tftp.h>
#include "tftp_internal.h"


/**
 * \brief starts the tftp server on this machine on a given port
 *
 * \param ip    ip address to be used
 * \param port  port to be used
 * \param cb    callback function called when clients connect
 *
 * \return  SYS_ERR_OK on success
 *          TFTP_ERR_* on failure
 */
errval_t tftp_server_accept(char *ip, uint16_t port, tfpt_server_cb_f_t cb)
{
    USER_PANIC("NYI");
    return SYS_ERR_OK;
}


/**
 * \brief terminates the tftp server connection
 *
 * \return  SYS_ERR_OK on success
 *          TFTP_ERR_* on failure
 */
errval_t tftp_server_terminate(void)
{
    USER_PANIC("NYI");
    return SYS_ERR_OK;
}
