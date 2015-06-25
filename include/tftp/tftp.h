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


#ifndef TFTP_H_
#define TFTP_H_ 1

/*
 * -------------------------------------------------------------------------------
 * TFTP Server
 * -------------------------------------------------------------------------------=
 */

///< server request callback function
typedef errval_t(*tfpt_server_cb_f_t)(void);

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
errval_t tftp_server_accept(char *ip, uint16_t port, tfpt_server_cb_f_t cb);

/**
 * \brief terminates the tftp server connection
 *
 * \return  SYS_ERR_OK on success
 *          TFTP_ERR_* on failure
 */
errval_t tftp_server_terminate(void);

/*
 * -------------------------------------------------------------------------------
 * TFTP Client
 * -------------------------------------------------------------------------------=
 */

/**
 * \brief   writes a file over tftp on the established connection
 *
 * \param name      name of the remote file to write
 * \param buf       buffer containing the data
 * \param buflen    length of the file to write
 *
 * \return  SYS_ERR_OK on success
 *          TFTP_ERR_* on failure
 */
errval_t tftp_client_write_file(char *name, void *buf, size_t buflen);


/**
 * \brief   reads a file over tftp on the established connection
 *
 * \param path      path of the file to readf rom
 * \param buf       buffer where to store the contents
 * \param buflen    maximum length of the buffer
 * \param ret_size  returns the number of bytes received
 *
 * \return  SYS_ERR_OK on success
 *          TFTP_ERR_* on failure
 */
errval_t tftp_client_read_file(char *path, void *buf, size_t buflen, size_t *ret_size);


/**
 * \brief attempts to initialize a new TFTP connection to a server
 *
 * \returns SYS_ERR_OK on success
 *          TFTP_ERR_* on failure
 */
errval_t tftp_client_connect(char *ip, uint16_t port);

/**
 * \brief terminates the connection to the tftp server
 *
 * \return SYS_ERR_OK on success
 *         TFTP_ERR_* on failure
 */
errval_t tftp_client_disconnect(void);


#endif
