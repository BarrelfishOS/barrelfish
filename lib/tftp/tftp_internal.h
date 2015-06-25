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

#ifndef TFTP_INTERNAL_H_
#define TFTP_INTERNAL_H_ 1

/*
 * ------------------------------------------------------------------------------
 * General defines
 * ------------------------------------------------------------------------------
 */
#define TFTP_BLOCKSIZE      512
#define TFTP_MAX_MSGSIZE    (4 + TFTP_BLOCKSIZE)
#define TFTP_TIMEOUT        500


/*
 * ------------------------------------------------------------------------------
 * Debugging options
 * ------------------------------------------------------------------------------
 */
//#define TFTP_DEBUG(x...) debug_printf("[tftp] " x)
#define TFTP_DEBUG(x...)
//#define TFTP_DEBUG_PACKETS(x...) debug_printf("[tftp] " x)
#define TFTP_DEBUG_PACKETS(x...)

/*
 * ------------------------------------------------------------------------------
 * operation codes
 * ------------------------------------------------------------------------------
 */

///< TFTP Operation Codes
typedef enum tftp_op {
    TFTP_OP_INVALID   = 0,  ///< op code is invalid
    TFTP_OP_READ_REQ  = 1,  ///< read request
    TFTP_OP_WRITE_REQ = 2,  ///< write request
    TFTP_OP_DATA      = 3,  ///< data response
    TFTP_OP_ACK       = 4,  ///< ack response
    TFTP_OP_ERROR     = 5   ///< error response
} tpft_op_t;


/**
 * \brief writes the opcode into the buffer
 *
 * \param buf       buffer where to write the opcode to
 * \param opcode    opcode to be written
 *
 * \return number of bytes written
 */
static inline size_t set_opcode(void *buf, uint16_t opcode)
{
    uint8_t *p = buf;
    *p = ((opcode >> 8) & 0xff); p++;
    *p = (opcode & 0xff);

    return sizeof(uint16_t);
}

/**
 * \brief obtains the opcode from the buffer
 *
 * \param buf   buffer where to extract the op code
 *
 * \return tftp op code
 */
static inline tpft_op_t get_opcode(void *buf)
{
    uint8_t *p = buf;
    uint16_t opcode = (*p) << 8; p++;
    opcode |= *p;

    return (tpft_op_t)opcode;
}

/*
 * ------------------------------------------------------------------------------
 * Operation codes
 * ------------------------------------------------------------------------------
 */

///< TFTP state
typedef enum tpft_st {
    TFTP_ST_INVALID,        ///< state is invalid
    TFTP_ST_CLOSED,         ///< connection is closed
    TFTP_ST_IDLE,           ///< the client is connected and in the  idle state
    TFTP_ST_LAST_ACK_SENT,  ///< last ack has been sent
    TFTP_ST_LAST_DATA_SENT, ///< last data response sent
    TFTP_ST_ERROR,          ///< tftp request resulted in an error
    TFTP_ST_READ_REQ_SENT,  ///< a read request has been sent
    TFTP_ST_WRITE_REQ_SENT, ///< a write request has been sent
    TFTP_ST_DATA_SENT,      ///< data respnse is being sent
    TFTP_ST_ACK_SENT,       ///< ack has been sent
} tftp_st_t;


/*
 * ------------------------------------------------------------------------------
 * Transfer modes
 * ------------------------------------------------------------------------------
 */

///< operation mode of the TFTP connection
typedef enum tftp_mode {
    TFTP_MODE_INVALID,      ///< invalid operation mode
    TFTP_MODE_OCTET,        ///< use octet moded
    TFTP_MODE_NETASCII,     ///< use netsascii mode
    TFTP_MODE_MAIL,         ///< use mail mode
} tftp_mode_t;

/**
 * \brief sets the transfer mode in a buffer
 *
 * \param buf   buffer where to write the transfer mode to
 * \param mode  transfer mode to write
 *
 * \returns number of bytes written
 */
static inline size_t set_mode(void *buf, tftp_mode_t mode)
{
    switch(mode) {
        case TFTP_MODE_OCTET:
            return snprintf(buf, 6, "octet")+1;
            break;
        case TFTP_MODE_NETASCII:
            return snprintf(buf, 6, "netascii")+1;
            break;
        case TFTP_MODE_MAIL:
            return snprintf(buf, 5, "mail")+1;
            break;
        default:
            assert(!"this should not happen");
            return 0;
    }
}

/**
 * \brief parses the transfer mode from the buffer
 *
 * \param buf   buffer to extract the transfer mode from
 *
 * \return tftp transfer mode
 */
static inline tftp_mode_t get_mode(void *buf)
{
    if (strncmp(buf, "octet", 5) == 0) {
        return TFTP_MODE_OCTET;
    } else if (strncmp(buf, "netascii", 8) == 0) {
        return TFTP_MODE_NETASCII;
    } else if (strncmp(buf, "mail", 4) == 0) {
        return TFTP_MODE_MAIL;
    }

    return TFTP_MODE_INVALID;
}

/*
 * ------------------------------------------------------------------------------
 * Error
 * ------------------------------------------------------------------------------
 */

///< possible tftp errors in packages
typedef enum tftp_err {
    TFTP_ERR_NOT_DEFINED = 0,             ///< not defined
    TFTP_ERR_NOT_FOUND =  1,              ///< file not found
    TFTP_ERR_ACCESS_DENIED =  2,          ///< access denied
    TFTP_ERR_DISK_FULL = 3,               ///< disk is full
    TFTP_ERR_UNKNOWN_TID =   4,           ///< unkown transfer id
    TFTP_ERR_ILLEGAL_OP = 5,              ///< illegal operation
    TFTP_ERR_FILE_EXISTS =  6,            ///< destination file exist
    TFTP_ERR_NO_SUCH_USER =  7,           ///< no such user
    TFTP_ERR_INVALID_BUFFER = 0xFFFFFFFF, ///< invalid buffer
} tftp_err_t;

/**
 * \brief extracts the error code from an error package
 *
 * \param buf       buffer to extract the error code from
 * \param buflen    length of the buffer in bytes
 *
 * \returns tftp error code
 */
static inline tftp_err_t get_error(void *buf, size_t buflen) {
    if (buflen < 5) {
        return TFTP_ERR_INVALID_BUFFER;
    }
    uint8_t *p = buf;

    return (tftp_err_t)((p[2] << 8) + p[3]);
}

/**
 * \brief sets the error code in a buffer
 *
 * \param buf   buffer where to write the error code
 * \param error error code to write
 *
 * \returns number of bytes written
 */
static inline size_t set_error(void *buf, tftp_err_t error)
{
    uint8_t *p = buf;
    p[0] = 0;
    p[1] = 1;
    p[2] = ((error >> 8) & 0xff);
    p[3] = (error & 0xff);

    return 4;
}

/*
 * ------------------------------------------------------------------------------
 * Data packets
 * ------------------------------------------------------------------------------
 */

/**
 * \brief extracts the block number from a buffer
 *
 * \param buf       buffer where to extract the block number
 * \param buflen    length of the buffer
 *
 * \return block number or TFTP_BLOCKNO_INVALID
 */
static inline uint32_t get_block_no(void *buf, size_t buflen)
{
    if (buflen < 4) {
        return TFTP_ERR_INVALID_BUFFER;
    }

    uint8_t *data = buf;
    return (data[2] << 8) + data[3];
}

/**
 * \brief sets the block number in a buffer
 *
 * \param buf       buffer where to write the block number to
 * \param blockno   block number to write
 *
 * \return  number of bytes written
 */
static inline size_t set_block_no(void *buf, uint16_t blockno) {
    uint8_t *p = buf;
    *p = (uint8_t)((blockno >> 8) & 0xff); p++;
    *p = (uint8_t)(blockno & 0xff);
    return sizeof(uint16_t);
}

/*
 * ------------------------------------------------------------------------------
 * Sending generic messages
 * ------------------------------------------------------------------------------
 */
errval_t tftp_send_ack(struct udp_pcb *pcb, uint32_t blockno,
                       struct ip_addr *addr, u16_t port,
                       struct pbuf *p, void *payload);
#endif /* TFTP_INTERNAL_H_ */
