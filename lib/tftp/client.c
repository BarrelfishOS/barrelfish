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
#include <barrelfish/waitset.h>
#include <barrelfish/nameservice_client.h>

#include <net_sockets/net_sockets.h>
#include <arpa/inet.h>

#include <tftp/tftp.h>

#include "tftp_internal.h"


// error definitions
#define TFTP_ERR_BUSY 1
#define TFTP_ERR_DISCONNECTED 1
#define TFTP_ERR_NOT_FOUND 1
#define TFTP_ERR_ACCESS_DENIED 1
#define TFTP_ERR_FILE_EXISTS 1



///< the TFTP client
struct tftp_client
{
    /* client state */
    tftp_st_t state;

    /* connection information */
    struct in_addr server_ip;
    uint16_t server_port;
    tftp_mode_t mode;

    /* request information */
    uint32_t block;
    size_t bytes;
    void *buf;
    size_t buflen;

    /* connection information */
    struct net_socket *pcb;
    void *ppayload;
};


struct tftp_client tftp_client;


static errval_t tftp_client_send_data(struct net_socket *socket, uint32_t blockno, void *buf,
                                      uint32_t length, struct in_addr addr, uint16_t port)
{
    void *payload = tftp_client.ppayload;
    errval_t err;

    size_t offset = set_opcode(payload, TFTP_OP_DATA);
    offset += set_block_no(payload + offset, blockno);
    if (length > TFTP_BLOCKSIZE) {
        length = TFTP_BLOCKSIZE;
    }

    memcpy(payload + offset, buf, length);
    err = net_send_to(socket, payload, length + offset, addr, port);
    assert(err_is_ok(err));
    return SYS_ERR_OK;
}


/*
 * ------------------------------------------------------------------------------
 * Recv Handlers
 * ------------------------------------------------------------------------------
 */

static void tftp_client_handle_write(struct net_socket *socket, void *data,
    size_t size, struct in_addr ip_address, uint16_t port)
{
    USER_PANIC("NYI");
    tpft_op_t op = get_opcode(data);
    uint32_t blockno;
    switch(op) {
        case TFTP_OP_ACK :
            blockno = get_block_no(data, size);
            if (blockno == TFTP_ERR_INVALID_BUFFER) {
                TFTP_DEBUG("failed to decode block number in data packet\n");
                break;
            }

            if (blockno == tftp_client.block) {
                if (tftp_client.state == TFTP_ST_LAST_DATA_SENT) {
                    tftp_client.state = TFTP_ST_CLOSED;
                    break;
                }

                uint32_t offset = TFTP_BLOCKSIZE * blockno;
                uint32_t length = TFTP_BLOCKSIZE;
                if (tftp_client.buflen - offset < TFTP_BLOCKSIZE) {
                    length = tftp_client.buflen - offset;
                    tftp_client.state = TFTP_ST_LAST_DATA_SENT;
                }

                tftp_client.block++;

                tftp_client_send_data(socket, tftp_client.block, tftp_client.buf + offset, length,
                                      ip_address, port);
                tftp_client.state = TFTP_ST_DATA_SENT;
            } else  {
                TFTP_DEBUG("got double packet: %u\n", blockno);
            }

            break;
        case TFTP_OP_ERROR :
            TFTP_DEBUG("got a error packet\n");
            break;
        default:
            tftp_client.state = TFTP_ST_ERROR;
            break;
    }
}

static void tftp_client_handle_read(struct net_socket *socket, void *data,
    size_t size, struct in_addr ip_address, uint16_t port)
{
    tpft_op_t op = get_opcode(data);
    uint32_t blockno;
    switch(op) {
        case TFTP_OP_DATA :
            blockno = get_block_no(data, size);
            if (blockno == TFTP_ERR_INVALID_BUFFER) {
                TFTP_DEBUG("failed to decode block number in data packet\n");
                break;
            }

            if (blockno == tftp_client.block) {
                if (size < 5) {
                    TFTP_DEBUG("too small pbuf lenth\n");
                }

                void *buf = data + 4;
                size_t length = size - 4;
                TFTP_DEBUG_PACKETS("received block %u of size %lu bytes\n", blockno, length);

                if (tftp_client.buflen < tftp_client.bytes + length) {
                    TFTP_DEBUG("too less bufferspace available\n");
                    length = tftp_client.buflen - tftp_client.bytes;
                }
                memcpy(tftp_client.buf + tftp_client.bytes, buf, length);

                int r = tftp_send_ack(socket, blockno, ip_address, port,
                                      tftp_client.ppayload);
                if (r != SYS_ERR_OK) {
                    tftp_client.state = TFTP_ST_ERROR;
                    break;
                }
                tftp_client.state = TFTP_ST_ACK_SENT;
                tftp_client.block++;
                tftp_client.bytes += length;
                if (length < TFTP_BLOCKSIZE) {
                    TFTP_DEBUG("setting the last ack state\n");
                    tftp_client.state = TFTP_ST_LAST_ACK_SENT;
                }
            } else  {
                TFTP_DEBUG("got double packet: %u\n", blockno);
                int r = tftp_send_ack(socket, blockno, ip_address, port,
                                      tftp_client.ppayload);
                if (r != SYS_ERR_OK) {
                    tftp_client.state = TFTP_ST_ERROR;
                    break;
                }
                tftp_client.state = TFTP_ST_ACK_SENT;
            }

            break;
        case TFTP_OP_ERROR :
            TFTP_DEBUG("got a error packet\n");
            get_error(data, size);
            tftp_client.state = TFTP_ST_ERROR;
            break;
        default:
            tftp_client.state = TFTP_ST_ERROR;
            TFTP_DEBUG("unexpected packet\n");
            break;
    }
}


static void tftp_client_recv_handler(void *user_state, struct net_socket *socket,
    void *data, size_t size, struct in_addr ip_address, uint16_t port)
{
    switch(tftp_client.state) {
        case TFTP_ST_WRITE_REQ_SENT:
        case TFTP_ST_DATA_SENT :
        case TFTP_ST_LAST_DATA_SENT :
            tftp_client_handle_write(socket, data, size, ip_address, port);
            break;
        case TFTP_ST_READ_REQ_SENT :
        case TFTP_ST_ACK_SENT :
            tftp_client_handle_read(socket, data, size, ip_address, port);
            break;
        default:
            TFTP_DEBUG("unexpected state: %u\n", tftp_client.state);
            break;
    }
}

static void new_request(char *path, tpft_op_t opcode)
{
    size_t path_length = strlen(path);
    assert(strlen(path) + 14 < TFTP_MAX_MSGSIZE);

    void *payload = tftp_client.ppayload;

    memset(payload, 0, path_length + 16);

    size_t length = set_opcode(payload, opcode);

    length += snprintf(payload + length, path_length + 1, "%s", path) + 1;
    length += set_mode(payload + length, tftp_client.mode);

    TFTP_DEBUG("sending udp payload of %lu bytes\n", length);

    errval_t err;
    err = net_send_to(tftp_client.pcb, payload, length, tftp_client.server_ip, tftp_client.server_port);
    if (err != SYS_ERR_OK) {
        TFTP_DEBUG("send failed\n");
    }
}


errval_t tftp_client_write_file(char *name, void *buf, size_t buflen)
{
    if (tftp_client.state < TFTP_ST_IDLE) {
        TFTP_DEBUG("attempt to read file with no connection");
        return TFTP_ERR_DISCONNECTED;
    }

    if (tftp_client.state > TFTP_ST_IDLE) {
        return TFTP_ERR_BUSY;
    }

    tftp_client.buf = buf;
    tftp_client.buflen = buflen;
    tftp_client.block = 1;
    tftp_client.state = TFTP_ST_WRITE_REQ_SENT;
    tftp_client.bytes = 0;

    return SYS_ERR_OK;
}

errval_t tftp_client_read_file(char *path, void *buf, size_t buflen, size_t *ret_size)
{
    if (tftp_client.state < TFTP_ST_IDLE) {
        TFTP_DEBUG("attempt to read file with no connection");
        return TFTP_ERR_DISCONNECTED;
    }

    if (tftp_client.state > TFTP_ST_IDLE) {
        return TFTP_ERR_BUSY;
    }

    tftp_client.buf = buf;
    tftp_client.buflen = buflen;
    tftp_client.block = 1;
    tftp_client.state = TFTP_ST_READ_REQ_SENT;
    tftp_client.bytes = 0;

    assert(tftp_client.pcb);

    TFTP_DEBUG("read request of file %s\n", path);

    new_request(path, TFTP_OP_READ_REQ);

    while(tftp_client.state > TFTP_ST_ERROR) {
        event_dispatch(get_default_waitset());
    }

    TFTP_DEBUG("tftp read file done.\n");

    if (ret_size) {
        *ret_size = tftp_client.bytes;
    }

    if (tftp_client.state == TFTP_ST_ERROR) {
        tftp_client.state = TFTP_ST_IDLE;
        return -1;
    }

    tftp_client.state = TFTP_ST_IDLE;

    return SYS_ERR_OK;
}



/**
 * \brief attempts to initialize a new TFTP connection to a server
 *
 * \returns SYS_ERR_OK on success
 *          TFTP_ERR_* on failure
 */
errval_t tftp_client_connect(char *ip, uint16_t port)
{
    switch(tftp_client.state) {
        case TFTP_ST_INVALID :
            net_sockets_init();
            tftp_client.pcb = net_udp_socket();
            TFTP_DEBUG("new connection from uninitialized state\n");
            break;
        case TFTP_ST_CLOSED :
            TFTP_DEBUG("new connection from closed state\n");
            tftp_client.pcb = net_udp_socket();
            break;
        default:
            TFTP_DEBUG("connection already established, cannot connect\n");
            return TFTP_ERR_BUSY;
    }

    if (tftp_client.pcb == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    tftp_client.server_port = port;

    int ret = inet_aton(ip, &tftp_client.server_ip);
    if (ret == 0) {
        TFTP_DEBUG("Invalid IP addr: %s\n", ip);
        return 1;
    }

    TFTP_DEBUG("connecting to %s:%" PRIu16 "\n", ip, port);

    errval_t r;
    r = net_bind(tftp_client.pcb, (struct in_addr){(INADDR_ANY)}, 0);
    if (r != SYS_ERR_OK) {
        USER_PANIC("UDP bind failed");
    }
    debug_printf("bound to %d\n", tftp_client.pcb->bound_port);

    // r = net_connect(tftp_client.pcb, tftp_client.server_ip, tftp_client.server_port, NULL);
    // if (r != SYS_ERR_OK) {
    //     USER_PANIC("UDP connect failed");
    // }

    TFTP_DEBUG("registering recv handler\n");
    net_set_on_received(tftp_client.pcb, tftp_client_recv_handler);

    tftp_client.state = TFTP_ST_IDLE;
    tftp_client.mode = TFTP_MODE_OCTET;
    tftp_client.ppayload = net_alloc(TFTP_MAX_MSGSIZE);
    TFTP_DEBUG("all set up. connection idle\n");
    return SYS_ERR_OK;
}

errval_t tftp_client_disconnect(void)
{
    net_free(tftp_client.ppayload);
    net_close(tftp_client.pcb);
    tftp_client.state = TFTP_ST_CLOSED;
    return SYS_ERR_OK;
}
