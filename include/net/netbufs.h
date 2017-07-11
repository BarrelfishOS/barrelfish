/**
 * @brief 
 *  net.h
 */

/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */


#ifndef LIB_NET_INCLUDE_NETWORKING_BUFFER_H_
#define LIB_NET_INCLUDE_NETWORKING_BUFFER_H_

#define NETWORKING_BUFFER_DEFAULT_SIZE 2048

struct net_buf_pool;
struct pbuf;
struct devq;

/**
 * @brief initializes the networking buffer pools
 *
 * @param dev_q     the device queue to create the buffer pool for
 * @param numbuf    number of initial buffers
 * @param size      size of the networking buffer
 * @param retbp     buffer pool to initialize
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t net_buf_pool_alloc(struct devq *dev_q, size_t numbuf, size_t size,
                            struct net_buf_pool **retbp);


errval_t net_buf_pool_free(struct net_buf_pool *retbp);


/**
 * @brief grows the number of available buffers
 *
 * @param bp        buffer pool to grow
 * @param numbuf    number of buffers to create
 * @param size      size of a buffer
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t net_buf_grow(struct net_buf_pool *bp, size_t numbuf,
                                size_t size);

/**
 * @brief adds a previously allocated frame to the buffer pool
 *
 * @param bp            buffer pool to add the frame to
 * @param frame         frame capability
 * @param buffersize    size of a buffer
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t net_buf_add(struct net_buf_pool *bp,
                               struct capref frame, size_t buffersize);

struct pbuf *net_buf_alloc(struct net_buf_pool *bp);

/**
 * @brief
 * @param p
 */
void net_buf_free(struct pbuf *p);

/**
 * @brief
 * @param bp
 * @param regionid
 * @param offset
 * @return
 */
struct pbuf *net_buf_get_by_region(struct net_buf_pool *bp,
                                             uint32_t regionid, size_t offset);

#endif /* LIB_NET_INCLUDE_NETWORKING_BUFFER_H_ */
