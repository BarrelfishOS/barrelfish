/*
 * Copyright (c) 2007-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BUFFER_TBL_H_
#define BUFFER_TBL_H_

#include <barrelfish/barrelfish.h>
#include "sfn5122f.h"

/**
 * @brief adds one or several 4k buffer to the buffer table
 * 
 * @param phys_addr     the physical address of the buffer
 * @param num_buf       the number of buffers to add
 * @param qid           for descriptor queue (RX/TX/EV) queues
 *                      the qid should be 0 (used as owner id)
 *                      otherwise for send/recv buffers for userlvl
 *                      networking != 0
 * @param userpsace     is userlvl networking enabled
 * @param d             handle to device registers
 *
 * @return the id of of the added buffer in the table
 */
uint64_t alloc_buf_tbl_entries(uint64_t phys_addr,
                               uint32_t num_buf, 
                               uint16_t qid ,
                               bool userspace, 
                               sfn5122f_t *d);

/**
 * @brief removes one or several 4k buffer from the buffer table
 * 
 * @param buftbl_idx    the index from which to remove the entry
 * @param num_buf       the number of buffers to remove
 * @param d             handle to device registers
 *
 */
void free_buf_tbl_entries(uint64_t buftbl_idx,
                          uint32_t num_buf,
                          sfn5122f_t *d);
#endif // ndef BUFFER_TBL_H_
