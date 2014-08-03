/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_XOMP_H_
#define LIB_XOMP_H_



/*
 * XOMP channel settings
 */
#define XOMP_MSG_CHAN_SIZE 4096
#define XOMP_MSG_FRAME_SIZE (2* XOMP_MSG_CHAN_SIZE)

typedef uint64_t xomp_wid_t;

/*
 *
 */


#include <xomp/xomp_master.h>
#include <xomp/xomp_worker.h>

#endif // LIB_XOMP_H_

