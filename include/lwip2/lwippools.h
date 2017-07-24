/**
 * \file lwippools.h
 * \brief 
 */


/*
 * Copyright (c) 2017 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

//#ifndef LIB_LWIP_2_0_2_SRC_INCLUDE_LWIPPOOLS_H_
//#define LIB_LWIP_2_0_2_SRC_INCLUDE_LWIPPOOLS_H_ 1

/* @file lwippools.h
 * Define three pools with sizes 256, 512, and 1512 bytes */
LWIP_MALLOC_MEMPOOL_START
LWIP_MALLOC_MEMPOOL_END

/* My sys_arch uses memory pools to allocate mbox and sems */
//LWIP_MEMPOOL(SYS_MBOX, 22, sizeof(struct sys_mbox_struct), "SYS_MBOX")
//LWIP_MEMPOOL(SYS_SEM, 12, sizeof(struct sys_sem_struct), "SYS_SEM")


//#endif /* LIB_LWIP_2_0_2_SRC_INCLUDE_LWIPPOOLS_H_ */





