/**
 * \file
 * \brief Network server thread of the bulk server
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */



#ifndef BLOCK_LOCAL_SERVER_H
#define BLOCK_LOCAL_SERVER_H

/* ---------------------------  Server State  ---------------------------- */
enum local_server_state {
    SERVICE_STATE_UNINITIALIZED = 0,
    SERVICE_STATE_EXPORTED = 1,
    SERVICE_STATE_RUNNING = 2,
    SERVICE_STATE_STOPPED = 3,
    SERVICE_STATE_FAILURE = 4,
};

#define SERVICE_FLAG_DEFAULT 0x0
#define SERVICE_FLAG_CLIENT  0x1



errval_t block_local_init(uint32_t flags);

errval_t block_local_start(void);

errval_t block_local_stop(void);


#endif /* BLOCK_LOCAL_SERVER_H */
