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



#ifndef BLOCK_NETWORK_SERVER_H
#define BLOCK_NETWORK_SERVER_H

#include "network_common.h"


errval_t block_net_init(uint16_t port);

errval_t block_net_start(void);

errval_t block_net_stop(void);

struct block_net_server *block_net_server_lookup(size_t block_start);

#endif /* BLOCK_NETWORK_SERVER_H */
