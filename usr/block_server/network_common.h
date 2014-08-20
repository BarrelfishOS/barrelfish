/**
 * \file
 * \brief Network client of the block service
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BLOCK_NETWORK_COMMON_H
#define BLOCK_NETWORK_COMMON_H

enum block_net_msg_type {
    BLOCK_SETUP,    ///< transfer setup information of the bulk channel
    BLOCK_READ,     ///< issue a block read request
    BLOCK_WRITE     ///< issue a block write request
};

/**
 * data structure representing the data of the messages transferred over the
 * service channel
 */
struct block_net_msg {
    enum block_net_msg_type op;
    uintptr_t blockid;
};



#endif /* BLOCK_NETWORK_COMMON_H */
