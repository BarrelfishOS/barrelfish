/**
 * \file
 * \brief net socket server shared parameters
 */

/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8006 Zurich. Attn: Systems Group.
 */

#ifndef NETSS_H_
#define NETSS_H_

//#define NETSS_DEBUG

// parameters
#define NO_OF_BUFFERS 128
#define BUFFER_SIZE 16384
#define NETSOCKET_LOOP_ITER 100
#define MAX_SEND_FRAMES 2

//#define POLLING

#if defined(NETSS_DEBUG) 
#define NET_SOCK_DEBUG(x...) do { printf("net_sock:%s.%d:%s:%d: ", \
            disp_name(), disp_get_core_id(), __func__, __LINE__); \
                printf(x);\
        } while (0)
#else
#define NET_SOCK_DEBUG(x...) ((void)0)
#endif 

#endif /* NETSS_H_ */
