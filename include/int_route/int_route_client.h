/**
 * \file
 * \brief Interrupt routing server header file
 */

/*
 * Copyright (c) 2008, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INT_ROUTE_CLIENT_H_
#define INT_ROUTE_CLIENT_H_

errval_t int_route_client_connect(void);
errval_t int_route_client_route(struct capref intsrc, int irq_idx,
        struct capref intdest);

#endif /* INT_ROUTE_CLIENT_H_ */
