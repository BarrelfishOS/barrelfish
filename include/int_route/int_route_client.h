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
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INT_ROUTE_CLIENT_H_
#define INT_ROUTE_CLIENT_H_

typedef void (*interrupt_handler_fn)(void *);

/* Setup the client */
errval_t int_route_client_connect(void);

/* Connect an interrupt source capability with a interrupt destination cap */
errval_t int_route_client_route(struct capref intsrc, int irq_idx,
        struct capref intdest);

/* Do it all, route, allocate EP, connect to EP to handler */
errval_t int_route_client_route_and_connect(struct capref intsrc, int irq_idx,
        struct waitset *ws, interrupt_handler_fn handler, void *handler_arg);

#endif /* INT_ROUTE_CLIENT_H_ */
