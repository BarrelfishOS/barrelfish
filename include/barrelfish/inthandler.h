/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _BARRELFISH_INTHANDLER_H
#define _BARRELFISH_INTHANDLER_H

#include <sys/cdefs.h>

__BEGIN_DECLS

typedef void (*interrupt_handler_fn)(void *);


errval_t inthandler_setup_movable_cap(struct capref dest_cap, interrupt_handler_fn handler, void *handler_arg,
                                  interrupt_handler_fn reloc_handler,
                                  void *reloc_handler_arg);

errval_t inthandler_setup_movable(interrupt_handler_fn handler, void *handler_arg,
                                  interrupt_handler_fn reloc_handler,
                                  void *reloc_handler_arg,
                                  uint64_t *ret_vector);
errval_t inthandler_setup(interrupt_handler_fn handler, void *handler_arg,
                          uint64_t *ret_vector);
errval_t inthandler_setup_arm(interrupt_handler_fn handler, void *handler_arg,
        uint32_t irq);

errval_t alloc_dest_irq_cap(struct capref *retcap);

extern struct waitset *barrelfish_interrupt_waitset;

__END_DECLS

#endif
