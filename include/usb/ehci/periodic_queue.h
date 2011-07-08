/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * Contains routines and definitions related to periodic 
 * queue management. 
 *
 * FIXME: Not yet implemented ...
 */


#ifndef PERIODIC_QUEUE_H
#define PERIODIC_QUEUE_H

//struct ehci_mem periodic_address;

void enable_periodic_schedule(void);
void disable_periodic_schedule(void);
void setup_periodic_queue(void);
void init_iTD_elem(void *start_vaddr, size_t sz);
void add_iTD(iTD x);
void clean_and_exit(void);

void periodic_queue_init(ehci_op_t dev);


// TODO: put it at its proper location 
int remove_queue_head(uint64_t identifier);

#endif                          // PERIODIC_QUEUE_H
