/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef MONITOR_SERVER_H
#define MONITOR_SERVER_H

errval_t monitor_client_setup(struct spawninfo *si);
errval_t monitor_client_setup_mem_serv(void);
errval_t monitor_client_setup_monitor(void);
errval_t monitor_server_init(struct monitor_binding *b);
errval_t monitor_rpc_init(void);
errval_t monitor_server_arch_init(struct monitor_binding *b);
void set_monitor_rpc_iref(iref_t iref);

#endif
