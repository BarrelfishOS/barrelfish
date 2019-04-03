/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef MONITOR_SPAWN_H
#define MONITOR_SPAWN_H

errval_t spawn_all_domains(void);
errval_t spawn_domain(char *name);
errval_t spawn_domain_with_args(const char *name, char *const argv[],
                                char *const envp[]);
errval_t spawn_module_with_args(const char *name, struct mem_region *module,
                                char *const argv[], char *const envp[]);
errval_t span_domain(struct capref vroot, struct capref dispframe);
errval_t spawn_spawnd(struct intermon_binding *b);

#endif
