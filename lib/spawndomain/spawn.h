/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef SPAWN_H
#define SPAWN_H

lvaddr_t map_module(struct mem_region *module, size_t *retsize);
const char *getopt(const char **optstring, char *buf, size_t buflen,
                   size_t *optlen);

errval_t spawn_symval_register(const char *binary, uint32_t idx,
                               const char *symname, genvaddr_t addres);
errval_t spawn_symval_lookup(const char *binary, uint32_t idx, char **ret_name,
                             genvaddr_t *ret_addr);
#endif
