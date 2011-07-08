/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ROUTING_QUERIES_H
#define ROUTING_QUERIES_H

struct query{
    char *str[MAX_CPUS];
    int radix;
};

extern struct query queries[MAX_CPUS][100];

void queries_init(void);

#endif // ROUTING_QUERIES_H
