/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>

#include <barrelfish/barrelfish.h>

#include "bulk_net_backend.h"

void stack_alloc_init(struct stack_allocator *alloc, size_t size)
{
    alloc->size = size;
    alloc->top = 0;
    alloc->stack = calloc(size, sizeof(void *));
}

bool stack_alloc_free(struct stack_allocator *alloc, void *el)
{
    if (alloc->top >= alloc->size) {
        return false;
    }

    alloc->stack[alloc->top++] = el;
    return true;
}

void *stack_alloc_alloc(struct stack_allocator *alloc)
{
    if (alloc->top == 0) {
        return NULL;
    }
    return alloc->stack[--alloc->top];
}

