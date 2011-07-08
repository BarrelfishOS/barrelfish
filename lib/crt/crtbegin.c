/**
 * \file
 * \brief C++ startup code. Contains .ctor section header and _main().
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdint.h>

void *__dso_handle = &__dso_handle;

typedef void (*CDtor)(void);

static CDtor ctors[1]
__attribute__ ((unused, section(".ctors"), aligned(sizeof(CDtor))))
    = { (CDtor)(-1) };

int _main(int argc, char *argv[]);
int main(int argc, char *argv[]);

static void call_global_ctors(void)
{
    intptr_t n = (intptr_t)ctors[0];

    if(n == -1) {
        for(n = 0; ctors[n + 1] != 0; n++);
    }

    for(intptr_t i = n; i >= 1; i--) {
        ctors[i]();
    }
}

int _main(int argc, char *argv[])
{
    call_global_ctors();
    return main(argc, argv);
}
