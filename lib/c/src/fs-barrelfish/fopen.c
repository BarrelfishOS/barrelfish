/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

FILE *(*_libc_fopen_func)(const char *fname, const char *prot);

FILE *fopen(const char *fname, const char *prot)
{
    if (_libc_fopen_func == NULL) {
        fprintf(stderr, "Warning: fopen() called with _libc_fopen_func unset\n");
        return NULL;
    } else {
        return _libc_fopen_func(fname, prot);
    }
}

FILE *fdopen(int fd, const char *mode)
{
    fprintf(stderr, "Warning: fdopen() unimplemented\n");
    return NULL;
}
