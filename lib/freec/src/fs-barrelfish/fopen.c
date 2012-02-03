/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

FILE *(*_freec_fopen_func)(const char *fname, const char *prot);

FILE *fopen(const char *fname, const char *prot)
{
    if (_freec_fopen_func == NULL) {
        fprintf(stderr, "Warning: fopen() called with _freec_fopen_func unset\n");
        return NULL;
    } else {
        return _freec_fopen_func(fname, prot);
    }
}

FILE *fdopen(int fd, const char *mode)
{
    fprintf(stderr, "Warning: fdopen() unimplemented\n");
    return NULL;
}

FILE *freopen(const char *fname, const char *mode, FILE *stream)
{
    fprintf(stderr, "Warning: freopen() unimplemented\n");
    return NULL;
}

FILE *popen(const char *command, const char *type)
{
    fprintf(stderr, "Warning: popen() unimplemented\n");
    return NULL;
}

int pclose(FILE *stream)
{
    fprintf(stderr, "Warning: pclose() unimplemented\n");
    return -1;
}
