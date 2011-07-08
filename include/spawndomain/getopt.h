/**
 * \file
 * \brief Very simple commandline argument parsing.
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef SPAWN_GETOPT_H
#define SPAWN_GETOPT_H

#include <stddef.h>

const char *getopt(const char **optstring, char *buf, size_t buflen,
                   size_t *optlen);

#endif // SPAWN_GETOPT_H
