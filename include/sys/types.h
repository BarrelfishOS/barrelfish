/**
 * \file sys/types.h
 * \brief Include for libmsun, because it searches types.h in sys
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


#ifndef _SYS_TYPES_H
#define _SYS_TYPES_H

#include <stdint.h>
#include <sys/_types.h>
#include <barrelfish/types.h>

typedef int mode_t;
typedef int fd_set;
typedef long ino_t;
typedef long dev_t;
typedef domainid_t pid_t;

#endif // _SYS_TYPES_H
