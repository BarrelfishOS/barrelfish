/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


#ifndef BARRELFISH_SIGNAL_H_
#define BARRELFISH_SIGNAL_H_

#define SIGINT          2

#define SIG_ERR ((void *) -1)
#define SIG_DFL ((void *) 0)
#define SIG_IGN ((void *) 1)

typedef void (*signalhandler_t)(int);

signalhandler_t signal(int sugnum, signalhandler_t handler);

#endif // BARRELFISH_SIGNAL_H_
