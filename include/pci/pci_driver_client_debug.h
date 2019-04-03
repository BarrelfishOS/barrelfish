/*
 * Copyright (c) 2018, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef PCI_DRIVER_CLIENT_DEBUG_H
#define PCI_DRIVER_CLIENT_DEBUG_H

/*****************************************************************
 * Debug printer:
 *****************************************************************/
#define PCI_DRIVER_CLIENT_DEBUG 1

#if defined(PCI_DRIVER_CLIENT_DEBUG ) || defined(GLOBAL_DEBUG)
#define PDC_DEBUG(fmt, ...) printf("pci_driver_client:" fmt, ##__VA_ARGS__)
#else
#define PDC_DEBUG(fmt, ...) ((void)0)
#endif

#endif
