/**
 * \file
 * \brief Fixed capability locations and badges for user-defined part of cspace
 */

/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_CSPACE_H
#define BARRELFISH_CSPACE_H

#include <barrelfish_kpi/init.h>

/* Root CNode */
#define ROOTCN_FREE_SLOTS       (ROOTCN_SLOTS_USER+0)   ///< free slots to place EPs

/* Task CNode */
#define TASKCN_SLOT_SELFEP      (TASKCN_SLOTS_USER+0)   ///< Endpoint to self
#define TASKCN_SLOT_INITEP      (TASKCN_SLOTS_USER+1)   ///< End Point to init (for monitor and memserv)
#define TASKCN_SLOT_MONITOREP   (TASKCN_SLOTS_USER+1)   ///< lrpc endpoint to monitor (for all other domains)
#define TASKCN_SLOTS_FREE       (TASKCN_SLOTS_USER+2)   ///< first free slot in taskcn

// taskcn appears at the beginning of cspace, so the cptrs match the slot numbers
#define CPTR_ROOTCN     TASKCN_SLOT_ROOTCN      ///< Cptr to init's root CNode

/* FIXME: Well know virtual addresses for some pages
   that can be mapped into user domain */

#endif // BARRELFISH_CSPACE_H
