/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef APIC_DEBUG_H_
#define APIC_DEBUG_H_


/*****************************************************************
 * Debug printer and its power-switch:
 *****************************************************************/

#define APIC_SERVICE_DEBUG 1

#if defined(APIC_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
#define APIC_DEBUG(x...) printf("ioapic_service: " x)
#else
#define APIC_DEBUG(x...) ((void)0)
#endif

#endif // APIC_DEBUG_H_
