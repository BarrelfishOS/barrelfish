/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INTEL_VTD_DEBUG_H_
#define INTEL_VTD_DEBUG_H_ 1


#define INTEL_VTD_DEBUG_ENABLED 1

#define INTEL_VTD_DEBUG_COMMANDS_ENABLE 1
#define INTEL_VTD_DEBUG_IOTLB_ENABLE 1


#if INTEL_VTD_DEBUG_ENABLED
#define INTEL_VTD_DEBUG_PRINT(x...) debug_printf("[vtd] " x)
#else
#define INTEL_VTD_DEBUG_PRINT(x...)
#endif

#if INTEL_VTD_DEBUG_COMMANDS_ENABLE
#define INTEL_VTD_DEBUG_COMMANDS(x...) INTEL_VTD_DEBUG_PRINT("[cmd] " x)
#else
#define INTEL_VTD_DEBUG_COMMANDS(x...)
#endif

#if INTEL_VTD_DEBUG_IOTLB_ENABLE
#define INTEL_VTD_DEBUG_IOTLB(x...) INTEL_VTD_DEBUG_PRINT("[iotlb] " x)
#else
#define INTEL_VTD_DEBUG_IOTLB(x...)
#endif

#endif /// INTEL_VTD_H_