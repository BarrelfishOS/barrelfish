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
#define INTEL_VTD_DEBUG_CAP_ENABLE 0

#define INTEL_VTD_DEBUG_CTXT_ENABLED 0
#define INTEL_VTD_DEBUG_ROOT_ENABLED 0
#define INTEL_VTD_DEBUG_DEVICES_ENABLED 1
#define INTEL_VTD_DEBUG_DOMAINS_ENABLED 1

#define INTEL_VTD_DEBUG(X...) debug_printf("[vtd] " X);


#define INTEL_VTD_ERROR(x...) debug_printf("[vtd] [error] " x)
#define INTEL_VTD_NOTICE(x...) debug_printf("[vtd] [notice] " x)



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

#if INTEL_VTD_DEBUG_CAP_ENABLE
#define INTEL_VTD_DEBUG_CAP(x...) INTEL_VTD_DEBUG_PRINT("[cap] " x)
#else
#define INTEL_VTD_DEBUG_CAP(x...)
#endif

#if INTEL_VTD_DEBUG_CTXT_ENABLED
#define INTEL_VTD_DEBUG_CTABLE(x...) INTEL_VTD_DEBUG_PRINT("[ctxt] " x)
#else
#define INTEL_VTD_DEBUG_CTABLE(x...)
#endif

#if INTEL_VTD_DEBUG_ROOT_ENABLED
#define INTEL_VTD_DEBUG_RTABLE(x...) INTEL_VTD_DEBUG_PRINT("[root] " x)
#else
#define INTEL_VTD_DEBUG_RTABLE(x...)
#endif

#if INTEL_VTD_DEBUG_DEVICES_ENABLED
#define INTEL_VTD_DEBUG_DEVICES(x...) INTEL_VTD_DEBUG_PRINT("[devs] " x)
#else
#define INTEL_VTD_DEBUG_DEVICES(x...)
#endif

#if INTEL_VTD_DEBUG_DOMAINS_ENABLED
#define INTEL_VTD_DEBUG_DOMAINS(x...) INTEL_VTD_DEBUG_PRINT("[doms] " x)
#else
#define INTEL_VTD_DEBUG_DOMAINS(x...)
#endif


#endif /// INTEL_VTD_H_