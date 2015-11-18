/**
 * \file
 * \brief Kernel memory debug machinery
 */

/*
 * Copyright (c) 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef NKM_DEBUG_H
#define NKM_DEBUG_H

#ifdef NKMTEST_DEBUG_MAP_UNMAP
#define DEBUG_MAP_UNMAP(...) debug_printf(__VA_ARGS__)
#else
#define DEBUG_MAP_UNMAP(...)
#endif

#ifdef NKMTEST_DEBUG_INVALID_MAPPINGS
#define DEBUG_INVALID_MAPPINGS(...) debug_printf(__VA_ARGS__)
#else
#define DEBUG_INVALID_MAPPINGS(...)
#endif

#ifdef NKMTEST_DEBUG_MODIFY_FLAGS
#define DEBUG_MODIFY_FLAGS(...) debug_printf(__VA_ARGS__)
#else
#define DEBUG_MODIFY_FLAGS(...)
#endif

#endif //NKM_DEBUG_H
