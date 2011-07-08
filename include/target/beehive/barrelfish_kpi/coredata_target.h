/**
 * \file
 * \brief Data sent to a newly booted beehive kernel
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef COREDATA_H
#define COREDATA_H

#include <bexec.h>

// XXX Shouldn't really be visible outside kernel
typedef struct stringtable {
    unsigned smagic, slength, soffset[];
} *pstringtable_t;

/**
 * \brief Data sent to a newly booted kernel
 *
 * \bug Should use mackerel to define this struct instead of packing
 * it so that it matches up between heterogeneous cores.
 */
struct beehive_core_data {
    bexec_t   *cpu_module;    ///< bexec structure for the cpu module
    genpaddr_t module_start;  ///< The start of the cpu module
    genpaddr_t module_end;    ///< The end of the cpu module

    bexec_t   *monitor_module;    ///< bexec structure for the cpu module
    genpaddr_t monitor_binary;
    genpaddr_t monitor_binary_size;

    coreid_t src_core_id;
    int      chanid;

    genpaddr_t memory_base;
    genpaddr_t memory_limit;

    struct bootinfo *bootinfo;
    pstringtable_t strings;

} __attribute__ ((packed));


#endif
