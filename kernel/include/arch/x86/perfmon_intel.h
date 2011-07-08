/**
 * \file
 * \brief Intel 64 performance monitoring infrastructure.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef PERFMON_INTEL_H
#define PERFMON_INTEL_H

void perfmon_init(void);

void perfmon_measure_start0(uint8_t event, uint8_t umask);
uint64_t perfmon_measure_read0(void);

#endif
