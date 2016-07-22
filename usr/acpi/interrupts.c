/**
 * \file
 * \brief Interrupt management (Local and IOAPICs) and routing
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <arch/aarch64/hw_records_arch.h>
#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/cpu_arch.h>
#include <acpi.h>
#include <mm/mm.h>

#include <skb/skb.h>
#include <octopus/getset.h>
#include <trace/trace.h>

