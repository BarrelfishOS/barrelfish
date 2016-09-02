/**
 * \file
 * \brief Null implementation of plat_advance_aps
 */

/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>

#include <boot_protocol.h>

/* Many platforms don't need any help to get the boot driver up. */
void
plat_advance_aps(void) {
}
