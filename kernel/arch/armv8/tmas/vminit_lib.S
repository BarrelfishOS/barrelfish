/*
 * Copyright (c) 2016, ETH Zurich.
 * Copyright (c) 2016, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

/* Utility routines for booting. */

.section .vminit, "ax"

.global eret

/* Arguments x0-x3 are passed through */
eret:
    eret
