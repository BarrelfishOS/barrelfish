
/*
 * Copyright (c) 2017 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <offsets.h> // BOOT_STACK_PHYS

.text

.globl psci_boot_entry, parking_boot_entry



/*
    efiboot_secondary(struct armv8_core_data *)

    This is the entry point for the new core when using the PSCI
    protocol.

    We need to setup the stack before we can jump to the secondary
    init code

 */
psci_boot_entry:
    ldr x1, [x0]
    mov sp, x1
    b efiboot_secondary_bringup

/*
 * This is the entry point for the parking protocol
 */
parking_boot_entry:
    wfi
