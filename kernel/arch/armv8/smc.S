/*
 * Copyright (c) 2017 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <asmoffsets.h>
#include <offsets.h>

#define ARM_SMCCC_QUIRK_NONE        0
#define ARM_SMCCC_QUIRK_QCOM_A6     1 /* Save/restore register a6 */

.global invoke_arm_smc64
.global invoke_arm_hvc64

/*
 * void invoke_arm_smc(uintptr_t fn, uintptr_t a1, uintptr_t a2,
 *                     uintptr_t a3, uintptr_t a4, uintptr_t a5,
 *                     uintptr_t a6, uintptr_t a7, struct psci_retval *res);
 */
invoke_arm_smc64:
    smc  #0
    ldr x4, [sp]
    stp x0, x1, [x4]
    ret

/*
 * void invoke_arm_smc(uintptr_t fn, uintptr_t a1, uintptr_t a2,
 *                     uintptr_t a3, uintptr_t a4, uintptr_t a5,
 *                     uintptr_t a6, uintptr_t a7, struct psci_retval *res,);
 */
invoke_arm_hvc64:
    hvc  #0
    ldr x4, [sp]
    stp x0, x1, [x4]
    ret
