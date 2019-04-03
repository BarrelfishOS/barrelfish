/*
 * Copyright (c) 2017 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <asmoffsets.h>
#include <offsets.h>


.global invoke_arm_smc
.global invoke_arm_hvc

/*
 * We use the firest 8 arguments to be passed through the registers and
 * the address of the struct to hold the return values on the stack
 */

/*
 * void invoke_arm_smc(uintptr_t a0, uintptr_t a1, uintptr_t a2,
 *                     uintptr_t a3, uintptr_t a4, uintptr_t a5,
 *                     uintptr_t a6, uintptr_t a7, struct arm_smc_hvc_retval *res);
 */
invoke_arm_smc:
    smc  #0
    ldr x4, [sp]
    stp x0, x1, [x4]
    ret

/*
 * void invoke_arm_hvc(uintptr_t a0, uintptr_t a1, uintptr_t a2,
 *                     uintptr_t a3, uintptr_t a4, uintptr_t a5,
 *                     uintptr_t a6, uintptr_t a7, struct arm_smc_hvc_retval *res,);
 */
invoke_arm_hvc:
    hvc  #0
    ldr x4, [sp]
    stp x0, x1, [x4]
    ret
