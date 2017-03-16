
/*
 * Copyright (c) 2017 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARM_SMC_H_
#define ARM_SMC_H_

/**
 * @brief struct to hold the result values for the SMC instruction on arm
 */
struct arm_smc_hvc_retval
{
    uintptr_t a0;   ///< register x0 / r0
    uintptr_t a1;   ///< register x1 / r1
    uintptr_t a2;   ///< register x2 / r2
    uintptr_t a3;   ///< register x3 / r3
};

/**
 * @brief invokes the secure monitor instruction on ARM
 *
 * @param a0    Argument 0 (the SMC function to call)
 * @param a1..7 Arguments for the SMC function
 * @param res   Pointer to the struct to hold the return values
 */
void invoke_arm_smc(uintptr_t a0, uintptr_t a1, uintptr_t a2,
                    uintptr_t a3, uintptr_t a4, uintptr_t a5,
                    uintptr_t a6, uintptr_t a7, struct arm_smc_hvc_retval *res);


/**
 * @brief invokes the hypervisor call instruction on ARM
 *
 * @param a0    Argument 0 (the HVC function to call)
 * @param a1..7 Arguments for the HVC function
 * @param res   Pointer to the struct to hold the return values
 */
void invoke_arm_hvc(uintptr_t a0, uintptr_t a1, uintptr_t a2,
                    uintptr_t a3, uintptr_t a4, uintptr_t a5,
                    uintptr_t a6, uintptr_t a7, struct arm_smc_hvc_retval *res);


#endif /* ARM_SMC_H_ */
