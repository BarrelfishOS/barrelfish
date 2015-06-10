/*
 * Copyright (c) 2015 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBCPUID_AMD_H_
#define LIBCPUID_AMD_H_ 1

#include <string.h>

/**
 * \brief verifies the CPU
 *
 * \returns TRUE if its a Intel CPU
 *          FALSE otherwise
 */
static inline bool cpuid_amd_check_vendor(void)
{
    struct cpuid_regs reg = CPUID_REGS_INITIAL(0, 0);
    cpuid_exec(&reg);

    return (strncmp((char *)&reg.ebx, CPUID_VENDOR_STRING_AMD, 12) == 0);
}

/**
 * \brief fills the vendor specific handler functions
 *
 * \param fn_tab  function pointer table to be filled
 */
void cpuid_amd_set_handlers(struct cpuid_functions *fn_tab);

#endif /* CPUID_INTERNAL_H_ */
