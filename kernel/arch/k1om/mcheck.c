/**
 * \file
 * \brief x86 machine check architecture initialisation / "driver"
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <x86.h>
#include <arch/x86/mcheck.h>
#include <dev/ia32_dev.h>
#include <dev/amd64_dev.h>

/*
 * The Intel Xeon Phi coprocessor does not support MCA as defined by the Intel
 * Pentium Pro and later Intel processors. However, MCEs on the Intel Xeon Phi
 * coprocessor are compatible with the Intel Pentium processor.
 *
 * TODO: There are additional MCE registers on the SBOXs
 */

/**
 * \brief Enable machine check reporting for the Intel Xeon Phi
 */
void
mcheck_init(void)
{
    // use CPUID to check for support and get processor family
    uint32_t eax, edx;
    cpuid(1, &eax, NULL, NULL, &edx);

    uint8_t proc_family = (eax >> 8) & 0xf;

    /*
     *
     * While the Intel Xeon Phi coprocessor does support MCA and MCE
     * capabilities, the CPUID feature bits used to identify the processor
     * supports for these features are not set on the Intel Xeon Phi coprocessor.
     */

    // if MCE unsupported, skip this entirely
    debug(SUBSYS_STARTUP, "Machine-check exceptions supported\n");

    debug(SUBSYS_STARTUP, "Machine-check architecture supported, "
          "family 0x%x\n",
          proc_family);

    // if the ctl register is present, enable all MCA features
    ia32_mcg_cap_t mcg_cap = ia32_mcg_cap_rd(NULL);

    /*
     * The Intel Xeon Phi coprocessor provides three MC MSR banks.
     */
    int num_banks = ia32_mcg_cap_count_extract(mcg_cap);

    assert(num_banks == 3);

    if (ia32_mcg_cap_ctl_p_extract(mcg_cap)) {
        ia32_mcg_ctl_wr(NULL, ia32_mc_enable);
    }

    for (int i = 0; i < num_banks; i++) {
        ia32_mc_ctl_wr(NULL, i, ia32_mc_enable);
    }

    // clear all errors
    for (int i = 0; i < num_banks; i++) {
        ia32_mc_status_wr(NULL, i, 0UL);
    }

    // enable machine-check exceptions
    amd64_cr4_mce_wrf(NULL, 1);
}
