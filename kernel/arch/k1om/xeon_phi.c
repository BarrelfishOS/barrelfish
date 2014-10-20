/**
 * \file
 * \brief Kernel debugging functions
 */

/*
 * Copyright (c) 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <stdio.h>

#include <xeon_phi.h>


void xeon_phi_init_early(void)
{
#if 0
    x86_init.timers.timer_init = mic_timer_init_common;
    x86_init.irqs.pre_vector_init = x86_init_noop;

    x86_platform.calibrate_tsc = intel_mic_calibrate_tsc;
    x86_platform.get_wallclock = intel_mic_get_wallclock;
    x86_platform.set_wallclock = intel_mic_set_wallclock;

    machine_ops.shutdown  = _mic_shutdown;
    legacy_pic = &null_legacy_pic;
    mic_sbox_mmio_va = NULL;
    no_sync_cmos_clock = 1;
#endif
}
