/**
 * \file
 * \brief Platform code for ARM Fixed Virtual Platform simulators, modelling a
 *        Versatile Express board.
 */

#include <kernel.h>

#include <init.h>
#include <platform.h>

/* The VE FVP models have 4GB of RAM (see FVP reference guide, table 5-1).
   We don't implement LPAE, so we're ignoring the upper 2GB. */
size_t platform_get_ram_size(void)
{
    assert(!mmu_is_enabled());
    return 2 * (1UL << 30);
}

/* The FVP peripheral clock is 100MHz (see FVP reference guide, S5.3). */
const uint32_t tsc_hz = 100 * 1000 * 1000;
