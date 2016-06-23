/**
 * \file
 * \brief ARM Cortex A9 Snoop Control Unit (SCU) driver.
 */

/*
 * Copyright (c) 2015 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#include <a9_scu.h>
#include <kernel.h>
#include <paging_kernel_arch.h>
#include <dev/cortex_a9_scu_dev.h>

//
// We only have one SCU.
//
static cortex_a9_scu_t scu;
static bool initialized = false;

//
// The SCU is not very big...
//
#define SCU_SIZE 0x100

#define MSG(format, ...) printk( LOG_NOTE, "CortexA9 SCU: "format, ## __VA_ARGS__ )

/**
 * Initialize the SCU to be found at physical address 'pa'
 */
void a9_scu_init( lpaddr_t pa )
{
    assert( !initialized );
    lvaddr_t scu_base = paging_map_device(pa, SCU_SIZE);
    cortex_a9_scu_initialize(&scu, (mackerel_addr_t)scu_base); 
    MSG("initialized at 0x%"PRIxLVADDR"\n", scu_base);
    initialized = true;
}

/**
 * Print out the SCU contents
 */
void a9_scu_print(void)
{
    assert( initialized );
    char buf[ 1024 ];
    cortex_a9_scu_pr( buf, 1024, &scu );
    printf("%s\n", buf);
}

/**
 * Enable the SCU.  Must have been initialized.
 */
void a9_scu_enable(void)
{
    assert( initialized );
    cortex_a9_scu_control_enable_wrf(&scu, 1);
    // (should invalidate d-cache here?)

}

/** 
 * Return the core count.
 */
size_t a9_scu_core_count(void)
{
    assert( initialized );
    return cortex_a9_scu_config_numcpus_rdf(&scu) + 1;
}
