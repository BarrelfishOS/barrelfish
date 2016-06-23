/**
 * \file
 * \brief kernel driver for the spinlock module, used for serial output
 * see OMAP4460 TRM chapter 21 for a functional description
 */

/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <paging_kernel_arch.h>
#include <omap44xx_spinlock.h>
#include <dev/omap/omap44xx_spinlock_dev.h>
#include <omap44xx_map.h>


#define MSG(format, ...) printk( LOG_NOTE, "OMAP44xx spinlock: "format, ## __VA_ARGS__ )

static omap44xx_spinlock_t spinlock;
static bool locks_initialized;  //allows early initialization

/**
 * \brief Map the OMAP44xx spinlock device into kernel virtual memory
 * and reset it. 
 */

errval_t omap44xx_spinlock_init(void)
{
    if (locks_initialized) {
	MSG("already initialized; skipping.\n");
	return SYS_ERR_OK;
    }
    
    lvaddr_t base = paging_map_device(OMAP44XX_MAP_L4_CFG_SPINLOCK, 
				      OMAP44XX_MAP_L4_CFG_SPINLOCK_SIZE);
    MSG("init base = 0x%"PRIxLVADDR"\n", base);
    omap44xx_spinlock_initialize(&spinlock, (mackerel_addr_t)base);

    // This next line to rest the module is probably not needed.
    omap44xx_spinlock_sysconfig_softreset_wrf(&spinlock, 1);
    
    MSG("testing spinlock: first read (should be 0): 0x%x\n", 
                omap44xx_spinlock_lock_reg_i_taken_rdf(&spinlock, 2));
                
    MSG("testing spinlock: second read (should be 1): 0x%x\n", 
                omap44xx_spinlock_lock_reg_i_taken_rdf(&spinlock, 2));
                
    omap44xx_spinlock_lock_reg_i_taken_wrf(&spinlock, 2, 0);//clear lock 
    locks_initialized = true;
    MSG("init done.\n");
    return SYS_ERR_OK;
}

/**
 * \brief acquire an OMAP44xx spinlock.  We repeatedly read the
 * register; 0 means we have the lock, 1 means we have to try again. 
 */
void omap44xx_spinlock_acquire( unsigned locknumber ) {
    assert( locknumber < OMAP44XX_SPINLOCK_NUM_LOCKS );
    while( omap44xx_spinlock_lock_reg_i_taken_rdf(&spinlock, locknumber)) { }
}

/**
 * \brief release an OMAP44xx spinlock.
 */
void omap44xx_spinlock_release( unsigned locknumber ) {
    assert( locknumber < OMAP44XX_SPINLOCK_NUM_LOCKS );
    omap44xx_spinlock_lock_reg_i_taken_wrf(&spinlock, locknumber, 0);
}

