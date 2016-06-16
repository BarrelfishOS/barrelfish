/**
 * \file
 * \brief interface for Cortex A9 Snoop Control Unit
 */
/*
 * Copyright (c) 2015 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaestr. 6, CH-8092 Zurich. 
 * Attn: Systems Group.
 */

#ifndef __A9_SCU_H
#define __A9_SCU_H

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
#include <kernel.h>

/**
 * Initialize the SCU to be found at physical address 'pa'
 */
extern void a9_scu_init( lpaddr_t pa );

/**
 * Print out the SCU contents
 */
extern void a9_scu_print(void);

/**
 * Enable the SCU.  Must have been previously initialized.
 */
extern void a9_scu_enable(void);

/** 
 * Return the core count.
 */
extern size_t a9_scu_core_count(void);


#endif //__A9_SCU_H
