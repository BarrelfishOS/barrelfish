/*
 * kaluga.h
 *
 *  Created on: Feb 26, 2012
 *      Author: gz
 */

#ifndef KALUGA_H_
#define KALUGA_H_

#include <barrelfish/barrelfish.h>
#include <dist2/definitions.h>

#include "queue.h"
#include "debug.h"

#define TRIGGER_ALWAYS (DIST_PERSIST | DIST_ON_SET | DIST_ON_DEL | DIST_ALWAYS_SET)
#define BSP_CORE_ID 0

extern coreid_t my_core_id;
extern uint32_t my_arch_id;

errval_t watch_for_cores(void);

void init_environ(void);
errval_t init_boot_modules(void);

#endif /* KALUGA_H_ */
