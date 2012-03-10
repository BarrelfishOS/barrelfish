#ifndef KALUGA_H_
#define KALUGA_H_

#include <barrelfish/barrelfish.h>
#include <dist2/dist2.h>

#include "queue.h"
#include "debug.h"

#define TRIGGER_ALWAYS (DIST_PERSIST | DIST_ON_SET | DIST_ON_DEL | DIST_ALWAYS_SET)
#define BSP_CORE_ID 0

extern coreid_t my_core_id;
extern uint32_t my_arch_id;

errval_t trigger_existing_and_watch(const char*,
        trigger_handler_fn, void*,  dist2_trigger_id_t*);


#include "boot_modules.h"
#include "start_pci.h"
#include "start_cpu.h"
#include "driver_startup.h"

#endif /* KALUGA_H_ */
