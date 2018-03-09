#ifndef KALUGA_H_
#define KALUGA_H_

#include <barrelfish/barrelfish.h>
#include <octopus/octopus.h>
#include <driverkit/driverkit.h>

#include "queue.h"
#include "debug.h"

#define BSP_CORE_ID 0

extern coreid_t my_core_id;
extern uint32_t my_arch_id;
extern struct pci_addr eth0;
extern char **environ;

#include "boot_modules.h"
#include "start_pci.h"
#include "start_cpu.h"
#include "driver_startup.h"
#include "device_caps.h"
#include "int_caps.h"
#include "driver_domains.h"
#include "int_route/int_model.h"

errval_t arch_startup(char * add_device_db_file);

#endif /* KALUGA_H_ */
