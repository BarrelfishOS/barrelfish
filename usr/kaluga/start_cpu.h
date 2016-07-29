#ifndef START_CPU_H_
#define START_CPU_H_

#include <errors/errno.h>

struct int_startup_argument;

errval_t watch_for_cores(void);
errval_t wait_for_all_spawnds(void);
errval_t start_boot_driver(coreid_t where,
                           struct module_info* mi,
                           char* record, struct driver_argument * int_arg);


#endif /* START_CPU_H_ */
