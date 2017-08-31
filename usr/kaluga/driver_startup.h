#ifndef DRIVER_STARTUP_H_
#define DRIVER_STARTUP_H_

#include <errors/errno.h>

#include "boot_modules.h"

struct int_startup_argument;
errval_t default_start_function(coreid_t, struct module_info*, char*,
        struct driver_argument * arg);
errval_t start_networking(coreid_t, struct module_info*, char*,
        struct driver_argument * arg);

errval_t newstyle_start_function(coreid_t where, struct module_info* driver, char* record,
        struct driver_argument* int_arg);

#endif /* DRIVER_STARTUP_H_ */
