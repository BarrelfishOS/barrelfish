#ifndef DRIVER_STARTUP_H_
#define DRIVER_STARTUP_H_

#include <errors/errno.h>

#include "boot_modules.h"

struct int_startup_argument;
errval_t default_start_function(coreid_t, struct module_info*, char*,
        struct driver_argument * arg);
errval_t start_networking(coreid_t, struct module_info*, char*,
        struct driver_argument * arg);

#endif /* DRIVER_STARTUP_H_ */
