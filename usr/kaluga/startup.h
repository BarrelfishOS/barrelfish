#ifndef CUSTOM_STARTUP_H_
#define CUSTOM_STARTUP_H_

#include <errors/errno.h>

#include "boot_modules.h"

errval_t default_start_function(coreid_t, struct module_info*, char*);
errval_t start_networking(coreid_t, struct module_info*, char*);

#endif /* CUSTOM_STARTUP_H_ */
