#ifndef DRIVER_DOMAIN_H_
#define DRIVER_DOMAIN_H_

#include <errors/errno.h>

#include "boot_modules.h"

struct domain_instance* instantiate_driver_domain(char* name, coreid_t where);

#endif /* DRIVER_DOMAIN_H_ */
