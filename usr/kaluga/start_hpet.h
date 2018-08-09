#ifndef START_HPET_H_
#define START_HPET_H_

#include <errors/errno.h>
#include<int_caps.h>

errval_t watch_for_hpet(void);

void hpet_change_event(oct_mode_t mode, const char* device_record, void* st);

errval_t start_hpet_driver(coreid_t where, struct module_info* driver, char* record, struct driver_argument* arg);

#endif /* START_HPET_H_ */