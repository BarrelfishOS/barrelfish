#ifndef START_CPU_H_
#define START_CPU_H_

#include <errors/errno.h>

errval_t watch_for_cores(void);
errval_t watch_for_ioapic(void);

#endif /* START_CPU_H_ */
