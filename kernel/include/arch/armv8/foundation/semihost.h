#ifndef __SEMIHOST_H
#define __SEMIHOST_H

#include <stdint.h>

uint64_t semihost_syscall(uint64_t op, void *param);

#endif /* __SEMIHOST_H */
