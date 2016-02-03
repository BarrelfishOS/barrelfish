#ifndef __SEMIHOST_H
#define __SEMIHOST_H

#include <stdint.h>

static inline uint64_t semihost_syscall(uint64_t op, void *param) {
    uint64_t ret;

    __asm volatile(
        "mov x0, %[op];"
        "mov x1, %[param];"
        "hlt 0xf000;"
        "mov %[ret], x0;"
        : [ret] "=r" (ret)
        : [op] "r" (op), [param] "r" (param)
        : "x0", "x1");

    return ret;
}

#endif /* __SEMIHOST_H */
