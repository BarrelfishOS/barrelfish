#include <stdio.h>

#include <offsets.h>
#include <serial.h>

#include <foundation/semihost.h>

void vminit(void) __attribute__((noreturn,section(".vminit")));

void
vminit(void) {
    while(1);
}
