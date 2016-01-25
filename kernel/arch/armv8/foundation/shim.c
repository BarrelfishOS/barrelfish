#include <stdio.h>

#include <offsets.h>
#include <serial.h>

/*
 * Kernel stack.
 *
 * This is the one and only kernel stack for a kernel instance.
 * ARMv8 requires that the stack is 16-byte aligned.
 */
uintptr_t kernel_stack[KERNEL_STACK_SIZE/sizeof(uintptr_t)]
    __attribute__ ((aligned(16)));

void shim_entry(void) __attribute__((noreturn));

void
shim_entry(void) {
    serial_early_init(serial_console_port);

    printf("Shim loader starting.\n");

    while(1);
}
