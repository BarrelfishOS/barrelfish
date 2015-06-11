#include <kernel.h>
#include <serial.h>

/**
 * \brief Kernel stack.
 *
 * This is the one and only kernel stack for a kernel instance.
 */
uintptr_t kernel_stack[KERNEL_STACK_SIZE / sizeof(uintptr_t)] __attribute__ ((aligned(16)));

__attribute__((noreturn))
void arch_init(void *arg);
void arch_init(void *arg)
{
    // set console port: XXX which?
    serial_console_port = 0;

    // init serial console, skip hwinit for now
    serial_console_init(false);

    // print something
    printf("Barrelfish APM88xxxx CPU driver starting at addr 0x%"
            PRIxLVADDR" on core %"PRIuCOREID"\n",
            local_phys_to_mem((lpaddr_t)&kernel_first_byte), my_core_id);

    while(1);
}
