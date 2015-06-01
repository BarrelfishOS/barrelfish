#include <kernel.h>

/**
 * \brief Kernel stack.
 *
 * This is the one and only kernel stack for a kernel instance.
 */
uintptr_t kernel_stack[KERNEL_STACK_SIZE / sizeof(uintptr_t)] __attribute__ ((aligned(16)));

void arch_init(void *arg);
void arch_init(void *arg)
{
}
