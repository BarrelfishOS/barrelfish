#include <stdio.h>

#include <offsets.h>
#include <serial.h>

#include <foundation/semihost.h>

/*
 * Kernel stack.
 *
 * This is the one and only kernel stack for a kernel instance.
 * ARMv8 requires that the stack is 16-byte aligned.
 */
uintptr_t kernel_stack[KERNEL_STACK_SIZE/sizeof(uintptr_t)]
    __attribute__ ((aligned(16)));

void shim_entry(void) __attribute__((noreturn,section(".text.shim")));

struct semihost_cmdline {
    char *buf;
    uint64_t length;
};

int semihost_get_cmdline(struct semihost_cmdline *cmdline)
    __attribute__((noinline));

int
semihost_get_cmdline(struct semihost_cmdline *cmdline) {
    return semihost_syscall(0x15, cmdline);
}

const char test_string[] = "test\n";

void
shim_entry(void) {
    struct semihost_cmdline cmdline;
    char cmdline_buf[256];

    serial_early_init(serial_console_port);

    printf("Shim loader starting.\n");

    semihost_syscall(0x04, (void *)test_string);

    cmdline.buf= cmdline_buf;
    cmdline.length= 256;
    int r= semihost_get_cmdline(&cmdline);
    printf("%d %p %p %d\n", r, cmdline_buf, cmdline.buf, cmdline.length);
    printf("\"%s\"\n", cmdline.buf);

    while(1);
}
