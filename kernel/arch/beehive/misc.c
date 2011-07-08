/** \file
 * \brief Miscellaneous kernel support code.
 *
 * This file contains miscellaneous architecture-independent kernel support
 * code that doesn't belong anywhere else.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <stdarg.h>
#include <stdio.h>
#include <exec.h>
#include <misc.h>

#define DEFAULT_LOGLEVEL        LOG_DEBUG
#define DEFAULT_SUBSYSTEM_MASK  (~0L)

/**
 * Global kernel loglevel.
 */
int kernel_loglevel = DEFAULT_LOGLEVEL;

/**
 * Default kernel subsystem message mask. Determines messages of what subsystems
 * get output.
 */
int kernel_log_subsystem_mask = DEFAULT_SUBSYSTEM_MASK;

/**
 * 'true' if kernel should handle and context switch on timer ticks.
 * Pass the ticks parameter on the kernel command line if you
 * want to change this.
 */
bool kernel_ticks_enabled = true;

/**
 * The current time since kernel start in timeslices.
 */
size_t kernel_now = 0;


__attribute__((noinline)) void debug_print_backtrace(void)
{
    uintptr_t fp = (uintptr_t)__builtin_frame_address(0);
    int i=0;
    printf("Backtrace follows:\n");
    printf("   ---Frame-- -Address-\n");
    while ((fp != 0) && ((fp & (sizeof(fp)-1)) == 0)) {
	printf("%.2i 0x%.8x 0x%.8x\n", i, fp, ((uintptr_t *)fp)[1]);
	fp = *(uintptr_t *)fp;
	i++;
    }
    printf("%.2i 0x%.8x\n", i, fp);
}



/**
 * \brief Print a message and halt the kernel.
 *
 * Something irrecoverably bad happened. Print a panic message, then halt.
 */
void panic(const char *msg, ...)
{
    va_list ap;
    static char buf[256];

    va_start(ap, msg);
    vsnprintf(buf, sizeof(buf), msg, ap);
    va_end(ap);

    printf("kernel PANIC! (core %d) %.*s\n", my_core_id, (int)sizeof(buf), buf);

    debug_print_backtrace();

    extern void _end_simulation(void);
    _end_simulation();

    // We don't have GDB support - so don't breakpoint
    //breakpoint();
    
    // Halt calls panic! :-)
    halt();
}

/**
 * \brief Log a kernel message.
 *
 * Logs printf()-style message 'msg', having loglevel 'level' to the default
 * kernel console(s). Additional arguments are like printf(). Whether the
 * message is put out depends on the current kernel log level.
 *
 * \param level Loglevel of message.
 * \param msg   The message (printf() format string)
 */
void printk(int level, const char *msg, ...)
{
    if(kernel_loglevel > level) {
        va_list ap;
        static char buf[256];

        va_start(ap, msg);
        vsnprintf(buf, sizeof(buf), msg, ap);
        va_end(ap);

        printf("kernel %.*s", (int)sizeof(buf), buf);
    }
}

/**
 * Helper function used in the implementation of assert()
 */
void __assert(const char *exp, const char *file, const char *func, int line)
{
    panic("kernel assertion \"%s\" failed at %s:%d", exp, file, line);
}
#if 0
void
wait_cycles(uint64_t duration)
{
    uint64_t stoptime;
    stoptime = arch_get_cycle_count() + duration;
    printk(LOG_NOTE, "Waiting %lu cycles...\n", duration);
    while (arch_get_cycle_count() < stoptime);
}
#endif

/**
 * Kernel trace buffer
 */
lvaddr_t kernel_trace_buf = 0;
