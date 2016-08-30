/** \file
 * \brief Miscellaneous kernel support code.
 *
 * This file contains miscellaneous architecture-independent kernel support
 * code that doesn't belong anywhere else.
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
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

    printf("kernel PANIC! %.*s\n", (int)sizeof(buf), buf);

    halt();
}

/**
 * \brief Spin forever
 *
 */
void halt(void)
{
    while(1) __asm volatile("wfe");
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

        printf("kernel.%02d %.*s", my_core_id, (int)sizeof(buf), buf);
    }
}

/**
 * Helper function used in the implementation of assert()
 */
void __assert_func(const char *file, int line, const char *func, const char *exp)
{
    panic("kernel assertion \"%s\" failed at %s:%d", exp, file, line);
}
