/** \file
 * \brief Logging kernel support code.
 *
 */

/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <barrelfish_kpi/cpu.h>
#include <exec.h>
#include <misc.h>
#include <dispatch.h>
#include <trace/trace.h>
#include <logging.h>
#include <debug.h>

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

    printf("kernel %d PANIC! %.*s\n", my_core_id, (int)sizeof(buf), buf);

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
    if(kernel_loglevel >= level) {
        va_list ap;
        static char buf[256];

        va_start(ap, msg);
        vsnprintf(buf, sizeof(buf), msg, ap);
        va_end(ap);

        printf("kernel %d: %.*s", my_core_id, (int)sizeof(buf), buf);
    }
}

/**
 * Helper function used in the implementation of assert()
 */
void __assert(const char *func, const char *file, int line, const char *exp)
{
    panic("kernel assertion \"%s\" failed at %s:%d", exp, file, line);
}


/**
 * Kernel trace buffer
 */
lvaddr_t kernel_trace_buf = 0;

struct trace_application kernel_trace_boot_applications[TRACE_MAX_BOOT_APPLICATIONS];

int kernel_trace_num_boot_applications = 0;
