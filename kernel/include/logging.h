/**
 * \file
 * \brief Kernel logging infrastructure headers.
 *
 * All C source in the kernel should include this file first.
 * This file should contain only definitions and prototypes that are
 * required for the majority of kernel code.
 */

/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __KERNEL_LOGGING_H
#define __KERNEL_LOGGING_H

#include <assert.h>
#include <stddef.h>
#include <stdio.h> // printf for debug
#include <stdint.h>
#include <inttypes.h>
#include <stdbool.h>
#include <barrelfish_kpi/types.h>
#include <barrelfish_kpi/cpu.h>
#include <barrelfish_kpi/registers_arch.h>
#include <errors/errno.h>
#include <bitmacros.h>
#include <debug.h>
#include <offsets.h> /* XXX */
#include <schedule.h>


#define DEFAULT_LOGLEVEL        LOG_NOTE
#define DEFAULT_SUBSYSTEM_MASK  (~0L)


/**
 * Kernel subsystems.
 */
enum KERNEL_SUBSYSTEM {
    SUBSYS_STARTUP        = (1 << 0),        ///< Startup
    SUBSYS_GDB            = (1 << 1),        ///< GDB stub
    SUBSYS_APIC           = (1 << 2),        ///< APIC driver
    SUBSYS_ELF            = (1 << 3),        ///< ELF64 loader
    SUBSYS_PAGING         = (1 << 4),        ///< Paging
    SUBSYS_SYSCALL        = (1 << 5),        ///< System calls
    SUBSYS_CAPS           = (1 << 6),        ///< Capabilities
    SUBSYS_DISPATCH       = (1 << 7),        ///< Scheduling and dispatch
    SUBSYS_IO             = (1 << 8),        ///< Low-level IO operations
};
/**
 * Kernel message loglevels.
 */
enum KERNEL_LOG_LEVEL {
    LOG_PANIC      = 0,       ///< Panic
    LOG_ERR        = 1,       ///< Error
    LOG_WARN       = 2,       ///< Warning
    LOG_NOTE       = 3,       ///< Notice
    LOG_DEBUG      = 4,       ///< Debug
};

void panic(const char *, ...)
    __attribute__((noreturn, format(printf, 1, 2)));
void printk(int level, const char *msg, ...)
    __attribute__ ((format(printf, 2, 3)));
int printf_nolog(const char * fmt, ...)
    __attribute__ ((format(printf, 1, 2)));

/**
 * Command-line variable to set kernel logging level.
 */
extern int kernel_loglevel;

/**
 * Command-line variable to control which subsystems log. Bits defined
 * SUBSYS_* definitions in this file.
 */
extern int kernel_log_subsystem_mask;

/**
 * \brief Log a kernel debug message.
 *
 * Logs printf()-style debug message 'fmt' from subsystem 'subs'
 * to the default kernel console(s). Additional arguments are like
 * printf(). Whether the message is put out depends on the current
 * kernel log level, as well as on the current kernel subsystem log
 * mask.  'debug' is a macro so that the cost of marshalling the
 * arguments is avoided if the relevant debugging is disabled.
 *
 * \param _subs     Subsystem this message stems from.
 * \param _fmt      The message (printf() format string)
 */

#define debug(_subs, _fmt, ...) \
do { \
    if (((_subs) & kernel_log_subsystem_mask) && (kernel_loglevel >= LOG_DEBUG)) \
        printk(LOG_DEBUG, _fmt, ## __VA_ARGS__); \
} while(0)

#endif // __KERNEL_LOGGING_H
