//===------------------------- abort_message.cpp --------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is dual licensed under the MIT and the University of Illinois Open
// Source Licenses. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "abort_message.h"

#ifdef __BIONIC__
#include <android/set_abort_message.h>
#include <syslog.h>
#endif

#pragma GCC visibility push(hidden)

#if __APPLE__
#   if defined(__has_include) && __has_include(<CrashReporterClient.h>)
#       define HAVE_CRASHREPORTERCLIENT_H 1
#       include <CrashReporterClient.h>
#   endif
#endif

#ifdef BARRELFISH
// XXX: do not want to include <barrelfish/domain.h> <barrelfish/dispatch.h>
// and <barrelfish/threads.h> here, so we give the plain declarations for the
// functions we need to get Barrelfish styling in the abort message.
// -SG, 2015-02-18
#define DISP_NAME_LEN   16
extern "C" {
extern const char *disp_name(void);
extern coreid_t disp_get_core_id(void);
extern uintptr_t thread_id(void);
}
#endif

__attribute__((visibility("hidden"), noreturn))
void abort_message(const char* format, ...)
{
    // write message to stderr
#if __APPLE__
    fprintf(stderr, "libc++abi.dylib: ");
#endif

#ifdef BARRELFISH
    // use Barrelfish debug_printf style to get info about dispatcher and
    // thread.
    fprintf(stderr, "\033[34m%.*s.\033[31m%u.%lu\033[0m: ", DISP_NAME_LEN,
            disp_name(), disp_get_core_id(), thread_id());
#endif
    va_list list;
    va_start(list, format);
    vfprintf(stderr, format, list);
    va_end(list);
    fprintf(stderr, "\n");

#if __APPLE__ && HAVE_CRASHREPORTERCLIENT_H
    // record message in crash report
    char* buffer;
    va_list list2;
    va_start(list2, format);
    vasprintf(&buffer, format, list2);
    va_end(list2);
    CRSetCrashLogMessage(buffer);
#elif __BIONIC__
    char* buffer;
    va_list list2;
    va_start(list2, format);
    vasprintf(&buffer, format, list2);
    va_end(list2);

    // Show error in tombstone.
    android_set_abort_message(buffer);

    // Show error in logcat.
    openlog("libc++abi", 0, 0);
    syslog(LOG_CRIT, "%s", buffer);
    closelog();
#endif

    abort();
}

#pragma GCC visibility pop
