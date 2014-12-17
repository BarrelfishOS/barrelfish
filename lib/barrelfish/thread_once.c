/* Copyright (c) 2007, Google Inc.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *     * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above
 * copyright notice, this list of conditions and the following disclaimer
 * in the documentation and/or other materials provided with the
 * distribution.
 *     * Neither the name of Google Inc. nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

// Taken from C++11 proposal N2660:
//   "Dynamic Initialization and Destruction with Concurrency"
//   http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2008/n2660.htm

#include <barrelfish/threads.h>

/// Protects global_epoch and all thread_once_t writes.
static struct thread_mutex thread_once_mutex = THREAD_MUTEX_INITIALIZER;

/// Signalled whenever a fast_pthread_once_t is finalized.
static struct thread_cond thread_once_condvar = THREAD_COND_INITIALIZER;

#define THREAD_ONCE_BEING_INITIALIZED (THREAD_ONCE_INIT - 1)

static thread_once_t thread_once_global_epoch = 0;

__thread thread_once_t thread_once_local_epoch;

void thread_once_internal(thread_once_t *control, void (*func)(void)) {
    thread_once_t x = *control; // unprotected access
    if (x > thread_once_local_epoch) {
        thread_mutex_lock(&thread_once_mutex);
        if (*control == THREAD_ONCE_INIT) {
            *control = THREAD_ONCE_BEING_INITIALIZED;
            thread_mutex_unlock(&thread_once_mutex);
            (*func)();
            thread_mutex_lock(&thread_once_mutex);
            thread_once_global_epoch++;
            *control = thread_once_global_epoch;
            thread_cond_broadcast(&thread_once_condvar);
        } else {
            while (*control == THREAD_ONCE_BEING_INITIALIZED) {
                thread_cond_wait(&thread_once_condvar, &thread_once_mutex);
            }
        }
        thread_once_local_epoch = thread_once_global_epoch;
        thread_mutex_unlock(&thread_once_mutex);
    }
}
