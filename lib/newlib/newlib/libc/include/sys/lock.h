#ifndef __SYS_LOCK_H__
#define __SYS_LOCK_H__

#ifdef BARRELFISH

/* delegate locking to Barrelfish thread_mutex
 *
 * Our implementation is in libc/sys/barrelfish/lock.c essentially wrappers
 * around Barrelfish's thread_mutex_{init,lock,unlock} to not get into cyclic
 * #include hell.
 * We need to include thread_sync here to have the full definition for struct
 * thread_mutex.
 */
#include <barrelfish/thread_sync.h>

typedef struct thread_mutex _LOCK_T;
typedef struct thread_mutex _LOCK_RECURSIVE_T;

void bf_libc_lock_init(struct thread_mutex *lock);
void bf_libc_lock_close(struct thread_mutex *lock);
void bf_libc_lock_acquire(struct thread_mutex *lock);
void bf_libc_lock_acquire_recursive(struct thread_mutex *lock);
void bf_libc_lock_try_acquire(struct thread_mutex *lock);
void bf_libc_lock_try_acquire_recursive(struct thread_mutex *lock);
void bf_libc_lock_release(struct thread_mutex *lock);

#define __LOCK_INIT(class,lock) class _LOCK_T lock = THREAD_MUTEX_INITIALIZER;
#define __LOCK_INIT_RECURSIVE(class,lock) class _LOCK_RECURSIVE_T lock = THREAD_MUTEX_INITIALIZER;
#define __lock_init(lock) bf_libc_lock_init(&(lock))
#define __lock_init_recursive(lock) bf_libc_lock_init(&(lock))
#define __lock_close(lock) bf_libc_lock_close(&(lock)
#define __lock_close_recursive(lock) bf_libc_lock_close(&(lock))
#define __lock_acquire(lock) bf_libc_lock_acquire(&(lock))
#define __lock_acquire_recursive(lock) bf_libc_lock_acquire_recursive(&(lock))
#define __lock_try_acquire(lock) bf_libc_lock_try_acquire(&(lock))
#define __lock_try_acquire_recursive(lock) bf_libc_lock_try_acquire_recursive(&(lock))
#define __lock_release(lock) bf_libc_lock_release(&(lock))
#define __lock_release_recursive(lock) bf_libc_lock_release(&(lock))

#else /* ! BARRELFISH */

/* dummy lock routines for single-threaded aps */
typedef int _LOCK_T;
typedef int _LOCK_RECURSIVE_T;

#include <_ansi.h>

#define __LOCK_INIT(class,lock) static int lock = 0;
#define __LOCK_INIT_RECURSIVE(class,lock) static int lock = 0;
#define __lock_init(lock) (_CAST_VOID 0)
#define __lock_init_recursive(lock) (_CAST_VOID 0)
#define __lock_close(lock) (_CAST_VOID 0)
#define __lock_close_recursive(lock) (_CAST_VOID 0)
#define __lock_acquire(lock) (_CAST_VOID 0)
#define __lock_acquire_recursive(lock) (_CAST_VOID 0)
#define __lock_try_acquire(lock) (_CAST_VOID 0)
#define __lock_try_acquire_recursive(lock) (_CAST_VOID 0)
#define __lock_release(lock) (_CAST_VOID 0)
#define __lock_release_recursive(lock) (_CAST_VOID 0)

#endif

#endif /* __SYS_LOCK_H__ */
