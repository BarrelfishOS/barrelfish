/*
 * Copyright (c) 2013, 2014, University of Washington.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <pthread.h>
#include <assert.h>
#include <barrelfish/barrelfish.h>
#include <errno.h>
#include <string.h>
#include <signal.h>

#include <posixcompat.h> // for pthread_placement stuff

typedef void (*destructor_fn_t)(void *);
typedef void *(*start_fn_t)(void *);

struct   pthread_mutex_attr
{
  int pshared;
  int kind;
  int robustness;
};


struct pthread_mutex {
    struct thread_mutex mutex;
    int locked;
    struct pthread_mutex_attr attrs;
};




struct pthread_cond {
    struct thread_cond cond;
};

#define PTHREADS_RWLOCK_MAGIC 0xdeadbeef

struct pthread_rwlock
{
  pthread_mutex_t mtxExclusiveAccess;
  pthread_mutex_t mtxSharedAccessCompleted;
  pthread_cond_t cndSharedAccessCompleted;
  int nSharedAccessCount;
  int nExclusiveAccessCount;
  int nCompletedSharedAccessCount;
  int nMagic;
};


struct pthread {
    struct thread *thread;
    int core; //< for spanned domains core on which thread is running
    const void *keys[PTHREAD_KEYS_MAX];
    start_fn_t start_fn;
    void *arg;
    void *retval;
};

struct pthread_attr {
    int stacksize;
};

static pthread_key_t key_index = 0;
static struct thread_mutex key_mutex = THREAD_MUTEX_INITIALIZER;
static destructor_fn_t destructors[PTHREAD_KEYS_MAX];

static int start_pthread(void *arg)
{
    struct pthread *myself = arg;

    // Initialize TLS
    thread_set_tls_key(0, myself);

    // Run the thread
    myself->retval = myself->start_fn(myself->arg);

    // Call all key destructors
    for(pthread_key_t i = 0; i < key_index; i++) {
        if ((destructors[i] != NULL) && (myself->keys[i] != NULL)) {
            void *value = (void *) myself->keys[i];
            myself->keys[i] = NULL;
            destructors[i](value);
        }
    }

    // 'myself' data structure is freed when joined with this thread
    return 0;
}

/*
 * Optional pthread placement policy for spanned domains
 */
static pthread_placement_fn pthread_placement = NULL;
errval_t posixcompat_pthread_set_placement_fn(pthread_placement_fn fn)
{
    pthread_placement = fn;
    return SYS_ERR_OK;
}

int pthread_create(pthread_t *pthread, const pthread_attr_t *attr,
                   void *(*start_routine) (void *), void *arg)
{
    size_t stacksize = THREADS_DEFAULT_STACK_BYTES;

    if(attr != NULL) {
        stacksize = (*attr)->stacksize;
    }

    *pthread = malloc(sizeof(struct pthread));
    assert(*pthread != NULL);
    memset(*pthread, 0, sizeof(struct pthread));

    // XXX: attributes are ignored.
    (*pthread)->start_fn = start_routine;
    (*pthread)->arg = arg;

    // Start the thread
    (*pthread)->core = disp_get_core_id();
    if (pthread_placement) {
        (*pthread)->core = pthread_placement(PTHREAD_ACTION_CREATE, 0);
    }
    struct thread *nt;
    errval_t err = domain_thread_create_on_varstack(
                     (*pthread)->core, start_pthread, *pthread, stacksize, &nt);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "pthread_create");
        return 1;
    }

    (*pthread)->thread = nt;
    debug_printf("%s: %p -> %"PRIuPTR"\n", __FUNCTION__, *pthread,
            thread_get_id((*pthread)->thread));
    return 0;
}

pthread_t pthread_self(void)
{
    pthread_t self = thread_get_tls_key(0);

    // If NULL, we're the first thread, not created via pthreads.
    // Create a pthread structure.
    if(self == NULL) {
        struct pthread *myself = malloc(sizeof(struct pthread));
        assert(myself != NULL);
        memset(myself, 0, sizeof(struct pthread));
        myself->thread = thread_self();
        thread_set_tls_key(0, myself);
        self = myself;
    }

    return self;
}

void *pthread_getspecific(pthread_key_t key)
{
    if(key >= PTHREAD_KEYS_MAX) {
        return NULL;
    }

    return (void *)pthread_self()->keys[key];
}

int pthread_setspecific(pthread_key_t key, const void *val)
{
    if(key >= PTHREAD_KEYS_MAX) {
        return EINVAL;
    }

    pthread_self()->keys[key] = val;
    return 0;
}

int pthread_attr_init(pthread_attr_t *attr)
{
    *attr = malloc(sizeof(struct pthread_attr));
    (*attr)->stacksize = THREADS_DEFAULT_STACK_BYTES;
    // No attributes
    return 0;
}

static struct thread_mutex mutex_mutex = THREAD_MUTEX_INITIALIZER;

int pthread_mutex_init(pthread_mutex_t *mutex,
                       const pthread_mutexattr_t *attr)
{
    // XXX: Attributes ignored.
    *mutex = malloc(sizeof(struct pthread_mutex));
    if(*mutex == NULL) {
        return -1;
    }

    thread_mutex_init(&(*mutex)->mutex);
    (*mutex)->locked = 0;
    if (attr && *attr) {
        debug_printf("kind = %u\n", (*attr)->kind);
        memcpy(&(*mutex)->attrs, *attr, sizeof(struct pthread_mutex_attr));
    } else {
        (*mutex)->attrs.kind = PTHREAD_MUTEX_NORMAL;
        (*mutex)->attrs.robustness = 0;
        (*mutex)->attrs.pshared = PTHREAD_PROCESS_PRIVATE;
    }
    return  0;
}

int pthread_mutex_destroy(pthread_mutex_t *mutex)
{
    if(*mutex != PTHREAD_MUTEX_INITIALIZER) {
        free(*mutex);
    }

    return 0;
}

int pthread_mutex_lock(pthread_mutex_t *mutex)
{
    thread_mutex_lock(&mutex_mutex);

    if(*mutex == PTHREAD_MUTEX_INITIALIZER) {
        pthread_mutex_init(mutex, NULL);
    }

    (*mutex)->locked++;
    thread_mutex_unlock(&mutex_mutex);
    if ((*mutex)->attrs.kind == PTHREAD_MUTEX_RECURSIVE) {
        thread_mutex_lock_nested(&(*mutex)->mutex);
    } else {
        thread_mutex_lock(&(*mutex)->mutex);
    }
    return 0;
}

int pthread_mutex_unlock(pthread_mutex_t *mutex)
{
    thread_mutex_lock(&mutex_mutex);

    if(*mutex == PTHREAD_MUTEX_INITIALIZER) {
        pthread_mutex_init(mutex, NULL);
    }

    if((*mutex)->locked == 0) {
        thread_mutex_unlock(&mutex_mutex);
        return 0;
    }

    (*mutex)->locked--;
    thread_mutex_unlock(&mutex_mutex);
    thread_mutex_unlock(&(*mutex)->mutex);
    return 0;
}

int pthread_mutex_trylock(pthread_mutex_t *mutex)
{
    thread_mutex_lock(&mutex_mutex);

    if(*mutex == PTHREAD_MUTEX_INITIALIZER) {
        pthread_mutex_init(mutex, NULL);
    }

    thread_mutex_unlock(&mutex_mutex);

    int retval = (thread_mutex_trylock(&(*mutex)->mutex) ? 0 : EBUSY);

    if(retval != EBUSY) {
        thread_mutex_lock(&mutex_mutex);
        (*mutex)->locked++;
        thread_mutex_unlock(&mutex_mutex);
    }

    return retval;
}

int pthread_cond_init(pthread_cond_t *cond,
			const pthread_condattr_t *attr)
{
    *cond = malloc(sizeof(struct pthread_cond));
    if(*cond == NULL) {
        return -1;
    }

    thread_cond_init(&(*cond)->cond);
    return 0;
}

int pthread_cond_signal(pthread_cond_t *cond)
{
    thread_mutex_lock(&mutex_mutex);
    if(*cond == PTHREAD_COND_INITIALIZER) {
        pthread_cond_init(cond, NULL);
    }
    thread_mutex_unlock(&mutex_mutex);

    thread_cond_signal(&(*cond)->cond);
    return 0;
}

int pthread_cond_timedwait(pthread_cond_t *cond,
                           pthread_mutex_t *mutex,
                           const struct timespec *timeout)
{
    thread_mutex_lock(&mutex_mutex);
    if(*cond == PTHREAD_COND_INITIALIZER) {
        pthread_cond_init(cond, NULL);
    }
    if(*mutex == PTHREAD_MUTEX_INITIALIZER) {
        pthread_mutex_init(mutex, NULL);
    }
    thread_mutex_unlock(&mutex_mutex);

    assert(!"NYI");
    return -1;
}

int pthread_cond_wait(pthread_cond_t *cond, pthread_mutex_t *mutex)
{
    thread_mutex_lock(&mutex_mutex);
    if(*cond == PTHREAD_COND_INITIALIZER) {
        pthread_cond_init(cond, NULL);
    }
    if(*mutex == PTHREAD_MUTEX_INITIALIZER) {
        pthread_mutex_init(mutex, NULL);
    }
    thread_mutex_unlock(&mutex_mutex);

    thread_cond_wait(&(*cond)->cond, &(*mutex)->mutex);
    return 0;
}

int pthread_cond_broadcast(pthread_cond_t *cond)
{
    thread_mutex_lock(&mutex_mutex);

    if(*cond == PTHREAD_COND_INITIALIZER) {
         pthread_cond_init(cond, NULL);
    }
    thread_mutex_unlock(&mutex_mutex);

    thread_cond_broadcast(&(*cond)->cond);

    return 0;
}

int pthread_cond_destroy(pthread_cond_t *cond)
{
    if (cond != NULL) {
        free(*cond);
    }
    return 0;
}

int pthread_join(pthread_t thread, void **retval)
{
    debug_printf("%s: %p\n", __FUNCTION__, thread);
    errval_t err = domain_thread_join(thread->thread, NULL);
    assert(err_is_ok(err));

    if (pthread_placement) {
        pthread_placement(PTHREAD_ACTION_DESTROY, thread->core);
    }

    if (retval != NULL) {
        *retval = thread->retval;
    }
    free(thread);
    return 0;
}

int pthread_key_create(pthread_key_t *key,
                       void (*callback) (void *))
{
    int retval = 0;

    thread_mutex_lock(&key_mutex);

    if(key_index == PTHREAD_KEYS_MAX) {
        retval = EAGAIN;
        goto out;
    }

    *key = key_index;
    destructors[key_index] = callback;
    key_index++;

 out:
    thread_mutex_unlock(&key_mutex);
    return retval;
}

int pthread_key_delete(pthread_key_t key)
{
    thread_mutex_lock(&key_mutex);

    int result = EINVAL;
    if ((key < PTHREAD_KEYS_MAX) && (destructors[key] != NULL)) {
        destructors[key] = NULL;
        result = 0;
    }

    thread_mutex_unlock(&key_mutex);
    return result;
}

int pthread_mutexattr_init(pthread_mutexattr_t *attr)
{
    int result = 0;
    pthread_mutexattr_t ma;

    ma = (pthread_mutexattr_t) calloc (1, sizeof (*ma));

    if (ma == NULL) {
        result = ENOMEM;
    }
    else {
        ma->pshared = PTHREAD_PROCESS_PRIVATE;
        ma->kind = PTHREAD_MUTEX_DEFAULT;
    }

    *attr = ma;

    return result;
}

int pthread_mutexattr_destroy(pthread_mutexattr_t *attr)
{
    int result = 0;

    if (attr == NULL || *attr == NULL) {
        result = EINVAL;
    } else {
        pthread_mutexattr_t ma = *attr;

        *attr = NULL;
        free (ma);
    }

    return result;
}

int pthread_mutexattr_getpshared(const pthread_mutexattr_t *attr,
                                 int *pshared)
{
    int result;

    if ((attr != NULL && *attr != NULL) && (pshared != NULL)) {
        *pshared = (*attr)->pshared;
        result = 0;
    } else {
        result = EINVAL;
    }

    return result;
}

int pthread_mutexattr_gettype(pthread_mutexattr_t *attr, int *type)
{
    int result = 0;

    if (attr != NULL && *attr != NULL && type != NULL) {
        *type = (*attr)->kind;
    } else {
        result = EINVAL;
    }

    return result;
}

int pthread_mutexattr_settype(pthread_mutexattr_t *attr, int type)
{
    int result = 0;

    if ((attr != NULL && *attr != NULL)) {
        switch (type) {
            case PTHREAD_MUTEX_NORMAL:
            case PTHREAD_MUTEX_RECURSIVE:
            case PTHREAD_MUTEX_ERRORCHECK:
                (*attr)->kind = type;
                break;
            default:
                result = EINVAL;
                break;
        }
    } else {
        result = EINVAL;
    }

    return result;
}

int pthread_mutexattr_setpshared(pthread_mutexattr_t *attr, int pshared)
{
    int result;

    if ((attr != NULL && *attr != NULL)
         && ((pshared == PTHREAD_PROCESS_SHARED)
                  || (pshared == PTHREAD_PROCESS_PRIVATE))) {

        if (pshared == PTHREAD_PROCESS_SHARED) {
#if !defined( _POSIX_THREAD_PROCESS_SHARED )
            result = ENOSYS;
            pshared = PTHREAD_PROCESS_PRIVATE;
#else
            result = 0;
#endif /* _POSIX_THREAD_PROCESS_SHARED */
        } else {
            result = 0;
        }

        (*attr)->pshared = pshared;
    } else {
        result = EINVAL;
    }

    return (result);
}



int pthread_equal(pthread_t pt1, pthread_t pt2)
{
    if (pt1 == NULL && pt2 == NULL) {
        return 1;
    }
    return pt1->thread == pt2->thread;
}

int pthread_rwlock_init(pthread_rwlock_t *rwlock,
            const pthread_rwlockattr_t *attr)
{
    pthread_rwlock_t rwl;

    rwl = calloc(1, sizeof(struct pthread_rwlock));
    if (rwl == NULL) {
        return ENOMEM;
    }

    rwl->nMagic = PTHREADS_RWLOCK_MAGIC;
    rwl->mtxExclusiveAccess = PTHREAD_MUTEX_INITIALIZER;
    rwl->mtxSharedAccessCompleted = PTHREAD_MUTEX_INITIALIZER;

    pthread_cond_init (&rwl->cndSharedAccessCompleted, NULL);
    *rwlock = rwl;

    return 0;
}

int pthread_rwlock_unlock(pthread_rwlock_t *rwlock)
{
    int result, result1;
    pthread_rwlock_t rwl;

    if (rwlock == NULL || *rwlock == NULL) {
        return (EINVAL);
    }

    if (*rwlock == PTHREAD_RWLOCK_INITIALIZER) {
        result = pthread_rwlock_init(rwlock, NULL);
        if (result) {
            return result;
        }
    }

    rwl = *rwlock;

    if (rwl->nMagic != PTHREADS_RWLOCK_MAGIC) {
        return EINVAL;
    }

    if (rwl->nExclusiveAccessCount == 0) {
        if ((result = pthread_mutex_lock (&(rwl->mtxSharedAccessCompleted))) != 0) {
            return result;
        }

        if (++rwl->nCompletedSharedAccessCount == 0) {
            result = pthread_cond_signal (&(rwl->cndSharedAccessCompleted));
        }

        result1 = pthread_mutex_unlock (&(rwl->mtxSharedAccessCompleted));
    } else {
        rwl->nExclusiveAccessCount--;
        result = pthread_mutex_unlock (&(rwl->mtxSharedAccessCompleted));
        result1 = pthread_mutex_unlock (&(rwl->mtxExclusiveAccess));
    }

    return ((result != 0) ? result : result1);
}

int pthread_rwlock_wrlock(pthread_rwlock_t *rwlock)
{
    int result;
    pthread_rwlock_t rwl;

    if (rwlock == NULL || *rwlock == NULL) {
        return (EINVAL);
    }

    if (*rwlock == PTHREAD_RWLOCK_INITIALIZER) {
        result = pthread_rwlock_init(rwlock, NULL);
        if (result) {
            return result;
        }
    }

    rwl = *rwlock;

    if (rwl->nMagic != PTHREADS_RWLOCK_MAGIC) {
        return EINVAL;
    }

    if ((result = pthread_mutex_lock (&(rwl->mtxExclusiveAccess))) != 0) {
        return result;
    }

    if ((result = pthread_mutex_lock (&(rwl->mtxSharedAccessCompleted))) != 0) {
        (void) pthread_mutex_unlock (&(rwl->mtxExclusiveAccess));
        return result;
    }

    if (rwl->nExclusiveAccessCount == 0) {
        if (rwl->nCompletedSharedAccessCount > 0) {
            rwl->nSharedAccessCount -= rwl->nCompletedSharedAccessCount;
            rwl->nCompletedSharedAccessCount = 0;
        }

        if (rwl->nSharedAccessCount > 0) {
            rwl->nCompletedSharedAccessCount = -rwl->nSharedAccessCount;

            /*
             * This routine may be a cancelation point
             * according to POSIX 1003.1j section 18.1.2.
             */
           // pthread_cleanup_push (ptw32_rwlock_cancelwrwait, (void *) rwl);

            do {
                result = pthread_cond_wait (&(rwl->cndSharedAccessCompleted),
                                            &(rwl->mtxSharedAccessCompleted));
            } while (result == 0 && rwl->nCompletedSharedAccessCount < 0);

            //pthread_cleanup_pop ((result != 0) ? 1 : 0);

            if (result == 0) {
                rwl->nSharedAccessCount = 0;
            }
        }
    }

    if (result == 0) {
        rwl->nExclusiveAccessCount++;
    }

    return result;
}

int pthread_rwlock_rdlock(pthread_rwlock_t *rwlock)
{
  int result;
  pthread_rwlock_t rwl;

  if (rwlock == NULL || *rwlock == NULL) {
      return EINVAL;
  }

  /*
   * We do a quick check to see if we need to do more work
   * to initialise a static rwlock. We check
   * again inside the guarded section of ptw32_rwlock_check_need_init()
   * to avoid race conditions.
   */
   if (*rwlock == PTHREAD_RWLOCK_INITIALIZER) {
        result = pthread_rwlock_init(rwlock, NULL);
        if (result) {
            return result;
        }
    }

  rwl = *rwlock;

  if (rwl->nMagic != PTHREADS_RWLOCK_MAGIC) {
      return EINVAL;
  }

  if ((result = pthread_mutex_lock (&(rwl->mtxExclusiveAccess))) != 0) {
      return result;
  }

  if (++rwl->nSharedAccessCount == 0xFFFFFFFF) {
      if ((result = pthread_mutex_lock (&(rwl->mtxSharedAccessCompleted))) != 0) {
          (void) pthread_mutex_unlock (&(rwl->mtxExclusiveAccess));
          return result;
      }

      rwl->nSharedAccessCount -= rwl->nCompletedSharedAccessCount;
      rwl->nCompletedSharedAccessCount = 0;

      if ((result = pthread_mutex_unlock (&(rwl->mtxSharedAccessCompleted))) != 0) {
          (void) pthread_mutex_unlock (&(rwl->mtxExclusiveAccess));
          return result;
      }
  }

  return (pthread_mutex_unlock (&(rwl->mtxExclusiveAccess)));
}


int _pthread_once(pthread_once_t *ctrl, void (*init) (void))
{
    if (ctrl == NULL || init == NULL) {
        return EINVAL;
    }
    thread_once(ctrl, init);
    return 0;
}

int pthread_setcancelstate(int state, int *oldstate)
{
    // XXX: Not supported
    if(oldstate != NULL) {
        *oldstate = PTHREAD_CANCEL_ENABLE;
    }
    return 0;
}

int pthread_setcanceltype(int type, int *oldtype)
{
    // XXX: Not supported
    if(oldtype != NULL) {
        *oldtype = PTHREAD_CANCEL_DEFERRED;
    }
    return 0;
}

int pthread_sigmask(int how, const sigset_t *set, sigset_t *oldset)
{
    return sigprocmask(how, set, oldset);
}

int pthread_attr_getstacksize(const pthread_attr_t *attr, size_t *stacksize)
{
    *stacksize = (*attr)->stacksize;
    return 0;
}

int pthread_attr_setstacksize(pthread_attr_t *attr, size_t stacksize)
{
    (*attr)->stacksize = stacksize;
    return 0;
}

int pthread_cancel(pthread_t thread)
{
    assert(!"NYI");
    return -1;
}
