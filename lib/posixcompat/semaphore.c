/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <errno.h>
#include <semaphore.h>
#include <assert.h>
#include <barrelfish/barrelfish.h>
#include <string.h>
#include "posixcompat.h"

#include <if/nameservice_defs.h>
#include <if/nameservice_rpcclient_defs.h>

int sem_init(sem_t *sem, int pshared, unsigned int value)
{
    POSIXCOMPAT_DEBUG("sem_init(%p, %d, %u)\n", sem, pshared, value);

    memset(sem, 0, sizeof(sem_t));

    if(pshared != 0) {
        sem->pshared = 1;
        /* fprintf(stderr, "sem_init called with pshared != 0. Ignoring.\n"); */

		POSIXCOMPAT_DEBUG("%d: sem_init(%p, %d, %u)\n", disp_get_domain_id(), sem, pshared, value);

        struct nameservice_rpc_client *r = get_nameservice_rpc_client();
        assert(r != NULL);

        errval_t err, reterr;
        err = r->vtbl.sem_new(r, value, &sem->id, &reterr);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "sem_new");
        }
        if (err_is_fail(reterr)) {
            USER_PANIC_ERR(reterr, "sem_new reterr");
        }
    } else {
        sem->pshared = 0;
        thread_sem_init(&sem->thread_sem, value);
    }
    return 0;
}

int sem_destroy(sem_t *sem)
{
    POSIXCOMPAT_DEBUG("sem_destroy(%p)\n", sem);
    assert(!"NYI");
    // Nothing needed
    return 0;
}

int sem_wait(sem_t *sem)
{
  	POSIXCOMPAT_DEBUG("%d: sem_wait(%p, %u): %p\n%p\n%p\n", disp_get_domain_id(), sem, sem->id,
	 __builtin_return_address(0),
	 __builtin_return_address(1),
	 __builtin_return_address(2));

    if(!sem->pshared) {
        thread_sem_wait(&sem->thread_sem);
    } else {
        struct nameservice_rpc_client *r = get_nameservice_rpc_client();
        assert(r != NULL);

        errval_t err;
        err = r->vtbl.sem_wait(r, sem->id);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "sem_wait");
        }
    }

    return 0;
}

int sem_trywait(sem_t *sem)
{
	POSIXCOMPAT_DEBUG("%d: sem_trywait(%p, %u)\n", disp_get_domain_id(), sem, sem->id);

    if(!sem->pshared) {
        if(thread_sem_trywait(&sem->thread_sem)) {
            return 0;
        } else {
            errno = EAGAIN;
            return -1;
        }
    } else {
        struct nameservice_rpc_client *r = get_nameservice_rpc_client();
        assert(r != NULL);

        errval_t err;
        bool success;
        err = r->vtbl.sem_trywait(r, sem->id, &success);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "sem_wait");
        }

        if(success) {
	  		POSIXCOMPAT_DEBUG("%d: sem_trywait(%p, %u) success!\n", disp_get_domain_id(), sem, sem->id);
            return 0;
        } else {
	  		POSIXCOMPAT_DEBUG("%d: sem_trywait(%p, %u) no success\n", disp_get_domain_id(), sem, sem->id);
            errno = EAGAIN;
            return -1;
        }
    }

    assert(!"Should not reach here");
}

int sem_post(sem_t *sem)
{
  	POSIXCOMPAT_DEBUG("%d: sem_post(%p, %u): %p %p %p\n", disp_get_domain_id(), sem, sem->id,
	 __builtin_return_address(0),
	 __builtin_return_address(1),
	 __builtin_return_address(2));

    if(!sem->pshared) {
        thread_sem_post(&sem->thread_sem);
    } else {
        struct nameservice_rpc_client *r = get_nameservice_rpc_client();
        assert(r != NULL);

        errval_t err;
        err = r->vtbl.sem_post(r, sem->id);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "sem_post");
        }
    }

    return 0;
}
