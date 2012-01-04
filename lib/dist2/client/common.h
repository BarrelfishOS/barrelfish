/**
 * \file
 * \brief Contains common functions/macros/defines used throughout
 * the dist2 client library.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef DIST2_COMMON_H_
#define DIST2_COMMON_H_

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#include <barrelfish/barrelfish.h>
#include <if/dist2_defs.h>
#include <if/dist2_rpcclient_defs.h>

struct dist2_rpc_client* get_dist_rpc_client(void);
struct dist2_binding* get_dist_event_binding(void);

#define LOCK_DIST_BINDING(cl)
#define UNLOCK_DIST_BINDING(cl)

#define MAX_RECORD_LENGTH (5*1024)

static inline errval_t allocate_string(char *object, va_list args,
        size_t *length, char **buf)
{
    *length = vsnprintf(NULL, 0, object, args);

    if (*length > MAX_RECORD_LENGTH) {
        return DIST2_ERR_RECORD_SIZE;
    }

    *buf = malloc((*length) + 1); // include \0
    if (buf == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    return SYS_ERR_OK;
}

/**
 * \brief Thread-safe variant of strtok as provided by POSIX
 *
 * \see http://linux.die.net/man/3/strtok
 *
 * \param str String to tokenize.
 * \param delim Tokenize by delimiter.
 * \param saveptr Used by strtok_r to store state.
 * \return Tokenized strings.
 */
static inline char* strtok_r(char *s, const char *delim, char **saveptr)
{
    if (s == NULL)
        s = *saveptr; /* recommence just after last token */
    s += strspn(s, delim); /* skip leading delimeters */
    if (*s == '\0')
        return NULL;
    *saveptr = s + strcspn(s, delim); /* remember end of token string */
    if ((**saveptr) != '\0')
        *(*(saveptr))++ = '\0'; /* terminate token string */
    return s;
}

#endif /* DIST2_COMMON_H_ */
