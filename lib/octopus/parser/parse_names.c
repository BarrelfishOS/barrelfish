/**
 * \file
 * \brief Parse function for format returned by get_names.
 */

/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */
#include <stdlib.h>
#include <stdio.h>

#include <barrelfish/barrelfish.h>
#include <octopus/getset.h>

#include "strnatcmp.h"

static char* mystrdup(char* data)
{

    char *p = malloc(strlen(data) + 1);
    if (p == NULL) {
        return NULL;
    }

    strcpy(p, data);
    return p;
}

static int cmpstringp(const void *p1, const void *p2)
{
    return strnatcmp(*(char * const *) p1, *(char * const *) p2);
}

/**
 * \brief Parses output from get_names call and stores it in an array.
 *
 * \note Use oct_free_names() to free names array.
 *
 * \param[in] input Comma separated string of names
 * \param[out] names Array of strings containing all names
 * \param[out] len Size of array.
 *
 * \retval LIB_ERR_MALLOC_FAIL
 * \retval SYS_ERR_OK
 */
errval_t oct_parse_names(char* input, char*** names, size_t* len)
{
    errval_t err = SYS_ERR_OK;

    char *p = mystrdup(input);
    if (p == NULL) {
        err = LIB_ERR_MALLOC_FAIL;
        goto out;
    }

    // first get the number of elements
    char* saveptr = NULL;
    size_t i;
    char* tok = p; // just make sure it's non-null
    char* first = p;
    for (i = 0; tok != NULL; i++, first = NULL) {
        tok = strtok(first, ",");
    }
    assert(p != NULL);
    free(p);
    p = NULL;
    *len = --i;

    *names = malloc(sizeof(char*) * i);
    if (*names == NULL) {
        *len = 0;
        err = LIB_ERR_MALLOC_FAIL;
        goto out;
    }
    memset(*names, 0, sizeof(char*) * i);

    // now get the actual elements
    saveptr = NULL;
    tok = input; // just make sure it's non-null
    first = input;
    for (i = 0; tok != NULL; i++, first = NULL) {
        tok = strtok(first, ", ");
        if (tok != NULL) {
            (*names)[i] = mystrdup(tok);
            if ((*names)[i] == NULL) {
                oct_free_names(*names, i);
                *names = NULL;
                *len = 0;
                err = LIB_ERR_MALLOC_FAIL;
                goto out;
            }
        } else {
            break;
        }
    }
    qsort(*names, *len, sizeof(char*), cmpstringp);

out:
    return err;
}

/**
 * \brief Helper function to free an array of strings.
 *
 * Frees all entries of the array and the array itself.
 *
 * \param names Non-null array of strings.
 * \param len Size of the names array

 * \see oct_get_names
 */
void oct_free_names(char** names, size_t len)
{
    //assert(names != NULL);
    for (size_t i = 0; i < len; i++) {
        free(names[i]);
    }

    free(names);
}