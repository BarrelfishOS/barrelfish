/**
 * \file
 * \brief Dictionary type
 */

/*
 * Copyright (c) 2008, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef DICTIONARY_H_
#define DICTIONARY_H_

#include <barrelfish/barrelfish.h>

typedef enum uint8_t {
    TYPE_STRING = 1,
    TYPE_WORD,
    TYPE_OPAQUE,
    TYPE_CAPABILITY,
} ENTRY_TYPE;

/**
 * \brief dictionary_t is the abstract type of
 *   datastructures that are able to store key/value pairs
 */
struct dictionary {
    int (*put_word)(struct dictionary*, char*, size_t, uintptr_t);
    ENTRY_TYPE (*get)(struct dictionary*, char*, size_t, void**);
    int (*size)(struct dictionary*);
    int (*remove)(struct dictionary*, char*, size_t);
};

#endif /*DICTIONARY_H_*/
