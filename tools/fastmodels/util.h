/*
 * Copyright (c) 2015,2016, ETH Zuerich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __UTIL_H
#define __UTIL_H

#include <string.h>

#define COVER(x, y) (((x) + ((y)-1)) / (y))
#define ROUNDUP(x, y) (COVER(x,y) * (y))
#define PAGE_4k (1<<12)
#define roundpage(x) COVER((x), PAGE_4k)

/* Copy a base+length string into a null-terminated string.  Destination
 * buffer must be large enough to hold the terminator i.e. n+1 characters. */
static inline void
ntstring(char *dest, const char *src, size_t len) {
    memcpy(dest, src, len);
    dest[len]= '\0';
}

#endif /* __UTIL_H */
