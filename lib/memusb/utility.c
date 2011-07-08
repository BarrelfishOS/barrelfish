/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <usb/utility.h>

// Simple array clearing utility 
void clear_arr(void *arr, uint64_t sz)
{
    uint8_t *ptr = (uint8_t *) arr;
    for (uint64_t i = 0; i < sz; i++)
        ptr[i] = '\0';
}

// Simple char array printing utility
// Required where the string is not NULL terminated 
void print_str(char *str, int sz)
{
    int i;
    for (i = 0; i < sz; i++)
        printf("%c", str[i]);
}

void print_uint8(uint8_t * buff, int sz)
{
    int i;
    for (i = 0; i < sz; i++)
        printf(" [[%d]: %x]", i, buff[i]);
}

void copy_data(void *s, void *d, uint64_t max_sz)
{
    uint8_t *src = (uint8_t *) s;
    uint8_t *dest = (uint8_t *) d;
    uint64_t sz = max_sz, idx;

    for (idx = 0; idx < sz; idx++) {
        *(dest + idx) = *(src + idx);
    }
}
