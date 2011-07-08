/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * Contains various printing, clearing utilities 
 * used through out the USB system 
 */

#ifndef USB_UTILITY_H
#define USB_UTILITY_H

#include <barrelfish/barrelfish.h>
#include <stdio.h>


// Simple array clearing utility 
void clear_arr(void *arr, uint64_t sz);

// Simple char array printing utility
// Required where the string is not NULL terminated 
void print_str(char *str, int sz);

// Simple hex dumping utility
void print_uint8(uint8_t * str, int sz);

// Simple array copying utility
void copy_data(void *s, void *d, uint64_t max_sz);


#define HIGHER_ADD(x,y) ( (x) >> y )

//XXX: Might be a bad idea to hard code 4kB page size 
#define GET_PAGE(x)   (((x>>12)) << 12 )

#define GET_OFFSET(x) (x - GET_PAGE(x))

#endif                          // USB_UTILITY_H
