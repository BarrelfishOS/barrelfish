/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * Contains memory management related definitions and 
 * functions. 
 *
 * Used and include in all subsystems. 
 */

#include <barrelfish/barrelfish.h>
#include <stdio.h>
#include <timer/timer.h>

#ifndef USB_MEM_H
#define USB_MEM_H


#define EHCI_MEM_TYPE_ITD  1
#define EHCI_MEM_TYPE_QH   2
#define EHCI_MEM_TYPE_QE   3
#define EHCI_MEM_TYPE_IO   4

#define EHCI_MEM_FREE      1    //0b11110111
#define EHCI_MEM_USED      0

#define USB_NEAR_EHCI  1
#define USB_NEAR_SELF  2
#define USB_DONT_CARE  0



/* 
 * To keep track of EHCI memory usage 
 */

typedef struct usb_page {
    struct capref frame;
    struct frame_identity frame_id;
    void *va;
    void *pa;
    int valid;
} usb_page;



typedef struct usb_mem {
    void *va;
    void *pa;
    int type;
    int free;
    int size;
    struct capref cap;
} usb_mem;


void usb_mem_init(void);

usb_mem malloc_iobuff(uint32_t sz, uint8_t flag);

usb_mem malloc_qh(void);
usb_mem malloc_64(void);

usb_mem malloc_qe(void);
usb_mem malloc_32(void);


usb_mem *malloc_qh_n(int n);
usb_mem *malloc_qe_n(int n);


void free_qh(usb_mem mem);
void free_64(usb_mem mem);

void free_qe(usb_mem mem);
void free_32(usb_mem mem);


void free_qh_n(usb_mem * mem, int n);
void free_qe_n(usb_mem * mem, int n);

void free_iobuff(usb_mem iobuff);

void print_qh_list(void);

void print_usb_mem(usb_mem mem);

void print_memory_stats(void);

void *map_cap(struct capref cap, uint32_t sz);

void set_ehci_core_id(uint64_t ehci_core);

#endif                          // USB_MEM_H
