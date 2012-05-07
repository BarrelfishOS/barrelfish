/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * Header file that contains all relevant data strcture needed
 * for interacting with EHCI controller hardware
 */

#ifndef __EHCI_DS_H
#define __EHCI_DS_H

#include <barrelfish/barrelfish.h>

#include <stdio.h>
#include <stdint.h>

// Mackerel interfacing
#include <dev/ehci_dev.h>

#define N_TRANSACTIONS 8
#define N_BUFF_PTR 4
#define N_QUEUE_BUFF 5          // Number of queue buffer (page 41)
#define N_OVERLAY_ARR 5



// Transaction related macros, used with queue elements

#define USB_OUT_TOKEN   0
#define USB_IN_TOKEN    1
#define USB_SETUP_TOKEN 2


/*
 * HC Devices from MACKEREL files
 */

ehci_t ehci_device;

// To elliminate gcc bit field alignment errors
#pragma pack(push,1)

/* Frame list element pointer */

typedef union h_ptr {
    uint32_t raw;
    struct {
        uint8_t t:1;            // Valid Invalid bit
        uint8_t typ:2;          // Type
        uint8_t ann:2;          // Don't care
        uintptr_t ptr:27;       // Frame pointer
    } __attribute__ ((packed)) str;
} __attribute__ ((packed)) h_ptr;

/* iTD transaction status and control list */
typedef union itd_cs_list {
    uint32_t raw;
    struct {
        uint16_t offset:12;
        uint8_t pg:3;
        uint8_t ioc:1;
        uint16_t length:12;
        uint8_t status:4;
    } __attribute__ ((packed)) str;
} __attribute__ ((packed)) itd_cs_list;


/* iTD buffer page pointer list */

// For page type zero
typedef union buff_ptr_page0 {
    uint32_t raw;
    struct {
        uint8_t dev_addr:7;
        uint8_t ann:1;
        uint8_t end_pt:4;
        uint32_t buff_ptr:20;
    } __attribute__ ((packed)) str;
} __attribute__ ((packed)) buff_ptr_page0;

// For page type one
typedef union buff_ptr_page1 {
    uint32_t raw;
    struct {
        uint16_t pkt_size:11;
        uint8_t dir:1;
        uint32_t buff_ptr:20;
    } __attribute__ ((packed)) str;
} __attribute__ ((packed)) buff_ptr_page1;

// For page type 2
typedef union buff_ptr_page2 {
    uint32_t raw;
    struct {
        uint8_t multi:2;
        uint16_t ann:10;
        uint32_t buff_ptr:20;
    } __attribute__ ((packed)) str;
} __attribute__ ((packed)) buff_ptr_page2;

// For page type 3-6

typedef union buff_ptr_page36 {
    uint32_t raw;
    struct {
        uint16_t ann:12;
        uint32_t buff_ptr:20;
    } __attribute__ ((packed)) str;
} __attribute__ ((packed)) buff_ptr_page36;


/* Section 3.3
 * iTD High speed Transfer descriptor
 * Total of 32X16 bytes.
 */

typedef struct iTD {
    union h_ptr next;           // 1 byte, ptr
    union itd_cs_list status_ctrl_list[N_TRANSACTIONS];
    // N_TRANSACTIONS bytes, transaction
    union buff_ptr_page0 buff_pg0;      // 1 byte, ptr0
    union buff_ptr_page1 buff_pg1;      // 1 byte, ptr1
    union buff_ptr_page2 buff_pg2;      // 1 byte, ptr2
    union buff_ptr_page36 buff_pg36[N_BUFF_PTR];        // N_BUFF_PTR bytes
} iTD;


/* Section 3.4 Split transaction isochronous transfer descriptor
 * siTD
 */

typedef union sitd_cap {
    uint32_t raw;
    struct {
        uint8_t dev_addr:7;
        uint8_t ann:1;
        uint8_t end_pt:4;
        uint8_t ann1:4;
        uint8_t hub_addr:7;
        uint8_t ann2:1;
        uint8_t port_num:7;
        uint8_t dir:1;
    } __attribute__ ((packed)) str;
} __attribute__ ((packed)) sitd_cap;

typedef union mframe_ctrl {
    uint32_t raw;
    struct {
        uint8_t s_mask:8;
        uint8_t c_mask:8;
        uint16_t ann:16;
    } __attribute__ ((packed)) str;
} __attribute__ ((packed)) mframe_ctrl;

typedef union sitd_state {
    uint32_t raw;
    struct {
        uint8_t status:8;
        uint8_t c_prog_mask:8;
        uint16_t total_bytes:10;
        uint8_t ann:4;
        uint8_t page:1;
        uint8_t ioc:1;
    } __attribute__ ((packed)) str;
} __attribute__ ((packed)) sitd_state;


typedef union sitd_buff_ptr_list0 {
    uint32_t raw;
    struct {
        uint16_t offset:12;
        uint32_t buff_ptr:20;
    } __attribute__ ((packed)) str;
} __attribute__ ((packed)) sitd_buff_ptr_list0;


typedef union sitd_buff_ptr_list1 {
    uint32_t raw;
    struct {
        uint8_t t_count:2;
        uint8_t tp:2;
        uint8_t ann:6;
        uint32_t buff_ptr;
    } __attribute__ ((packed)) str;
} __attribute__ ((packed)) sitd_buff_ptr_list1;

typedef union siTD_back_ptr {
    uint32_t raw;
    struct {
        uint8_t t:1;
        uint8_t ann:4;
        uint32_t back_ptr:27;
    } __attribute__ ((packed)) str;
} __attribute__ ((packed)) siTD_back_ptr;


typedef struct siTD {
    union h_ptr nxt_link_ptr;
    union sitd_cap cap;
    union mframe_ctrl mframe;
    union sitd_state transfer_state;
    union sitd_buff_ptr_list0 bptr_list0;
    union sitd_buff_ptr_list1 bptr_list1;
    union siTD_back_ptr bptr_list;
} __attribute__ ((packed)) siTD;



/*
 * Queue managment related data structure
 */

typedef union qTD_token {
    uint32_t raw;
    struct {
        uint8_t status:8;
        uint8_t pid:2;
        uint8_t cerr:2;
        uint8_t c_page:3;
        uint8_t ioc:1;
        uint16_t total_bytes:15;
        uint8_t dt:1;
    } __attribute__ ((packed)) str;
} __attribute__ ((packed)) qTD_token;

typedef union qTD_buff_list {
    uint32_t raw;
    struct {
        uint16_t offset:12;
        uint32_t buff_ptr:20;
    } __attribute__ ((packed)) str;
} __attribute__ ((packed)) qTD_buff_list;

typedef struct qTD {
    union h_ptr next_qTD;
    union h_ptr alt;
    union qTD_token q_token;
    union qTD_buff_list buff_list[N_QUEUE_BUFF];
    // XXX: One has offset, in rest offset is rsvd !!
} __attribute__ ((packed)) qTD;

/*
 * Queue head realted data structure
 */

typedef union endpt_char {
    uint32_t raw;
    struct {
        uint8_t dev_addr:7;
        uint8_t i:1;
        uint8_t endpt_num:4;
        uint8_t eps:2;
        uint8_t dtc:1;
        uint8_t head:1;
        uint16_t max_packet_length:11;
        uint8_t ctrl_endpt_flag:1;
        uint8_t nack_reload:4;
    } __attribute__ ((packed)) str;
} __attribute__ ((packed)) endpt_char;

typedef union endpt_cap {
    uint32_t raw;
    struct {
        uint8_t s_mask:8;
        uint8_t c_mask:8;
        uint16_t hub_addr:7;
        uint16_t port_num:7;
        uint8_t mult:2;
    } __attribute__ ((packed)) str;
} __attribute__ ((packed)) endpt_cap;


typedef struct q_head {
    union h_ptr next_qhead;
    union endpt_char ep_char;
    union endpt_cap ep_cap;
    union h_ptr current;
    union h_ptr next;
    union h_ptr alt;
    //XXX: This actually has NakCnt, so use cautiously, use RAW
    union qTD_token q_token;
    union qTD_buff_list buff_list[N_OVERLAY_ARR];
    //FIXME: remove this shortcut, expand it and use it properly
} __attribute__ ((packed)) q_head;


#pragma pack()

// Couple of printing routines used in debugging mode

void print_qhead(struct q_head *ptr);
void print_qTD(struct qTD *ptr);

#endif                          // __EHCI_DS_H
