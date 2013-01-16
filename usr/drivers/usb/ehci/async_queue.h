/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * Contains functions and wrapper structs
 * used in transaction queue managment of
 * asynchronous requests.
 */

#ifndef ASYNC_QUEUE_H
#define ASYNC_QUEUE_H

#include <barrelfish/barrelfish.h>
#include <usb/mem/usb_mem.h>
#include "ehci.h"

#include <if/ehci_defs.h>

// Wrapper structs

/*
 * Wrapper for hardware structs
 * These structs act as wrapper for the core EHCI structs.
 * These are used to keep track of a request and maintain
 * the async list properly.
 */
typedef struct qhead_wrapper_t {
    struct q_head *qh;
    struct qhead_wrapper_t *next;
    struct qhead_wrapper_t *prev;

    struct ehci_service_response *rsp;
    uint64_t len;
    uint8_t req_type;

    struct qTD_wrapper_t *qe_last;
    struct qTD_wrapper_t *qe_next;
    usb_mem mem;
    //struct thread_sem *wait_flag;
    uint8_t status_flag;
    uint8_t type;
    uint32_t dbug;
    int n;
    uint8_t dev;
    uint8_t ep_num;
    uint8_t dir;
    bool toggle;
    uint32_t packet_sz;

} qhead_wrapper_t;


/*
 * Wrapper for queue elements
 */

typedef struct qTD_wrapper_t {
    struct qTD *q_elem;
    struct qTD_wrapper_t *next;
    struct qTD_wrapper_t *prev;
    struct qhead_wrapper_t *qhw;
    usb_mem mem;
} qTD_wrapper_t;



#define REQ_TYPE_DCTRL 0x0
#define REQ_TYPE_CTRL  0x1
#define REQ_TYPE_BULK  0x2


#define FLAG_ACTIVE          0x1
#define FLAG_MARK_REMOVE     0x2
#define FLAG_REMOVED         0x4

#define NODE_TYPE_HEAD       0x1
#define NODE_TYPE_NONE       0x2

#define WAIT      0x01
#define RELEASE   0x00

#define IS_HEAD(x) ((x) & NODE_TYPE_HEAD)
#define IS_NONE(x) ((x>>1) & NODE_TYPE_NONE)

void async_queue_init(void);

void enable_async_schedule(ehci_op_t dev, char *dbug);

void disable_async_schedule(ehci_op_t dev, char *dbug);

void internal_queue_req(qhead_wrapper_t * node);

void notify_scan(void);

void notify_remove(void);

#endif                          // ASYNC_QUEUE_H
