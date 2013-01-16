/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


#ifndef USB_ERROR_CODES_H
#define USB_ERROR_CODES_H

#include <barrelfish/barrelfish.h>

#include "ehci_status.h"


#define REQ_FINISHED    (0)
#define REQ_ACTIVE      (-1)
#define REQ_HALTED      (-2)
#define REQ_DBUFF_ERR   (-3)
#define REQ_BABBLE      (-4)
#define REQ_TX_ERR      (-5)
#define REQ_MISS        (-6)
#define REQ_SPLITX      (-7)
#define REQ_PERR        (-8)
#define DEV_REMOVED     (-9)

/* 
 * This function basically maps the request status to different request 
 * error stauts. So that client driver can know what happened. 
 *
 * This mapping is required because USB status are > 0. Which can not 
 * be returned to client driver. For any transaction 
 *
 * return > 0   --> All went OK. 'return_val' bytes of data is avaiable 
 * return == 0  --> All went OK. No data was expected back.
 * return < 0   --> Something went wrong and above defined negative error codes tell what.
 */

// XXX: This takes uint8_t type. Must *NOT* be used with above defined codes
// which are negatives.
int switch_error(uint8_t type);

/* 
 * Bigger interface for same, used by ehci_core, which takes three 
 * parameters. A request can fail on any stage of these three. 
 */
int get_return_status(uint8_t setup, uint8_t data, uint8_t status);


int switch_error(uint8_t type)
{
    switch (type) {
        // This case 1 should never happen, until due to 
        // some strange timing discrepancies 

    case USB_TRAN_STATUS_FINISHED:
        return REQ_FINISHED;

    case USB_TRAN_STATUS_ACTIVE:
        return REQ_ACTIVE;

    case USB_TRAN_STATUS_HALTED:
        return REQ_HALTED;

    case USB_TRAN_STATUS_DBUFF_ERR:
        return REQ_DBUFF_ERR;

    case USB_TRAN_STATUS_BABBLE:
        return REQ_BABBLE;

    case USB_TRAN_STATUS_TX_ERR:
        return REQ_TX_ERR;

    case USB_TRAN_STATUS_MISS:
        return REQ_MISS;

    case USB_TRAN_STATUS_SPLITX:
        return REQ_SPLITX;

    case USB_TRAN_STATUS_PERR:
        return REQ_PERR;
    }

    // Multiple error in a transaction  
    return -101;
}





int get_return_status(uint8_t setup, uint8_t data, uint8_t status)
{
    if (setup == 0) {
        if (data == 0) {
            if (status == 0) {
                return 0;       // All good 
            } else
                return switch_error(status);
        } else
            return switch_error(data);

    } else
        return switch_error(setup);

}


#endif                          // USB_ERROR_CODES_H
