/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * toggle_state.c 
 *
 * Contains logic for shared data management specially 
 * for toggle state of an endpoint. 
 */


#include <usb/shared_state.h>
#include <usb/mem/usb_mem.h>
#include <ehci_debug.h>


#include "toggle_state.h"

usb_shared_state_t *dev_toggle_arr = NULL;

/*
 * \brief Maps the cap that contains the page
 *        which has shared data.
 *\param cap Capability containing shared data
 *\param sz  Size of page 
 */
void map_dev_arr(struct capref cap, uint32_t sz)
{
    static bool done = false;
    if (!done) {
        dev_toggle_arr = map_cap(cap, sz);
        done = true;
        dprintf("Device toggle state arr successfully mapped\n");
    }
}

/*
 * \brief Retuns the address of mapped shared page 
 */

usb_shared_state_t *get_dev_arr_add(void)
{
    return dev_toggle_arr;
}
