/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


/*
 * periodic_queue.c: contains logic for periodic queue managment
 *                   which in NOT YET IMPLEMENTED
 */

#include <barrelfish/barrelfish.h>
#include <usb/ehci/periodic_queue.h>


/*
 *
 * XXX: FIXME: NYI -- periodic list management
 */

void enable_periodic_schedule(void)
{
    /*if(check_already_enabled)
       {
       return ;
       }
       else
       {
       check for enable == status 
       enable_periodic_schedule;
       while(status ! = status); // perhaps can do sleep here ?
       }
     */
    return;
}

void disable_periodic_schedule(void)
{
    // 4.6 check for active split transaction 
    // remove from the queue first 
}

void setup_periodic_queue(void)
{
    // allocate_frame;
    // pass physical address to ehci, PERIODICLISTBASE register
    // ensure that ehci is in halted state before writing FRINDEX
    // init_iTD_to_all_slots; but iTD is for isochronous ? interrupts are managed by queue heads ?
    // mark_them_as_1 T-bit set to 1, being unused (1024 X 4 = 4K = 1 Frame) ? continious frame allocation
    // XXX: Not, because in the list only physical address is save 
    // so, 1024 iTD will require 1024 X 64 / 1024 * 4 = 16 frames (assuming the 4K page size, but has to be determined 
    // on run time). So allocate 16 frames, init them with iTD and set the T-Bit as 1 and with Disbaled queue managment   

}

void init_iTD_elem(void *start_vaddr, size_t sz)
{
    // Get how many frames 
    // allocate a iTD
    // set up the frames in iTD and multiplier and otherthings after checking the device capabilites 
    // Gotta: uframe 6 frame boundary crossing ?? page 70 
    // return it
    // if more than one is required then retern them concatenated ?
}

void add_iTD(iTD x)
{
    // Check caching status 
    // find location 
    // insert iTD
}

void clean_and_exit(void)
{
    // Cancel the queue 
    // Do check caching ...(perhaps first disable and halt the controller)
    // Free data structs
}

int remove_queue_head(uint64_t identifier)
{
    // if disabled, then just remove
    // otherwise, 
    // if active, then do not remove 
    // deactivate all queueheads 
    // check for H bit ...act accordingly...mark which will ne next H
    // ping the doorbell
    // wait for interrrupt (function call back, who will ack ?) and then remove and free 

    return 0;
}
