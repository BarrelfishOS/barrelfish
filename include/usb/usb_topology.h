/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * Contains defintions and structs used in 
 * internal USB tree topology maintanace 
 * of USB
 */

#ifndef USB_TOPOLOGY_H
#define USB_TOPOLOGY_H

#include <barrelfish/barrelfish.h>

#include <stdio.h>
#include <stdlib.h>

#include "usb_device.h"

/*
 * Struct which is used to maintian USB tree topology
 */
typedef struct usb_node_t {
    usb_device_t *dev;          // Corresponding USB device 
    struct usb_node_t *parent;  // Parent for it 
    struct usb_node_t *child;   // For internal nodes, which have children
    struct usb_node_t *child_next;      // Sibling link 
    int no_child;               // Total children
    int connected;              // Current status of the node 
} usb_node_t;

int insert_node(usb_device_t * parent, usb_device_t * node);
int remove_node(usb_device_t * node);
int still_connected(usb_node_t * node);
int init_usb_tree(void);

#endif                          // USB_TOPOLOGY_H
