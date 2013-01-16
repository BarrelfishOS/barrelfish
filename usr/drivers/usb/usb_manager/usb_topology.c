/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <usb/usb_topology.h>
#include <usb/usb_debug.h>

#include "shared_services.h"

/*
 * XXX: This code segment (in the file) is the least tested code 
 * and has a high probability that it contains various bugs.
 */

static usb_node_t *usb_dev_tree = NULL;

/*
 * \brief Init the USB tree topology. It assigns one node i.e. root node 
 *        (EHCI). And as the other devices are enumerated they are connected
 *        through. 
 */

int init_usb_tree(void)
{
    dprintf("%s\n", __func__);
    static bool init = false;
    if (init)
        return 0;

    init = true;
    if (!usb_dev_tree)          // NULL
    {
        usb_dev_tree = (usb_node_t *) malloc(sizeof (usb_node_t));
        assert(usb_dev_tree);
        usb_dev_tree->parent = NULL;    // Root hub
        usb_dev_tree->child = NULL;
        // Sibling connections, for ROOT hub always NULL 
        usb_dev_tree->child_next = NULL;
        usb_dev_tree->no_child = 0;
        usb_dev_tree->connected = CONN_RH;
        usb_dev_tree->dev = NULL;       // no dev info for the root hub
        return 0;
    } else
        return -1;              // Double call ?? 
}

/*
 * \brief This function looks up all siblings/children of a node "on" 
 *        to locate node for device containing address "address". It 
 *        implements a primitive recursive logic. 
 * \param on Node to be searched on 
 * \param addresss Address to be searched
 */

static usb_node_t *locate_parent(usb_node_t * on, uint8_t address)
{
    usb_node_t *temp, *temp2;

    if (on->dev == NULL) {
        // On root hub this will be zero 
        // We did  not find address 
        return NULL;
    }

    if (on->dev->address == address)    // If this is the node 
        return on;


    temp2 = on->child;          // scan all children

    while (temp2 != NULL) {
        temp = locate_parent(temp2, address);
        if (temp)
            return temp;
        else
            temp2 = temp2->child_next;
    }

    // We are on the root hub 
    return NULL;
}

/*
 * \brief Inserts a new node "dev" to the parent node "parent".
 *
 * \param parent Node on which insertion has to be done
 * \param dev Node to be inserted
 */

int insert_node(usb_device_t * parent, usb_device_t * dev)
{
    usb_node_t *node = NULL, *temp;
    node = (usb_node_t *) malloc(sizeof (usb_node_t));
    node->child = NULL;
    node->child_next = NULL;
    node->no_child = 0;
    node->connected = CONN_RH;
    node->dev = dev;
    usb_node_t *parent_node = NULL;

    if (parent == NULL)         // Insertion on root hub
        parent_node = usb_dev_tree;
    else
        parent_node = locate_parent(usb_dev_tree, parent->address);


    if (parent_node->child == NULL) {
        // No children, yet. Make one  
        usb_dev_tree->child = node;
    } else {
        // We already have few other children connected to root hub 
        temp = usb_dev_tree->child;
        while (temp->child_next != NULL);       // Locate where to insert 
        temp->child = node;
    }

    node->parent = parent_node;
    set_dev_status(node->dev->address, CONN_RH);
    parent_node->no_child++;
    return 0;
}

/*
 * \brief Recursively destroyes and free all resources used in USB sub-tree 
 *        containing "root" as the root of the tree. 
 */

static void destroy_tree(usb_node_t * root)
{
    if (root == NULL)
        return;
    usb_node_t *node, *temp;
    node = root->child;
    while (node != NULL) {
        // Destroy their children's tree
        destroy_tree(node);
        node = node->child_next;
    }
    node = root->child;
    while (node != NULL) {
        // Remove the children
        temp = node->child_next;
        //node->dev->status = DISCONN_RH;
        set_dev_status(node->dev->address, DISCONN_RH);
        release_address(node->dev->address);
        free(node);
        node = temp;
    }
}

/*
 * \brief Removes a particular node from the USB tree and takes care if there 
 *        are more than one devices connected thtough that tree.
 *
 *\param rm_node Node to be removed
 */
int remove_node(usb_device_t * rm_node)
{
    usb_node_t *parent = locate_parent(usb_dev_tree, rm_node->address);
    usb_node_t *node = NULL, *prev = NULL;
    if (!parent)
        assert(!"Wrong parent id passed. Either we are"
               "looking parent for HC or invalid node ID\n");

    node = parent->child;
    prev = NULL;
    while (node->dev->address != rm_node->address) {
        prev = node;
        node = node->child_next;
    }
    if (prev != NULL)           // Not a single child
        prev->child_next = node->child_next;
    else
        parent->child = NULL;

    destroy_tree(node);
    free(node);
    return 0;
}

/*
 * \brief Returns the connectivity status of a node 
 */

int still_connected(usb_node_t * node)
{
    // XXX: This should be updated in some other thread 
    // Which on removal of a device, should mark all 
    // its children as unreachable from root hub
    return node->connected;
}
