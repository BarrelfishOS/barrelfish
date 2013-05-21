/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <barrelfish/barrelfish.h>

#include <usb/usb.h>
#include <usb/usb_descriptor.h>
#include <usb/usb_parse.h>

/*------------------------------------------------------------------------*
 *  usb_desc_foreach
 *
 * This function is the safe way to iterate across the USB config
 * descriptor. It contains several checks against invalid
 * descriptors. If the "desc" argument passed to this function is
 * "NULL" the first descriptor, if any, will be returned.
 *
 * Return values:
 *   NULL: End of descriptors
 *   Else: Next descriptor after "desc"
 *------------------------------------------------------------------------*/

/**
 * \brief use this function to iterate over the USB configuration descriptor
 *
 * \param cd    the configuration descriptor to iterate over
 * \param desc  the descriptor to start from
 *
 * \return  NULL: there iare no mor descriptors
 *          Else: the next descriptor after "desc"
 */
struct usb_descriptor *usb_parse_next_descriptor(
        struct usb_config_descriptor *cd, struct usb_descriptor *_desc)
{
    uint8_t *desc_next;
    uint8_t *start;
    uint8_t *end;
    uint8_t *desc;

    /* be NULL safe */
    if (cd == NULL)
        return (NULL);

    /* We assume that the "wTotalLength" has been checked. */
    start = (uint8_t *) cd;
    end = start + cd->wTotalLength;
    desc = (uint8_t *) _desc;

    /* Get start of next USB descriptor. */
    if (desc == NULL)
        desc = start;
    else
        desc = desc + desc[0];

    /* Check that the next USB descriptor is within the range. */
    if ((desc < start) || (desc >= end))
        return (NULL); /* out of range, or EOD */

    /* Check that the second next USB descriptor is within range. */
    desc_next = desc + desc[0];
    if ((desc_next < start) || (desc_next > end))
        return (NULL); /* out of range */

    /* Check minimum descriptor length. */
    if (desc[0] < 3)
        return (NULL); /* too short descriptor */

    /* Return start of next descriptor. */
    return ((struct usb_descriptor *) desc);
}


#if 0
/*------------------------------------------------------------------------*
 *  usb_idesc_foreach
 *
 * This function will iterate the interface descriptors in the config
 * descriptor. The parse state structure should be zeroed before
 * calling this function the first time.
 *
 * Return values:
 *   NULL: End of descriptors
 *   Else: A valid interface descriptor
 *------------------------------------------------------------------------*/
struct usb_interface_descriptor *
usb_idesc_foreach(struct usb_config_descriptor *cd,
        struct usb_idesc_parse_state *ps)
{
    struct usb_interface_descriptor *id;
    uint8_t new_iface;

    /* retrieve current descriptor */
    id = (struct usb_interface_descriptor *) ps->desc;
    /* default is to start a new interface */
    new_iface = 1;

    while (1) {
        id = (struct usb_interface_descriptor *) usb_desc_foreach(cd,
                (struct usb_descriptor *) id);
        if (id == NULL)
            break;
        if ((id->bDescriptorType == UDESC_INTERFACE)
                && (id->bLength >= sizeof(*id))) {
            if (ps->iface_no_last == id->bInterfaceNumber)
                new_iface = 0;
            ps->iface_no_last = id->bInterfaceNumber;
            break;
        }
    }

    if (ps->desc == NULL) {
        /* first time */
    } else if (new_iface) {
        /* new interface */
        ps->iface_index++;
        ps->iface_index_alt = 0;
    } else {
        /* new alternate interface */
        ps->iface_index_alt++;
    }

    /* store and return current descriptor */
    ps->desc = (struct usb_descriptor *) id;
    return (id);
}

/*------------------------------------------------------------------------*
 *  usb_edesc_foreach
 *
 * This function will iterate all the endpoint descriptors within an
 * interface descriptor. Starting value for the "ped" argument should
 * be a valid interface descriptor.
 *
 * Return values:
 *   NULL: End of descriptors
 *   Else: A valid endpoint descriptor
 *------------------------------------------------------------------------*/
struct usb_endpoint_descriptor *
usb_edesc_foreach(struct usb_config_descriptor *cd,
        struct usb_endpoint_descriptor *ped)
{
    struct usb_descriptor *desc;

    desc = ((struct usb_descriptor *) ped);

    while ((desc = usb_desc_foreach(cd, desc))) {
        if (desc->bDescriptorType == UDESC_INTERFACE) {
            break;
        }
        if (desc->bDescriptorType == UDESC_ENDPOINT) {
            if (desc->bLength < sizeof(*ped)) {
                /* endpoint descriptor is invalid */
                break;
            }
            return ((struct usb_endpoint_descriptor *) desc);
        }
    }
    return (NULL);
}

/*------------------------------------------------------------------------*
 *  usb_ed_comp_foreach
 *
 * This function will iterate all the endpoint companion descriptors
 * within an endpoint descriptor in an interface descriptor. Starting
 * value for the "ped" argument should be a valid endpoint companion
 * descriptor.
 *
 * Return values:
 *   NULL: End of descriptors
 *   Else: A valid endpoint companion descriptor
 *------------------------------------------------------------------------*/
struct usb_endpoint_ss_comp_descriptor *
usb_ed_comp_foreach(struct usb_config_descriptor *cd,
        struct usb_endpoint_ss_comp_descriptor *ped)
{
    struct usb_descriptor *desc;

    desc = ((struct usb_descriptor *) ped);

    while ((desc = usb_desc_foreach(cd, desc))) {
        if (desc->bDescriptorType == UDESC_INTERFACE)
            break;
        if (desc->bDescriptorType == UDESC_ENDPOINT)
            break;
        if (desc->bDescriptorType == UDESC_ENDPOINT_SS_COMP) {
            if (desc->bLength < sizeof(*ped)) {
                /* endpoint companion descriptor is invalid */
                break;
            }
            return ((struct usb_endpoint_ss_comp_descriptor *) desc);
        }
    }
    return (NULL);
}

/*------------------------------------------------------------------------*
 *  usbd_get_no_descriptors
 *
 * This function will count the total number of descriptors in the
 * configuration descriptor of type "type".
 *------------------------------------------------------------------------*/
uint8_t usbd_get_no_descriptors(struct usb_config_descriptor *cd, uint8_t type)
{
    struct usb_descriptor *desc = NULL;
    uint8_t count = 0;

    while ((desc = usb_desc_foreach(cd, desc))) {
        if (desc->bDescriptorType == type) {
            count++;
            if (count == 0xFF)
                break; /* crazy */
        }
    }
    return (count);
}

/*------------------------------------------------------------------------*
 *  usbd_get_no_alts
 *
 * Return value:
 *   Number of alternate settings for the given interface descriptor
 *   pointer. If the USB descriptor is corrupt, the returned value can
 *   be greater than the actual number of alternate settings.
 *------------------------------------------------------------------------*/
uint8_t usbd_get_no_alts(struct usb_config_descriptor *cd,
        struct usb_interface_descriptor *id)
{
    struct usb_descriptor *desc;
    uint8_t n;
    uint8_t ifaceno;

    /* Reset interface count */

    n = 0;

    /* Get the interface number */

    ifaceno = id->bInterfaceNumber;

    /* Iterate all the USB descriptors */

    desc = NULL;
    while ((desc = usb_desc_foreach(cd, desc))) {
        if ((desc->bDescriptorType == UDESC_INTERFACE)
                && (desc->bLength >= sizeof(*id))) {
            id = (struct usb_interface_descriptor *) desc;
            if (id->bInterfaceNumber == ifaceno) {
                n++;
                if (n == 0xFF)
                    break; /* crazy */
            }
        }
    }
    return (n);
}
#endif
