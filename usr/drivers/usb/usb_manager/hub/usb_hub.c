/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

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
#include <usb/usb_device.h>

#include <usb_controller.h>
#include <usb_hub.h>
#include <usb_device.h>
#include <usb_transfer.h>

/*static const struct usb_transfer_setup hub_config[USB_HUB_NUM_TRANSFERS] = {
 [0] = {
 .type = USB_ENDPOINT_TYPE_INTR,
 .direction = USB_ENDPOINT_DIRECTION_ANY,
 .timeout = 0,
 .flags = {
 .short_xfer_ok = 1,
 .pipe_on_falure = 1,
 },
 .max_bytes = 0,
 .interval = USB_HUB_INTR_INTERVAL
 },
 };*/

struct usb_device *usb_hub_get_device(struct usb_hub *hub,
        struct usb_hub_port *port)
{
    return (NULL);
}

/**
 * \brief   checks if the hub device lies within an acceptable depdth, such that
 *          the attached USB devices have a specification conform depth
 *
 * \param   device: device we want to check
 *
 * \return  0: the depth of the device in the tree lies withing the range
 *          1: the depth of the device is too deep
 */
static uint8_t usb_hub_too_deep(struct usb_device *device)
{
    switch (device->speed) {
        case USB_SPEED_FULL:
        case USB_SPEED_LOW:
        case USB_SPEED_HIGH:
            /* revision 2.0 */
            if (device->depth > USB_HUB_MAX_DEPTH) {
                return (1);
            }
            break;
        case USB_SPEED_SUPER:
            /* revision 3.0 */
            assert(!"Super Speed not supported");
            break;
        default:
            break;
    }
    return (0);
}

/**
 * \brief this function reattaches the device on this port and reads out the
 *        device information and does the basic initialization such as
 *        assigning an address
 *
 * \param hub       the hub to attach the device
 * \param portno    the number of the port to attach the device
 *
 * \return USB_ERR_OK on success
 *         USB_ERR_XX on failure
 */
static usb_error_t usb_hub_reattach_port(struct usb_hub *hub, uint8_t portno)
{
    USB_DEBUG_TR("usb_hub_reattach_port(%u) \n", portno);
    usb_error_t err = USB_ERR_OK;
    uint8_t timeout = 0;

    struct usb_device *child = usb_hub_get_device(hub,
            hub->ports + (portno - 1));
    struct usb_hub_port_status ps;
    while (1) {

        err = usb_hub_clear_port_feature(hub->device,
                USB_HUB_FEATURE_C_PORT_CONNECTION, portno);
        if (err) {
            break;
        }

        if (child != NULL) {
            USB_DEBUG("reattach: freeing up child and restart fresh\n");
            /* free up the device to restart fresh */
            usb_device_free(child, 0);
            child = NULL;
        }
        err = usb_hub_get_port_status(hub->device, portno, &ps);
        if (err) {
            debug_printf("ERROR: could not get port status. Port=%u", portno);
            break;
        }

        if (!ps.wPortStatus.connection) {
            /* no device attached... error */
            debug_printf("WARNING: no device connected to port %u.\n", portno);
            break;
        }

        /* check for the power on the port */
        if (!ps.wPortStatus.power_state) {
            debug_printf("WARNING: Connected port %u has no power!\n", portno);
        }

        if (!ps.wPortStatus.device_mode) {
            if (ps.wPortStatus.suspend) {
                USB_DEBUG("Port %u is suspended. Wake up.\n", portno);
                err = usb_hub_clear_port_feature(hub->device,
                        USB_HUB_FEATURE_PORT_SUSPEND, portno);
            }

            USB_WAIT(USB_DELAY_PORT_POWERUP);

            err = usb_hub_reset_port(hub->device, portno);
            if (err != USB_ERR_OK) {
                debug_printf("WARNING: port reset failed.\n");
                break;
            }

            err = usb_hub_get_port_status(hub->device, portno, &ps);
            if (err != USB_ERR_OK) {
                debug_printf("ERROR: Could not read port status (%u)\n",
                        portno);
                break;
            }

            if (ps.wPortChange.connect || !ps.wPortStatus.connection) {
                if (timeout) {
                    debug_printf("WARTNING: timed out. "
                            "Giving up port reset...\n");
                    break;
                }
                timeout = 1;
                continue;
            }
        }

        /*
         * figuring out the device speed, this depends on the hub speed
         * and the port status fields
         */
        usb_speed_t speed = hub->device->speed;
        switch (hub->device->speed) {
            case USB_SPEED_HIGH:
                if (ps.wPortStatus.is_hs) {
                    speed = USB_SPEED_HIGH;
                    USB_DEBUG("device speed is HS -> HIGH SPEED\n");
                } else if (ps.wPortStatus.is_ls) {
                    USB_DEBUG("device speed is HS -> LOW SPEED\n");
                    speed = USB_SPEED_LOW;
                } else {
                    USB_DEBUG("device speed is HS -> FULL SPEED\n");
                    speed = USB_SPEED_FULL;
                }
                break;

            case USB_SPEED_FULL:
                if (ps.wPortStatus.is_ls) {
                    speed = USB_SPEED_LOW;
                    USB_DEBUG("device speed is FS -> LOW SPEED\n");
                } else {
                    speed = USB_SPEED_FULL;
                    USB_DEBUG("device speed is FS -> FULL SPEED\n");
                }
                break;
            case USB_SPEED_LOW:
                USB_DEBUG("device speed is LS -> LOW SPEED\n");
                speed = USB_SPEED_LOW;
                break;
            case USB_SPEED_SUPER:
                assert(!"NYI: SUPER SPEED DEVICES");
                break;
            default:
                /* same speed as parent */
                break;
        }

        if (speed == USB_SPEED_SUPER) {
            /* here goes some special handling for super speeds */
            assert(!"NYI: super speed timeout handling\n");
        }

        usb_mode_t mode = USB_MODE_HOST;
        if (ps.wPortStatus.device_mode) {
            USB_DEBUG("new device in DEVICE_MODE\n");
            mode = USB_MODE_DEVICE;
        }

        child = usb_device_alloc(hub->device->controller, hub->device,
                hub->device->depth + 1, portno - 1, portno, speed, mode);
        if (child == NULL) {
            debug_printf("Could not allocate a new device!\n");
        }

        return (USB_ERR_OK);
    }

    if (child != NULL) {
        usb_device_free(child, 0);
        child = NULL;
    }

    if (err == USB_ERR_OK) {
        if (ps.wPortStatus.enabled) {
            err = usb_hub_clear_port_feature(hub->device,
                    USB_HUB_FEATURE_PORT_ENABLE, portno);
        }
    }
    if (err) {
        debug_printf("WARNING: Device problem, disabling port\n");
    }
    return (err);
}

static usb_error_t usb_hub_explore_device(struct usb_hub *hub,
        struct usb_hub_port *port)
{

    return (USB_ERR_OK);
}

static usb_error_t usb_hub_suspend_resume_port(struct usb_hub *hub,
        uint8_t portno)
{
    assert(!"NYI: don't support suspend / resume at the moment.\n");
    return (USB_ERR_OK);
}

/**
 * \brief initializes a new USB hub device and checks for attached devices
 *
 * \param hub_device the USB device which is a hub
 *
 * \return USB_ERR_OK on success
 *         USB_ERR_XX on failure
 */
usb_error_t usb_hub_init(struct usb_device *hub_device)
{
    USB_DEBUG_TR("usb_hub_init()\n");
    struct usb_device *parent_hub = hub_device->parent_hub;
    usb_error_t err;

    /* check if the hub has an appropriate depth */
    if (usb_hub_too_deep(hub_device)) {
        debug_printf("Hub is too deep!. Ignored. \n");
        /* TODO: err */
        return (USB_ERR_TOO_DEEP);
    }

    if (!(hub_device->flags.self_powered) && parent_hub
            && (!parent_hub->flags.self_powered)) {
        /*
         * note: bus powered hubs cannot be connected to other
         * bus powered hubs.
         */
        debug_printf("WARNING: insufficient power.\n");
        /* TODO: ERR */
        return (USB_ERR_INVAL);
    }

    struct usb_hub_descriptor desc;

    uint8_t nports = 0;
    uint16_t powerdelay = 0;

    switch (hub_device->speed) {
        case USB_SPEED_LOW:
        case USB_SPEED_FULL:
        case USB_SPEED_HIGH:
            err = usb_hub_get_hub_descriptor(hub_device, 1, &desc);
            if (err != USB_ERR_OK) {
                debug_printf("ERROR: could not get hub descriptor \n");
                return (err);
            }
            /* get the number of ports */
            nports = desc.bNbrPorts;

            if (nports > 127) {
                debug_printf("WARNING: invalid port count\n");
                return (USB_ERR_INVAL);
            }

            /* get the power delay */
            powerdelay = USB_HUB_POWER_ON_DELAY(&desc);

            /*
             * we just got the first byte of data for the ports, so we
             * have to get more data if the port count is bigger than
             * the first byte i.e. 8
             */
            if (nports >= 8) {
                err = usb_hub_get_hub_descriptor(hub_device, nports, &desc);

                if (err != USB_ERR_OK) {
                    debug_printf("ERROR: could not get hub descriptor \n");
                    return (err);
                }

                if (desc.bNbrPorts != nports) {
                    debug_printf("ERROR: Volatile port count?? \n");
                    return (USB_ERR_INVAL);
                }
            }
            break;

        case USB_SPEED_SUPER:
            assert(!"NYI: super speed not supported\n");
            break;
        default:
            debug_printf("ERROR: Invalid device speed!\n");
            return (USB_ERR_INVAL);
            break;
    }

    if (nports == 0) {
        debug_printf("ERROR: hub has no ports\n");
        return (USB_ERR_IOERROR);
        /* TODO: ERROR */
    }

    struct usb_hub *hub = malloc(
            sizeof(struct usb_hub) + (sizeof(usb_hub_port_t) * nports));

    if (hub == NULL) {
        debug_printf("ERROR: Could not allocate memory for hub struct\n");
        return (USB_ERR_NOMEM);
    }

    /* do the binding */
    hub_device->hub = hub;
    hub->device = hub_device;

    hub->num_ports = nports;

    if (hub_device->flags.self_powered) {
        hub->portpower = USB_POWER_MAX;
    } else {
        hub->portpower = USB_POWER_MIN;
    }

    /* setup the interrupt pipe */
    if (USB_DEVICE_IS_ROOTHUB(hub_device)) {
        /*
         * the root hub is special, it needs no interrupt transfer
         */
        USB_DEBUG("device was the root hub\n");
        err = USB_ERR_OK;
    } else {
        /* TODO: usb_transfer_setup(udev, &iface_index, sc->sc_xfer,
         uhub_config, UHUB_N_TRANSFER, sc, &sc->sc_mtx); */
    }
    if (err != USB_ERR_OK) {
        debug_printf("COuld not setup the interrupt transfer\n");
        free(hub);
        return (err);
    }

    USB_WAIT(USB_DELAY_PORT_POWERUP);

    uint8_t portno = 0;
    uint8_t removable = 0;

    /* enumerate the devices and setup the data structures */
    for (uint8_t portindex = 0; portindex < nports; portindex++) {
        struct usb_hub_port *port = hub->ports + portindex;

        port->device_index = 0;
        port->restarts = 0;

        /* the port number (1 based) is the port index (0 based) + 1 */
        portno = portindex + 1;

        switch (hub_device->speed) {
            case USB_SPEED_LOW:
            case USB_SPEED_FULL:
            case USB_SPEED_HIGH:
                if (USB_HUB_DEVICE_REMOVABLE(&desc, portno)) {
                    removable++;
                }
                break;
            case USB_SPEED_SUPER:
                assert(!"NYI: super speed\n");
                break;
            default:
                debug_printf("WARNING: unknown speed, "
                        "assuming device removable\n");
                removable++;
                break;
        }

        if (err == USB_ERR_OK) {
            /* all fine we can turn on the power on that port */
            err = usb_hub_set_port_feature(hub_device,
                    USB_HUB_FEATURE_PORT_POWER, portno);
        }

        if (err != USB_ERR_OK) {
            debug_printf("WARNING: could not power on the port\n");
        }

        /* wait for powerdelay ms till the power power is good  */
        USB_WAIT(powerdelay);
    }

    /* start the interrupt transfer */
    if (hub->xfers[0] != NULL) {
        usb_transfer_start(hub->xfers[0]);
    }

    return (USB_ERR_OK);
}

/**
 * \brief this function is called upon removal of the USB hub device.
 *        All the child devices need to be removed
 *
 * \brief hub_device the USB device (hub) which is removed
 *
 * \return USB_ERR_OK on success
 *         USB_ERR_XX on failure
 */
usb_error_t usb_hub_deinit(struct usb_device *hub_device)
{
    return (USB_ERR_OK);
}

/**
 * \brief this function is called when a port hub change event is detected.
 *        all the ports are explored and the devices are identified
 *
 * \param usb_device the hub to explore
 *
 * \return USB_ERR_OK on success
 *         USB_ERR_XX on failure
 */
usb_error_t usb_hub_explore(struct usb_device *hub_device)
{
    usb_error_t err;

    struct usb_hub *hub = hub_device->hub;

    if (hub == NULL) {
        debug_printf("ERROR: hub_explore() - bad context.\n");
        return (USB_ERR_BAD_CONTEXT);
    }

    /*
     * if the USB hub is too deep in the USB device tree, then the attached
     * USB devices have a depth which would not be USB specification conform,
     * so if this is the case, skip the exploration process at this stage.
     */
    if (usb_hub_too_deep(hub_device)) {
        debug_printf("WARNING: hub_explore() - too deep.\n");
        return (USB_ERR_TOO_DEEP);
    }

    /* start exploring the ports by loop over the ports */

    usb_hub_port_t *port = NULL;
    uint8_t portno = 0;
    struct usb_hub_port_status ps;

    for (uint32_t i = 0; i < hub->num_ports; i++) {
        port = hub->ports + i;
        portno = i + 1;

        err = usb_hub_get_port_status(hub->device, portno, &ps);
        if (err != USB_ERR_OK) {
            debug_printf("WARNING: Could not read port status. Hub gone?%s\n",
                    usb_get_error_string(err));
            break;
        }

        /* check if we have a port over current condition */
        if (ps.wPortChange.over_current) {
            debug_printf("NOTICE: Over current condition on port %u.\n",
                    portno);
            err = usb_hub_clear_port_feature(hub->device,
                    USB_HUB_FEATURE_C_PORT_OVER_CURRENT, portno);
            if (err != USB_ERR_OK) {
                debug_printf(
                        "WARNING: Could not clear port feature. Hub gone?\n");
                break;
            }
        }

        /*
         * check if the port got disabled, this indicates an error condition
         * on the port
         */
        if (ps.wPortChange.disabled) {
            USB_DEBUG("WARNING: Port %i got disabled.\n", i);
            err = usb_hub_clear_port_feature(hub->device,
                    USB_HUB_FEATURE_C_PORT_ENABLE, portno);
            if (err != USB_ERR_OK) {
                debug_printf(
                        "WARNING: Could not clear port feature. Hub gone?\n");
                break;
            }

            if (ps.wPortChange.connect) {
                USB_DEBUG("NOTICE: Device on ort %u is gone.\n", i);
                /* the device is gone, ignore the port error */

            } else if (ps.wPortStatus.enabled) {
                debug_printf("WARNING: illegal enable change on port %u\n",
                        portno);
            } else {
                if (port->restarts == USB_HUB_MAX_RESTARTS) {
                    debug_printf("WARNING: too many restarts on port %u.\n ",
                            i);
                } else {
                    ps.wPortChange.connect = 1;
                    port->restarts++;
                }
            }
        }

        if (ps.wPortChange.connect) {
            USB_DEBUG("NOTICE: New device on port %u: reattach\n", portno);
            err = usb_hub_reattach_port(hub, portno);
            if (err != USB_ERR_OK) {
                debug_printf("WARNING: Could not reattach port. Hub gone?\n");
                break;
            }
        }

        if (ps.wPortChange.resumed || ps.wPortChange.linkstate) {
            USB_DEBUG("NOTICE: suspend/resume device on port %i.\n", i);
            err = usb_hub_suspend_resume_port(hub, portno);
            if (err != USB_ERR_OK) {
                debug_printf("WARNING: Could not resume the port. Hub gone?\n");
                break;
            }
        }

        err = usb_hub_explore_device(hub, port);
        if (err != USB_ERR_OK) {
            /* no device is present, just continue */
            continue;
        }

        port->restarts = 0;

    }

    return (USB_ERR_OK);
}

void usb_hub_bandwidth_alloc(struct usb_xfer *xfer)
{
    assert(!"NYI");
}

void usb_hub_bandwidth_free(struct usb_xfer *xfer)
{
    assert(!"NYI.");
}

/**
 *
 */
usb_error_t usb_hub_query_info(struct usb_hub *hub, uint8_t *ret_nports,
        uint8_t *ret_tt)
{
    struct usb_hub_descriptor desc;
    usb_error_t err = USB_ERR_OK;

    uint8_t nports = 0;
    uint8_t tt = 0;

    switch (hub->device->speed) {
        case USB_SPEED_LOW:
        case USB_SPEED_HIGH:
        case USB_SPEED_FULL:
            err = usb_hub_get_hub_descriptor(hub->device, 1, &desc);
            if (err) {
                debug_printf("ERROR: Failed to get hub descriptor\n");
                break;
            }
            nports = desc.bNbrPorts;
            if (nports > 127) {
                nports = 127;
            }
            if (hub->device->speed == USB_SPEED_HIGH) {
                tt = desc.wHubCharacteristics.tt_think_time;
            }
            break;
        case USB_SPEED_SUPER:
            assert(!"NYI: super speed hub\n");
            break;
        default:
            break;
    }

    if (ret_nports != NULL) {
        *ret_nports = nports;
    }

    if (ret_tt != NULL) {
        *ret_tt = tt;
    }

    return (err);
}

void usb_hub_root_interrupt(struct usb_host_controller *hc)
{
    USB_DEBUG_TR("usb_hub_root_interrupt()\n");
    if (hc == NULL) {
        debug_printf("WARNING: No host controller\n");
        return;
    }
    if ((hc->devices == NULL) || (hc->root_hub == NULL)) {
        debug_printf("WARNING: No root hub\n");
        return;
    }
    if (usb_hub_explore(hc->root_hub) != USB_ERR_OK) {
        debug_printf("WARNING: explore failed\n");
    }
}
