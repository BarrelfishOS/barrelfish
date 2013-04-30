/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBUSB_HUB_DESCRITPOR_H_
#define LIBUSB_HUB_DESCRITPOR_H_

#include <stdint.h>

// the hub descriptor type
#define USB_DESCRIPTOR_TYPE_HUB	0x29

/**
 * ------------------------------------------------------------------------
 * USB Hub Class Descriptor (USB Specification, Rev 2.0, Section 11.23.2.1 )
 * ------------------------------------------------------------------------
 * The device descriptor for USB hub class devices.
 *
 * Fields:
 *  - bLength:          	Number of bytes in this descriptor,
 *  - bDescriptorType:  	Descriptor Type, value: 29H for hub descriptor
 * 	- bNbrPorts:			Number of downstream facing ports of this hub
 * 	- wHubCharacteristics
 *  	- port_indicator	is true when port indicator LEDs are supported
 *  	- tt_think_time		Transaction Translation Thing Time
 *  	- protection_mode	Over-current protection mode
 *  	- compound_device	is true when the hub is a compound device
 *  	- power_mode		Logical Power Switching Modes
 * 	- bPwrOn2PwrGood:		time from power on till port is accessible
 * 							in 2ms steps
 * 	- bHubContrCurrent:		max current requirements of the Hub in mA.
 *	- bDeviceRemovable		Indicates if a port has a removable device attached
 *							size depends on the number of ports. This field is
 *							reported on byte-granularity.
 *
 *
 */
struct usb_hub_characteristics
{
    uint8_t _reserved;
    uint8_t port_indicator :1;
    uint8_t tt_think_time :2;
    uint8_t protection_mode :2;
    uint8_t compound_device :1;
    uint8_t power_mode :2;
};

typedef struct usb_hub_characteristics usb_hub_characteristics_t;

struct usb_hub_class_descriptor
{
    uint8_t bDesLength;
    uint8_t bDescriptorType;
    uint8_t bNbrPorts;
    struct usb_hub_characteristics wHubCharacteristics;
    uint8_t bPwrOn2PwrGood;
    uint8_t bHubContrCurrent;
    uint8_t bDeviceRemovable[32];
};

/*
 * Logical Power Switching Modes
 *	GANGED = all ports power at once
 */
#define USB_HUB_POWER_GANGED			0
#define USB_HUB_POWER_INDIVIDUAL		1
#define USB_HUB_POWER_NO_SWITCH			2

/*
 * Over-current Protection Modes
 *	GLOBAL: over-current information is reported as an aggregate
 *	INDIVIDUAL: over-current information on a per-port basis
 */
#define USB_HUB_PROTECTION_GLOBAL		0
#define USB_HUB_PROTECTION_INDIVIDUAL	1
#define USB_HUB_PROTECTION_NONE			2

/*
 * Transaction Translation Thing Time
 */
#define USB_HUB_TT_TIME_8		0
#define USB_HUB_TT_TIME_16		1
#define USB_HUB_TT_TIME_24		2
#define USB_HUB_TT_TIME_32		3

/*
 * Returns the delay from power on to power good in milli seconds
 */
#define USB_HUB_POWER_ON_DELAY(hub) (hub->bPwrOn2PwrGood*2)

// The maximum supported ports
#define USB_HUB_MAX_PORTS 255

// checks if the device at port is removable
#define USB_HUB_DEVICE_REMOVABLE(desc, port) \
		(!(((desc)->DeviceRemovable[(i)/8] >> ((i) % 8)) & 1))

// size definition
#define USB_HUB_DESCRIPTOR_MIN_SIZE 8

/**
 * ------------------------------------------------------------------------
 * USB Hub Status (USB Specification, Rev 2.0, Table 11-19 )
 * ------------------------------------------------------------------------
 * The hub status struct is returned when querying the hub with the
 * usb_hub_get_hub_status() request.
 *
 * Fields:
 *  - local_power_source:	This field indicates whether hub power is being
 *  						provided by an external source or from the USB.
 *  - over_current:			this field indicates that the sum of all the ports’
 *  						current has exceeded the specified maximum and all
 *  						ports have been placed in the Powered-off state.
 *  - local_power_change: 	his field indicates that a change has occurred in
 *  						the hub’s Local Power Source field in wHubStatus.
 *  - over_current_change:	This field indicates if a change has occurred in
 *  						the Over-Current field in wHubStatus.
 */
struct usb_hub_status
{
    uint16_t _reserved :14;
    uint8_t local_power_source :1;
    uint8_t over_current :1;
    uint16_t _reserved_ :14;
    uint8_t local_power_change :1;
    uint8_t over_current_change :1;
};

typedef struct usb_hub_status usb_hub_status_t;

#define USB_HUB_STATUS_HAS_POWERSUPPLY(st) (st->local_power_source == 0)

/**
 * ------------------------------------------------------------------------
 * USB Hub Port Status (USB Specification, Rev 2.0, Table 11-19 )
 * ------------------------------------------------------------------------
 * The hub status struct is returned when querying a hub port with the
 * usb_hub_get_port_status() request.
 *
 * Fields:
 * 	- port_indicator:		set if indicator color is software controlled
 * 	- port_test_mode:		indicates if the port is in test mode
 * 	- port_high_speed:		bit is set if attached device runs in high speed
 * 	- port_low_speed:		bit is set if attached device runs in low speed
 * 	- port_power:			reflects the port's logical power control state
 * 							(not necessarily if there is power on the port)
 * 	- port_reset:			is set when the host whishes to reset the attached
 * 							device on this port
 * 	- port_over_current:	indicates if an over current condition exists
 * 	- port_suspend:			indicates if the device on this port is suspended
 * 	- port_enable:			indicates if the port is enabled
 * 	- port_connection:		indicates if a device is connected to the port *
 *  - reset_change:			set when reset processing on this port is complete.
 *  - over_current_change:	This field indicates if a change has occurred in
 *  						the Over-Current field in wHubStatus.
 * 	- suspend_change:		set when the resume process has completed
 * 	- port_enable_change:	set when port is disabled because of error condition
 *  - connect_change:		set when current connect status has changed
 *
 * The bit locations in the wPortStatus and wPortChange fields correspond
 * in a one-to-one fashion where applicable.
 *
 * USB Specification Rev 2.0, Section 11.24.2.7.1, gives a detailed description
 */
struct usb_hub_port_status
{
    struct {
    uint8_t _reserved :3;
    uint8_t port_indicator :1;
    uint8_t port_test_mode :1;
    uint8_t port_high_speed :1;
    uint8_t port_low_speed :1;
    uint8_t port_power :1;
    uint8_t _reserved_ :3;
    uint8_t port_reset :1;
    uint8_t port_over_current :1;
    uint8_t port_suspend :1;
    uint8_t port_enable :1;
    uint8_t port_connection :1;
    } wPortStatus;
    struct {
    uint16_t _reserved__ :11;
    uint8_t reset_change :1;
    uint8_t over_current_change :1;
    uint8_t suspend_change :1;
    uint8_t port_enable_change :1;
    uint8_t connect_change :1;
    } wPortChange;
};

/*
 * device class codes
 */
#define USB_HUB_CLASS_CODE  0x09
#define USB_HUB_SUBCLASS_CODE 0x00
#define USB_HUB_PROTOCOL_FSHUB       0x00
#define USB_HUB_PROTOCOL_HSHUBSTT    0x01
#define USB_HUB_PROTOCOL_HSHUBMTT    0x02
#define USB_HUB_PROTOCOL_SSHUB       0x03

/*
 * interface class code
 */
#define USB_HUB_IFACE_CLASS_CODE     0x09
#define USB_HUB_IFACE_SUBCLASS_CODE      0
#define USB_HUB_IFACE_PROTOCOL_FSHUB       0
#define USB_HUB_IFACE_PROTOCOL_HSHUBSTT    0   /* Yes, same as previous */
#define USB_HUB_IFACE_PROTOCOL_HSHUBMTT    1

#endif /* LIBUSB_HUB_DESCRITPOR_H_ */
