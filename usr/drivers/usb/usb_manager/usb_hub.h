/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
 
#define USB_DESCRIPTOR_TYPE_HUB 

#define USB_HUB_MAX_DEPTH   5
 
/*
 * ------------------------------------------------------------------------
 * USB Hub Class Descriptor (USB Specification, Rev 2.0, Section 11.23.2.1)
 * ------------------------------------------------------------------------
 * Hub class descriptors are device descriptors that have specialized
 * information for the hub class.  
 *
 * Fields:
 *  - bDescLength:          length of the descriptor in bytes
 *  - bDescriptorType:      always USB_DESCRIPTOR_TYPE_HUB
 *  - bNbrPorts:            number of downstream facing ports
 *  - wHubCharacteristics:  the characteristics of the hub
 *    - port_indicator:     port indicator supported flag
 *    - think_time:         time needed for transaction gap
 *    - protection:         over-current protection mode
 *    - is_compound:        compound device flag
 *    - logical_pwr:        logical power switching mode
 *  - bPwrOn2PwrGood:       time from power on to power good
 *  - bHubContrCurrent:     max current requirements in mA
 *  - deviceRemovable:      bitmap indicating if a port has a
 *                          removable device attachet to it
 *  - portPwrCtrlMask:      compatiblity to 1.0 devices, all
 *                          the bits should be set to 1.
 */
typedef struct {
    uint8_t _reserved,
    uint8_t port_indicator: 1,
    uint8_t think_time: 2,
    uint8_t protection:2, 
    uint8_t is_compound: 1,
    uint8_t logical_pwr: 2
} usb_hub_characteristics_t;

typedef struct {
    uint8_t                     bDescLength,
    uint8_t                     bDescriptorType,
    uint8_t                     bNbrPorts,
    usb_hub_characteristics_t   wHubCharacteristics,
    uint8_t                     bPwrOn2PwrGood,
    uint8_t                     bHubContrCurrent,
    uint8_t                     DeviceRemovable[32],
    uint8_t                     portPwrCtrlMask[1] /* deprecated */
} usb_hub_descriptor_t;

// size informatou about hub descriptor
#define USB_HUB_DESCRIPTOR_SIZE 9

// maximum number of ports supported by a hub
#define USB_HUB_MAX_PORTS 255

// logical power switching modes
#define USB_HUB_PWR_GANGED       0
#define USB_HUB_PWR_INDIVIDUAL   1
#define USB_HUB_PWR_NO_SWITCH    2

// over-current protection modes
#define USB_HUB_PROTECTION_GLOBAL       0
#define USB_HUB_PROTECTION_INDIVIDUAL   1
#define USB_HUB_PROTECTION_NONE         2

// compound device indicator flag
#define USB_HUB_IS_COMPOUND 1

// think times for the transaction gab
#define USB_HUB_THINK_8FS   0
#define USB_HUB_THINK_16FS  1
#define USB_HUB_THINK_24FS  2
#define USB_HUB_THINK_32FS  3

// get the device removable flag for port i
#define USB_HUB_IS_REMOVABLE(descr, i) \
    (((desc)->DeviceRemovable[(i)/8] >> ((i) % 8)) & 1)
    
    
  
/*
 * ------------------------------------------------------------------------
 * USB Hub Status Descriptor (USB Specification, Rev 2.0, Section 11.24.2.6)
 * ------------------------------------------------------------------------
 * Hub class descriptors are device descriptors that have specialized
 * information for the hub class.  
 *
 * Fields:
 *  - wHubStatus:   device status flags of the USB hub device
 *                  if a bit is one, then this means something bad
 *  - wHubChange:   indicating if local power or over current state
 *                  has changed on the hub
 */    
typedef struct {
    uint16_t wHubStatus,
    uint16_t wHubChange
} usb_hub_status_t;

// wHubStatus fields
#define USB_HUB_STATUS_LOCAL_POWER      0x0001
#define USB_HUB_STATUS_OVER_CURRENT     0x0002
#define USB_HUB_STATUS_OK(status)       (status->wHubStatus == 0x0000)    
#define USH_HUB_STATUS_CHANGED(status)  (status->wHubChange > 0x0000)    



/*
 * ------------------------------------------------------------------------
 * USB Hub Status Descriptor (USB Specification, Rev 2.0, Section 11.24.2.7)
 * ------------------------------------------------------------------------
 * Hub class descriptors are device descriptors that have specialized
 * information for the hub class.  
 *
 * Fields:
 *  - port_indicator:       software or default indicator colors
 *  - port_test:            port in test mode flag
 *  - port_high_speed:      high speed device attached
 *  - port_low_speed:       low speed device attached
 *  - port_power:           power on flag
 *  - port_reset:           host whishes to reset the attached device
 *  - port_over_current:    current power drain exeeds maximum
 *  - port_suspend:         the device on this port is suspended
 *  - port_enabled:         port enabled flag (settable by the USB system)
 *  - port_connection:      a device is connected to this port
 *  - change_reset:         reset has been completed
 *  - change_over_current:  over current indicator has changed
 *  - change_suspend:       device has transitioned out of supsended state
 *  - change_enable:        port disabled because port_error condition
 *  - change_status:        change occured in ports current connection status
 */    
typedef struct {
    uint8_t _reserved           : 3,
    uint8_t port_indicator      : 1,
    uint8_t port_test           : 1,
    uint8_t port_high_speed     : 1,
    uint8_t port_low_speed      : 1,
    uint8_t port_power          : 1,
    uint8_t _reserved2          : 3,
    uint8_t port_reset          : 1,
    uint8_t port_over_current   : 1,
    uint8_t port_suspend        : 1,
    uint8_t port_enabled        : 1,
    uint8_t port_connection     : 1,
    uint8_t _reserved3          : 11,
    uint8_t change_reset        : 1, 
    uint8_t change_over_current : 1,
    uint8_t change_suspend      : 1,
    uint8_t change_enable       : 1,
    uint8_t change_status       : 1,
} usb_hup_port_status_t;





/* Hub specific request */
#define UR_GET_BUS_STATE	0x02
#define UR_CLEAR_TT_BUFFER	0x08
#define UR_RESET_TT		0x09
#define UR_GET_TT_STATE		0x0a
#define UR_STOP_TT		0x0b

/* Hub features */
#define UHF_C_HUB_LOCAL_POWER	0
#define UHF_C_HUB_OVER_CURRENT	1
#define UHF_PORT_CONNECTION	0
#define UHF_PORT_ENABLE		1
#define UHF_PORT_SUSPEND	2
#define UHF_PORT_OVER_CURRENT	3
#define UHF_PORT_RESET		4
#define UHF_PORT_POWER		8
#define UHF_PORT_LOW_SPEED	9
#define UHF_C_PORT_CONNECTION	16
#define UHF_C_PORT_ENABLE	17
#define UHF_C_PORT_SUSPEND	18
#define UHF_C_PORT_OVER_CURRENT	19
#define UHF_C_PORT_RESET	20
#define UHF_PORT_TEST		21
#define UHF_PORT_INDICATOR	22


