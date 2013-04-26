/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
 
 /*
  * =======================================================================
  * This file contains the definitions of the data structures used by
  * the host controller driver. This data structures are completely
  * software related and do not depend on hardware specific layout.
  * =======================================================================
  */

/*
 * ------------------------------------------------------------------------
 * OHCI Host Controller Driver Software Endpoint Descriptor
 * ------------------------------------------------------------------------
 * Fields:
 *  - list_index:        identifier of which EdList this ED is inserted
 *  - paused_flag:       Non zero if ED is queued on PausedEDRestart list
 *  - physical_address:  physical address of the hc_ED
 *  - next_ed:           vaddr pointer to the next ED. 
 *  - prev_ed:           vaddr pointer to the previous ED
 *                       Shadowing the phyaddr list of the host controller.
 *  - endpoint:          pointer to the software structure of the endpoint
 *  - reclamation_frame: time (frane number) when it's safe to remove
 *  - prev_paused_ed:    vaddr pointer to the previous ED in paused state
 *  - next_paused_ed:    vaddr pointer to the next ED in paused state
 *  - hc_ED:             host controller endpoint descriptor
 */
typedef struct usb_ohcid_ed {
    uint8_t             list_index,
    uint8_t             paused,
    uint16_t            reserved,
    phyaddr_t           physical_address,
    struct usb_ohcid_ed *prev_ed,
    struct usb_ohcid_ed *next_ed,
    usb_ohci_ep_t       *endpoint,
    uint32_t            reclamation_frame,
    struct usb_ohcid_ed *prev_paused_ed,
    struct usb_ohcid_ed *next_paused_ed,
    usb_ohci_ed_t       hcED,
} usb_ohcid_ed_t;
    
// size definitions
#define USB_OHCID_ED_SIZE ((sizeof (struct usb_ohcid_ed) + USB_OHCI_ED_ALIGN - 1) / USB_OHCI_ED_ALIGN * USB_OHCI_ED_ALIGN)
#define USB_OHCID_ED_CHUNK	(BASE_PAGE_SIZE / OHCI_SED_SIZE)
 
/*
 * ------------------------------------------------------------------------
 * OHCI Host Controller Driver Software Transfer Descriptor
 * ------------------------------------------------------------------------
 * Fields:
 *  - status:           status of this td
 *  - cancel_pending:   true if this USBD Request has been cancelled
 *  - physical_address: physical address of the TD
 *  - next_td:          virtual pointer to the next TD on the EP queue
 *  - next_req_td:      link to next TD of the same USB request
 *  - priv_req_td:      link to previous TD of the same USB request
 *  - request:          pointer to the USB transfer request
 *  - endpoint:         pointer to the endpoint of the transfer
 *  - transfer_count:   total number of bytes queued
 *  - hcTD:             host controllers transfer descriptor
 */
typedef struct usb_ohcid_td {
   uint8_t             status,
   bool                cancel_pending,
   phyaddr_t           physical_address,
   struct usb_ohcid_td *next_td,
   struct usb_ohcid_td *next_req_td,
   struct usb_ohcid_td *priv_req_td,
   usb_request_t       *request,
   usb_ohci_ep_t       *endpoint,
   uint32_t            transfer_count,
   usb_ohci_td_t       hcTD
} usb_ohcid_td_t;

// status codes for the sofware transfer descriptor
#define USB_OHCI_TD_STATUS_PENDING      0
#define USB_OHCI_TD_STATUS_COMPLETED    1
#define USB_OHCI_TD_STATUS_CANCELLED    2
#define USB_OHCI_TD_STATUS_NOT_FILLED   3

// size definitions
#define USB_OHCID_TD_SIZE ((sizeof (struct usb_ohcid_td) + USB_OHCI_TD_ALIGN - 1) / USB_OHCI_TD_ALIGN * USB_OHCI_TD_ALIGN)
#define USB_OHCID_TD_CHUNK (BASE_PAGE_SIZE / USB_OHCID_TD_SIZE)
 
/*
 * ------------------------------------------------------------------------
 * OHCI Host Controller Driver Software Endpoint
 * ------------------------------------------------------------------------
 * Fields:
 *  - type:             the type of this endpoint CTRL | BULK ...
 *  - list_index:       index into the device data EdList
 *  - device_data:      pointer to corresponding device data
 *  - control:          endpoint control structure
 *  - soft_ed:          current endpoint descriptor which is scheduled
 *  - ep_queue_head:    virtual pointer to the first TD on this EP's queue
 *  - ep_queue_tail:    virtual pointer to the last TD on this EP's queue
 *  - rate:             requested polling rate for an interrupt EP
 *  - bandwidth:        ISOCHR | INTR: amount of required bandwidth
 *                      CTRL | BULK:   max packet size
 *  - max_packet:       maximum packet size for this endpoint
 */ 
 typedef struct {
    uint8_t             type,
    uint8_t             list_index,
    uint16_t            reserved,
    usb_ohci_dev_t      *device_data,
    usb_ohci_ep_ctrl_t  control,
    usb_ohcid_ed_t      *ed,
    usb_ohcid_td_t      *ep_queue_head,
    usb_ohcid_td_t      *ep_queue_tail,
    uint32_t            rate,      
    uint32_t            bandwidth,
    uint32_t            max_packet,
 } usb_ohcid_ep_t;
 
 // endpoint type codes
 #define USB_OHCI_EP_TYPE_ISOC 0
 #define USB_OHCI_EP_TYPE_INTR 1
 #define USB_OHCI_EP_TYPE_CTRL 2
 #define USB_OHCI_EP_TYPE_BULK 3
 
 
/*
 * ------------------------------------------------------------------------
 * OHCI Host Controller Driver Endpoint Descriptor List
 * ------------------------------------------------------------------------
 * Fields:
 *  - head:             head of an HCD endpoint descriptor list
 *  - physical_head:    address to put the physical head pointer
 *  - bandwidth:        allocated bandwidth in this timeslize
 *  - next:             index of the next ED list for this timeslice
 */ 
 typedef struct {
    usb_ohcid_ed_t  *head,
    physaddr_t      *physical_head,
    uint32_t        bandwidth,
    uint8_t         next,
    uint8_t         reserved
 } usb_ohcid_ed_list_t;

 #define USB_OHCI_ED_IRQ_1MS   0
 #define USB_OHCI_ED_IRQ_2MS   1
 #define USB_OHCI_ED_IRQ_4MS   3
 #define USB_OHCI_ED_IRQ_8MS   7
 #define USB_OHCI_ED_IRQ_16MS 15
 #define USB_OHCI_ED_IRQ_32MS 31
 #define USB_OHCI_ED_CTRL     63
 #define USB_OHCI_ED_BULK     64
 #define USB_OHCI_ED_ISOCHR   0
 #define USB_OHCI_ED_NO_LISTS 65
 #define USB_OHCI_ED_EOF      0xFF
 
 #define USB_OHCI_NUM_ED_LIST 65
 
/*
 * ------------------------------------------------------------------------
 * OHCI Host Controller Driver Device Data
 * ------------------------------------------------------------------------
 * Fields:
 *  - hc:                     pointer to the host controller operational registers
 *  - hcca:                   pointer to the shared memory hcca block
 *  - endpoints:              list of connected HCD endpoint structs in FIFO order
 *  - free_ed:                list of free usb_ohci_soft_ed structures
 *  - free_td:                list of free usb_ohci_soft_td structures
 *  - stalled_ed_reclamation: list of ED to be freed
 *  - running_ed_reclamation: list of ED to be freed according to reclamation frame
 *  - paused_ed_restart:      list of ED to be restarted once cancelled are removed
 *  - ed_list:                active ED lists
 *  - frame_high_part:        upper bits of the 32bit frame number
 *  - bandwidth_availabe:     bandwidth supported by this host controller
 *  - bandwidth_used:         currenetly allocated bandwidth in this time slice
 *  - so_count:               schedule overrun count
 *  - so_stall_frame:         schedule overrun for stall count starts in this frame
 *  - so_limit_frame:         schedule overrun for bandwidth limit adjusts
 *  - so_limit_hit:           limit condition hit
 *  - so_stall_hit:           stall condition was hit
 */ 
 typedef struct {
    usb_ohci_op_regs_t *hc,
    usb_ohci_hcca_t *hcca,
    usb_ohcid_ep_t *endpoints,
    usb_ohcid_ed_t *free_ed,
    usb_ohcid_td_t *free_td,
    usb_ohcid_ed_t *stalled_ed_reclamation,
    usb_ohcid_ed_t *running_ed_reclamation,
    usb_ohcid_ed_t *paused_ed_restart,
    usb_ohci_ed_list_t ed_list[USB_OHCI_NUM_ED_LIST],
    uint32_t frame_high_part,
    uint32_t bandwidth_available,
    uint32_t bandwidth_used,
    uint32_t so_count,
    uint32_t so_stall_frame,
    uint32_t so_limit_frame,
    bool so_limit_hit,
    bool so_stall_hit,
 } usb_ohcid_dev_t;
 
