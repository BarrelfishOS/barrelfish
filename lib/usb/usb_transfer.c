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

#include <if/usb_manager_defs.h>
#include <if/usb_manager_rpcclient_defs.h>

#include <usb/usb.h>
#include <usb/usb_request.h>
#include <usb/usb_device.h>
#include <usb/usb_xfer.h>
#include <usb/usb_transfer.h>

/*
 * -------------------------------------------------------------------------
 * Internatl transfer state
 * -------------------------------------------------------------------------
 */

/**
 * struct containing information about the existing transfers
 */
struct usb_xfer_state {
    uint32_t tid;                   ///< the transfer id
    usb_transfer_cb_t *done_cb;     ///< pointer to the callback function
    usb_error_t error;              ///< the error condition of the transfer
    usb_tstate_t state;             ///< the state of the transfer
    struct usb_xfer_state *next;    ///< pointer to the next transfer
    struct usb_xfer_state *prev;    ///< previous transfer
};

/// stores the created transfer
//static struct usb_xfer_state xfers = NULL;

/*
 * -------------------------------------------------------------------------
 * Functions for setup/unsetup of USB transfers
 * -------------------------------------------------------------------------
 */

/**
 * \brief   sets up a new USB control transfer for the associated
 *          device, this allocates the resources, but does not start
 *          the execution of the transfer.
 *
 * \param   setup   setup parameters
 * \param   done_cb callback when the transfer completes
 * \param   ret_id  returns the ID of the created usb transfer
 *
 * \return  USB_ERR_OK on success
 *          USB_ERR_XX on failure
 */
usb_error_t usb_transfer_setup_control(usb_transfer_setup_t *setup,
        usb_transfer_cb_t *done_cb, usb_xfer_id_t *ret_id)
{
    return (USB_ERR_OK);
}

/**
 * \brief   sets up a new USB control transfer for the associated
 *          device, this allocates the resources, but does not start
 *          the execution of the transfer.
 *
 * \param   setup   setup parameters
 * \param   done_cb callback when the transfer completes
 * \param   ret_id  returns the ID of the created usb transfer
 *
 * \return  USB_ERR_OK on success
 *          USB_ERR_XX on failure
 */
usb_error_t usb_transfer_setup_isoc(usb_transfer_setup_t *setup,
        usb_transfer_cb_t *done_c, usb_xfer_id_t *ret_id)
{
    return (USB_ERR_OK);
}

/**
 * \brief   sets up a new USB bulk transfer for the associated
 *          device, this allocates the resources, but does not start
 *          the execution of the transfer.
 *
 * \param   setup   setup parameters
 * \param   done_cb callback when the transfer completes
 * \param   ret_id  returns the ID of the created usb transfer
 *
 * \return  USB_ERR_OK on success
 *          USB_ERR_XX on failure
 */
usb_error_t usb_transfer_setup_bulk(usb_transfer_setup_t *setup,
        usb_transfer_cb_t *done_c, usb_xfer_id_t *ret_id)
{
    return (USB_ERR_OK);
}

/**
 * \brief   sets up a new USB interrupt transfer for the associated
 *          device, this allocates the resources, but does not start
 *          the execution of the transfer.
 *
 * \param   setup   setup parameters
 * \param   done_cb callback when the transfer completes
 * \param   ret_id  returns the ID of the created usb transfer
 *
 * \return  USB_ERR_OK on success
 *          USB_ERR_XX on failure
 */
usb_error_t usb_transfer_setup_intr(usb_transfer_setup_t *setup,
        usb_transfer_cb_t *done_c, usb_xfer_id_t *ret_id)
{
    return (USB_ERR_OK);
}

/**
 * \brief   destroys the previously created USB transfer and frees up the
 *          used resources, this allocates the resources, but does not start
 *          the execution of the transfer.
 *
 * \param   tid the transfer id of the transfer to unsetup
 *
 * \return  USB_ERR_OK on success
 *          USB_ERR_XX on failure
 */
usb_error_t usb_transfer_unsetup(usb_xfer_id_t tid)
{
    return (USB_ERR_OK);
}

/*
 * -------------------------------------------------------------------------
 * Functions for transfer controlling
 * -------------------------------------------------------------------------
 */

/**
 * \brief   initiates the execution of an existing USB transfer
 *
 * \param   tid the ID of the transfer to start
 *
 * \return  USB_ERR_OK on sucess
 *          USB_ERR_XX on failure
 */
usb_error_t usb_transfer_start(usb_xfer_id_t tid)
{
    return (USB_ERR_OK);
}

/**
 * \brief   stops the execution of an existing USB transfer, this does not
 *          free up the allocated resources
 *
 * \param   tid the ID of the transfer to stop
 *
 * \return  USB_ERR_OK on sucess
 *          USB_ERR_XX on failure
 */
usb_error_t usb_transfer_stop(usb_xfer_id_t tid)
{
    return (USB_ERR_OK);
}

/**
 * \brief   handles the stall condition of a transfer and clears it
 *
 * \param   tid the ID of the stalled transfer
 *
 * \return  USB_ERR_OK on sucess
 *          USB_ERR_XX on failure
 */
usb_error_t usb_transfer_clear_stall(usb_xfer_id_t tid)
{
    return (USB_ERR_OK);
}

/*
 * -------------------------------------------------------------------------
 * Functions for getting the transfer state and status
 * -------------------------------------------------------------------------
 */

usb_error_t usb_transfer_state(usb_xfer_id_t tid, uint32_t *ret_state)
{
    return (USB_ERR_OK);
}

usb_error_t usb_transfer_status(usb_xfer_id_t tid, uint32_t *ret_actlen,
        uint32_t *ret_length, uint32_t *ret_actframes, uint32_t *ret_numframes)
{
    return (USB_ERR_OK);

}
