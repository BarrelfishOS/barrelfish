/**
 * \file
 * \brief "Serial" driver on host side
 */

/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>

#include <dev/xeon_phi/xeon_phi_serial_dev.h>

#include "xeon_phi_internal.h"

#define XEON_PHI_BUFFER_LENGTH 0x400
#define XEON_PHI_POLL_GIVEUP   0x100

struct xeon_phi_arg
{
    xeon_phi_serial_t base;
    uint8_t xid;
    uint8_t init;
    char buffer[XEON_PHI_BUFFER_LENGTH + 1];
    uint32_t idx;
};

static struct xeon_phi_arg recv_thread_arg;

#define ANSI_COLOR_RED     "\x1b[31m"
#define ANSI_COLOR_GREEN   "\x1b[32m"
#define ANSI_COLOR_YELLOW  "\x1b[33m"
#define ANSI_COLOR_BLUE    "\x1b[34m"
#define ANSI_COLOR_MAGENTA "\x1b[35m"
#define ANSI_COLOR_CYAN    "\x1b[36m"
#define ANSI_COLOR_RESET   "\x1b[0m"

static inline void xprintf(uint8_t xid,
                           char *fmt)
{
    switch (xid) {
        case 0:
            printf(ANSI_COLOR_YELLOW">> XEON_PHI.%u"ANSI_COLOR_RESET": %s\n", xid, fmt);
            break;
        case 1:
            printf(ANSI_COLOR_MAGENTA">> XEON_PHI.%u"ANSI_COLOR_RESET": %s\n", xid, fmt);
            break;
        default:
            printf("\x1b[32m>> XEON_PHI \033[31m%u\033[0m: %s\n", xid, fmt);
            break;
    }
}

uint32_t xeon_phi_serial_handle_recv(void)
{
    xeon_phi_serial_ctrl_t sctrl = xeon_phi_serial_reset;
    xeon_phi_serial_data_t sdata = xeon_phi_serial_reset;

    struct xeon_phi_arg *xarg = &recv_thread_arg;

    sctrl = xeon_phi_serial_ctrl_rd(&xarg->base);

    if (!sctrl) {
        return 0;
    }

    uint32_t i = 0;
    sdata = xeon_phi_serial_data_rd(&xarg->base);
    uint8_t has_data = 0;
    uint8_t value = 0;
    while (i < 4) {
        switch (i) {
            case 0:
                has_data = xeon_phi_serial_ctrl_value0_extract(sctrl);
                value = xeon_phi_serial_data_value0_extract(sdata);
                break;
            case 1:
                has_data = xeon_phi_serial_ctrl_value1_extract(sctrl);
                value = xeon_phi_serial_data_value1_extract(sdata);
                break;
            case 2:
                has_data = xeon_phi_serial_ctrl_value2_extract(sctrl);
                value = xeon_phi_serial_data_value2_extract(sdata);
                break;
            case 3:
                has_data = xeon_phi_serial_ctrl_value3_extract(sctrl);
                value = xeon_phi_serial_data_value3_extract(sdata);
                break;
        }

        if ((has_data & 0x80) || !has_data) {
            i++;
            continue;
        }
        if (has_data != xeon_phi_serial_data) {
            debug_printf("[xeon phi %d] : ERROR invalid ctrl value.%x\n",
                         xarg->xid,
                         has_data);
        }
        /* always issue a new line */
        if (value == '\n') {
            xarg->buffer[xarg->idx] = '\0';
            xprintf(xarg->xid, xarg->buffer);
            xarg->idx = 0;
            /* there was a "flush" so flush buffer to log */
        } else if (value == 0x4) {
            xarg->buffer[xarg->idx] = '\0';
            xprintf(xarg->xid, xarg->buffer);
            xarg->idx = 0;
            /* buffer is full, flush buffer */
        } else if (xarg->idx == XEON_PHI_BUFFER_LENGTH) {
            xarg->buffer[xarg->idx] = '\0';
            xprintf(xarg->xid, xarg->buffer);
            xarg->buffer[0] = value;
            xarg->idx = 1;
            /* just store the char */
        } else {
            xarg->buffer[xarg->idx] = value;
            xarg->idx++;
        }
        i++;
    }
    /* acknowledge the data receive */
    xeon_phi_serial_ctrl_rawwr(&xarg->base, xeon_phi_serial_reset);

    return 1;
}

static int xeon_phi_recv_handler(void *arg)
{
    uint32_t nodata;

    struct xeon_phi_arg *xarg = arg;

    debug_printf("[xeon phi %d] :  receive handler started.\n", xarg->xid);

    nodata = XEON_PHI_POLL_GIVEUP;

    while (1) {
        if (!xeon_phi_serial_handle_recv()) {
            if (--nodata) {
                continue;
            }

            // reset counter
            nodata = XEON_PHI_POLL_GIVEUP;

            thread_yield();

            continue;
        }
    }

    debug_printf("[xeon phi %d] : thread terminated.\n ", xarg->xid);

    return 0;
}

/**
 * \brief initializes a new thread for receiving the serial out of the card
 *
 * The sbox memory region has already been mapped
 */
errval_t xeon_phi_serial_start_recv_thread(struct xeon_phi *phi)
{
    if (!recv_thread_arg.init) {
        xeon_phi_serial_init(phi);
    }

    xeon_phi_recv_handler(&recv_thread_arg);
    //thread_create(xeon_phi_recv_handler, xarg);

    return SYS_ERR_OK;
}

/**
 * \brief initializes the serial receiver
 *
 * \param phi   pointer to the card information
 */
errval_t xeon_phi_serial_init(struct xeon_phi *phi)
{
    memset(&recv_thread_arg, 0, sizeof(struct xeon_phi_arg));

    recv_thread_arg.xid = phi->id;

    xeon_phi_serial_initialize(&recv_thread_arg.base, XEON_PHI_MMIO_TO_SBOX(phi));

    xeon_phi_serial_ctrl_rawwr(&recv_thread_arg.base, xeon_phi_serial_reset);

    recv_thread_arg.init = 1;

    return SYS_ERR_OK;
}
