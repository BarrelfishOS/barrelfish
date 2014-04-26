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

#include "xeon_phi.h"

#define XEON_PHI_BUFFER_LENGTH 0x400
#define XEON_PHI_POLL_GIVEUP   0x100

static xeon_phi_serial_t mmio_serial;

uint8_t xeon_phi_id = 0;

static inline void xprintf(uint8_t xid,
                           char *fmt)
{
    printf("\033[34m>> XEON_PHI\033[31m%u\033[0m: %s", xid, fmt);
}

struct xeon_phi_arg
{
    xeon_phi_serial_t base;
    uint8_t xid;
    char buffer[XEON_PHI_BUFFER_LENGTH + 1];
    uint32_t idx;
};

static int xeon_phi_recv_handler(void *arg)
{
    struct xeon_phi_arg *xarg = arg;
    uint32_t nodata;

    xeon_phi_serial_ctrl_t sctrl = xeon_phi_serial_reset;
    xeon_phi_serial_data_t sdata = xeon_phi_serial_reset;

    debug_printf("xeon phi receive handler started. %d\n", xarg->xid);

    xeon_phi_serial_ctrl_rawwr(&mmio_serial, xeon_phi_serial_reset);

    nodata = XEON_PHI_POLL_GIVEUP;

    while (1) {
        sctrl = xeon_phi_serial_ctrl_rd(&xarg->base);

        if (!sctrl) {
            if (--nodata) {
                continue;
            }
            // reset counter
            nodata = XEON_PHI_POLL_GIVEUP;

            thread_yield();

            continue;
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
                value = xeon_phi_serial_data_value0_extract(sdata);
                break;
            case 2:
                has_data = xeon_phi_serial_ctrl_value2_extract(sctrl);
                value = xeon_phi_serial_data_value0_extract(sdata);
                break;
            case 3:
                has_data = xeon_phi_serial_ctrl_value3_extract(sctrl);
                value = xeon_phi_serial_data_value0_extract(sdata);
                break;
            }

            if (has_data & 0x80) {
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
    }

    debug_printf("[xeon phi %d] : thread terminated.\n ", xarg->xid);

    free(xarg);
    return 0;
}

/**
 * \brief initializes a new thread for receiving the serial out of the card
 *
 * The sbox memory region has already been mapped
 */
errval_t serial_start_recv_thread(struct xeon_phi *phi)
{
    struct xeon_phi_arg *xarg = malloc(sizeof(struct xeon_phi_arg));
    if (xarg == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }
    memset(xarg, 0, sizeof(struct xeon_phi_arg));

    xarg->xid = phi->id;

    xeon_phi_serial_initialize(&xarg->base, XEON_PHI_MMIO_TO_SBOX(phi));

    /*
     * xxx: maybe store the thread pointer
     */
    thread_create(xeon_phi_recv_handler, xarg);

    return SYS_ERR_OK;
}
