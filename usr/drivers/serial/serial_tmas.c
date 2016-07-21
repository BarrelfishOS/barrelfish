/**
 * \file
 * \brief Serial port driver.
 */

/*
 * Copyright (c) 2007, 2008, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <pci/pci.h>
#include <barrelfish/inthandler.h>
#include <driverkit/driverkit.h>
#include "serial.h"
#include <dev/pl011_uart_dev.h>

errval_t serial_init(uint16_t lportbase, uint8_t irq)
{
    // offer service now we're up
    start_service();
    return SYS_ERR_OK;
}


void serial_write(char *c, size_t len)
{
    debug_printf("%.*s", len, c);
}
