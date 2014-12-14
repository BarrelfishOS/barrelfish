/**
 * \file
 * \brief Fish x86 specific commands
 */
/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <acpi_client/acpi_client.h>

#include "fish.h"

static bool acpi_connected = false;

int reset(int argc, char *argv[])
{
    if (!acpi_connected) {
        int r = connect_to_acpi();
        assert(r == 0);
        acpi_connected = true;
    }
    return acpi_reset();
}

int poweroff(int argc, char *argv[])
{
    if (!acpi_connected) {
        int r = connect_to_acpi();
        assert(r == 0);
        acpi_connected = true;
    }
    return acpi_sleep(4);
}