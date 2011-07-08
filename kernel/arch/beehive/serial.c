/**
 * \file
 * \brief The world's simplest serial driver.
 *
 */

/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <serial.h>

/** \brief Prints a single character to the default serial port. */
void serial_console_putchar(char c)
{
    // The RS232 is in IO position 0, register 0 XXX Should have a
    // MACRO in an arch header file to build these type of constants
    volatile int *const rs232 = (int *)0x02;

    int reg;
    while (((reg = *rs232) & 0x200) == 0);
    *rs232 = (int)0x200 | c;
}


/** \brief Reads a single character from the default serial port.
 * This function spins waiting for a character to arrive.
 */
char serial_console_getchar(void)
{
    // The RS232 is in IO position 0, register 0 XXX Should have a
    // MACRO in an arch header file to build these type of constants
    volatile int *const rs232 = (int *)0x02;

    int reg;
    while (((reg = *rs232) & 0x100) == 0);
    *rs232 = 0x100;
    return (char)(reg & 0xff);
}

void serial_debug_putchar(char c)
{
    serial_console_putchar(c);
}

char serial_debug_getchar(void)
{
    return serial_console_getchar();
}
