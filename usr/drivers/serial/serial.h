/**
 * \file
 * \brief Serial port driver.
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef SERIAL_H
#define SERIAL_H

void serial_write(char *c, size_t len);
void serial_poll(void);
int serial_init(uint16_t portbase, uint8_t irq);
void start_service(void);
void serial_input(char *data, size_t length);

#endif
