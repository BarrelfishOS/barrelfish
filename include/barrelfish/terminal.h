/**
 * \file
 * \brief Terminal emulator.
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef TERMINAL_H
#define TERMINAL_H

#include <sys/cdefs.h>

__BEGIN_DECLS

/**
 * \brief Callback function to be called when input arrives at the terminal.
 *
 * This function must not alter the input string.
 */
typedef void (*terminal_input_handler) (void * user_data, const char *data,
                                        size_t length);

errval_t terminal_register_input_handler (terminal_input_handler handler,
                                          void * user_data);
void terminal_unregister_input_handler (terminal_input_handler handler);

size_t terminal_write(const char *data, size_t length);
size_t terminal_read(char *data, size_t count);

void terminal_input(char *data, size_t length);
errval_t terminal_init(void);
errval_t terminal_want_stdin(unsigned int sources);

// XXX: arguments to terminal_want_stdin (bitmask)
#define TERMINAL_SOURCE_SERIAL   0x1
#define TERMINAL_SOURCE_KEYBOARD 0x2

__END_DECLS

#endif
