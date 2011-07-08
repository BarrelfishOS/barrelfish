/**
 * \file
 * \brief Commandline parameter parsing.
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef GETOPT_H
#define GETOPT_H

enum argtype {
    ArgType_Int,
    ArgType_Bool,
    ArgType_Custom
};

typedef int (*cmdarg_handler)(const char *arg, const char *val);

struct cmdarg {
    const char          *arg;
    enum argtype        type;

    union {
        int             *integer;
        bool            *boolean;
        cmdarg_handler  handler;
    } var;
};

void parse_commandline(const char *cmdline, struct cmdarg *cmdargs);

#endif // GETOPT_H
