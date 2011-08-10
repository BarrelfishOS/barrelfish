/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/* This file includes lwIP's netdb.h -- purely for compatibility */

#ifndef NETDB_H
#define NETDB_H

#include <lwip/netdb.h>

struct hostent *gethostbyname(const char *name);
int getaddrinfo(const char *nodename,
                const char *servname,
                const struct addrinfo *hints,
                struct addrinfo **res);
void freeaddrinfo(struct addrinfo *ai);
const char *gai_strerror(int ecode);

#endif
