/*
 * Copyright (c) 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

int dos2unixfn(const unsigned char dn[11], unsigned char *un, int lower);
int unix2dosfn(const unsigned char *un, unsigned char dn[12], int unlen, unsigned int gen);
