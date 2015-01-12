/**
 * \file
 * \brief Fish arm specific commands
 */
/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>

#include <stdlib.h>
#include "fish.h"

int reset(int argc, char *argv[])
{
   printf("RESET NOT SUPPORTED ON ARM\n");
   return EXIT_SUCCESS;
}

int poweroff(int argc, char *argv[])
{
   printf("POWER OFF NOT SUPPORTED ON ARM\n");
   return EXIT_SUCCESS;
}
