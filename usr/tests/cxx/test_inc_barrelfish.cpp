/*
 * Copyright (c) 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

/// XXX: there seems to be some issues with this.., disabling this warning for now
#pragma GCC diagnostic ignored "-Wshadow"
#include <iostream>
#pragma GCC diagnostic error "-Wshadow"

#include <barrelfish/barrelfish.h>


#include "cxxtest.hpp"

int main(int argc,
         char *argv[])
{
    std::cout << "Hello World!" << std::endl;

    std::cout << "Tests done: SUCCESS" << std::endl;
    return 0;
}
