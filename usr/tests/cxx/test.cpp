/*
 * Copyright (c) 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <iostream>

#include "cxxtest.hpp"

int main(int argc,
         char *argv[])
{
    std::cout << "Hello World!" << std::endl;
    std::cout << "This is cxx test" << std::endl;
    stl_list_test();
    stl_vector_test();
    stl_map_test();
    stl_exception_test();
    cx11_test();
    stl_thread_test();

    stl_chrono_test();

    std::cout << "Tests done: SUCCESS" << std::endl;
    return 0;
}
