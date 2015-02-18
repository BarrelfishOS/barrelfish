/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <iostream>
#include <vector>
#include <unordered_map>
#include <string>

#include "cxxtest.hpp"

static std::vector<std::string> mapkeys {{"0", "1", "2"}};

static std::unordered_map<std::string, uint32_t> simple_map
{{"0", 0}, {"2", 2}, {"1", 1}};

std::unordered_map<std::string, uint16_t> subscription_cost_to_id
{{"0", 0}, {"10", 1}, {"20", 2}, {"50", 3}};

static void stl_unordered_map_test_simple(void)
{
    std::cout << "STL unordered map: simple test" << std::endl;
    for (auto it = mapkeys.begin(); it != mapkeys.end(); ++it) {
        std::cout << *it << "->" << simple_map[*it] << std::endl;
    }
}

void stl_map_test(void)
{
    stl_unordered_map_test_simple();
}

#ifndef BARRELFISH
int main(void)
{
    stl_map_test();
    return 0;
}
#endif
