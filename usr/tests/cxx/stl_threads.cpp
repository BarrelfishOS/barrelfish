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
#include <string>
#include <thread>

#include "cxxtest.hpp"

static std::mutex tmx;
static int thread_counter;
static void thread_func(int v)
{
    std::lock_guard<std::mutex> l(tmx);
    std::cout << "thread " << v << std::endl;
    thread_counter++;
}

static void cxx11_vector_of_threads(void)
{
    int i;
    std::vector<std::thread> threads;
    for (i=0; i<2; ++i) {
        std::cout << "emplacing thread " << i << std::endl;
        threads.emplace_back(thread_func, i);
        std::cout << "emplaced thread " << i << std::endl;
    }
    i = 0;
    for (auto &t: threads) {
        std::cout << "joining thread " << i << std::endl;
        t.join();
        std::cout << "joined thread " << i << std::endl;
        i++;
    }
    std::cout << "threads joined, thread_counter=" << thread_counter << std::endl;
}

void stl_thread_test(void)
{
    cxx11_vector_of_threads();
}
