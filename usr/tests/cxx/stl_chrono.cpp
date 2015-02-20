/*
 * Copyright (c) 2015 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <iostream>
#include <chrono>
#include <thread>
#include <string>

#include "cxxtest.hpp"

static void func(int d)
{
    using namespace std::chrono;
    steady_clock::time_point start = steady_clock::now();
    steady_clock::time_point current, notified;
    while (true) {
        current = steady_clock::now();
        if (duration_cast<std::chrono::duration<double>>
                (current - start).count() >= d){
            break;
        }
        else {
            // notify user how much time of the experiment has passed
            long diff = duration_cast<std::chrono::duration
                <double>>(current - notified).count();
            if (diff >= (d/100.0)) {
                notified = high_resolution_clock::now();
                diff = duration_cast<std::chrono::duration
                    <double>>(notified - start).count();
                std::cout << (int) (100.0 * diff / d)
                    << "% completed." << std::endl;
            }
            // sleep for a while
            std::this_thread::sleep_for(std::chrono::milliseconds(500));
        }
    }
}

static void timeout_test(void) {
    auto s = std::chrono::steady_clock::now();
    std::thread t(func, 10);
    t.join();
    auto d = std::chrono::duration_cast<std::chrono::seconds>(std::chrono::steady_clock::now() - s);
    std::cout << ((d.count() >= 10) ? "PASS" : "FAIL") << std::endl;
}

void stl_chrono_test(void)
{
    timeout_test();
}
