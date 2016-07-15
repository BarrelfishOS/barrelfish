/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef AHCI_TEST
#define AHCI_TEST

#include <stdlib.h>
#include <stdbool.h>

enum AhciTest {
    AhciTest_READ,
    AhciTest_WRITE,
    AhciTest_BASIC,
    AhciTest_VERIFY,
};

void test_runner(int n, ...);

void ahci_simple_test(void);
void ahci_perf_sequential(size_t buffer_size, size_t block_size, bool write);
void ahci_verify_sequential(size_t buffer_size, size_t block_size);


#endif // AHCI_TEST
