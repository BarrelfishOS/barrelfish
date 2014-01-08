/**
 * \file
 * \brief Architecture-specific microbenchmarks.
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <microbenchmarks.h>
#include <arch/x86/apic.h>
#include <x86.h>

// address space switch (mov to cr3)
static int asswitch_func(struct microbench *mb)
{
    uint64_t start, end;
    uint64_t asvalue;

    // Put the cr3 value in the asvalue register for now
    __asm__ __volatile__("mov %%cr3, %0" : "=r" (asvalue));

    start = rdtscp();
    for (int i = 0; i < MICROBENCH_ITERATIONS; i++) {
        __asm__ __volatile__(
            "mov %0, %%cr3"
            :
            : "r" (asvalue));
    }
    end = rdtscp();

    mb->result = end - start;

    return 0;
}

static int wrmsr_func(struct microbench *mb)
{
    uint64_t start, end;

    start = rdtscp();
    for (int i = 0; i < MICROBENCH_ITERATIONS; i++) {
        wrmsr(MSR_IA32_FSBASE, 0);
    }
    end = rdtscp();

    mb->result = end - start;

    return 0;
}

static int init_ipi_func(struct microbench *mb)
{
    uint64_t start, end;

    start = rdtscp();
    for (int i = 0; i < MICROBENCH_ITERATIONS; i++) {
        apic_send_init_assert(1, xapic_none);
        apic_send_init_deassert();
    }
    end = rdtscp();

    mb->result = end - start;

    return 0;
}

static int start_ipi_func(struct microbench *mb)
{
    uint64_t start, end;

    start = rdtscp();
    for (int i = 0; i < MICROBENCH_ITERATIONS; i++) {
        apic_send_start_up(1, xapic_none, 0x6);
    }
    end = rdtscp();

    mb->result = end - start;

    return 0;
}

struct microbench arch_benchmarks[] = {
    {
        .name = "wrmsr",
        .run_func = wrmsr_func
    },
    {
        .name = "address space switch (mov to cr3)",
        .run_func = asswitch_func
    },
    {
        .name = "Send INIT IPI",
        .run_func = init_ipi_func
    },
    {
        .name = "Send SIPI",
        .run_func = start_ipi_func
    }
};

size_t arch_benchmarks_size = sizeof(arch_benchmarks) / sizeof(struct microbench);
