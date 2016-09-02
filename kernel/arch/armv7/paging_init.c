/*
 * Copyright (c) 2009-2013,2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>

#include <barrelfish_kpi/arm_core_data.h>
#include <cp15.h>
#include <paging_kernel_arch.h>

#define MSG(format, ...) printk( LOG_NOTE, "ARMv7-A: "format, ## __VA_ARGS__ )

bool paging_mmu_enabled(void)
{
    return false;
}

union arm_l1_entry l1_low [ARM_L1_MAX_ENTRIES]
    __attribute__((aligned(ARM_L1_ALIGN), section(".boot.tables")));
union arm_l1_entry l1_high[ARM_L1_MAX_ENTRIES]
    __attribute__((aligned(ARM_L1_ALIGN), section(".boot.tables")));
union arm_l2_entry l2_vec[ARM_L2_MAX_ENTRIES]
    __attribute__((aligned(ARM_L2_ALIGN), section(".boot.tables")));

static void map_kernel_section_lo(lvaddr_t va, union arm_l1_entry l1)
{
    assert( va < MEMORY_OFFSET );
    l1_low[ARM_L1_OFFSET(va)] = l1;
}

static void map_kernel_section_hi(lvaddr_t va, union arm_l1_entry l1)
{
    assert( va >= MEMORY_OFFSET );
    l1_high[ARM_L1_OFFSET(va)] = l1;
}

/* These are initialised by the linker, so we know where the boot driver code
 * is. */
extern char boot_start, boot_end;

/**
 * /brief Return an L1 page table entry to map a 1MB 'section' of
 * device memory located at physical address 'pa'.
 */
static union arm_l1_entry make_dev_section(lpaddr_t pa)
{
    union arm_l1_entry l1;

    l1.raw = 0;
    l1.section.type = L1_TYPE_SECTION_ENTRY;
    // l1.section.tex       = 1;
    l1.section.bufferable   = 0;
    l1.section.cacheable    = 0;
    l1.section.ap10         = 3; // prev value: 3 // RW/NA RW/RW
    // l1.section.ap10         = 1;    // RW/NA
    l1.section.ap2          = 0;
    l1.section.base_address = ARM_L1_SECTION_NUMBER(pa);
    return l1;
}

/**
 * /brief Return an L1 page table entry to map a 1MB 'section' of RAM
 * located at physical address 'pa'.
 */
static union arm_l1_entry make_ram_section(lpaddr_t pa)
{
    // Must be in the 1GB RAM region.
    assert(pa >= phys_memory_start && pa < (phys_memory_start + 0x40000000));
    union arm_l1_entry l1;

    l1.raw = 0;
    l1.section.type = L1_TYPE_SECTION_ENTRY;

    /* The next three fields (tex,b,c) don't mean quite what their names
       suggest.  This setting gives inner and outer write-back, write-allocate
       cacheable memory.  See ARMv7 ARM Table B3-10. */
    l1.section.tex          = 1;
    l1.section.cacheable    = 1;
    l1.section.bufferable   = 1;

    l1.section.execute_never = 0; /* XXX - We may want to revisit this. */

    l1.section.not_global    = 0; /* Kernel mappings are global. */
    l1.section.shareable     = 1; /* Cache coherent. */

    l1.section.ap10         = 1;  /* Kernel RW, no user access. */
    l1.section.ap2          = 0;

    l1.section.base_address = ARM_L1_SECTION_NUMBER(pa);
    return l1;
}

/**
 * Create kernel page tables.
 *
 * We use 1MB (ARM_L1_SECTION_BYTES) pages (sections) with a single-level table.
 * This allows 1MB*4k (ARM_L1_MAX_ENTRIES) = 4G per pagetable.
 *
 * Hardware details can be found in:
 * ARM Architecture Reference Manual, ARMv7-A and ARMv7-R edition
 *   B3: Virtual Memory System Architecture (VMSA)
 */
void paging_init(lpaddr_t ram_base, size_t ram_size,
                 struct arm_core_data *boot_core_data)
{
    /**
     * Make sure our page tables are correctly aligned in memory
     */
    assert(ROUND_UP((lpaddr_t)l1_low, ARM_L1_ALIGN) == (lpaddr_t)l1_low);
    assert(ROUND_UP((lpaddr_t)l1_high, ARM_L1_ALIGN) == (lpaddr_t)l1_high);

    MSG("Initialising kernel paging, using RAM at %08x-%08x\n",
        ram_base, ram_base + (ram_size - 1));

    /**
     * On many ARMv7-A platforms, physical RAM (phys_memory_start) is the same
     * as the offset of mapped physical memory within virtual address space
     * (phys_memory_start).  Some platforms (such as the Zynq) break this
     * rule, and thus we need to be very careful about how we enable the MMU.
     */
    if(MEMORY_OFFSET != phys_memory_start &&
       (lpaddr_t)&boot_end >= MEMORY_OFFSET) {
        /* If the init code's physical addresses overlap the kernel window,
         * they must be unchanged when we map those virtual addresses to RAM.
         * Otherwise our code will suddenly vanish.  This means that on
         * platforms with RAM somewhere other than 80000000, all
         * initialisation code should be allocated together, somewhere in the
         * first 2GB. */
        panic("The kernel memory window must either be 1-1, or init code\n"
              "must lie entirely outside it.\n");
    }

    /**
     * Zero the page tables: this has the effect of marking every PTE
     * as invalid.
     */
    memset(&l1_low,  0, sizeof(l1_low));
    memset(&l1_high, 0, sizeof(l1_high));
    memset(&l2_vec,  0, sizeof(l2_vec));

    /* Pass the addresses of the page tables to the CPU driver. */
    boot_core_data->kernel_l1_low=  (lpaddr_t)l1_low;
    boot_core_data->kernel_l1_high= (lpaddr_t)l1_high;
    boot_core_data->kernel_l2_vec=  (lpaddr_t)l2_vec;

    /**
     * Now we lay out the kernel's virtual address space.
     *
     * 00000000-7FFFFFFFF: 1-1 mappings (hardware we have not mapped
     *                     into high kernel space yet, and the init
     *                     code that is currently executing, in case
     *                     RAM doesn't start at 80000000).
     * 80000000-BFFFFFFFF: 1-1 mappings (this is 1GB of RAM).
     * C0000000-FEFFFFFFF: On-demand mappings of hardware devices,
     *                     allocated descending from DEVICE_OFFSET.
     * FF000000-FFEFFFFFF: Unallocated.
     * FFF00000-FFFFFFFFF: L2 table, containing:
     *      FFF00000-FFFEFFFF: Unallocated
     *      FFFF0000-FFFFFFFF: Exception vectors
     */    
    lvaddr_t vbase;
    lpaddr_t pbase;
    size_t i;
    /* Map the first 2GB 1-1. */
    vbase= 0; pbase= 0;
    for (i=0; i < ARM_L1_MAX_ENTRIES/2; i++) {
        map_kernel_section_lo(vbase, make_dev_section(pbase));
        vbase += ARM_L1_SECTION_BYTES;
        pbase += ARM_L1_SECTION_BYTES;
    }
    /* Map the next 1GB to the first 1GB of RAM. */
    vbase= MEMORY_OFFSET; pbase= phys_memory_start;
    for (i=0; i < ARM_L1_MAX_ENTRIES/4; i++) {
        map_kernel_section_hi(vbase, make_ram_section(pbase));
        vbase += ARM_L1_SECTION_BYTES;
        pbase += ARM_L1_SECTION_BYTES;
    }
}

void enable_mmu(lpaddr_t ttbr0, lpaddr_t ttbr1)
{
    /**
     * TTBCR: Translation Table Base Control register.
     *  TTBCR.N is bits[2:0]
     * In a TLB miss TTBCR.N determines whether TTBR0 or TTBR1 is used as the
     * base address for the translation table walk in memory:
     *  N == 0 -> always use TTBR0
     *  N >  0 -> if VA[31:32-N] > 0 use TTBR1 else use TTBR0
     *
     * TTBR0 is typically used for processes-specific addresses
     * TTBR1 is typically used for OS addresses that do not change on context
     *       switch
     *
     * set TTBCR.N = 1 to use TTBR1 for VAs >= MEMORY_OFFSET (=2GB)
     */
    uint32_t sctlr= cp15_read_sctlr();
    MSG(" MMU is currently ");
    if(sctlr & BIT(2)) {
                       printf("enabled.\n");
                       panic("MMU is enabled.\n");
    }
    else               printf("disabled.\n");
    MSG(" Alignment checking is currently ");
    if(sctlr & BIT(1)) printf("enabled.\n");
    else               printf("disabled.\n");
    MSG(" Caches are currently ");
    if(sctlr & BIT(0)) {
                       printf("enabled.\n");
                       panic("Caches are enabled.\n");
    }
    else               printf("disabled.\n");

    /* Force all outstanding operations to complete. */
    dsb(); isb();

    /* Ensure that the local caches and TLBs have no stale data. */
    invalidate_data_caches_pouu(false);
    invalidate_instruction_cache();
    invalidate_tlb();

    /* Install the new tables. */
    cp15_write_ttbr1(ttbr1);
    cp15_write_ttbr0(ttbr0);

    /* Set TTBR0&1 to each map 2GB. */
    #define TTBCR_N 1
    uint32_t ttbcr = cp15_read_ttbcr();
    ttbcr =  (ttbcr & ~MASK(3)) | TTBCR_N;
    cp15_write_ttbcr(ttbcr);
    STATIC_ASSERT(1UL<<(32-TTBCR_N) == MEMORY_OFFSET, "");
    #undef TTBCR_N

    /* Ensure all memory accesses have completed. */
    dsb();

    /* All 16 domains are set to 'client', and otherwise unused. */
    cp15_write_dacr(0x55555555);

    /* Start in ASID 0. */
    cp15_write_contextidr(0);

    /* Enable caches and the MMU.
       If RAM on this platform starts at 80000000, then this is quite simple,
       and we'll just keep executing without any trouble.  If RAM is somewhere
       else (say 0), then we've just created a duplicate mapping to the code
       that we're running, inside the kernel window, and we'll continue
       executing using the uncached device mappings we just created, until we
       call arch_init_2() at its kernel-window address.  This relies on having
       position-independent code.
     */
    sctlr= cp15_read_sctlr();
    sctlr|= BIT(12); /* I-Cache enabled. */
    sctlr|= BIT(11); /* Branch prediction enabled. */
    sctlr|= BIT(2);  /* D-Cache and unified caches enabled. */
    sctlr&= ~BIT(1); /* Alignment faults disabled. */
    sctlr|= BIT(0);  /* Level 1 MMU enabled. */
    cp15_write_sctlr(sctlr);

    /* Synchronise control register changes. */
    isb();

    /* We're now executing either through the new, cached kernel window
     * mappings, or through the uncached device mappings.  In either case, no
     * addresses have changed yet.  The one wrinkle is that the UART may have
     * just disappeared, if its physical address was >80000000.  Thus it's not
     * safe to print until we're definitely executing in the kernel window,
     * and have remapped it. */

    /* Any TLB entries will be stale, although there shouldn't be any. */
    invalidate_tlb();

    /* Ensure no memory accesses or instruction fetches occur before the MMU
     * is fully enabled. */
    dsb();
}
