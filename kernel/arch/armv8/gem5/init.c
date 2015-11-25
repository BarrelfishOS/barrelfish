/*
 * Copyright (c) 2009, ETH Zurich.
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <string.h>
#include <init.h>
#include <exceptions.h>
#include <exec.h>
#include <offsets.h>
#include <paging_kernel_arch.h>
#include <serial.h>
#include <stdio.h>
#include <arm_hal.h>
#include <cpiobin.h>
#include <getopt/getopt.h>
#include <sysreg.h>
#include <elf/elf.h>
#include <barrelfish_kpi/arm_core_data.h>

#include <startup_arch.h>
#include <kernel_multiboot.h>
#include <global.h>
#include <start_aps.h>
#include <kcb.h>
#include <coreboot.h>

#define GEM5_RAM_SIZE (256UL*1024*1024)

/*
 * Used to store the address of global struct passed during boot across kernel
 * relocations.
 */
//static uint32_t addr_global;

/*
 * Kernel stack.
 *
 * This is the one and only kernel stack for a kernel instance.
 * ARMv8 requires that the stack is 16-byte aligned.
 */
uintptr_t kernel_stack[KERNEL_STACK_SIZE/sizeof(uintptr_t)]
    __attribute__ ((aligned(16)));

/*
 * Boot-up L1 page table for addresses up to 2GB (translated by TTBR0)
 *
 * We reserve double the space needed, so we can to align the pagetables to
 * their size after relocation.
 */
static union armv8_l1_entry boot_l1_low[2 * PTABLE_NUM_ENTRIES]
    __attribute__ ((aligned(PTABLE_ENTRY_SIZE)));
static union armv8_l1_entry *aligned_boot_l1_low;
/*
 * Boot-up L1 page table for addresses >=2GB (translated by TTBR1)
 */
static union armv8_l1_entry boot_l1_high[2 * PTABLE_NUM_ENTRIES]
    __attribute__ ((aligned(PTABLE_ENTRY_SIZE)));
static union armv8_l1_entry *aligned_boot_l1_high;

/* XXX - this shouldn't be here. */
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#define CONSTRAIN(x, a, b) MIN(MAX(x, a), b)

/*
 * Kernel command line variables and binding options
 */

static int timeslice = 5; // Scheduling interval in ms

static struct cmdarg cmdargs[] = {
    { "consolePort", ArgType_UInt, { .uinteger = &serial_console_port}},
    { "debugPort",   ArgType_UInt, { .uinteger = &serial_debug_port}},
    { "loglevel",    ArgType_Int, { .integer = &kernel_loglevel }},
    { "logmask",     ArgType_Int, { .integer = &kernel_log_subsystem_mask }},
    { "timeslice",   ArgType_Int, { .integer = &timeslice }},
    {NULL, 0, {NULL}}
};

static inline void __attribute__ ((always_inline))
relocate_stack(lvaddr_t offset)
{
    __asm volatile (
        "add sp, sp, %[offset]\n\t" :: [offset] "r" (offset)
    );
}

static inline void __attribute__ ((always_inline))
relocate_got_base(lvaddr_t offset)
{
    __asm volatile (
        "add x10, x10, %[offset]\n\t" :: [offset] "r" (offset)
    );
}

static void paging_init(void)
{
	sysreg_invalidate_tlb_fn();

    /* Ensure that pagetables are aligned to 4KB. */
    aligned_boot_l1_low = (union armv8_l1_entry *)
        ROUND_UP((uintptr_t)boot_l1_low, PTABLE_SIZE);
    aligned_boot_l1_high = (union armv8_l1_entry *)
        ROUND_UP((uintptr_t)boot_l1_high, PTABLE_SIZE);

    lvaddr_t vbase = MEMORY_OFFSET - KERNEL_OFFSET, base = 0;

    for(size_t i=0;
        i < INIT_L1_ENTRIES/2;
        i++, base += HUGE_PAGE_SIZE, vbase += HUGE_PAGE_SIZE)
    {
        // Create a temporary mapping at low addresses.
        paging_map_kernel_l1_block(
                (uintptr_t)aligned_boot_l1_low, base, base);

        // Alias the same region above MEMORY_OFFSET.
        paging_map_kernel_l1_block(
                (uintptr_t)aligned_boot_l1_high, vbase, base);
    }

    // Activate the new page tables.
    sysreg_write_ttbr1_el1((lpaddr_t)aligned_boot_l1_high);
    sysreg_write_ttbr0_el1((lpaddr_t)aligned_boot_l1_low);
}

void kernel_startup_early(void)
{
    const char *cmdline;
    assert(glbl_core_data != NULL);
    cmdline = MBADDR_ASSTRING(glbl_core_data->cmdline);
    parse_commandline(cmdline, cmdargs);
    timeslice = CONSTRAIN(timeslice, 1, 20);
}

/**
 * \brief Continue kernel initialization in kernel address space.
 *
 * This function resets paging to map out low memory and map in physical
 * address space, relocating all remaining data structures. It sets up exception handling,
 * initializes devices and enables interrupts. After that it
 * calls arm_kernel_startup(), which should not return (if it does, this function
 * halts the kernel).
 */
static void  __attribute__ ((noinline,noreturn)) text_init(void)
{
    errval_t errval;

    // Relocate glbl_core_data to "memory"
    glbl_core_data = (struct arm_core_data *)
        local_phys_to_mem((lpaddr_t)glbl_core_data);

    // Relocate global to "memory"
    global = (struct global*)local_phys_to_mem((lpaddr_t)global);

    // Relocate kcb_current to "memory"
    kcb_current = (struct kcb *)
        local_phys_to_mem((lpaddr_t) kcb_current);

    // Map-out low memory
    if(glbl_core_data->multiboot_flags & MULTIBOOT_INFO_FLAG_HAS_MMAP) {
        struct arm_coredata_mmap *mmap = (struct arm_coredata_mmap *)
                local_phys_to_mem(glbl_core_data->mmap_addr);
        paging_arm_reset(mmap->base_addr, mmap->length);
    } else {
        paging_arm_reset(PHYS_MEMORY_START, GEM5_RAM_SIZE);
    }

    printf("Exception vectors (VBAR_EL1): %p\n", &vectors);
    sysreg_write_vbar_el1((uint64_t)&vectors);

    //kernel_startup_early();

    //initialize console
    serial_console_init(true);

    // do not remove/change this printf: needed by regression harness
    printf("Barrelfish CPU driver starting on ARMv8 Board id 0x%08"PRIx32"\n",
           hal_get_board_id());
    printf("The address of paging_map_kernel_section is %p\n",
           paging_map_kernel_section);

    errval = serial_debug_init();
    if (err_is_fail(errval)) {
        printf("Failed to initialize debug port: %d", serial_debug_port);
    }

    my_core_id = hal_get_cpu_id();

    gic_init();
    if(hal_cpu_is_bsp()) {
        // init SCU if more than one core present
        if(scu_get_core_count() > 4) {
            panic("ARM SCU doesn't support more than 4 cores!");
        }
        if(scu_get_core_count() > 1) {
            scu_enable();
        }
    }

    pit_init(timeslice, 0);
    //pit_init(timeslice, 1);
    tsc_init();

    coreboot_set_spawn_handler(CPU_ARM8, start_aps_arm_start);
    arm_kernel_startup();
}

/**
 * Entry point called from boot.S for bootstrap processor.
 * if is_bsp == true, then pointer points to multiboot_info
 * else pointer points to a global struct
 */

void arch_init(void *pointer)
{
    void __attribute__ ((noreturn)) (*reloc_text_init)(void) = 
        		(void *)local_phys_to_mem((lpaddr_t)text_init);

    struct Elf64_Shdr *rela, *symtab;
    struct arm_coredata_elf *elf = NULL;

    serial_early_init(serial_console_port);

    if(hal_cpu_is_bsp()) {
        struct multiboot_info *mb = (struct multiboot_info *)pointer;
        elf = (struct arm_coredata_elf *)&mb->syms.elf;
        memset(glbl_core_data, 0, sizeof(struct arm_core_data));
        glbl_core_data->start_free_ram =
                        ROUND_UP(max(multiboot_end_addr(mb),
                                     (uintptr_t)&kernel_final_byte),
                                 BASE_PAGE_SIZE);

        glbl_core_data->mods_addr = mb->mods_addr;
        glbl_core_data->mods_count = mb->mods_count;
        glbl_core_data->cmdline = mb->cmdline;
        glbl_core_data->mmap_length = mb->mmap_length;
        glbl_core_data->mmap_addr = mb->mmap_addr;
        glbl_core_data->multiboot_flags = mb->flags;

        // Construct the global structure
        memset(&global->locks, 0, sizeof(global->locks));
    } else {
        global = (struct global *)GLOBAL_VBASE;
        memset(&global->locks, 0, sizeof(global->locks));
        struct arm_core_data *core_data =
                (struct arm_core_data*)((lvaddr_t)&kernel_first_byte - BASE_PAGE_SIZE);
        glbl_core_data = core_data;
        
		glbl_core_data->cmdline = (lpaddr_t)&core_data->kernel_cmdline;
        my_core_id = core_data->dst_core_id;
        elf = &core_data->elf;
    }

    // XXX: print kernel address for debugging with gdb
    printf("Kernel starting at address 0x%"PRIxLVADDR"\n", &kernel_first_byte);

    // Find relocation section
    rela = elf64_find_section_header_type((struct Elf64_Shdr *)
                ((uintptr_t)elf->addr),
                elf->num, SHT_RELA);

    if (rela == NULL) {
        panic("Kernel image does not include relocation section!");
    }

    // Find symtab section
    symtab = elf64_find_section_header_type((struct Elf64_Shdr *)(lpaddr_t)elf->addr,
                elf->num, SHT_DYNSYM);

    if (symtab == NULL) {
        panic("Kernel image does not include symbol table!");
    }

    printf("Relocating to %p\n",
           MEMORY_OFFSET + (lvaddr_t)&kernel_first_byte);

    paging_init();

    sysreg_enable_mmu();

    // Relocate kernel image for top of memory
    elf64_relocate(MEMORY_OFFSET + (lvaddr_t)&kernel_first_byte,
            (lvaddr_t)&kernel_first_byte,
            (struct Elf64_Rela *)(rela->sh_addr - START_KERNEL_PHYS +
                                  &kernel_first_byte),
            rela->sh_size,
            (struct Elf64_Sym *)(symtab->sh_addr - START_KERNEL_PHYS +
                                 &kernel_first_byte),
            symtab->sh_size,
            START_KERNEL_PHYS, &kernel_first_byte);
    /*** Aliased kernel available now -- low memory still mapped ***/

    // Relocate stack to aliased location
    relocate_stack(MEMORY_OFFSET);

    //relocate got_base register to aliased location
    relocate_got_base(MEMORY_OFFSET);

    // Call aliased text_init() function and continue initialization
    reloc_text_init();
}
