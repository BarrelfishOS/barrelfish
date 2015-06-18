/**
 * \file
 * \brief x86-64 architecture initialization.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <string.h>
#include <stdio.h>
#include <paging_kernel_arch.h>
#include <elf/elf.h>
#include <init.h>
#include <irq.h>
#include <x86.h>
#include <serial.h>
#include <kernel_multiboot.h>
#include <kernel_boot_param.h>
#include <syscall.h>
#include <getopt/getopt.h>
#include <exec.h>
#include <kputchar.h>
#include <arch/x86/conio.h>
#include <arch/x86/pic.h>
#include <arch/x86/apic.h>
#include <arch/x86/mcheck.h>
#include <arch/x86/perfmon.h>
#include <arch/x86/rtc.h>
#include <target/x86/barrelfish_kpi/coredata_target.h>
#include <arch/x86/timing.h>
#include <arch/x86/startup_x86.h>
#include <arch/x86/start_aps.h>
#include <arch/x86/ipi_notify.h>
#include <barrelfish_kpi/cpu_arch.h>
#include <target/x86_64/barrelfish_kpi/cpu_target.h>
#include <barrelfish_kpi/asm_inlines_arch.h>
#include <coreboot.h>
#include <kcb.h>

#include <xeon_phi.h>
#include <xeon_phi/xeon_phi.h>

#include <dev/xapic_dev.h> // XXX
#include <dev/ia32_dev.h>
#include <dev/amd64_dev.h>

#include <linux_host.h>

/**
 * Used to store the address of global struct passed during boot across kernel
 * relocations.
 */
static uint64_t addr_global;

/**
 * \brief Kernel stack.
 *
 * This is the one and only kernel stack for a kernel instance.
 */
uintptr_t k1om_kernel_stack[K1OM_KERNEL_STACK_SIZE / sizeof(uintptr_t)];

/**
 * \brief Global Task State Segment (TSS).
 *
 * This is the global, static and only Task State Segment (TSS). It is used
 * for interrupt and exception handling (stack setup) while in user-space.
 */
static struct task_state_segment tss __attribute__ ((aligned (4)));

/**
 * \brief Global Descriptor Table (GDT) for processor this kernel is running on.
 *
 * This descriptor table is completely static, as segments are basically
 * turned off in 64-bit mode. They map flat-mode code and stack segments for
 * both kernel- and user-space and the only Task State Segment (TSS).
 */
union segment_descriptor gdt[] __attribute__ ((aligned (4))) = {
    [NULL_SEL] = {   // Null segment
        .raw = 0
    },
    [KCODE_SEL] = {   // Kernel code segment
        .d = {
            .lo_limit = 0xffff,
            .lo_base = 0,
            .type = 0xa,
            .system_desc = 1,
            .privilege_level = SEL_KPL,
            .present = 1,
            .hi_limit = 0xf,
            .available = 0,
            .long_mode = 1,
            .operation_size = 0,
            .granularity = 1,
            .hi_base = 0
        }
    },
    [KSTACK_SEL] = {   // Kernel stack segment
        .d = {
            .lo_limit = 0xffff,
            .lo_base = 0,
            .type = 2,
            .system_desc = 1,
            .privilege_level = SEL_KPL,
            .present = 1,
            .hi_limit = 0xf,
            .available = 0,
            .long_mode = 1,
            .operation_size = 0,
            .granularity = 1,
            .hi_base = 0
        }
    },
    [USTACK_SEL] = {   // User stack segment
        .d = {
            .lo_limit = 0xffff,
            .lo_base = 0,
            .type = 2,
            .system_desc = 1,
            .privilege_level = SEL_UPL,
            .present = 1,
            .hi_limit = 0xf,
            .available = 0,
            .long_mode = 1,
            .operation_size = 0,
            .granularity = 1,
            .hi_base = 0
        }
    },
    [UCODE_SEL] = {   // User code segment
        .d = {
            .lo_limit = 0xffff,
            .lo_base = 0,
            .type = 0xa,
            .system_desc = 1,
            .privilege_level = SEL_UPL,
            .present = 1,
            .hi_limit = 0xf,
            .available = 0,
            .long_mode = 1,
            .operation_size = 0,
            .granularity = 1,
            .hi_base = 0
        }
    },
    [TSS_LO_SEL] = {   // Global Task State Segment (TSS), lower 8 bytes
        .sys_lo = {
            .lo_limit = sizeof(tss) & 0xffff,
            .type = SDT_SYSTSS,
            .privilege_level = SEL_KPL,
            .present = 1,
            .hi_limit = (sizeof(tss) >> 16) & 0xf,
            .available = 0,
            .granularity = 0,
        }
    },
    [TSS_HI_SEL] = {   // Global Task State Segment (TSS), upper 8 bytes
        .sys_hi = {
            .base = 0
        }
    },
    [LDT_LO_SEL] = {    // Local descriptor table (LDT), lower 8 bytes
        .sys_lo = {
            .lo_limit = 0, // # 4k pages (since granularity = 1)
            .lo_base = 0, // changed by context switch path when doing lldt
            .type = 2, // LDT
            .privilege_level = SEL_UPL,
            .present = 1,
            .hi_limit = 0,
            .available = 0,
            .granularity = 1,
            .hi_base = 0
        }
    },
    [LDT_HI_SEL] = {    // Local descriptor table (LDT), upper 8 bytes
        .sys_hi = {
            .base = 0 // changed by context switch path when doing lldt
        }
    },
};

union segment_descriptor *ldt_descriptor = &gdt[LDT_LO_SEL];

/**
 * Bootup PML4, used to map both low (identity-mapped) memory and relocated
 * memory at the same time.
 */
static union x86_64_pdir_entry boot_pml4[PTABLE_SIZE]
__attribute__ ((aligned(BASE_PAGE_SIZE)));

/**
 * Bootup low-map PDPT and hi-map PDPT.
 */
static union x86_64_pdir_entry boot_pdpt[PTABLE_SIZE]
__attribute__ ((aligned(BASE_PAGE_SIZE))),
    boot_pdpt_hi[PTABLE_SIZE] __attribute__ ((aligned(BASE_PAGE_SIZE)));

/**
 * Bootup low-map PDIR, hi-map PDIR, and 1GB PDIR.
 */
static union x86_64_ptable_entry boot_pdir[PTABLE_SIZE]
__attribute__ ((aligned(BASE_PAGE_SIZE)));

static union x86_64_ptable_entry boot_pdir_hi[PTABLE_SIZE]
__attribute__ ((aligned(BASE_PAGE_SIZE)));

static union x86_64_ptable_entry boot_pdir_1GB[PTABLE_SIZE]
__attribute__ ((aligned(BASE_PAGE_SIZE)));

static union x86_64_ptable_entry boot_pdir_mmio[PTABLE_SIZE]
__attribute__ ((aligned(BASE_PAGE_SIZE)));

/**
 * This flag is set to true once the IDT is initialized and exceptions can be
 * caught.
 */
bool idt_initialized = false;

/**
 * \brief Setup bootup page table.
 *
 * This function sets up the page table needed to boot the kernel
 * proper.  The table identity maps the first 1 GByte of physical
 * memory in order to have access to various data structures and the
 * first MByte containing bootloader-passed data structures. It also
 * identity maps the local copy of the kernel in low memory and
 * aliases it in kernel address space.
 *
 * \param base  Start address of kernel image in physical address space.
 * \param size  Size of kernel image.
 */
static void paging_init(lpaddr_t base, size_t size)
{
    lvaddr_t vbase = local_phys_to_mem(base);

    // Align base to kernel page size
    if(base & X86_64_MEM_PAGE_MASK) {
        size += base & X86_64_MEM_PAGE_MASK;
        base -= base & X86_64_MEM_PAGE_MASK;
    }

    // Align vbase to kernel page size
    if(vbase & X86_64_MEM_PAGE_MASK) {
        vbase -= vbase & X86_64_MEM_PAGE_MASK;
    }

    // Align size to kernel page size
    if(size & X86_64_MEM_PAGE_MASK) {
        size += X86_64_MEM_PAGE_SIZE - (size & X86_64_MEM_PAGE_MASK);
    }

    // XXX: Cannot currently map more than one table of pages
    assert(size <= X86_64_MEM_PAGE_SIZE * X86_64_PTABLE_SIZE);
/*     assert(size <= MEM_PAGE_SIZE); */

    for(size_t i = 0; i < size; i += X86_64_MEM_PAGE_SIZE,
            base += X86_64_MEM_PAGE_SIZE, vbase += X86_64_MEM_PAGE_SIZE) {
        // No kernel image above 4 GByte
        assert(base < ((lpaddr_t)4 << 30));

        // Identity-map the kernel's physical region, so we don't lose ground
        paging_x86_64_map_table(&boot_pml4[X86_64_PML4_BASE(base)], (lpaddr_t)boot_pdpt);
        paging_x86_64_map_table(&boot_pdpt[X86_64_PDPT_BASE(base)], (lpaddr_t)boot_pdir);
        paging_x86_64_map_large(&boot_pdir[X86_64_PDIR_BASE(base)], base, PTABLE_PRESENT
                                | PTABLE_READ_WRITE | PTABLE_USER_SUPERVISOR);

        // Alias the same region at MEMORY_OFFSET
        paging_x86_64_map_table(&boot_pml4[X86_64_PML4_BASE(vbase)], (lpaddr_t)boot_pdpt_hi);
        paging_x86_64_map_table(&boot_pdpt_hi[X86_64_PDPT_BASE(vbase)], (lpaddr_t)boot_pdir_hi);
        paging_x86_64_map_large(&boot_pdir_hi[X86_64_PDIR_BASE(vbase)], base, PTABLE_PRESENT
                                | PTABLE_READ_WRITE | PTABLE_USER_SUPERVISOR);
    }

    // Identity-map the first 1G of physical memory for bootloader data
    paging_x86_64_map_table(&boot_pml4[0], (lpaddr_t)boot_pdpt);
    paging_x86_64_map_table(&boot_pdpt[0], (lpaddr_t)boot_pdir_1GB);
    for (int i = 0; i < X86_64_PTABLE_SIZE; i++) {
        paging_x86_64_map_large(&boot_pdir_1GB[X86_64_PDIR_BASE(X86_64_MEM_PAGE_SIZE * i)],
                                X86_64_MEM_PAGE_SIZE * i, PTABLE_PRESENT
                                | PTABLE_READ_WRITE | PTABLE_USER_SUPERVISOR);
    }

    /*
     * Identity MAP MMIO Register Region for "Serial Out" support
     * 0x08007D0000ULL
     *
     * PML4[0], PDIR[32]
     */
    paging_x86_64_map_table(&boot_pml4[X86_64_PML4_BASE(local_phys_to_mem(XEON_PHI_SBOX_BASE))],
                          (lpaddr_t) boot_pdpt_hi);
    paging_x86_64_map_table(&boot_pdpt_hi[X86_64_PDPT_BASE(local_phys_to_mem(XEON_PHI_SBOX_BASE))],
                          (lpaddr_t) boot_pdir_mmio);

    paging_x86_64_map_large(&boot_pdir_mmio[X86_64_PDIR_BASE(local_phys_to_mem(XEON_PHI_SBOX_BASE))],
                          XEON_PHI_SBOX_BASE,
                          PTABLE_PRESENT | PTABLE_READ_WRITE | PTABLE_USER_SUPERVISOR
                          | PTABLE_CACHE_DISABLED);

    // Activate new page tables
    paging_x86_64_context_switch((lpaddr_t)boot_pml4);
}

/**
 * \brief Setup default GDT.
 *
 * Loads the GDT register with the default GDT and reloads CS and SS
 * to point to the new entries. Resets all other segment registers to null.
 * Finally, completes setup of GDT to include TSS base address mapping and
 * loads TSS into task register.
 */
static void gdt_reset(void)
{
    lvaddr_t                     ptss = (lvaddr_t)&tss;
    struct region_descriptor    region = {
        .rd_limit = sizeof(gdt),
        .rd_base = (uint64_t)&gdt
    };

    // Load default GDT
    __asm volatile("lgdt %[region]" :: [region] "m" (region));

    // Reload segments
    __asm volatile("mov %[null], %%ds      \n\t"
                   "mov %[null], %%es      \n\t"
                   "mov %[ss], %%ss        \n\t"
                   "mov %[null], %%gs      \n\t"
                   "mov %[null], %%fs      \n\t"
                   "pushq %[cs]            \n\t"          // new CS
                   "lea 1f(%%rip), %%rax   \n\t"          // jumps to after lret
                   "pushq %%rax            \n\t"          // new IP
                   "lretq                  \n\t"          // fake return
                   "1:                     \n\t"          // we'll continue here
                   : /* No Output */
                   :
                   [null] "r" (0),
                   [ss] "r" (GSEL(KSTACK_SEL, SEL_KPL)),
                   [cs] "i" (GSEL(KCODE_SEL, SEL_KPL))
                   : "rax"
                   );

    // Complete setup of TSS descriptor (by inserting base address of TSS)
    gdt[TSS_LO_SEL].sys_lo.lo_base = ptss & 0xffffff;
    gdt[TSS_LO_SEL].sys_lo.hi_base = (ptss >> 24) & 0xff;
    gdt[TSS_HI_SEL].sys_hi.base = ptss >> 32;

    // Complete setup of TSS
    tss.rsp[0] = (lvaddr_t) &k1om_kernel_stack[K1OM_KERNEL_STACK_SIZE
                    / sizeof(uintptr_t)];

    // Load task state register
    __asm volatile("ltr %%ax" :: "a" (GSEL(TSS_LO_SEL, SEL_KPL)));
}

/**
 * \brief Relocates the active stack.
 *
 * This function relocates the stack, by adding 'offset' to the stack
 * pointer.
 *
 * \param offset        Offset to add to the stack pointer.
 */
static inline void __attribute__ ((always_inline))
relocate_stack(lvaddr_t offset)
{
    __asm volatile("add %[stack], %%rsp\n\t"
                   : /* No output */
                   : [stack] "er" (offset)
                   : "rsp"
                   );
}

/**
 * \brief Enable SYSCALL/SYSRET fast system calls.
 *
 * This function enables the SYSCALL/SYSRET pair of fast system calls in
 * long mode. Also sets the IA32_STAR and IA32_FMASK MSRs to point to the
 * user-space base selector and RFLAGS mask for SYSCALL/SYSRET fast system
 * calls.
 */
static inline void enable_fast_syscalls(void)
{
    // Segment selector bases for both kernel- and user-space for fast
    // system calls
    ia32_star_t star = ia32_star_rd(NULL);
    star = ia32_star_call_insert(star, GSEL(KCODE_SEL,  SEL_KPL));
    star = ia32_star_ret_insert( star, GSEL(KSTACK_SEL, SEL_UPL));
    ia32_star_wr(NULL, star);

    // Set ia32_lstar MSR to point to kernel-space system call multiplexer
    ia32_lstar_wr(NULL, (lvaddr_t)syscall_entry);

    // Set IA32_FMASK MSR for our OSes EFLAGS mask
    // We mask out everything (including interrupts).
    ia32_fmask_v_wrf(NULL, ~(RFLAGS_ALWAYS1) );

    // Enable fast system calls
    ia32_efer_sce_wrf(NULL, 1);
}

static inline void enable_tlb_flush_filter(void)
{
    uint32_t eax, ebx, ecx, edx;

    // Must read "AuthenticAMD"
    cpuid(0, &eax, &ebx, &ecx, &edx);
    if(ebx != 0x68747541 || ecx != 0x444d4163 || edx != 0x69746e65) {
        return;
    }

    // Is at least family 0fh?
    cpuid(1, &eax, &ebx, &ecx, &edx);
    if(((eax >> 8) & 0xf) != 0xf) {
        return;
    }

    debug(SUBSYS_STARTUP, "Enabling TLB flush filter\n");
    ia32_amd_hwcr_ffdis_wrf(NULL, 1);
}

/**
 * \brief Continue kernel initialization in kernel address space.
 *
 * This function resets paging to map out low memory and map in physical
 * address space, relocating all remaining data structures. It resets the
 * Global Descriptor Table for flat mode and to exclude legacy segments from
 * boot initialization code. It sets up the IDT for exception and interrupt
 * handling, initializes the local APIC and enables interrupts. After that it
 * calls kernel_startup(), which should not return (if it does, this function
 * halts the kernel).
 */
static void  __attribute__ ((noreturn, noinline)) text_init(void)
{
    // Reset global and locks to point to the memory in the pristine image
    global = (struct global*)addr_global;

    // re-initialize the console with the relocated address
    serial_console_init(local_phys_to_mem(XEON_PHI_SBOX_BASE));
    /*
     * Reset paging once more to use relocated data structures and map in
     * whole of kernel and available physical memory. Map out low memory.
     */
    paging_x86_64_reset();

    // Relocate global to "memory"
    global = (struct global*)local_phys_to_mem((lpaddr_t)global);

    // Relocate glbl_core_data to "memory"
    glbl_core_data = (struct x86_core_data *)
        local_phys_to_mem((lpaddr_t)glbl_core_data);

    /*
     * We know how much memory we have based on the card model
     */
    if (paging_x86_64_map_memory(0, K1OM_PHYSICAL_MEMORY_SIZE) != 0) {
        panic("error while mapping physical memory!");
    }

    /*
     * Also reset the global descriptor table (GDT), so we get
     * segmentation again and can catch interrupts/exceptions (the IDT
     * needs the GDT).
     */
    gdt_reset();

    // Arch-independent early startup
    kernel_startup_early();

    // Setup IDT
    setup_default_idt();
    idt_initialized = true;

    // initialize the Xeon Phi
    xeon_phi_init_early();

    // Enable machine check reporting
    mcheck_init();

    /**
     * 2.1.8.2.2 Interrupt Handling
     *
     * There are three different types of interrupt flows that are supported in
     * the Intel Xeon Phi coprocessor:
     *
     * + Local Interrupts These are the interrupts that are destined for one
     *   (or more) of the Intel Xeon Phi coprocessor cores located on the
     *   originating device. They appear in the form of APIC messages on the
     *   APIC serial bus.
     * + Remote Interrupts These are the interrupts which are destined for one
     *   (or more) of the Intel Xeon Phi coprocessor cores in other Intel Xeon
     *   PhiTM coprocessor devices. They appear as MMIO accesses on the PEG port.
     * + System Interrupts These are the interrupts which are destined for
     *   the host processor(s). They appear as INTx/MSI/MSI-X messages on the
     *   PEG port, depending upon the PCI configuration settings.
     */

    // Initialize local APIC
    apic_init();

    // do not remove/change this printf: needed by regression harness
    printf("Barrelfish CPU driver starting on k1om apic_id %u\n", apic_id);


    // Initialize local APIC timer
    if (kernel_ticks_enabled) {
        timing_calibrate();
        bool periodic = true;
        #ifdef CONFIG_ONESHOT_TIMER
        // we probably need a global variable like kernel_ticks_enabled
        periodic = false;
        #endif
        apic_timer_init(false, periodic);
        timing_apic_timer_set_ms(kernel_timeslice);
    } else {
        printk(LOG_WARN, "APIC timer disabled: NO timeslicing\n");
        apic_mask_timer();
    }

    // Initialize IPI notification mechanism
    ipi_notify_init();

    // Enable SYSCALL/SYSRET fast system calls
    /*
     * NOTE: the xeon phi does not support SYSENTER/SYSEXIT
     */
    enable_fast_syscalls();

    // Enable "no execute" page-level protection bit
    ia32_efer_nxe_wrf(NULL, 1);

    // Enable FPU and MMX
    enable_fpu();

    // Enable user-mode RDPMC opcode
    amd64_cr4_pce_wrf(NULL, 1);

    // AMD64: Check if TLB flush filter is enabled
    enable_tlb_flush_filter();

    // Call main kernel startup function -- this should never return
    kernel_startup();

    halt();
    // Returning here will crash! -- low pages not mapped anymore!
}

/**
 * \brief Architecture-specific initialization function.
 *
 * This function is called by the bootup code in boot.S to initialize
 * architecture-specific stuff. It is expected to call the kernel main
 * loop. This function never returns.
 *
 * The kernel expects one of two magic values in 'magic' that determine how it
 * has been booted. If 'magic' is #MULTIBOOT_INFO_MAGIC the kernel has been
 * booted by a (Multiboot-compliant) bootloader and this is the first image on
 * the boot CPU. It will relocate itself to a default position. If 'magic' is
 * #KERNEL_BOOT_MAGIC it has been booted by another image of itself and is
 * running on an (so-called) application CPU.
 *
 * This function sets up new page tables to alias the kernel
 * at #MEMORY_OFFSET. It also does any relocations necessary to the
 * "position-independent" code to make it run at the new location (e.g.
 * relocating the GOT). After all relocations, it calls text_init() of
 * the relocated image, which destroys the lower alias and may never return.
 *
 * For bsp kernels, the void pointer is of type multiboot_info, for application
 * CPUs, it is of type global. Global carries a pointer to multiboot_info.
 * Global also contains pointers to memory that is shared between kernels.
 *
 * \param magic         Boot magic value
 * \param pointer       Pointer to Multiboot Info or to Global structure
 */
void arch_init(uint64_t magic,
               void *pointer)
{
    /* pointer to the boot param struct set up by the boot loader */
    struct multiboot_info *mb = NULL;

    /* initialize the console port to the host */
    serial_console_init(XEON_PHI_SBOX_BASE);

    /* notify the host that we are running */
    notify_host();

    void __attribute__ ((noreturn))
    (*reloc_text_init)(void) = (void *)local_phys_to_mem((lpaddr_t)text_init);

    /* determine page-aligned physical address past end of multiboot */
    lvaddr_t dest = (lvaddr_t) &_start_kernel;
    if (dest & (BASE_PAGE_SIZE - 1)) {
        dest &= ~(BASE_PAGE_SIZE - 1);
        dest += BASE_PAGE_SIZE;
    }

    apic_bsp = magic == K1OM_BOOT_MAGIC;

    // XXX: print kernel address for debugging with gdb
    printf("Kernel starting at address 0x%"PRIxLVADDR"\n", local_phys_to_mem(dest));

    /*
     * If this is the boot image, make Multiboot information structure globally
     * known. Otherwise the passed value should equal the original structure.
     * If magic value does not match what we expect, we cannot proceed safely.
     */
    switch (magic) {
        case KERNEL_BOOT_MAGIC:
            /* kernel is started by another kernel */
            global = (struct global*) pointer;
            // Store the address of global to retrive it across relocation
            addr_global = (uint64_t) global;
            break;

        case K1OM_BOOT_MAGIC:
            /* kernel is started by the K1OM boot loader */
            mb = (struct multiboot_info *) pointer;

            printf("Barrelfish from weever: MBI: 0x%"PRIxLVADDR", Xeon Phi: [%u]\n",
                   (lpaddr_t) pointer,
                   mb->xeon_phi_id);

            struct xeon_phi_boot_params *bp;
            bp = (struct xeon_phi_boot_params *) (uintptr_t) mb->config_table;

            /*
             * XXX: The multiboot structure when invoked from the xloader will
             *      contain additional information.
             *
             *      CMDLINE:    the cmd line as set by the host OS
             *      MEM_LOWER:  the start of the multiboot image
             *      MEM_UPPER:  the end of the multiboot image
             */
            glbl_core_data->bp = (struct xeon_phi_boot_params *) local_phys_to_mem(mb
                            ->config_table);

            // Construct the global structure and store its address to retrieve it
            // across relocation
            memset(&global->locks, 0, sizeof(global->locks));
            addr_global = (uint64_t) global;

            break;

        default:
            addr_global = (uint64_t) global;
            break;
    }

    struct Elf64_Shdr *rela, *symtab;
    struct x86_coredata_elf *elf;
    uint32_t multiboot_flags;
    if (mb != NULL) { /* Multiboot info was passed */
        multiboot_flags = mb->flags;
        elf = (struct x86_coredata_elf *)&mb->syms.elf;

        // We need the ELF section header table for relocation
        if (!(multiboot_flags & MULTIBOOT_INFO_FLAG_HAS_ELF_SYMS)) {
            panic("Multiboot information structure does not include ELF section"
                  "header information -- Relocation impossible!");
        }

        // Determine where free RAM starts
        glbl_core_data->start_free_ram =
            ROUND_UP(max(multiboot_end_addr(mb), (uintptr_t)&_end_kernel),
                     BASE_PAGE_SIZE);
        printf("Start Free RAM at 0x%x (%i MB)\n",
               glbl_core_data->start_free_ram,
               glbl_core_data->start_free_ram >> 20);

        glbl_core_data->mods_addr = mb->mods_addr;
        glbl_core_data->mods_count = mb->mods_count;
        glbl_core_data->cmdline = mb->cmdline;
        glbl_core_data->mmap_length = mb->mmap_length;
        glbl_core_data->mmap_addr = mb->mmap_addr;
        glbl_core_data->xeon_phi_id = mb->xeon_phi_id;
    } else {
        /* No multiboot info, use the core_data struct */
        struct x86_core_data *core_data =
            (struct x86_core_data*)(dest - BASE_PAGE_SIZE);
        multiboot_flags = core_data->multiboot_flags;
        elf = &core_data->elf;
        glbl_core_data = core_data;
        core_data->cmdline = (lpaddr_t)&core_data->kernel_cmdline;
        my_core_id = core_data->dst_core_id;

        kcb_current = (struct kcb*) glbl_core_data->kcb;
        if (core_data->module_end > 4ul * (1ul << 30)) {
            panic("The cpu module is outside the initial 4GB mapping."
                  " Either move the module or increase initial mapping.");
        }
    }

    // We're only able to process Elf64_Rela entries
    if (elf->size != sizeof(struct Elf64_Shdr)) {
        panic("ELF section header entry size mismatch!");
    }

    // Find relocation section
    rela = elf64_find_section_header_type((struct Elf64_Shdr *)
                                          (lpaddr_t)elf->addr,
                                          elf->num, SHT_RELA);
    if (rela == NULL) {
        panic("Kernel image does not include relocation section!");
    }

    // Find symbol table section
    symtab = elf64_find_section_header_type((struct Elf64_Shdr *)
                                            (lpaddr_t)elf->addr,
                                            elf->num, SHT_DYNSYM);
    if (symtab == NULL) {
        panic("Kernel image does not include symbol table!");
    }

    // Alias kernel on top of memory, keep low memory
    paging_init((lpaddr_t)&_start_kernel, SIZE_KERNEL_IMAGE);

    // Relocate kernel image for top of memory
    elf64_relocate(K1OM_MEMORY_OFFSET + (lvaddr_t)&_start_kernel,
                   (lvaddr_t)&_start_kernel,
                   (struct Elf64_Rela *)(rela->sh_addr - K1OM_START_KERNEL_PHYS + &_start_kernel),
                   rela->sh_size,
                   (struct Elf64_Sym *)(symtab->sh_addr - K1OM_START_KERNEL_PHYS + &_start_kernel),
                   symtab->sh_size,
                   K1OM_START_KERNEL_PHYS, &_start_kernel);
    /*** Aliased kernel available now -- low memory still mapped ***/

    // Relocate stack to aliased location
    relocate_stack(K1OM_MEMORY_OFFSET);

    // Call aliased text_init() function and continue initialization
    reloc_text_init();
}
