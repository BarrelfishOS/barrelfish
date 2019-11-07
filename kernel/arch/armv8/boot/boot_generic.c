/*
 * Copyright (c) 2016, 2017, ETH Zurich.
 * Copyright (c) 2016, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

/* CPU driver VM initialisation.

   This is the entry point on booting the first core, and needs to deal with
   the state left by UEFI.  The CPU is mostly configured, in particular
   translation is enabled, and all RAM is mapped 1-1.  We'll also be in either
   EL3 or EL2.  We need to map the EL1 kernel window (TTBR1), drop to EL1, and
   jump to the next routine, which has already been relocated for us.
 */

#include <stdio.h>
#include <stdbool.h>

#include <barrelfish_kpi/types.h>
#include <init.h>
#include <offsets.h>
#include <sysreg.h>
#include <dev/armv8_dev.h>

#include <multiboot2.h>
#include <barrelfish_kpi/arm_core_data.h>

void eret(uint64_t a0, uint64_t a1, uint64_t a2, uint64_t a3);

void boot_bsp_init(uint32_t magic, lpaddr_t pointer)
    __attribute__((noreturn));
void boot_app_init(lpaddr_t pointer)
    __attribute__((noreturn));

/* low level debugging facilities */
#if DEBUG
#ifdef THUNDERX
#include <dev/pl011_uart_dev.h>

#define CN88XX_MAP_UART0_OFFSET 0x87E024000000UL

static pl011_uart_t uart;

static void debug_uart_initialize(void) {
    pl011_uart_initialize(&uart, (mackerel_addr_t) CN88XX_MAP_UART0_OFFSET);
}

static void debug_serial_putc(char c)
{
    while(pl011_uart_FR_txff_rdf(&uart) == 1) ;
    pl011_uart_DR_rawwr(&uart, c);
}
#elif defined(XGENE)
#include <dev/apm88xxxx/apm88xxxx_pc16550_dev.h>

#define CN88XX_MAP_UART0_OFFSET 0x87E024000000UL

apm88xxxx_pc16550_t uart;

static void debug_uart_initialize(void) {
    apm88xxxx_pc16550_initialize(&uart, (mackerel_addr_t)0x1C020000);
}

static void debug_serial_putc(char c)
{
    // Wait until FIFO can hold more characters
    while(!apm88xxxx_pc16550_LSR_thre_rdf(&uart));
    // Write character
    apm88xxxx_pc16550_THR_thr_wrf(&uart, c);
}
#elif defined(QEMU)
#include <dev/pl011_uart_dev.h>

#define QEMU_MAP_UART0_OFFSET 0x9000000UL

static pl011_uart_t uart;

static void debug_uart_initialize(void) {
    pl011_uart_initialize(&uart, (mackerel_addr_t) QEMU_MAP_UART0_OFFSET);
}

static void debug_serial_putc(char c)
{
    while(pl011_uart_FR_txff_rdf(&uart) == 1) ;
    pl011_uart_DR_rawwr(&uart, c);
}
#elif defined(IMX8X)
#include <dev/lpuart_dev.h>

#define IMX8X8_MAP_UART0_OFFSET 0x5A090000UL

static lpuart_t uart;

static void debug_uart_initialize(void) {
    lpuart_initialize(&uart, (mackerel_addr_t) IMX8X8_MAP_UART0_OFFSET);
}

static void debug_serial_putc(char c)
{
    while(lpuart_stat_tdre_rdf(&uart) == 0);
    lpuart_write_data_wr(&uart,c);
}
#endif

static void debug_serial_putchar(char c) {
    if (c == '\n') {
        debug_serial_putc('\r');
    }
    debug_serial_putc(c);
}


static void debug_print_string(char *str)
{
    while (str && *str) {
        debug_serial_putchar(*str);
        str++;
    }
}

/**
 * \brief Very basic hex print support
 */
static inline void debug_print_hex(uint64_t num) {
    static char chars[] = {
        '0',
        '1',
        '2',
        '3',
        '4',
        '5',
        '6',
        '7',
        '8',
        '9',
        'a',
        'b',
        'c',
        'd',
        'e',
        'f',
    };

    char buf[17];
    for (int i = 0; i < 16; i++) {
        int d = (num >> 4*i) & 0xf;
        buf[15-i] = chars[d];
    }
    buf[16] = '\0';
    debug_print_string(buf);
}
#else
#define debug_print_string(x)
#define debug_uart_initialize()
#define debug_print_hex(x)
#endif


void (*cpu_driver_entry)(lvaddr_t pointer);

static void configure_tcr(void) {
    armv8_TCR_EL1_t tcr_el1 = armv8_TCR_EL1_rd(NULL);
    // disable top byte ignored, EL1
    tcr_el1 = armv8_TCR_EL1_TBI1_insert(tcr_el1, 0);
    // disable top byte ignored, EL0
    tcr_el1 = armv8_TCR_EL1_TBI0_insert(tcr_el1, 0);
    // 48b IPA
    tcr_el1 = armv8_TCR_EL1_IPS_insert(tcr_el1, 5);
    // 4kB granule
    tcr_el1 = armv8_TCR_EL1_TG1_insert(tcr_el1, armv8_KB_4);
    // Walks inner shareable
    tcr_el1 = armv8_TCR_EL1_SH1_insert(tcr_el1, armv8_inner_shareable);
    // Walks outer WB WA
    tcr_el1 = armv8_TCR_EL1_ORGN1_insert(tcr_el1, armv8_WbRaWa_cache);
    // Walks inner WB WA
    tcr_el1 = armv8_TCR_EL1_IRGN1_insert(tcr_el1, armv8_WbRaWa_cache);
    // enable EL1 translation
    tcr_el1 = armv8_TCR_EL1_EPD1_insert(tcr_el1, 0);
    // 48b kernel VA
    tcr_el1 = armv8_TCR_EL1_T1SZ_insert(tcr_el1, 16);
    // 4kB granule
    tcr_el1 = armv8_TCR_EL1_TG0_insert(tcr_el1, armv8_KB_4);
    // Walks inner shareable
    tcr_el1 = armv8_TCR_EL1_SH0_insert(tcr_el1, armv8_inner_shareable);
    // Walks outer WB WA
    tcr_el1 = armv8_TCR_EL1_ORGN0_insert(tcr_el1, armv8_WbRaWa_cache);
    // Walks inner WB WA
    tcr_el1 = armv8_TCR_EL1_IRGN0_insert(tcr_el1, armv8_WbRaWa_cache);
    // enable EL0 translation
    tcr_el1 = armv8_TCR_EL1_EPD0_insert(tcr_el1, 0);
    // 48b user VA
    tcr_el1 = armv8_TCR_EL1_T0SZ_insert(tcr_el1, 16);
    armv8_TCR_EL1_wr(NULL, tcr_el1);
}


#define    DAIF_FIQ_BIT   (1 << 0)
#define    DAIF_IRQ_BIT   (1 << 1)


static void armv8_disable_interrupts(void)
{
    __asm volatile("msr DAIFSet, #3\n");
}

static void armv8_set_tcr(uint8_t el)
{
    switch(el) {
        case 3:
            //sysreg_write_ttbr0_el2(addr);
        case 2:
        {
            armv8_TCR_EL2_t reg = 0;
            reg = armv8_TCR_EL2_PS_insert(reg, 5);
            reg = armv8_TCR_EL2_T0SZ_insert(reg, (64 - 48));
            armv8_TCR_EL2_wr(NULL, reg);
            break;
        }
        case 1:
        {
            armv8_TCR_EL1_t reg = 0;
          // TODO: figure out what to set  reg = armv8_TCR_EL1_PS_insert(reg, 5);
            reg = armv8_TCR_EL1_T0SZ_insert(reg, (64 - 48));
            armv8_TCR_EL1_wr(NULL, reg);
            break;
        }
        default:
            assert("should not happen");
            return;
    }
}

static void armv8_set_ttbr0(uint8_t el, lpaddr_t addr)
{
    switch(el) {
        case 3:
            //sysreg_write_ttbr0_el2(addr);
        case 2:
            armv8_TTBR0_EL2_wr(NULL, addr);
            armv8_TTBR0_EL1_wr(NULL, addr);
            break;
        case 1:
            armv8_TTBR0_EL1_wr(NULL, addr);
            break;
        default:
            assert("should not happen");
            return;
    }
    __asm volatile("isb");
}

static void armv8_enable_mmu(uint8_t el)
{
    switch(el) {
        case 3:
            armv8_SCTLR_EL3_M_wrf(NULL, 0x1);
            __asm volatile("tlbi    alle3\n  isb");
            break;
        case 2:
            armv8_SCTLR_EL2_M_wrf(NULL, 0x1);
            __asm volatile("tlbi    alle2\n  isb");
            break;
        case 1:
            armv8_SCTLR_EL1_M_wrf(NULL, 0x1);
            __asm volatile("tlbi    vmalle1\n  isb");
            break;
        default:
            assert("should not happen");
            return;
    }
    __asm volatile("dsb sy\n isb");
}

static void armv8_invalidate_tlb(uint8_t el)
{
    switch(el) {
        case 3:
            armv8_SCTLR_EL3_M_wrf(NULL, 0x1);
            __asm volatile("tlbi    alle3");
            break;
        case 2:
            armv8_SCTLR_EL2_M_wrf(NULL, 0x1);
            __asm volatile("tlbi    alle2");
            break;
        case 1:
            armv8_SCTLR_EL1_M_wrf(NULL, 0x1);
            __asm volatile("tlbi    vmalle1");
            break;
        default:
            assert("should not happen");
            return;
    }
    __asm volatile("dsb sy\n isb");
}

static void armv8_invalidate_icache(void)
{
    __asm volatile(
      "ic      iallu \n"
      "dsb     sy \n"
      "isb \n"
      );
}

static void armv8_instruction_synchronization_barrier(void)
{
    __asm volatile("isb");
}

static void configure_spsr(uint8_t el) {
    armv8_SPSR_EL2_t spsr = 0;
    /* mask the exceptions */
    spsr = armv8_SPSR_EL2_D_insert(spsr, 1);
    spsr = armv8_SPSR_EL2_A_insert(spsr, 1);
    spsr = armv8_SPSR_EL2_I_insert(spsr, 1);
    spsr = armv8_SPSR_EL2_F_insert(spsr, 1);

    /* set el1 and use the SP_ELx stack */
    spsr = armv8_SPSR_EL2_M_lo_insert(spsr, (1<<2) | 1);

    switch(el) {
    case 3:
        armv8_SPSR_EL3_wr(NULL, spsr);
        return;
    case 2:
        armv8_SPSR_EL2_wr(NULL, spsr);
        break;
    case 1:
        armv8_SPSR_EL1_wr(NULL, spsr);
        return;
    default:
        return;
    }
}

static void configure_ttbr1(lpaddr_t addr)
{
    armv8_TTBR1_EL1_rawwr(NULL, addr);
}

static void configure_mair(void)
{
    /* Set memory type 0, for kernel use. */
    // attr0 = Normal Memory, Inner Write-back non transient
    // attr1 = Device-nGnRnE memory
    armv8_MAIR_EL1_wr(NULL, 0x00ff);
}

static void configure_sctlr(void)
/* Enable EL0/1 translation. */
{

    armv8_SCTLR_EL1_t val = 0;

    /* Traps EL0 execution of cache maintenance instructions to EL1 */
    val = armv8_SCTLR_EL1_UCI_insert(val, 0x1);

    /* write permissions implies execute never */
    //val = armv8_SCTLR_EL1_WXN_insert(val, 0x1);

    /* don't trap WFI/WFE instructions to EL1 */
    val = armv8_SCTLR_EL1_nTWE_insert(val, 0x1);
    val = armv8_SCTLR_EL1_nTWI_insert(val, 0x1);

    /* disable Traps EL0 accesses to the CTR_EL0 to EL1*/
    val = armv8_SCTLR_EL1_UCT_insert(val, 0x1);

    /* Allow EL0 to do DC ZVA */
    val = armv8_SCTLR_EL1_DZE_insert(val, 0x1);

    /* enable instruction cache */
    val = armv8_SCTLR_EL1_I_insert(val, 0x1);

    /*
     * EL0 execution of MRS , MSR(register) , or MSR(immediate) instructions
     * that access the DAIF is not trapped to EL1.
     */
    //val = armv8_SCTLR_EL1_UMA_insert(val, 0x1);

    /*
     * Enables accesses to the DMB, DSB, and ISB System
     * instructions in the (coproc== 1111 ) encoding space from EL0
     */
    val = armv8_SCTLR_EL1_CP15BEN_insert(val, 0x1);

    /* Enable SP alignment checks */
    val = armv8_SCTLR_EL1_SA0_insert(val, 0x1);
    val = armv8_SCTLR_EL1_SA_insert(val, 0x1);

    /* enable data cachable */
    val = armv8_SCTLR_EL1_C_insert(val, 0x1);

    /* enable alignment checks */
    val = armv8_SCTLR_EL1_A_insert(val, 0x1);

    /* enable mmu */
    val = armv8_SCTLR_EL1_M_insert(val, 0x1);

    armv8_SCTLR_EL1_wr(NULL, val);
}

static void configure_el3_traps(void)
{

    /* If we've started in EL3, that most likely means we're in the
     * simulator.  We don't use it at all, so just disable all traps to
     * EL3, and drop to non-secure EL2 (if it exists). */

    armv8_SCR_EL3_t val = 0;

    /* Don't trap secure timer access. */
    val = armv8_SCR_EL3_ST_insert(val, 0x1);

    /* Next EL is AArch64. */
    val = armv8_SCR_EL3_RW_insert(val, 0x1);

    /* HVC is enabled. */
    val = armv8_SCR_EL3_HCE_insert(val, 0x1);

    /* SMC is disabled. */
    val = armv8_SCR_EL3_SMD_insert(val, 0x1);

    /* External aborts don't trap to EL3. */
    val = armv8_SCR_EL3_EA_insert(val, 0x1);

    /* FIQs don't trap to EL3. */
    val = armv8_SCR_EL3_FIQ_insert(val, 0x1);

    /* IRQs don't trap to EL3. */
    val = armv8_SCR_EL3_IRQ_insert(val, 0x1);

    /* EL0 and EL1 are non-secure. */
    val = armv8_SCR_EL3_NS_insert(val, 0x1);

    armv8_SCR_EL3_wr(NULL, val);

    /* We don't need to set SCTLR_EL3, as we're not using it. */

    armv8_MDCR_EL3_t mdcr = 0;
    /* Allow event counting in secure state. */
    armv8_MDCR_EL3_SPME_insert(mdcr, 0x1);
    armv8_MDCR_EL3_wr(NULL, mdcr);
}

static void configure_el2_traps(void)
{
    /* check if EL2 is implemented */
    if (armv8_ID_AA64PFR0_EL1_EL2_rdf(NULL) == armv8_ID_EL_NOT_IMPLEMENTED) {
        return;
    }

    /* configure EL2 traps & mmu */

    armv8_HCR_EL2_t val = 0;

    /* For the Non-secure EL1&0 translation regime, for permitted accesses to a
     * memory location that use a common definition of the Shareability and
     * Cacheability of the location, there might be a loss of coherency if the
     * Inner Cacheability attribute for those accesses differs from the Outer
     * Cacheability attribute.*/
    val = armv8_HCR_EL2_MIOCNCE_insert(val, 1);

    /* Set the mode to be AARCH64 */
    val = armv8_HCR_EL2_RW_insert(val, 1);

    /* HVC instructions are UNDEFINED at EL2 and Non-secure EL1. Any resulting
     * exception is taken to the Exception level at which the HVC instruction
     * is executed.
     *
     * XXX: this will disable Hypervisor calls entirely, revisit for ARRAKIS
     */
    val = armv8_HCR_EL2_HCD_insert(val, 1);

    armv8_HCR_EL2_wr(NULL, val);


    /* disable traps to EL2 for timer accesses */

    armv8_CNTHCTL_EL2_t cnthctl;
    cnthctl = armv8_CNTHCTL_EL2_rd(NULL);
    cnthctl = armv8_CNTHCTL_EL2_EL1PCEN_insert(cnthctl, 0x1);
    cnthctl = armv8_CNTHCTL_EL2_EL1PCTEN_insert(cnthctl, 0x1);
    armv8_CNTHCTL_EL2_wr(NULL, cnthctl);
}

static void configure_el1_traps(void)
{
    /* disable traps for FP/SIMD access  */
    armv8_CPACR_EL1_FPEN_wrf(NULL, armv8_fpen_trap_none);
}

static void drop_to_el2(struct armv8_core_data *pointer)
{
    /* write the stack pointer for EL1 */
    armv8_SP_EL1_wr(NULL, pointer->cpu_driver_stack + KERNEL_OFFSET);

    /* Set the jump target */
    armv8_ELR_EL3_wr(NULL, (uint64_t)cpu_driver_entry);

    /* call exception return */
    eret((lpaddr_t)pointer + KERNEL_OFFSET, 0, 0, 0);
}

static void drop_to_el1(struct armv8_core_data *pointer)
{
    /* write the stack pointer for EL1 */
    armv8_SP_EL1_wr(NULL, pointer->cpu_driver_stack + KERNEL_OFFSET);

    /* Set the jump target */
    armv8_ELR_EL2_wr(NULL, (uint64_t)cpu_driver_entry);

    /* call exception return */
    eret((lpaddr_t)pointer + KERNEL_OFFSET, 0, 0, 0);
}

static void jump_to_cpudriver(struct armv8_core_data *pointer)
{
    // We are in EL1, so call arch_init directly.

    // Re-set the stack pointer
    sysreg_write_sp(pointer->cpu_driver_stack + KERNEL_OFFSET);
    cpu_driver_entry((lpaddr_t)pointer + KERNEL_OFFSET);
}


/* On entry:

   Execution is starting in LOW addresses
   Pointers to stack and multiboot are LOW addresses
   Single core running (not guaranteed to be core 0)
   CPU is in highest implemented exception level
   MMU enabled, 4k translation granule, 1:1 mapping of all RAM
   Little-endian mode
   Core caches (L1&L2) and TLB enabled
   Non-architectural caches disabled (e.g. L3)
   Interrupts enabled
   Generic timer initialized and enabled
   >= 128KiB stack
   ACPI tables available
   Register x0 contains a pointer to ARMv8 core data
 */
static void boot_generic_init(struct armv8_core_data *core_data) {

    cpu_driver_entry = (void *)core_data->cpu_driver_entry;

    uint8_t el = armv8_CurrentEL_EL_rdf(NULL);

    /* Configure the EL1 translation regime. */
    configure_tcr();

    /* Configure the kernel page tables for EL1. */
    configure_ttbr1(core_data->page_table_root);

    /* configure memory attributes */
    configure_mair();

    /* Enable EL0/1 translation. */
    configure_sctlr();

    /* configure spsr */
    configure_spsr(el);

    /* configure EL 1 traps*/
    configure_el1_traps();

    debug_print_string("Jumping to CPU driver\n");
    
    switch(el) {
    case 3:
        configure_el3_traps();
        configure_el2_traps();
        drop_to_el2(core_data);
        break;
    case 2:
        configure_el2_traps();
        drop_to_el1(core_data);
        break;
    case 1:
        jump_to_cpudriver(core_data);
        break;
    default:
        break;
    }
}

/**
 * @brief initializes an application core
 *
 * @param state pointer to the armv8_core_data structure
 *
 * This function is intended to bring the core to the same state as if it
 * has been booted by the UEFI boot loader.
 */
void boot_app_init(lpaddr_t pointer)
{
    debug_uart_initialize();
    debug_print_string("APP BOOTING\n");

    struct armv8_core_data *core_data = (struct armv8_core_data *)pointer;

    uint8_t current_el = armv8_CurrentEL_EL_rdf(NULL);

    if (current_el == 2) {
        uint64_t zero = 0;
        __asm volatile("MSR CPTR_EL2, %[zero]" : : [zero] "r" (zero));
    }

    // /* disable interrupts */
    armv8_disable_interrupts();

    /* set the ttbr0/1 */
    armv8_set_ttbr0(current_el, core_data->page_table_root);

    /* set the TCR */
    armv8_set_tcr(current_el);

    /* enable MMU */
    armv8_enable_mmu(current_el);

    /* invalidate TLB */
    armv8_invalidate_tlb(current_el);

    /* invalidate icache */
    armv8_invalidate_icache();
    armv8_instruction_synchronization_barrier();

    boot_generic_init(core_data);

    while(1) {
        __asm volatile("wfi \n");
    }
}

/* On entry:

   Execution is starting in LOW addresses
   Pointers to stack and multiboot are LOW addresses
   Single core running (not guaranteed to be core 0)
   CPU is in highest implemented exception level
   MMU enabled, 4k translation granule, 1:1 mapping of all RAM
   Little-endian mode
   Core caches (L1&L2) and TLB enabled
   Non-architectural caches disabled (e.g. L3)
   Interrupts enabled
   Generic timer initialized and enabled
   >= 128KiB stack
   ACPI tables available
   Register x0 contains the multiboot magic value
   Register x1 contains a pointer to ARMv8 core data
 */
void
boot_bsp_init(uint32_t magic, lpaddr_t pointer) {

    debug_uart_initialize();
    debug_print_string("BSP BOOTING\n");
    debug_print_string("Magic: ");
    debug_print_hex(magic);
    debug_print_string(", Pointer: ");
    debug_print_hex(pointer);
    debug_print_string("\n");

    /* Boot magic must be set */
    if (magic != MULTIBOOT2_BOOTLOADER_MAGIC) {
        debug_print_string("Invalid bootloader magic\n");
        goto stop;
    }

    struct armv8_core_data *core_data = (struct armv8_core_data *)pointer;

    debug_print_string("CPU driver entry: ");
    debug_print_hex(core_data->cpu_driver_entry);
    debug_print_string("\n");

    /* disable interrupts */
    armv8_disable_interrupts();
    
    boot_generic_init(core_data);

    stop:
    while(1) {
        __asm volatile("wfi \n");
    }
}
