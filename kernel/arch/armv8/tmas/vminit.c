/* CPU driver VM initialisation.

   This is the entry point on booting the first core, and needs to deal with
   the state left by UEFI.  The CPU is mostly configured, in particular
   translation is enabled, and all RAM is mapped 1-1.  We'll also be in either
   EL3 or EL2.  We need to map the EL1 kernel window (TTBR1), drop to EL1, and
   jump to the next routine, which has already been relocated for us.
 */

#include <stdio.h>

#include <init.h>
#include <offsets.h>
#include <sysreg.h>

void eret(uint64_t a0, uint64_t a1, uint64_t a2, uint64_t a3);

void vminit(uint32_t magic, void *pointer, void *kernel_stack_top)
    __attribute__((noreturn,section(".vminit")));

void *jump_target= &arch_init;

/* On entry:

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
   Register x1 contains a pointer to multiboot image
   Register x1 contains a pointer to top entry in the kernel stack
 */
void
vminit(uint32_t magic, void *pointer, void *stack) {
    int el= get_current_el();


    /* Configure the EL1 translation regime. */
    /* assert(el >= 1) */
    {
        uint64_t tcr_el1=
            (5UL   << 32)  | /* 48b IPA */
            (0UL   << 30)  | /* 4kB granule */
            (3UL   << 28)  | /* Walks inner shareable */
            (1UL   << 26)  | /* Walks outer WB WA */
            (1UL   << 24)  | /* Walks inner WB WA */
            (16UL  << 16)  | /* T1SZ = 16, 48b kernel VA */
            (0UL   << 14)  | /* 4kB granule */
            (3UL   << 12)  | // SH0, inner shareable
            (1UL   << 10)  | // ORGN0, walks outer WB WA
            (1UL   << 8)   | // IRGN0, walks inner WB WA
            (16UL  << 0)   ; /* T0SZ = 16, 48b user VA. TODO: should be a domain attribute */
        sysreg_write_ttbcr(tcr_el1);
    }

    /* Copy the current TTBR for EL1. */
    {
        uint64_t ttbr1_el1;
        if(el == 3) ttbr1_el1= sysreg_read_ttbr0_el3();
        if(el == 2) ttbr1_el1= sysreg_read_ttbr0_el2();
        else        ttbr1_el1= sysreg_read_ttbr0_el1();
        sysreg_write_ttbr1_el1(ttbr1_el1);
    }

    /* Enable EL0/1 translation. */
    {
        /* Set memory type 0, for kernel use. */
        // attr0 = Normal Memory, Inner Write-back non transient
        // attr1 = Device-nGnRnE memory
        sysreg_write_mair_el1(0x00ff);

        uint64_t sctlr=
            BIT(18) | /* Don't trap WFE */
            BIT(16) | /* Don't trap WFI */
            BIT(15) | /* Allow CTR_EL0 access */
            BIT(14) | /* Allow EL0 DC ZVA */
            BIT(12) | /* Instructions are cacheable */
            BIT(9)  | /* Don't trap MSR DAIF */
            BIT(5)  | /* Allow AArch32 barriers */
            BIT(4)  | /* Check stack alignment at EL0 */
            BIT(3)  | /* Check stack alignment at EL1 */
            BIT(2)  | /* Data is cacheable */
            BIT(1)  | /* Alignment checking */
            BIT(0)  ; /* EL0/1 translation enabled */
        sysreg_write_sctlr_el1(sctlr);

    }

    /* Do we have an EL2? */
    int have_el2;
    {
        uint64_t pfr0= sysreg_get_id_aa64pfr0_el1();
        int el2= FIELD(8,4,pfr0);
        have_el2= el2 == 1 || el2 == 2;
    }

    /* If so, we need to configure EL2 traps. */
    if (have_el2 && el > 1) {
        /* assert(el >= 2) */

        /* We'll never return to (or enter) EL2, so leave the MMU
         * unconfigured. */

        /* Disable all traps to EL2. */
        uint64_t hcr_el2=
            BIT(38) | /* Allow noncoherence for mismatched attributes. */
            BIT(31) | /* EL1 is AArch64. */
            BIT(29) ; /* HVC disabled (XXX revisit for Arrakis). */
        sysreg_write_hcr_el2(hcr_el2);
    }

    if (el > 1) {
        /* When we jump down to EL1, we'll reset the stack to the top (relocated
         * within the kernel window).  That's * fine, as we'll never return to
         * this context. */
        sysreg_write_sp_el1((uint64_t)stack + KERNEL_OFFSET);
    }

    if (el == 3) {
        /* If we've started in EL3, that most likely means we're in the
         * simulator.  We don't use it at all, so just disable all traps to
         * EL3, and drop to non-secure EL2 (if it exists). */

        uint64_t scr_el3=
            BIT(11) | /* Don't trap secure timer access. */
            BIT(10) | /* Next EL is AArch64. */
            BIT(8)  | /* HVC is enabled. */
            BIT(7)  | /* SMC is disabled. */
            BIT(3)  | /* External aborts don't trap to EL3. */
            BIT(2)  | /* FIQs don't trap to EL3. */
            BIT(1)  | /* IRQs don't trap to EL3. */
            BIT(0)  ; /* EL0 and EL1 are non-secure. */
        sysreg_write_scr_el3(scr_el3);

        /* We don't need to set SCTLR_EL3, as we're not using it. */

        uint64_t mdcr_el3=
            BIT(17) ; /* Allow event counting in secure state. */
        sysreg_write_mdcr_el3(mdcr_el3);

        /* Call plat_init() in EL1 */
        uint64_t spsr=
            0xf << 6 | /* Mask exceptions SPSR[9:6] */
            1   << 2 | /* EL1             SPSR[3:2] */
            1        ; /* Use EL1 stack pointer */
        sysreg_write_spsr_el3(spsr);
        sysreg_write_elr_el3((uint64_t)jump_target);
        eret(magic, (uint64_t)pointer + KERNEL_OFFSET, (uint64_t) (stack + KERNEL_OFFSET), 0);
    } else if (el == 2) {
        /* assert(el == 2) */

        /* Call plat_init() in EL1 */
        uint64_t spsr=
            0xf << 6 | /* Mask exceptions SPSR[9:6] */
            1   << 2 | /* EL1             SPSR[3:2] */
            1        ; /* Use EL1 stack pointer */
        sysreg_write_spsr_el2(spsr);
        sysreg_write_elr_el2((uint64_t)jump_target);
        eret(magic, (uint64_t)pointer + KERNEL_OFFSET, (uint64_t) (stack + KERNEL_OFFSET), 0);
    } else {
        // We are in EL1, so call arch_init directly.
        void (*relocated_arch_init)(uint32_t magic, void *pointer, uintptr_t stack);
        if ((lvaddr_t)arch_init < KERNEL_OFFSET) {
            relocated_arch_init = (void *)((lvaddr_t)arch_init + KERNEL_OFFSET);
        } else {
            relocated_arch_init = (void *)((lvaddr_t)arch_init);
        }

        // we may need to re set the stack pointer
        uint64_t sp = sysreg_read_sp();
        if (sp < KERNEL_OFFSET) {
            sysreg_write_sp(sp + KERNEL_OFFSET);
        }
        relocated_arch_init(magic, pointer + KERNEL_OFFSET, (uint64_t) (stack + KERNEL_OFFSET));
    }

    while(1);
}
