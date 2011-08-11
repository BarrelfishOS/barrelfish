/**
 * \file
 * \brief Intel 64 local APIC driver.
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
#include <arch/x86/apic.h>
#include <arch/x86/start_aps.h>
#include <paging_kernel_arch.h>
#include <x86.h>
#include <ia32_dev.h>
#include <xapic_dev.h>

#define APIC_BASE_ADDRESS_MASK  0x000ffffffffff000
#define APIC_PAGE_SIZE          4096
#define APIC_DEFAULT_PRIORITY   0x0000

// global pointers used in init_ap.S
extern uint64_t x86_64_start_ap;
extern uint64_t x86_64_init_ap_wait;
extern uint64_t x86_32_start_ap;
extern uint64_t x86_32_init_ap_wait;

bool bsp = true;
static xapic_t apic;

/**
 * \brief Initializes the local APIC timer.
 *
 * \param masked        Mask interrupt
 * \param periodic      Periodic or one-shot
 */
void apic_timer_init(bool masked, bool periodic)
{
    // Set APIC timer to periodic ticker mode
    xapic_lvt_timer_wr(&apic, (xapic_lvt_timer_t) {
            .vector = APIC_TIMER_INTERRUPT_VECTOR,
            .mask = masked ? xapic_masked : xapic_not_masked,
            .mode = periodic ? xapic_periodic : xapic_one_shot
            }
        );
}

/**
 * \brief Initializes performance counter overflow interrupt
 *
 */
void apic_perfcnt_init(void)
{
    // Activate use of APIC performance counter interrupts
    xapic_lvt_perf_cnt_wr(&apic, (xapic_lvt_mon_t) {
            .vector = APIC_PERFORMANCE_INTERRUPT_VECTOR,
                .msgType = 0,
                .mask = 0
            }
        );
}
/**
 * \brief Initializes performance counter overflow interrupt
 *
 */
void apic_perfcnt_stop(void)
{
    // Activate use of APIC performance counter interrupts
    xapic_lvt_perf_cnt_wr(&apic, (xapic_lvt_mon_t) {
            .vector = APIC_PERFORMANCE_INTERRUPT_VECTOR,
                .msgType = 0,
                .mask = 1
            }
        );
}

void apic_timer_set_count(uint32_t count)
{
    xapic_init_count_wr(&apic, count);
}

uint32_t apic_timer_get_count(void)
{
    return xapic_cur_count_rd(&apic);
}

void apic_timer_set_divide(xapic_divide_t divide)
{
    xapic_dcr_wr(&apic, (xapic_dcr_t) {
            .div_val = divide
        });
}

/** \brief This function initializes the local APIC.
    This function initialzes the local APIC by writes to the memory mapped
    registers and by enabling it in the MSR.
*/
void apic_init(void)
{
    //pointer to a variable used as pseudo-lock to synchronize the BSP
    //and the AP which gets enabled
#if defined (__x86_64__)
    volatile uint32_t *ap_wait = (volatile uint32_t *)
        local_phys_to_mem((lpaddr_t)&x86_64_init_ap_wait - ((lpaddr_t)&x86_64_start_ap) +
                          X86_64_REAL_MODE_LINEAR_OFFSET);
#elif defined (__i386__)
#       if !defined(__scc__) || defined(RCK_EMU)
    volatile uint32_t *ap_wait = (volatile uint32_t *)
        local_phys_to_mem((lpaddr_t)&x86_32_init_ap_wait - ((lpaddr_t)&x86_32_start_ap) +
                          X86_32_REAL_MODE_LINEAR_OFFSET);
#       endif
#else
#error "Architecture not supported"
#endif

#if !defined(__scc__) || defined(RCK_EMU)
    ia32_apic_base_t apic_base_msr = ia32_apic_base_rd(NULL);
    lpaddr_t apic_phys = (lpaddr_t)ia32_apic_base_rd_raw(NULL)
                          & APIC_BASE_ADDRESS_MASK;
    lvaddr_t apic_base = paging_map_device(apic_phys, APIC_PAGE_SIZE);
#else
    lpaddr_t apic_phys = (lpaddr_t)0xfee00000;
    lvaddr_t apic_base =
        paging_map_device((lpaddr_t)0xfee00000, APIC_PAGE_SIZE);
#endif

    if(apic_base == 0) {
        panic("apic_init(): could not map APIC registers");
    }

    debug(SUBSYS_APIC, "Accessing APIC at 0x%"PRIxLPADDR" / 0x%"PRIxLVADDR"\n",
          apic_phys, apic_base);
    xapic_initialize(&apic, (void *)apic_base);

#if !defined(__scc__) || defined(RCK_EMU)
    debug(SUBSYS_APIC, "APIC ID=%hhu\n", xapic_id_rd(&apic).id );
    if (apic_base_msr.bsp){
        debug(SUBSYS_APIC, "APIC: bootstrap processor\n");
        bsp = true;
    } else {
        debug(SUBSYS_APIC, "APIC: application processor\n");
        bsp = false;
        *ap_wait = AP_STARTED;
    }
#endif

    // initialize spurious interrupt register
    // no focus, software enabled
    xapic_svr_wr(&apic, (xapic_svr_t) {
                .vector = APIC_SPURIOUS_INTERRUPT_VECTOR,
                .enable = 1,
                .focus = 1 } );

    // Task priority
    xapic_tpr_wr(&apic, (xapic_priority_t) {
                .sub_class = APIC_DEFAULT_PRIORITY,
                .priority = APIC_DEFAULT_PRIORITY
                } );

    //LVT timer register
    //set vector number and disable reception of this interrupt
    xapic_lvt_timer_wr(&apic, (xapic_lvt_timer_t) {
                .vector = APIC_TIMER_INTERRUPT_VECTOR,
                .mask = xapic_masked } );

    //thermal sensor
    xapic_lvt_thermal_wr(&apic, (xapic_lvt_mon_t) {
                .vector = APIC_THERMAL_INTERRUPT_VECTOR,
                .mask = xapic_masked } );

#if defined(__scc__) && !defined(RCK_EMU)
    //LINT0: inter-core interrupt
    //generate fixed int
    xapic_lvt_lint0_wr(&apic, (xapic_lvt_lint_t) {
                .vector = APIC_INTER_CORE_VECTOR,
                .dlv_mode = xapic_fixed,
                .trig_mode = xapic_edge,
		.mask = xapic_not_masked } );

    //LINT1: usually used to generate an NMI
    //generate device interrupt
    xapic_lvt_lint1_wr(&apic, (xapic_lvt_lint_t) {
                .vector = 32,
		.dlv_mode = xapic_fixed,
                .trig_mode = xapic_edge,
                .mask = xapic_not_masked } );
#else
    //LINT0: external interrupts, delivered by the 8259 PIC
    //generate extInt as if INTR pin were activated
    //disabled (we use IOAPICs exclusively)
    xapic_lvt_lint0_wr(&apic, (xapic_lvt_lint_t) {
                .vector = 0,
                .dlv_mode = xapic_extint,
                .trig_mode = xapic_edge,
		.mask = xapic_masked } );

    //LINT1: usually used to generate an NMI
    //generate NMI
    //disabled (FIXME?)
    xapic_lvt_lint1_wr(&apic, (xapic_lvt_lint_t) {
                .vector = 0,
		.dlv_mode = xapic_extint, //xapic_nmi,
                .trig_mode = xapic_edge,
                .mask = xapic_masked } );
#endif

    //error interrupt register
    xapic_lvt_err_wr(&apic, (xapic_lvt_err_t) {
            .vector = APIC_ERROR_INTERRUPT_VECTOR,
		.mask = xapic_not_masked } );

#if 0 // this doesn't seem necessary, and causes problems on non-AMD HW -AB
    //we need to enable this bit in the AMD case, but not in the Intel case.
    if (CPU_IS_M5_SIMULATOR) {
        printf("Warning: not setting xapic_eacr_wr on M5\n");
    } else {
        //    if (strcmp(cpuid_res.cpu_vendor,"AuthenticAMD")==0) {
        xapic_eacr_wr(&apic, (xapic_eacr_t) { .iern = 1 } );
        //    }
    }
#endif

#if 0 // Dump the APIC
    static char prtbuf[1000];
    xapic_pr(prtbuf,sizeof(prtbuf),&apic);
    printf("%.*s", (int)sizeof(prtbuf), prtbuf);
#endif

#if !defined(__scc__) || defined(RCK_EMU)
    // enable the thing, if it wasn't already!
    if (!apic_base_msr.global) {
        apic_base_msr.global = 1;
        ia32_apic_base_wr(NULL,apic_base_msr);
    }
#endif
}

/** \brief This function sends an IPI
    Two INIT IPIs can be sent, assert or de-assert INIT IPIs. This function is
    called by either apic_send_init_assert or apic_send_init_deassert.
    apic_init() has to be called once before using this function,
    because this function doesn't map the physical page of the APIC
    registers another time.
    \param int_cmd_1 The structure containing the bits for interrupt
           command register 1
    \param destination The destination for the INIT IPI
    \param wait Should we wait for delivery?
*/

static void apic_send_ipi( xapic_icr_lo_t cmd, uint8_t destination, bool wait)
{
    xapic_icr_lo_un u;
    u.val = cmd;

    //clear the previous error, if nobody was interested before.
    //otherwise it isn't possible to send another IPI
    xapic_esr_wr_raw(&apic,0);
    xapic_esr_wr_raw(&apic,0);

    //send the IPI
    xapic_icr_hi_wr(&apic, (xapic_icr_hi_t) { .dest = destination } );
    xapic_icr_lo_wr_raw(&apic, u.raw);

    // Wait for delivery
    while( wait && xapic_icr_lo_rd(&apic).dlv_stat );
}

/** \brief Send an INIT IPI (assert mode)
    This function sends an INIT IPI to the destination processor
    or the processors included in the shorthand in assert mode.
    \param destination The destination, if shorthand = xapic_none
    \param destination_shorthand THe shorthand of the destination which may be
           xapic_none, xapic_self, xapic_all_inc or xapic_all_exc
*/

void apic_send_init_assert(uint8_t destination, uint8_t destination_shorthand)
{
    apic_send_ipi( (xapic_icr_lo_t) { .vector = 0,
                .dlv_mode = xapic_init,
                .dst_mode = xapic_dst_phys,
                .level = xapic_lvl_set,
                .trig_mode = xapic_level,
                .dst_short = destination_shorthand
        }, destination, true );
}

/** \brief Send an INIT IPI (de-assert mode)
    This function sends an INIT IPI to all processors. INIT IPIs in
    de-assert mode are always sent to all processors, regardless of the
    destination value and the destination shorthand value.
*/

void apic_send_init_deassert(void)
{
    apic_send_ipi( (xapic_icr_lo_t) { .vector = 0,
                .dlv_mode = xapic_init,
                .dst_mode = xapic_dst_phys,
                .level = xapic_lvl_clr,
                .trig_mode = xapic_level,
                .dst_short = xapic_all_inc }, 0, true );
}


/** \brief Send a Start-Up IPI

    This function sends a Start-Up IPI to the destination processor
    or the processors included in the shorthand.
    \param destination The destination, if shorthand = xapic_none
    \param destination_shorthand THe shorthand of the destination which may be
           xapic_none, xapic_self, xapic_all_inc or xapic_all_exc
*/

void apic_send_start_up(uint8_t destination,
                        uint8_t destination_shorthand,
                        uint8_t realmode_startpage)
{
    apic_send_ipi( (xapic_icr_lo_t) { .vector = realmode_startpage,
                .dlv_mode = xapic_startup,
                .dst_mode = xapic_dst_phys,
                .level = xapic_lvl_set,
                .trig_mode = xapic_edge,
                .dst_short = destination_shorthand }, destination, true );
}


/** \brief Send an standard IPI to the definded target APIC
    This function sends an standard IPI to the target APIC
    \param destination The destination, if shorthand = xapic_none
    \param destination_shorthand THe shorthand of the destination which may be
           xapic_none, xapic_self, xapic_all_inc or xapic_all_exc
*/

void apic_send_std_ipi(uint8_t destination,
                       uint8_t destination_shorthand,
                       uint8_t vector)
{
    apic_send_ipi( (xapic_icr_lo_t) { .vector = vector,
                .dlv_mode = xapic_fixed,
                .dst_mode = xapic_dst_phys,
                .level = xapic_lvl_clr,
                .trig_mode = xapic_edge,
                .dst_short = destination_shorthand }, destination, false );
}

/** \brief This function sends an EOI to the local APIC
    An End-Of-Interrupt is sent to the local APIC. This function should be
    called at the end of an interrupt handler.
*/
void apic_eoi(void)
{
    //has to be 0 for future compability
    xapic_eoi_wr(&apic,0);
}

/** \brief This function sends a specific EOI to the local APIC
    A specific End-Of-Interrupt is sent to the local APIC. This function should
    be called at the end of an interrupt handler.
*/
void apic_seoi(uint8_t int_nr)
{
    xapic_seoi_wr(&apic, (xapic_seoi_t) { .vector = int_nr } );
}

uint8_t apic_get_id(void)
{
    return xapic_id_rd(&apic).id;
}

/**
 * Mask the timer interrupt.
 */
void apic_mask_timer(void)
{
    xapic_lvt_timer_t tv = xapic_lvt_timer_rd(&apic);
    tv.mask = xapic_masked;
    xapic_lvt_timer_wr(&apic,tv);
}

/**
 * Unmask the timer interrupt.
 */
void apic_unmask_timer(void)
{
    xapic_lvt_timer_t tv = xapic_lvt_timer_rd(&apic);
    tv.mask = xapic_not_masked;
    xapic_lvt_timer_wr(&apic,tv);
}

bool arch_core_is_bsp(void)
{
    return bsp;
}

xapic_esr_t apic_get_esr(void)
{
    // 10.5.3: Have to write to ESR before reading it
    xapic_esr_wr_raw(&apic, 0);
    return xapic_esr_rd(&apic);
}
