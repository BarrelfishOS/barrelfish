/**
 * \file
 * \brief Timer calibration and setting functions.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include <kernel.h>
#include <systime.h>
#include <arch/x86/apic.h>
#include <arch/x86/rtc.h>
#include <arch/x86/pit.h>
#include <arch/x86/global.h>
#include <arch/x86/timing.h>

#ifdef __k1om__
#include <xeon_phi.h>
#endif

static uint32_t apic_frequency = 0;
static uint64_t tscperms = 0;

#if !defined(__k1om__)
/**
 * \brief Calibrates local APIC timer against RTC.
 * \return Local APIC timer ticks per RTC second.
 */
static uint32_t __attribute__((unused)) calibrate_apic_timer_rtc(systime_t *systime_freq)
{
    // Set APIC timer to one-shot mode
    apic_timer_init(true, false);
    apic_timer_set_divide(xapic_by1);
    // Wait until start of new second

    uint64_t tsc_start, tsc_end;
    uint8_t start = rtc_read_secs(), now;
    do {
        now = rtc_read_secs();
    } while(now == start);

    apic_timer_set_count(UINT32_MAX);
    tsc_start = rdtsc();

    // Wait a second
    //uint32_t oldcount = UINT32_MAX;
    int reads = 0;
    start = now;
    do {
        now = rtc_read_secs();
#if 0
        // Assert timer never underflows
        // XXX: this fires on QEMU, because the APIC timer is driven directly by
        // gettimeofday() and may go backwards if ntpd is adjusting the clock
        uint32_t curc = apic_timer_get_count();
        assert(curc <= oldcount);
        oldcount = curc;
#endif
        reads++;
    } while(start == now);

    // Get new count
    uint32_t curcount = apic_timer_get_count();
    tsc_end = rdtsc();
    assert(curcount != 0);

    *systime_freq = tsc_end - tsc_start;
    uint32_t tps = UINT32_MAX - curcount;
    printk(LOG_NOTE, "Measured APIC frequency: %u Hz in one RTC second,"
        "systime frequency: %lu Hz\n", tps, *systime_freq);
    return tps;
}
#endif

#if defined(__k1om__)
/**
 * \brief Calibrates local APIC timer against RTC.
 * \return Local APIC timer ticks per RTC second.
 */
static uint32_t calibrate_apic_timer_k1om(void)
{
    // Set APIC timer to one-shot mode
    apic_timer_init(true, false);
    apic_timer_set_divide(xapic_by1);
    // Wait until start of new second

    /*
     * The Intel Xeon PhiTM coprocessor has a SBox MMIO register that provides
     * the current CPU frequency, which can be used to calibrate the LAPIC timer.
     * The TOD clock has to be emulated in software to query the host OS for the
     * time at bootup and then using the LAPIC timer interrupt to update it.
     *
     * COREFREQ = COREFREQ
    */

    /* TODO: mackerel */
    volatile uint32_t *freq;
    freq = (uint32_t*)local_phys_to_mem(XEON_PHI_SBOX_BASE + 0x4100);
    printf("Core Frequency: %u\n", (*freq) & 0xFFF );
    return (((*freq)&0xFFF) * 1000*1000 );
}
#endif


#if 0
/**
 * \brief Calibrates PIT against RTC.
 * \return PIT ticks per RTC second.
 */
static uint32_t calibrate_pit_rtc(void)
{
    // Calibrate against RTC
    rtc_init();
    pit_init();

    // Wait until start of new second
    rtc_wait_next_second();

    pit_timer0_set(0xffff, false);

    // Wait a second
    uint16_t oldcnt = pit_timer0_read();
    uint32_t ticks = 0;
    int reads = 0;
    do {
        uint16_t cnt = pit_timer0_read();
        if(cnt <= oldcnt) {
            ticks += oldcnt - cnt;
        } else {
            ticks += oldcnt + (0xffff - cnt);
        }
        oldcnt = cnt;
        reads++;
    } while((rtc_read_cmos(0xa) & 0x80) == 0);

    printf("Measured %d PIT counts in one RTC second, reads = %d.\n", ticks, reads);
    return ticks;
}

#endif


/**
 * \brief Calibrates local APIC timer against PIT.
 * \return Local APIC timer ticks per PIT second.
 */
static uint32_t __attribute__((unused)) calibrate_apic_timer_pit(systime_t *systime_freq)
{
    // Set APIC timer to one-shot mode
    apic_timer_init(true, false);
    apic_timer_set_divide(xapic_by1);

    // Calibrate against PIT
    pit_init();
    pit_timer0_set(0xffff, false, true); // only lsb

    // Start both timers
    apic_timer_set_count(UINT32_MAX);

    // Wait a second (1,193,182 ticks)
    uint16_t oldcnt = pit_timer0_read_lsb();
    uint64_t timestamp = rdtsc();
    uint32_t ticks = 0;
    do {
        uint16_t cnt = pit_timer0_read_lsb();
        if (cnt <= oldcnt) {
            ticks += oldcnt - cnt;
        } else {
            ticks += oldcnt + 256 - cnt;
        }
        oldcnt = cnt;

    } while (ticks < PIT_TIMER0_FREQUENCY);

    uint64_t afreq = UINT32_MAX - apic_timer_get_count();
    uint64_t sfreq = rdtsc() - timestamp;

    *systime_freq = sfreq;
    printf("Measured APIC frequency: %ld Hz, systime frequency: %ld Hz\n",
           afreq, sfreq);
    return afreq;
}


/// Number of measurement iterations
#define MAX_ITERATIONS  100

/**
 * \brief Calibrates TSC against local APIC timer.
 * \return TSC ticks per local APIC timer tick.
 */
static uint64_t __attribute__((unused)) calibrate_tsc_apic_timer(void)
{
    // Must tick with higher granularity than a millisecond
    assert(apic_frequency > 1000);
    uint32_t ticksperms = apic_frequency / 1000;

    // XXX: Let's hope this fits on the stack (reserve half the stack)
    /* assert(sizeof(uint64_t) * MAX_ITERATIONS < KERNEL_STACK_SIZE / 2); */
    uint64_t timestamps[MAX_ITERATIONS];
    memset(timestamps, 0, sizeof(timestamps));

    // Set APIC timer to periodic mode
    apic_timer_init(true, true);
    apic_timer_set_divide(xapic_by1);
    apic_timer_set_count(UINT32_MAX);

    // Do all measurement iterations
    uint32_t oldcnt = apic_timer_get_count();
    for(int i = 0; i < MAX_ITERATIONS; i++) {
        // Wait a millisecond
        uint32_t ticks = 0;
        do {
            uint32_t cnt = apic_timer_get_count();
            if(cnt <= oldcnt) {
                ticks += oldcnt - cnt;
            } else {
                ticks += oldcnt + (UINT32_MAX - cnt);
            }
            oldcnt = cnt;
        } while(ticks < ticksperms);

        timestamps[i] = rdtsc();
    }

    // Calculate average
    uint64_t tpms = 0;
    for(int i = 1; i < MAX_ITERATIONS; i++) {
        assert(timestamps[i - 1] < timestamps[i]);
        tpms += timestamps[i] - timestamps[i - 1];
    }
    tpms /= MAX_ITERATIONS - 1;

    // Check that jitter is not too high.
    // No more than 1% of values should deviate more than 1%
    // from the average.
    unsigned int outliers = 0;
    uint64_t avgdistance = 0;
    for(int i = 1; i < MAX_ITERATIONS; i++) {
        uint64_t diff = timestamps[i] - timestamps[i - 1],
            distance = diff > tpms ? diff - tpms : tpms - diff;

        if(distance > tpms / 100) {
            outliers++;
        }

        avgdistance += distance;
    }
    avgdistance /= MAX_ITERATIONS - 1;

    // Always round up
    if(outliers > ((MAX_ITERATIONS - 1) / 100 + 1)) {
        printk(LOG_WARN, "Considerable TSC jitter detected! %" PRIu64 " ticks "
               "on average.\n", avgdistance);
    }

    printk(LOG_NOTE, "Measured %" PRIu64 " TSC counts per ms, "
           "%d data points. Average jitter %" PRIu64 " TSC ticks.\n",
           tpms, MAX_ITERATIONS - 1, avgdistance);
    return tpms;
}

void timing_apic_timer_set_ms(unsigned int ms)
{
    // Must tick with higher granularity than a millisecond
    assert(apic_frequency > 1000);
    assert(ms < UINT32_MAX / (apic_frequency / 1000));

    apic_timer_set_divide(xapic_by1);
    apic_timer_set_count(ms * (apic_frequency / 1000));
}

uint32_t timing_get_apic_ticks_per_sec(void)
{
    return apic_frequency;
}

uint64_t timing_get_tsc_per_ms(void)
{
    // Has to tick with at least ms granularity
    assert(tscperms > 0);
    return tscperms;
}

void timing_calibrate(void)
{
    if (CPU_IS_M5_SIMULATOR) {
        // Guess -- avoid delay of calibration
        printk(LOG_WARN, "Warning: using hard-coded timer calibration on M5\n");
        apic_frequency = 31250000;
        tscperms = apic_frequency / 1000;
    } else {
        if(apic_is_bsp()) {
#ifdef __k1om__
            apic_frequency = calibrate_apic_timer_k1om();
            tscperms = calibrate_tsc_apic_timer();
            systime_frequency = tscperms * 1000;
#else
            // apic_frequency = calibrate_apic_timer_pit(&systime_frequency);
            apic_frequency = calibrate_apic_timer_rtc(&systime_frequency);
#endif
            global->apic_frequency = apic_frequency;
            global->systime_frequency = systime_frequency;
        } else {
            apic_frequency = global->apic_frequency;
            systime_frequency = global->systime_frequency;
        }
        tscperms = systime_frequency / 1000;
        apic_systime_frequency_ratio = ((uint64_t)apic_frequency << 32) / systime_frequency;
    }
}

systime_t systime_now(void)
{
    return rdtsc();
}
