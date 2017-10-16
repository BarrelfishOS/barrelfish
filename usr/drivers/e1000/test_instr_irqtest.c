#include <stdio.h>

#include "test_instr.h"

#ifndef UNDER_TEST
#error "Must set UNDER_TEST define"
#endif

static uint64_t lsc_interrupt_counter = 0;

static int e1000_initialized = false;
static uint64_t int_trigger_counter = 0;
static uint64_t ticks_per_msec, current_tick, last_int_trigger_ticks;

void test_instr_init(e1000_device_t *dev){
    // Setup state for test
    errval_t err;
    err = sys_debug_get_tsc_per_ms(&ticks_per_msec);
    printf("Ticks per msec: %"PRIu64".\n", ticks_per_msec);
    assert(err_is_ok(err));

    e1000_set_interrupt_throttle(dev, E1000_INT_THROTTLE_RATE_DISABLED);
    e1000_initialized = true;

    printf("e1000_irqtest: Disabled interrupt throttling\n");
};

void test_instr_interrupt(e1000_device_t *dev, e1000_intreg_t icr){
    if (e1000_intreg_lsc_extract(icr) != 0) {
        printf("link-state interrupt\n");
        lsc_interrupt_counter++;
    }
};

#define ICR_E1000E_OTHER 24

void test_instr_periodic(e1000_device_t *dev){

    if(int_trigger_counter >= 50){
        if (abs(int_trigger_counter - lsc_interrupt_counter) <= 5) {
            printf("triggerred: %"PRIi64" and received %"PRIi64" interrupts. (+-5 is okay).\n",
                    int_trigger_counter, lsc_interrupt_counter);
            printf("TEST SUCCESS\n");
        }
        else {
            printf("triggerred: %"PRIi64" and received %"PRIi64" interrupts. (+-5 is okay).\n",
                    int_trigger_counter, lsc_interrupt_counter);
            printf("TEST FAILURE\n");
        }
        exit(0);
    }
    if(e1000_initialized){
        current_tick = rdtsc();
        if(last_int_trigger_ticks + ticks_per_msec*300 < current_tick){
            last_int_trigger_ticks = current_tick;
            printf("Creating Link change interrupt...\n");

            // Cause an (artificial) interrupt. Trigger LSC for legacy
            // and 'other' for MSIx
            e1000_intreg_t ics = 0;
            ics = e1000_intreg_lsc_insert(ics, 1);
            ics |= 1 << ICR_E1000E_OTHER;

            e1000_ics_rawwr(dev->device, ics);
            int_trigger_counter++;
        }
    }
};
