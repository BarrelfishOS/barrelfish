#include <stdio.h>
#include <barrelfish/deferred.h>

#include "test_instr.h"

#ifndef UNDER_TEST
#error "Must set UNDER_TEST define"
#endif

static uint64_t ticks_per_msec = 0;

void test_instr_init(struct e1000_driver_state *eds){
    // Setup state for test
    errval_t err;
    if(ticks_per_msec == 0){
        err = sys_debug_get_tsc_per_ms(&ticks_per_msec);
        printf("Ticks per msec: %"PRIu64".\n", ticks_per_msec);
        assert(err_is_ok(err));
    }

    e1000_set_interrupt_throttle(eds, E1000_INT_THROTTLE_RATE_DISABLED);
    eds->test_initialized = true;

    printf("e1000_irqtest: Disabled interrupt throttling\n");

    static struct periodic_event test_periodic;
    err = periodic_event_create(&test_periodic, get_default_waitset(), 
            300*1000, MKCLOSURE(test_instr_periodic, eds));

    assert(err_is_ok(err));
    printf("e1000_irqtest: Registered periodic event\n");
};

#define ICR_E1000E_OTHER 24

static void trigger_lsc_interrupt(struct e1000_driver_state * eds){
    printf("(%s) Creating Link change interrupt...\n", eds->inst_name);

    // Cause an (artificial) interrupt. Trigger LSC for legacy
    // and 'other' for MSIx
    e1000_intreg_t ics = 0;
    ics = e1000_intreg_lsc_insert(ics, 1);
    ics |= 1 << ICR_E1000E_OTHER;

    e1000_ics_rawwr(eds->device, ics);
    eds->int_trigger_counter++;
}


void test_instr_interrupt(struct e1000_driver_state *eds, e1000_intreg_t icr){
    printf("e1000_irqtest (%s): got interrupt!!!\n", eds->inst_name);
    if (e1000_intreg_lsc_extract(icr) != 0) {
        printf("link-state interrupt\n");
        eds->lsc_interrupt_counter++;
    }
};

void test_instr_periodic(void *arg){
    uint64_t current_tick;
    struct e1000_driver_state *eds = arg;
    if(eds->int_trigger_counter >= 50){
        if (abs(eds->int_trigger_counter - eds->lsc_interrupt_counter) <= 5) {
            printf("triggerred: %d and received %d interrupts. (+-5 is okay).\n",
                    eds->int_trigger_counter, eds->lsc_interrupt_counter);
            printf("TEST SUCCESS (%s)\n", eds->inst_name);
        }
        else {
            printf("triggerred: %d and received %d interrupts. (+-5 is okay).\n",
                    eds->int_trigger_counter, eds->lsc_interrupt_counter);
            printf("(%s) TEST FAILURE\n", eds->inst_name);
        }
        exit(0);
    }
    if(eds->test_initialized){
        current_tick = rdtsc();
        if(eds->last_int_trigger_ticks + ticks_per_msec*300 < current_tick){
            eds->last_int_trigger_ticks = current_tick;
            trigger_lsc_interrupt(eds);
        }
    }
};
