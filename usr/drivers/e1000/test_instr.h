#ifndef _TEST_INSTR_H_
#define _TEST_INSTR_H_

#include "e1000n.h"

/**
 * Called after the card has been initialized
 */
void test_instr_init(struct e1000_driver_state *eds);

/**
 * Called periodically
 */
void test_instr_periodic(void *arg);

/**
 * Called on every interrupt.
 */
void test_instr_interrupt(struct e1000_driver_state *eds, e1000_intreg_t icr);

#ifndef UNDER_TEST
/**
 * Provide No-op stubs.
 */
void test_instr_init(struct e1000_driver_state *eds){};
void test_instr_periodic(void *eds){};
void test_instr_interrupt(struct e1000_driver_state *eds, e1000_intreg_t icr){};
#endif

#endif
