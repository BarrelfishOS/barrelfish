#ifndef _TEST_INSTR_H_
#define _TEST_INSTR_H_

#include "e1000n.h"

/**
 * Called after the card has been initialized
 */
void test_instr_init(e1000_device_t *dev);

/**
 * Called periodically
 */
void test_instr_periodic(e1000_device_t *dev);

/**
 * Called on every interrupt.
 */
void test_instr_interrupt(e1000_device_t *dev, e1000_intreg_t icr);


#ifndef UNDER_TEST
/**
 * Provide No-op stubs.
 */
inline void test_instr_init(e1000_device_t *dev){};
inline void test_instr_periodic(e1000_device_t *dev){};
inline void test_instr_interrupt(e1000_device_t *dev, e1000_intreg_t icr){};
#endif

#endif
