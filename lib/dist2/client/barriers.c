#include <dist2/barrier.h>

#include "common.h"

/**
 *
 * b = barrier_init(n)
 * barrier_enter(b);        blocks until all n have entered
 *
 * [critical section]
 *
 * barrier_leave(b);        blocks until all clients have come here
 *
 *
 *
 *
 **/



/**
 *
 * barrier_init( myBarrier { registered: 0 } )
 *
 *
 * barrier_enter()
 *  subscribe( myBarrier { registered: > %d }, numberOfCores )
 *  set_publish( myBarrier { registered: incr() } )
 *
 *
 * barrier_leave( myBarrier { wait_until: decr() } )
 *
 * barrier_destroy()
 *
 */
