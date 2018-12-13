#ifndef PL130_DEBUG_H_
#define PL130_DEBUG_H_

#define PL130_DEBUG_ON

#if defined(PL130_DEBUG_ON)
#define PL130_DEBUG(x...) debug_printf("pl130_dist:" x)
#else
#define PL130_DEBUG(x...) ((void)0) 
#endif 

#endif
