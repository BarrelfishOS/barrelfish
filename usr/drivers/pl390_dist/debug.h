#ifndef PL390_DEBUG_H_
#define PL390_DEBUG_H_

//#define PL390_DEBUG_ON

#if defined(PL390_DEBUG_ON)
#define PL390_DEBUG(x...) debug_printf("pl390_dist:" x)
#else
#define PL390_DEBUG(x...) ((void)0) 
#endif 

#endif
