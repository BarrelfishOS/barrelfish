#ifndef __E1000_DEBUG_H__
#define __E1000_DEBUG_H__

#include "e1000n.h"

/*****************************************************************
 * Debug printer:
 *****************************************************************/
  
#if defined(E1000_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
#define E1000_DEBUG(fmt, ...) printf(DRIVER_STRING fmt, ##__VA_ARGS__)
#else
#define E1000_DEBUG(fmt, ...) ((void)0)
#endif

#endif
