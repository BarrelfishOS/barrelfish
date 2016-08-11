#ifndef BLK_DEBUG_H_
#define BLK_DEBUG_H_

//#define BLK_DEBUG_ENABLE 1

#if defined(BLK_DEBUG_ENABLE) || defined(GLOBAL_DEBUG)
#define BLK_DEBUG(x...) printf("BLK: " x)
#else
#define BLK_DEBUG(x...) ((void)0)
#endif

#endif
