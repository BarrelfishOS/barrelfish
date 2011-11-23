#ifndef DIST2_DEBUG_H_
#define DIST2_DEBUG_H_

#define DIST_SERVICE_DEBUG 1

#if defined(DIST_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
#define DIST2_DEBUG(x...) printf("dist2_service: " x)
#else
#define DIST2_DEBUG(x...) ((void)0)
#endif

#endif // DIST2_DEBUG_H_
