#define DEBUG 1
#ifdef DEBUG
#define DEBUG_COW(x...) debug_printf(x)
#else
#define DEBUG_COW(x...) ((void)0)
#endif

