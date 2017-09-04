#ifndef USR_DRIVERS_MLX4_DEBUG_H_
#define USR_DRIVERS_MLX4_DEBUG_H_

#include <barrelfish/debug.h>

#define MLX4_DEBUG_ENABLED 1

#if MLX4_DEBUG_ENABLED
#define MLX4_DEBUG(x...) debug_printf(x)
#define MLX4_WARN(x...) debug_printf(x)
#define MLX4_ERR(x...) debug_printf(x)
#else
#define MLX4_DEBUG(x... )
#define MLX4_WARN(x... )
#define MLX4_ERR(x... )
#endif

#endif /* USR_DRIVERS_MLX4_DEBUG_H_ */
