#ifndef DIST2_INIT_H_
#define DIST2_INIT_H_

#include <barrelfish/barrelfish.h>

errval_t event_server_init(void);
errval_t rpc_server_init(void);

errval_t dist_server_init(void);

#endif /* DIST2_INIT_H_ */
