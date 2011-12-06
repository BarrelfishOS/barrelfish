#ifndef DIST2_COMMON_H_
#define DIST2_COMMON_H_

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#include <barrelfish/barrelfish.h>

#include <if/dist_event_defs.h>
#include <if/dist_rpcclient_defs.h>


struct dist_rpc_client* get_dist_rpc_client(void);
struct dist_event_binding* get_dist_event_binding(void);

#define MAX_RECORD_LENGTH (5*1024)

static inline errval_t allocate_string(char* object, va_list args, size_t* length, char** buf)
{
	*length = vsnprintf(NULL, 0, object, args);

	if(*length > MAX_RECORD_LENGTH) {
		return DIST2_ERR_RECORD_SIZE;
	}

	*buf = malloc((*length)+1); // include \0
	if(buf == NULL) {
		return LIB_ERR_MALLOC_FAIL;
	}

	return SYS_ERR_OK;
}

#endif /* DIST2_COMMON_H_ */
