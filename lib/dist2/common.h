#ifndef COMMON_H_
#define COMMON_H_

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#include <barrelfish/barrelfish.h>

#define MAX_OBJECT_LENGTH (5*1024)

static inline errval_t format_object(char** out, char* fmt, va_list args)
{
	assert(out != NULL);
	assert(fmt != NULL);
	errval_t err = SYS_ERR_OK;

	// Construct query
	*out = malloc(MAX_OBJECT_LENGTH);
	if(*out == NULL) {
		// TODO
		assert(!"todo malloc error");
	}

	size_t bytes_written = vsnprintf(*out, MAX_OBJECT_LENGTH, fmt, args);

	if(bytes_written >= MAX_OBJECT_LENGTH) {
		// TODO return error!
		assert(!"Object string too big, return error!");
	}

	return err;
}

#endif /* COMMON_H_ */
