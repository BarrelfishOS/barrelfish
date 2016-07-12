#include "int_model.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define UINT64_T_HEX_LENGTH 18
#define SEP_LENGTH 1
#define PARAM_NAME "int_model="

errval_t int_startup_argument(struct int_startup_argument * arg, char ** out) {
    *out = malloc(strlen(PARAM_NAME) + 3*UINT64_T_HEX_LENGTH + 2*SEP_LENGTH + 1);
    sprintf(*out, PARAM_NAME "%d:%"PRIx64":%"PRIx64, (int)arg->model, arg->int_range_start,
            arg->int_range_end);
    return SYS_ERR_OK;

}
