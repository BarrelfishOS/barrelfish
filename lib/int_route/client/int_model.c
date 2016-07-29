#include "int_route/int_model.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define UINT64_T_HEX_LENGTH 18
#define SEP_LENGTH 1
#define PARAM_NAME "int_model="

/*
 *  Turn an int_startup_argument into a string
 *  out must be free'd by the caller.
 */
errval_t int_startup_argument_to_string(struct int_startup_argument * arg, char ** out) {
    *out = malloc(strlen(PARAM_NAME) + 3*UINT64_T_HEX_LENGTH + 2*SEP_LENGTH + 1);
    sprintf(*out, PARAM_NAME "%d:%"PRIx64":%"PRIx64, (int)arg->model, arg->int_range_start,
            arg->int_range_end);
    return SYS_ERR_OK;

}

/* Parse a argv string into a int_startup_argument */
errval_t int_startup_argument_parse(char * in, struct int_startup_argument * out) {
    if(strncmp(in, PARAM_NAME, strlen(PARAM_NAME)) == 0) {
        int m = sscanf(in, PARAM_NAME "%d:%"SCNx64":%"SCNx64, (int*)&(out->model), &(out->int_range_start),
                &(out->int_range_end));
        if(m == 3) return SYS_ERR_OK;
    }
    return SYS_ERR_IRQ_INVALID;
}
