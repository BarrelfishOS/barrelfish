#include "int_route/int_model.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <inttypes.h>

#define UINT64_T_HEX_LENGTH 18
#define SEP_LENGTH 1
#define PARAM_NAME "int_model="

errval_t int_startup_argument_init(struct int_startup_argument * arg) {
    memset(arg, 0, sizeof(struct int_startup_argument));
    return SYS_ERR_OK;
}

/*
 *  Turn an int_startup_argument into a string
 *  out must be free'd by the caller.
 */
errval_t int_startup_argument_to_string(struct int_startup_argument * arg, char ** out) {
    int out_len = strlen(PARAM_NAME) + 3*UINT64_T_HEX_LENGTH + 2*SEP_LENGTH + 1;
    if(arg->msix_ctrl_name){
        out_len += strlen(SEP_LENGTH + arg->msix_ctrl_name); 
    }
    *out = malloc(out_len);
    snprintf(*out, out_len, PARAM_NAME "%d:%"PRIx64":%"PRIx64,
            (int)arg->model,
            arg->int_range_start, arg->int_range_end);

    // append msix ctlr name
    if(arg->msix_ctrl_name){
        strcat(*out, ":");
        strcat(*out, arg->msix_ctrl_name);
    }
    return SYS_ERR_OK;

}

/* Parse a argv string into a int_startup_argument */
errval_t int_startup_argument_parse(char * in, struct int_startup_argument * out) {
    if(strncmp(in, PARAM_NAME, strlen(PARAM_NAME)) == 0) {
        int name_start = -1;
        int m = sscanf(in, PARAM_NAME "%d:%"SCNx64":%"SCNx64":%n",
                (int*)&(out->model),
                &(out->int_range_start),
                &(out->int_range_end),
                &name_start);
        if(name_start > -1){
           out->msix_ctrl_name = in + name_start; 
        }

        if(m == 3) return SYS_ERR_OK;
    }
    return SYS_ERR_IRQ_NO_ARG;
}
