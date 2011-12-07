/**
 * \file
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>

#include <if/dist_defs.h>
#include <if/dist_thc.h>
#include <thc/thc.h>


int main(int argc, char *argv[])
{
    printf("hello world!\n");
    errval_t err;

    struct dist_thc_client_binding_t cl;
    struct dist_binding* b = NULL;
    err = dist_thc_connect_by_name("dist2_rpc",
                                   get_default_waitset(),
                                   IDC_BIND_FLAGS_DEFAULT,
                                   &b);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "connect failed!");
        abort();
    }

    err = dist_thc_init_client(&cl, b, b);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "init failed");
        abort();
    }


    //cl.call_seq.del();

    return EXIT_SUCCESS;
}
