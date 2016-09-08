/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/deferred.h>
#include <devif/queue_interface.h>
#include <devif/sfn5122f_devif_direct.h>

int main(int argc, char *argv[])
{

    errval_t err;
    struct devq* q;   

    struct endpoint_state my_state = {
        .endpoint_type = ENDPOINT_TYPE_FORWARD,
        .device_name = "", // name will be assigned 
        .features = 0,
        // TODO .f
    };

    printf("Forward queue created\n");
    err = devq_create(&q, &my_state, "sfn5122f", 1);
    if (err_is_fail(err)){
        printf("%s \n", err_getstring(err));
        USER_PANIC("Allocating devq failed \n");
    }    

    devq_event_loop(&my_state);
    //messages_handler_loop();
}

