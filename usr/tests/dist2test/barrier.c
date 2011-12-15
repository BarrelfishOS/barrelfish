/**
 * \file
 * \brief start for libdist2 tests
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <dist2/dist2.h>


int main(int argc, char *argv[])
{
	assert(argc >= 2);
    errval_t err = SYS_ERR_OK;
    dist_init();

    size_t wait_for = atoi(argv[1]);
    char* record = NULL;
    debug_printf("Barrier test with: %lu processes:\n", wait_for);

    err = dist_barrier_enter("my_barrier", &record, wait_for);
    if(err_is_ok(err)) {
		debug_printf("Execute Barrier code section\n");
		debug_printf("Barrier record is: %s\n", record);
    }
    else {
    	DEBUG_ERR(err, "Barrier enter fail.");
    	abort();
    }
    err = dist_barrier_leave(record);
    DEBUG_ERR(err, "Leaving Barrier");
    assert(err_is_ok(err));

    debug_printf("Process no longer inside barrier.\n");

    free(record);
    return EXIT_SUCCESS;
}
