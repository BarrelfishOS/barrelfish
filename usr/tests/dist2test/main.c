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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <skb/skb.h>
#include <dist2/getset.h>

static errval_t get_set_test(void)
{
	printf("Running get_set_test()\n");
	char* data = NULL;

	errval_t err = dist_set("object1 { weight: %d }", 20);
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_set failed!");
		return err;
	}
	err = dist_get("object1", &data);
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_get failed!");
		return err;
	}
	assert(strcmp(data, "[weight :: [val 20]]") == 0);
	free(data);


	// TODO: Do we want this?
	err = dist_set("object2 { weight: 20, weight: 25 }");
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_set failed!");
		return err;
	}
	err = dist_get("object2 { weight: 25 }", &data);
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_get failed!");
		return err;
	}
	assert(strcmp(data, "[weight :: [val 25]]") == 0);
	free(data);


	err = dist_set("object3 { attr: somestring, weight: 20, fl: 12.0, bool: true }");
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_set failed!");
		return err;
	}
	err = dist_get("object3", &data);
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_get failed!");
		return err;
	}
	printf("data: %s\n", data);
	free(data);

	printf("\tget_set_test() success.\n");
	return SYS_ERR_OK;
}

int main(int argc, char *argv[])
{
    errval_t err = SYS_ERR_OK;
    skb_client_connect();

    printf("Start dist2 Tests:\n");
    err = get_set_test();
    assert(err_is_ok(err));



    printf("dist2test passed successfully!\n");
	return EXIT_SUCCESS;
}
