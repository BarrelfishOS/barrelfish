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
#include <stdlib.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <skb/skb.h>
#include <dist2/getset.h>

int main(int argc, char *argv[])
{
    skb_client_connect();

	printf("dist2 Library tests:\n");

	printf("dist_set:\n");
	errval_t err = dist_set("object1 { weight: 20 }");
	//assert(err_is_ok(err));
	printf("dist_set returned: %s\n", err_getstring(err));


	printf("dist_get:\n");
	char* data = NULL;
	err = dist_get("object1", &data);

	printf("dist_get returned: %s\n", err_getstring(err));
	printf("data is: %s\n", data);


	return EXIT_SUCCESS;
}
