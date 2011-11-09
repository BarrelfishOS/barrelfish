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
#include <dist2/dist2.h>

static errval_t get_set_test(void)
{
	printf("Running get_set_test()\n");
	char* data = NULL;

	// Set some data objects
	errval_t err = dist_set("object1 { weight: %d }", 20);
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_set failed!");
		return err;
	}

	// TODO: Do we want this?
	err = dist_set("object2 { weight: 20, weight: 25 }");
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_set failed!");
		return err;
	}

	err = dist_set("object3 { attr: 'A text string.', weight: 9, fl: 12.0 }");
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_set failed!");
		return err;
	}

	err = dist_set("object4 { attr: 'Somestring', weight: 20, fl: 12.0, bool: true }");
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_set failed!");
		return err;
	}

	err = dist_set("object5 { pattern1: '123abab', pattern2: 'StringToTestRegexMatching', pattern3: '2010-10-10' }");
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_set failed!");
		return err;
	}

	// Query for objects
	err = dist_get("object1", &data);
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_get failed!");
		return err;
	}
	assert(strcmp(data, "object1 [weight :: [val 20]]") == 0);
	free(data);

	err = dist_get("object2 { weight: 25 }", &data);
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_get failed!");
		return err;
	}
	//printf("data: %s\n", data);
	assert(strcmp(data, "object2 [weight :: [val 25]]") == 0);
	free(data);

	err = dist_get("object4", &data);
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_get failed!");
		return err;
	}
	//printf("data: %s\n", data);
	assert(strcmp(data, "object4 [bool :: [val true], fl :: [val 12.0], weight :: [val 20], attr :: [val Somestring]]") == 0);
	free(data);

	err = dist_get("_ { weight: >= 10, fl: > 11.0 }", &data);
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_get failed!");
		return err;
	}
	//printf("data: %s\n", data);
	assert(strcmp(data, "object4 [bool :: [val true], fl :: [val 12.0], weight :: [val 20], attr :: [val Somestring]]") == 0);
	free(data);

	err = dist_get("_ { weight: >= 10, fl: > 11.0 }", &data);
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_get failed!");
		return err;
	}
	//printf("data: %s\n", data);
	assert(strcmp(data, "object4 [bool :: [val true], fl :: [val 12.0], weight :: [val 20], attr :: [val Somestring]]") == 0);
	free(data);

	/*
	 * Not working atm. need GNU libc regex library
	 *
	err = dist_get("_ { pattern1: r'^12.*ab$' }", &data);
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_get failed!");
		return err;
	}
	printf("data: %s\n", data);
	//assert(strcmp(data, "object4 [bool :: [val true], fl :: [val 12.0], weight :: [val 20], attr :: [val Somestring]]") == 0);
	free(data);
	*/


	printf("\tget_set_test() success.\n");
	return SYS_ERR_OK;
}


static void subhandler(subscription_t id, char* object)
{
	printf("subhandler: %lu: %s\n", id, object);
}


static errval_t pub_sub_test(void)
{
	errval_t err = SYS_ERR_OK;

	subscription_t id = 0;
	err = dist_subscribe(subhandler, &id, "_ { weight: %d }", 10);
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_get failed!");
		return err;
	}
	printf("subscription done with id: %lu\n", id);

	err = dist_publish("publishIt { weight: %d, height: %d, depth: %d }", 10, 20, 30);
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_get failed!");
		return err;
	}

	return err;
}

int main(int argc, char *argv[])
{
    errval_t err = SYS_ERR_OK;
    skb_client_connect();
    dist_init();

    printf("Start dist2 Tests:\n");
    err = get_set_test();
    assert(err_is_ok(err));

    err = pub_sub_test();
    assert(err_is_ok(err));


    printf("dist2test passed successfully!\n");

    messages_handler_loop();

	return EXIT_SUCCESS;
}
