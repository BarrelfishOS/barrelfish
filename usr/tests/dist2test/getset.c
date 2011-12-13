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
int id = 0;

static errval_t get_set_test(void)
{
	printf("Running get_set_test()\n");
	char* data = NULL;

	/*errval_t err2 = dist_get("loafjaksdfj", &data);
	assert(err_no(err2) == DIST2_ERR_NO_RECORD);*/

	// Set some data objects
	errval_t err = dist_set(SET_DEFAULT, "object1 { weight: %d }", 20);
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_set failed!");
		return err;
	}

	// TODO: Do we want this?
	err = dist_set(SET_DEFAULT, "object2 { weight: 25, weight: 20 }");
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_set failed!");
		return err;
	}

	err = dist_set(SET_DEFAULT, "object3 { attr: 'A text string.', weight: 9, fl: 12.0 }");
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_set failed!");
		return err;
	}

	err = dist_set(SET_DEFAULT, "object4 { attr: 'Somestring', weight: 20, fl: 12.0 }");
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_set failed!");
		return err;
	}

	err = dist_set(SET_DEFAULT, "object5 { pattern1: '123abab', pattern2: 'StringToTestRegexMatching', pattern3: '2010-10-10' }");
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_set failed!");
		return err;
	}

	err = dist_set(SET_SEQUENTIAL, "seq_ { desc: 'This is a sequential Object.' }");
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
	assert(strcmp(data, "object1 { weight: 20 }") == 0);
	free(data);

	err = dist_get("object2 { weight: 25 }", &data);
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_get failed!");
		return err;
	}
	//printf("data: %s\n", data);
	assert(strcmp(data, "object2 { weight: 25 }") == 0);
	free(data);

	err = dist_get("object4", &data);
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_get failed!");
		return err;
	}
	//printf("data: %s\n", data);
	assert(strcmp(data, "object4 { attr: Somestring, weight: 20, fl: 12.0 }") == 0);
	free(data);

	err = dist_get("_ { weight: >= 10, fl: > 11.0 }", &data);
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_get failed!");
		return err;
	}
	//printf("data: %s\n", data);
	assert(strcmp(data, "object4 { attr: Somestring, weight: 20, fl: 12.0 }") == 0);
	free(data);

	err = dist_get("_ { weight: >= 10, fl: > 11.0 }", &data);
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_get failed!");
		return err;
	}
	//printf("data: %s\n", data);
	assert(strcmp(data, "object4 { attr: Somestring, weight: 20, fl: 12.0 }") == 0);
	free(data);

	err = dist_del("object4");
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_get failed!");
		return err;
	}
	err = dist_get("object4", &data);
	assert(err_no(err) == DIST2_ERR_NO_RECORD);
	//free(data); TODO??

	err = dist_set(SET_DEFAULT, "object4 { attr: 'Somestring', weight: 20, fl: 12.0 }");
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_set failed!");
		return err;
	}
	err = dist_get("object4", &data);
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_get failed!");
		return err;
	}
	assert(strcmp(data, "object4 { attr: Somestring, weight: 20, fl: 12.0 }") == 0);
	free(data);

	err = dist_del("object1");
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_get failed!");
		return err;
	}
	err = dist_get("object1", &data);
	printf("data: %s\n", data);
	assert(err_no(err) == DIST2_ERR_NO_RECORD);


	err = dist_get("object2 { weight: 25 }", &data);
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_get failed!");
		return err;
	}
	//printf("data: %s\n", data);
	assert(strcmp(data, "object2 { weight: 25 }") == 0);
	free(data);

	char** names = NULL;
	size_t size = 0;
	err = dist_get_names(&names, &size, "_ { weight: _ }");
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "dist_get_names failed!");
        return err;
    }
    assert(size == 3);
    assert(strcmp(names[0], "object2") == 0);
    assert(strcmp(names[1], "object3") == 0);
    assert(strcmp(names[2], "object4") == 0);
    dist_free_names(names, size); // XXX name strings not freed

    err = dist_get_names(&names, &size, "_ { attr: _, weight: %d }", 20);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "dist_get_names failed!");
        return err;
    }
    assert(size == 1);
    assert(strcmp(names[0], "object4") == 0);
    dist_free_names(names, size); // XXX name strings not freed


	// TODO implement dist_del with constraints, attributes!

	//Not working atm. need GNU libc regex library

	//err = dist_get("_ { pattern1: r'^12.*ab$' }", &data);
	//if(err_is_fail(err)) {
	//	DEBUG_ERR(err, "dist_get failed!");
	//	return err;
	//}
	//printf("data: %s\n", data);
	//assert(strcmp(data, "object4 [bool :: [val true], fl :: [val 12.0], weight :: [val 20], attr :: [val Somestring]]") == 0);
	//free(data);

	printf("\tget_set_test() success.\n");
	return SYS_ERR_OK;
}

size_t incoming_messages = 0;

static void subhandler(subscription_t id1, char* object, void* st)
{
	assert(object != NULL);

	switch(id1) {
		case 0:
		    printf("object: %s\n", object);
			assert(strcmp(object, "publishIt { weight: 10, height: 20, depth: 30 }") == 0);
		break;

		case 1:
			assert(strcmp(object, "publishIt { age: 10 }") == 0);
		break;
	}

	printf("subhandler(%lu): id:%lu obj:%s\n", ++incoming_messages, id1, object);
	free(object);
}


static errval_t pub_sub_test(void)
{
	errval_t err = SYS_ERR_OK;

	subscription_t id1 = 0;
	err = dist_subscribe(subhandler, NULL, &id1, "_ { weight: %d }", 10);
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_get failed!");
		return err;
	}
	printf("subscription done with id: %lu\n", id1);

	subscription_t id2 = 0;
	err = dist_subscribe(subhandler, NULL, &id2, "_ { age: > %d }", 9);
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_get failed!");
		return err;
	}
	printf("subscription done with id: %lu\n", id2);

	err = dist_publish("publishIt { weight: %d, height: %d, depth: %d }", 10, 20, 30);
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_publish failed!");
		return err;
	}

	err = dist_unsubscribe(id);

	// Should not deliver
	err = dist_publish("publishIt { weight: %d, height: %d }", 10, 9999);
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_publish failed!");
		return err;
	}

	// Should not deliver
	err = dist_publish("publishIt { age: %d }", 9);
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_publish failed!");
		return err;
	}

	err = dist_publish("publishIt { age: %d }", 10);
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_publish failed!");
		return err;
	}

	return err;
}


static void subhandler2(subscription_t id1, char* object, void* st)
{
	assert(object != NULL);
	debug_printf("subhandler2(%lu): id:%lu obj:%s\n", ++incoming_messages, id1, object);
	free(object);
}

static void main_subscriber(void) {
	errval_t err;

	subscription_t id1 = 0;
	err = dist_subscribe(subhandler2, NULL, &id1, "_ { weight: %d }", 10);
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_subscribe failed!");
		abort();
	}
	printf("subscription done with id: %lu\n", id1);

	subscription_t id2 = 0;
	err = dist_subscribe(subhandler2, NULL, &id2, "_ { age: > %d }", 9);
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "dist_subscribe failed!");
		abort();
	}
	printf("subscription done with id: %lu\n", id2);

	messages_handler_loop();
}


int main(int argc, char *argv[])
{
    errval_t err = SYS_ERR_OK;
    skb_client_connect();
    dist_init();


    if (argc == 2 && strcmp(argv[1], "subscriber") == 0) {
        main_subscriber();
    } else {

		err = get_set_test();
		assert(err_is_ok(err));

		err = pub_sub_test();
		assert(err_is_ok(err));
    }

    messages_handler_loop(); // so it does not exit yet...

    printf("dist2test passed successfully!\n");
	return EXIT_SUCCESS;
}
