/**
 * \file
 * \brief Test RPC calls with triggers in dist2
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
#include <dist2/dist2.h>
#include <skb/skb.h>
#include "common.h"


int main(int argc, char *argv[])
{
    errval_t err;
    err = skb_client_connect();
    ASSERT_ERR_OK(err);
    dist_init();
    
    size_t i = 0;
    while(1) {
	/*
	char** names = NULL;
	size_t len = 0;
	err = dist_get_names(&names, &len, "_ { attr: _}");
	ASSERT_ERR_OK(err);
	debug_printf("names: %p, len is: %lu", names, len);
	for(size_t i=0; i<len; i++) {
		debug_printf("names[%lu] at %p: %s\n", i, names[i], names[i]);
	}
	dist_free_names(names, len);*/

/*
	char* rec = "rec_ { lock: '123' }";
	char* ret = NULL;
	err = dist_set_get(SET_SEQUENTIAL, &ret, rec);
	DEBUG_ERR(err, "getset return");
	ASSERT_ERR_OK(err);
	
	err = dist_del(ret);
	ASSERT_ERR_OK(err);

	free(ret);
*/

/*        char* rec = "rec%d { lock: '123' }";
        err = dist_set(rec, ++i);
        DEBUG_ERR(err, "set return");
        ASSERT_ERR_OK(err);

	err = dist_del(rec, i);
	ASSERT_ERR_OK(err);

	size_t len;
	char** names;
	err = dist_get_names(&names, &len, "_ { lock: _ }");
	ASSERT_ERR(err, DIST2_ERR_NO_RECORD); */

	char* fmt = "add_object(rec%d, [ lock::'123' ]).";
	char buf[100];
	sprintf(buf, fmt, ++i);
	
	char* res = NULL;
	char* str_error = NULL;
	int ierr = 0;
	err = skb_evaluate(buf, &res, &str_error, &ierr);
	debug_printf("skb evaluate set: %s\n", res);
	ASSERT_ERR_OK(err);
	assert(ierr == 0);
	free(res);
	free(str_error);


        fmt = "del_object(rec%d).";
        sprintf(buf, fmt, i);

	res = NULL;
	str_error = NULL;
	ierr = 0;
        err = skb_evaluate(buf, &res, &str_error, &ierr);
        ASSERT_ERR_OK(err);
        assert(ierr == 0);
        free(res);
        free(str_error);
    }

    return EXIT_FAILURE;
}
