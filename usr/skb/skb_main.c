/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <eclipse.h>

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <vfs/vfs.h>
#include <sys/types.h>
#include <unistd.h>

#include "dist/service.h"
#include "dist/predicates.h"
#include "include/skb_server.h"
#include "shared_lib_dict.h"
#include "skb_debug.h"

#define MEMORY_SIZE 32*1024*1024
#define ECLIPSE_DIR "/skb"

#define RESULT_BUF_SIZE 1024

int skb_init(void);
void execute_string(char *string);


void test_function_call_ez(void) {
	printf("test_function_call_ez\n");
}

void test_function_call(char* what) {
	printf("what was: %s\n", what);
}


int main(int argc, char**argv)
{
    // we'll be needing this...
    vfs_mkdir("/tmp");

//make sure, that dlsym has the right table to the statically compiled-in
//shared libraries...
    dlopen_set_params(&funcs, sizeof(funcs)/sizeof(struct function_entry));

//now set the right values for the eclipse-clp engine
    ec_set_option_int(EC_OPTION_IO, MEMORY_IO);
    ec_set_option_ptr(EC_OPTION_ECLIPSEDIR, ECLIPSE_DIR);
    ec_set_option_long(EC_OPTION_GLOBALSIZE, MEMORY_SIZE);

//    ec_.m.vm_flags |= 8;
    int n = ec_init();
    if (n != 0) {
        SKB_DEBUG("\nskb_main: ec_init() failed. Return code = %d\n", n);
    } else {
        SKB_DEBUG("\nskb_main: ec_init() succeeded.\n");
    }

    execute_string("set_flag(print_depth,100).");

    event_server_init();
    skb_server_init();
    SKB_DEBUG("\nskb initialized\n");

    chdir(ECLIPSE_DIR);


//  execute_string("append([1,2,3],[5,6,7],L),write(output,L).");

    // dist2 related stuff
    execute_string("[objects].");
    execute_string("[pubsub].");
    execute_string("[bindings].");
    ec_external(ec_did("identification_complete", 1), p_identification_complete, ec_did("eclipse",0));

    execute_string("[queries].");

//  execute_string("get_local_affinity(1,B,L),write(output,[B,L]).");

//  execute_string("lib(branch_and_bound).");
//  execute_string("minimize(member(X,[4,1,2]),X),write(output,X).");

    //don't terminate the skb domain
    messages_handler_loop();
}

int skb_init(void)
{
    int n;

    SKB_DEBUG("\ninitialize eclipse\n");
    n = ec_init();

    if (n != 0) {
        SKB_DEBUG("\nskb_main: ec_init() failed.");
    }
    return (0);
}


void execute_string(char *string)
{
    char    buf[RESULT_BUF_SIZE];
    int n;

    ec_post_string(string);
    int res = 7; //means that we have to flush the output.
    while (res == 7) {
        res = ec_resume();
        SKB_DEBUG("\nres = %d\n", res);

        //give back the result and the error messages.
        //in case there is no message, a '.' is still returned
        n = ec_queue_read(1, buf, RESULT_BUF_SIZE);
        if ((n >=0) && (n < RESULT_BUF_SIZE)) {
            buf[n] = 0;
        }
        SKB_DEBUG("eclipse returned: %s with length %d.\n", buf,n);

        n = ec_queue_read(2, buf, RESULT_BUF_SIZE);
        if ((n >=0) && (n < RESULT_BUF_SIZE)) {
            buf[n] = 0;
        }
        SKB_DEBUG("eclipse error returned: %s with length %d.\n", buf,n);
    }
}

void post_and_execute_string(void)
{
    char    buf[RESULT_BUF_SIZE];
    int n;

    //post and execute the string.
    //should be more sophisticated in the future...
//    ec_post_string(exec_string);
    ec_resume();

    //give back the result and the error messages.
    //in case there is no message, a '.' is still returned
    n = ec_queue_read(1, buf, RESULT_BUF_SIZE);
    SKB_DEBUG("eclipse returned: %s.\n", buf);

    n = ec_queue_read(2, buf, RESULT_BUF_SIZE);
    SKB_DEBUG("eclipse error returned: %s.\n", buf);
}

