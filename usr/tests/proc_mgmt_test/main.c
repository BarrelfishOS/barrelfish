/** \file
 *  \brief Process Management test.
 */

/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/deferred.h>
#include <barrelfish/proc_mgmt_client.h>
#include <bench/bench.h>

#define PROC_MGMT_BENCH 1
#define PROC_MGMT_BENCH_MIN_RUNS 150

int my_id;
// int total_ids;

static void test_spawn(coreid_t core_id, char *argv[],
	                   struct capref *ret_domain_cap)
{
	assert(ret_domain_cap != NULL);

	errval_t err = proc_mgmt_spawn_program(core_id,
		                                   "/x86_64/sbin/proc_mgmt_test",
		                                   argv, NULL, 0, ret_domain_cap);
	if (err_is_ok(err)) {
		// printf("OK: my_id=%i, spawn(%u, proc_mgmt_test)\n", my_id, core_id);
	} else {
		// printf("FAIL: spawn(%u, proc_mgmt_test), err=%s\n", core_id,
		// 	   err_getstring(err));
	}
}

// static void test_span(coreid_t core_id)
// {
// 	errval_t err = proc_mgmt_span(core_id);
// 	if (err_is_ok(err)) {
// 		printf("OK: span(%u)\n", core_id);
// 	} else {
// 		printf("FAIL: span(%u), err=%s\n", core_id, err_getstring(err));
// 	}
// }

// static void test_kill(struct capref domain_cap)
// {
// 	errval_t err = proc_mgmt_kill(domain_cap);
// 	if (err_is_ok(err)) {
// 		printf("OK: kill\n");
// 	} else {
// 		printf("FAIL: kill, err=%s\n", err_getstring(err));
// 	}
// }

// static void test_wait(struct capref domain_cap)
// {
// 	uint8_t code;
// 	errval_t err = proc_mgmt_wait(domain_cap, &code);
// 	if (err_is_ok(err)) {
// 		printf("OK: wait, code=%u\n", code);
// 	} else {
// 		printf("FAIL: wait, err=%s\n", err_getstring(err));
// 	}
// }

static inline cycles_t calculate_time(cycles_t tsc_start, cycles_t tsc_end)
{
    cycles_t result;
    if (tsc_end < tsc_start) {
        result = (LONG_MAX - tsc_start) + tsc_end - bench_tscoverhead();
    } else {
        result = (tsc_end - tsc_start - bench_tscoverhead());
    }
    return result;
}

int main(int argc, char **argv)
{
	if (argc == 3) {
		bench_init();
		
		cycles_t tsc_start, tsc_end;
    	cycles_t result;
    	uint64_t tscperus;
    	
    	bench_ctl_t *ctl = calloc(1, sizeof(*ctl));
	    ctl->mode = BENCH_MODE_FIXEDRUNS;
	    ctl->result_dimensions = 1;
	    ctl->min_runs = PROC_MGMT_BENCH_MIN_RUNS;
        ctl->data = calloc(ctl->min_runs * ctl->result_dimensions,
        	               sizeof(*ctl->data));

		errval_t err = sys_debug_get_tsc_per_ms(&tscperus);
	    assert(err_is_ok(err));
	    tscperus /= 1000;

		coreid_t target_core = 7;//strcmp(argv[1], "1") == 0 ? 1 : 2;
		char *spawn_argv[] = { "proc_mgmt_test", "noop", NULL };
		my_id = atoi(argv[1]);
        struct capref domain_cap;
		// total_ids = atoi(argv[2]);

        barrelfish_usleep((0 - 1 * my_id) * 100 * 1000);

	    // ctl = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 1, 100);
	    do {
	    	// debug_printf("BEFORE test_spawn\n");
	        tsc_start = bench_tsc();

			test_spawn(target_core, spawn_argv, &domain_cap);
			// test_wait(domain_cap);

	        tsc_end = bench_tsc();
	        result = calculate_time(tsc_start, tsc_end);

			barrelfish_usleep(0 * 1000 * 1000);
	    }while (!bench_ctl_add_run(ctl, &result));
	    // } while (true);

	    bench_ctl_dump_analysis(ctl, 0, "client", tscperus);

    	bench_ctl_destroy(ctl);

		barrelfish_usleep(5 * 1000 * 1000);

		printf("TEST DONE\n");
	} else {
		// for(;;) {
  //       	errval_t err = event_dispatch(get_default_waitset());
  //       	if(err_is_fail(err)) {
  //           	USER_PANIC_ERR(err, "event_dispatch");
  //       	}
  //       }
	}

	// printf("HELLO from proc_mgmt_test, argc=%u\n", argc);
	// // errval_t err;
	// if (argc == 1) {
	// 	struct capref domain_cap_0;
	// 	char *argv_0[] = { "proc_mgmt_test", "1", NULL };
	// 	test_spawn(0, argv_0, &domain_cap_0);
	// 	barrelfish_usleep(5*1000*1000);
	// 	// while (true);
	// 	test_kill(domain_cap_0);
	// 	test_wait(domain_cap_0);
	// 	// barrelfish_usleep(0 * 1*1*1);

	// 	struct capref domain_cap_1;
	// 	char *argv_1[] = { "proc_mgmt_test", "1", "2", NULL };
	// 	test_spawn(1, argv_1, &domain_cap_1);
	// 	test_wait(domain_cap_1);

	// 	printf("TEST DONE\n");
	// 	// barrelfish_usleep(5*1000*1000);

	// 	// err = proc_mgmt_kill(domain_cap);
	// 	// if (err_is_fail(err)) {
	// 	// 	USER_PANIC_ERR(err, "failed to kill proc_mgmt_test");
	// 	// }

	// 	// uint8_t status;
	// 	// err = proc_mgmt_wait(domain_cap, &status);
	// 	// printf("2nd proc_mgmt_test finished with status %u\n", status);
	// } else if (argc == 2) {
	// 	// test_span(disp_get_core_id() == 0 ? 1 : 0);

	// 	// // struct capability ret;
	// 	// // err = debug_cap_identify(cap_domainid, &ret);
	// 	// // if (err_is_fail(err)) {
	// 	// // 	USER_PANIC_ERR(err, "failed to identify cap_domainid");
	// 	// // }
	// 	// // assert(ret.type == ObjType_Domain);
	// 	// // printf("proc_mgmt_test: cap_domainid = { .coreid=%u, .core_local_id=%u "
	// 	// // 	   "}\n", ret.u.domain.coreid, ret.u.domain.core_local_id);

	// 	// // coreid_t other_core = disp_get_core_id() == 0 ? 1 : 0;
	// 	// // err = proc_mgmt_span(other_core);//domain_new_dispatcher(other_core, span_cb, NULL);
	// 	// // if (err_is_fail(err)) {
	// 	// // 	USER_PANIC_ERR(err, "failed to span proc_mgmt_test on core %u\n",
	// 	// // 		           other_core);
	// 	// // }

	// 	// // barrelfish_usleep(5*1000*1000);
	// 	// // printf("Main dispatcher exiting...\n");

	// 	for(;;) {
 //        	errval_t err = event_dispatch(get_default_waitset());
 //        	if(err_is_fail(err)) {
 //            	USER_PANIC_ERR(err, "event_dispatch");
 //        	}
 //        }
	// } else {
	// 	// barrelfish_usleep(5 * 1000 * 1000);
	// 	// We'll just exit normally here, spawner should be waiting for us.
	// }

    return 0;
}
