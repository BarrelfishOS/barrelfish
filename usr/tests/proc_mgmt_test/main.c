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
#include <barrelfish/sys_debug.h>
#include <barrelfish/spawn_client.h>
#include <bench/bench.h>

#define PROC_MGMT_BENCH 1
#define PROC_MGMT_BENCH_MIN_RUNS 20

static errval_t test_spawn(coreid_t core_id, char *argv[],
	                       struct capref *ret_domain_cap)
{
	assert(ret_domain_cap != NULL);

	errval_t err = spawn_program(core_id, "/x86_64/sbin/proc_mgmt_test",
		                         argv, NULL, 0, ret_domain_cap);
	if (err_is_fail(err)) {
        return err;
	}
    return SYS_ERR_OK;
}

/*
static void test_span(coreid_t core_id)
{
 	errval_t err = proc_mgmt_span(core_id);
 	if (err_is_fail(err)) {
    }
}

static void test_kill(struct capref domain_cap)
{
 	errval_t err = proc_mgmt_kill(domain_cap);
 	if (err_is_ok(err)) {
        USER_PANIC("Failed killing domain")
    }
}

static void test_wait(struct capref domain_cap)
{
 	uint8_t code;
 	errval_t err = proc_mgmt_wait(domain_cap, &code);
 	if (err_is_fail(err)) {
        USER_PANIC("Failed waiting for domain");
 	}
}
*/

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

static void print_ps(domainid_t* domains, size_t len)
{
    errval_t err;
    for(size_t i = 0; i < len; i++) {
        struct spawn_ps_entry pse;
        char *argbuf, status;
        size_t arglen;
        errval_t reterr;

        err = spawn_get_status(domains[i], &pse, &argbuf, &arglen, &reterr);
        if (err_is_fail(err)) {
            USER_PANIC("PS FAILED \n");
        }
        if(err_is_fail(reterr)) {
            USER_PANIC("PS FAILED \n");
        }

        switch(pse.status) {
        case 0:
            status = 'N';
            break;

        case 1:
            status = 'R';
            break;

        case 2:
            status = 'S';
            break;

        case 3:
            status = 'S';
            break;

        default:
            status = '?';
            break;
        }

        printf("%-8u\t%c\t", domains[i], status);
        size_t pos = 0;
        for(int p = 0; pos < arglen && p < MAX_CMDLINE_ARGS;) {
            printf("%s ", &argbuf[pos]);
            char *end = memchr(&argbuf[pos], '\0', arglen - pos);
            assert(end != NULL);
            pos = end - argbuf + 1;
        }
        printf("\n");

        free(argbuf);
    }
}

static void run_benchmark_spawn(coreid_t target_core)
{
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

    struct capref domain_cap;

    char *spawn_argv[] = { "proc_mgmt_test", "0", "norun", NULL};
    do {
        tsc_start = bench_tsc();

        test_spawn(target_core, spawn_argv, &domain_cap);

        tsc_end = bench_tsc();
        result = calculate_time(tsc_start, tsc_end);
    } while (!bench_ctl_add_run(ctl, &result));    

    cap_destroy(domain_cap);
    bench_ctl_dump_analysis(ctl, 0, "client", tscperus);

    bench_ctl_destroy(ctl);
}

int main(int argc, char **argv)
{   
    errval_t err;
	if (argc == 3) {
        if (strcmp("starter", argv[2]) == 0) {
            // just continue;
        } else if (strcmp("norun", argv[2]) == 0) {
            // directly return
            return 0;
        } else if (strcmp("run", argv[2]) == 0) {   
            // Run infinite loop
            printf("Running infinite Loop");
            while(true) {
                printf("Running infinite Loop");
                barrelfish_usleep(1000*1000);
                event_dispatch_non_block(get_default_waitset());
            }
        } else if (strcmp("sleeper", argv[2]) == 0) {
            // Process that we wait for to finish
            printf("Running for a few seconds \n");
            barrelfish_usleep(10*1000*1000);
            printf("Sleeper exit\n");
            return 0;
        } else if (strcmp("span", argv[2]) == 0) {
            // Process that spans domains
            if (disp_get_core_id() == 0) {
                spawn_span(1);
            } else {
                spawn_span(0);
            }
            while(true) {
                event_dispatch(get_default_waitset());
            }
        } else {
            USER_PANIC("Unknown Role \n ");
        }

	} else {
        USER_PANIC("Not enough arguments to run test \n ");
    }

    struct capref domain_cap;
    printf("Testing kill on same core\n");
    char *spawn_argv[] = { "proc_mgmt_test", "0", "run", NULL};
    err = test_spawn(disp_get_core_id(), spawn_argv, &domain_cap);
    if (err_is_fail(err)) {
        USER_PANIC("Failed spawning program proc_mgmt_test \n");
    }  

    //starting a process takes some time ...
    barrelfish_usleep(5*1000*1000);

    printf("Killing process \n");
 	err = spawn_kill(domain_cap);
 	if (err_is_fail(err)) {
        USER_PANIC("Failed waiting for domain \n");
 	}
 
    // Killing a process takes some time ...
    barrelfish_usleep(5*1000*1000);

    printf("Testing kill on other core\n");
    err = test_spawn(disp_get_core_id()+1, spawn_argv, &domain_cap);
    if (err_is_fail(err)) {
        USER_PANIC("Failed spawning program proc_mgmt_test \n");
    }  

    barrelfish_usleep(5*1000*1000);

    printf("Killing process \n");
 	err = spawn_kill(domain_cap);
 	if (err_is_fail(err)) {
        USER_PANIC("Failed waiting for domain \n");
 	}

    // TODO check if process was killed
    printf("Testing spaning on different core\n");
    char *spawn_argv3[] = { "proc_mgmt_test", "0", "span", NULL};
    err = test_spawn(disp_get_core_id()+1, spawn_argv3, &domain_cap);
    if (err_is_fail(err)) {
        USER_PANIC("Failed spawning program proc_mgmt_test \n");
    }

    printf("Testing spaning on same core\n");
    err = test_spawn(disp_get_core_id(), spawn_argv3, &domain_cap);
    if (err_is_fail(err)) {
        USER_PANIC("Failed spawning program proc_mgmt_test \n");
    }  

    printf("Testing wait on different core process\n");
    char *spawn_argv2[] = { "proc_mgmt_test", "0", "sleeper"};
    err = test_spawn(disp_get_core_id()+1, spawn_argv2, &domain_cap);
    if (err_is_fail(err)) {
        USER_PANIC("Failed spawning program proc_mgmt_test \n");
    }  
 
    barrelfish_usleep(5*1000*1000);
    
 	uint8_t code;
    printf("Waiting for process on different core to finish \n");
 	err = spawn_wait(domain_cap, &code, false);
 	if (err_is_fail(err)) {
        USER_PANIC("Failed waiting for domain \n");
 	}
    printf("Unblocked \n");

    printf("Testing wait on same core process\n");
    err = test_spawn(disp_get_core_id(), spawn_argv2, &domain_cap);
    if (err_is_fail(err)) {
        USER_PANIC("Failed spawning program proc_mgmt_test \n");
    }  
 
    barrelfish_usleep(5*1000*1000);
    
    printf("Waiting for process on same core to finish \n");
 	err = spawn_wait(domain_cap, &code, true);
 	if (err_is_fail(err)) {
        USER_PANIC("Failed waiting for domain \n");
 	}
    printf("Nowait hang return code %d \n", code);
 	err = spawn_wait(domain_cap, &code, false);
 	if (err_is_fail(err)) {
        USER_PANIC("Failed waiting for domain \n");
 	}
    printf("Unblocked \n");

 	err = spawn_wait(domain_cap, &code, true);
 	if (err_is_fail(err)) {
        USER_PANIC("Failed waiting for domain \n");
 	}
    printf("Nowait hang return code %d \n", code);
    printf("Running benchmarks core 0 \n");
    run_benchmark_spawn(disp_get_core_id());
    printf("Running benchmarks core 1 \n");
    run_benchmark_spawn(disp_get_core_id()+1);

    for (int j = 0; j < 10; j++) {
        domainid_t* domains;
        size_t len;
        printf("Get domain list sorted \n");
        err = spawn_get_domain_list(true, &domains, &len);
        if (err_is_fail(err)){
            USER_PANIC("Failed getting domain ids \n");
        }

        print_ps(domains, len);
        free(domains);

        printf("Get domain list unsorted \n");
        err = spawn_get_domain_list(false, &domains, &len);
        if (err_is_fail(err)){
            USER_PANIC("Failed getting domain ids \n");
        }

        print_ps(domains, len);

        free(domains);
    }
    printf("TEST DONE\n");
    return 0;
}
