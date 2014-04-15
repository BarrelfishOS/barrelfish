/**
 * \file
 * \brief Core boot main
 */
/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "coreboot.h"

uint64_t start = 0;
uint64_t end = 0;

coreid_t my_arch_id;
struct capref kernel_cap;

bool done = false;
bool kcb_stored = false;
struct capref kcb;

#if defined(MICROBENCH)
static int real_main(int argc, char **argv);
int main(int argc, char **argv)
{
    errval_t err;

    vfs_init();

    bench_arch_init();

    err = connect_to_acpi();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "connect to acpi failed.");
    }

    err = oct_init();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Octopus initialization failed.");
    }

#if DOWNUPDATE
    int argc_down = 4;
    char *argv_down[] = {
        "x86boot",
        "auto",
        "down",
        "1"
    };
    int argc_up = 4;
    char *argv_up[] = {
        "x86boot",
        "auto",
        "update",
        "1"
    };

    uint64_t start_down, end_down, start_up, end_up;

    printf("# ticks-down ms-down ticks-up ms-up\n");
    for (size_t i = 0; i < 20; i++) {
        start_down = bench_tsc();
        real_main(argc_down, argv_down);
        end_down = bench_tsc();

        start_up = bench_tsc();
        real_main(argc_up, argv_up);
        end_up = bench_tsc();

        printf("%lu %lu %lu %lu\n",
               end_down - start_down, bench_tsc_to_ms(end_down - start_down),
               end_up - start_up, bench_tsc_to_ms(end_up - start_up));

    }
#endif

#if UPDATE
    int argc_update = 4;
    char *argv_update[] = {
        "x86boot",
        "auto",
        "update",
        "1"
    };
    uint64_t start_up, end_up;

    printf("# ticks-update ms-update ticks-x86boot ms-x86boot\n");
    for (size_t i = 0; i < 20; i++) {

        start_up = bench_tsc();
        real_main(argc_update, argv_update);
        end_up = bench_tsc();

        //while(*ap_dispatch != 2);

        printf("%lu %lu %lu %lu\n",
               end_up - start_up, bench_tsc_to_ms(end_up - start_up),
               end - start, bench_tsc_to_ms(end - start));
    }
#endif

#if TAKE
    int argc_up = 4;
    // Give kcb from core 2 to core 1
    char *argv_up[] = {
        "x86boot",
        "auto",
        "up",
        "2"
    };

    int argc_give = 5;
    // Give kcb from core 2 to core 1
    char *argv_give[] = {
        "x86boot",
        "auto",
        "give",
        "2",
        "1"
    };

    int argc_take = 6;
    // Taking kcb.2 from core 1 to core 0
    char *argv_take_ping[] = {
        "x86boot",
        "auto",
        "take",
        "2",
        "1",
        "0"
    };

    // Taking kcb.2 from core 0 to core 1
    char *argv_take_pong[] = {
        "x86boot",
        "auto",
        "take",
        "2",
        "0",
        "1"
    };

    // Set-up, start 2 and give 2 to 1
    real_main(argc_up, argv_up);
    real_main(argc_give, argv_give);

    uint64_t start_take1, end_take1;
    uint64_t start_take2, end_take2;

    printf("# ticks-takegive ms-takegive\n");
    for (size_t i = 0; i < 20; i++) {

        start_take1 = bench_tsc();
        real_main(argc_take, argv_take_ping);
        end_take1 = bench_tsc();

        start_take2 = bench_tsc();
        real_main(argc_take, argv_take_pong);
        end_take2 = bench_tsc();

        printf("%lu %lu %lu %lu\n",
               end_take1 - start_take1, bench_tsc_to_ms(end_take1 - start_take1),
               end_take2 - start_take2, bench_tsc_to_ms(end_take2 - start_take2));

    }
#endif


}
#endif

#if !defined(MICROBENCH)
int main(int argc, char **argv)
#else
static int real_main(int argc, char **argv)
#endif
{
    start = bench_tsc();

    errval_t err;
    for (size_t i = 0; i < argc; i++) {
        DEBUG("%s:%s:%d: argv[i]=%s\n",
              __FILE__, __FUNCTION__, __LINE__, argv[i]);
    }
    if (argc < 4) {
        DEBUG("%s:%s:%d: Not enough arguments\n", __FILE__, __FUNCTION__, __LINE__);
        return 1;
    }

#if !defined(MICROBENCH)
    vfs_init();

    bench_arch_init();

    err = connect_to_acpi();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "connect to acpi failed.");
    }

    err = oct_init();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Octopus initialization failed.");
    }
#endif

    struct monitor_binding *st = get_monitor_binding();
    st->rx_vtbl.boot_core_reply = boot_core_reply;
    st->rx_vtbl.power_down_response = power_down_response;


    struct monitor_blocking_rpc_client *mc = get_monitor_blocking_rpc_client();
    err = mc->vtbl.get_arch_core_id(mc, (uintptr_t *)&my_arch_id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "get_arch_core_id failed.");
    }
    DEBUG("%s:%d: my_arch_id is %"PRIuCOREID"\n", __FILE__, __LINE__, my_arch_id);

    err = mc->vtbl.get_kernel_cap(mc, &kernel_cap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "get_kernel_cap failed.");
    }

    for (size_t i = 0; i < argc; i++) {
        DEBUG("%s:%d: argv[i]=%s\n", __FILE__, __LINE__, argv[i]);
    }

    coreid_t target_id = (coreid_t) strtol(argv[3], NULL, 16);
    assert(target_id < MAX_COREID);


    err = create_or_get_kcb_cap(target_id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Can not get kcb.");
    }

    //enum cpu_type type = (enum cpu_type) atoi(argv[4]);
    //assert(type < CPU_TYPE_NUM);

    if (!strcmp(argv[2], "up")) { // TODO(gz) should be boot!
        char sched[32] = { 0 };
        if ((strlen(argv[2]) > 3) && argv[2][2] == '=') {
            char *s = argv[2] + 3;
            int i;
            for (i = 0; i < 31; i++) {
                if (!s[i] || s[i] == ' ') {
                    break;
                }
            }
            memcpy(sched, s, i);
            sched[i] = 0;
        }

        struct capref frame;
        size_t framesize;
        struct frame_identity urpc_frame_id;
        err = frame_alloc_identify(&frame, MON_URPC_SIZE, &framesize, &urpc_frame_id);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "frame_alloc_identify failed.");
        }
        err = cap_mark_remote(frame);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Can not mark cap remote.");
            return err;
        }

        struct monitor_binding *mb = get_monitor_binding();
        err = mb->tx_vtbl.boot_core_request(mb, NOP_CONT, target_id, frame);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "boot_core_request failed");
        }

        err = spawn_xcore_monitor(target_id, target_id, CPU_X86_64, sched,
                                  "loglevel=2 logmask=0", urpc_frame_id);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "spawn xcore monitor failed.");
        }
    } else if (!strcmp(argv[2], "update")) {
        char sched[32] = { 0 };
        if ((strlen(argv[2]) > 3) && argv[2][2] == '=') {
            char *s = argv[2] + 3;
            int i;
            for (i = 0; i < 31; i++) {
                if (!s[i] || s[i] == ' ') {
                    break;
                }
            }
            memcpy(sched, s, i);
            sched[i] = 0;
        }

        struct capref frame;
        size_t framesize;
        struct frame_identity urpc_frame_id;
        err = frame_alloc_identify(&frame, MON_URPC_SIZE, &framesize, &urpc_frame_id);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "frame_alloc_identify failed.");
        }
        err = cap_mark_remote(frame);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Can not mark cap remote.");
            return err;
        }

        // do clean(ish) shutdown
        // TODO(gz): Use designated IRQ number
        err = sys_debug_send_ipi(target_id, 0, 40);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "debug_send_ipi to power it down failed.");
        }

        done = true;
        err = spawn_xcore_monitor(target_id, target_id, CPU_X86_64, sched,
                                  "loglevel=0 logmask=0", urpc_frame_id);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "spawn xcore monitor failed.");
        }

        //TODO(gz): while (*ap_dispatch != 1);
    } else if (!strcmp(argv[2], "down")) {
        DEBUG("%s:%d: Power it down...\n", __FILE__, __LINE__);
        /*err = st->tx_vtbl.power_down(st, NOP_CONT, target_id);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "power_down failed.");
        }*/

        // TODO(gz): Use designated IRQ number
        err = sys_debug_send_ipi(target_id, 0, 40);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "debug_send_ipi to power it down failed.");
        }
        done = true;
    } else if (!strcmp(argv[2], "give")) {
        assert (argc == 5);
        DEBUG("%s:%d: Give kcb from core %s to core %s...\n",
              __FILE__, __LINE__, argv[3], argv[4]);

        coreid_t destination_id = (coreid_t) strtol(argv[4], NULL, 16);
        assert(destination_id < MAX_COREID);

        /*DEBUG("%s:%s:%d: power down target_id=%"PRIuCOREID"\n",
               __FILE__, __FUNCTION__, __LINE__, target_id);
        err = st->tx_vtbl.power_down(st, NOP_CONT, target_id);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "power_down failed.");
        }

        while(!done) {
            err = event_dispatch(get_default_waitset());
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "error in event_dispatch");
            }
        }
        done = false;*/
        // TODO(gz): Use designated IRQ number
        err = sys_debug_send_ipi(target_id, 0, 40);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "debug_send_ipi to power it down failed.");
        }
        done = true;

        err = give_kcb_to_new_core(destination_id, kcb);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "Can not send KCB to another core.");
        }
    } else if (!strcmp(argv[2], "take")) {
        assert (argc == 6);
        DEBUG("%s:%s:%d: Taking kcb.%s from core %s to core %s\n", __FILE__,
              __FUNCTION__, __LINE__, argv[3], argv[4], argv[5]);

        coreid_t source_id = (coreid_t) strtol(argv[4], NULL, 16);
        coreid_t destination_id = (coreid_t) strtol(argv[5], NULL, 16);

        assert(source_id < MAX_COREID);
        assert(destination_id < MAX_COREID);

        struct monitor_blocking_rpc_client *mc = get_monitor_blocking_rpc_client();

        errval_t ret_err;
        // send message to monitor to be relocated -> don't switch kcb ->
        // remove kcb from ring -> msg ->
        // (disp_save_rm_kcb -> next/home/... kcb -> enable switching)
        errval_t err = mc->vtbl.forward_kcb_rm_request(mc, source_id, kcb, &ret_err);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "forward_kcb_request failed.");
        }
        if (err_is_fail(ret_err)) {
            USER_PANIC_ERR(ret_err, "forward_kcb_request failed.");
        }

        err = give_kcb_to_new_core(destination_id, kcb);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "Can not send KCB to another core.");
        }
        done = true;

        /*char sched[32] = { 0 };
        if ((strlen(argv[2]) > 3) && argv[2][2] == '=') {
             char *s=argv[2]+3;
             int i;
             for (i = 0; i < 31; i++) {
                 if (!s[i] || s[i] == ' ') {
                     break;
                 }
             }
             memcpy(sched, s, i);
             sched[i] = 0;
        }

        struct capref frame;
        size_t framesize;
        struct frame_identity urpc_frame_id;
        err = frame_alloc_identify(&frame, MON_URPC_SIZE, &framesize, &urpc_frame_id);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "frame_alloc_identify failed.");
        }
        err = cap_mark_remote(frame);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Can not mark cap remote.");
            return err;
        }

        struct monitor_binding *mb = get_monitor_binding();
        err = mb->tx_vtbl.boot_core_request(mb, NOP_CONT, target_id, frame);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "boot_core_request failed");
        }

        err = spawn_xcore_monitor(target_id, target_id, CPU_X86_64, sched,
                                  "loglevel=0 logmask=1", urpc_frame_id);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "spawn xcore monitor failed.");
        }*/
    } else if (!strcmp(argv[2], "upwith")) { // TODO(gz) should be boot!
        assert(argc == 5);
        char sched[32] = { 0 };
        if ((strlen(argv[2]) > 3) && argv[2][2] == '=') {
            char *s = argv[2] + 3;
            int i;
            for (i = 0; i < 31; i++) {
                if (!s[i] || s[i] == ' ') {
                    break;
                }
            }
            memcpy(sched, s, i);
            sched[i] = 0;
        }

        coreid_t destination_id = (coreid_t) strtol(argv[4], NULL, 16);

        struct capref frame;
        size_t framesize;
        struct frame_identity urpc_frame_id;
        err = frame_alloc_identify(&frame, MON_URPC_SIZE, &framesize, &urpc_frame_id);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "frame_alloc_identify failed.");
        }
        err = cap_mark_remote(frame);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Can not mark cap remote.");
            return err;
        }

        // TODO(gz): Use designated IRQ number
        err = sys_debug_send_ipi(2, 0, 40);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "debug_send_ipi to power it down failed.");
        }
        done = true;

        err = spawn_xcore_monitor(destination_id, destination_id, CPU_X86_64, sched,
                                  "loglevel=2 logmask=0", urpc_frame_id);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "spawn xcore monitor failed.");
        }
    } else if (!strcmp(argv[2], "resume")) {
        DEBUG("%s:%s:%d: Resume...\n", __FILE__, __FUNCTION__, __LINE__);
        err = invoke_start_core();
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "resume core failed.");
        }
        done = true;
    } else {
        DEBUG("%s:%s:%d: unknown cmd = %s\n",
              __FILE__, __FUNCTION__, __LINE__, argv[2]);
        done = true;
    }

    DEBUG("%s:%s:%d: Wait for message.\n",
          __FILE__, __FUNCTION__, __LINE__);
    while (!done) {
        err = event_dispatch(get_default_waitset());
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "error in event_dispatch");
        }
    }

    DEBUG("%s:%s:%d: We're done here...\n", __FILE__, __FUNCTION__, __LINE__);

#if defined(ENSURE_SEQUENTIAL)
    char *barrier;
    err = oct_barrier_enter("x86boot", &barrier, 2);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "can not enter x86boot.");
    }

    err = oct_barrier_leave(barrier);
    if (err_is_fail(err)) {
        if (err_no(err) == OCT_ERR_NO_RECORD) {
            // ignore
        } else {
            USER_PANIC_ERR(err, "oct_barrier_leave");
        }
    }
#endif

    return 0;
}
