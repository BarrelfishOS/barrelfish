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

#include <getopt.h>
#include "coreboot.h"

coreid_t my_arch_id;
struct capref kernel_cap;

bool done = false;

bool benchmark_flag = false;
bool debug_flag = true;
bool new_kcb_flag = false;
bool nomsg_flag = false;

struct bench_data *bench_data = NULL;

char* cmd_kernel_binary = NULL;
char* cmd_monitor_binary = NULL;
char* cmd_kernel_args = "loglevel=2 logmask=0";

#define APIC_INTER_HALT_VECTOR 248

static void load_arch_id(void)
{
    struct monitor_blocking_rpc_client *mc = get_monitor_blocking_rpc_client();
    errval_t err = mc->vtbl.get_arch_core_id(mc, (uintptr_t *)&my_arch_id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "get_arch_core_id failed.");
    }
    DEBUG("%s:%d: my_arch_id is %"PRIuCOREID"\n", __FILE__, __LINE__, my_arch_id);
}

static void setup_monitor_messaging(void)
{
    struct monitor_binding *st = get_monitor_binding();
    st->rx_vtbl.boot_core_reply = boot_core_reply;
    st->rx_vtbl.power_down_response = power_down_response;
}

static void load_kernel_cap(void)
{
    struct monitor_blocking_rpc_client *mc = get_monitor_blocking_rpc_client();
    errval_t err = mc->vtbl.get_kernel_cap(mc, &kernel_cap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "get_kernel_cap failed.");
    }
}

static void initialize(void)
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

    setup_monitor_messaging();
    load_arch_id();
    load_kernel_cap();
}


typedef int(*cmd_fn)(int argc, char** argv);
struct cmd {
    char* name;
    char* desc;
    char* help;
    cmd_fn fn;
    int argc;
};

static int list_kcb(int argc, char **argv) {
    char** names;
    size_t len;
    errval_t err = oct_get_names(&names, &len, "r'kcb\\.[0-9]+'");
    assert(err_is_ok(err));

    for (size_t i=0; i<len; i++) {
        char* record;
        err = oct_get(&record, names[i]);
        assert(err_is_ok(err));

        uint64_t barrelfish_id, kcb_id;
        char* cap_key;
        err = oct_read(record, "_ { kcb_id: %d, barrelfish_id: %d, cap_key: %s }",
                       &barrelfish_id, &kcb_id, &cap_key);
        assert(err_is_ok(err));

        printf("KCB %"PRIu64": CORE_ID=%"PRIu64" CAP_STORAGE_KEY=%s\n",
               kcb_id, barrelfish_id, cap_key);
        
        free(cap_key);
        free(record);
    }
    if (len == 0) {
        DEBUG("%s:%s:%d: No KCB found?\n",
              __FILE__, __FUNCTION__, __LINE__);
    }

    done = true;
    oct_free_names(names, len);
    return 0;
}

static errval_t get_apic_id(coreid_t core_id, archid_t* apic_id)
{
    char* record = NULL;
    errval_t err = oct_get(&record, "hw.processor.%"PRIuCOREID"", core_id);
    if (err_is_fail(err)) {
        goto out;
    }

    uint64_t apic, enabled;
    err = oct_read(record, "_ { apic_id: %d, enabled: %d }",
                   &apic, &enabled);
    assert (enabled);
    if (err_is_fail(err)) {
        goto out;
    }

    *apic_id = (archid_t) apic;
out:
    free(record);
    return err;
}

static int list_cpu(int argc, char **argv) {
    char** names;
    size_t len;
    errval_t err = oct_get_names(&names, &len, "r'hw\\.processor\\.[0-9]+'");
    assert(err_is_ok(err));

    for (size_t i=0; i<len; i++) {
        char* record;
        err = oct_get(&record, names[i]);
        assert(err_is_ok(err));

        uint64_t barrelfish_id, apic_id, processor_id, enabled;
        err = oct_read(record, "_ { barrelfish_id: %d, apic_id: %d, processor_id: %d, enabled: %d }",
                       &barrelfish_id, &apic_id, &processor_id, &enabled);
        assert(err_is_ok(err));

        printf("CPU %"PRIu64": APIC_ID=%"PRIu64" APIC_PROCESSOR_ID=%"PRIu64" ENABLED=%"PRIu64"\n",
               barrelfish_id, apic_id, processor_id, enabled);

        free(record);
    }
    if (len == 0) {
        DEBUG("%s:%s:%d: No cpus found?\n",
              __FILE__, __FUNCTION__, __LINE__);
    }

    done = true;
    oct_free_names(names, len);
    return 0;
}

static int boot_cpu(int argc, char **argv)
{
    coreid_t target_id = (coreid_t) strtol(argv[1], NULL, 16);
    assert(target_id < MAX_COREID);
    
    archid_t target_apic_id;
    errval_t err = get_apic_id(target_id, &target_apic_id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "get_apic_id failed.");
    }

    struct capref kcb;
    err = create_or_get_kcb_cap(target_id, &kcb);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Can not get KCB.");
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
        USER_PANIC_ERR(err, "Can not mark cap remote.");
    }

    struct monitor_binding *mb = get_monitor_binding();
    err = mb->tx_vtbl.boot_core_request(mb, NOP_CONT, target_id, frame);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "boot_core_request failed");
    }

    err = spawn_xcore_monitor(target_id, target_apic_id, 
                              CPU_X86_64, cmd_kernel_args,
                              urpc_frame_id, kcb);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "spawn xcore monitor failed.");
    }

    return 0;
}

/*static int halt_boot(int argc, char **argv)
{
    coreid_t boot_id = (coreid_t) strtol(argv[1], NULL, 16);
    assert(boot_id < MAX_COREID);

    struct capref kcb;
    errval_t err = create_or_get_kcb_cap(boot_id, &kcb);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Can not get KCB.");
    }

    coreid_t down_id = (coreid_t) strtol(argv[2], NULL, 16);

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

    DEBUG("Power down %"PRIuCOREID", lets hope kcb for %"PRIuCOREID" runs there.",
          down_id, boot_id);
    err = sys_debug_send_ipi(down_id, 0, APIC_INTER_HALT_VECTOR);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "debug_send_ipi to power it down failed.");
    }
    done = true;

    DEBUG("Booting %"PRIuCOREID"...", boot_id);
    err = spawn_xcore_monitor(boot_id, boot_id, CPU_X86_64,
                              cmd_kernel_args, urpc_frame_id,
                              kcb);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "spawn xcore monitor failed.");
    }

    return 0;
}*/


static int update_cpu(int argc, char** argv)
{
    coreid_t target_id = (coreid_t) strtol(argv[1], NULL, 16);
    assert(target_id < MAX_COREID);
    struct capref kcb;
    errval_t err = create_or_get_kcb_cap(target_id, &kcb);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Can not get KCB.");
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

    // do clean shutdown
    err = sys_debug_send_ipi(target_id, 0, APIC_INTER_HALT_VECTOR);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "debug_send_ipi to power it down failed.");
    }

    done = true;
    err = spawn_xcore_monitor(target_id, target_id, CPU_X86_64,
                              cmd_kernel_args,
                              urpc_frame_id, kcb);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "spawn xcore monitor failed.");
    }

    //TODO(gz): while (*ap_dispatch != 1);
    return 0;
}

static int stop_cpu(int argc, char** argv)
{
    coreid_t target_id = (coreid_t) strtol(argv[1], NULL, 16);
    assert(target_id < MAX_COREID);

    errval_t err = sys_debug_send_ipi(target_id, 0, APIC_INTER_HALT_VECTOR);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "debug_send_ipi to power it down failed.");
    }
    done = true;

    // The next line is crucial for harness test to pass
    printf("Core %"PRIuCOREID" stopped.", target_id);
    return 0;
}

static int give_kcb(int argc, char** argv)
{
    assert (argc == 3);
    DEBUG("%s:%d: Give KCB from core %s to core %s...\n",
          __FILE__, __LINE__, argv[1], argv[2]);

    coreid_t target_id = (coreid_t) strtol(argv[1], NULL, 16);
    assert(target_id < MAX_COREID);
    struct capref kcb;
    errval_t err = create_or_get_kcb_cap(target_id, &kcb);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Can not get KCB.");
    }

    coreid_t destination_id = (coreid_t) strtol(argv[2], NULL, 16);
    assert(destination_id < MAX_COREID);

    /*err = sys_debug_send_ipi(target_id, 0, APIC_INTER_HALT_VECTOR);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "debug_send_ipi to power it down failed.");
    }*/
    done = true;

    err = give_kcb_to_new_core(destination_id, kcb);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Can not send KCB to another core.");
    }

    return 0;
}

static int remove_kcb(int argc, char** argv)
{
    assert (argc == 3);
    DEBUG("%s:%s:%d: Taking kcb.%s from core %s\n", __FILE__,
          __FUNCTION__, __LINE__, argv[1], argv[2]);

    coreid_t target_id = (coreid_t) strtol(argv[1], NULL, 16);
    assert(target_id < MAX_COREID);
    struct capref kcb;
    errval_t err = create_or_get_kcb_cap(target_id, &kcb);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Can not get KCB.");
    }

    coreid_t source_id = (coreid_t) strtol(argv[2], NULL, 16);
    assert(source_id < MAX_COREID);

    struct monitor_blocking_rpc_client *mc = get_monitor_blocking_rpc_client();

    errval_t ret_err;
    // send message to monitor to be relocated -> don't switch kcb ->
    // remove kcb from ring -> msg ->
    // (disp_save_rm_kcb -> next/home/... kcb -> enable switching)
    err = mc->vtbl.forward_kcb_rm_request(mc, source_id, kcb, &ret_err);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "forward_kcb_request failed.");
    }
    if (err_is_fail(ret_err)) {
        USER_PANIC_ERR(ret_err, "forward_kcb_request failed.");
    }
    done = true;

    //coreid_t destination_id = (coreid_t) strtol(argv[3], NULL, 16);
    //assert(destination_id < MAX_COREID);
    //
    // Move KCB to a core that is currently running
    //
    /*
    err = give_kcb_to_new_core(destination_id, kcb);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Can not send KCB to another core.");
    }
    */

    //
    // Boot the removed KCB on a core that is currently not running
    //
    /*
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

    err = spawn_xcore_monitor(target_id, target_id,
                              CPU_X86_64,
                              cmd_kernel_args,
                              urpc_frame_id, kcb);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "spawn xcore monitor failed.");
    }*/

    return 0;
}

static struct cmd commands[] = {
    {
        "boot",
        "Boot a fresh core with a KCB.",
        "boot <target apic_id>",
        boot_cpu,
        2
    },
    {
        "update",
        "Update the kernel on an existing core.",
        "update <target apic_id>",
        update_cpu,
        2
    },
    {
        "stop",
        "Stop execution on an existing core.",
        "stop <target apic_id>",
        stop_cpu,
        2
    },
    {
        "give",
        "Give kcb from one core to another.",
        "give <from apic_id> <to apic_id>",
        give_kcb,
        3
    },
    {
        "rmkcb",
        "Remove a KCB from a core.",
        "rm <kcb number> <source apic id>",
        remove_kcb,
        3
    },
    /*{
        "hb",
        "Halt a core and reboot another core. (Remove when we have proper move).",
        "hb <boot apic id> <power down apic id>",
        halt_boot,
        2
    },*/
    {
        "lscpu",
        "List current status of all cores.",
        "lscpu",
        list_cpu,
        1
    },
    {
        "lskcb",
        "List current KCBs.",
        "lskcb",
        list_kcb,
        1
    },
    {NULL, NULL, NULL, NULL, 0},
};

static struct option long_options[] = {
    {"debug",   no_argument,       0, 'd'},
    {"kernel",  required_argument, 0, 'k'},
    {"monitor", required_argument, 0, 'x'},
    {"kargs",   required_argument, 0, 'a'},
    {"newkcb",  no_argument,       0, 'n'},
    {"nomsg",   no_argument,       0, 'm'},
    {"help",    no_argument,       0, 'h'},
    {0, 0, 0, 0}
};

static void print_help(char* argv0)
{
    printf("Usage: %s [OPTIONS] <COMMAND> [<args>]\n", argv0);
    printf("Options:\n");
    printf("\t -d, --debug\n");
    printf("\t\t Print debug information\n");
    printf("\t -k, --kernel\n");
    printf("\t\t Overwrite default kernel binary\n");
    printf("\t -x, --monitor\n");
    printf("\t\t Overwrite default monitor binary\n");
    printf("\t -a, --kargs\n");
    printf("\t\t Overwrite default kernel cmd arguments\n");
    printf("\t -n, --newkcb\n");
    printf("\t\t Create a new KCB even if there is already one for that core\n");
    printf("\t -m, --nomsg\n");
    printf("\t\t Don't wait for monitor message at the end\n");
    printf("\n");
    printf("Commands:\n");
    for (size_t i = 0; commands[i].name != NULL; i++) {
        printf("\t %s\t\t%s\n", commands[i].name, commands[i].desc);
    }
}

static void print_cmd_help(char* cmd)
{
    size_t i = 0;
    for (; commands[i].name != NULL; i++) {
        if (strcmp(cmd, commands[i].name) == 0) {
            break;
        }
    }

    printf("%s - %s\n", commands[i].name, commands[i].desc);
    printf("%s\n", commands[i].help);
}

int main (int argc, char **argv)
{
    initialize();
    int ret = -1;

    DEBUG("x86boot start");
    
    // ENSURE_SEQUENTIAL
    errval_t err;
    char *lock;
    err = oct_lock("x86boot.lock", &lock);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "can lock x86boot.");
    }
    //

    DEBUG("x86boot got lock");
    // Parse arguments, call handler function
    int c;
    while (1) {
        /* getopt_long stores the option index here. */
        int option_index = 0;

        c = getopt_long (argc, argv, "k:a:x:hnmd",
                         long_options, &option_index);
        if (c == -1) {
            break; // End of the options
        }

        switch (c) {
        case 0:
            // Long options handled by their short handles
            break;

        case 'k':
            cmd_kernel_binary = optarg;
            break;

        case 'a':
            cmd_kernel_args = optarg;
            break;

        case 'x':
            cmd_monitor_binary = optarg;
            break;

        case 'm':
            nomsg_flag = true;
            break;

        case 'n':
            new_kcb_flag = true;
            break;

        case 'd':
            debug_flag = true;
            break;

        case '?':
        case 'h':
            print_help(argv[0]);
            goto out;
            break;
        default:
            abort();
            break;
        }
    }

    if (optind < argc) {
        for (; optind < argc; optind++) {
            for (size_t i = 0; commands[i].name != NULL; i++) {
                if (strcmp(argv[optind], commands[i].name) == 0) {
                    if (argc - optind < commands[i].argc) {
                        print_cmd_help(commands[i].name);
                        goto out;
                    }
                    else {
                        ret = commands[i].fn(argc-optind, argv+optind);
                        break;
                    }
                }
            }
        }
    }

    if (ret == -1) {
        print_help(argv[0]);
    }
    else if (!nomsg_flag) {
        DEBUG("%s:%s:%d: Wait for message.\n",
              __FILE__, __FUNCTION__, __LINE__);
        while(!done) {
            errval_t err = event_dispatch(get_default_waitset());
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "error in event_dispatch");
            }
        }
    }

out:
    // END ENSURE SEQUENTIAL
    err = oct_unlock(lock);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "can not unlock x86boot.");
    }
    // 

    DEBUG("x86boot is done.");
    return ret;
}
