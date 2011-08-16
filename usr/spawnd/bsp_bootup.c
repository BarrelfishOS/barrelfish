/**
 * \file
 * \brief Bootstrap logic for spawn daemon.
 *
 * \bug The monitor explicitly requires an indication
 * when it will not be asked to boot more cores.
 * So that it can setup its routing tables, etc.
 * To accomodate the bug, the domain waits for pci to finish iterating
 * the ACPI tables before informing the monitors of which cores to boot.
 */

/*
 * Copyright (c) 2010-2011, ETH Zurich. 
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/cpu_arch.h>

#include <spawndomain/spawndomain.h>

#include <vfs/vfs.h>
#include <skb/skb.h>

#include <dist/barrier.h>

#include <if/monitor_defs.h>


#include "internal.h"


#define MIN(a,b) ((a) < (b) ? (a) : (b))

struct mapping {
    int arch_id;
    bool present;
};

static struct mapping mappings[MAX_CPUS];
static int num_cores = 0;
static const char *kernel_cmdline;

static void state_machine(void);

static void boot_core_reply(struct monitor_binding *st, errval_t msgerr)
{
    if (err_is_fail(msgerr)) {
        DEBUG_ERR(msgerr, "Msg err in boot_core_reply");
        printf("msgerr in boot_core_reply, exiting\n");
        abort();
    }

    state_machine();
}

/**
 * \brief Msg sent by monitor when it has booted all requested cores
 * and initialized
 */
static void boot_initialize_reply(struct monitor_binding *st)
{
    state_machine();
}

static errval_t get_kernel_cmdline(const char *bootmods)
{
    // find something in the kernel string
    char *cmdline_pos = strstr(bootmods, "/sbin/cpu");
    if (cmdline_pos == NULL) {
        DEBUG_ERR(SPAWN_ERR_GET_CMDLINE_ARGS, "didn't find kernel command line");
        return SPAWN_ERR_GET_CMDLINE_ARGS;
    }

    // find the line containing the something
    const char *start = bootmods, *end = NULL;
    do {
        if (end != NULL) {
            start = end + 1;
        }
        end = strchr(start, '\n');
        if (end == NULL) {
            return SPAWN_ERR_GET_CMDLINE_ARGS;
        }
    } while (end < cmdline_pos);

    // copy it out
    char *buf = malloc(end - start + 1);
    if (buf == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }
    memcpy(buf, start, end - start);
    buf[end - start] = '\0';
    kernel_cmdline = buf;
    return SYS_ERR_OK;
}

static void state_machine(void)
{
    errval_t err;
    static int state_id = 0;
    struct monitor_binding *mb = get_monitor_binding();

    switch (state_id) {
    case 0: // Set reply handlers
        mb->rx_vtbl.boot_core_reply = boot_core_reply;
        mb->rx_vtbl.boot_initialize_reply = boot_initialize_reply;
        state_id++;

        // Fall through

    case 1: { // Boot all present cores
        static int mapping_index = 0;

        while (1) {
            if (mapping_index == MAX_CPUS) {
                break;
            }
            if (!mappings[mapping_index].present) {
                mapping_index++;
                continue;
            }
            if (mapping_index == my_core_id) {
                mapping_index++;
                continue;
            }
            break;
        }

        if (mapping_index != MAX_CPUS) {
            err = mb->tx_vtbl.boot_core_request(mb, NOP_CONT, mapping_index,
                                                mappings[mapping_index].arch_id,
                                                CURRENT_CPU_TYPE,
                                                kernel_cmdline);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "sending boot core request");
            }

            mapping_index++;
            break;
        } else { // Send boot_initialize_request message
            err = mb->tx_vtbl.boot_initialize_request(mb, NOP_CONT);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "sending boot initialize request");
            }

            state_id++;
            break;
        }
    }

    case 2: { // wait for all spawnd's to come up

        for (coreid_t c = 0; c < MAX_CPUS; c++) {
            if (mappings[c].present && c != my_core_id) {
                err = nsb_wait_n(c, SERVICE_BASENAME);
                if (err_is_fail(err)) {
                    USER_PANIC_ERR(err, "nameservice barrier wait for %s.%d",
                                   SERVICE_BASENAME, c);
                }                
            }
        }

        err = nsb_register(ALL_SPAWNDS_UP);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "nameservice barrier register for %s", 
                           ALL_SPAWNDS_UP);
        }                

        // offer the spawn service
        err = start_service();
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "starting spawnd service");
        }
        break;
    }

    default:
        assert(!"Should not get here");
        printf("Should not get here, exiting\n");
        abort();
    }
}

/* -------------------------- MAIN ------------------------------- */

static void set_skb_present(char *str)
{
    while (*str != '\0') {
        if (!isdigit(*str)) {
            str++;
            continue;
        }
        mappings[num_cores].arch_id = strtol(str, &str, 10);
        mappings[num_cores].present = true;
        num_cores++;
    }
}

static void set_cmdline_present(const char *str)
{
    num_cores = 1; // include self

    char *p = strchr(str, '=');
    assert(p != NULL);
    p++;
    while(*p != '\0') {
        int id_from = strtol(p, (char **)&p, 10), id_to = id_from;
        if(*p == '-') {
            p++;
            id_to = strtol(p, (char **)&p, 10);
        }
        assert(*p == ',' || *p == '\0');
        if(*p != '\0') {
            p++;
        }
        for(int i = id_from; i <= id_to; i++) {
            if (CURRENT_CPU_TYPE == CPU_BEEHIVE) {
                mappings[i].arch_id = i;
                mappings[i].present = true;
                num_cores++;
            } else {
                mappings[num_cores].arch_id = i;
                mappings[num_cores].present = true;
                num_cores++;
            }
        }
    }
}

void bsp_bootup(const char *bootmodules, int argc, const char *argv[])
{
    errval_t err;

    // Find kernel cmdline
    err = get_kernel_cmdline(bootmodules);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "invalid kernel cmdline");
    }

    bool use_skb = true;
    for (int i = 1; i < argc; i++) {
        if(!strncmp(argv[i],"bootapic-x86_64=",strlen("bootapic-x86_64="))) {
            use_skb = false;
            set_cmdline_present(argv[i]);
            break; // XXX: We do not yet support heterogeneous systems so only one cmdline arg is expected
        }
        if(!strncmp(argv[i],"bootapic-x86_32=",strlen("bootapic-x86_32="))) {
            use_skb = false;
            set_cmdline_present(argv[i]);
            break; // XXX: We do not yet support heterogeneous systems so only one cmdline arg is expected
        }
        if(!strncmp(argv[i],"bootscc=",strlen("bootscc="))) {
            use_skb = false;
            set_cmdline_present(argv[i]);
            break; // XXX: We do not yet support heterogeneous systems so only one cmdline arg is expected
        }
        if(!strncmp(argv[i],"bootbees=",strlen("bootbees="))) {
            use_skb = false;
            set_cmdline_present(argv[i]);
            break; // XXX: We do not yet support heterogeneous systems so only one cmdline arg is expected
        }
        if(!strcmp(argv[i],"bootarm")) {
            use_skb = false;
            break; // XXX: We do not yet support heterogeneous systems so only one cmdline arg is expected
        }
        if(!strcmp(argv[i],"boot")) {
            continue;
        }
        debug_printf("Invalid arg (%s) or architecture not supported.\n",
               argv[i]);
        abort();
    }

    if (use_skb) {
        err = skb_client_connect();
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "skb_client_connect failed");
        }

        // debug_printf("Waiting for pci to finish\n");
        /* Wait for pci to finish ACPI enumeration.
           This uses the nameserver as a lock server.
           When pci is done enumeration, it will add this to the server.
        */
        iref_t iref;
        err = nameservice_blocking_lookup("pci_discovery_done", &iref);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "nameservice_blocking_lookup failed");
        }

        /* Get the list of APIC IDs */
        char *result, *str_err;
        int int_err;
        err = skb_evaluate("get_apic_id_list(L),write(L).",
                           &result, &str_err, &int_err);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "skb_evaluate failed");
        }
        set_skb_present(result);
        free(result);
        free(str_err);

        /* Add the mapping to the skb */
        for (int i = 0; i < MAX_CPUS; i++) {
            if (!mappings[i].present) {
                continue;
            }

            err = skb_add_fact("corename(%d, x86_64, apic(%d)).",
                               i, mappings[i].arch_id);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "failed to add core mapping to skb");
            }
        }

#ifdef BARRELFISH_MULTIHOP_CHAN_H
        // wait until routing tables are set up (for multi-hop routing)
        err = nameservice_blocking_lookup("rts_done", &iref);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "nameservice_blocking_lookup failed");
        }
#endif // BARRELFISH_MULTIHOP_CHAN_H

    }

    for (int i = 0; i < MAX_CPUS; i++) {
        if (mappings[i].present) {
            debug_printf("coreid %d is arch id %d\n", i, mappings[i].arch_id);
        }
    }

    state_machine();
}
