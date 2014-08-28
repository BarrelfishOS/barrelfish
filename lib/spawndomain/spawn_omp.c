/**
 * \file
 * \brief functionality to spawn domains
 */

/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <if/octopus_defs.h>
#include <if/octopus_rpcclient_defs.h>
#include <if/monitor_defs.h>
#include <octopus/getset.h> // for oct_read TODO
#include <octopus/trigger.h> // for NOP_TRIGGER


#include "spawn.h"

errval_t spawn_symval_lookup(const char *binary, uint32_t idx,
                             char **ret_name, genvaddr_t *ret_addr)
{
    errval_t err;

    size_t len;

    if (binary[0]== '_') {
        binary++;
    }

    len = snprintf(NULL, 0, "%s.omp.%u", binary, idx);
    char *omp_entry = malloc(len+1);
    if (omp_entry == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }
    snprintf(omp_entry, len+1, "%s.omp.%u", binary, idx);

    struct octopus_rpc_client *r = get_octopus_rpc_client();
    if (r == NULL) {
        return LIB_ERR_NAMESERVICE_NOT_BOUND;
    }

    // transform to lower case
    for (int i = 0; i < len; ++i) {
        if (omp_entry[i] >= 'A' && omp_entry[i] <= 'Z') {
            omp_entry[i] -= ('A'-'a');
        }
    }

    char* record = NULL;
    octopus_trigger_id_t tid;
    errval_t error_code;
    err = r->vtbl.get(r, omp_entry, NOP_TRIGGER, &record, &tid, &error_code);
    if (err_is_fail(err)) {
        goto out;
    }
    err = error_code;
    if (err_is_fail(err)) {
        if (err_no(err) == OCT_ERR_NO_RECORD) {
            err = err_push(err, LIB_ERR_NAMESERVICE_UNKNOWN_NAME);
        }
        goto out;
    }

    uint64_t addr = 0;
    char *symname = NULL;
    err = oct_read(record, "_ { sym: %s, addr: %d }", &symname, &addr);
    if (err_is_fail(err) || symname == NULL) {
        err = err_push(err, LIB_ERR_NAMESERVICE_INVALID_NAME);
        goto out;
    }
    if (ret_addr != NULL) {
        *ret_addr = addr;
    }
    if (ret_name != NULL) {
        *ret_name = strdup(symname);
    }

out:
    free(record);
    free(omp_entry);
    return err;
}

errval_t spawn_symval_register(const char *binary, uint32_t idx,
                               const char *symname, genvaddr_t address)
{

    errval_t err = SYS_ERR_OK;

    struct octopus_rpc_client *r = get_octopus_rpc_client();
    if (r == NULL) {
        return LIB_ERR_NAMESERVICE_NOT_BOUND;
    }

    if (symname[0] == '_') {
        symname++;
    }
    // Format record
    static const char* format = "%s.omp.%u { sym: %s, addr: %d }";
    size_t len = snprintf(NULL, 0, format, binary, idx, symname, address);
    char* record = malloc(len+1);
    if (record == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }
    snprintf(record, len+1, format, binary, idx, symname, address);
    // transform to lower case
    for (int i = 0; i < len; ++i) {
        if (record[i] >= 'A' && record[i] <= 'Z') {
            record[i] -= ('A'-'a');
        }
    }

    char* ret = NULL;
    octopus_trigger_id_t tid;
    errval_t error_code;
    err = r->vtbl.set(r, record, 0, NOP_TRIGGER, 0, &ret, &tid, &error_code);
    if (err_is_fail(err)) {
        goto out;
    }
    err = error_code;

out:
    free(record);
    return err;
}

