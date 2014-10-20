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
#include <barrelfish/dispatch.h> // for disp_name()
#include <spawndomain/spawndomain.h>

#include <if/octopus_defs.h>
#include <if/octopus_rpcclient_defs.h>
#include <if/monitor_defs.h>
#include <octopus/getset.h> // for oct_read TODO
#include <octopus/trigger.h> // for NOP_TRIGGER


#include "spawn.h"

struct symval {
    char *name;
    genvaddr_t addr;
};

uint32_t symval_count;

struct symval *symvals;

errval_t spawn_symval_count(uint32_t *ret_count)
{
    if (!symvals) {
        errval_t err;

        err = spawn_symval_cache_init(1);
        if (err_is_fail(err)) {
            return err;
        }
    }

    if (ret_count) {
        *ret_count = symval_count;
    }
    return SYS_ERR_OK;
}

errval_t spawn_symval_cache_init(uint8_t lazy)
{
    errval_t err;

    genvaddr_t count = 0;
    err = spawn_symval_lookup_idx(0, NULL, &count);
    if (err_is_fail(err)) {
        return err;
    }

    symval_count = count;

    if (count > 0) {
        symvals = calloc(count + 1, sizeof(struct symval));
        if (symvals == NULL) {
            return LIB_ERR_MALLOC_FAIL;
        }
    }

    if (lazy) {
        return SYS_ERR_OK;
    }

    for (uint32_t i = 1; i <= count; ++i) {
        err = spawn_symval_lookup_idx(i, &symvals[i].name, &symvals[i].addr);
        if (err_is_fail(err)) {
            return err;
        }
    }

    return SYS_ERR_OK;
}

errval_t spawn_symval_lookup_name(char *name, uint32_t *ret_idx,  genvaddr_t *ret_addr)
{
    errval_t err;

    if (!symvals) {
        err = spawn_symval_cache_init(1);
        if (err_is_fail(err)) {
            return err;
        }
    }

    for (uint32_t i = 1; i <= symval_count; ++i) {
        if (symvals[i].addr == 0) {
            err = spawn_symval_lookup_idx(i, &symvals[i].name, &symvals[i].addr);
            if (err_is_fail(err)) {
                return err;
            }
        }
        if (strcmp(name, symvals[i].name) == 0) {
            if (ret_idx) {
                *ret_idx = i;
            }
            if (ret_addr) {
                *ret_addr = symvals[i].addr;
            }
            return SYS_ERR_OK;
        }
    }
    // todo: errval
    return -1;
}

errval_t spawn_symval_lookup_addr(genvaddr_t addr, uint32_t *ret_idx, char **ret_name)
{
    errval_t err;

    if (!symvals) {
        err = spawn_symval_cache_init(1);
        if (err_is_fail(err)) {
            return err;
        }
    }

    for (uint32_t i = 1; i <= symval_count; ++i) {
        if (symvals[i].addr == 0) {
            err = spawn_symval_lookup_idx(i, &symvals[i].name, &symvals[i].addr);
            if (err_is_fail(err)) {
                return err;
            }
        }
        if (symvals[i].addr == addr) {
            if (ret_idx) {
                *ret_idx = i;
            }
            if (ret_name) {
                *ret_name = symvals[i].name;
            }

            return SYS_ERR_OK;
        }
    }

    // TODO: errval
    return -1;
}

errval_t spawn_symval_lookup_idx(uint32_t idx, char **ret_name, genvaddr_t *ret_addr)
{
    if (symvals) {
        if (symvals[idx].addr != 0) {
            if (ret_name) {
                *ret_name = symvals[idx].name;
            }
            if (ret_addr) {
                *ret_addr = symvals[idx].addr;
            }
        }
    }

    return spawn_symval_lookup(disp_name(), idx, ret_name, ret_addr);
}

errval_t spawn_symval_lookup(const char *binary, uint32_t idx, char **ret_name,
                             genvaddr_t *ret_addr)
{
    errval_t err;

    size_t len;

    len = snprintf(NULL, 0, "%s.omp.%"PRIu32, binary, idx);
    char *omp_entry = malloc(len+1);
    if (omp_entry == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }
    snprintf(omp_entry, len+1, "%s.omp.%"PRIu32, binary, idx);

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

