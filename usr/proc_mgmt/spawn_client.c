/**
 * \file
 * \brief Spawn client for the process management service.
 */

/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "spawn_client.h"
#include "spawnd_state.h"

/**
 * \brief Request the spawn daemon on a specific core to spawn a program
 *
 * \param coreid          Core ID on which to spawn the program
 * \param path            Absolute path in the file system to an executable
 *                        image suitable for the given core
 * \param argv            Command-line arguments, NULL-terminated
 * \param envp            Optional environment, NULL-terminated
 *                        (pass NULL to inherit)
 * \param inheritcn_cap   Cap to a CNode containing capabilities to be inherited
 * \param argcn_cap       Cap to a CNode containing capabilities passed as
 *                        arguments
 * \param flags           Flags to spawn
 * \param ret_domain_cap  If non-NULL, filled in with domain cap of new domain
 *
 * \bug flags are currently ignored
 */
errval_t spawn_with_caps(coreid_t core_id, const char *path,
                         const char *argvbuf, size_t argvbytes,
                         const char *envbuf, size_t envbytes,
                         struct capref inheritcn_cap, struct capref argcn_cap,
                         uint8_t flags, struct capref *ret_domain_cap)
{
    errval_t err, msgerr;

    if (!spawnd_state_exists(core_id)) {
        USER_PANIC("not connected to spawnd on the requested core");
    }
    struct spawnd_state *state = spawnd_state_get(core_id);
    assert(state != NULL);
    struct spawn_binding *cl = state->b;
    assert(cl != NULL);

    struct capref domain_cap;
    err = slot_alloc(&domain_cap);
    if (err_is_fail(err)) {
    	USER_PANIC_ERR(err, "slot_alloc domain_cap");
    }
    err = cap_retype(domain_cap, cap_procmng, 0, ObjType_Domain, 0, 1);
	if (err_is_fail(err)) {
	    USER_PANIC_ERR(err, "cap_retype domain_cap from cap_procmng");
	}

    if (capref_is_null(inheritcn_cap) && capref_is_null(argcn_cap)) {
        err = cl->rpc_tx_vtbl.spawn_proc_mgmt_domain(cl, domain_cap,
        		path, argvbuf, argvbytes, envbuf, envbytes, flags, &msgerr);
    } else {
        err = cl->rpc_tx_vtbl.spawn_proc_mgmt_domain_with_caps(cl, domain_cap,
        		path, argvbuf, argvbytes, envbuf, envbytes, inheritcn_cap,
        		argcn_cap, flags, &msgerr);
    }
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error sending spawn request");
    } else if (err_is_fail(msgerr)) {
        goto out;
    }

    if (ret_domain_cap != NULL) {
        *ret_domain_cap = domain_cap;
    }

out:
    return msgerr;
}

/**
 * \brief Request the spawn daemon on a specific core to spawn a program
 *
 * \param coreid          Core ID on which to spawn the program
 * \param path            Absolute path in the file system to an executable
 *                        image suitable for the given core
 * \param argv            Command-line arguments, NULL-terminated
 * \param envp            Optional environment, NULL-terminated
 *                        (pass NULL to inherit)
 * \param inheritcn_cap   Cap to a CNode containing capabilities to be inherited
 * \param argcn_cap       Cap to a CNode containing capabilities passed as
 *                        arguments
 * \param flags           Flags to spawn
 * \param ret_domain_cap  If non-NULL, filled in with domain cap of new domain
 *
 * \bug flags are currently ignored
 */
errval_t spawn(coreid_t core_id, const char *path, const char *argvbuf,
               size_t argvbytes, const char *envbuf, size_t envbytes,
               uint8_t flags, struct capref *ret_domain_cap)
{
    return spawn_with_caps(core_id, path, argvbuf, argvbytes, envbuf, envbytes,
                           NULL_CAP, NULL_CAP, flags, ret_domain_cap);
}

/**
 * \brief Request the spawn daemon on a specific core to span an existing domain
 *
 * \param domain_cap Identifying capability for the domain to span
 * \param core_id    Core ID on which to span
 * \param vroot      Vspace root for the dispatcher to span
 * \param dispframe  Frame for the dispatcher to span
 */
errval_t span(struct capref domain_cap, coreid_t core_id, struct capref vroot,
              struct capref dispframe)
{
    errval_t err, msgerr;

    if (!spawnd_state_exists(core_id)) {
        USER_PANIC("not connected to spawnd on the requested core");
    }
    struct spawnd_state *state = spawnd_state_get(core_id);
    assert(state != NULL);
    struct spawn_binding *cl = state->b;
    assert(cl != NULL);

    err = cl->rpc_tx_vtbl.span(cl, domain_cap, vroot, dispframe, &msgerr);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error sending span request");
    }

    return msgerr;
}
