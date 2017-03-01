/**
 * \file
 * \brief ACPI RPC Client
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/caddr.h>
#include <barrelfish/nameservice_client.h>

#include <if/acpi_defs.h>
#include <if/acpi_defs.h>

#include <acpi_client/acpi_client.h>

static struct acpi_connection {
    bool is_done;
    errval_t err;
} state;

static struct acpi_binding* binding;

errval_t acpi_client_get_device_handle(const char *dev_id,
                                       acpi_device_handle_t *ret_handle)
{
    assert(binding != NULL);
    errval_t err, msgerr;
    err = binding->rpc_tx_vtbl.get_handle(binding, dev_id, ret_handle, &msgerr);
    return err_is_fail(err) ? err : msgerr;
}

errval_t acpi_client_eval_integer(acpi_device_handle_t handle,
                                  const char *path, uint64_t *data)
{
    assert(binding != NULL);
    errval_t err, msgerr;
    err = binding->rpc_tx_vtbl.eval_integer(binding, handle, path, data, &msgerr);
    return err_is_fail(err) ? err : msgerr;
}


errval_t acpi_reset(void)
{
    assert(binding != NULL);
    errval_t err, msgerr;
    err = binding->rpc_tx_vtbl.reset(binding, &msgerr);
    return err_is_fail(err) ? err : msgerr;
}

errval_t acpi_sleep(int st)
{
    assert(binding != NULL);
    errval_t err, msgerr;
    err = binding->rpc_tx_vtbl.sleep(binding, st, &msgerr);
    return err_is_fail(err) ? err : msgerr;
}

// Kludge for VBE driver
errval_t acpi_get_vbe_bios_cap(struct capref *retcap, size_t *retsize)
{
    assert(binding != NULL);
    errval_t err, msgerr;
    assert(retcap != NULL);
    assert(retsize != NULL);
    uint32_t s;
    err = slot_alloc(retcap);
    if (err_is_fail(err)) {
        return err;
    }
    err = binding->rpc_tx_vtbl.get_vbe_bios_cap(binding, &msgerr, retcap, &s);
    *retsize = s;
    return err_is_fail(err) ? err : msgerr;
}

errval_t vtd_create_domain(struct capref pml4)
{
    assert(binding != NULL);
    errval_t err, msgerr;
    err = binding->rpc_tx_vtbl.create_domain(binding, pml4, &msgerr);
    return err_is_fail(err) ? err : msgerr;
}

errval_t vtd_delete_domain(struct capref pml4)
{
    assert(binding != NULL);
    errval_t err, msgerr;
    err = binding->rpc_tx_vtbl.delete_domain(binding, pml4, &msgerr);
    return err_is_fail(err) ? err : msgerr;
}

errval_t vtd_domain_add_device(int seg, int bus, int dev, int func, struct capref pml4)
{
    assert(binding != NULL);
    errval_t err, msgerr;
    err = binding->rpc_tx_vtbl.vtd_add_device(binding, seg, bus, dev, func, pml4, &msgerr);
    return err_is_fail(err) ? err : msgerr;
}

errval_t vtd_domain_remove_device(int seg, int bus, int dev, int func, struct capref pml4)
{
    assert(binding != NULL);
    errval_t err, msgerr;
    err = binding->rpc_tx_vtbl.vtd_remove_device(binding, seg, bus, dev, func, pml4, &msgerr);
    return err_is_fail(err) ? err : msgerr;
}

errval_t vtd_add_devices(void)
{
    assert(binding != NULL);
    errval_t err, msgerr;
    err = binding->rpc_tx_vtbl.vtd_id_dom_add_devices(binding, &msgerr);
    return err_is_fail(err) ? err : msgerr;
}

struct acpi_binding* get_acpi_binding(void)
{
    assert(binding != NULL);
    return binding;
}

static void rpc_bind_cb(void *st, errval_t err, struct acpi_binding* b)
{
    if (err_is_ok(err)) {
        binding = b;
        acpi_rpc_client_init(binding);
    } // else: Do nothing

    assert(!state.is_done);
    state.is_done = true;
    state.err = err;
}

errval_t connect_to_acpi(void)
{
    errval_t err;
    iref_t iref;

    err = nameservice_blocking_lookup("acpi", &iref);
    if (err_is_fail(err)) {
        return err;
    }

    state.is_done = false;
    err = acpi_bind(iref, rpc_bind_cb, NULL, get_default_waitset(),
            IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err_push(err, FLOUNDER_ERR_BIND);
    }

    //  Wait for callback to complete
    while (!state.is_done) {
        messages_wait_and_handle_next();
    }

    return state.err;

}
