/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <barrelfish/ump_chan.h>

#include <xeon_phi/xeon_phi.h>
#include <xeon_phi/xeon_phi_domain.h>

#define EXPECT_SUCCESS(err, msg) if(err_is_fail(err)) {USER_PANIC_ERR(err, msg); }
#define EXPECT_EQUAL(res, exp) if ((res) != (exp)) {USER_PANIC("results dont match: expected: %lx, was: %lx", (exp), (res));}

static void do_test(xphi_dom_id_t expected_result, coreid_t core)
{
    errval_t err;
    xphi_dom_id_t result;

#ifdef __k1om__
    char *iface = xeon_phi_domain_build_iface("ns_test_iface", disp_xeon_phi_id(),
                                              core);
#else
    char *iface = xeon_phi_domain_build_iface("ns_test_iface", XEON_PHI_DOMAIN_HOST,
                                              core);
#endif

    debug_printf("Exact lookup: {%s}\n", iface);
    err = xeon_phi_domain_lookup(iface, &result);
    EXPECT_SUCCESS(err, "lookup exact");
    EXPECT_EQUAL(result, expected_result);
    free(iface);

#ifdef __k1om__
    iface = xeon_phi_domain_build_iface("ns_test_iface", disp_xeon_phi_id(),
                                              XEON_PHI_DOMAIN_DONT_CARE);
#else
    iface = xeon_phi_domain_build_iface("ns_test_iface", XEON_PHI_DOMAIN_HOST,
                                              XEON_PHI_DOMAIN_DONT_CARE);
#endif
    debug_printf("Core don't care lookup {%s}\n", iface);
    err = xeon_phi_domain_lookup(iface, &result);
    EXPECT_SUCCESS(err, "Core don't care lookup");
    EXPECT_EQUAL(result, expected_result);

    iface = xeon_phi_domain_build_iface("ns_test_iface", XEON_PHI_DOMAIN_DONT_CARE,
                                        core);
    debug_printf("Domain don't care lookup: {%s}\n", iface);
    err = xeon_phi_domain_lookup(iface, &result);
    EXPECT_SUCCESS(err, "lookup don't care domain");
    EXPECT_EQUAL(result, expected_result);
    free(iface);

    iface = xeon_phi_domain_build_iface("ns_test_iface", XEON_PHI_DOMAIN_DONT_CARE,
                                XEON_PHI_DOMAIN_DONT_CARE);
    debug_printf("All don't care lookup: {%s}\n", iface);
    err = xeon_phi_domain_lookup(iface, &result);
    EXPECT_SUCCESS(err, "All don't care lookup:");
    EXPECT_EQUAL(result, expected_result);
    free(iface);
}

int main(int argc,
         char **argv)
{
    errval_t err;

    debug_printf("Xeon Phi Name Service Test started.\n");

#ifdef __k1om__
    char *iface = xeon_phi_domain_build_iface("ns_test_iface", disp_xeon_phi_id(),
                                              disp_get_core_id());
#else
    char *iface = xeon_phi_domain_build_iface("ns_test_iface", XEON_PHI_DOMAIN_HOST,
                    disp_get_core_id());
#endif
    debug_printf("registering: {%s} with 0xcafebabe\n", iface);

    err = xeon_phi_domain_register(iface, 0xcafebabe);
    assert(err_is_ok(err));
    free(iface);

    do_test(0xcafebabe, disp_get_core_id());

#ifdef __k1om__
    iface = xeon_phi_domain_build_iface("ns_test_iface", disp_xeon_phi_id(),
                                              disp_get_core_id()+1);
#else
    iface = xeon_phi_domain_build_iface("ns_test_iface", XEON_PHI_DOMAIN_HOST,
                                              disp_get_core_id()+1);
#endif
    debug_printf("registering: {%s} with 0xdeadbeef\n", iface);
    err = xeon_phi_domain_register(iface, 0xdeadbeef);
    assert(err_is_ok(err));
    free(iface);

    debug_printf("XXX: domain and all dont't care are expected to fail\n");
    do_test(0xdeadbeef, disp_get_core_id()+1);
}

