/*
 * Copyright (c) 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_SPAWN_CLIENT_H
#define BARRELFISH_SPAWN_CLIENT_H

#include <sys/cdefs.h>
#include <barrelfish_kpi/types.h>
#include <spawndomain/spawndomain.h>
__BEGIN_DECLS

struct spawn_ps_entry {
    uint8_t status;
};



/* Inherit CNode, layout convention #spawn_program_with_caps expects */
#define INHERITCN_SLOT_FDSPAGE   1  ///< cap for inherited file descriptors
#define INHERITCN_SLOT_SESSIONID 2  ///< Session ID domain belongs to
#define INHERITCN_SLOT_KERNELCAP 3     ///< Kernel capability for core boot

errval_t spawn_program_with_caps(coreid_t coreid, const char *path,
                                 char *const argv[], char *const envp[],
                                 struct capref inheritcn_cap,
                                 struct capref argcn_cap, spawn_flags_t flags,
                                 domainid_t *ret_domainid);
errval_t spawn_arrakis_program(coreid_t coreid, const char *path,
                                 char *const argv[], char *const envp[],
                                 struct capref inheritcn_cap,
                                 struct capref argcn_cap, spawn_flags_t flags,
                                 domainid_t *ret_domainid);
errval_t spawn_program(coreid_t coreid, const char *path,
                       char *const argv[], char *const envp[],
                       spawn_flags_t flags, domainid_t *ret_domainid);
errval_t spawn_program_on_all_cores(bool same_core, const char *path,
                                    char *const argv[], char *const envp[],
                                    spawn_flags_t flags, domainid_t *ret_domainid,
                                    coreid_t* spawn_count);
errval_t spawn_kill(domainid_t domainid);
errval_t spawn_exit(uint8_t exitcode);
errval_t spawn_wait_coreid(coreid_t coreid, domainid_t domainid, uint8_t *exitcode, bool nohang);
errval_t spawn_wait(domainid_t domainid, uint8_t *exitcode, bool nohang);
errval_t spawn_wait_core(coreid_t coreid, domainid_t domainid,
                         uint8_t *exitcode, bool nohang);
errval_t spawn_binding(coreid_t coreid, struct spawn_binding **ret_client);
errval_t spawn_get_domain_list(uint8_t **domains, size_t *len);
errval_t spawn_get_status(uint8_t domain, struct spawn_ps_entry *pse,
                          char **argbuf, size_t *arglen, errval_t *reterr);

errval_t alloc_inheritcn_with_caps(struct capref *inheritcn_capp,
                                   struct capref fdcap,
                                   struct capref sidcap,
                                   struct capref kernelcap);

errval_t spawn_dump_capabilities(domainid_t domainid);
__END_DECLS

#endif // BARRELFISH_SPAWN_CLIENT_H
