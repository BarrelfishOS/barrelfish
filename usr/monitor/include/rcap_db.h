/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef RCAP_DB_H
#define RCAP_DB_H

errval_t rcap_db_init (void);
errval_t rcap_db_add(struct capability * cap, bool has_desc);
bool rcap_db_exists(struct capability *cap);
errval_t rcap_db_get_info(struct capability *cap, bool * has_desc, 
                          coremask_t *on_cores);
errval_t rcap_db_update_on_recv (struct capability * cap, bool has_desc,
                                 coremask_t on_cores, coreid_t from_core);
errval_t rcap_db_acquire_lock(struct capability *cap, struct rcap_st * st);
errval_t rcap_db_remote_lock_req(struct capability *cap, coreid_t from_core, 
                                 recordid_t ccast_recordid);
errval_t rcap_db_release_lock(struct capability *cap, coremask_t to_cores);
errval_t rcap_db_remote_unlock(struct capability *cap, coreid_t from_core);
errval_t rcap_db_acquire_recursive_lock(struct capability *cap,
                                        struct rcap_st * st);
errval_t rcap_db_remote_recursive_lock_req(struct capability *cap,
                                           coreid_t from_core, 
                                           recordid_t ccast_recordid);
errval_t rcap_db_release_recursive_lock(struct capability *cap, 
                                        coremask_t to_cores);
errval_t rcap_db_remote_recursive_unlock(struct capability *cap, 
                                         coreid_t from_core);
errval_t rcap_db_remote_new_core(struct capability * cap, coreid_t send_core, 
                                 coreid_t recv_core);
errval_t rcap_db_remote_details_req(struct capability * cap, 
                                    coreid_t from_core);
errval_t rcap_db_remote_recv_details(struct capability * cap, 
                                     coreid_t from_core, bool has_desc);
errval_t rcap_db_delete (struct capability * cap);
errval_t rcap_db_remote_delete (struct capability * cap, coreid_t from_core);
errval_t rcap_db_retype(struct capability * cap, bool has_descendents);
errval_t rcap_db_remote_retype (struct capability * cap, bool has_descendents,
                                coreid_t from_core);
errval_t rcap_db_revoke(struct capability * cap);
errval_t rcap_db_remote_revoke (struct capability * cap, coreid_t from_core);

#endif
