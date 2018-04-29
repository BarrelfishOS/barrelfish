/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH8092 Zurich.
 * Attn: Systems Group.
 */

#ifndef DRIVERKIT_HWMODEL_H
#define DRIVERKIT_HWMODEL_H 1

#include <barrelfish/types.h>
#include <errors/errno.h>

struct dmem;

errval_t driverkit_hwmodel_ram_alloc(struct capref *dst,
                                     size_t bytes, int32_t dstnode,
                                     int32_t *nodes);

errval_t driverkit_hwmodel_frame_alloc(struct capref *dst,
                                       size_t bytes, int32_t dstnode,
                                       int32_t *nodes);

errval_t driverkit_hwmodel_get_map_conf(struct capref dst,
                                        int32_t nodeid,
                                        char *ret_conf_buf, size_t ret_conf_size,
                                        genpaddr_t *ret_addr);



errval_t driverkit_hwmodel_vspace_map(int32_t nodeid, struct capref frame,
                                      vregion_flags_t flags, struct dmem *dmem);

errval_t driverkit_hwmodel_vspace_map_fixed(int32_t nodeid,
                                            genvaddr_t addr,
                                            struct capref frame,
                                            vregion_flags_t flags,
                                            struct dmem *dmem);



/**
 * Allocate space to map frame in address space nodeid.
 */
errval_t driverkit_hwmodel_vspace_alloc(struct capref frame,
                                        int32_t nodeid, genvaddr_t *addr);
errval_t driverkit_hwmode_vspace_get_vaddr(struct capref frame,
                                           int32_t nodeid, genvaddr_t *addr,
                                           bool *already_mapped);

int32_t driverkit_hwmodel_get_my_node_id(void);
int32_t driverkit_hwmodel_lookup_dram_node_id(void);
int32_t driverkit_hwmodel_lookup_pcibus_node_id(void);
int32_t driverkit_hwmodel_lookup_node_id(const char *path);

errval_t driverkit_hwmodel_reverse_resolve(struct capref dst,
                                     int32_t nodeid,
                                     genpaddr_t *ret_addr);




/**
 * SKB interface helpers
 */
struct hwmodel_name {
    int32_t nodeid;
    uint64_t address;
};

// Parse the output generated by write_regions in decoding_net4.pl
void driverkit_parse_namelist(char *in, struct hwmodel_name *names, int *conversions);


// Disable using the model for allocation
//#define DISABLE_MODEL

#define HWMODEL_QUERY_DEBUG_ENABLED 1
#if defined(HWMODEL_QUERY_DEBUG_ENABLED)
#define HWMODEL_QUERY_DEBUG(x...) do { printf("MODELQUERY: " x); printf("\n"); } while(0)

#else
#define HWMODEL_QUERY_DEBUG(x...) ((void)0)
#endif




#endif // DRIVERKIT_HWMODEL_H
