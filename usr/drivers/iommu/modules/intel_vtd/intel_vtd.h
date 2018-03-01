/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INTEL_VTD_H_
#define INTEL_VTD_H_ 1

#include <dev/vtd_dev.h>
#include <dev/vtd_iotlb_dev.h>

#include "intel_vtd_debug.h"

struct vtd;


extern vregion_flags_t vtd_table_map_attrs;


typedef uint16_t vtd_domid_t;


#define VTD_NUM_ROOT_ENTRIES	256

typedef enum {
    VTD_ENTRY_TYPE_BASE,
    VTD_ENTRY_TYPE_EXTENDED,
} vtd_entry_type_t;

struct vtd_root_table {
    struct capref   rtcap;
    struct capref   mappingcncap;
    struct cnoderef mappigncn;
};


struct vtd_ctxt_table {
    struct capref   ctcap;
    struct capref   mappingcncap;
    struct cnoderef mappigncn;
    uint8_t         rt_idx;
};

struct vtd_domain {
    vtd_domid_t                id;
    struct capref              ptroot;
    lpaddr_t                   ptroot_base;
    struct vtd_domain_mapping *devmappings;
};

struct vtd_domain_mapping {
    struct vtd_domain_mapping *next;
    struct vtd_domain_mapping *prev;
    struct vtd                *vtd;
    struct vtd_domain         *domain;
    struct capref              mappingcap;

    uint8_t             bus;
    uint8_t             idx;
};

#define vtd_ctxt_id_to_dev(idx) (idx >> 3)
#define vtd_ctxt_id_to_fun(idx) (idx & 0x7)
#define vtd_dev_fun_to_ctxt_id(dev, fun) ((dev << 3) | fun)


typedef enum {
    VTD_VERSION_INVALID = 0,
    VTD_VERSION_1_0     = 1,
    VTD_VERSION_MAX     = 2,
} vtd_version_t;

struct vtd {
    struct {
        vtd_t               vtd;
        vtd_iotlb_t         iotlb;
    } registers;

    vtd_version_t           version;


    uint16_t                pci_segment;
    vtd_entry_type_t        entry_type;

    /* The root table */
    struct vtd_root_table    root_table;

    /* the context descriptor tables */
    struct vtd_ctxt_table    ctxt_tables[VTD_NUM_ROOT_ENTRIES];

    struct vtd_domain        **domains;
    uint16_t                 domains_max;

    uint8_t                  address_width_max;

    struct {
        bool                     device_tlb;
        bool                     queued_invalidation;
        bool                     tlb_page_invalidation;
        bool                     interrupt_remapping;
        bool                     interrupt_extended;
        bool                     interrupt_posting;
        bool                     page_walk_coherency;
        bool                     snoop_control;
        bool                     pass_through;
        bool                     extended_context;
        bool                     nested_translation;
        bool                     memory_types;
        bool                     pasid;
        bool                     page_requests;
        bool                     page_requiest_drain;
        bool                     execute_requests;
        bool                     supervisor_requests;
        bool                     nowrite_flag;
        bool                     extended_access_flag;
        bool                     adv_fault_logging;
        bool                     prot_mem_hi;
        bool                     prot_mem_lo;
        bool                     huge_pages;
        bool                     write_draining;
        bool                     read_draining;
        bool                     req_wb_flush;
        bool                     paging_3_level;
        bool                     paging_4_level;
        bool                     paging_5_level;
        bool                     cachingmode;
    } capabilities;

};


errval_t vtd_create(struct vtd *v, struct capref regs, uint16_t segment,
                    nodeid_t proximity);
errval_t vtd_destroy(struct vtd *v);
errval_t vtd_set_root_table(struct vtd *vtd);

static inline bool vtd_device_tlb_present(struct vtd *v)
{
    return v->capabilities.device_tlb;
}

static inline struct vtd *vtd_get_for_device(uint8_t bus, uint8_t dev,
                                            uint8_t fun)
{
    return NULL;
}

static inline struct vtd_ctxt_table *vtd_get_ctxt_table(struct vtd *vtd, uint8_t idx)
{
    return &vtd->ctxt_tables[idx];
}

/* root table */
errval_t vtd_root_table_create(struct vtd_root_table *rt, nodeid_t proximity);
errval_t vtd_root_table_destroy(struct vtd_root_table *rt);
errval_t vtd_root_table_map(struct vtd_root_table *rt, size_t idx,
                            struct vtd_ctxt_table *ctx);
errval_t vtd_root_table_map_all(struct vtd_root_table *rt,
                                struct vtd_ctxt_table *ctx);
errval_t vtd_root_table_unmap(struct vtd_root_table *rt, size_t idx);

/* context table */
errval_t vtd_ctxt_table_create(struct vtd_ctxt_table *ct, nodeid_t proximity);
errval_t vtd_ctxt_table_create_all(struct vtd_ctxt_table *ct, nodeid_t proximity);
errval_t vtd_ctxt_table_destroy(struct vtd_ctxt_table *ct);
errval_t vtd_ctxt_table_destroy_all(struct vtd_ctxt_table *ct);
errval_t vtd_ctxt_table_map(struct vtd_ctxt_table *ctxt,
                            struct vtd_domain *dom,
                            struct vtd_domain_mapping *mapping);
errval_t vtd_ctxt_table_unmap(struct vtd_domain_mapping *mapping);


errval_t vtd_domains_init(vtd_domid_t max_domains);
errval_t vtd_domains_create(struct vtd_domain **domain, struct capref rootpt);
errval_t vtd_domains_destroy(struct vtd_domain *domain);
errval_t vtd_domains_add_device(struct vtd_domain *d, uint8_t bus, uint8_t dev,
                                uint8_t fun);
errval_t vtd_domains_remove_device(struct vtd_domain *d, uint8_t bus, uint8_t dev,
                                   uint8_t fun);

static inline vtd_domid_t vtd_domains_get_id(struct vtd_domain *d)
{
    return d->id;
}


errval_t vtd_interrupt_remapping_init(struct vtd *vtd);

/* some proxies to allocate memory */

static inline errval_t iommu_allocate_memory_page(struct capref *frame,
                                                  size_t bytes, nodeid_t proximity)
{
    return frame_alloc(frame, bytes, NULL);
}

static inline errval_t iommu_allocate_ram(struct capref *frame,
                                          uint8_t bits, nodeid_t proximity)
{
    return ram_alloc(frame, bits);
}





#endif /// INTEL_VTD_H_