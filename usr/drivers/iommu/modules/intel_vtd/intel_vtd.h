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

#include "intel_vtd_debug.h"

struct vtd;


extern vregion_flags_t vtd_table_map_attrs;


typedef uint16_t vtd_domid_t;


/**
 * @brief the number of entries in the root table. this corresponds to the
 *        maximum possible number of PCI busses which is an 8-bit number,
 *        hence 256 entries
 */
#define VTD_NUM_ROOT_ENTRIES	256

/**
 * @brief the entry format of the root and context tables.
 *
 * There exist two different entry types. a base version and the extended
 * representation
 */
typedef enum {
    VTD_ENTRY_TYPE_BASE,
    VTD_ENTRY_TYPE_EXTENDED,
} vtd_entry_type_t;


/**
 * @brief structure to hold the software representation of the VT-d root table
 */
struct vtd_root_table {
    ///< capability of the root table
    struct capref   rtcap;

    ///< capability of the mapping cap CNODE
    struct capref   mappingcncap;

    ////< cnode reference of the mapping cap
    struct cnoderef mappigncn;

    ///< the VT-d unit this root tabel belongs
    struct vtd  *vtd;
    /*
     * TODO: currently we use one root table per VT-d unit. However, we might
     * share a single root table between multiple VT-d units, if applicable
     */
};

/**
 * @brief structure represent a VT-d context table
 *
 * each context table has one root table.
 */
struct vtd_ctxt_table {
    ///< capability of the context table
    struct capref           ctcap;

    ///< capability of the CNODE for this context table
    struct capref           mappingcncap;

    ///< cnode for the mapping caps
    struct cnoderef         mappigncn;

    ///< pointer to the parent root table.
    struct vtd_root_table  *root_table;

    ///< index of the context table in the root tbale
    uint8_t                 root_table_idx;
};

/**
 * @brief represents a device context in the VT-d
 */
struct vtd_device {
    ///< pci bus
    uint8_t                 bus;

    ///< pci device
    uint8_t                 device;

    ///< pci function
    uint8_t                 function;

    ///< the mapping cap for this device
    struct capref           mappingcap;

    ///< the context table this device is mapped in
    struct vtd_ctxt_table  *ctxt_table;

    ///< the protection domain of the device
    struct vtd_domain      *domain;
};

/**
 * @brief represents a protection domain of the VT-d unit
 */
struct vtd_domain {
    ///< the domain id
    vtd_domid_t                 id;

    ///< the root page table of the domain
    struct capref               ptroot;

    ///< the physical base address of the root page table
    lpaddr_t                    ptroot_base;

    ///< list of mappings
    struct vtd_domain_mapping  *devmappings;
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
    vtd_t                   vtd_dev;

    vtd_version_t           version;
    uint8_t                 flags;

    uint16_t                pci_segment;
    vtd_entry_type_t        entry_type;

    /* The root table */
    struct vtd_root_table    root_table;

    /* the context descriptor tables */
    struct vtd_ctxt_table    ctxt_tables[VTD_NUM_ROOT_ENTRIES];

    nodeid_t proximity_domain;

    struct vtd_domain        **domains;
    uint16_t                 max_domains;

    uint8_t                  max_guest_address_width;

    uint8_t                  max_page_size_bits;
    uint8_t                  max_fault_recording_regs;
    struct {

        bool                     device_tlb;
        bool                     queued_invalidation;
        bool                     tlb_page_invalidation;
        bool                     interrupt_remapping;
        bool                     extended_interrupt_mode;
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
        bool                     write_draining;
        bool                     read_draining;
        bool                     req_wb_flush;
        bool                     paging_3_level;
        bool                     paging_4_level;
        bool                     paging_5_level;
        bool                     cachingmode;
    } capabilities;

};


errval_t vtd_create(struct vtd *v, struct capref regs);
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
errval_t vtd_root_table_create(struct vtd_root_table *rt, struct vtd *vtd);
errval_t vtd_root_table_destroy(struct vtd_root_table *rt);




errval_t vtd_root_table_map(struct vtd_root_table *rt, uint8_t idx,
                            struct vtd_ctxt_table *ctx);
errval_t vtd_root_table_unmap(struct vtd_root_table *rt, size_t idx);



/* context table */
errval_t vtd_ctxt_table_create(struct vtd_ctxt_table *ct, struct vtd *vtd);
errval_t vtd_ctxt_table_destroy(struct vtd_ctxt_table *ct);
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






#endif /// INTEL_VTD_H_