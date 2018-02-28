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

#define INTEL_VTD_DEBUG(X...) debug_printf("[vtd] " X);

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



struct vtd {
    struct {
        vtd_t               vtd;
        vtd_iotlb_t         iotlb;
    } registers;

    uint16_t                pci_segment;
    vtd_entry_type_t        entry_type;

    /* The root table */
    struct vtd_root_table    root_table;

    /* the context descriptor tables */
    struct vtd_ctxt_table    ctxt_tables[VTD_NUM_ROOT_ENTRIES];

    struct vtd_domain        **domains;
    uint16_t                 domains_max;

    uint8_t                  address_width_max;

    bool                     device_tlb_present;
};


errval_t vtd_create(struct vtd *v, struct capref regs, uint16_t segment,
                    nodeid_t proximity);
errval_t vtd_destroy(struct vtd *v);
errval_t vtd_set_root_table(struct vtd *vtd);

static inline bool vtd_device_tlb_present(struct vtd *v)
{
    return v->device_tlb_present;
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


#define INTEL_VTD_COMMAND_TIMEOUT 0x1000

/* control bits */

static inline void vtd_cmd_translation_toggle(struct vtd *vtd, bool toggle)
{
    bool pending;
    uint32_t timeout = INTEL_VTD_COMMAND_TIMEOUT;
    vtd_GCMD_te_wrf(&vtd->registers.vtd, toggle);
    do {
        pending = vtd_GSTS_tes_rdf(&vtd->registers.vtd);
    } while((pending != toggle) && timeout--);
    assert(vtd_GSTS_tes_rdf(&vtd->registers.vtd) == toggle);
}

static inline void vtd_cmd_translation_disable(struct vtd *vtd)
{
    vtd_cmd_translation_toggle(vtd, 0);
}

static inline void vtd_cmd_translation_enable(struct vtd *vtd)
{
    vtd_cmd_translation_toggle(vtd, 1);
}

static inline void vtd_cmd_set_root_table_ptr(struct vtd *vtd)
{
    uint32_t pending;
    uint32_t timeout = INTEL_VTD_COMMAND_TIMEOUT;
    vtd_GCMD_srtp_wrf(&vtd->registers.vtd, 1);
    do {
        pending = vtd_GSTS_rtps_rdf(&vtd->registers.vtd);
    } while((pending == 0) && timeout--);
}

static inline void vtd_cmd_set_fault_log(struct vtd *vtd)
{
    uint32_t pending;
    uint32_t timeout = INTEL_VTD_COMMAND_TIMEOUT;
    vtd_GCMD_sfl_wrf(&vtd->registers.vtd, 1);
    do {
        pending = vtd_GSTS_fls_rdf(&vtd->registers.vtd);
    } while((pending == 0) && timeout--);
}

static inline void vtd_cmd_adv_fault_logging_toggle(struct vtd *vtd, bool toggle)
{
    uint32_t pending;
    uint32_t timeout = INTEL_VTD_COMMAND_TIMEOUT;
    vtd_GCMD_eafl_wrf(&vtd->registers.vtd, toggle);
    do {
        pending = vtd_GSTS_afls_rdf(&vtd->registers.vtd);
    } while((pending != toggle) && timeout--);
}

static inline void vtd_cmd_adv_fault_logging_enable(struct vtd *vtd)
{
    vtd_cmd_adv_fault_logging_toggle(vtd, 1);
}

static inline void vtd_cmd_adv_fault_logging_disable(struct vtd *vtd)
{
    vtd_cmd_adv_fault_logging_toggle(vtd, 0);
}

static inline void vtd_cmd_write_buffer_flush(struct vtd *vtd)
{
    uint32_t pending;
    uint32_t timeout = INTEL_VTD_COMMAND_TIMEOUT;
    vtd_GCMD_wbf_wrf(&vtd->registers.vtd, 1);
    do {
        pending = vtd_GSTS_wbfs_rdf(&vtd->registers.vtd);
    } while((pending != 0) && timeout--);
}

static inline void vtd_cmd_queued_invalidation_toggle(struct vtd *vtd, bool toggle)
{
    uint32_t pending;
    uint32_t timeout = INTEL_VTD_COMMAND_TIMEOUT;
    vtd_GCMD_qie_wrf(&vtd->registers.vtd, toggle);
    do {
        pending = vtd_GSTS_qies_rdf(&vtd->registers.vtd);
    } while((pending != toggle) && timeout--);
}

static inline void vtd_cmd_queued_invalidation_enable(struct vtd *vtd)
{
    vtd_cmd_queued_invalidation_toggle(vtd, 1);
}

static inline void vtd_cmd_queued_invalidation_disable(struct vtd *vtd)
{
    vtd_cmd_queued_invalidation_toggle(vtd, 0);
}

static inline void vtd_cmd_interrupt_remapping_toggle(struct vtd *vtd, bool toggle)
{
    uint32_t pending;
    uint32_t timeout = INTEL_VTD_COMMAND_TIMEOUT;
    vtd_GCMD_ire_wrf(&vtd->registers.vtd, toggle);
    do {
        pending = vtd_GSTS_ires_rdf(&vtd->registers.vtd);
    } while((pending != toggle) && timeout--);
}

static inline void vtd_cmd_interrupt_remapping_enable(struct vtd *vtd)
{
    vtd_cmd_interrupt_remapping_toggle(vtd, 1);
}

static inline void vtd_cmd_interrupt_remapping_disable(struct vtd *vtd)
{
    vtd_cmd_interrupt_remapping_toggle(vtd, 0);
}

static inline void vtd_cmd_set_interrupt_remap_table_ptr(struct vtd *vtd)
{
    uint32_t pending;
    uint32_t timeout = INTEL_VTD_COMMAND_TIMEOUT;
    vtd_GCMD_sirtp_wrf(&vtd->registers.vtd, 1);
    do {
        pending = vtd_GSTS_irtps_rdf(&vtd->registers.vtd);
    } while((pending == 0) && timeout--);
}

static inline void vtd_cmd_compat_format_interrupt_toggle(struct vtd *vtd, bool toggle)
{
    uint32_t pending;
    uint32_t timeout = INTEL_VTD_COMMAND_TIMEOUT;
    vtd_GCMD_cfi_wrf(&vtd->registers.vtd, toggle);
    do {
        pending = vtd_GSTS_cfis_rdf(&vtd->registers.vtd);
    } while((pending != toggle) && timeout--);
}

static inline void vtd_cmd_compat_format_interrupt_enable(struct vtd *vtd)
{
    vtd_cmd_compat_format_interrupt_toggle(vtd, 1);
}

static inline void vtd_cmd_compat_format_interrupt_disable(struct vtd *vtd)
{
    vtd_cmd_compat_format_interrupt_toggle(vtd, 0);
}







#endif /// INTEL_VTD_H_