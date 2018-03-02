/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include "intel_vtd.h"
#include "intel_vtd_commands.h"
#include "intel_vtd_iotlb.h"

static inline uint8_t vtd_get_supported_bits(struct vtd *vtd)
{
    if (vtd_CAP_sllps30_rdf(&vtd->vtd_dev)) {
        return 30;
    } else if (vtd_CAP_sllps21_rdf(&vtd->vtd_dev)) {
        return 21;
    } else {
        return 12;
    }
}

static errval_t vtd_parse_capabilities(struct vtd *vtd)
{
    /*
     * TODO: 5LP: 5-level Paging Support
     * 0: Hardware does not support 5-level paging for requests-with-PASID
     *    subject to first-level translation.
     * 1: Hardware supports 5-level paging for requests-with-PASID subject to
     *    first-level translation.
     *
     * TODO: FL1GP: First Level 1-GByte Page Support
     * A value of 1 in this field indicates 1-GByte page size is supported for
     * first-level translation.
     *
     * TODO: MAMV: Maximum Address Mask Value
     *
     * TODO: DIT: Device-TLB Invalidation Throttle
     * TODO: PSS: PASID Size Supported
     * TODO: DIS: Deferred Invalidate Support
     * TODO: MHMV: Maximum Handle Mask Value
     */

    INTEL_VTD_DEBUG_CAP("=================================================\n");


    /*
     * QI: Queued Invalidation support
     */
    vtd->capabilities.queued_invalidation = vtd_ECAP_qis_rdf(&vtd->vtd_dev);
    INTEL_VTD_DEBUG_CAP("Queued invalidation: %u\n",
                        vtd->capabilities.queued_invalidation);

    /*
     * IR: Interrupt Remapping support
     * Implementations reporting this field as Set must also support
     * Queued Invalidation (QI)
     */
    vtd->capabilities.interrupt_remapping = vtd_ECAP_ir_rdf(&vtd->vtd_dev);
    assert(!vtd->capabilities.interrupt_remapping ||
           (vtd->capabilities.interrupt_remapping && vtd->capabilities.queued_invalidation));
    INTEL_VTD_DEBUG_CAP("Interrupt remapping: %u\n",
                        vtd->capabilities.interrupt_remapping);

    /*
     * EIM: Extended Interrupt Mode
     * 0: On Intel® 64 platforms, hardware supports only 8-bit APIC-IDs
     *    (xAPIC Mode).
     * 1: On Intel® 64 platforms, hardware supports 32-bit APIC-IDs
     *    (x2APIC mode).
     *
     * This field is valid only on Intel® 64 platforms reporting Interrupt
     * Remapping support (IR field Set).
     */
    if (vtd->capabilities.interrupt_remapping) {
        vtd->capabilities.extended_interrupt_mode = vtd_ECAP_eim_rdf(&vtd->vtd_dev);
    } else {
        vtd->capabilities.extended_interrupt_mode = 0;
    }
    INTEL_VTD_DEBUG_CAP("Extended interrupt mode: %u\n",
                        vtd->capabilities.extended_interrupt_mode);


    /*
     * PI: Posted Interrupts Support
     * Hardware implementations reporting this field as Set must also report
     * Interrupt Remapping support (IR field in Extended Capability Register)
     * field as Set. Refer Section 5.2 for description of interrupt posting.
     */
    vtd->capabilities.interrupt_posting = vtd_CAP_pi_rdf(&vtd->vtd_dev);
    assert(!vtd->capabilities.interrupt_posting ||
           (vtd->capabilities.interrupt_posting && vtd->capabilities.extended_interrupt_mode));
    INTEL_VTD_DEBUG_CAP("Interrupt posting: %u\n",
                        vtd->capabilities.interrupt_posting);

    /*
     * PASID: Process Address Space ID Support
     */
    vtd->capabilities.pasid = vtd_ECAP_pasid_rdf(&vtd->vtd_dev);
    INTEL_VTD_DEBUG_CAP("Process address space ID support: %u\n",
                        vtd->capabilities.pasid);

    /*
     * DRD: Read Draining (Refer Section 6.5.5)
     * Hardware implementations reporting support for process-address-spaces
     * (PASID=1 in Extended Capability Register) must report this field as 1.
     */
    vtd->capabilities.read_draining = vtd_CAP_drd_rdf(&vtd->vtd_dev);
    assert(!vtd->capabilities.pasid ||
           (vtd->capabilities.pasid && vtd->capabilities.read_draining));
    INTEL_VTD_DEBUG_CAP("Read Draining: %u\n",
                        vtd->capabilities.read_draining);

    /*
     * DWD: Write Draining (Refer Section 6.5.5)
     * Hardware implementations reporting support for process-address-spaces
     * (PASID=1 in Extended Capability Register) must report this field as 1.
     */
    vtd->capabilities.write_draining = vtd_CAP_dwd_rdf(&vtd->vtd_dev);
    assert(!vtd->capabilities.pasid ||
           (vtd->capabilities.pasid && vtd->capabilities.write_draining));
    INTEL_VTD_DEBUG_CAP("Write draining: %u\n",
                        vtd->capabilities.write_draining);

    /*
     * PSI: Page Selective Invalidation
     *  0: Hardware supports only global and domain-selective invalidates for IOTLB.
     *  1: Hardware supports page-selective, domain-selective, and global invalidates for IOTLB.
     *
     * implementations supporting PASID must support page/address selective IOTLB
     * invalidation for first-level translation.
     */
    vtd->capabilities.tlb_page_invalidation = vtd_CAP_psi_rdf(&vtd->vtd_dev);
    assert(!vtd->capabilities.pasid ||
           (vtd->capabilities.pasid && vtd->capabilities.tlb_page_invalidation));
    INTEL_VTD_DEBUG_CAP("TLB - page selective invalidation: %u\n",
                        vtd->capabilities.tlb_page_invalidation);

    /*
     * SLLPS: Second Level Large Page Support
     * Hardware implementations supporting a specific large-page size must support
     * all smaller largepage sizes. i.e., only valid values for this field
     * are 0000b, 0001b, 0011b.
     */
    if (vtd_CAP_sllps30_rdf(&vtd->vtd_dev)) {
        vtd->max_page_size_bits = HUGE_PAGE_BITS;
        assert(vtd_CAP_sllps21_rdf(&vtd->vtd_dev));
    } else if(vtd_CAP_sllps21_rdf(&vtd->vtd_dev)) {
        vtd->max_page_size_bits = LARGE_PAGE_BITS;
    } else {
        vtd->max_page_size_bits = BASE_PAGE_BITS;
    }

    INTEL_VTD_DEBUG_CAP("Maximum page size: 2^%u\n", vtd->max_page_size_bits);


    /*
     * ZLR: Zero Length Read
     * DMA remapping hardware implementations are recommended to report ZLR
     * field as Set.
     */
    if (!vtd_CAP_zlr_rdf(&vtd->vtd_dev)) {
        INTEL_VTD_NOTICE("Zero Length Reads capablity not set\n");
    }


    /*
     * MGAW: Maximum Guest Address Width
     * This field indicates the maximum guest physical address width supported
     * by second-level translation in remapping hardware
     */
    vtd->max_guest_address_width = vtd_CAP_mgaw_rdf(&vtd->vtd_dev) + 1;
    INTEL_VTD_DEBUG_CAP("Maximum guest address width: 2^%u\n",
                        vtd->max_guest_address_width);


    /*
     * SAGAW: Supported Adjusted Guest Address Widths
     *
     * This 5-bit field indicates the supported adjusted guest address widths
     * (which in turn represents the levels of page-table walks for the 4KB base
     * page size) supported by the hardware implementation.
     *
     * Software must ensure that the adjusted guest address width used to set up
     * the page tables is one of the supported guest address widths reported in
     * this field.
     */
    vtd->capabilities.paging_3_level = vtd_CAP_sagaw39_rdf(&vtd->vtd_dev);
    vtd->capabilities.paging_4_level = vtd_CAP_sagaw48_rdf(&vtd->vtd_dev);
    vtd->capabilities.paging_5_level = vtd_CAP_sagaw57_rdf(&vtd->vtd_dev);

    INTEL_VTD_DEBUG_CAP("Adjusted guest address width: %s%s%s\n",
                        vtd->capabilities.paging_3_level ? "39-bits, " : "",
                        vtd->capabilities.paging_4_level ? "48-bits, " : "",
                        vtd->capabilities.paging_5_level ? "57-bits" : "");

    /*
     * CM: Caching Mode
     * Hardware implementations of this architecture must support a value of 0
     * in this field.
     *
     * 0: Not-present and erroneous entries are not cached in any of the
     *    remapping caches. Invalidations are not required for modifications to
     *    individual not present or invalid entries. However, any modifications
     *    that result in decreasing the effective permissions or partial
     *    permission increases require invalidations for them to be effective.
     * 1: Not-present and erroneous mappings may be cached in the remapping caches.
     *    Any software updates to the remapping structures (including updates to
     *    "notpresent" or erroneous entries) require explicit invalidation
     */
    vtd->capabilities.cachingmode = vtd_CAP_cm_rdf(&vtd->vtd_dev);
    INTEL_VTD_DEBUG_CAP("Caching Mode: %u\n",
                        vtd->capabilities.cachingmode);

    /*
     * PHMR: Protected High-Memory Region
     */
    vtd->capabilities.prot_mem_hi = vtd_CAP_phmr_rdf(&vtd->vtd_dev);
    INTEL_VTD_DEBUG_CAP("Protected high-memory region: %u\n",
                        vtd->capabilities.prot_mem_hi);

    /*
     * PLMR: Protected Low-Memory Region
     */
    vtd->capabilities.prot_mem_lo = vtd_CAP_plmr_rdf(&vtd->vtd_dev);
    INTEL_VTD_DEBUG_CAP("Protected low-memory region: %u\n",
                        vtd->capabilities.prot_mem_lo);

    /*
     * RWBF: Required Write-Buffer Flushing
     *
     * 0: Indicates no write-buffer flushing is needed to ensure changes to
     *    memoryresident structures are visible to hardware.
     * 1: Indicates software must explicitly flush the write buffers to ensure
     *    updates made to memory-resident remapping structures are visible to
     *    hardware. Refer to Section 6.8 for more details on write buffer flushing
     *    requirements.
     */
    vtd->capabilities.req_wb_flush = vtd_CAP_rwbf_rdf(&vtd->vtd_dev);
    INTEL_VTD_DEBUG_CAP("Required write-buffer flushing: %u\n",
                        vtd->capabilities.req_wb_flush);

    /*
     * AFL: Advanced Fault Logging
     * if 0 only primary fault logging is supported.
     */
    vtd->capabilities.adv_fault_logging = vtd_CAP_afl_rdf(&vtd->vtd_dev);
    INTEL_VTD_DEBUG_CAP("Advanced fault logging: %u\n",
                        vtd->capabilities.adv_fault_logging);

    /*
     * ND: Number of domains supported
     */
    vtd->max_domains = 1 << ((2 * vtd_CAP_nd_rdf(&vtd->vtd_dev)) + 4);
    INTEL_VTD_DEBUG_CAP("Maximum domains: %u\n", vtd->max_domains);


    /*
     * EAFS: Extended Accessed Flag Support
     * This field is valid only when PASID field is reported as Set.
     */
    if (vtd->capabilities.pasid) {
        vtd->capabilities.extended_access_flag = vtd_ECAP_eafs_rdf(&vtd->vtd_dev);
    } else {
        vtd->capabilities.extended_access_flag = 0;
    }
    INTEL_VTD_DEBUG_CAP("Extended accessed flag supported: %u\n",
                        vtd->capabilities.extended_access_flag);

    /*
     * PRS: Page Request Support
     * This field is valid only when Device-TLB (DT) field is reported as Set.
     */
    vtd->capabilities.page_requests = vtd_ECAP_prs_rdf(&vtd->vtd_dev);
    INTEL_VTD_DEBUG_CAP("Page request supported: %u\n",
                        vtd->capabilities.page_requests);

    /*
     * DT: Device-TLB support
     * Implementations reporting this field as Set must also support Queued
     * Invalidation (QI)
     * Hardware implementations supporting I/O Page Requests (PRS field Set in
     * Extended Capability register) must report a value of 1b in this field.
     */
    vtd->capabilities.device_tlb = vtd_ECAP_dt_rdf(&vtd->vtd_dev);
    assert(!vtd->capabilities.device_tlb ||
           (vtd->capabilities.device_tlb && vtd->capabilities.queued_invalidation));
    assert(!vtd->capabilities.page_requests ||
           (vtd->capabilities.device_tlb && vtd->capabilities.page_requests));
    INTEL_VTD_DEBUG_CAP("Device-TLB support: %u\n",
                        vtd->capabilities.device_tlb);

    /*
     * NWFS: No Write Flag Support
     * This field is valid only when Device-TLB support (DT) field is reported as Set
     */
    if (vtd->capabilities.device_tlb) {
        vtd->capabilities.nowrite_flag = vtd_ECAP_nwfs_rdf(&vtd->vtd_dev);
    } else {
        vtd->capabilities.nowrite_flag = 0;
    }
    INTEL_VTD_DEBUG_CAP("No write flag support: %u\n",
                        vtd->capabilities.device_tlb);


    /*
     * ERS: Execute Request Support
     * This field is valid only when PASID field is reported as Set.
     */
    if (vtd->capabilities.pasid) {
        vtd->capabilities.execute_requests = vtd_ECAP_ers_rdf(&vtd->vtd_dev);
    } else {
        vtd->capabilities.execute_requests = 0;
    };
    INTEL_VTD_DEBUG_CAP("Execute request support: %u\n",
                        vtd->capabilities.execute_requests);

    /*
     * SRS: Supervisor Request Support
     * This field is valid only when PASID field is reported as Set.
     */
    if (vtd->capabilities.pasid) {
        vtd->capabilities.supervisor_requests = vtd_ECAP_srs_rdf(&vtd->vtd_dev);
    } else {
        vtd->capabilities.supervisor_requests = 0;
    }
    INTEL_VTD_DEBUG_CAP("Supervisor request support: %u\n",
                        vtd->capabilities.supervisor_requests);

    /* NEST: Nested Translation Support
     * This field is valid only when PASID field is reported as Set.
     */
    if (vtd->capabilities.pasid) {
        vtd->capabilities.nested_translation = vtd_ECAP_nest_rdf(&vtd->vtd_dev);
    } else {
        vtd->capabilities.nested_translation = 0;
    }
    INTEL_VTD_DEBUG_CAP("Nested translation support: %u\n",
                        vtd->capabilities.nested_translation);

    /*
     * ECS: Extended Context Support
     * Implementations reporting PASID or PRS fields as Set, must report this
     * field as Set
     */
    vtd->capabilities.extended_context = vtd_ECAP_ecs_rdf(&vtd->vtd_dev);
    assert(!vtd->capabilities.pasid ||
           (vtd->capabilities.pasid && vtd->capabilities.extended_context));
    assert(!vtd->capabilities.page_requests ||
           (vtd->capabilities.page_requests && vtd->capabilities.extended_context));
    INTEL_VTD_DEBUG_CAP("Extended context support: %u\n",
                        vtd->capabilities.extended_context);

    /*
     * MTS: Memory Type Support
     * This field is valid only when PASID and ECS fields are reported as Set.
     *
     * Remapping hardware units with, one or more devices that operate in processor
     * coherency domain, under its scope must report this field as Set
     */
    if (vtd->capabilities.extended_context && vtd->capabilities.pasid) {
        vtd->capabilities.memory_types = vtd_ECAP_mts_rdf(&vtd->vtd_dev);
    } else {
        vtd->capabilities.memory_types = 0;
    }
    INTEL_VTD_DEBUG_CAP("Memory type support: %u\n",
                        vtd->capabilities.memory_types);


    /*
     * SC: Snoop Control
     * Implementations are recommended to support Snoop Control to support
     * software usages that require Snoop Control for assignment of devices
     * behind a remapping hardware unit.
     */
    vtd->capabilities.snoop_control = vtd_ECAP_sc_rdf(&vtd->vtd_dev);
    if (!vtd->capabilities.snoop_control) {
        INTEL_VTD_NOTICE("Snoop Control is not supported\n");
    }


    /*
     * PT: Pass Through
     * Pass-through translation is specified through Translation-Type (T) field
     * value of 10b in context-entries, or T field value of 010b in
     * extended-context-entries.
     *
     * Hardware implementation supporting PASID must report a value of 1b in this
     * field.
     */
    vtd->capabilities.pass_through = vtd_ECAP_pt_rdf(&vtd->vtd_dev);
    assert(!vtd->capabilities.pasid ||
           (vtd->capabilities.pasid && vtd->capabilities.pass_through));
    INTEL_VTD_DEBUG_CAP("TLB - pass through support: %u\n",
                        vtd->capabilities.pass_through);

    /*
     * @brief
     * C: Page-walk Coherency
     *
     * This field indicates if hardware access to the root, context,
     * extended-context and interrupt-remap tables, and second-level
     * paging structures for requests-without PASID, are coherent (snooped) or not.
     *
     * 0: Indicates hardware accesses to remapping structures are non-coherent.
     * 1: Indicates hardware accesses to remapping structures are coherent.
     *
     * Hardware access to advanced fault log, invalidation queue, invalidation
     * semaphore, page-request queue, PASID-table, PASID-state table, and
     * first-level page-tables are always coherent.
     */
    vtd->capabilities.page_walk_coherency = vtd_ECAP_pwc_rdf(&vtd->vtd_dev);
    INTEL_VTD_DEBUG_CAP("Page-walk coherency: %u\n",
                        vtd->capabilities.page_walk_coherency);

    /*
     * PDS: Page-request Drain Support
     *  0: Hardware does not support Pagerequest Drain (PD) flag in inv_wait_dsc.
     *  1: Hardware supports Page-request Drain (PD) flag in inv_wait_dsc.
     *
     *  This field is valid only when Device-TLB support field is reported as Set.
     */
    if (vtd->capabilities.device_tlb) {
        vtd->capabilities.page_requiest_drain = vtd_ECAP_pds_rdf(&vtd->vtd_dev);
    } else {
        vtd->capabilities.page_requiest_drain = 0;
    }
    INTEL_VTD_DEBUG_CAP("Page-request drain support: %u\n",
                        vtd->capabilities.page_requiest_drain);


    vtd->max_fault_recording_regs = vtd_CAP_nfr_rdf(&vtd->vtd_dev) + 1;
    INTEL_VTD_DEBUG_CAP("Fault recording registers: %u\n",
                        vtd->max_fault_recording_regs);

    INTEL_VTD_DEBUG_CAP("=================================================\n");

    return SYS_ERR_OK;
}

/**
 * @brief checks the version of the hardware
 *
 * @param vtd   pointer to the VT-d structure
 *
 * @return SYS_ERR_OK on success, VTD_ERR_NO_UNITS if the unit is not supported
 *
 */
static errval_t vtd_check_version(struct vtd *vtd)
{
    uint16_t version = 0;
    version = vtd_VER_max_rdf(&vtd->vtd_dev);
    version <<= 4;
    version |= vtd_VER_min_rdf(&vtd->vtd_dev);

    if (version != 0x10) {
        INTEL_VTD_DEBUG("Intel VT-d Version v%x not supported.\n", version);
        return VTD_ERR_NO_UNITS;
    }

    INTEL_VTD_DEBUG("Initializing Intel VT-d IOMMU v%x\n",  version);

    vtd->version = VTD_VERSION_1_0;

    return SYS_ERR_OK;
}



errval_t vtd_create(struct vtd *vtd, struct capref regs, uint16_t segment,
                    nodeid_t proximity)
{
    errval_t err;

    INTEL_VTD_DEBUG("initializing iommu for segment %u\n", segment);

    /* map the registers */
    void *registers_vbase;
    err = vspace_map_one_frame_attr(&registers_vbase, BASE_PAGE_SIZE, regs,
                                    VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);
    if (err_is_fail(err)) {
        return err;
    }

    INTEL_VTD_DEBUG("Registers mapped at: %p\n", registers_vbase);

    vtd_initialize(&vtd->vtd_dev, registers_vbase, NULL, NULL);

    size_t iro = vtd_ECAP_iro_rdf(&vtd->vtd_dev);
    size_t fro = vtd_CAP_fro_rdf(&vtd->vtd_dev);

    INTEL_VTD_DEBUG("IOTLB offset: %zu\n", iro);
    INTEL_VTD_DEBUG("FRR offset:   %zu\n", fro);

    vtd_initialize(&vtd->vtd_dev, registers_vbase,
                   registers_vbase + iro, registers_vbase + fro);


    vtd->pci_segment = segment;


    err = vtd_check_version(vtd);
    if (err_is_fail(err)) {
        return err;
    }

    /* parse the capability and extended capability register */
    vtd_parse_capabilities(vtd);


    /* TODO: set these according to the record or args */

    if (vtd_ECAP_ecs_rdf(&vtd->vtd_dev)) {
        INTEL_VTD_DEBUG("Extended Context Entry Supported\n");
        vtd->entry_type = VTD_ENTRY_TYPE_EXTENDED;
        INTEL_VTD_DEBUG("Override: Use base entries\n");
        vtd->entry_type = VTD_ENTRY_TYPE_BASE;
    } else {
        INTEL_VTD_DEBUG("Using base contex entries\n");
        vtd->entry_type = VTD_ENTRY_TYPE_BASE;
    }


    err = vtd_domains_init(vtd->max_domains);
    if (err_is_fail(err)) {
        goto err_out;
    }

    err = vtd_interrupt_remapping_init(vtd);
    if (err_is_fail(err)) {
        goto err_out;
    }




    /* create the root table */
    err = vtd_root_table_create(&vtd->root_table, proximity);
    if (err_is_fail(err)) {
        goto err_out;
    }

    err = vtd_ctxt_table_create_all(vtd->ctxt_tables, proximity);
    if (err_is_fail(err)) {
        goto err_out_1;
    }

    /* inserting context tables */
    err = vtd_root_table_map_all(&vtd->root_table, vtd->ctxt_tables);
    if (err_is_fail(err)) {
        goto err_out_2;
    }

    vtd_set_root_table(vtd);

    vtd_cmd_translation_enable(vtd);


//    skb_add_fact("vtd_enabled(%"PRIu16",%"PRIu8").", u->pci_seg, vtd_coherency(u));

    return SYS_ERR_OK;

    err_out_2:
    vtd_ctxt_table_destroy_all(vtd->ctxt_tables);
    err_out_1:
    vtd_root_table_destroy(&vtd->root_table);
    err_out:
    vspace_unmap(registers_vbase);
    return err;

}

errval_t vtd_destroy(struct vtd *v)
{
    USER_PANIC("NYI");
    return SYS_ERR_OK;
}

errval_t vtd_set_root_table(struct vtd *v)
{
    errval_t err;
    struct vnode_identity id;
    err = invoke_vnode_identify(v->root_table.rtcap, &id);
    if (err_is_fail(err)) {
        return err;
    }

    /* setting the root table type */
    switch(id.type) {
        case ObjType_VNode_VTd_root_table :
            vtd_RTADDR_rtt_wrf(&v->vtd_dev, 0);
            break;
        /*
         * TODO: extended table type support
         * case ObjType_VNode_VTd_ext_root_table :
         *  vtd_RTADDR_rtt_wrf(&v->vtd_dev, 1);
         *   break;
         */
        default:
            return VTD_ERR_INVALID_CAP;
    }

    // Update the root-table pointer
    vtd_cmd_set_root_table_ptr(v, id.base);

    // Globally invalidate the context-cache and then globally invalidate
    // the IOTLB (only in this order).
    //vtd_context_cache_glob_inval(unit);

    vtd_iotlb_invalidate(v);

    return SYS_ERR_OK;
}

#if 0


#define VTD_ENABLE_DISABLE_TIMEOUT 10000

static errval_t vtd_toggle_translation(struct vtd *vtd, bool enable)
{
    vtd_GCMD_te_wrf(&vtd->vtd_dev, 1);
    for (size_t i = 0; i < VTD_ENABLE_DISABLE_TIMEOUT; i++) {
        if (vtd_GSTS_tes_rdf(&vtd->vtd_dev) == enable) {
            return SYS_ERR_OK;
        }
    }
    if (vtd_GSTS_tes_rdf(&vtd->vtd_dev) == enable) {
        return SYS_ERR_OK;
    }
    USER_PANIC("ENABLE TRANSLATION TIMEOUT\n");
    return -1;
}

errval_t vtd_enable_translation(struct vtd *vtd)
{
    return vtd_toggle_translation(vtd, true);
}

errval_t vtd_disable_translation(struct vtd *vtd)
{
    return vtd_toggle_translation(vtd, false);
}
#endif