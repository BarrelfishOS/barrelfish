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
    if (vtd_CAP_sllps30_rdf(&vtd->registers.vtd)) {
        return 30;
    } else if (vtd_CAP_sllps21_rdf(&vtd->registers.vtd)) {
        return 21;
    } else {
        return 12;
    }
}

static errval_t vtd_parse_capabilities(struct vtd *vtd)
{
    vtd->capabilities.device_tlb              = vtd_ECAP_dt_rdf(&vtd->registers.vtd);
    vtd->capabilities.queued_invalidation     = vtd_ECAP_qis_rdf(&vtd->registers.vtd);
    vtd->capabilities.tlb_page_invalidation   = vtd_CAP_psi_rdf(&vtd->registers.vtd);
    vtd->capabilities.interrupt_remapping     = vtd_ECAP_ir_rdf(&vtd->registers.vtd);
    vtd->capabilities.interrupt_extended      = vtd_ECAP_eim_rdf(&vtd->registers.vtd);
    vtd->capabilities.interrupt_posting       = vtd_CAP_pi_rdf(&vtd->registers.vtd);
    vtd->capabilities.page_walk_coherency     = vtd_ECAP_pwc_rdf(&vtd->registers.vtd);
    vtd->capabilities.snoop_control           = vtd_ECAP_sc_rdf(&vtd->registers.vtd);
    vtd->capabilities.pass_through            = vtd_ECAP_pt_rdf(&vtd->registers.vtd);
    vtd->capabilities.extended_context        = vtd_ECAP_ecs_rdf(&vtd->registers.vtd);
    vtd->capabilities.nested_translation      = vtd_ECAP_nest_rdf(&vtd->registers.vtd);
    vtd->capabilities.memory_types            = vtd_ECAP_mts_rdf(&vtd->registers.vtd);
    vtd->capabilities.pasid                   = vtd_ECAP_pasid_rdf(&vtd->registers.vtd);
    vtd->capabilities.page_requests           = vtd_ECAP_prs_rdf(&vtd->registers.vtd);
    vtd->capabilities.page_requiest_drain     = vtd_ECAP_pdf_rdf(&vtd->registers.vtd);
    vtd->capabilities.execute_requests        = vtd_ECAP_ers_rdf(&vtd->registers.vtd);
    vtd->capabilities.supervisor_requests     = vtd_ECAP_srs_rdf(&vtd->registers.vtd);
    vtd->capabilities.nowrite_flag            = vtd_ECAP_nwfs_rdf(&vtd->registers.vtd);
    vtd->capabilities.extended_access_flag    = vtd_ECAP_eafs_rdf(&vtd->registers.vtd);
    vtd->capabilities.adv_fault_logging       = vtd_CAP_afl_rdf(&vtd->registers.vtd);
    vtd->capabilities.prot_mem_hi             = vtd_CAP_phmr_rdf(&vtd->registers.vtd);
    vtd->capabilities.prot_mem_lo             = vtd_CAP_plmr_rdf(&vtd->registers.vtd);
    vtd->capabilities.huge_pages              = vtd_CAP_sllps30_rdf(&vtd->registers.vtd);
    vtd->capabilities.write_draining          = vtd_CAP_dwd_rdf(&vtd->registers.vtd);
    vtd->capabilities.read_draining           = vtd_CAP_drd_rdf(&vtd->registers.vtd);
    vtd->capabilities.req_wb_flush            = vtd_CAP_rwbf_rdf(&vtd->registers.vtd);
    vtd->capabilities.paging_3_level          = vtd_CAP_sagaw39_rdf(&vtd->registers.vtd);
    vtd->capabilities.paging_4_level          = vtd_CAP_sagaw48_rdf(&vtd->registers.vtd);
    vtd->capabilities.paging_5_level          = vtd_CAP_sagaw57_rdf(&vtd->registers.vtd);
    vtd->capabilities.cachingmode             = vtd_CAP_cm_rdf(&vtd->registers.vtd);

    /* we require support for large pages*/
    if (!vtd_CAP_sllps21_rdf(&vtd->registers.vtd)) {
        INTEL_VTD_ERROR("does not support large pages! Aborting. \n");
        return -1;
    }

    if (vtd->capabilities.paging_5_level != vtd_CAP_lp5_rdf(&vtd->registers.vtd)) {
        INTEL_VTD_ERROR("Mismatch in SAGAW and 5LP. Aborting. \n");
        return -1;
    }

    return SYS_ERR_OK;
}

static errval_t vtd_check_version(struct vtd *vtd)
{
    uint16_t version = 0;
    version = vtd_VER_max_rdf(&vtd->registers.vtd);
    version <<= 4;
    version |= vtd_VER_min_rdf(&vtd->registers.vtd);

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

    vtd_initialize(&vtd->registers.vtd, registers_vbase);
    size_t iro = vtd_ECAP_iro_rdf(&vtd->registers.vtd);

    INTEL_VTD_DEBUG("IOTLB offset: %zu\n", iro);
    vtd_iotlb_initialize(&vtd->registers.iotlb, registers_vbase + iro);

    err = vtd_check_version(vtd);
    if (err_is_fail(err)) {
        return err;
    }

    /* parse the capability and extended capability register */
    vtd_parse_capabilities(vtd);


    /* TODO: set these according to the record or args */

    if (vtd_ECAP_ecs_rdf(&vtd->registers.vtd)) {
        INTEL_VTD_DEBUG("Extended Context Entry Supported\n");
        vtd->entry_type = VTD_ENTRY_TYPE_EXTENDED;
        INTEL_VTD_DEBUG("Override: Use base entries\n");
        vtd->entry_type = VTD_ENTRY_TYPE_BASE;
    } else {
        INTEL_VTD_DEBUG("Using base contex entries\n");
        vtd->entry_type = VTD_ENTRY_TYPE_BASE;
    }

    /* get the number of domain ids */
    vtd->domains_max = 1 << ((2 * vtd_CAP_nd_rdf(&vtd->registers.vtd)) + 4);
    INTEL_VTD_DEBUG("Maximum domains: %u (%u)\n", vtd->domains_max, vtd_CAP_nd_rdf(&vtd->registers.vtd));

    err = vtd_domains_init(vtd->domains_max);
    if (err_is_fail(err)) {
        goto err_out;
    }

    err = vtd_interrupt_remapping_init(vtd);
    if (err_is_fail(err)) {
        goto err_out;
    }
    if (vtd->capabilities.interrupt_remapping) {

    }



    vtd->address_width_max = vtd_CAP_mgaw_rdf(&vtd->registers.vtd) + 1;
    INTEL_VTD_DEBUG("Maximum guess addres width: %u\n", vtd->address_width_max);

    INTEL_VTD_DEBUG("Supports 5 Level Paging: %s\n",
                    vtd_CAP_lp5_rdf(&vtd->registers.vtd) ? "YES" : "NO");

    INTEL_VTD_DEBUG("Supports 1G Pages: %s\n",
                    vtd_CAP_sllps30_rdf(&vtd->registers.vtd) ? "YES" : "NO");

    INTEL_VTD_DEBUG("Supports 2M Pages: %s\n",
                    vtd_CAP_sllps21_rdf(&vtd->registers.vtd) ? "YES" : "NO");

    INTEL_VTD_DEBUG("Supported Adjusted Guest Address 57-bits: %s\n",
                    vtd_CAP_sagaw57_rdf(&vtd->registers.vtd) ? "YES" : "NO");

    INTEL_VTD_DEBUG("Supported Adjusted Guest Address 48-bits: %s\n",
                    vtd_CAP_sagaw48_rdf(&vtd->registers.vtd) ? "YES" : "NO");

    INTEL_VTD_DEBUG("Supported Adjusted Guest Address 39-bits: %s\n",
                    vtd_CAP_sagaw39_rdf(&vtd->registers.vtd) ? "YES" : "NO");

    INTEL_VTD_DEBUG("Supports nested translation %s\n",
                    vtd_ECAP_nest_rdf(&vtd->registers.vtd) ? "YES" : "NO");


    INTEL_VTD_DEBUG("Page-walk coherency %s\n",
                    vtd_ECAP_pwc_rdf(&vtd->registers.vtd) ? "YES" : "NO");

    vtd->pci_segment = segment;

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
            vtd_RTADDR_rtt_wrf(&v->registers.vtd, 0);
            break;
        /*
         * TODO: extended table type support
         * case ObjType_VNode_VTd_ext_root_table :
         *  vtd_RTADDR_rtt_wrf(&v->registers.vtd, 1);
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
    vtd_GCMD_te_wrf(&vtd->registers.vtd, 1);
    for (size_t i = 0; i < VTD_ENABLE_DISABLE_TIMEOUT; i++) {
        if (vtd_GSTS_tes_rdf(&vtd->registers.vtd) == enable) {
            return SYS_ERR_OK;
        }
    }
    if (vtd_GSTS_tes_rdf(&vtd->registers.vtd) == enable) {
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