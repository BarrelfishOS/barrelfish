/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INTEL_VTD_COMANDS_H_
#define INTEL_VTD_COMANDS_H_ 1

#include <dev/vtd_dev.h>

#include "intel_vtd_debug.h"

#define INTEL_VTD_COMMAND_TIMEOUT 0x1000

static inline void vtd_cmd_translation_toggle(struct vtd *vtd, bool toggle)
{
    bool pending;
    uint32_t timeout = INTEL_VTD_COMMAND_TIMEOUT;
    vtd_GCMD_te_wrf(&vtd->vtd_dev, toggle);
    do {
        pending = vtd_GSTS_tes_rdf(&vtd->vtd_dev);
    } while((pending != toggle) && timeout--);
    assert(vtd_GSTS_tes_rdf(&vtd->vtd_dev) == toggle);
}

static inline void vtd_cmd_translation_disable(struct vtd *vtd)
{
    INTEL_VTD_DEBUG_COMMANDS("translation disable\n");
    vtd_cmd_translation_toggle(vtd, 0);
}

static inline void vtd_cmd_translation_enable(struct vtd *vtd)
{
    INTEL_VTD_DEBUG_COMMANDS("translation enable\n");
    vtd_cmd_translation_toggle(vtd, 1);
}

static inline void vtd_cmd_set_root_table_ptr(struct vtd *vtd, genpaddr_t addr)
{
    bool pending;

    vtd_RTADDR_rta_wrf(&vtd->vtd_dev, (addr >> BASE_PAGE_BITS));

    uint32_t timeout = INTEL_VTD_COMMAND_TIMEOUT;
    vtd_GCMD_srtp_wrf(&vtd->vtd_dev, 1);
    do {
        pending = vtd_GSTS_rtps_rdf(&vtd->vtd_dev);
    } while((pending == 0) && timeout--);
    assert(vtd_GSTS_rtps_rdf(&vtd->vtd_dev));
}

static inline void vtd_cmd_set_fault_log(struct vtd *vtd)
{
    bool pending;

    uint32_t timeout = INTEL_VTD_COMMAND_TIMEOUT;
    vtd_GCMD_sfl_wrf(&vtd->vtd_dev, 1);
    do {
        pending = vtd_GSTS_fls_rdf(&vtd->vtd_dev);
    } while((pending == 0) && timeout--);
    assert(vtd_GSTS_fls_rdf(&vtd->vtd_dev));
}

static inline void vtd_cmd_adv_fault_logging_toggle(struct vtd *vtd, bool toggle)
{
    bool pending;
    uint32_t timeout = INTEL_VTD_COMMAND_TIMEOUT;
    vtd_GCMD_eafl_wrf(&vtd->vtd_dev, toggle);
    do {
        pending = vtd_GSTS_afls_rdf(&vtd->vtd_dev);
    } while((pending != toggle) && timeout--);
    assert(vtd_GSTS_afls_rdf(&vtd->vtd_dev) == toggle);
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
    bool pending;
    uint32_t timeout = INTEL_VTD_COMMAND_TIMEOUT;
    vtd_GCMD_wbf_wrf(&vtd->vtd_dev, 1);
    do {
        pending = vtd_GSTS_wbfs_rdf(&vtd->vtd_dev);
    } while((pending != 0) && timeout--);
    assert(vtd_GSTS_wbfs_rdf(&vtd->vtd_dev) == 0);
}

static inline void vtd_cmd_queued_invalidation_toggle(struct vtd *vtd, bool toggle)
{
    bool pending;
    uint32_t timeout = INTEL_VTD_COMMAND_TIMEOUT;
    vtd_GCMD_qie_wrf(&vtd->vtd_dev, toggle);
    do {
        pending = vtd_GSTS_qies_rdf(&vtd->vtd_dev);
    } while((pending != toggle) && timeout--);
    assert(vtd_GSTS_qies_rdf(&vtd->vtd_dev) == toggle);
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
    bool pending;
    uint32_t timeout = INTEL_VTD_COMMAND_TIMEOUT;
    vtd_GCMD_ire_wrf(&vtd->vtd_dev, toggle);
    do {
        pending = vtd_GSTS_ires_rdf(&vtd->vtd_dev);
    } while((pending != toggle) && timeout--);
    assert(vtd_GSTS_ires_rdf(&vtd->vtd_dev) == toggle);
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
    bool pending;
    uint32_t timeout = INTEL_VTD_COMMAND_TIMEOUT;
    vtd_GCMD_sirtp_wrf(&vtd->vtd_dev, 1);
    do {
        pending = vtd_GSTS_irtps_rdf(&vtd->vtd_dev);
    } while((pending == 0) && timeout--);
    assert(vtd_GSTS_irtps_rdf(&vtd->vtd_dev));
}

static inline void vtd_cmd_compat_format_interrupt_toggle(struct vtd *vtd, bool toggle)
{
    bool pending;
    uint32_t timeout = INTEL_VTD_COMMAND_TIMEOUT;
    vtd_GCMD_cfi_wrf(&vtd->vtd_dev, toggle);
    do {
        pending = vtd_GSTS_cfis_rdf(&vtd->vtd_dev);
    } while((pending != toggle) && timeout--);
    assert(vtd_GSTS_cfis_rdf(&vtd->vtd_dev) == toggle);
}

static inline void vtd_cmd_compat_format_interrupt_enable(struct vtd *vtd)
{
    vtd_cmd_compat_format_interrupt_toggle(vtd, 1);
}

static inline void vtd_cmd_compat_format_interrupt_disable(struct vtd *vtd)
{
    vtd_cmd_compat_format_interrupt_toggle(vtd, 0);
}


#endif /// INTEL_VTD_COMMANDS_H_
