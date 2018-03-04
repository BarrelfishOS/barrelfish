/**
 * \file acpi_parse_dmar.c
 * \brief ACPI DMA REMAPPING STRUCTURE
 *
 * Intel Virtualization Technology for Directed I/O Architecture Specification,
 * Rev. 2.5 November 2017, Chapter 8 BIOS Considerations
 * https://software.intel.com/sites/default/files/managed/c5/15/vt-directed-io-spec.pdf
 */

/*
 * Copyright (c) 2017 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>

#include <skb/skb.h>
#include <octopus/getset.h>

#include <hw_records.h>

#include "acpi_debug.h"
#include "acpi_shared.h"


#define SKB_SCHEMA_DMAR \
    "dmar(%" PRIu8 ", %" PRIu8 ")."

#define SKB_SCHEMA_DMAR_HW_UNIT \
    "dmar_drhd(%" PRIu8 ", %" PRIu16 ", %" PRIu64 ")."

#define SKB_SCHEMA_DMAR_RESERVED_MEMORY \
    "dmar_rmem(%" PRIu16 ", %" PRIu64 ", %" PRIu64 ")."

#define SKB_SCHEMA_DMAR_ATSR \
    "dmar_atsr(%" PRIu8 ", %" PRIu16 ")."

#define SKB_SCHEMA_DMAR_RHSA \
    "dmar_rhsa(%" PRIu64 ", %" PRIu32 ")."

#define SKB_SCHEMA_DMAR_ANDD \
     "dmar_andd(%" PRIu8 ", %s)."

#define SKB_SCHEMA_DMAR_DEVSC \
    "dmar_devsc(%" PRIu8 ", %" PRIu8 ", %" PRIu16 ", %" PRIu8  "%" PRIu8 \
                ", %" PRIu8 ", %" PRIu8  ")."


/*
 * The Device Scope Structure is made up of Device Scope Entries. Each Device
 * Scope Entry may be used to indicate a PCI endpoint device, a PCI sub-hierarchy,
 * or devices such as I/OxAPICs or HPET (High Precision Event Timer).
 *
 * A PCI sub-hierarchy is defined as the collection of PCI controllers that are
 * downstream to a specific PCI-PCI bridge. To identify a PCI sub-hierarchy,
 * the Device Scope Entry needs to identify only the parent PCI-PCI bridge of
 * the sub-hierarchy.
 */
static errval_t parse_device_scope(ACPI_DMAR_DEVICE_SCOPE *dsc, void *end,
                                   uint16_t segment, enum AcpiDmarScopeType type,
                                   bool include_all_flag)
{
    errval_t err;
    ACPI_DMAR_PCI_PATH *pcip;

    while((void *)dsc < end) {
        if ((dsc->Length - sizeof(ACPI_DMAR_DEVICE_SCOPE))
                > sizeof(ACPI_DMAR_PCI_PATH)) {
            debug_printf("  > [dmar] [dscp] Too deep in the hierarchy, we curently "
                         "only handle path length of 1.\n");
            dsc = (ACPI_DMAR_DEVICE_SCOPE *) ((uint8_t *) dsc + dsc->Length);
            continue;
        }
        assert((dsc->Length - sizeof(ACPI_DMAR_DEVICE_SCOPE)) == sizeof(ACPI_DMAR_PCI_PATH));
        pcip = (ACPI_DMAR_PCI_PATH *)((uint8_t *)dsc + sizeof(ACPI_DMAR_DEVICE_SCOPE));

        /* we currently just put the raw entry into the SKB */
        err = skb_add_fact(SKB_SCHEMA_DMAR_DEVSC, type, dsc->EntryType,
                           segment, dsc->Bus, pcip->Device, pcip->Function,
                           dsc->EnumerationId);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Failed to insert fact into the SKB"
                    SKB_SCHEMA_DMAR_DEVSC "\n", type, dsc->EntryType,
                      segment, dsc->Bus, pcip->Device, pcip->Function,
                      dsc->EnumerationId);
        }

        switch(dsc->EntryType) {
            case ACPI_DMAR_SCOPE_TYPE_ENDPOINT:
                /*
                 * 0x01: PCI Endpoint Device - The device identified by the 'Path'
                 * field is a PCI endpoint device. This type must not be used in
                 * Device Scope of DRHD structures with INCLUDE_PCI_ALL flag Set.
                 */
                debug_printf("  > [dmar] [dscp] PCI Endpoint Device. Enumeration ID=%u,"
                                     "Start Bus: %u, Path Length: %u\n",
                             dsc->EnumerationId, dsc->Bus, (dsc->Length - 6) >> 1);
                assert(dsc->EnumerationId == 0);
               // assert(!(type == ACPI_DMAR_TYPE_HARDWARE_UNIT && include_all_flag));

                break;
            case ACPI_DMAR_SCOPE_TYPE_BRIDGE:
                /*
                 * 0x02: PCI Sub-hierarchy - The device identified by the 'Path'
                 * field is a PCI-PCI bridge. In this case, the specified bridge
                 * device and all its downstream devices are included in the scope.
                 * This type must not be in Device Scope of DRHD structures with
                 * INCLUDE_PCI_ALL flag Set.
                 */
                debug_printf("  > [dmar] [dscp] PCI-PCI Bridge. Enumeration ID=%u,"
                                     "Start Bus: %u, Path Length: %u\n",
                             dsc->EnumerationId, dsc->Bus, (dsc->Length - 6) >> 1);
                assert(dsc->EnumerationId == 0);
                //assert(!(type == ACPI_DMAR_TYPE_HARDWARE_UNIT && include_all_flag));
                break;
            case ACPI_DMAR_SCOPE_TYPE_IOAPIC:
                /*
                 * 0x03: IOAPIC - The device identified by the 'Path' field is
                 * an I/O APIC (or I/O SAPIC) device, enumerated through the
                 * ACPI MADT I/O APIC (or I/O SAPIC) structure.
                 *
                 * Enumeration ID: the IOAPIC ID as provided in ACPI MADT
                 */

                debug_printf("  > [dmar] [dscp] IOAPIC. Enumeration ID=%u,"
                                     "Start Bus: %u, Path Length: %u\n",
                             dsc->EnumerationId, dsc->Bus, (dsc->Length - 6) >> 1);

                break;
            case ACPI_DMAR_SCOPE_TYPE_HPET:
                /* 0x04: MSI_CAPABLE_HPET1 - The device identified by the 'Path'
                 * field is an HPET Timer Block capable of generating MSI (Message
                 * Signaled interrupts). HPET hardware is reported through ACPI
                 * HPET structure.
                 *
                 * Enumeration ID: HPET Number corresponding to the APCI HPET block
                 */
                debug_printf("  > [dmar] [dscp] MSI HPET Device. Enumeration ID=%u,"
                                     "Start Bus: %u, Path Length: %u\n",
                             dsc->EnumerationId, dsc->Bus, (dsc->Length - 6) >> 1);
                break;
            case ACPI_DMAR_SCOPE_TYPE_NAMESPACE:
                /*
                 * 0x05: ACPI_NAMESPACE_DEVICE - The device identified by the
                 * 'Path' field is an ACPI namespace enumerated device capable
                 * of generating DMA requests.
                 *
                 * Enumeration ID is the ACPI device number as in  ANDD structure
                 */
                debug_printf("  > [dmar] [dscp] ACPI Namespace device. Enumeration ID=%u,"
                                     "Start Bus: %u, Path Length: %u\n",
                             dsc->EnumerationId, dsc->Bus, (dsc->Length - 6) >> 1);
            default:
                return ACPI_ERR_INVALID_HANDLE;
        }

       dsc = (ACPI_DMAR_DEVICE_SCOPE *) ((uint8_t *) dsc + dsc->Length);
    }

    return SYS_ERR_OK;
}

/**
 * @brief parses the DMA remapping hardware unit structure
 *
 * @param drhd  pointer to the ACPI DRHD sub table
 * @param end   pointer to the end of the sub table
 *
 * @return SYS_ERR_OK on success, errval on failure
 *
 * Each remapping hardware unit is reported by such a structure. there is
 * at least one structure per segment.
 */
static errval_t parse_hardware_unit(ACPI_DMAR_HARDWARE_UNIT *drhd, void *end)
{
    errval_t err;

    debug_printf("[dmar] [drhd] " SKB_SCHEMA_DMAR_HW_UNIT "\n",
                 drhd->Flags, drhd->Segment, drhd->Address);

    err = skb_add_fact(SKB_SCHEMA_DMAR_HW_UNIT, drhd->Flags, drhd->Segment,
                       drhd->Address);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Failed to insert into SKB: " SKB_SCHEMA_DMAR_HW_UNIT "\n",
                  drhd->Flags, drhd->Segment, drhd->Address);
    }

    /*
     * If Set, this remapping hardware unit has under its scope all PCI
     * compatible devices in the specified Segment, except devices reported
     * under the scope of other remapping hardware units for the same Segment.
     * If a DRHD structure with INCLUDE_PCI_ALL flag Set is reported for a
     * Segment, it must be enumerated by BIOS after all other DRHD structures
     * for the same Segment1. A DRHD structure with INCLUDE_PCI_ALL flag Set
     * may use the 'DeviceScope' field to enumerate I/OxAPIC and HPET
     * devices under its scope.
     */
    if (drhd->Flags & ACPI_DMAR_INCLUDE_ALL) {
        debug_printf("[dmar] [drhd] ACPI_DMAR_INCLUDE_ALL set for segment %u\n",
                     drhd->Segment);
    }

    /*
     * The Device Scope structure contains zero or more Device Scope Entries
     * that identify devices in the specified segment and under the scope of
     * this remapping hardware unit.
     */
    void *sub = ((uint8_t *)drhd) + sizeof(ACPI_DMAR_HARDWARE_UNIT);
    err = parse_device_scope(sub, end, drhd->Segment, ACPI_DMAR_TYPE_HARDWARE_UNIT,
                             drhd->Flags & ACPI_DMAR_INCLUDE_ALL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Failed to parse device scope: " SKB_SCHEMA_DMAR_HW_UNIT "\n",
                  drhd->Flags, drhd->Segment, drhd->Address);
    }

    debug_printf("[dmar] [drhd] set " HW_PCI_IOMMU_RECORD_FORMAT "\n",
                 HW_PCI_IOMMU_INTEL, drhd->Flags, drhd->Segment, drhd->Address);
    return oct_mset(SET_SEQUENTIAL, HW_PCI_IOMMU_RECORD_FORMAT, HW_PCI_IOMMU_INTEL,
                    drhd->Flags, drhd->Segment, drhd->Address);
}


/**
 * @brief parses the reserved memory region (RMRR) structures
 *
 * @param rmem  pointer ot the ACIP RMRR sub table
 * @param end   pointer to the end of the table
 *
 * @return SYS_ERR_OK on success, errval on failure
 *
 * The RMRR regions are expected to be used for legacy usages
 * (such as USB, UMA Graphics, etc.) requiring reserved memory.
 *
 * The BIOS reports each memory region through a RMRR structure and a list of
 * devices that require access to the specified reserved memory region.
 */
static errval_t parse_reserved_memory(ACPI_DMAR_RESERVED_MEMORY *rmem, void *end)
{
    errval_t err;

    debug_printf("[dmar] [rmem] " SKB_SCHEMA_DMAR_RESERVED_MEMORY "\n",
                 rmem->Segment, rmem->BaseAddress, rmem->EndAddress);

    err = skb_add_fact(SKB_SCHEMA_DMAR_RESERVED_MEMORY, rmem->Segment,
                       rmem->BaseAddress, rmem->EndAddress);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Failed to insert into SKB: "
                  SKB_SCHEMA_DMAR_RESERVED_MEMORY "\n",
                  rmem->Segment, rmem->BaseAddress, rmem->EndAddress);
    }

    void *sub = ((uint8_t *)rmem) + sizeof(ACPI_DMAR_RESERVED_MEMORY);
    return parse_device_scope(sub, end, rmem->Segment,
                              ACPI_DMAR_TYPE_RESERVED_MEMORY, false);
}


/**
 * @brief parses the root-port address translation services (ATSR) structure
 *
 * @param atsr      pointer to the ACPI sub table
 * @param end       end address of the table
 *
 * @return SYS_ERR_OK on success, errval on failure
 *
 * This structure is only for platforms supporting device TLBs. For each
 * PCI segment there shall be one ATSR structure. The structure identifies
 * which PCI Express root ports supporting ATS transactions
 */
static errval_t parse_root_ats_capabilities(ACPI_DMAR_ATSR *atsr, void *end)
{
    errval_t err;

    debug_printf("[dmar] [atsr] " SKB_SCHEMA_DMAR_ATSR "\n",
                 atsr->Flags, atsr->Segment);

    err = skb_add_fact(SKB_SCHEMA_DMAR_ATSR, atsr->Flags, atsr->Segment);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Failed to insert into the SKB: " SKB_SCHEMA_DMAR_ATSR "\n",
                  atsr->Flags, atsr->Segment);
    }

    /*
     *  Bit 0: ALL_PORTS:
     *      If Set, all PCI-Express Root Ports in the segment support ATS transactions.
     *      If Clear, only root-ports indicated by the device scope fields support
     *                ATS transactions
     */
    if (atsr->Flags & ACPI_DMAR_ALL_PORTS) {
        debug_printf("[dmar] [atsr] ACPI_DMAR_ALL_PORTS flag is set. "
                      "Omitting device scope structures\n");
        return SYS_ERR_OK;
    }

    /*
     * The Device Scope structure is described in Section 8.3.1. All Device Scope
     * Entries in this structure must have a Device Scope Entry Type of
     * 02h
     */
    void *sub = ((uint8_t *)atsr) + sizeof(ACPI_DMAR_ATSR);
    return parse_device_scope(sub, end, atsr->Segment, ACPI_DMAR_TYPE_ROOT_ATS,
                              false);
}


/**
 * @brief parses the Remapping Hardware Static Affinity Structure
 *
 * @param rhsa  pointer to the ACPI sub table
 *
 * @return SYS_ERR_OK on success, errval on failure
 *
 * On systems with NUMA nodes, it may be better peformance wise to allocate
 * translation structures on the NUMA node close by the hardware unit.
 * This RHSA structure provides proximity information similar to the SRAT
 * table.
 */
static errval_t parse_hardware_resource_affinity(ACPI_DMAR_RHSA *rhsa)
{
    debug_printf("[dmar] [rhsa] " SKB_SCHEMA_DMAR_RHSA "\n",
                 rhsa->BaseAddress, rhsa->ProximityDomain);

    return skb_add_fact(SKB_SCHEMA_DMAR_RHSA, rhsa->BaseAddress,
                        rhsa->ProximityDomain);
}

/**
 * @brief parses ACPI name-space device declarations
 *
 * @param andd  pointer to the ACPI namespace table
 *
 * @return SYS_ERR_OK on success, errval on failure
 *
 * NOTE: This is not yet implemented.
 */
static errval_t parse_namespace_device_declaration(ACPI_DMAR_ANDD *andd)
{
    debug_printf("[dmar] [andd] NYI! " SKB_SCHEMA_DMAR_ANDD "\n",
                 andd->DeviceNumber, andd->DeviceName);

    return skb_add_fact(SKB_SCHEMA_DMAR_ANDD, andd->DeviceNumber,
                        andd->DeviceName);
}


/**
 * @brief  Parses the DMA Remapping Reporting Table (DMAR)
 *
 * @return SYS_ERR_OK on success, error value on failure
 */
errval_t acpi_parse_dmar(void)
{
    errval_t err;

    ACPI_STATUS         as;
    ACPI_TABLE_DMAR     *dmar;
    ACPI_TABLE_HEADER   *ath;

    /* Get the ACPI DMAR table (the DMAR) */
    as = AcpiGetTable(ACPI_SIG_DMAR, 1, (ACPI_TABLE_HEADER **)&ath);

    if(ACPI_FAILURE(as)) {
        debug_printf("No DMAR found in ACPI! Cannot initialize IO MMUs.\n");

        return oct_mset(SET_SEQUENTIAL, HW_PCI_IOMMU_RECORD_FORMAT, HW_PCI_IOMMU_INTEL,
                        0, 0, 0);

        return ACPI_ERR_OBJECT_NOT_FOUND;
    }
    else {
        dmar = (ACPI_TABLE_DMAR*)ath;
    }

    /*
     * Width: Number of bits that can be addressed by the platform in DMA.
     *        the field is stored as W = Width + 1.
     *
     * Flags:
     * Bit 0: INTR_REMAP
     * Bit 1: X2APIC_OPT_OUT
     * Bit 2: DMA_CTRL_PLATFORM_OPT_IN_FLAG
     */
    skb_add_fact(SKB_SCHEMA_DMAR, dmar->Width + 1, dmar->Flags);


    debug_printf("DMAR Revision: %u, Size=%u, OEM=%s, HAW=%u, flags=%x\n",
                 dmar->Header.Revision, dmar->Header.Length, dmar->Header.OemId,
                 dmar->Width + 1, dmar->Flags);


    void *p = (void *)dmar + sizeof(ACPI_TABLE_DMAR);
    void *table_end = (void *)dmar + dmar->Header.Length;

    while(p < table_end) {
        ACPI_DMAR_HEADER *sh = (ACPI_DMAR_HEADER *)p;
        assert(sh->Length);
        void *p_end = p + sh->Length;

        switch (sh->Type) {
            case ACPI_DMAR_TYPE_HARDWARE_UNIT:
                err = parse_hardware_unit(p, p_end);
                if (err_is_fail(err)) {
                    DEBUG_ERR(err, "parsing hardware unit failed. Continuing...\n");
                }
                break;
            case ACPI_DMAR_TYPE_RESERVED_MEMORY:
                err = parse_reserved_memory(p, p_end);
                if (err_is_fail(err)) {
                    DEBUG_ERR(err, "reserved memory failed. Continuing...\n");
                }
                break;
            case ACPI_DMAR_TYPE_ROOT_ATS:
                err = parse_root_ats_capabilities(p, p_end);
                if (err_is_fail(err)) {
                    DEBUG_ERR(err, "parsing root ats caps failed. Continuing...\n");
                }
                break;
            case ACPI_DMAR_TYPE_HARDWARE_AFFINITY:
                err = parse_hardware_resource_affinity(p);
                if (err_is_fail(err)) {
                    DEBUG_ERR(err, "parsing resource affinity failed. "
                                   "Continuing...\n");
                }
                break;
            case ACPI_DMAR_TYPE_NAMESPACE:
                err = parse_namespace_device_declaration(p);
                if (err_is_fail(err)) {
                    DEBUG_ERR(err, "parsing namespace declarations failed. "
                                   "Continuing...\n");
                }
                break;
            default:
                USER_PANIC("Discovered unknown subtable %u. Consider updating"
                           "ACPI. Skipping\n", sh->Type);
        }
        p = p_end;
    }

    return SYS_ERR_OK;
}
