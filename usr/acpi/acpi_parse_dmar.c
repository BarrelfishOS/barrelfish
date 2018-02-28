/**
 * \file acpi_parse_dmar.c
 * \brief 
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

#include <arch/aarch64/hw_records_arch.h>
#include <arch/x86_64/hw_records_arch.h>

#include <skb/skb.h>
#include <octopus/getset.h>
#include <trace/trace.h>


#include "acpi_debug.h"
#include "acpi_shared.h"



#define SKB_SCHEMA_DMAR \
    "dmar(%" PRIu8 ")."

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



static errval_t parse_device_scope(ACPI_DMAR_DEVICE_SCOPE *dsc, void *end,
                                   uint16_t segment, enum AcpiDmarScopeType type)
{
    errval_t err;
    ACPI_DMAR_PCI_PATH *pcip;

    while((void *)dsc < end) {
        debug_printf("[dmar] [dscp] parsing device scope. length %u, segment %u, "
                             "memory [%p..%p]\n",
                     dsc->Length, segment, dsc, end);

        pcip = (ACPI_DMAR_PCI_PATH *)((uint8_t *)dsc + sizeof(ACPI_DMAR_DEVICE_SCOPE));
        void *pcip_end = ((uint8_t *) dsc + dsc->Length);
        while((void *)pcip < pcip_end) {
            uint64_t bus = 0xffff;

            debug_printf("[dmar] [dscp] parsing PCI path of PCI Addr=[%u.%u.%u]\n",
                         dsc->Bus, pcip->Function, pcip->Device);

            #if 0
            err = skb_execute_query("bridge(PCIE,addr(%d,%d,%d),_,_,_,_,_,secondary(BUS)),"
                                    "write(secondary_bus(BUS)).", dsc->Bus,
                                    pcip->Device, pcip->Function);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "[dmar] [dscp] SKB query failed\n");
                goto loop_next_pcip;
            }


            err = skb_read_output("secondary_bus(%" SCNu64 ")", &bus);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "[dmar] [dscp] reading query result\n");
                goto loop_next_pcip;
            }
            #endif
            debug_printf("[dmar] [dscp] " SKB_SCHEMA_DMAR_DEVSC "\n",
                         type, dsc->EntryType, segment, (uint8_t)bus, pcip->Device,
                         pcip->Function, dsc->EnumerationId);

            err = skb_add_fact(SKB_SCHEMA_DMAR_DEVSC, type, dsc->EntryType,
                               segment, (uint8_t)bus, pcip->Device, pcip->Function,
                               dsc->EnumerationId);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "Failed to insert fact into the SKB"
                          SKB_SCHEMA_DMAR_DEVSC "\n", type, dsc->EntryType,
                          segment, (uint8_t)bus, pcip->Device, pcip->Function,
                          dsc->EnumerationId);
            }
//loop_next_pcip:
            pcip += 1;
        }

        dsc = (ACPI_DMAR_DEVICE_SCOPE *) ((uint8_t *) dsc + dsc->Length);
    }

    return SYS_ERR_OK;
}


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

    void *sub = ((uint8_t *)drhd) + sizeof(ACPI_DMAR_HARDWARE_UNIT);
    err = parse_device_scope(sub, end, drhd->Segment, ACPI_DMAR_TYPE_HARDWARE_UNIT);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Failed to parse device scope: " SKB_SCHEMA_DMAR_HW_UNIT "\n",
                  drhd->Flags, drhd->Segment, drhd->Address);
    }

    debug_printf("[dmar] [drhd] set " HW_PCI_IOMMU_RECORD_FORMAT "\n",
                 HW_PCI_IOMMU_INTEL, drhd->Flags, drhd->Segment, drhd->Address);
    return oct_mset(SET_SEQUENTIAL, HW_PCI_IOMMU_RECORD_FORMAT, HW_PCI_IOMMU_INTEL,
                    drhd->Flags, drhd->Segment, drhd->Address);
}

static errval_t parse_reserved_memory(ACPI_DMAR_RESERVED_MEMORY *rmem, void *end)
{
    errval_t err;

    debug_printf("[dmar] [rmem] " SKB_SCHEMA_DMAR_RESERVED_MEMORY "\n",
                 rmem->Segment, rmem->BaseAddress, rmem->EndAddress);

    err = skb_add_fact(SKB_SCHEMA_DMAR_RESERVED_MEMORY,
                        rmem->Segment, rmem->BaseAddress, rmem->EndAddress);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Failed to insert into SKB: "
                  SKB_SCHEMA_DMAR_RESERVED_MEMORY "\n",
                  rmem->Segment, rmem->BaseAddress, rmem->EndAddress);
    }

    void *sub = ((uint8_t *)rmem) + sizeof(ACPI_DMAR_RESERVED_MEMORY);
    return parse_device_scope(sub, end, rmem->Segment,
                              ACPI_DMAR_TYPE_RESERVED_MEMORY);
}

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

    if (atsr->Flags & ACPI_DMAR_ALL_PORTS) {
        return err;
    }

    void *sub = ((uint8_t *)atsr) + sizeof(ACPI_DMAR_ATSR);
    return parse_device_scope(sub, end, atsr->Segment, ACPI_DMAR_TYPE_ROOT_ATS);
}

static errval_t parse_hardware_resource_affinity(ACPI_DMAR_RHSA *rhsa)
{
    debug_printf("[dmar] [rhsa] " SKB_SCHEMA_DMAR_RHSA "\n",
                 rhsa->BaseAddress, rhsa->ProximityDomain);

    return skb_add_fact(SKB_SCHEMA_DMAR_RHSA, rhsa->BaseAddress,
                        rhsa->ProximityDomain);
}

static errval_t parse_namespace_device_declaration(ACPI_DMAR_ANDD *andd)
{
    debug_printf("[dmar] [andd] " SKB_SCHEMA_DMAR_ANDD "\n",
                 andd->DeviceNumber, andd->DeviceName);

    return skb_add_fact(SKB_SCHEMA_DMAR_ANDD, andd->DeviceNumber,
                        andd->DeviceName);
}


errval_t acpi_parse_dmar(void)
{
    errval_t err;

    ACPI_STATUS         as;
    ACPI_TABLE_DMAR     *dmar;
    ACPI_TABLE_HEADER   *ath;

    // Get the ACPI DMAR table (the DMAR)
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

    skb_add_fact(SKB_SCHEMA_DMAR, dmar->Flags);


    debug_printf("DMAR Revision: %u, Size=%u, OEM=%s\n", dmar->Header.Revision,
               dmar->Header.Length, dmar->Header.OemId);

    void *p = (void *)dmar + sizeof(ACPI_TABLE_DMAR);
    void *table_end = (void *)dmar + dmar->Header.Length;

    while(p < table_end) {
        ACPI_DMAR_HEADER *sh = (ACPI_DMAR_HEADER *)p;
        assert(sh->Length);
        void *p_end = p + sh->Length;

        debug_printf("parsing region: [%p...%p]\n", p, p_end -1);

        switch (sh->Type) {
            case ACPI_DMAR_TYPE_HARDWARE_UNIT:
                err = parse_hardware_unit(p, p_end);
                if (err_is_fail(err)) {
                    DEBUG_ERR(err, "parsing hardware unit failed\n");
                }
                break;
            case ACPI_DMAR_TYPE_RESERVED_MEMORY:
                err = parse_reserved_memory(p, p_end);
                if (err_is_fail(err)) {
                    DEBUG_ERR(err, "reserved memory failed\n");
                }
                break;
            case ACPI_DMAR_TYPE_ROOT_ATS:
                err = parse_root_ats_capabilities(p, p_end);
                if (err_is_fail(err)) {
                    DEBUG_ERR(err, "parsing root ats caps failed\n");
                }
                break;
            case ACPI_DMAR_TYPE_HARDWARE_AFFINITY:
                err = parse_hardware_resource_affinity(p);
                if (err_is_fail(err)) {
                    DEBUG_ERR(err, "parsing resource affinity failed\n");
                }
                break;
            case ACPI_DMAR_TYPE_NAMESPACE:
                err = parse_namespace_device_declaration(p);
                if (err_is_fail(err)) {
                    DEBUG_ERR(err, "parsing namespace declarations failed\n");
                }
                break;
            default:
                assert(!"Reserved for future use!\n");
        }
        p = p_end;
    }

    return SYS_ERR_OK;
}
