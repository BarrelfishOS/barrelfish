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


/*
 * local_apic(proc_id:8, apicid:8, flags:32);
 * io_apic(apic_id:8, pbaseaddress:32, globalirqbase:32)
 * interrupt_override(bus:8, sourceirq:8, globalirq:32, intiflags:16)
 * nmi_source(intiflags:16, globalirq:32)
 * local_apic_nmi(proc_id:8, intiflags:16, lint:8)
 * local_apic_overritde(address:64)
 * io_sapic(id:8, globalirqbase:32, address:64)
 * local_sapic(proc_id:8, id:8, eid:8, lapicflags:32, uid:32, uidstring:x)
 * interrupt_source(intiflags:16, type:8, id:8, eid:8, iosapicvector:8, globalirq:32, flags:32)
 * local_x2apic(localapicid:32, laicflags:32,uid:32)
 * local_x2apic_nmi(intiflags:16, uid:32, lint:8)
 * generic_interrupt(
     CpuInterfaceNumber:32; Uid:32, Flags:32, ParkingVersion:32,
    PerformanceInterrupt:32, ParkedAddress:64, BaseAddress:64,
    GicvBaseAddress:64, GichBaseAddress:64, VgicInterrupt:32, GicrBaseAddress:64,
    ArmMpidr:64, EfficiencyClass:8)
 * generic_distributor(gicid:32, baseaddr:64, globalirqbase:32, version:8)
 * generic_msi_frame(msiframeid:32, baseaddr:64, flags:32, spicount:16, spibase:16)
 * generic_redistributor(baseaddr: 64, length:32)
 * generic_translator(translationid:32, baseaddr:64)
 */

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

    debug_printf("[dmar] [drhd]\n");

    while((void *)dsc < end) {
        assert(dsc->Length - sizeof(ACPI_DMAR_DEVICE_SCOPE) == 2);
        pcip = (ACPI_DMAR_PCI_PATH *)((uint8_t *)dsc + sizeof(ACPI_DMAR_DEVICE_SCOPE));
        void *pcip_end = ((uint8_t *) dsc + dsc->Length);
        while((void *)pcip < pcip_end) {
            err = skb_execute_query("bridge(PCIE,addr(%d,%d,%d),_,_,_,_,_,secondary(BUS)),"
                                    "write(secondary_bus(BUS)).", dsc->Bus,
                                    pcip->Device, pcip->Function);
            if (err_is_fail(err)) {
                goto loop_next_pcip;
            }

            uint64_t bus;
            err = skb_read_output("secondary_bus(%" SCNu64 ")", &bus);
            if (err_is_fail(err)) {
                goto loop_next_pcip;
            }

            debug_printf("[dmar] [drhd]" SKB_SCHEMA_DMAR_DEVSC "\n",
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
loop_next_pcip:
            pcip += 1;
        }

        dsc = (ACPI_DMAR_DEVICE_SCOPE *) ((uint8_t *) dsc + dsc->Length);
    }

    return SYS_ERR_OK;
}


static errval_t parse_hardware_unit(ACPI_DMAR_HARDWARE_UNIT *drhd, void *end)
{
    errval_t err;

    debug_printf("[dmar] [drhd]" SKB_SCHEMA_DMAR_HW_UNIT "\n",
                 drhd->Flags, drhd->Segment, drhd->Address);

    err = skb_add_fact(SKB_SCHEMA_DMAR_HW_UNIT, drhd->Flags, drhd->Segment,
                       drhd->Address);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Failed to insert into SKB: " SKB_SCHEMA_DMAR_HW_UNIT "\n",
                  drhd->Flags, drhd->Segment, drhd->Address);
    }

    void *sub = ((uint8_t *)drhd) + sizeof(ACPI_DMAR_ATSR);
    err = parse_device_scope(sub, end, drhd->Segment, ACPI_DMAR_TYPE_HARDWARE_UNIT);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Failed to parse device scope: " SKB_SCHEMA_DMAR_HW_UNIT "\n",
                  drhd->Flags, drhd->Segment, drhd->Address);
    }

    return oct_mset(SET_SEQUENTIAL, HW_PCI_IOMMU_RECORD_FORMAT, HW_PCI_IOMMU_INTEL,
                    drhd->Flags, drhd->Segment, drhd->Address);
}

static errval_t parse_reserved_memory(ACPI_DMAR_RESERVED_MEMORY *rmem, void *end)
{
    errval_t err;

    debug_printf("[dmar] [rmem]" SKB_SCHEMA_DMAR_RESERVED_MEMORY "\n",
                 rmem->Segment, rmem->BaseAddress, rmem->EndAddress);

    err = skb_add_fact(SKB_SCHEMA_DMAR_RESERVED_MEMORY,
                        rmem->Segment, rmem->BaseAddress, rmem->EndAddress);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Failed to insert into SKB: "
                  SKB_SCHEMA_DMAR_RESERVED_MEMORY "\n",
                  rmem->Segment, rmem->BaseAddress, rmem->EndAddress);
    }

    void *sub = ((uint8_t *)rmem) + sizeof(ACPI_DMAR_ATSR);
    return parse_device_scope(sub, end, rmem->Segment,
                              ACPI_DMAR_TYPE_RESERVED_MEMORY);
}

static errval_t parse_root_ats_capabilities(ACPI_DMAR_ATSR *atsr, void *end)
{
    errval_t err;

    debug_printf("[dmar] [atsr]" SKB_SCHEMA_DMAR_ATSR "\n",
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
    debug_printf("[dmar] [rhsa]" SKB_SCHEMA_DMAR_RHSA "\n",
                 rhsa->BaseAddress, rhsa->ProximityDomain);

    return skb_add_fact(SKB_SCHEMA_DMAR_RHSA, rhsa->BaseAddress,
                        rhsa->ProximityDomain);
}

static errval_t parse_namespace_device_declaration(ACPI_DMAR_ANDD *andd)
{
    debug_printf("[dmar] [andd]" SKB_SCHEMA_DMAR_ANDD "\n",
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
        return ACPI_ERR_OBJECT_NOT_FOUND;
    }
    else {
        dmar = (ACPI_TABLE_DMAR*)ath;
    }

    skb_add_fact("dmar(%"PRIu8")", dmar->Flags);


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
                parse_hardware_unit(p, p_end);
                break;
            case ACPI_DMAR_TYPE_RESERVED_MEMORY:
                parse_reserved_memory(p, p_end);
                break;
            case ACPI_DMAR_TYPE_ROOT_ATS:
                parse_root_ats_capabilities(p, p_end);
                break;
            case ACPI_DMAR_TYPE_HARDWARE_AFFINITY:
                err = parse_hardware_resource_affinity(p);
                break;
            case ACPI_DMAR_TYPE_NAMESPACE:
                err = parse_namespace_device_declaration(p);
                break;
            default:
                assert(!"Reserved for future use!\n");
        }
        p = p_end;
    }

    return SYS_ERR_OK;
}
