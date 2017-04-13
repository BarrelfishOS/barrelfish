/**
 * \file acpi_parse_madt.c
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

static coreid_t barrelfish_id_counter = 1;


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

#define SKB_SCHEMA_LOCAL_APIC \
    "local_apic(%" PRIu8 ", %" PRIu8 ", %" PRIu32 ")."
#define SKB_SCHEMA_IO_APIC \
    "io_apic(%" PRIu8 ", %" PRIu32 ", %" PRIu32 ")."
#define SKB_SCHEMA_INTERRUPT_OVERRIDE \
    "interrupt_override(%" PRIu8 ", %" PRIu8 ", %" PRIu32 ", %" PRIu16 ")."
#define SKB_SCHEMA_NMI_SOURCE \
    "nmi_source(%" PRIu16 ", %" PRIu32 ")."
#define SKB_SCHEMA_LOCAL_APIC_NMI \
    "local_apic_nmi(%" PRIu8 ", %" PRIu16 ", %" PRIu8 ")."
#define SKB_SCHEMA_LOCAL_APIC_OVERRIDE \
    "local_apic_override(%" PRIu64 ")."
#define SKB_SCHEMA_IO_SAPIC \
    "io_sapic(%" PRIu8 ", %" PRIu32 ", %" PRIu64 ")."
#define SKB_SCHEMA_LOCAL_SAPIC \
    "local_sapic(%" PRIu8 ", %" PRIu8 ", %" PRIu8", %" PRIu32 ", %" PRIu32 ", %s)."
#define SKB_SCHEMA_INTERRUPT_SOURCE \
    "interrupt_source(%" PRIu16 ", %" PRIu8 ", %" PRIu8", %" PRIu8 ", %" PRIu32 ", %" PRIu32 ")."
#define SKB_SCHEMA_LOCAL_X2APIC \
    "local_x2apic(%" PRIu32 ", %" PRIu32 ", %" PRIu32 ")."
#define SKB_SCHEMA_LOCAL_X2APIC_NMI \
    "local_x2apic_nmi(%" PRIu16 ", %" PRIu32 ", %" PRIu8 ")."
#define SKB_SCHEMA_GENERIC_INTERRUPT \
    "generic_interrupt(%" PRIu32 ", %" PRIu32 ", %" PRIu32 ", %" PRIu32 ", %" PRIu32 ", %" \
                          PRIu64 ", %" PRIu64 ", %" PRIu64 ", %" PRIu64 ", %" PRIu32 ", %" \
                          PRIu64 ", %" PRIu64 ", %" PRIu8 ")."
#define SKB_SCHEMA_GENERIC_DISTRIBUTOR \
    "generic_distributor(%" PRIu32 ", %" PRIu64 ", %" PRIu32 ", %" PRIu8 ")."
#define SKB_SCHEMA_GENERIC_MSI_FRAME \
    "generic_msi_frame(%" PRIu32 ", %" PRIu64 ", %" PRIu32 ", %" PRIu16 ", %" PRIu16 ")."
#define SKB_SCHEMA_GENERIC_REDISTRIBUTOR \
    "generic_redistributor(%" PRIu64 ", %" PRIu32 ")."
#define SKB_SCHEMA_GENERIC_TRANSLATOR \
    "generic_translator(%" PRIu32 ", %" PRIu64 ")."




static errval_t parse_entry_local_apic(ACPI_MADT_LOCAL_APIC *s)
{
    errval_t err;

    APCI_DEBUG(SKB_SCHEMA_LOCAL_APIC, s->ProcessorId, s->Id, s->LapicFlags);

    ACPI_DEBUG("Found local APIC: CPU = %d, ID = %d, usable = %d\n",
            s->ProcessorId, s->Id,
            s->LapicFlags & ACPI_MADT_ENABLED);

    trace_event(TRACE_SUBSYS_ACPI, TRACE_EVENT_ACPI_APIC_ADDED, s->ProcessorId);

    err = skb_add_fact(SKB_SCHEMA_LOCAL_APIC, s->ProcessorId, s->Id, s->LapicFlags);
    if (err_is_fail(err)) {
        return err;
    }

    coreid_t barrelfish_id;
    if (my_hw_id == s->Id) {
        barrelfish_id = 0; // BSP core is 0
    } else {
        barrelfish_id = barrelfish_id_counter++;
    }

    /* compatibility */
    skb_add_fact("apic(%d,%d,%"PRIu32").", s->ProcessorId, s->Id,
                 s->LapicFlags & ACPI_MADT_ENABLED);

    return oct_set(HW_PROCESSOR_X86_RECORD_FORMAT, barrelfish_id,
                   s->LapicFlags & ACPI_MADT_ENABLED, barrelfish_id,
                   s->Id, CPU_X86_64, s->ProcessorId, s->Id);
}

static errval_t parse_entry_io_apic(ACPI_MADT_IO_APIC *s)
{
    errval_t err;

    ACPI_DEBUG(SKB_SCHEMA_IO_APIC, s->Id, s->Address, s->GlobalIrqBase);
    skb_add_fact(SKB_SCHEMA_IO_APIC, s->Id, s->Address, s->GlobalIrqBase);

    ACPI_DEBUG("Found I/O APIC: ID = %d, mem base = 0x%"PRIx32", "
           "INTI base = %"PRIu32"\n", s->Id, s->Address, s->GlobalIrqBase);

    skb_add_fact("ioapic(%d,%"PRIu32",%"PRIu32").", s->Id, s->Address, s->GlobalIrqBase);
    skb_add_fact("memory_region(%"PRIu32",%u,%zu, %u,%u).",
                 s->Address,
                 BASE_PAGE_BITS, //as used elswhere in acpi.c
                 ((size_t)1) << BASE_PAGE_BITS, //as used elswhere in acpi.c
                 RegionType_IOAPIC,
                 0);

    char ioapic_lbl[255];
    char query_buf[1024];
    snprintf(query_buf,1024, "add_ioapic_controller(Lbl, %d, %d),"
            "write('\n'), writeln(Lbl).",
            s->Id, s->GlobalIrqBase);
    err = skb_execute(query_buf);
    if(err_is_fail(err)){
        DEBUG_SKB_ERR(err,"add_ioapic_controller");
    }
    skb_read_output_at(strchr(skb_get_output(),'\n'),"%s",ioapic_lbl);
    ACPI_DEBUG("Added ioapic ctrl, lbl=%s\n",ioapic_lbl);

    err = init_one_ioapic(s, ioapic_lbl);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "Unable to initialize I/O APIC (ID = %d)",
                  s->Id);
        abort();
    }

    return SYS_ERR_OK;
}

static errval_t parse_entry_interrupt_override(ACPI_MADT_INTERRUPT_OVERRIDE *s)
{
    ACPI_DEBUG(SKB_SCHEMA_INTERRUPT_OVERRIDE, s->Bus, s->SourceIrq, s->GlobalIrq,
               s->IntiFlags);

    skb_add_fact(SKB_SCHEMA_INTERRUPT_OVERRIDE, s->Bus, s->SourceIrq, s->GlobalIrq,
                   s->IntiFlags);

    ACPI_DEBUG("Found interrupt override: bus = %d, bus_irq = %d, "
           "GSI = %"PRIu32", flags = %x\n", s->Bus, s->SourceIrq,
           s->GlobalIrq, s->IntiFlags);

    // ACPI spec says these are only for ISA interrupts
    assert(s->SourceIrq < N_ISA_INTERRUPTS);

    interrupt_overrides[s->SourceIrq] = s->GlobalIrq;

    if (s->IntiFlags == 0) {
        break;
    }

    lpc_ioapic_redir_tbl_t entry = ioapic_redir_tmpl_isa;
    struct ioapic *a = find_ioapic(s->GlobalIrq);
    if (a == NULL) {
        ACPI_DEBUG("Warning: unknown IOAPIC for GSI %"PRIu32", ignored"
                  " interrupt override flags.\n", s->GlobalIrq);
        break;
    }

    // Set polarity
    assert((s->IntiFlags & ACPI_MADT_POLARITY_MASK)
           != ACPI_MADT_POLARITY_RESERVED);

    switch(s->IntiFlags & ACPI_MADT_POLARITY_MASK) {
    case ACPI_MADT_POLARITY_ACTIVE_HIGH:
        entry.polarity = lpc_ioapic_active_high;
        break;

    case ACPI_MADT_POLARITY_ACTIVE_LOW:
        entry.polarity = lpc_ioapic_active_low;
        break;
    }

    // Set trigger mode
    assert((s->IntiFlags & ACPI_MADT_TRIGGER_MASK)
           != ACPI_MADT_TRIGGER_RESERVED);

    switch(s->IntiFlags & ACPI_MADT_TRIGGER_MASK) {
    case ACPI_MADT_TRIGGER_EDGE:
        entry.trigger = lpc_ioapic_edge;
        break;

    case ACPI_MADT_TRIGGER_LEVEL:
        // XXX: should be lpc_ioapic_level
        entry.trigger = lpc_ioapic_edge;
        break;
    }

    ioapic_setup_inti(a, s->GlobalIrq - a->irqbase, entry);

    return SYS_ERR_OK;
}

static errval_t parse_entry_nmi_source(ACPI_MADT_NMI_SOURCE *e)
{
    ACPI_DEBUG(SKB_SCHEMA_NMI_SOURCE, e->IntiFlags, e->GlobalIrq);

    return skb_add_fact(SKB_SCHEMA_NMI_SOURCE, e->IntiFlags, e->GlobalIrq);;
}

static errval_t parse_entry_local_apic_nmi(ACPI_MADT_LOCAL_APIC_NMI *s)
{
    ACPI_DEBUG(SKB_SCHEMA_LOCAL_APIC_NMI, s->ProcessorId, s->IntiFlags, s->Lint);

    /* compatibility entry */
    skb_add_fact("apic_nmi(%d,%d,%d).",s->ProcessorId, s->IntiFlags, s->Lint);

    return skb_add_fact(SKB_SCHEMA_LOCAL_APIC_NMI, s->ProcessorId, s->IntiFlags,
                        s->Lint);
}

static errval_t parse_entry_local_apic_override(ACPI_MADT_LOCAL_APIC_OVERRIDE *e)
{
    ACPI_DEBUG(SKB_SCHEMA_LOCAL_APIC_OVERRIDE, e->Address);

    return skb_add_fact(SKB_SCHEMA_LOCAL_APIC_OVERRIDE, e->Address);
}

static errval_t parse_entry_io_sapic(ACPI_MADT_IO_SAPIC *e)
{
    ACPI_DEBUG(SKB_SCHEMA_IO_SAPIC, e->Id, e->GlobalIrqBase, e->Address);

    return skb_add_fact(SKB_SCHEMA_IO_SAPIC, e->Id, e->GlobalIrqBase, e->Address);
}

static errval_t parse_entry_local_sapic(ACPI_MADT_LOCAL_SAPIC *e)
{
    ACPI_DEBUG(SKB_SCHEMA_LOCAL_SAPIC, e->ProcessorId, e->Id, e->Eid, e->LapicFlags,
               e->Uid, e->UidString);

    return skb_add_facts(SKB_SCHEMA_LOCAL_SAPIC, e->ProcessorId, e->Id, e->Eid,
                         e->LapicFlags, e->Uid, e->UidString);;
}

static errval_t parse_entry_interrupt_source(ACPI_MADT_INTERRUPT_SOURCE *e)
{
    ACPI_DEBUG(SKB_SCHEMA_INTERRUPT_SOURCE, e->IntiFlags, e->Type, e->Id, e->Eid,
               e->IoSapicVector, e->GlobalIrq, e->Flags);

    return skb_add_facts(SKB_SCHEMA_INTERRUPT_SOURCE, e->IntiFlags, e->Type, e->Id,
                         e->Eid, e->IoSapicVector, e->GlobalIrq, e->Flags);
}

static errval_t parse_entry_local_x2apic(ACPI_MADT_LOCAL_X2APIC *e)
{
    ACPI_DEBUG(SKB_SCHEMA_LOCAL_X2APIC, e->LocalApicId, e->LapicFlags, e->Uid);

    return skb_add_facts(SKB_SCHEMA_LOCAL_X2APIC, e->LocalApicId, e->LapicFlags, e->Uid);
}

static errval_t parse_entry_local_x2apic_nmi(ACPI_MADT_LOCAL_X2APIC_NMI *e)
{
    ACPI_DEBUG(SKB_SCHEMA_LOCAL_X2APIC_NMI, e->Uid, e->Lint);

    return skb_add_facts(SKB_SCHEMA_LOCAL_X2APIC_NMI, e->Uid, e->Lint);
}

static errval_t parse_entry_generic_interrupt(ACPI_MADT_GENERIC_INTERRUPT *s)
{
    errval_t err;

    ACPI_DEBUG(SKB_SCHEMA_GENERIC_INTERRUPT, s->CpuInterfaceNumber, s->Uid, s->Flags,
               s->ParkingVersion, s->PerformanceInterrupt, s->ParkedAddress,
               s->BaseAddress, s->GicvBaseAddress, s->GichBaseAddress,
               s->VgicInterrupt, s->GicrBaseAddress, s->ArmMpidr, s->EfficiencyClass);

    err = skb_add_facts(SKB_SCHEMA_GENERIC_INTERRUPT, s->CpuInterfaceNumber,
                        s->Uid, s->Flags, s->ParkingVersion, s->PerformanceInterrupt,
                        s->ParkedAddress, s->BaseAddress, s->GicvBaseAddress,
                        s->GichBaseAddress, s->VgicInterrupt, s->GicrBaseAddress,
                        s->ArmMpidr, s->EfficiencyClass);
    if (err_is_fail(err)) {
        return err;
    }

    /* figure out the barrelfish ID */
    coreid_t barrelfish_id;
    if (my_hw_id == s->Uid) {
        barrelfish_id = 0; // BSP core is 0
    } else {
        barrelfish_id = barrelfish_id_counter++;
    }

    /* figure out the boot protocol */
    if (s->ParkingVersion) {
        /* parking */
        err = skb_add_fact("boot_driver_entry(%"PRIu64",%s).", s->ArmMpidr,
                           "armBootParking");
    } else {
        /* psci */
        err = skb_add_fact("boot_driver_entry(%"PRIu64",%s).", s->ArmMpidr,
                            "armBootPSCI");
    }

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to add the boot protocol, continuing anyway\n");
    }

    /* setting the octopus record */
    return oct_set(HW_PROCESSOR_ARMV8_RECORD_FORMAT, barrelfish_id,
                   s->Flags & ACPI_MADT_ENABLED, barrelfish_id, s->ArmMpidr,
                   CPU_ARM8, s->CpuInterfaceNumber, s->Uid, s->Flags, s->ParkingVersion,
                   s->PerformanceInterrupt, s->ParkedAddress, s->BaseAddress,
                   s->GicvBaseAddress, s->GichBaseAddress, s->VgicInterrupt,
                   s->GicrBaseAddress, s->ArmMpidr);
}

static errval_t parse_entry_generic_distributor(ACPI_MADT_GENERIC_DISTRIBUTOR *s)
{
    ACPI_DEBUG(SKB_SCHEMA_GENERIC_DISTRIBUTOR, s->GicId, s->BaseAddress,
               s->GlobalIrqBase, s->Version);

    return skb_add_facts(SKB_SCHEMA_GENERIC_DISTRIBUTOR, s->GicId, s->BaseAddress,
                         s->GlobalIrqBase, s->Version);
}

static errval_t parse_entry_generic_msi_frame(ACPI_MADT_GENERIC_MSI_FRAME *s)
{
    ACPI_DEBUG(SKB_SCHEMA_GENERIC_REDISTRIBUTOR, s->MsiFrameId, s->BaseAddress,
               s->Flags, s->SpiCount, s->SpiBase);

    return skb_add_facts(SKB_SCHEMA_GENERIC_REDISTRIBUTOR, s->MsiFrameId,
                         s->BaseAddress, s->Flags, s->SpiCount, s->SpiBase);
}

static errval_t parse_entry_generic_redistributor(ACPI_MADT_GENERIC_REDISTRIBUTOR *s)
{
    ACPI_DEBUG(SKB_SCHEMA_GENERIC_REDISTRIBUTOR, s->BaseAddress, s->Length);

    return skb_add_fact(SKB_SCHEMA_GENERIC_REDISTRIBUTOR, s->BaseAddress, s->Length);
}

static errval_t parse_entry_generic_translator(ACPI_MADT_GENERIC_TRANSLATOR *s)
{
    ACPI_DEBUG(SKB_SCHEMA_GENERIC_TRANSLATOR, s->TranslationId, s->BaseAddress);

    return skb_add_fact(SKB_SCHEMA_GENERIC_TRANSLATOR, s->TranslationId,
                       s->BaseAddress);
}



errval_t acpi_parse_madt(void)
{
    errval_t err;

    ACPI_STATUS         as;
    ACPI_TABLE_MADT     *madt;
    ACPI_TABLE_HEADER   *ath;

    // Get the ACPI APIC table (the MADT)
    as = AcpiGetTable("APIC", 1, (ACPI_TABLE_HEADER **)&ath);

    if(ACPI_FAILURE(as)) {
        ACPI_DEBUG("No MADT found in ACPI! Cannot initialize I/O APICs.\n");
        return -1; // TODO: error value
    }
    else {
        madt = (ACPI_TABLE_MADT*)ath;
    }


    ACPI_DEBUG("MADT Revision: %u, Size=%u, OEM=%s\n", madt->Header.Revision,
               madt->Header.Length, madt->Header.OemId);

    void *p = (void *)madt + sizeof(ACPI_TABLE_MADT);
    void *table_end = (void *)madt + madt->Header.Length;

    while(p < table_end) {
        ACPI_SUBTABLE_HEADER *sh = (ACPI_SUBTABLE_HEADER *)p;

        switch(sh->Type) {
        case ACPI_MADT_TYPE_LOCAL_APIC :
            err = parse_entry_local_apic((ACPI_MADT_LOCAL_APIC *)sh);
            break;
        case ACPI_MADT_TYPE_IO_APIC :
            err = parse_entry_io_apic((ACPI_MADT_IO_APIC *)sh);
            break;
        case ACPI_MADT_TYPE_INTERRUPT_OVERRIDE :
            err = parse_entry_interrupt_override((ACPI_MADT_INTERRUPT_OVERRIDE *)sh);
            break;
        case ACPI_MADT_TYPE_NMI_SOURCE :
            err = parse_entry_nmi_source((ACPI_MADT_NMI_SOURCE *)sh);
            break;
        case ACPI_MADT_TYPE_LOCAL_APIC_NMI :
            err = parse_entry_local_apic_nmi((ACPI_MADT_LOCAL_APIC_NMI *)sh);
            break;
        case ACPI_MADT_TYPE_LOCAL_APIC_OVERRIDE :
            err = parse_entry_local_apic_override((ACPI_MADT_LOCAL_APIC_OVERRIDE *)sh);
            break;
        case ACPI_MADT_TYPE_IO_SAPIC :
            err = parse_entry_io_sapic((ACPI_MADT_IO_SAPIC *)sh);
            break;
        case ACPI_MADT_TYPE_LOCAL_SAPIC :
            err = parse_entry_local_sapic((ACPI_MADT_LOCAL_SAPIC *)sh);
            break;
        case ACPI_MADT_TYPE_INTERRUPT_SOURCE :
            err = parse_entry_interrupt_source((ACPI_MADT_INTERRUPT_SOURCE *)sh);
            break;
        case ACPI_MADT_TYPE_LOCAL_X2APIC :
            err = parse_entry_local_x2apic((ACPI_MADT_LOCAL_X2APIC *)sh);
            break;
        case ACPI_MADT_TYPE_LOCAL_X2APIC_NMI :
            err = parse_entry_local_x2apic_nmi((ACPI_MADT_LOCAL_X2APIC_NMI *)sh);
            break;
        case ACPI_MADT_TYPE_GENERIC_INTERRUPT :
            err = parse_entry_generic_interrupt((ACPI_MADT_GENERIC_INTERRUPT *)sh);
            break;
        case ACPI_MADT_TYPE_GENERIC_DISTRIBUTOR :
            err = parse_entry_generic_distributor((ACPI_MADT_GENERIC_DISTRIBUTOR *)sh);
            break;
        case ACPI_MADT_TYPE_GENERIC_MSI_FRAME :
            err = parse_entry_generic_msi_frame((ACPI_MADT_GENERIC_MSI_FRAME *)sh);
            break;
        case ACPI_MADT_TYPE_GENERIC_REDISTRIBUTOR :
            err = parse_entry_generic_redistributor((ACPI_MADT_GENERIC_REDISTRIBUTOR *)sh);
            break;
        case ACPI_MADT_TYPE_GENERIC_TRANSLATOR :
            err = parse_entry_generic_translator((ACPI_MADT_GENERIC_TRANSLATOR *)sh);
            break;
        default:
            /* reserved */
            err = SYS_ERR_OK;
        }

        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to parse the entry. continuing\n");
        }

        assert(sh->Length);
        p += sh->Length;
    }

    return SYS_ERR_OK;
}
