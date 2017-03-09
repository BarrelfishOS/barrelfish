/**
 * \file
 * \brief Interrupt management (Local and IOAPICs) and routing
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <arch/aarch64/hw_records_arch.h>
#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/cpu_arch.h>
#include <acpi.h>
#include <mm/mm.h>

#include <skb/skb.h>
#include <octopus/getset.h>
#include <trace/trace.h>

#include "acpi_debug.h"
#include "acpi_shared.h"


int init_all_interrupt_sources(void)
{
    ACPI_STATUS         as;
    ACPI_TABLE_MADT     *madt;
    ACPI_TABLE_HEADER   *ath;

    static coreid_t barrelfish_id_counter = 1;

    // Get the ACPI APIC table (the MADT)
    as = AcpiGetTable("APIC", 1, (ACPI_TABLE_HEADER **)&ath);

    if(ACPI_FAILURE(as)) {
        ACPI_DEBUG("No MADT found in ACPI! Cannot initialize I/O APICs.\n");
        return -1;
    }
    else {
        madt = (ACPI_TABLE_MADT*)ath;
    }

    ACPI_DEBUG("MADT Revision: %u, Size=%u, OEM=%s\n", madt->Header.Revision,
               madt->Header.Length, madt->Header.OemId);

    //uint8_t revision = madt->Header.Revision;

    // Walk all subtables (after the main table entries)
    void *p = (void *)madt + sizeof(ACPI_TABLE_MADT);
    while(p < (void *)madt + madt->Header.Length) {
        ACPI_SUBTABLE_HEADER *sh = (ACPI_SUBTABLE_HEADER *)p;

        uint8_t length = sh->Length;

        switch(sh->Type) {
        case ACPI_MADT_TYPE_LOCAL_APIC:
            {
                debug_printf("WARNING LOCAL APIC found on non x86\n");
            }
            break;

        case ACPI_MADT_TYPE_IO_APIC:
            {
                debug_printf("WARNING IO APIC found on non x86\n");
            }
            break;

        case ACPI_MADT_TYPE_INTERRUPT_OVERRIDE:
            {
                debug_printf("WARNING INTERRUPT_OVERRIDE found on non x86\n");
            }
            break;

        case ACPI_MADT_TYPE_LOCAL_APIC_NMI:
            debug_printf("WARNING LOCAL_APIC_NMI found on non x86\n");
            break;
        case ACPI_MADT_TYPE_GENERIC_INTERRUPT:
            {
            ACPI_MADT_GENERIC_INTERRUPT *gi = (ACPI_MADT_GENERIC_INTERRUPT *)sh;

            /*
             * ACPI Spec 6.1 - 5.2.12.14 GIC CPU Interface (GICC) Structure
             *
             * CPU Interface Number
             * GIC's CPU Interface Number. In GICv1/v2 implementations, this
             * value matches the bit index of the associated processor in the GIC
             * distributor's GICD_ITARGETSR register.
             * For GICv3/4 implementations this field must be provided by the
             * platform, if compatibility mode is supported.
             * If it is not supported by the implementation, then this field must be
             * zero
             *
             * UID
             * The OS associates this GICC Structure with a processor device
             * object in the namespace when the _UID child object of the
             * processor device evaluates to a numeric value that matches the
             * numeric value in this field.
             *
             * Parking Address
             * The 64-bit physical address of the processor's Parking Protocol
             * mailbox
             *
             * ParkingVersion
             * Version of the ARM-Processor Parking Protocol implemented. See
             * http://uefi.org/acpi. The document link is listed under
             * "Multiprocessor Startup for ARM Platforms"
             * For systems that support PSCI exclusively and do not support the
             * parking protocol, this field must be set to 0
             *
             * BaseAddress
             * On GICv1/v2 systems and GICv3/4 systems in GICv2 compatibility
             * mode, this field holds the 64-bit physical address at which the
             * processor can access this GIC CPU Interface. If provided here, the
             * "Local Interrupt Controller Address" field in the MADT must be
             * ignored by the OSPM.
             *
             * GICV
             * Address of the GIC virtual CPU interface registers. If the platform
             * is not presenting a GICv2 with virtualization extensions this field
             * can be 0.
             *
             * GICH
             * Address of the GIC virtual interface control block registers. If the
             * platform is not presenting a GICv2 with virtualization extensions
             * this field can be 0.
             *
             * On systems supporting GICv3 and above, this field holds the 64-bit
             * physical address of the associated Redistributor. If all of the GIC
             * Redistributors are in the always-on power domain, GICR structures
             * should be used to describe the Redistributors instead, and this field
             * must be set to 0.
             *
             * MPIDR
             * This fields follows the MPIDR formatting of ARM architecture.
             * If the implements ARMv7 architecure then the format must be:
             *  Bits [63:24] Must be zero
             *  Bits [23:16] Aff2 : Match Aff2 of target processor MPIDR
             *  Bits [15:8] Aff1 : Match Aff1 of target processor MPIDR
             *  Bits [7:0] Aff0 : Match Aff0 of target processor MPIDR
             *
             * For platforms implementing ARMv8 the format must be:
             *  Bits [63:40] Must be zero
             *  Bits [39:32] Aff3 : Match Aff3 of target processor MPIDR
             *  Bits [31:24] Must be zero
             *  Bits [23:16] Aff2 : Match Aff2 of target processor MPIDR
             *  Bits [15:8] Aff1 : Match Aff1 of target processor MPIDR
             *  Bits [7:0] Aff0 : Match Aff0 of target processor MPIDR
             */

/*
            printf("Found GENERIC_INTERRUPT: BaseAddress=0x%016"
                       PRIx64
                       ", ParkingVersion=0x%" PRIu32
                       ", ParkedAddress=0x%016" PRIx64
                       ", GicvBaseAddress=0x%016" PRIx64
                       ", GichBaseAddress=0x%016" PRIx64
                       ", GicrBaseAddress=0x%016" PRIx64
                       ", CpuInterfaceNumber=%" PRIu32 ", Uid=%" PRIu32
                       ", ArmMpidr =%" PRIu64 "\n",
                       gi->BaseAddress, gi->ParkingVersion, gi->ParkedAddress, gi->GicvBaseAddress, gi->GichBaseAddress,
                       gi->GicrBaseAddress, gi->CpuInterfaceNumber, gi->Uid, gi->ArmMpidr);

            */

            coreid_t barrelfish_id;
            if (my_hw_id == gi->Uid) {
                barrelfish_id = 0; // BSP core is 0
            }
            else {
                barrelfish_id = barrelfish_id_counter++;
            }

            /* TODO: figure out which facts you need */
            skb_add_fact("generic_interrupt(%"PRIu64",%"PRIu64",%"PRIu64",%"PRIu64",%d,%d).",
                         gi->BaseAddress, gi->GicvBaseAddress, gi->GichBaseAddress,
                         gi->ParkedAddress, gi->CpuInterfaceNumber, gi->Uid);

            if (gi->ParkingVersion) {
                /* parking */
                skb_add_fact("boot_driver_entry(%"PRIu64",%s).", gi->ArmMpidr,
                             "armBootParking");
            } else {
                /* psci */
                skb_add_fact("boot_driver_entry(%"PRIu64",%s).", gi->ArmMpidr,
                                             "armBootPSCI");
            }

            errval_t err = oct_set(HW_PROCESSOR_ARMV8_RECORD_FORMAT,
                                   barrelfish_id,
                                   gi->Flags & ACPI_MADT_ENABLED,
                                   barrelfish_id,
                                   gi->ArmMpidr,
                                   CURRENT_CPU_TYPE,
                                   gi->CpuInterfaceNumber,
                                   gi->Uid,
                                   gi->Flags,
                                   gi->ParkingVersion,
                                   gi->PerformanceInterrupt,
                                   gi->ParkedAddress,
                                   gi->BaseAddress,
                                   gi->GicvBaseAddress,
                                   gi->GichBaseAddress,
                                   gi->VgicInterrupt,
                                   gi->GicrBaseAddress,
                                   gi->ArmMpidr);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "failed to set record");
            }
            }
            break;
        case ACPI_MADT_TYPE_GENERIC_DISTRIBUTOR:
            {
            ACPI_MADT_GENERIC_DISTRIBUTOR *gd = (ACPI_MADT_GENERIC_DISTRIBUTOR *)sh;
            debug_printf("Found GENERIC DISTRIBUTOR: BaseAddress=0x%016"
                                   PRIx64 ", GicId=%" PRIu32 ", Version=%" PRIu8
                                   ", GlobalIrqBase=%" PRIu32 "\n", gd->BaseAddress,
                                   gd->GicId , gd->Version, gd->GlobalIrqBase);
            skb_add_fact("generic_distributor(%"PRIu64",%d,%d,%d).",
                         gd->BaseAddress, gd->GicId, gd->GlobalIrqBase, gd->Version);
            }



            break;
        case ACPI_MADT_TYPE_GENERIC_MSI_FRAME:
            {
            ACPI_MADT_GENERIC_MSI_FRAME *msi = (ACPI_MADT_GENERIC_MSI_FRAME *)sh;
            ACPI_DEBUG("Found local APIC GENERIC MSI FRAME: BaseAddress=0x%016"
                       PRIx64 ", MsiFrameId=%" PRIu32 "\n", msi->BaseAddress,
                       msi->MsiFrameId);
            skb_add_fact("generic_msi_frame(%"PRIu64",%d,%d,%d,%d).",
                         msi->BaseAddress, msi->MsiFrameId, msi->SpiBase,
                         msi->SpiBase, msi->Flags);
            }
            break;
        case ACPI_MADT_TYPE_GENERIC_REDISTRIBUTOR:
        {
            ACPI_MADT_GENERIC_REDISTRIBUTOR *grd = (ACPI_MADT_GENERIC_REDISTRIBUTOR *)sh;
            debug_printf("Found GENERIC REDISTRIBUTOR: BaseAddress=0x%016"
                        PRIx64 ", Length=%" PRIu32 "\n", grd->BaseAddress,
                        grd->Length);
            skb_add_fact("generic_redistributor(%"PRIu64",%d).",
                         grd->BaseAddress, grd->Length);
        }
            break;
        case ACPI_MADT_TYPE_GENERIC_TRANSLATOR:
        {
            ACPI_MADT_GENERIC_TRANSLATOR *gt = (ACPI_MADT_GENERIC_TRANSLATOR *)sh;
            ACPI_DEBUG("Found local APIC GENERIC TRANSLATOR: TranslationId=%"
                        PRIu32 ", BaseAddress=0x%016" PRIx64 "\n", gt->TranslationId,
                        gt->BaseAddress);
            skb_add_fact("generic_translator(%"PRIu64",%d).",
                         gt->BaseAddress, gt->TranslationId);
        }
            break;
        default:
            ACPI_DEBUG("Unknown subtable type %d\n", sh->Type);
            break;
        }
        assert(length);
        p += length;
    }


#if 0
    /* XXX: Quirk hack for QEMU
     * There is no override for the timer interrupt, although it appears as IRQ2.
     */
    if (strncmp(madt->Header.OemId, "QEMU", 4) == 0
        && interrupt_overrides[0] == 0) {

    }
#endif

    ACPI_DEBUG("DONE: MADT Element %p / %p\n", p, (void *)madt + madt->Header.Length);

    return 0;
}

errval_t enable_and_route_interrupt(int gsi, coreid_t dest, int vector)
{
    USER_PANIC("NYI!");
    return SYS_ERR_OK;
}


errval_t acpi_interrupts_arch_setup(void)
{
    return SYS_ERR_OK;
}
