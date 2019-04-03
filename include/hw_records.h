/**
 * \file hw.h
 * \brief 
 */

/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INCLUDE_HW_RECORDS_ARCH_H_
#define INCLUDE_HW_RECORDS_ARCH_H_

#include <barrelfish_kpi/types.h>

/*
 * ===========================================================================
 * Generic Processor Record
 * ===========================================================================
 *
 * @enabled         flag whether this processor is enabled
 * @barrelfish_id   barrelfish ID of this processor
 * @hw_id           hardware assigned ID of this processor
 * @type            processor type, represents enum cpu_type
 */

/**
 * @brief the generic fields of a processor record
 */
#define HW_PROCESSOR_GENERIC_FIELDS "enabled: %d, " \
                                    "barrelfish_id: %d, " \
                                    "hw_id: %d, " \
                                    "type: %d"

/**
 * @brief regular expression to match for new generic processors
 */
#define HW_PROCESSOR_GENERIC_REGEX "r'hw\\.processor\\.[0-9]+' { " \
                                      "enabled: 1, " \
                                      "barrelfish_id: _, " \
                                      "hw_id: _, " \
                                      "type: _ }"

/*
* ===========================================================================
* PCI Root Bridge Records
* ===========================================================================
*/
#define HW_PCI_ROOTBRIDGE_RECORD_FIELDS \
"bus: %d, device: %d, function: %d, maxbus: %d, acpi_node: '%s'"

#define HW_PCI_ROOTBRIDGE_RECORD_FORMAT \
"hw.pci.rootbridge. { " HW_PCI_ROOTBRIDGE_RECORD_FIELDS " }";

#define HW_PCI_ROOTBRIDGE_RECORD_REGEX \
"r'hw\\.pci\\.rootbridge\\.[0-9]+' { bus: _, device: _, function: _," \
" maxbus: _, acpi_node: _ }"


/*
* ===========================================================================
* IOMMU hardware records
* ===========================================================================
*/

typedef enum {
HW_PCI_IOMMU_UNKNOWN    = 0,
HW_PCI_IOMMU_INTEL      = 1,
HW_PCI_IOMMU_AMD        = 2,
HW_PCI_IOMMU_ARM        = 3,
HW_PCI_IOMMU_DMAR_FAIL  = 4
} hw_pci_iommu_t;

#define HW_PCI_IOMMU_RECORD_FIELDS \
"index: %d, type: %d, flags: %d, segment: %d, address: %" PRIu64

#define HW_PCI_IOMMU_RECORD_FIELDS_READ \
"index: %d, type: %d, flags: %d, segment: %d, address: %d"

#define HW_PCI_IOMMU_RECORD_FORMAT \
"hw.pci.iommu. { " HW_PCI_IOMMU_RECORD_FIELDS " }"

#define HW_PCI_IOMMU_RECORD_REGEX \
"r'hw\\.pci\\.iommu\\.[0-9]+' { index: _, type: _, flags: _, segment: _, address: _ }"

#define SKB_SCHEMA_IOMMU_DEVICE \
"iommu_device(%" PRIu32 ", %" PRIu32 ", %" PRIu8 ", %" PRIu8 ", "\
              "addr(%" PRIu16 ", %" PRIu8 ", %" PRIu8 ", %" PRIu8 "), "\
              "%" PRIu8  ")."

#define SKB_SCHEMA_DMAR_DEV \
"dmar_dev(%" SCNu32 ", %" SCNu8 ", %" SCNu8 ", "\
            "addr(%" SCNu16 ", %" SCNu8 ", %" SCNu8 ", %" SCNu8 "), "\
            "%" SCNu8  ")"

/*
* ===========================================================================
* HPET hardware records
* ===========================================================================
*/
#define HW_HPET_RECORD_FIELDS \
"address: %" PRIu64 ", uid:%" PRIu64

#define HW_HPET_RECORD_FIELDS_READ \
"address:%d, uid:%d" 

#define HW_HPET_RECORD_FORMAT \
"hw.hpet.{" HW_HPET_RECORD_FIELDS " }"

#define HW_HPET_RECORD_REGEX \
"r'hw\\.hpet\\.[0-9]+' { address: _, uid: _}"

#define HW_HPET_COMP_RECORD_FIELDS \
"hpet_uid:%" PRIu64 ", index:%" PRIu64

#define HW_HPET_COMP_RECORD_FIELDS_READ \
"hpet_uid: %d, index:%d" 

#define HW_HPET_COMP_RECORD_FORMAT \
"hw.hpet_comp.{" HW_HPET_COMP_RECORD_FIELDS " }"

#define HW_HPET_COMP_RECORD_REGEX \
"r'hw\\.hpet_comp\\.[0-9]+' { hpet_uid: _, index: _}"


#endif /* INCLUDE_HW_RECORDS_ARCH_H_ */
