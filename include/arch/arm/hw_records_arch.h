/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INCLUDE_ARCH_ARM_HW_RECORDS_ARCH_H_
#define INCLUDE_ARCH_ARM_HW_RECORDS_ARCH_H_

#include <hw_records.h>

#define HW_PROCESSOR_ARM_FIELDS "{ " HW_PROCESSOR_GENERIC_FIELDS " }"

#define HW_PROCESSOR_ARM_RECORD_FORMAT \
            "hw.processor.%d " HW_PROCESSOR_ARM_FIELDS

#define HW_PROCESSOR_ARM_REGEX  "r'hw\\.processor\\.[0-9]+' { " \
                                      "enabled: 1, " \
                                      "barrelfish_id: _, " \
                                      "type: _ }"

#endif /* INCLUDE_ARCH_ARM_HW_RECORDS_ARCH_H_ */
