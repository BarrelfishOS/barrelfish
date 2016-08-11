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
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INCLUDE_ARCH_AARCH64_HW_RECORDS_ARCH_H_
#define INCLUDE_ARCH_AARCH64_HW_RECORDS_ARCH_H_

#include <hw_records.h>

#define HW_PROCESSOR_ARMV8_FIELDS "{ " HW_PROCESSOR_GENERIC_FIELDS ", " \
                                      "cpu_interface_number: %d, " \
                                      "uid: %d, " \
                                      "mailbox: %" PRIx64 ", " \
                                      "parkingversion: %d, " \
                                      "mpdir: %" PRIx64 " }"

#define HW_PROCESSOR_ARMV8_RECORD_FORMAT "hw.processor.%d " HW_PROCESSOR_ARMV8_FIELDS



#define HW_PROCESSOR_ARMV8_REGEX  "r'hw\\.processor\\.[0-9]+' { " \
                                      "cpu_interface_number: _, " \
                                      "uid: _, " \
                                      "hw_id: _," \
                                      "mailbox: _, " \
                                      "parkingversion: _, " \
                                      "mpdir: _, " \
                                      "enabled: 1, " \
                                      "barrelfish_id: _, " \
                                      "type: _ }"



#endif /* INCLUDE_ARCH_AARCH64_HW_RECORDS_ARCH_H_ */
