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

#ifndef INCLUDE_ARCH_X86_HW_RECORDS_ARCH_H_
#define INCLUDE_ARCH_X86_HW_RECORDS_ARCH_H_

#include <hw_records.h>

#define HW_PROCESSOR_X86_FIELDS "{ " HW_PROCESSOR_GENERIC_FIELDS ", " \
                                     "processor_id: %d, " \
                                     "apic_id: %d }"

#define HW_PROCESSOR_X86_RECORD_FORMAT "hw.processor.%d " HW_PROCESSOR_X86_FIELDS



#define HW_PROCESSOR_X86_REGEX  "r'hw\\.processor\\.[0-9]+' { " \
                                      "processor_id: _, " \
                                      "apic_id: _, " \
                                      "enabled: 1, " \
                                      "barrelfish_id: _, " \
                                      "type: _ }"



#endif /* INCLUDE_ARCH_X86_HW_RECORDS_ARCH_H_ */
