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

#ifndef INCLUDE_HW_RECORDS_ARCH_H_
#define INCLUDE_HW_RECORDS_ARCH_H_

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


#endif /* INCLUDE_HW_RECORDS_ARCH_H_ */
