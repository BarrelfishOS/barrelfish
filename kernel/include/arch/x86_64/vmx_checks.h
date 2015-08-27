/**
 * \file
 * \brief Functions used in the rigorous checks that are performed (optionally) 
 *  before launching and/or resuming a VM-guest.
 */

/*
 * Copyright (c) 2014, University of Washington.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich. 
 * Attn: Systems Group.
 */

#ifndef VMX_CHECKS_H
#define VMX_CHECKS_H

#define INTR_TYPE_EXT_INTR 0
#define INTR_TYPE_NMI 2
#define INTR_TYPE_HW_EXCEP 3
#define INTR_TYPE_SW_INTR 4
#define INTR_TYPE_PRIV_SW_EXCEP 5
#define INTR_TYPE_SW_EXCEP 6
#define INTR_TYPE_OTHER 7

#define ACCESS_UNUSABLE (1 << 16)
#define SEL_TI (1 << 2)
#define SEL_RPL (0x3)
#define SEG_TYPE_ACCESSED (1 << 0)
#define SEG_TYPE_READABLE (1 << 1)
#define SEG_TYPE_CODE_SEGMENT (1 << 3)

static inline int seg_reg_usable(int access_rights)
{
    return !(access_rights & ACCESS_UNUSABLE);
}

static inline int seg_access_type(uint64_t access_rights)
{
    return ((access_rights >> 0) & 0xF);
}

static inline int seg_access_s(uint64_t access_rights)
{
    return ((access_rights >> 4) & 0x1);
}

static inline int seg_access_dpl(uint64_t access_rights)
{
    return ((access_rights >> 5) & 0x3);
}

static inline int seg_access_p(uint64_t access_rights)
{
    return ((access_rights >> 7) & 0x1);
}

static inline int seg_access_l(uint64_t access_rights)
{
    return ((access_rights >> 13) & 0x1);
}

static inline int seg_access_db(uint64_t access_rights)
{
    return ((access_rights >> 14) & 0x1);
}

static inline int seg_access_g(uint64_t access_rights)
{
    return ((access_rights >> 15) & 0x1);
}

static inline int ept_type(uint64_t eptp) 
{
    return ((eptp >> 0) & 0x7);
}

static inline int ept_page_walk_length(uint64_t eptp)
{
    return ((eptp >> 3) & 0x7);
}

static inline int ept_accessed_dirty_enable(uint64_t eptp)
{
    return ((eptp >> 6) & 0x1);
}

void check_guest_state_area(void);
void check_host_state_area(void);
void check_vmx_controls(void);

#endif // VMX_CHECKS_H
