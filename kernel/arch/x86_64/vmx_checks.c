/**
 * \file
 * \brief Before entry into a VM-guest commences, the state of logical processor
 *  and VMCS are checked to ensure that the following will transpire successfully:
 * 
 *  - entry into the guest
 *  - VMX non-root operation after entry
 *  - exit from the guest  
 *
 * The guest-state area, host-state area, and VMX controls are checked using these
 * functions to guarantee that these tests pass.
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

#include <kernel.h>
#include <paging_kernel_arch.h>
#include <vmkit.h>
#include <vmx_checks.h>

#include <dev/ia32_dev.h>

static bool ia32e_guest, unrestricted_guest;

static inline bool is_within_pa_width(uint64_t addr) 
{
    uint64_t phys_addr_width_mask = pa_width_mask();
    return ((addr & ~phys_addr_width_mask) == 0);
}

static inline void check_guest_cr0(void)
{
    uint64_t pp_controls;
    errval_t err = vmread(VMX_EXEC_PRIM_PROC, &pp_controls);

    uint64_t cr0_fixed0 = ia32_vmx_cr0_fixed0_rd(NULL);
    uint64_t cr0_fixed1 = ia32_vmx_cr0_fixed1_rd(NULL);
    
    // CR0.NW and CR0.CD are not changed by VM entry, so are not checked
    cr0_fixed0 &= ~(CR0_NW | CR0_CD);
    if (unrestricted_guest) {
        bool sec_ctls_set = sec_ctls_used(pp_controls);
        assert(sec_ctls_set == true);
	cr0_fixed0 &= ~(CR0_PE | CR0_PG);
    } 

    uint64_t guest_cr0;
    err += vmread(VMX_GUEST_CR0, &guest_cr0);
    assert(err_is_ok(err));
    assert(((guest_cr0 | cr0_fixed0) & cr0_fixed1) == guest_cr0);

    if (guest_cr0 & CR0_PG) {
        assert(guest_cr0 & CR0_PE);
    }

    if (ia32e_guest) {
        assert(guest_cr0 & CR0_PG);
    }
}

static inline void check_guest_cr3(void)
{
    uint64_t guest_cr3;
    errval_t err = vmread(VMX_GUEST_CR3, &guest_cr3);
    assert(err_is_ok(err));
    bool within_pa_width = is_within_pa_width(guest_cr3);
    assert(within_pa_width);
}

static inline void check_guest_cr4(void)
{
    uint64_t guest_cr4;
    errval_t err = vmread(VMX_GUEST_CR4, &guest_cr4);
    assert(err_is_ok(err));
    uint32_t cr4_fixed0 = ia32_vmx_cr4_fixed0_rd(NULL);
    uint32_t cr4_fixed1 = ia32_vmx_cr4_fixed1_rd(NULL);
    assert(((guest_cr4 | cr4_fixed0) & cr4_fixed1) == guest_cr4);

    if (ia32e_guest) {
        assert(guest_cr4 & CR4_PAE);
    } else {
        assert((guest_cr4 & CR4_PCIDE) == 0);
    }
}

static inline void check_guest_efer(void)
{
    uint64_t entry_controls;
    errval_t err = vmread(VMX_ENTRY_CONTROLS, &entry_controls);

    if (entry_controls & ENTRY_CLTS_LOAD_EFER) {
        uint64_t guest_efer;
	err += vmread(VMX_GUEST_EFER_F, &guest_efer);

	// Bits reserved must be set to 0:
       
	// Bits 1:7
	assert(((guest_efer >> 1) & 0x7F) == 0);
	// Bit 9
	assert(((guest_efer >> 9) & 0x1) == 0);
	// Bits 12:63
	assert((guest_efer & ~0xFFF) == 0);
	
        uint64_t guest_cr0;
	err += vmread(VMX_GUEST_CR0, &guest_cr0);

	bool lma_set = !!(guest_efer & EFER_LMA); 
	assert(lma_set == ia32e_guest);
	
	if (guest_cr0 & CR0_PG) {
	    bool lme_set = !!(guest_efer & EFER_LME);
	    assert(lma_set == lme_set);
	}
    }
    assert(err_is_ok(err));
}

static void check_guest_control_registers(void)
{
    check_guest_cr0();
    check_guest_cr3();
    check_guest_cr4();    
}

static void check_guest_seg_sel(void)
{
    // TR
    uint64_t tr_sel;
    errval_t err = vmread(VMX_GUEST_TR_SEL, &tr_sel);
    assert((tr_sel & SEL_TI) == 0);

    // LDTR
    uint64_t ldtr_access_rights, ldtr_sel;
    err += vmread(VMX_GUEST_LDTR_ACCESS, &ldtr_access_rights);
    err += vmread(VMX_GUEST_LDTR_SEL, &ldtr_sel);
    if (seg_reg_usable(ldtr_access_rights)) {
        assert((ldtr_sel & SEL_TI) == 0);
    }
    
    // SS
    // The guest will not be in virtual-8086 mode
    if (!unrestricted_guest) {
        uint64_t ss_sel, cs_sel;
	err += vmread(VMX_GUEST_SS_SEL, &ss_sel);
	err += vmread(VMX_GUEST_CS_SEL, &cs_sel);
        int ss_rpl = (ss_sel & SEL_RPL);
	int cs_rpl = (cs_sel & SEL_RPL);
	assert(ss_rpl == cs_rpl);
    }
    assert(err_is_ok(err));
}

static void check_guest_seg_base(void)
{
    // TR, FS, GS
    uint64_t tr_base, fs_base, gs_base;
    errval_t err = vmread(VMX_GUEST_TR_BASE, &tr_base);
    err += vmread(VMX_GUEST_FS_BASE, &fs_base);
    err += vmread(VMX_GUEST_GS_BASE, &gs_base);
    bool tr_base_canonical = is_canonical(tr_base);
    assert(tr_base_canonical);

    bool fs_base_canonical = is_canonical(fs_base);
    assert(fs_base_canonical);

    bool gs_base_canonical = is_canonical(gs_base);
    assert(gs_base_canonical);

    // LDTR
    uint64_t ldtr_access_rights;
    err += vmread(VMX_GUEST_LDTR_ACCESS, &ldtr_access_rights);
    if (seg_reg_usable(ldtr_access_rights)) {
        uint64_t ldtr_base;
	err += vmread(VMX_GUEST_LDTR_BASE, &ldtr_base);
	bool ldtr_base_canonical = is_canonical(ldtr_base);
	assert(ldtr_base_canonical);
    }

    // CS
    uint64_t cs_base;
    err += vmread(VMX_GUEST_CS_BASE, &cs_base);
    assert((cs_base & ~0xFFFFFFFF) == 0);

    // SS, DS, ES
    uint64_t ss_access_rights, ds_access_rights, es_access_rights;
    err += vmread(VMX_GUEST_SS_ACCESS, &ss_access_rights);
    err += vmread(VMX_GUEST_DS_ACCESS, &ds_access_rights);
    err += vmread(VMX_GUEST_ES_ACCESS, &es_access_rights);

    if (seg_reg_usable(ss_access_rights)) {
        uint64_t ss_base;
	err += vmread(VMX_GUEST_SS_BASE, &ss_base);
	bool ss_base_canonical = is_canonical(ss_base);
	assert(ss_base_canonical);
    }

    if (seg_reg_usable(ds_access_rights)) {
        uint64_t ds_base;
	err += vmread(VMX_GUEST_DS_BASE, &ds_base);
	bool ds_base_canonical = is_canonical(ds_base);
	assert(ds_base_canonical);
    }

    if (seg_reg_usable(es_access_rights)) {
        uint64_t es_base;
	err += vmread(VMX_GUEST_ES_BASE, &es_base);
	bool es_base_canonical = is_canonical(es_base);
	assert(es_base_canonical);
    }
    assert(err_is_ok(err));
}

static void check_guest_seg_access_type(void) 
{
    // CS
    uint64_t cs_access_rights;
    errval_t err = vmread(VMX_GUEST_CS_ACCESS, &cs_access_rights);
    int cs_type = seg_access_type(cs_access_rights);
    if (unrestricted_guest) {
        assert(cs_type == 3 || cs_type == 9 || cs_type == 11 || 
	       cs_type == 13 || cs_type == 15);
    } else {
        assert(cs_type == 9 || cs_type == 11 || cs_type == 13 || 
	       cs_type == 15);
    }

    // SS
    uint64_t ss_access_rights;
    err += vmread(VMX_GUEST_SS_ACCESS, &ss_access_rights);
    if (seg_reg_usable(ss_access_rights)) {
        int ss_type = seg_access_type(ss_access_rights);
	assert(ss_type == 3 || ss_type == 7);
    }

    // DS
    uint64_t ds_access_rights;
    err += vmread(VMX_GUEST_DS_ACCESS, &ds_access_rights);
    if (seg_reg_usable(ds_access_rights)) {
        int ds_type = seg_access_type(ds_access_rights);
	assert(ds_type & SEG_TYPE_ACCESSED); 
	if (ds_type & SEG_TYPE_CODE_SEGMENT) {
	    assert(ds_type & SEG_TYPE_READABLE);
	}
    }

    // ES
    uint64_t es_access_rights;
    err += vmread(VMX_GUEST_ES_ACCESS, &es_access_rights);
    if (seg_reg_usable(es_access_rights)) {
        int es_type = seg_access_type(es_access_rights);
	assert(es_type & SEG_TYPE_ACCESSED); 
	if (es_type & SEG_TYPE_CODE_SEGMENT) {
	    assert(es_type & SEG_TYPE_READABLE);
	}
    }

    // FS
    uint64_t fs_access_rights;
    err += vmread(VMX_GUEST_FS_ACCESS, &fs_access_rights);
    if (seg_reg_usable(fs_access_rights)) {
        int fs_type = seg_access_type(fs_access_rights);
	assert(fs_type & SEG_TYPE_ACCESSED); 
	if (fs_type & SEG_TYPE_CODE_SEGMENT) {
	    assert(fs_type & SEG_TYPE_READABLE);
	}
    }

    // GS
    uint64_t gs_access_rights;
    err += vmread(VMX_GUEST_GS_ACCESS, &gs_access_rights);
    if (seg_reg_usable(gs_access_rights)) {
        int gs_type = seg_access_type(gs_access_rights);
	assert(gs_type & SEG_TYPE_ACCESSED); 
	if (gs_type & SEG_TYPE_CODE_SEGMENT) {
	    assert(gs_type & SEG_TYPE_READABLE);
	}
    }

    // TR
    uint64_t tr_access_rights;
    err += vmread(VMX_GUEST_TR_ACCESS, &tr_access_rights);
    int tr_type = seg_access_type(tr_access_rights);
    if (ia32e_guest) {
        assert(tr_type == 11);
    } else {
        assert(tr_type == 3 || tr_type == 11);
    }

    // LDTR
    uint64_t ldtr_access_rights;
    err += vmread(VMX_GUEST_LDTR_ACCESS, &ldtr_access_rights);
    if (seg_reg_usable(ldtr_access_rights)) {
        int ldtr_type = seg_access_type(ldtr_access_rights);    
        assert(ldtr_type == 2);
    }
    assert(err_is_ok(err));
}

static void check_guest_seg_access_s(void)
{
    // CS
    uint64_t cs_access_rights;
    errval_t err = vmread(VMX_GUEST_CS_ACCESS, &cs_access_rights);
    int cs_s = seg_access_s(cs_access_rights);
    assert(cs_s == 1);

    // SS
    uint64_t ss_access_rights;
    err += vmread(VMX_GUEST_SS_ACCESS, &ss_access_rights);
    if (seg_reg_usable(ss_access_rights)) {
        int ss_s = seg_access_s(ss_access_rights);
	assert(ss_s == 1);
    }

    // DS
    uint64_t ds_access_rights;
    err += vmread(VMX_GUEST_DS_ACCESS, &ds_access_rights);
    if (seg_reg_usable(ds_access_rights)) {
        int ds_s = seg_access_s(ds_access_rights);
	assert(ds_s == 1);
    }

    // ES
    uint64_t es_access_rights;
    err += vmread(VMX_GUEST_ES_ACCESS, &es_access_rights);
    if (seg_reg_usable(es_access_rights)) {
        int es_s = seg_access_s(es_access_rights);
	assert(es_s == 1);
    }

    // FS
    uint64_t fs_access_rights;
    err += vmread(VMX_GUEST_FS_ACCESS, &fs_access_rights);
    if (seg_reg_usable(fs_access_rights)) {
        int fs_s = seg_access_s(fs_access_rights);
	assert(fs_s == 1);
    }

    // GS
    uint64_t gs_access_rights;
    err += vmread(VMX_GUEST_GS_ACCESS, &gs_access_rights);
    if (seg_reg_usable(gs_access_rights)) {
        int gs_s = seg_access_s(gs_access_rights);
	assert(gs_s == 1);
    }
  
    // TR
    uint64_t tr_access_rights;
    err += vmread(VMX_GUEST_TR_ACCESS, &tr_access_rights);
    int tr_s = seg_access_s(tr_access_rights);
    assert(tr_s == 0);
   
    // LDTR
    uint64_t ldtr_access_rights;
    err += vmread(VMX_GUEST_LDTR_ACCESS, &ldtr_access_rights);
    if (seg_reg_usable(ldtr_access_rights)) {
	int ldtr_s = seg_access_s(ldtr_access_rights);
	assert(ldtr_s == 0);
    }
    assert(err_is_ok(err));
}

static void check_guest_seg_access_dpl(void)
{
    // CS
    uint64_t cs_access_rights;
    errval_t err = vmread(VMX_GUEST_CS_ACCESS, &cs_access_rights);
    int cs_dpl = seg_access_dpl(cs_access_rights);

    int cs_type = seg_access_type(cs_access_rights);
    if (cs_type == 3) {
	assert(unrestricted_guest);
	assert(cs_dpl == 0);
    }

    uint64_t ss_access_rights;
    err += vmread(VMX_GUEST_SS_ACCESS, &ss_access_rights);
    int ss_dpl = seg_access_dpl(ss_access_rights);

    if (cs_type == 9 || cs_type == 11) {
        assert(cs_dpl == ss_dpl);
    }

    if (cs_type == 13 || cs_type == 15) {
        assert(cs_dpl <= ss_dpl);
    }

    // SS
    if (!unrestricted_guest) {
        uint64_t ss_sel;
	err += vmread(VMX_GUEST_SS_SEL, &ss_sel);
	int ss_rpl = (ss_sel & SEL_RPL);
	assert(ss_rpl == ss_dpl);
    }

    uint64_t guest_cr0;
    err += vmread(VMX_GUEST_CR0, &guest_cr0);

    if (cs_type == 3 || (guest_cr0 & CR0_PE) == 0) {
        assert(ss_dpl == 0);
    }

    // DS
    uint64_t ds_access_rights;
    err += vmread(VMX_GUEST_DS_ACCESS, &ds_access_rights);
    bool ds_usable = seg_reg_usable(ds_access_rights);
    int ds_type = seg_access_type(ds_access_rights);

    if (!unrestricted_guest && ds_usable && (0 <= ds_type && ds_type <= 11)) {
        uint64_t ds_sel;
	err += vmread(VMX_GUEST_DS_SEL, &ds_sel);
	int ds_rpl = (ds_sel & SEL_RPL);
	int ds_dpl = seg_access_dpl(ds_access_rights);

	assert(ds_dpl >= ds_rpl);
    }

    // ES
    uint64_t es_access_rights;
    err += vmread(VMX_GUEST_DS_ACCESS, &es_access_rights);
    bool es_usable = seg_reg_usable(es_access_rights);
    int es_type = seg_access_type(es_access_rights);

    if (!unrestricted_guest && es_usable && (0 <= es_type && es_type <= 11)) {
        uint64_t es_sel;
	err += vmread(VMX_GUEST_ES_SEL, &es_sel);
	int es_rpl = (es_sel & SEL_RPL);
	int es_dpl = seg_access_dpl(es_access_rights);

	assert(es_dpl >= es_rpl);
    }

    // FS
    uint64_t fs_access_rights;
    err += vmread(VMX_GUEST_FS_ACCESS, &fs_access_rights);
    bool fs_usable = seg_reg_usable(fs_access_rights);
    int fs_type = seg_access_type(fs_access_rights);

    if (!unrestricted_guest && fs_usable && (0 <= fs_type && fs_type <= 11)) {
        uint64_t fs_sel;
	err += vmread(VMX_GUEST_FS_SEL, &fs_sel);
	int fs_rpl = (fs_sel & SEL_RPL);
	int fs_dpl = seg_access_dpl(fs_access_rights);

	assert(fs_dpl >= fs_rpl);
    }

    // GS
    uint64_t gs_access_rights;
    err += vmread(VMX_GUEST_GS_ACCESS, &gs_access_rights);
    bool gs_usable = seg_reg_usable(gs_access_rights);
    int gs_type = seg_access_type(gs_access_rights);

    if (!unrestricted_guest && gs_usable && (0 <= gs_type && gs_type <= 11)) {
        uint64_t gs_sel;
	err += vmread(VMX_GUEST_GS_SEL, &gs_sel);
	int gs_rpl = (gs_sel & SEL_RPL);
	int gs_dpl = seg_access_dpl(gs_access_rights);

	assert(gs_dpl >= gs_rpl);
    }
    assert(err_is_ok(err));
}

static void check_guest_seg_access_p(void)
{
    // CS
    uint64_t cs_access_rights;
    errval_t err = vmread(VMX_GUEST_CS_ACCESS, &cs_access_rights);
    int cs_p = seg_access_p(cs_access_rights);
    assert(cs_p == 1);
    
    // SS
    uint64_t ss_access_rights;
    err += vmread(VMX_GUEST_SS_ACCESS, &ss_access_rights);
    if (seg_reg_usable(ss_access_rights)) {
        int ss_p = seg_access_p(ss_access_rights);
	assert(ss_p == 1);
    }

    // DS
    uint64_t ds_access_rights;
    err += vmread(VMX_GUEST_DS_ACCESS, &ds_access_rights);
    if (seg_reg_usable(ds_access_rights)) {
        int ds_p = seg_access_p(ds_access_rights);
	assert(ds_p == 1);
    }

    // ES
    uint64_t es_access_rights;
    err += vmread(VMX_GUEST_ES_ACCESS, &es_access_rights);
    if (seg_reg_usable(es_access_rights)) {
        int es_p = seg_access_p(es_access_rights);
	assert(es_p == 1);
    }

    // FS
    uint64_t fs_access_rights;
    err += vmread(VMX_GUEST_FS_ACCESS, &fs_access_rights);
    if (seg_reg_usable(fs_access_rights)) {
        int fs_p = seg_access_p(fs_access_rights);
	assert(fs_p == 1);
    }
    
    // GS
    uint64_t gs_access_rights;
    err += vmread(VMX_GUEST_GS_ACCESS, &gs_access_rights);
    if (seg_reg_usable(gs_access_rights)) {
        int gs_p = seg_access_p(gs_access_rights);
	assert(gs_p == 1);
    }

    // TR
    uint64_t tr_access_rights;
    err += vmread(VMX_GUEST_TR_ACCESS, &tr_access_rights);
    int tr_p = seg_access_p(tr_access_rights);
    assert(tr_p == 1);

    // LDTR
    uint64_t ldtr_access_rights;
    err += vmread(VMX_GUEST_LDTR_ACCESS, &ldtr_access_rights);
    if (seg_reg_usable(ldtr_access_rights)) {  
	int ldtr_p = seg_access_p(ldtr_access_rights);
	assert(ldtr_p == 1);
    }
    assert(err_is_ok(err));
}

static void check_guest_seg_access_db(void)
{
    // CS
    uint64_t cs_access_rights;
    errval_t err = vmread(VMX_GUEST_CS_ACCESS, &cs_access_rights);
    assert(err_is_ok(err));
    int cs_l = seg_access_l(cs_access_rights);
    if (ia32e_guest && (cs_l == 1)) {
        int cs_db = seg_access_db(cs_access_rights);
	assert(cs_db == 0);
    }
}

static void check_guest_seg_access_g(void)
{
    // CS
    uint64_t cs_access_rights;
    errval_t err = vmread(VMX_GUEST_CS_ACCESS, &cs_access_rights);
    int cs_g = seg_access_g(cs_access_rights);
    
    uint64_t cs_lim;
    err += vmread(VMX_GUEST_CS_LIM, &cs_lim);
    // If there is at least one bit in the range 0:11 that is set to 0
    if ((cs_lim & 0xFFF) ^ 0xFFF) {
        assert(cs_g == 0);
    }
    // If there is at least one bit in the range 20:31 that is set to 1
    if ((cs_lim >> 20) & 0xFFF) {
        assert(cs_g == 1);
    }

    // SS
    uint64_t ss_access_rights;
    err += vmread(VMX_GUEST_SS_ACCESS, &ss_access_rights);
    if (seg_reg_usable(ss_access_rights)) {
        int ss_g = seg_access_g(ss_access_rights);
	uint64_t ss_lim;
	err += vmread(VMX_GUEST_SS_LIM, &ss_lim);
	// If there is at least one bit in the range 0:11 that is set to 0
	if ((ss_lim & 0xFFF) ^ 0xFFF) {
	    assert(ss_g == 0);
	}
	// If there is at least one bit in the range 20:31 that is set to 1
	if ((ss_lim >> 20) & 0xFFF) {
	    assert(ss_g == 1);
	}	
    }

    // DS
    uint64_t ds_access_rights;
    err += vmread(VMX_GUEST_DS_ACCESS, &ds_access_rights);
    if (seg_reg_usable(ds_access_rights)) {
        int ds_g = seg_access_g(ds_access_rights);
	uint64_t ds_lim;
	err += vmread(VMX_GUEST_DS_LIM, &ds_lim);
	// If there is at least one bit in the range 0:11 that is set to 0
	if ((ds_lim & 0xFFF) ^ 0xFFF) {
	    assert(ds_g == 0);
	}
	// If there is at least one bit in the range 20:31 that is set to 1
	if ((ds_lim >> 20) & 0xFFF) {
	    assert(ds_g == 1);
	}	
    }

    // ES
    uint64_t es_access_rights;
    err += vmread(VMX_GUEST_ES_ACCESS, &es_access_rights);
    if (seg_reg_usable(es_access_rights)) {
        int es_g = seg_access_g(es_access_rights);
	uint64_t es_lim;
	err += vmread(VMX_GUEST_ES_LIM, &es_lim);
	// If there is at least one bit in the range 0:11 that is set to 0
	if ((es_lim & 0xFFF) ^ 0xFFF) {
	    assert(es_g == 0);
	}
	// If there is at least one bit in the range 20:31 that is set to 1
	if ((es_lim >> 20) & 0xFFF) {
	    assert(es_g == 1);
	}	
    }

    // FS
    uint64_t fs_access_rights;
    err += vmread(VMX_GUEST_FS_ACCESS, &fs_access_rights);
    if (seg_reg_usable(fs_access_rights)) {
        int fs_g = seg_access_g(fs_access_rights);
	uint64_t fs_lim;
	err += vmread(VMX_GUEST_FS_LIM, &fs_lim);
	// If there is at least one bit in the range 0:11 that is set to 0
	if ((fs_lim & 0xFFF) ^ 0xFFF) {
	    assert(fs_g == 0);
	}
	// If there is at least one bit in the range 20:31 that is set to 1
	if ((fs_lim >> 20) & 0xFFF) {
	    assert(fs_g == 1);
	}	
    }
    
    // GS
    uint64_t gs_access_rights;
    err += vmread(VMX_GUEST_GS_ACCESS, &gs_access_rights);
    if (seg_reg_usable(gs_access_rights)) {
        int gs_g = seg_access_g(gs_access_rights);
	uint64_t gs_lim;
	err += vmread(VMX_GUEST_GS_LIM, &gs_lim);
	// If there is at least one bit in the range 0:11 that is set to 0
	if ((gs_lim & 0xFFF) ^ 0xFFF) {
	    assert(gs_g == 0);
	}
	// If there is at least one bit in the range 20:31 that is set to 1
	if ((gs_lim >> 20) & 0xFFF) {
	    assert(gs_g == 1);
	}	
    }

    // TR
    uint64_t tr_access_rights;
    err += vmread(VMX_GUEST_TR_ACCESS, &tr_access_rights);
    if (seg_reg_usable(tr_access_rights)) {
        int tr_g = seg_access_g(tr_access_rights);
	uint64_t tr_lim;
	err += vmread(VMX_GUEST_TR_LIM, &tr_lim);
	// If there is at least one bit in the range 0:11 that is set to 0
	if ((tr_lim & 0xFFF) ^ 0xFFF) {
	    assert(tr_g == 0);
	}
	// If there is at least one bit in the range 20:31 that is set to 1
	if ((tr_lim >> 20) & 0xFFF) {
	    assert(tr_g == 1);
	}	
    }

    // LDTR
    uint64_t ldtr_access_rights;
    err += vmread(VMX_GUEST_LDTR_ACCESS, &ldtr_access_rights);
    if (seg_reg_usable(ldtr_access_rights)) {
        int ldtr_g = seg_access_g(ldtr_access_rights);
	uint64_t ldtr_lim;
	err += vmread(VMX_GUEST_LDTR_LIM, &ldtr_lim);
	// If there is at least one bit in the range 0:11 that is set to 0
	if ((ldtr_lim & 0xFFF) ^ 0xFFF) {
	    assert(ldtr_g == 0);
	}
	// If there is at least one bit in the range 20:31 that is set to 1
	if ((ldtr_lim >> 20) & 0xFFF) {
	    assert(ldtr_g == 1);
	}	
    }
    assert(err_is_ok(err));
}

static void check_guest_seg_access_rsvd(void)
{
    // CS
    uint64_t cs_access_rights;
    errval_t err = vmread(VMX_GUEST_CS_ACCESS, &cs_access_rights);
    int cs_rsvd_low = ((cs_access_rights >> 8) & 0xF);
    int cs_rsvd_high = ((cs_access_rights >> 17) & 0x7FFF);
    assert(cs_rsvd_low == 0);
    assert(cs_rsvd_high == 0);

    // SS
    uint64_t ss_access_rights;
    err += vmread(VMX_GUEST_SS_ACCESS, &ss_access_rights);
    if (seg_reg_usable(ss_access_rights)) {
        int ss_rsvd_low = ((ss_access_rights >> 8) & 0xF);
	int ss_rsvd_high = ((ss_access_rights >> 17) & 0x7FFF);
	assert(ss_rsvd_low == 0);
	assert(ss_rsvd_high == 0);
    }

    // DS
    uint64_t ds_access_rights;
    err += vmread(VMX_GUEST_DS_ACCESS, &ds_access_rights);
    if (seg_reg_usable(ds_access_rights)) {
        int ds_rsvd_low = ((ds_access_rights >> 8) & 0xF);
	int ds_rsvd_high = ((ds_access_rights >> 17) & 0x7FFF);
	assert(ds_rsvd_low == 0);
	assert(ds_rsvd_high == 0);
    }

    // ES
    uint64_t es_access_rights;
    err += vmread(VMX_GUEST_ES_ACCESS, &es_access_rights);
    if (seg_reg_usable(es_access_rights)) {
        int es_rsvd_low = ((es_access_rights >> 8) & 0xF);
	int es_rsvd_high = ((es_access_rights >> 17) & 0x7FFF);
	assert(es_rsvd_low == 0);
	assert(es_rsvd_high == 0);
    }

    // FS
    uint64_t fs_access_rights;
    err += vmread(VMX_GUEST_FS_ACCESS, &fs_access_rights);
    if (seg_reg_usable(fs_access_rights)) {
        int fs_rsvd_low = ((fs_access_rights >> 8) & 0xF);
	int fs_rsvd_high = ((fs_access_rights >> 17) & 0x7FFF);
	assert(fs_rsvd_low == 0);
	assert(fs_rsvd_high == 0);
    }
    
    // GS
    uint64_t gs_access_rights;
    err += vmread(VMX_GUEST_GS_ACCESS, &gs_access_rights);
    if (seg_reg_usable(gs_access_rights)) {
        int gs_rsvd_low = ((gs_access_rights >> 8) & 0xF);
	int gs_rsvd_high = ((gs_access_rights >> 17) & 0x7FFF);
	assert(gs_rsvd_low == 0);
	assert(gs_rsvd_high == 0);
    }

    // TR
    uint64_t tr_access_rights;
    err += vmread(VMX_GUEST_TR_ACCESS, &tr_access_rights);
    int tr_rsvd_low = ((tr_access_rights >> 8) & 0xF);
    int tr_rsvd_high = ((tr_access_rights >> 17) & 0x7FFF);
    assert(tr_rsvd_low == 0);
    assert(tr_rsvd_high == 0);

    // LDTR
    uint64_t ldtr_access_rights;
    err += vmread(VMX_GUEST_LDTR_ACCESS, &ldtr_access_rights);
    if (seg_reg_usable(ldtr_access_rights)) {
	int ldtr_rsvd_low = ((ldtr_access_rights >> 8) & 0xF);
	int ldtr_rsvd_high = ((ldtr_access_rights >> 17) & 0x7FFF);
	assert(ldtr_rsvd_low == 0);
	assert(ldtr_rsvd_high == 0);
    }
    assert(err_is_ok(err));
}

// The guest will not be virtual-8086
static void check_guest_seg_access_rights(void)
{    
    check_guest_seg_access_type();
    check_guest_seg_access_s();
    check_guest_seg_access_dpl();
    check_guest_seg_access_p();
    check_guest_seg_access_db();
    check_guest_seg_access_g();
    check_guest_seg_access_rsvd();
}

static void check_guest_seg_regs(void)
{
    check_guest_seg_sel();
    check_guest_seg_base();
    check_guest_seg_access_rights();
}

static void check_guest_desc_table_regs(void)
{
    uint64_t gdtr_base, idtr_base;
    errval_t err = vmread(VMX_GUEST_GDTR_BASE, &gdtr_base);
    err += vmread(VMX_GUEST_IDTR_BASE, &idtr_base);
    bool gdtr_base_canonical = is_canonical(gdtr_base);
    assert(gdtr_base_canonical);

    bool idtr_base_canonical = is_canonical(idtr_base);
    assert(idtr_base_canonical);

    uint64_t gdtr_lim, idtr_lim;
    err += vmread(VMX_GUEST_GDTR_LIM, &gdtr_lim);
    err += vmread(VMX_GUEST_IDTR_LIM, &idtr_lim);
    assert(err_is_ok(err));
    assert((gdtr_lim & 0xFFFF0000) == 0);
    assert((idtr_lim & 0xFFFF0000) == 0);
}

static void check_guest_rip(void)
{
    uint64_t guest_rip;
    errval_t err = vmread(VMX_GUEST_RIP, &guest_rip);

    // Guest RIP
    uint64_t cs_access_rights;
    err += vmread(VMX_GUEST_CS_ACCESS, &cs_access_rights);
    int cs_l = seg_access_l(cs_access_rights);
    if ((ia32e_guest == false) || (cs_l == 0)) {
        assert((guest_rip & ~0xFFFFFFFF) == 0);
    } else {
      // bits N:63 must be identical, if the process supports N < 64 
      // linear-address bits
        assert((guest_rip & ~pa_width_mask()) == 0 ||
	       ~(guest_rip & ~pa_width_mask()) == pa_width_mask());
    }
    assert(err_is_ok(err));
}

static void check_guest_rflags(void)
{
    // Guest RFLAGS
    uint64_t guest_rflags;
    errval_t err = vmread(VMX_GUEST_RFLAGS, &guest_rflags);

    bool virtual_8086_guest = !!(guest_rflags & RFLAGS_VM);
    assert(virtual_8086_guest == false);
    
    // Reserved bits 22:63 must be zero
    assert((guest_rflags & ~0x3FFFFF) == 0); 
    
    // Bits 3, 5, and 15 must be zero
    assert((guest_rflags & (1 << 3UL)) == 0);
    assert((guest_rflags & (1 << 5UL)) == 0);
    assert((guest_rflags & (1 << 15UL)) == 0);
    
    // Reserved bit 1 must be 1
    assert(guest_rflags & (1 << 1UL));

    uint64_t guest_cr0;
    err += vmread(VMX_GUEST_CR0, &guest_cr0);
    if (ia32e_guest || ((guest_rflags & CR0_PE) == 0)) {
        assert((guest_rflags & RFLAGS_VM) == 0);
    }

    uint64_t intr_info;
    err += vmread(VMX_EXIT_INTR_INFO, &intr_info);
    bool intr_valid_bit = !!(intr_info & (1 << 31UL));
    int intr_type = ((intr_info >> 8) & 0x3);
    if (intr_valid_bit && intr_type == INTR_TYPE_EXT_INTR) {
        assert(guest_rflags & RFLAGS_IF);
    }
    assert(err_is_ok(err));
}

void check_guest_state_area(void)
{
    uint64_t entry_controls, sec_ctls;
    errval_t err = vmread(VMX_ENTRY_CONTROLS, &entry_controls);
    err += vmread(VMX_EXEC_SEC_PROC, &sec_ctls);

    ia32e_guest = !!(entry_controls & ENTRY_CLTS_IA32E_MODE);
    unrestricted_guest = !!(sec_ctls & SP_CLTS_UNRSTD_GUEST);

    check_guest_control_registers();

    uint64_t sysenter_esp, sysenter_eip;
    err += vmread(VMX_GUEST_SYSENTER_ESP, &sysenter_esp);
    err += vmread(VMX_GUEST_SYSENTER_EIP, &sysenter_eip);
    assert(err_is_ok(err));

    bool sysenter_esp_canonical = is_canonical(sysenter_esp);
    assert(sysenter_esp_canonical);
    bool sysenter_eip_canonical = is_canonical(sysenter_eip);
    assert(sysenter_eip_canonical);

    check_guest_efer();
    check_guest_seg_regs();
    check_guest_desc_table_regs();

    check_guest_rip();
    check_guest_rflags();
}

static void check_host_cr0(void)
{
    uint64_t host_cr0;
    errval_t err = vmread(VMX_HOST_CR0, &host_cr0);
    assert(err_is_ok(err));

    uint64_t cr0_fixed0 = ia32_vmx_cr0_fixed0_rd(NULL);
    uint64_t cr0_fixed1 = ia32_vmx_cr0_fixed1_rd(NULL);
    assert(((host_cr0 | cr0_fixed0) & cr0_fixed1) == host_cr0);
}

static void check_host_cr3(void)
{
    uint64_t host_cr3;
    errval_t err = vmread(VMX_HOST_CR3, &host_cr3);
    assert(err_is_ok(err));

    bool within_pa_width = is_within_pa_width(host_cr3);
    assert(within_pa_width);
}

static void check_host_cr4(void)
{
    uint64_t exit_controls;
    errval_t err = vmread(VMX_EXIT_CONTROLS, &exit_controls);

    uint64_t host_cr4;
    err += vmread(VMX_HOST_CR4, &host_cr4);
    assert(err_is_ok(err));

    uint32_t cr4_fixed0 = ia32_vmx_cr4_fixed0_rd(NULL);
    uint32_t cr4_fixed1 = ia32_vmx_cr4_fixed1_rd(NULL);
    assert(((host_cr4 | cr4_fixed0) & cr4_fixed1) == host_cr4);

    bool host_size_set = !!(exit_controls & EXIT_CLTS_HOST_SIZE);	        
    if (host_size_set) {
        assert(host_cr4 & CR4_PAE);
    } else {
        assert((host_cr4 & CR4_PCIDE) == 0);
    }
}

static void check_host_control_registers(void)
{
    check_host_cr0();
    check_host_cr3();
    check_host_cr4();    
}

static void check_host_efer(void)
{
    uint64_t exit_controls;
    errval_t err = vmread(VMX_EXIT_CONTROLS, &exit_controls);
    if (exit_controls & EXIT_CLTS_LOAD_EFER) {
        uint64_t host_efer;
	err += vmread(VMX_HOST_EFER_F, &host_efer);
	// Bits reserved must be set to 0:

	// Bits 1:7
	assert(((host_efer >> 1) & 0x7F) == 0);
	// Bit 9
	assert(((host_efer >> 9) & 0x1) == 0);
	// Bits 12:63
	assert((host_efer & ~0xFFF) == 0);

	bool host_size_set = !!(exit_controls & EXIT_CLTS_HOST_SIZE);	        
	bool lma_set = !!(host_efer & EFER_LMA); 
	assert(lma_set == host_size_set);

	bool lme_set = !!(host_efer & EFER_LME); 
	assert(lme_set == host_size_set);
    }
    assert(err_is_ok(err));
}

static void check_host_seg_sel(void)
{  
    // CS
    uint64_t cs_sel;
    errval_t err = vmread(VMX_HOST_CS_SEL, &cs_sel);
    int cs_rpl = (cs_sel & SEL_RPL);
    int cs_ti = (cs_sel & SEL_TI);
    assert(cs_rpl == 0);
    assert(cs_ti == 0);
    assert(cs_sel != 0);

    // SS
    uint64_t ss_sel;
    err += vmread(VMX_HOST_SS_SEL, &ss_sel);
    int ss_rpl = (ss_sel & SEL_RPL);
    int ss_ti = (ss_sel & SEL_TI);
    assert(ss_rpl == 0);
    assert(ss_ti == 0);

    uint64_t exit_controls;
    err += vmread(VMX_EXIT_CONTROLS, &exit_controls);
    bool host_size_set = !!(exit_controls & EXIT_CLTS_HOST_SIZE);	        
    if (!host_size_set) {
        assert(ss_sel != 0);
    }
    
    // DS
    uint64_t ds_sel;
    err += vmread(VMX_HOST_DS_SEL, &ds_sel);
    int ds_rpl = (ds_sel & SEL_RPL);
    int ds_ti = (ds_sel & SEL_TI);
    assert(ds_rpl == 0);
    assert(ds_ti == 0);

    // ES
    uint64_t es_sel;
    err += vmread(VMX_HOST_ES_SEL, &es_sel);
    int es_rpl = (es_sel & SEL_RPL);
    int es_ti = (es_sel & SEL_TI);
    assert(es_rpl == 0);
    assert(es_ti == 0);

    // FS
    uint64_t fs_sel;
    err += vmread(VMX_HOST_FS_SEL, &fs_sel);
    int fs_rpl = (fs_sel & SEL_RPL);
    int fs_ti = (fs_sel & SEL_TI);
    assert(fs_rpl == 0);
    assert(fs_ti == 0);

    // GS
    uint64_t gs_sel;
    err += vmread(VMX_HOST_GS_SEL, &gs_sel);
    int gs_rpl = (gs_sel & SEL_RPL);
    int gs_ti = (gs_sel & SEL_TI);
    assert(gs_rpl == 0);
    assert(gs_ti == 0);

    // TR
    uint64_t tr_sel;
    err += vmread(VMX_HOST_TR_SEL, &tr_sel);
    assert(err_is_ok(err));

    int tr_rpl = (tr_sel & SEL_RPL);
    int tr_ti = (tr_sel & SEL_TI);
    assert(tr_rpl == 0);
    assert(tr_ti == 0);
    assert(tr_sel != 0);
}

static void check_host_seg_base(void)
{
    uint64_t fs_base;
    errval_t err = vmread(VMX_HOST_FS_BASE, &fs_base);
    bool fs_base_canonical = is_canonical(fs_base);
    assert(fs_base_canonical);

    uint64_t gs_base;
    err += vmread(VMX_GUEST_GS_BASE, &gs_base);
    bool gs_base_canonical = is_canonical(gs_base);
    assert(gs_base_canonical);

    uint64_t tr_base;
    err += vmread(VMX_GUEST_TR_BASE, &tr_base);
    assert(err_is_ok(err));

    bool tr_base_canonical = is_canonical(tr_base);
    assert(tr_base_canonical);
}

static void check_host_seg_regs(void)
{
    check_host_seg_sel();
    check_host_seg_base();
}

static void check_host_desc_table_regs(void)
{
    uint64_t idtr_base;
    errval_t err = vmread(VMX_GUEST_IDTR_BASE, &idtr_base);
    bool idtr_base_canonical = is_canonical(idtr_base);
    assert(idtr_base_canonical);

    uint64_t gdtr_base;
    err += vmread(VMX_GUEST_GDTR_BASE, &gdtr_base);
    assert(err_is_ok(err));

    bool gdtr_base_canonical = is_canonical(gdtr_base);
    assert(gdtr_base_canonical);
}

static void check_host_rip(void)
{
    uint64_t host_rip;
    errval_t err = vmread(VMX_HOST_RIP, &host_rip);

    uint64_t exit_controls;
    err += vmread(VMX_EXIT_CONTROLS, &exit_controls);
    assert(err_is_ok(err));

    bool host_size_set = !!(exit_controls & EXIT_CLTS_HOST_SIZE);	        
    if (host_size_set) {
        bool rip_canonical = is_canonical(host_rip);
	assert(rip_canonical);
    } else {
        assert((host_rip & ~0xFFFFFFFF) == 0);
    }
}

void check_host_state_area(void)
{
    check_host_control_registers();

    uint64_t sysenter_esp, sysenter_eip;
    errval_t err = vmread(VMX_HOST_SYSENTER_ESP, &sysenter_esp);
    err += vmread(VMX_HOST_SYSENTER_EIP, &sysenter_eip);
    assert(err_is_ok(err));

    bool sysenter_esp_canonical = is_canonical(sysenter_esp);
    assert(sysenter_esp_canonical);
    bool sysenter_eip_canonical = is_canonical(sysenter_eip);
    assert(sysenter_eip_canonical);

    check_host_efer();
    check_host_seg_regs();
    check_host_desc_table_regs();

    check_host_rip();
}

// Checks pertaining to the "Enable EPT" and "EPT-violation #VE" controls
static void check_vmx_controls_ept(void)
{
    bool within_pa_width;
    uint64_t pp_controls, sp_controls;
    errval_t err = vmread(VMX_EXEC_PRIM_PROC, &pp_controls);
    err += vmread(VMX_EXEC_SEC_PROC, &sp_controls);

    if (unrestricted_guest) {
        assert(sp_controls & SP_CLTS_ENABLE_EPT);
    }

    if (sp_controls & SP_CLTS_ENABLE_EPT) {
        assert(pp_controls & PP_CLTS_SEC_CTLS);

	uint64_t eptp;
	err += vmread(VMX_EPTP_F, &eptp);

	ia32_vmx_ept_vpid_t ept_vpid_msr = ia32_vmx_ept_vpid_rd(NULL); 
	int set_ept_type = ept_type(eptp);
	
	switch(set_ept_type) {
	case 0x0: // Uncacheable
	    assert(ia32_vmx_ept_vpid_ucmt_extract(ept_vpid_msr));
	    break;
	case 0x4: // Execute-only
	    assert(ia32_vmx_ept_vpid_eot_extract(ept_vpid_msr));
	    break;
	case 0x6: // Write-back
	    assert(ia32_vmx_ept_vpid_wbmt_extract(ept_vpid_msr));
	    break;
	default:
	    assert(!"EPT type value is reserved");
	}

	assert(ia32_vmx_ept_vpid_pwl4_extract(ept_vpid_msr));
	assert(ept_page_walk_length(eptp) == 3);

	assert(((eptp >> 7) & 0x1F) == 0);
        within_pa_width = is_within_pa_width(eptp);
	assert(within_pa_width);

	bool accessed_dirty = ia32_vmx_ept_vpid_ept_adf_extract(ept_vpid_msr);
	if (!accessed_dirty) {
	    assert(ept_accessed_dirty_enable(eptp) == 0);
	}
    }

    if (sp_controls & SP_CLTS_EPT_VIOL) {
        assert(pp_controls & PP_CLTS_SEC_CTLS);
	uint64_t vexcp_info_addr;
	err += vmread(VMX_VEXCP_INFO_F, &vexcp_info_addr);
	assert((vexcp_info_addr & BASE_PAGE_MASK) == 0);
	within_pa_width = is_within_pa_width(vexcp_info_addr);
	assert(within_pa_width); 
    }
    assert(err_is_ok(err));
}

// Checks related to address-space size
static void check_vmx_controls_addr_space(void)
{
    uint64_t exit_controls;
    errval_t err = vmread(VMX_EXIT_CONTROLS, &exit_controls);

    bool host_size_set = !!(exit_controls & EXIT_CLTS_HOST_SIZE);	        
    
    uint64_t host_efer;
    err += vmread(VMX_HOST_EFER_F, &host_efer);
    assert(err_is_ok(err));

    if (host_efer & EFER_LMA) {
        assert(host_size_set);
    } else {
        assert(!ia32e_guest);
	assert(!host_size_set);
    }

    if (!host_size_set) {
        assert(!ia32e_guest);
    }
}

void check_vmx_controls(void)
{
    uint64_t msr_bitmap_addr;
    errval_t err = vmread(VMX_MSRBMP_F, &msr_bitmap_addr); 
    assert((msr_bitmap_addr & BASE_PAGE_MASK) == 0);
    bool within_pa_width = is_within_pa_width(msr_bitmap_addr);
    assert(within_pa_width);

    uint64_t io_bitmap_a_addr, io_bitmap_b_addr;
    err += vmread(VMX_IOBMP_A_F, &io_bitmap_a_addr);
    assert((io_bitmap_a_addr & BASE_PAGE_MASK) == 0);
    within_pa_width = is_within_pa_width(io_bitmap_a_addr);
    assert(within_pa_width);

    err += vmread(VMX_IOBMP_B_F, &io_bitmap_b_addr);
    assert((io_bitmap_b_addr & BASE_PAGE_MASK) == 0);
    within_pa_width = is_within_pa_width(io_bitmap_b_addr);
    assert(within_pa_width);

    uint64_t pin_controls, pp_controls, sp_controls;
    err += vmread(VMX_EXEC_PIN_BASED, &pin_controls);
    err += vmread(VMX_EXEC_PRIM_PROC, &pp_controls);
    err += vmread(VMX_EXEC_SEC_PROC, &sp_controls);

    uint64_t exit_controls;
    err += vmread(VMX_EXIT_CONTROLS, &exit_controls);

    if (pp_controls & PP_CLTS_TPR_SHADOW) {
        uint64_t vapic_addr;
	err += vmread(VMX_VAPIC_F, &vapic_addr);
	assert((vapic_addr & BASE_PAGE_MASK) == 0);
	within_pa_width = is_within_pa_width(vapic_addr);
	assert(within_pa_width);      

	uint64_t tpr_threshold;
	err += vmread(VMX_TPR_THRESHOLD, &tpr_threshold);
        if((sp_controls & SP_CLTS_VIRQ_DEL) == 0) {	  	
	    // Bits 4:31 must be 0
	    assert((tpr_threshold & 0xFFFFFFF0) == 0);
	}

	if ((sp_controls & SP_CLTS_VIRQ_DEL) == 0 && 
	    (sp_controls & SP_CLTS_VIRT_APIC) == 0) {
	    lvaddr_t vapic_page = local_phys_to_mem((lpaddr_t)vapic_addr);
	    assert(vapic_page != 0);
	    // virtual task-priority register is at offset 0x80
	    lvaddr_t vtpr_addr = vapic_page + 0x80;
	    uint32_t vtpr = *((uint32_t *)vtpr_addr);
	    assert((tpr_threshold & 0xF) <= ((vtpr >> 4) & 0xF));
	}
    } else {
        assert((sp_controls & SP_CLTS_VIRT_X2APIC) == 0);
	assert((sp_controls & SP_CLTS_VIRT_APIC_REG) == 0);
	assert((sp_controls & SP_CLTS_VIRQ_DEL) == 0);
    }

    if ((pin_controls & PIN_CTLS_NMI) == 0) {
        assert((pin_controls & PIN_CTLS_VIRT_NMI) == 0);
    }

    if ((pin_controls & PIN_CTLS_VIRT_NMI) == 0) {
        assert((pp_controls & PP_CLTS_NMI_WINDOW) == 0);
    }

    if (sp_controls & SP_CLTS_VIRT_APIC) {
        assert(pp_controls & PP_CLTS_SEC_CTLS);
	uint64_t apic_access_addr;
	err += vmread(VMX_APIC_ACC_F, &apic_access_addr);
	assert((apic_access_addr & BASE_PAGE_MASK) == 0);
	within_pa_width = is_within_pa_width(apic_access_addr);
	assert(within_pa_width);
    }

    if (sp_controls & SP_CLTS_VIRT_X2APIC) {
        assert(pp_controls & PP_CLTS_SEC_CTLS);
	assert((sp_controls & SP_CLTS_VIRT_APIC) == 0);
    }

    if (sp_controls & SP_CLTS_VIRQ_DEL) {
        assert(pp_controls & PP_CLTS_SEC_CTLS);
        assert(pin_controls & PIN_CTLS_EXT_INTR);
    }

    if (pin_controls & PIN_CTLS_POSTED_INTR) {
        assert(sp_controls & SP_CLTS_VIRQ_DEL);
	assert(exit_controls & EXIT_CLTS_ACK_INTR);
	
	uint64_t pinv;
	err += vmread(VMX_PINV, &pinv);
	// The posted-interrupt notification vector must be in the 
	// range 0-255
	assert((pinv & 0xFF00) == 0);

	uint64_t pid_addr;
	err += vmread(VMX_PID_F, &pid_addr);
	assert((pid_addr & 0x3F) == 0);
	within_pa_width = is_within_pa_width(pid_addr);
	assert(within_pa_width);
    }

    if (sp_controls & SP_CLTS_ENABLE_VPID) {
        assert(pp_controls & PP_CLTS_SEC_CTLS);
	
	uint64_t vpid;
	err += vmread(VMX_VPID, &vpid);
	assert(vpid != 0);
    }

    if (sp_controls & SP_CLTS_VMCS_SHADOW) {
        assert(pp_controls & PP_CLTS_SEC_CTLS);
	uint64_t vmread_bitmap_addr, vmwrite_bitmap_addr;
	err += vmread(VMX_VMREAD_BMP_F, &vmread_bitmap_addr);
	err += vmread(VMX_VMWRITE_BMP_F, &vmwrite_bitmap_addr);

	assert((vmread_bitmap_addr & BASE_PAGE_MASK) == 0);
	within_pa_width = is_within_pa_width(vmread_bitmap_addr);
	assert(within_pa_width); 

	assert((vmwrite_bitmap_addr & BASE_PAGE_MASK) == 0);
	within_pa_width = is_within_pa_width(vmwrite_bitmap_addr);
	assert(within_pa_width); 	
    }
    assert(err_is_ok(err));

    check_vmx_controls_ept();
    check_vmx_controls_addr_space();
}
