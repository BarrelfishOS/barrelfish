/*
 * Copyright (c) 2015 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <cpuid_internal.h>

#include <dev/cpuid_amd_dev.h>
/*
 * ===============================================================================
 * helpers
 * ===============================================================================
 */

static uint16_t lookup_cache_assoc(uint8_t key)
{
    switch (key) {
        case cpuid_amd_cache_assoc_disabled:
            return 0;
        case cpuid_amd_cache_assoc_direct:
            return 1;
        case cpuid_amd_cache_assoc_2way:
            return 2;
        case cpuid_amd_cache_assoc_4way:
            return 4;
        case cpuid_amd_cache_assoc_8way:
            return 8;
        case cpuid_amd_cache_assoc_16way:
            return 16;
        case cpuid_amd_cache_assoc_32way:
            return 32;
        case cpuid_amd_cache_assoc_48way:
            return 48;
        case cpuid_amd_cache_assoc_64way:
            return 64;
        case cpuid_amd_cache_assoc_96way:
            return 96;
        case cpuid_amd_cache_assoc_128way :
            return 128;
        case cpuid_amd_cache_assoc_fully :
            return 0xff;
    }
    return 0;
}

/*
 * ===============================================================================
 * basic processor information
 * ===============================================================================
 */

static errval_t proc_name(char *buf, size_t len)
{
    // check if this operation is supported
    if (cpuid_g_max_input_extended < 4) {
        return CPUID_ERR_UNSUPPORTED_FUNCTION;
    }

    size_t written;

    struct cpuid_regs reg = CPUID_REGS_INITIAL(0x80000002, 0);
    cpuid_exec(&reg);
    written = snprintf(buf, len, "%s", (char *)&reg.eax);
    len -= written;
    buf += written;

    reg.eax = 0x80000003;
    reg.ecx = 0x0;
    cpuid_exec(&reg);
    written = snprintf(buf, len, "%s", (char *)&reg.eax);
    len -= written;
    buf += written;

    reg.eax = 0x80000004;
    reg.ecx = 0x0;
    cpuid_exec(&reg);
    written = snprintf(buf, len, "%s", (char *)&reg.eax);
    len -= written;
    buf += written;

    return SYS_ERR_OK;
}

static errval_t proc_family(struct cpuid_proc_family *family)
{
    if (cpuid_g_max_input_basic < 1) {
        return CPUID_ERR_UNSUPPORTED_FUNCTION;
    }

    struct cpuid_regs reg = CPUID_REGS_INITIAL(1, 0);
    cpuid_exec(&reg);

    cpuid_amd_family_t f = (cpuid_amd_family_t)&reg.eax;

    family->stepping = cpuid_amd_family_stepping_extract(f);
    family->family = cpuid_amd_family_family_extract(f);
    family->model = cpuid_amd_family_model_extract(f);
    // amd has no proc type
    family->type = 0x0;

    if (family->family == 0x0f) {
        uint16_t model = cpuid_amd_family_extmodel_extract(f);
        family->model += (model << 4);
    }

    /* if family is zero we have to consider the extended family id */
    if (family->family != 0x0f) {
        family->family += cpuid_amd_family_extfamily_extract(f);
    }

    return SYS_ERR_OK;
}

static uint32_t proc_max_input_basic(void)
{
    struct cpuid_regs reg  = CPUID_REGS_INITIAL(0, 0);
    cpuid_exec(&reg);

    return reg.eax;
}

static uint32_t proc_max_input_extended(void)
{
    struct cpuid_regs reg  = CPUID_REGS_INITIAL(0x80000000, 0);
    cpuid_exec(&reg);

    return reg.eax;
}

static errval_t frequency_info(struct cpuid_freqinfo *fi)
{
    return CPUID_ERR_UNSUPPORTED_FUNCTION;
}

/*
 * ===============================================================================
 * cache topology information
 * ===============================================================================
 */

static errval_t cache_info_alternate(struct cpuid_cacheinfo *ci, uint32_t idx)
{
    if (CPUID_EXTENDED_INPUT_MASK(cpuid_g_max_input_extended) < 0x6) {
        CPUID_PRINTF("amd_cache_info not supported. max fnct= %x\n",
                             cpuid_g_max_input_extended);
        return CPUID_ERR_UNSUPPORTED_FUNCTION;
    }

    if (idx >= 4) {
        return CPUID_ERR_INVALID_INDEX;
    }


    uint32_t fn = 0x80000006;
    if (idx < 2) {
        fn = 0x80000005;
    }
    struct cpuid_regs reg  = CPUID_REGS_INITIAL(fn, 0);
    cpuid_exec(&reg);

    switch(idx) {
        case 0:;
            cpuid_amd_l1_dcache_t l1d = (cpuid_amd_l1_dcache_t)&reg.ecx;
            ci->associativity = cpuid_amd_l1_dcache_assoc_extract(l1d);
            ci->size = cpuid_amd_l1_dcache_size_extract(l1d) * 1024;
            ci->linesize = cpuid_amd_l1_dcache_linesize_extract(l1d);
            ci->level = 1;
            ci->shared = 1;
            ci->type = CPUID_CACHE_TYPE_DATA;
            break;
        case 1:;
            cpuid_amd_l1_icache_t l1i = (cpuid_amd_l1_icache_t)&reg.edx;
            ci->associativity = cpuid_amd_l1_icache_assoc_extract(l1i);
            ci->size = cpuid_amd_l1_icache_size_extract(l1i) * 1024;
            ci->linesize = cpuid_amd_l1_icache_linesize_extract(l1i);
            ci->level = 1;
            /* TODO: get the number of cores sharing that cache */
            ci->shared = 1;
            ci->type = CPUID_CACHE_TYPE_INSTR;
            break;
        case 2:;
            cpuid_amd_l2_cache_t l2 = (cpuid_amd_l2_cache_t)&reg.ecx;
            ci->associativity = lookup_cache_assoc(cpuid_amd_l2_cache_assoc_extract(l2));
            assert(cpuid_amd_l2_cache_assoc_extract(l2));
            assert(ci->associativity);
            ci->size = cpuid_amd_l2_cache_size_extract(l2) * 1024;
            ci->linesize = cpuid_amd_l2_cache_linesize_extract(l2);
            ci->level = 2;
            /* TODO: get the number of cores sharing that cache */
            ci->shared = 1;
            ci->type = CPUID_CACHE_TYPE_UNIFIED;
            break;
        case 3:;
            /*
             * This provides the processor’s third level cache characteristics
             * shared by all cores
             */
            cpuid_amd_l3_cache_t l3 = (cpuid_amd_l3_cache_t)&reg.edx;
            ci->associativity = lookup_cache_assoc(cpuid_amd_l3_cache_assoc_extract(l3));
            ci->size = cpuid_amd_l3_cache_size_extract(l3) * 512 * 1024;
            ci->linesize = cpuid_amd_l3_cache_linesize_extract(l3);
            ci->level = 3;
            /* TODO: get the number of cores sharing that cache */
            ci->shared = 1;
            ci->type = CPUID_CACHE_TYPE_UNIFIED;
            break;
        default :
            return CPUID_ERR_INVALID_INDEX;
    }
    if (ci->size == 0 || ci->associativity == 0 || ci->linesize == 0) {
        /* no size, associativity indicates invalid / disabled cache */
        return CPUID_ERR_INVALID_INDEX;
    }

    ci->inclusive = 1;
    ci->sets = (ci->size / ci->linesize) / ci->associativity;
    /* TODO: get the number of cores sharing that cache */
    ci->shared = 1;
    ci->name = cpuid_cache_names[ci->level][ci->type];

    return SYS_ERR_OK;
}

static errval_t cache_info(struct cpuid_cacheinfo *ci, uint32_t idx)
{
    if (CPUID_EXTENDED_INPUT_MASK(cpuid_g_max_input_extended) < 0x1d) {
        return cache_info_alternate(ci, idx);
    }

    struct cpuid_regs reg = CPUID_REGS_INITIAL(0x8000001d, idx);
    cpuid_exec(&reg);

    cpuid_amd_cache_info_eax_t ci_a = (cpuid_amd_cache_info_eax_t) &reg.eax;
    cpuid_amd_cache_info_ebx_t ci_b = (cpuid_amd_cache_info_ebx_t) &reg.ebx;

    ci->level = cpuid_amd_cache_info_eax_level_extract(ci_a);

    switch (cpuid_amd_cache_info_eax_ctype_extract(ci_a)) {
        case cpuid_amd_cache_type_data :
            /* data cache */
            ci->type = CPUID_CACHE_TYPE_DATA;
            break;
        case cpuid_amd_cache_type_instr :
            /* instruction cache */
            ci->type = CPUID_CACHE_TYPE_INSTR;
            break;
        case cpuid_amd_cache_type_unified :
            ci->type = CPUID_CACHE_TYPE_UNIFIED;
            /* unified cache */
            break;
        default:
            /* no more cache */
            ci->type = CPUID_CACHE_TYPE_INVALID;
            return CPUID_ERR_INVALID_INDEX;
            break;
    }

    ci->name = cpuid_cache_names[ci->level][ci->type];
    ci->linesize = cpuid_amd_cache_info_ebx_cachelinesize_extract(ci_b)+1;

    /* the the number of sets */
    ci->sets = reg.ecx + 1;

    if (cpuid_amd_cache_info_eax_fullyassoc_extract(ci_a)) {
        ci->size = (size_t)ci->linesize * ci->sets;
        ci->associativity = 0xff;
    } else {
        ci->associativity = cpuid_amd_cache_info_ebx_assoc_extract(ci_b)+1;
        ci->size = (size_t)ci->linesize * ci->sets * ci->associativity;
    }

    ci->shared = cpuid_amd_cache_info_eax_num_sharing_extract(ci_a) +1;

    cpuid_amd_cache_info_edx_t ci_d = (cpuid_amd_cache_info_edx_t) &reg.edx;
    ci->inclusive = cpuid_amd_cache_info_edx_inclusive_extract(ci_d);

    return SYS_ERR_OK;
}

static uint16_t cache_line_size(void)
{
    if (cpuid_g_max_input_basic < 1) {
        return CPUID_ERR_UNSUPPORTED_FUNCTION;
    }

    struct cpuid_regs reg = CPUID_REGS_INITIAL(1, 0);
    cpuid_exec(&reg);

    cpuid_amd_miscinfo_t mi = (cpuid_amd_miscinfo_t)&reg.ebx;

    return cpuid_amd_miscinfo_cflush_sz_extract(mi) * 8;
}

/*
 * ===============================================================================
 * TLB information
 * ===============================================================================
 */

static errval_t tlb_info(struct cpuid_tlbinfo *ti, uint32_t idx)
{
    if (CPUID_EXTENDED_INPUT_MASK(cpuid_g_max_input_extended) < 0x5) {
        return CPUID_ERR_UNSUPPORTED_FUNCTION;
    }
    struct cpuid_regs reg = CPUID_REGS_INITIAL(0x80000005, 0);
    ti->pagesize = 0;
    if (idx < 4) {
        /* level 1 tlb 4k/2m pages */
        ti->level = 1;
        reg.eax = 0x80000005;
        cpuid_exec(&reg);
        cpuid_amd_tlb_l1_t tlb;
        if (idx < 2) {
            tlb = (cpuid_amd_tlb_l1_t)&reg.eax;
            ti->pagesize = LARGE_PAGE_SIZE;
        } else {
            tlb = (cpuid_amd_tlb_l1_t)&reg.ebx;
            ti->pagesize = BASE_PAGE_SIZE;
        }
        if (idx & 0x1) {
            ti->type = CPUID_CACHE_TYPE_DATA;
            ti->associativity = cpuid_amd_tlb_l1_dtlb_assoc_extract(tlb);
            ti->entries = cpuid_amd_tlb_l1_dtlb_sz_extract(tlb);
        } else {
            ti->type = CPUID_CACHE_TYPE_INSTR;
            ti->associativity = cpuid_amd_tlb_l1_itlb_assoc_extract(tlb);
            ti->entries = cpuid_amd_tlb_l1_itlb_sz_extract(tlb);
        }
        return SYS_ERR_OK;
    } else if (idx < 8) {
        /* level 2 tlb 4k/2m pages */
        if (CPUID_EXTENDED_INPUT_MASK(cpuid_g_max_input_extended) < 0x6) {
            return CPUID_ERR_UNSUPPORTED_FUNCTION;
        }
        idx -= 4;
        ti->level = 2;
        ti->pagesize = (idx < 2) ? LARGE_PAGE_SIZE : BASE_PAGE_SIZE;
        reg.eax = 0x80000006;

    } else if (idx < 12){
        /* huge pages */
        if (CPUID_EXTENDED_INPUT_MASK(cpuid_g_max_input_extended) < 0x19) {
            return CPUID_ERR_UNSUPPORTED_FUNCTION;
        }
        idx -= 8;
        if (idx < 2) {
            ti->level = 1;
        } else {
            ti->level = 2;
        }
        ti->pagesize = HUGE_PAGE_SIZE;
        reg.eax = 0x80000019;
    } else {
        return CPUID_ERR_INVALID_INDEX;
    }

    cpuid_exec(&reg);
    cpuid_amd_tlb_l1_t tlb2;
    if (idx < 2) {
        tlb2 = (cpuid_amd_tlb_l1_t)&reg.eax;
    } else {
        tlb2 = (cpuid_amd_tlb_l1_t)&reg.ebx;
    }

    if (idx & 0x1) {
        ti->type = CPUID_CACHE_TYPE_DATA;
        ti->associativity = cpuid_amd_tlb_l2_dtlb_assoc_extract(tlb2);
        ti->entries = cpuid_amd_tlb_l2_dtlb_sz_extract(tlb2);
    } else {
        ti->type = CPUID_CACHE_TYPE_INSTR;
        ti->associativity = cpuid_amd_tlb_l2_itlb_assoc_extract(tlb2);
        ti->entries = cpuid_amd_tlb_l2_itlb_sz_extract(tlb2);
    }
    ti->associativity = lookup_cache_assoc(ti->associativity);

    return SYS_ERR_OK;
}

/*
 * ===============================================================================
 * thread and topology information
 * ===============================================================================
 */

static errval_t thread_info(struct cpuid_threadinfo *ti)
{
    if (cpuid_g_max_input_basic < 1) {
        return CPUID_ERR_UNSUPPORTED_FUNCTION;
    }

    struct cpuid_regs reg  = CPUID_REGS_INITIAL(0x01, 0);
    cpuid_exec(&reg);

    cpuid_amd_miscinfo_t mi = (cpuid_amd_miscinfo_t)&reg.ebx;

    uint8_t local_apic_id = cpuid_amd_miscinfo_init_apicid_extract(mi);
    uint8_t logical_processors = cpuid_amd_miscinfo_max_log_proc_extract(mi);

    if (!((reg.edx >> 28) & 0x1)) {
        /* TODO: then the following is not valid */
        ti->core = 0;
        ti->hyperthread = 0;
        ti->package = local_apic_id;
        return SYS_ERR_OK;
    }

    if (CPUID_EXTENDED_INPUT_MASK(cpuid_g_max_input_extended) < 8) {
        return CPUID_ERR_UNSUPPORTED_FUNCTION;
    }

    reg.eax = 0x80000008;
    reg.ecx = 0x0;
    cpuid_exec(&reg);

    cpuid_amd_apicid_t ac = (cpuid_amd_apicid_t)&reg.ecx;

    uint8_t mnc;
    uint8_t ApicIdCoreIdSize = cpuid_amd_apicid_apic_sz_extract(ac);
    uint8_t nc = cpuid_amd_apicid_ncores_extract(ac);
    if (ApicIdCoreIdSize == 0) {
        // Used by legacy dual-core/single-core processors
        mnc = nc + 1;
    } else {
        // use ApicIdCoreIdSize[3:0] field
        mnc = 1 << ApicIdCoreIdSize;
    }

    //XXX: not sure about these calculations.
    //        uint8_t hyperthreads = logical_processors / nc;
    uint8_t ht = logical_processors / mnc;
    uint8_t ht_shift = cpuid_bits_needed(ht - 1);
    uint8_t ht_mask = (1 << ht_shift) - 1;
    uint8_t core_shift = cpuid_bits_needed(mnc - 1);
    uint8_t core_mask = (1 << core_shift) - 1;

    ti->core = (local_apic_id >> (ht_shift)) & core_mask;
    ti->package = local_apic_id >> (ht_shift + core_shift);
    ti->hyperthread = local_apic_id & ht_mask;

    return SYS_ERR_OK;
}

static errval_t topology_info(struct cpuid_topologyinfo *topo, uint8_t idx)
{
    return 1;
    return SYS_ERR_OK;
}

static errval_t feature_info(struct cpuid_featureinfo *fi)
{
    USER_PANIC("NYI");
    return SYS_ERR_OK;
}

static errval_t address_space_info(struct cpuid_adressspaceinfo *ai)
{
    if (CPUID_EXTENDED_INPUT_MASK(cpuid_g_max_input_extended) < 0x8) {
        return CPUID_ERR_UNSUPPORTED_FUNCTION;
    }

    struct cpuid_regs reg  = CPUID_REGS_INITIAL(0x80000008, 0);
    cpuid_exec(&reg);

    cpuid_amd_addrspace_t as = (cpuid_amd_addrspace_t)&reg.eax;

    ai->physical = cpuid_amd_addrspace_physical_extract(as);
    ai->virtual = cpuid_amd_addrspace_linear_extract(as);
    ai->guest_physical = cpuid_amd_addrspace_guest_extract(as);
    if (ai->guest_physical == 0) {
        ai->guest_physical = ai->physical;
    }

    return SYS_ERR_OK;
}

/*
 * ===============================================================================
 * backend initialization
 * ===============================================================================
 */

/**
 * \brief fills the vendor specific handler functions
 *
 * \param fn_tab  function pointer table to be filled
 */
void cpuid_amd_set_handlers(struct cpuid_functions *fn_tab)
{
    fn_tab->proc_name = proc_name;
    fn_tab->proc_family = proc_family;
    fn_tab->proc_max_input_basic = proc_max_input_basic;
    fn_tab->proc_max_input_extended = proc_max_input_extended;
    fn_tab->frequency_info = frequency_info;

    fn_tab->cache_info = cache_info;
    fn_tab->cache_line_size = cache_line_size;

    fn_tab->tlb_info = tlb_info;

    fn_tab->thread_info = thread_info;
    fn_tab->topology_info = topology_info;

    fn_tab->feature_info = feature_info;

    fn_tab->address_space_info = address_space_info;
}
