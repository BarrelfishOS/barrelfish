/*
 * Copyright (c) 2015 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <cpuid_internal.h>

#include <dev/cpuid_intel_dev.h>

static uint8_t cache_info_in_leaf_4 = 0;

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

    cpuid_intel_family_t f = (cpuid_intel_family_t)&reg.eax;

    family->stepping = cpuid_intel_family_stepping_extract(f);
    family->family = cpuid_intel_family_family_extract(f);
    family->model = cpuid_intel_family_model_extract(f);
    family->type = cpuid_intel_family_proctype_extract(f);

    if (family->family == 0x06 || family->family == 0x0f) {
        uint16_t model = cpuid_intel_family_extmodel_extract(f);
        family->model += (model << 4);
    }

    /* if family is zero we have to consider the extended family id */
    if (family->family != 0x0f) {
        family->family += cpuid_intel_family_extfamily_extract(f);
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
    if (cpuid_g_max_input_basic < 0x16) {
        return CPUID_ERR_UNSUPPORTED_FUNCTION;
    }

    struct cpuid_regs reg = CPUID_REGS_INITIAL(0x16, 0);
    cpuid_exec(&reg);

    cpuid_intel_frequency_t fq = (cpuid_intel_frequency_t)&reg.eax;
    fi->base =  cpuid_intel_frequency_mhz_extract(fq);

    fq = (cpuid_intel_frequency_t)&reg.ebx;
    fi->max =  cpuid_intel_frequency_mhz_extract(fq);

    fq = (cpuid_intel_frequency_t)&reg.ecx;
    fi->bus =  cpuid_intel_frequency_mhz_extract(fq);

    return SYS_ERR_OK;
}

/*
 * ===============================================================================
 * cache topology information
 * ===============================================================================
 */

#define CPUID_FILL_TLB(_val,_tci, _assoc, _entries, _level, _type, _pgsz) \
                case _val: \
                    _tci->is_tlb = 1; \
                    _tci->ti.associativity = _assoc; \
                    _tci->ti.entries = _entries; \
                    _tci->ti.level = _level; \
                    _tci->ti.type = _type; \
                    _tci->ti.pagesize = _pgsz;\
                    return SYS_ERR_OK;

#define CPUID_FILL_CACHE(_val,_tci, _assoc, _size, _level, _type, _shared, _linesize) \
                case _val: \
                    _tci->is_tlb = 0; \
                    _tci->ci.linesize = _linesize;\
                    _tci->ci.associativity = _assoc; \
                    _tci->ci.sets = _size / _assoc / _tci->ci.linesize; \
                    _tci->ci.level = _level; \
                    _tci->ci.type = _type; \
                    _tci->ci.size = _size; \
                    _tci->ci.shared = _shared;\
                    _tci->ci.name = cpuid_cache_names[_tci->ci.level][_tci->ci.type]; \
                    _tci->ci.inclusive=1; \
                    return SYS_ERR_OK;

struct tlb_cache_info {
    uint8_t is_tlb;
    union {
        struct cpuid_cacheinfo ci;
        struct cpuid_tlbinfo ti;
    };
};

static errval_t cache_info_lookup(struct tlb_cache_info *tci, uint8_t value)
{
    switch(value) {
        case 0x00:
        //GeneralNull descriptor, this byte contains no information
            break;
        //TLB Instruction TLB: 4 KByte pages, 4-way set associative, 32 entries
        CPUID_FILL_TLB(0x01, tci, 4, 32, 1, CPUID_CACHE_TYPE_INSTR, BASE_PAGE_SIZE);
        CPUID_FILL_TLB(0x02, tci, 0xff, 2, 1, CPUID_CACHE_TYPE_INSTR, LARGE_PAGE_SIZE);
        CPUID_FILL_TLB(0x03, tci, 4, 64, 1, CPUID_CACHE_TYPE_DATA, BASE_PAGE_SIZE);
        CPUID_FILL_TLB(0x04, tci, 4, 8, 1, CPUID_CACHE_TYPE_DATA, LARGE_PAGE_SIZE);
        CPUID_FILL_TLB(0x05, tci, 4, 32, 1, CPUID_CACHE_TYPE_DATA, LARGE_PAGE_SIZE);
        //Cache 1st-level instruction cache: 8 KBytes, 4-way set associative, 32 byte line size
        CPUID_FILL_CACHE(0x06,tci, 4, 8*1024, 1, CPUID_CACHE_TYPE_INSTR, 1, 32)
        //Cache 1st-level instruction cache: 16 KBytes, 4-way set associative, 32 byte line size
        CPUID_FILL_CACHE(0x08,tci, 4, 16*1024, 1, CPUID_CACHE_TYPE_INSTR, 1, 32)
        //Cache 1st-level instruction cache: 32KBytes, 4-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0x09,tci, 4, 32*1024, 1, CPUID_CACHE_TYPE_INSTR, 1, 64)
        //Cache 1st-level data cache: 8 KBytes, 2-way set associative, 32 byte line size
        CPUID_FILL_CACHE(0x0a,tci, 2, 8*1024, 1, CPUID_CACHE_TYPE_DATA, 1, 32)
        // Instruction TLB: 4 MByte pages, 4-way set associative, 4 entries
        CPUID_FILL_TLB(0x0b, tci, 4, 4, 1, CPUID_CACHE_TYPE_INSTR, LARGE_PAGE_SIZE);
        //Cache 1st-level data cache: 16 KBytes, 4-way set associative, 32 byte line size
        CPUID_FILL_CACHE(0x0c,tci, 4, 16*1024, 1, CPUID_CACHE_TYPE_DATA, 1, 32)
        //Cache 1st-level data cache: 16 KBytes, 4-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0x0d,tci, 4, 16*1024, 1, CPUID_CACHE_TYPE_DATA, 1, 64)
        //Cache 1st-level data cache: 24 KBytes, 6-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0x0e,tci, 6, 24*1024, 1, CPUID_CACHE_TYPE_DATA, 1, 64)
        //Cache 2nd-level cache: 128 KBytes, 2-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0x1d,tci, 2, 128*1024, 2, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 2nd-level cache: 256 KBytes, 8-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0x21,tci, 2, 128*1024, 2, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 3rd-level cache: 512 KBytes, 4-way set associative, 64 byte line size, 2 lines per sector
        CPUID_FILL_CACHE(0x22,tci, 4, 512*1024, 3, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 3rd-level cache: 1 MBytes, 8-way set associative, 64 byte line size, 2 lines per sector
        CPUID_FILL_CACHE(0x23,tci, 8, 1*1024*1024, 3, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 2nd-level cache: 1 MBytes, 16-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0x24,tci, 16, 1*1024*1024, 2, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 3rd-level cache: 2 MBytes, 8-way set associative, 64 byte line size, 2 lines per sector
        CPUID_FILL_CACHE(0x25,tci, 8, 2*1024*1024, 3, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 3rd-level cache: 4 MBytes, 8-way set associative, 64 byte line size, 2 lines per sector
        CPUID_FILL_CACHE(0x29,tci, 8, 4*1024*1024, 3, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 1st-level data cache: 32 KBytes, 8-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0x2c,tci, 8, 32*1024, 1, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 1st-level instruction cache: 32 KBytes, 8-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0x30,tci, 8, 32*1024, 1, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache No 2nd-level cache or, if processor contains a valid 2nd-level cache, no 3rd-level cache
        case 40:
            break;
        //Cache 2nd-level cache: 128 KBytes, 4-way set associative, 32 byte line size
        CPUID_FILL_CACHE(0x41,tci, 4, 128*1024, 2, CPUID_CACHE_TYPE_UNIFIED, 1, 32)
        //Cache 2nd-level cache: 256 KBytes, 4-way set associative, 32 byte line size
        CPUID_FILL_CACHE(0x42,tci, 4, 256*1024, 2, CPUID_CACHE_TYPE_UNIFIED, 1, 32)
        //Cache 2nd-level cache: 512 KBytes, 4-way set associative, 32 byte line size
        CPUID_FILL_CACHE(0x43,tci, 4, 512*1024, 2, CPUID_CACHE_TYPE_UNIFIED, 1, 32)
        //Cache 2nd-level cache: 1 MByte, 4-way set associative, 32 byte line size
        CPUID_FILL_CACHE(0x44,tci, 4, 1024*1024, 2, CPUID_CACHE_TYPE_UNIFIED, 1, 32)
        //Cache 2nd-level cache: 2 MByte, 4-way set associative, 32 byte line size
        CPUID_FILL_CACHE(0x45,tci, 4, 2*1024*1024, 2, CPUID_CACHE_TYPE_UNIFIED, 1, 32)
        //Cache 3rd-level cache: 4 MByte, 4-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0x46,tci, 4, 4*1024*1024, 3, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 3rd-level cache: 8 MByte, 8-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0x47,tci, 8, 8*1024*1024, 3, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 2nd-level cache: 3MByte, 12-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0x48,tci, 12, 3*1024*1024, 2, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 3rd-level cache: 4MB, 16-way set associative, 64-byte line size (Intel Xeon processor MP, Family 0FH, Model 06H); 2nd-level cache: 4 MByte, 16-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0x49,tci, 16, 4*1024*1024, 3, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 3rd-level cache: 6MByte, 12-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0x4a,tci, 12, 6*1024*1024, 3, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 3rd-level cache: 8MByte, 16-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0x4b,tci, 16, 8*1024*1024, 3, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 3rd-level cache: 12MByte, 12-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0x4c,tci, 12, 12*1024*1024, 3, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 3rd-level cache: 16MByte, 16-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0x4d,tci, 16, 16*1024*1024, 3, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 2nd-level cache: 6MByte, 24-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0x4e,tci, 24, 24*1024*1024, 2, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //TLB Instruction TLB: 4 KByte pages, 32 entries
        CPUID_FILL_TLB(0x4f, tci, 0xff, 32, 1, CPUID_CACHE_TYPE_INSTR, BASE_PAGE_SIZE);
        //TLB Instruction TLB: 4 KByte and 2-MByte or 4-MByte pages, 64 entries
        CPUID_FILL_TLB(0x50, tci, 0xff, 64, 1, CPUID_CACHE_TYPE_INSTR, LARGE_PAGE_SIZE);
        //TLB Instruction TLB: 4 KByte and 2-MByte or 4-MByte pages, 128 entries
        CPUID_FILL_TLB(0x51, tci, 0xff, 128, 1, CPUID_CACHE_TYPE_INSTR, LARGE_PAGE_SIZE);
        //TLB Instruction TLB: 4 KByte and 2-MByte or 4-MByte pages, 256 entries
        CPUID_FILL_TLB(0x52, tci, 0xff, 256, 1, CPUID_CACHE_TYPE_INSTR, BASE_PAGE_SIZE);
        //TLB Instruction TLB: 2-MByte or 4-MByte pages, fully associative, 7 entries
        CPUID_FILL_TLB(0x55, tci, 0xff, 7, 1, CPUID_CACHE_TYPE_INSTR, LARGE_PAGE_SIZE);
        //TLB Data TLB0: 4 MByte pages, 4-way set associative, 16 entries
        CPUID_FILL_TLB(0x56, tci, 4, 16, 1, CPUID_CACHE_TYPE_DATA, LARGE_PAGE_SIZE);
        //TLB Data TLB0: 4 KByte pages, 4-way associative, 16 entries
        CPUID_FILL_TLB(0x57, tci, 4, 16, 1, CPUID_CACHE_TYPE_DATA, BASE_PAGE_SIZE);
        //TLB Data TLB0: 4 KByte pages, fully associative, 16 entries
        CPUID_FILL_TLB(0x59, tci, 0xff, 16, 1, CPUID_CACHE_TYPE_DATA, BASE_PAGE_SIZE);
        //TLB Data TLB0: 2-MByte or 4 MByte pages, 4-way set associative, 32 entries
        CPUID_FILL_TLB(0x5a, tci, 4, 32, 1, CPUID_CACHE_TYPE_DATA, LARGE_PAGE_SIZE);
        //TLB Data TLB: 4 KByte and 4 MByte pages, 64 entries
        CPUID_FILL_TLB(0x5b, tci, 0xff, 64, 1, CPUID_CACHE_TYPE_DATA, BASE_PAGE_SIZE);
        //TLB Data TLB: 4 KByte and 4 MByte pages,128 entries
        CPUID_FILL_TLB(0x5c, tci, 0xff, 128, 1, CPUID_CACHE_TYPE_DATA, BASE_PAGE_SIZE);
        //TLB Data TLB: 4 KByte and 4 MByte pages,256 entries
        CPUID_FILL_TLB(0x5d, tci, 0xff, 256, 1, CPUID_CACHE_TYPE_DATA, BASE_PAGE_SIZE);
        case 0x60:
        //Cache 1st-level data cache: 16 KByte, 8-way set associative, 64 byte line size
            break;
        //TLB Instruction TLB: 4 KByte pages, fully associative, 48 entries
        CPUID_FILL_TLB(0x61, tci, 0xff, 48, 1, CPUID_CACHE_TYPE_INSTR, BASE_PAGE_SIZE);
        //TLB Data TLB: 1 GByte pages, 4-way set associative, 4 entries
        CPUID_FILL_TLB(0x63, tci, 4, 4, 1, CPUID_CACHE_TYPE_INSTR, HUGE_PAGE_SIZE);
        //Cache 1st-level data cache: 8 KByte, 4-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0x66,tci, 4, 8*1024, 1, CPUID_CACHE_TYPE_DATA, 1, 64)
        //Cache 1st-level data cache: 16 KByte, 4-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0x67,tci, 4, 16*1024, 1, CPUID_CACHE_TYPE_DATA, 1, 64)
        //Cache 1st-level data cache: 32 KByte, 4-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0x68,tci, 4, 32*1024, 1, CPUID_CACHE_TYPE_DATA, 1, 64)
        case 0x70:
        //Cache Trace cache: 12 K-μop, 8-way set associative
        case 0x71:
        //Cache Trace cache: 16 K-μop, 8-way set associative
        case 0x72:
        //Cache Trace cache: 32 K-μop, 8-way set associative
            break;
        //TLB Instruction TLB: 2M/4M pages, fully associative, 8 entries
        CPUID_FILL_TLB(0x76, tci, 0xff, 8, 1, CPUID_CACHE_TYPE_INSTR, LARGE_PAGE_SIZE);
        //Cache 2nd-level cache: 1 MByte, 4-way set associative, 64byte line size
        CPUID_FILL_CACHE(0x78,tci, 4, 1*1024*1024, 2, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 2nd-level cache: 128 KByte, 8-way set associative, 64 byte line size, 2 lines per sector
        CPUID_FILL_CACHE(0x79,tci, 8, 128*1024, 2, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 2nd-level cache: 256 KByte, 8-way set associative, 64 byte line size, 2 lines per sector
        CPUID_FILL_CACHE(0x7a,tci, 8, 256*1024, 2, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 2nd-level cache: 512 KByte, 8-way set associative, 64 byte line size, 2 lines per sector
        CPUID_FILL_CACHE(0x7b,tci, 8, 512*1024, 2, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 2nd-level cache: 1 MByte, 8-way set associative, 64 byte line size, 2 lines per sector
        CPUID_FILL_CACHE(0x7c,tci, 8, 1024*1024, 2, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 2nd-level cache: 2 MByte, 8-way set associative, 64byte line size
        CPUID_FILL_CACHE(0x7d,tci, 8, 2*1024*1024, 2, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 2nd-level cache: 512 KByte, 2-way set associative, 64-byte line size
        CPUID_FILL_CACHE(0x7f,tci, 2, 512*1024, 2, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 2nd-level cache: 512 KByte, 8-way set associative, 64-byte line size
        CPUID_FILL_CACHE(0x80,tci, 8, 512*1024, 2, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 2nd-level cache: 256 KByte, 8-way set associative, 32 byte line size
        CPUID_FILL_CACHE(0x82,tci, 8, 256*1024, 2, CPUID_CACHE_TYPE_UNIFIED, 1, 32)
        //Cache 2nd-level cache: 512 KByte, 8-way set associative, 32 byte line size
        CPUID_FILL_CACHE(0x83,tci, 8, 512*1024, 2, CPUID_CACHE_TYPE_UNIFIED, 1, 32)
        //Cache 2nd-level cache: 1 MByte, 8-way set associative, 32 byte line size
        CPUID_FILL_CACHE(0x84,tci, 8, 1024*1024, 2, CPUID_CACHE_TYPE_UNIFIED, 1, 32)
        //Cache 2nd-level cache: 2 MByte, 8-way set associative, 32 byte line size
        CPUID_FILL_CACHE(0x85,tci, 8, 2*1024*1024, 2, CPUID_CACHE_TYPE_UNIFIED, 1, 32)
        //Cache 2nd-level cache: 512 KByte, 4-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0x86,tci, 4, 512*1024, 2, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 2nd-level cache: 1 MByte, 8-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0x87,tci, 8, 1*1024*1024, 2, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //DTLB DTLB: 4k pages, fully associative, 32 entries
        CPUID_FILL_TLB(0xa0, tci, 0xff, 32, 1, CPUID_CACHE_TYPE_DATA, BASE_PAGE_SIZE);
        //TLB Instruction TLB: 4 KByte pages, 4-way set associative, 128 entries
        CPUID_FILL_TLB(0xb0, tci, 0x4, 128, 1, CPUID_CACHE_TYPE_INSTR, BASE_PAGE_SIZE);
        //TLB Instruction TLB: 2M pages, 4-way, 8 entries or 4M pages, 4-way, 4 entries
        CPUID_FILL_TLB(0xb1, tci, 0x4, 8, 1, CPUID_CACHE_TYPE_INSTR, LARGE_PAGE_SIZE);
        //TLB Instruction TLB: 4KByte pages, 4-way set associative, 64 entries
        CPUID_FILL_TLB(0xb2, tci, 0x4, 64, 1, CPUID_CACHE_TYPE_INSTR, BASE_PAGE_SIZE);
        //TLB Data TLB: 4 KByte pages, 4-way set associative, 128 entries
        CPUID_FILL_TLB(0xb3, tci, 0x4, 128, 1, CPUID_CACHE_TYPE_DATA, BASE_PAGE_SIZE);
        //TLB Data TLB1: 4 KByte pages, 4-way associative, 256 entries
        CPUID_FILL_TLB(0xb4, tci, 0x4, 256, 1, CPUID_CACHE_TYPE_DATA, BASE_PAGE_SIZE);
        //TLB Instruction TLB: 4KByte pages, 8-way set associative, 64 entries
        CPUID_FILL_TLB(0xb5, tci, 0x8, 64, 1, CPUID_CACHE_TYPE_INSTR, BASE_PAGE_SIZE);
        //TLB Instruction TLB: 4KByte pages, 8-way set associative, 128 entries
        CPUID_FILL_TLB(0xb6, tci, 0x8, 128, 1, CPUID_CACHE_TYPE_INSTR, BASE_PAGE_SIZE);
        //TLB Data TLB1: 4 KByte pages, 4-way associative, 64 entries
        CPUID_FILL_TLB(0xba, tci, 0x4, 64, 1, CPUID_CACHE_TYPE_DATA, BASE_PAGE_SIZE);
        //TLB Data TLB: 4 KByte and 4 MByte pages, 4-way associative, 8 entries
        CPUID_FILL_TLB(0xc0, tci, 0x4, 8, 1, CPUID_CACHE_TYPE_DATA, LARGE_PAGE_SIZE + BASE_PAGE_SIZE);
        //STLB Shared 2nd-Level TLB: 4 KByte/2MByte pages, 8-way associative, 1024 entries
        CPUID_FILL_TLB(0xc1, tci, 0x8, 1024, 2, CPUID_CACHE_TYPE_UNIFIED, LARGE_PAGE_SIZE + BASE_PAGE_SIZE);
        //DTLB DTLB: 4 KByte/2 MByte pages, 4-way associative, 16 entries
        CPUID_FILL_TLB(0xc2, tci, 0x4, 16, 1, CPUID_CACHE_TYPE_DATA, LARGE_PAGE_SIZE + BASE_PAGE_SIZE);
        //STLB Shared 2nd-Level TLB: 4 KByte /2 MByte pages, 6-way associative, 1536 entries. Also 1GBbyte pages, 4-way, 16 entries.
        CPUID_FILL_TLB(0xc3, tci, 0x6, 1536, 2, CPUID_CACHE_TYPE_UNIFIED, BASE_PAGE_SIZE);
        //STLB Shared 2nd-Level TLB: 4 KByte pages, 4-way associative, 512 entries
        CPUID_FILL_TLB(0xca, tci, 0x4, 512, 2, CPUID_CACHE_TYPE_UNIFIED, BASE_PAGE_SIZE);
        //Cache 3rd-level cache: 512 KByte, 4-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0xd0,tci, 4, 512*1024, 3, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 3rd-level cache: 1 MByte, 4-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0xd1,tci, 4, 1*1024*1024, 3, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 3rd-level cache: 2 MByte, 4-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0xd2,tci, 4, 2*1024*1024, 3, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 3rd-level cache: 1 MByte, 8-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0xd6,tci, 8, 1*1024*1024, 3, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 3rd-level cache: 2 MByte, 8-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0xd7,tci, 8, 2*1024*1024, 3, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 3rd-level cache: 4 MByte, 8-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0xd8,tci, 8, 4*1024*1024, 3, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 3rd-level cache: 1.5 MByte, 12-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0xdc,tci, 12, 512*1024 + 1024*1024, 3, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 3rd-level cache: 3 MByte, 12-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0xdd,tci, 12, 3*1024*1024, 3, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 3rd-level cache: 6 MByte, 12-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0xde,tci, 12, 6*1024*1024, 3, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 3rd-level cache: 2 MByte, 16-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0xe2,tci, 16, 2*1024*1024, 3, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 3rd-level cache: 4 MByte, 16-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0xe3,tci, 16, 4*1024*1024, 3, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 3rd-level cache: 8 MByte, 16-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0xe4,tci, 16, 8*1024*1024, 3, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 3rd-level cache: 12MByte, 24-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0xea,tci, 24, 12*1024*1024, 3, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 3rd-level cache: 18MByte, 24-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0xeb,tci, 24, 18*1024*1024, 3, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        //Cache 3rd-level cache: 24MByte, 24-way set associative, 64 byte line size
        CPUID_FILL_CACHE(0xec,tci, 24, 24*1024*1024, 3, CPUID_CACHE_TYPE_UNIFIED, 1, 64)
        case 0xF0:
        //Prefetch: 64byte prefetching
        case 0xF1:
        //Prefetching 128-Byte prefetching
        case 0xFF:
         //General CPUID leaf 2 does not report cache descriptor information, use CPUID leaf 4 to query cache parameters
        default:
            break;
    }

    return CPUID_ERR_INVALID_INDEX;
}


static errval_t cache_info_alternate(struct cpuid_cacheinfo *ci, uint32_t idx)
{
    if (cpuid_g_max_input_basic < 4) {
        return CPUID_ERR_UNSUPPORTED_FUNCTION;
    }

    struct cpuid_regs reg = CPUID_REGS_INITIAL(4, idx);
    cpuid_exec(&reg);

    cpuid_intel_cache_info_basic_t ci_a = (cpuid_intel_cache_info_basic_t) &reg.eax;
    cpuid_intel_cache_info_ebx_t ci_b = (cpuid_intel_cache_info_ebx_t) &reg.ebx;

    ci->level = cpuid_intel_cache_info_basic_level_extract(ci_a);

    switch (cpuid_intel_cache_info_basic_ctype_extract(ci_a)) {
        case cpuid_intel_cache_type_data :
            /* data cache */
            ci->type = CPUID_CACHE_TYPE_DATA;
            break;
        case cpuid_intel_cache_type_instr :
            /* instruction cache */
            ci->type = CPUID_CACHE_TYPE_INSTR;
            break;
        case cpuid_intel_cache_type_unified :
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
    ci->linesize = cpuid_intel_cache_info_ebx_coherency_extract(ci_b)+1;

    /* the the number of sets */
    ci->sets = reg.ecx + 1;

    if (cpuid_intel_cache_info_basic_fullyassoc_extract(ci_a)) {
        ci->size = (size_t)ci->linesize * ci->sets;
        ci->associativity = 0xff;
    } else {
        ci->associativity = cpuid_intel_cache_info_ebx_assoc_extract(ci_b)+1;
        ci->size = (size_t)ci->linesize * ci->sets * ci->associativity;
    }

    ci->shared = cpuid_intel_cache_info_basic_maxlog_extract(ci_a) +1;

    cpuid_intel_cache_info_edx_t ci_d = (cpuid_intel_cache_info_edx_t) &reg.edx;
    ci->inclusive = cpuid_intel_cache_info_edx_inclusive_extract(ci_d);

    return SYS_ERR_OK;
}


static errval_t cache_info(struct cpuid_cacheinfo *cinfo, uint32_t idx)
{
    if (cpuid_g_max_input_basic < 2) {
        return CPUID_ERR_UNSUPPORTED_FUNCTION;
    }

    if (cache_info_in_leaf_4) {
        return cache_info_alternate(cinfo, idx);
    }

    struct cpuid_regs reg = CPUID_REGS_INITIAL(2, 0);
    cpuid_exec(&reg);

    cpuid_intel_cache_info_t ci = (cpuid_intel_cache_info_t)&reg.eax;
    assert(cpuid_intel_cache_info_d0_extract(ci) == 0x01);

    uint32_t *values = &reg.eax;
    uint8_t *cval;
    uint32_t currentidx = 0;
    for (int i = 0; i < 4; ++i) {
        ci = (cpuid_intel_cache_info_t)(values + i);

        /* check for validity */
        if (cpuid_intel_cache_info_v0_extract(ci)) {
            continue;
        }

        cval = (uint8_t *)ci;
        for (int j = (i == 0); j < 4; ++j) {
            struct tlb_cache_info tci;
            if (cval[j] == 0x00) {
                continue;
            } else if (cval[j] == 0xff) {
                /*
                 * a value of 0xff indicates that the cache values are reported
                 * in leaf 4 of cpuid
                 */
                cache_info_in_leaf_4 = 0x1;
                return cache_info_alternate(cinfo, idx);
            }

            if (err_is_ok(cache_info_lookup(&tci, cval[j]))) {
                if (!tci.is_tlb) {
                    if (currentidx == idx) {
                        *cinfo = tci.ci;
                        return SYS_ERR_OK;
                    }
                    currentidx++;
                }
            }

        }
    }

    return CPUID_ERR_INVALID_INDEX;

}

static uint16_t cache_line_size(void)
{
    if (cpuid_g_max_input_basic < 1) {
        return CPUID_ERR_UNSUPPORTED_FUNCTION;
    }

    struct cpuid_regs reg = CPUID_REGS_INITIAL(1, 0);
    cpuid_exec(&reg);

    cpuid_intel_miscinfo_t mi = (cpuid_intel_miscinfo_t)&reg.ebx;

    return cpuid_intel_miscinfo_cflush_sz_extract(mi) * 8;
}

static errval_t tlb_info(struct cpuid_tlbinfo *ti, uint32_t idx)
{
    if (cpuid_g_max_input_basic < 2) {
        return CPUID_ERR_UNSUPPORTED_FUNCTION;
    }

    struct cpuid_regs reg = CPUID_REGS_INITIAL(2, 0);
    cpuid_exec(&reg);

    cpuid_intel_cache_info_t ci = (cpuid_intel_cache_info_t)&reg.eax;
    assert(cpuid_intel_cache_info_d0_extract(ci) == 0x01);

    uint32_t *values = &reg.eax;
    uint8_t *cval;
    uint32_t currentidx = 0;
    for (int i = 0; i < 4; ++i) {
        ci = (cpuid_intel_cache_info_t)(values + i);

        /* check for validity */
        if (cpuid_intel_cache_info_v0_extract(ci)) {
            continue;
        }

        cval = (uint8_t *)ci;
        for (int j = (i == 0); j < 4; ++j) {
            struct tlb_cache_info tci;
            if (cval[j] == 0x00) {
                continue;
            }
            if (err_is_ok(cache_info_lookup(&tci, cval[j]))) {
                if (tci.is_tlb) {
                    if (currentidx == idx) {
                        *ti = tci.ti;
                        return SYS_ERR_OK;
                    }
                    currentidx++;
                }
            }

        }
    }

    return CPUID_ERR_INVALID_INDEX;

}

/*
 * ===============================================================================
 * thread and topology information
 * ===============================================================================
 */

static errval_t thread_info(struct cpuid_threadinfo *ti)
{
    if (cpuid_g_max_input_basic < 4) {
        return CPUID_ERR_UNSUPPORTED_FUNCTION;
    }

    struct cpuid_regs reg = CPUID_REGS_INITIAL(0x1, 0);

    cpuid_exec(&reg);

    if (!((reg.edx >> 28) & 0x1)) {
        /* TODO: the cpu supports no hyperthreading */
        ti->hyperthread = 0;
        USER_PANIC("NYI");
        return SYS_ERR_OK;
    }

    cpuid_intel_miscinfo_t f = (cpuid_intel_miscinfo_t)&reg.ebx;

    coreid_t logical_cores = cpuid_intel_miscinfo_max_log_proc_extract(f);
    coreid_t apic_id = cpuid_intel_miscinfo_init_apicid_extract(f);

    reg.eax = 0x4;
    reg.ecx = 0;
    cpuid_exec(&reg);

    cpuid_intel_cache_info_basic_t ci = (cpuid_intel_cache_info_basic_t)&reg.eax;
    coreid_t cores_per_package = cpuid_intel_cache_info_basic_maxphys_extract(ci);
    coreid_t ht = logical_cores / cores_per_package;

    uint8_t ht_shift = cpuid_bits_needed(ht - 1);
    uint8_t ht_mask = (1 << ht_shift) - 1;
    uint8_t core_shift = cpuid_bits_needed(cores_per_package - 1);
    uint8_t core_mask = (1 << core_shift) - 1;

    ti->core = (apic_id >> (ht_shift)) & core_mask;
    ti->package = apic_id >> (ht_shift + core_shift);
    ti->hyperthread = apic_id & ht_mask;

    return SYS_ERR_OK;
}

static errval_t topology_info(struct cpuid_topologyinfo *topo, uint8_t idx)
{
    struct cpuid_regs reg = CPUID_REGS_INITIAL(0xb, idx);
    cpuid_exec(&reg);

    printf("getting the topology information %u\n", idx);

    cpuid_intel_topology_ecx_t ta = (cpuid_intel_topology_eax_t)&reg.eax;
    cpuid_intel_topology_ebx_t tb = (cpuid_intel_topology_ebx_t)&reg.ebx;

    printf("getting the topology information %u, %u\n", idx, cpuid_intel_topology_eax_x2apic_shift_extract(ta));

    cpuid_intel_topology_ecx_t tc = (cpuid_intel_topology_ecx_t)&reg.ecx;
    switch (cpuid_intel_topology_ecx_level_type_extract(tc)) {
        case cpuid_intel_topology_level_smt:
            printf("found smt at level %u\n", idx);
            break;
        case cpuid_intel_topology_level_core:
            printf("found core at level %u\n", idx);
            break;
        case cpuid_intel_topology_level_invalid:
            printf("level invalid %u\n", idx);
            return CPUID_ERR_INVALID_INDEX;
            break;
        default:
            printf("level u %u\n", idx);
            return CPUID_ERR_INVALID_INDEX;
            break;
    }


    printf("Level numer=%u, logical proc=%u\n", cpuid_intel_topology_ecx_level_number_extract(tc), cpuid_intel_topology_ebx_logical_proc_extract(tb));


    topo->nextshift = cpuid_intel_topology_eax_x2apic_shift_extract(ta);
    topo->x2apic = reg.edx;

    return SYS_ERR_OK;
}

static errval_t feature_info(struct cpuid_featureinfo *fi)
{
    if (cpuid_g_max_input_basic < 0x1) {
        return CPUID_ERR_UNSUPPORTED_FUNCTION;
    }

    struct cpuid_regs reg  = CPUID_REGS_INITIAL(0x1, 0);
    cpuid_exec(&reg);

    cpuid_intel_features_t ft = (cpuid_intel_features_t)&reg.ecx;
    /* CPU features */

    fi->htt     = cpuid_intel_features_htt_extract(ft);
    fi->apic    = cpuid_intel_features_apic_extract(ft);
    fi->x2apic  = cpuid_intel_features_x2apic_extract(ft);
    fi->tsc     = cpuid_intel_features_tsc_extract(ft);
    fi->dca     = cpuid_intel_features_dca_extract(ft);

    /* instructions  */
    fi->monitor = cpuid_intel_features_monitor_extract(ft);
    fi->cmov    = cpuid_intel_features_cmov_extract(ft);

    /* virtual memory */
    fi->pse36  = cpuid_intel_features_pse36_extract(ft);
    fi->pse    = cpuid_intel_features_pse_extract(ft);
    fi->pae    = cpuid_intel_features_pae_extract(ft);
    fi->pat    = cpuid_intel_features_pat_extract(ft);
    fi->pge    = cpuid_intel_features_pge_extract(ft);
    fi->mtrr   = cpuid_intel_features_mtrr_extract(ft);

    fi->page2M = 1;


    /* cache control */
    fi->clsh    = cpuid_intel_features_clfsh_extract(ft);
    fi->cnxt_id = cpuid_intel_features_cntx_id_extract(ft);

    /* virtualization technology */
    fi->vmx    = cpuid_intel_features_vmx_extract(ft);
    fi->vme    = cpuid_intel_features_vme_extract(ft);
    fi->svm    = 0;

    /* vector instructions */
    fi->mmx   = cpuid_intel_features_mmx_extract(ft);
    fi->sse   = cpuid_intel_features_sse_extract(ft);
    fi->sse2  = cpuid_intel_features_sse2_extract(ft);
    fi->sse3  = cpuid_intel_features_sse3_extract(ft);
    fi->sse41 = cpuid_intel_features_sse4_1_extract(ft);
    fi->sse42 = cpuid_intel_features_sse4_2_extract(ft);
    fi->avx   = cpuid_intel_features_avx_extract(ft);

    if (CPUID_EXTENDED_INPUT_MASK(cpuid_g_max_input_extended) < 0x1) {
        return SYS_ERR_OK;
    }

    reg.eax = 0x80000001;
    reg.ecx = 0;
    cpuid_exec(&reg);

    cpuid_intel_features_ext_edx_t ft2 = (cpuid_intel_features_ext_edx_t)&reg.edx;
    fi->nx     = cpuid_intel_features_ext_edx_nx_extract(ft2);
    fi->page1G = cpuid_intel_features_ext_edx_page1G_extract(ft2);;
    fi->lm     = cpuid_intel_features_ext_edx_lm_extract(ft2);

    return SYS_ERR_OK;
}

static errval_t address_space_info(struct cpuid_adressspaceinfo *ai)
{
    if (CPUID_EXTENDED_INPUT_MASK(cpuid_g_max_input_extended) < 0x8) {
        return CPUID_ERR_UNSUPPORTED_FUNCTION;
    }

    struct cpuid_regs reg  = CPUID_REGS_INITIAL(0x80000008, 0);
    cpuid_exec(&reg);

    cpuid_intel_addrspace_t as = (cpuid_intel_addrspace_t)&reg.eax;

    ai->physical = cpuid_intel_addrspace_physical_extract(as);
    ai->virtual = cpuid_intel_addrspace_linear_extract(as);
    /* this information is not provided by intel*/
    ai->guest_physical = 0;

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
void cpuid_intel_set_handlers(struct cpuid_functions *fn_tab)
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
