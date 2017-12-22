/**
 * \file
 * \brief x86-32 interrupt/exception handling
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*-
 * Copyright (c) 1989, 1990 William F. Jolitz
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *      from: @(#)segments.h    7.1 (Berkeley) 5/9/91
 * $FreeBSD$
 */

#ifndef KERNEL_IRQ_H
#define KERNEL_IRQ_H

/*
 * AMD64 Segmentation Data Structures and definitions
 */

/*
 * Selectors
 */

#define SEL_RPL_MASK    3       /* requester priv level */
#define ISPL(s) ((s)&3)         /* what is the priority level of a selector */
#define SEL_KPL 0               /* kernel priority level */
#define SEL_UPL 3               /* user priority level */
#define ISLDT(s)        ((s)&SEL_LDT)   /* is it local or global */
#define SEL_LDT 4               /* local descriptor table */
#define IDXSEL(s)       (((s)>>3) & 0x1fff)             /* index of selector */
#define LSEL(s,r)       (((s)<<3) | SEL_LDT | r)        /* a local selector */
#define GSEL(s,r)       (((s)<<3) | r)                  /* a global selector */

/**
 * Gate descriptors (e.g. indirect descriptors, trap, interrupt etc. 128 bit)
 * Only interrupt and trap gates have gd_ist.
 */
struct  gate_descriptor {
    uint64_t gd_looffset:16;       /* gate offset (lsb) */
    uint64_t gd_selector:16;       /* gate segment selector */
    uint64_t gd_xx:5;              /* reserved */
    uint64_t gd_mbz:3;             /* must be zero */
    uint64_t gd_type:5;            /* segment type */
    uint64_t gd_dpl:2;             /* segment descriptor priority level */
    uint64_t gd_p:1;               /* segment descriptor present */
    uint64_t gd_hioffset:16;       /* gate offset (msb) */
} __attribute__((packed));

        /* system segments and gate types */
#define SDT_SYSNULL      0      /* system null */
#define SDT_SYSLDT       2      /* system local descriptor table */
#define SDT_SYSTSS       9      /* system available 32 bit TSS */
#define SDT_SYSBSY      11      /* system busy 32 bit TSS */
#define SDT_SYSCGT      12      /* system 32 bit call gate */
#define SDT_SYSIGT      14      /* system 32 bit interrupt gate */
#define SDT_SYSTGT      15      /* system 32 bit trap gate */

        /* memory segment types */
#define SDT_MEMRO       16      /* memory read only */
#define SDT_MEMROA      17      /* memory read only accessed */
#define SDT_MEMRW       18      /* memory read write */
#define SDT_MEMRWA      19      /* memory read write accessed */
#define SDT_MEMROD      20      /* memory read only expand dwn limit */
#define SDT_MEMRODA     21      /* memory read only expand dwn limit accessed */
#define SDT_MEMRWD      22      /* memory read write expand dwn limit */
#define SDT_MEMRWDA     23      /* memory read write expand dwn limit accessed */
#define SDT_MEME        24      /* memory execute only */
#define SDT_MEMEA       25      /* memory execute only accessed */
#define SDT_MEMER       26      /* memory execute read */
#define SDT_MEMERA      27      /* memory execute read accessed */
#define SDT_MEMEC       28      /* memory execute only conforming */
#define SDT_MEMEAC      29      /* memory execute only accessed conforming */
#define SDT_MEMERC      30      /* memory execute read conforming */
#define SDT_MEMERAC     31      /* memory execute read accessed conforming */

/*
 * Size of IDT table
 */
#define NIDT    256             /* 32 reserved, 16 h/w, 0 s/w, linux's 0x80 */

/// Number of (reserved) hardware exceptions
#define NEXCEPTIONS             32

/// Size of hardware IRQ dispatch table == #NIDT - #NEXCEPTIONS exceptions
#define NDISPATCH               (NIDT - NEXCEPTIONS)


/*
 * Entries in the Global Descriptor Table (GDT)
 */
#define NULL_SEL        0       /**< Null descriptor */
#define KCODE_SEL       1       /**< Kernel code descriptor */
#define KSTACK_SEL      2       /**< Shared user/kernel stack descriptor */
#define USTACK_SEL      3       /**< User stack descriptor */
#define UCODE_SEL       4       /**< User code descriptor */
#define TSS_SEL         5       /**< Task State Segment (TSS) */
#define DISP_SEL        6       /**< Dispatcher pointer */
#define NGDT_MEM        7       /**< Number of descriptors */

/**
 * region descriptors, used to load gdt/idt tables before segments yet exist.
 */
struct region_descriptor {
    uint16_t rd_limit;          /**< segment extent */
    uint64_t rd_base;           /**< base address  */
} __attribute__((packed));

/**
 * \brief Segment descriptor
 */
union segment_descriptor {
    uint64_t raw;
    struct {
        uint64_t lo_limit:16;
        uint64_t lo_base:24;
        uint64_t type:4;
        uint64_t system_desc:1;
        uint64_t privilege_level:2;
        uint64_t present:1;
        uint64_t hi_limit:4;
        uint64_t available:1;
        uint64_t long_mode:1;
        uint64_t operation_size:1;
        uint64_t granularity:1;
        uint64_t hi_base:8;
    } d;
    struct {
        uint64_t lo_limit:16;
        uint64_t lo_base:24;
        uint64_t type:4;
        uint64_t always0:1;
        uint64_t privilege_level:2;
        uint64_t present:1;
        uint64_t hi_limit:4;
        uint64_t available:1;
        uint64_t always0_1:2;
        uint64_t granularity:1;
        uint64_t hi_base:8;
    } tss;
};

struct task_state_segment {
    uint16_t    previous_task_link;
    uint16_t    reserved;
    uint32_t    esp0;
    uint32_t    ss0;
    uint32_t    esp1;
    uint32_t    ss1;
    uint32_t    esp2;
    uint32_t    ss2;
    uint32_t    cr3_pdbr;
    uint32_t    eip;
    uint32_t    eflags;
    uint32_t    eax;
    uint32_t    ecx;
    uint32_t    edx;
    uint32_t    ebx;
    uint32_t    esp;
    uint32_t    ebp;
    uint32_t    esi;
    uint32_t    edi;
    uint32_t    es;
    uint32_t    cs;
    uint32_t    ss;
    uint32_t    ds;
    uint32_t    fs;
    uint32_t    gs;
    uint32_t    ldt_segment_selector;
    uint16_t    T;
    uint16_t    iomap_base;
} __attribute__ ((packed));

void setup_default_idt(void);

errval_t irq_connect(struct capability *dest_cap, capaddr_t endpoint_adr);
errval_t irq_table_alloc(int *outvec);
errval_t irq_table_set(unsigned int nidt, capaddr_t endpoint);
errval_t irq_table_delete(unsigned int nidt);
struct kcb;
errval_t irq_table_notify_domains(struct kcb *kcb);
errval_t irq_table_alloc_dest_cap(uint8_t dcn_vbits, capaddr_t dcn, capaddr_t out_cap_addr);


#endif
