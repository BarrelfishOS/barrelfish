/**
 * \file
 * \brief Kernel debugging functions
 */

/*
 * Copyright (c) 2008, 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <stdio.h>
#include <x86.h>
#include <arch/x86/debug.h>
#include <arch/x86_64/irq.h>
#include <arch/x86_64/vmx_vmkit.h> // for rd_idtr
#include <paging_kernel_arch.h>

union lin_addr {
    uint64_t raw;
    struct {
        uint64_t  offset       :12;
        uint64_t  ptable       :9;
        uint64_t  pdir         :9;
        uint64_t  pdpt         :9;
        uint64_t  pml4         :9;
        uint64_t  sign_extend  :16;
    } d;
};

void debug_vaddr_identify(lvaddr_t debug_pml4, lvaddr_t vaddr)
{
    int i;
    printf("cr3 register      %lx\n", debug_pml4);
    printf("identifying vaddr %lx\n", vaddr);

    volatile uint64_t *temp = (uint64_t*)vaddr;

    for(i = 0; i < 512; i++) {
        printf("at addr %lx content is %lx\n", (uint64_t)(temp + i), *(temp + i));
    }
    printf("\n");

    union lin_addr lin_addr;
    lin_addr.raw = (uint64_t)vaddr;

    printf("vaddr broken down\n");
    printf("sign_extend = %x\n", lin_addr.d.sign_extend);
    printf("pml4        = %x\n", lin_addr.d.pml4);
    printf("pdpt        = %x\n", lin_addr.d.pdpt);
    printf("pdir        = %x\n", lin_addr.d.pdir);
    printf("ptable      = %x\n", lin_addr.d.ptable);
    printf("offset      = %x\n", lin_addr.d.offset);

    uint64_t *pml4et;
    pml4et = (uint64_t*)(debug_pml4 +
                         (lin_addr.d.pml4 * sizeof(union x86_64_pdir_entry)));
    printf("PML4e addr = %016lx ", (uint64_t)pml4et);
    printf("content = %016lx\n", *pml4et);

    lvaddr_t pdpt_addr;
    pdpt_addr = local_phys_to_mem(((union x86_64_pdir_entry*)pml4et)->d.base_addr << 12);
    uint64_t *pdptet;
    pdptet = (uint64_t*)(pdpt_addr +
                         (lin_addr.d.pdpt * sizeof(union x86_64_pdir_entry)));
    printf("PDPe  addr = %016lx ", (uint64_t)pdptet);
    printf("content = %016lx\n", *pdptet);

    if (x86_64_pdir_entry_leafp ((union x86_64_pdir_entry*) pdptet)) {
	lpaddr_t addr = ((union x86_64_ptable_entry*)pdptet)->huge.base_addr << 30;
	printf("addr = %016lx, 1G page\n", addr);
	return;
    }

    lvaddr_t pdir_addr;
    pdir_addr = local_phys_to_mem(((union x86_64_pdir_entry*)pdptet)->d.base_addr << 12);
    uint64_t *pdiret;
    pdiret = (uint64_t*)(pdir_addr +
                         (lin_addr.d.pdir * sizeof(union x86_64_pdir_entry)));
    printf("PGDe  addr = %016lx ", (uint64_t)pdiret);
    printf("content = %016lx\n", *pdiret);

    if (x86_64_pdir_entry_leafp ((union x86_64_pdir_entry*) pdiret)) {
	lpaddr_t addr = ((union x86_64_ptable_entry*)pdiret)->large.base_addr << 21;
	printf("addr = %016lx, 2M page\n", addr);
	return;
    }

    lvaddr_t ptable_addr;
    ptable_addr = local_phys_to_mem(((union x86_64_pdir_entry*)pdiret)->d.base_addr << 12);
    uint64_t *ptableet;
    ptableet = (uint64_t*)(ptable_addr +
                         (lin_addr.d.ptable * sizeof(union x86_64_pdir_entry)));
    printf("PTe   addr = %016lx ", (uint64_t)ptableet);
    printf("content = %016lx\n", *ptableet);

    lpaddr_t addr = ((union x86_64_ptable_entry*)ptableet)->base.base_addr << 12;
    printf("addr = %016lx, 4K page\n", addr);
}

uintptr_t kernel_virt_to_elf_addr(void *addr)
{
    return (uintptr_t)addr - (uintptr_t)&_start_kernel + START_KERNEL_PHYS;
}
#define gate_offset(g) ((g).offset_low | ((unsigned long)(g).offset_middle << 16) | ((unsigned long)(g).offset_high << 32))

static inline uintptr_t gd_offset (struct gate_descriptor *gd)
{
    return (uintptr_t) ((((uint64_t) gd->gd_hioffset) << 16)
		       |(((uint64_t) gd->gd_looffset) & 0xffff));
}

static void dump_gate_descriptor (struct gate_descriptor *gd, char *desc, int i)
{
    char *type;

    switch (gd->gd_type) {
    case SDT_SYSNULL:  type = "NULL";      break;
    case SDT_SYSLDT:   type = "LDT";       break;
    case SDT_SYSTSS:   type = "TSS avail"; break;
    case SDT_SYSBSY:   type = "TSS busy";  break;
    case SDT_SYSCGT:   type = "call gate"; break;
    case SDT_SYSIGT:   type = "intr gate"; break;
    case SDT_SYSTGT:   type = "trap gate"; break;
    default:
	if (gd->gd_type >= SDT_MEMRO)
	    type = "memory";
	else
	    type = "invalid";
    };

    char *iscanon = is_canonical (gd_offset (gd)) ? "ok" : "non-canonical!";
    printf ("  %s[%d]%p = { .sel = %d, .offset = 0x%lx(%s), .present = %d, .type = 0x%x (%s), .istidx = %d, .dpl = %x }\n"
	    , desc, i, gd, gd->gd_selector, gd_offset (gd), iscanon, gd->gd_p,    gd->gd_type, type, gd->gd_ist,   gd->gd_dpl);
}

void dump_idt (void)
{
    union {
	uint64_t        raw;
	struct {
	    uint64_t  bytes:16;
	    uint64_t offset:32;
	} __attribute__ ((packed));
    } __attribute__ ((packed)) idtr;

    idtr.raw = rd_idtr ();

    int amd64_idt_entry_size = 16;
    int             idt_elts = (idtr.bytes + 1) / amd64_idt_entry_size;
    struct gate_descriptor (*idt)[256] __attribute__ ((aligned (16))) =
      (struct gate_descriptor (*)[256]) (uint64_t) idtr.offset;

    printf ("Dumping %d entries of IDT at %x (raw IDTR=%lx):\n", idt_elts, idtr.offset, idtr.raw);
    for (int i = 0; i < idt_elts; i++)
	dump_gate_descriptor (&(*idt)[i], "IDT", i);
    printf ("-- end of IDT --\n");
}

/*
 * Stack walker internals
 */
#if defined (CONFIG_KERNEL_STACK_TRACE)

static inline uint64_t __attribute__((always_inline)) rdrbp (void)
{
    uint64_t rbp;
    __asm__("movq %%rbp, %0" : "=r" (rbp) :);
    return rbp;
}

struct stack_frame {
    struct stack_frame *next_frame;
    uint64_t            return_address;
};

static uint64_t debug_stackwalker_stack_top, debug_stackwalker_stack_bottom = -1;
static uint64_t debug_stackwalker_text_start, debug_stackwalker_text_end;
static struct Elf64_Sym
               *debug_stackwalker_dynsyms;
static    char *debug_stackwalker_dynstr;
static      int debug_stackwalker_nsyms;
const       int debug_stackwalker_max_frames = 16;

static bool debug_stackwalker_initialized_p (void)
{
    return debug_stackwalker_stack_bottom != -1;
}

static int stack_pointer_valid_p (uint64_t ptr, uint64_t top, uint64_t bottom)
{
    return bottom <= ptr && ptr < top && !(ptr & 0x7);
}

static void walk_stack (uint64_t rbp, uint64_t stack_top, uint64_t stack_bottom,
			int max_frames, void (*walk_callback) (struct stack_frame *))
{
    struct stack_frame *frame = (struct stack_frame *) rbp;
    int i = 0;
    while ((! max_frames || i < max_frames) &&
	   stack_pointer_valid_p ((uint64_t) frame->next_frame, stack_top, stack_bottom)) {
	walk_callback (frame);
	frame = frame->next_frame;
	i++;
    }
}

static uint64_t maybe_local_phys_to_mem (uint64_t addr)
{
    return addr < X86_64_PADDR_SPACE_LIMIT ? local_phys_to_mem (addr) : addr;
}

static uint64_t maybe_mem_to_local_phys (uint64_t addr)
{
    return addr < X86_64_PADDR_SPACE_LIMIT ? addr : mem_to_local_phys (addr);
}

/**
 * \brief Normalize an address to correspond to the unrelocated ELF symbols.
 */
static uint64_t addr_to_elf (uint64_t addr)
{
    if (addr == (uint64_t) -1)
	return addr;
    uint64_t phys_start = mem_to_local_phys (debug_stackwalker_text_start);
    return addr < phys_start ? addr : maybe_mem_to_local_phys (addr) - phys_start;
}

static bool try_explain_address (uint64_t addr, int n, char *name, int64_t *delta, struct Elf64_Sym *sym, uint64_t next_value, char *next_name) {
    if        (addr < sym->st_value + sym->st_size) {
        *delta = addr - sym->st_value;
        snprintf (name, n, "%s", & debug_stackwalker_dynstr[sym->st_name]);
        return true;
    } else if (addr < next_value) {
        *delta = addr - next_value;
        snprintf (name, n, "past %s(0x%lx+0x%lx), before %s(0x%lx)",
                  & debug_stackwalker_dynstr[sym->st_name], addr_to_elf (sym->st_value), sym->st_size,
                  next_name, addr_to_elf (next_value));
        return true;
    }
    return false;
}

static void resolve_addr2sym (uint64_t addr, int n, char *name, int64_t *delta)
{
    addr = maybe_local_phys_to_mem (addr);
    *delta = addr - _start_kernel;

    if (! debug_stackwalker_initialized_p ()) {
	snprintf (name, n, "<symbols unavailable> _start_kernel");
	return;
    }

    /* First, rule out outright misses: */
    if        (addr < debug_stackwalker_text_start) {
	snprintf (name, n, "<below .text> _start_kernel");
	return;
    } else if (addr >= debug_stackwalker_text_end) {
	snprintf (name, n, "<above .text> _start_kernel");
	return;
    } else if (addr < debug_stackwalker_dynsyms[0].st_value) {
	snprintf (name, n, "<before first symbol> .text");
	*delta = addr - debug_stackwalker_text_start;
	return;
    }

    /* Next, handle first N-1 symbols: */
    for (int i = 0; i < debug_stackwalker_nsyms - 1; i++) {
	struct Elf64_Sym     *sym = & debug_stackwalker_dynsyms[i];
	struct Elf64_Sym *nextsym = sym + 1;
	if (try_explain_address (addr, n, name, delta, sym, nextsym->st_value, & debug_stackwalker_dynstr[nextsym->st_name]))
	    return;
    }

    /* Finally, the special case of the last symbol: */
    struct Elf64_Sym *sym = & debug_stackwalker_dynsyms[debug_stackwalker_nsyms - 1];
    if (try_explain_address (addr, n, name, delta, sym, debug_stackwalker_text_end, "<end-of-.text>"))
	return;

    snprintf (name, n, "<algorithm failure> _start_kernel");
    return;
}

static uint64_t __attribute__((used)) resolve_sym2addr (char *symname)
{
    if (! debug_stackwalker_initialized_p ())
	return (uint64_t) -1;

    for (int i = 0; i < debug_stackwalker_nsyms; i++) {
	struct Elf64_Sym *sym = & debug_stackwalker_dynsyms[i];
	if (! strcmp (symname, & debug_stackwalker_dynstr[sym->st_name]))
	    return sym->st_value;
    }

    return (uint64_t) -1;
}

static void print_frame(struct stack_frame * frame)
{
    uint64_t    addr = frame->return_address;
    char symbol_name[128] = { 0 };
    int64_t    delta;
    resolve_addr2sym (addr, 127, symbol_name, &delta);

    printf ("%16lx ELF:%lx %20s %s 0x%x\n\r",
            addr, addr_to_elf (addr), symbol_name, delta < 0 ? "-" : "+", (uint32_t) (delta < 0 ? -delta : delta));
}

static void __dump_stack (uint64_t rbp)
{
    if (! debug_stackwalker_initialized_p ()) {
	printf("ERROR: stack walker is not initialized\n\r");
	return;
    }
    printf ("    call trace (rbp: %lx, stack: %lx-%lx, text: %lx-%lx):\n\r",
	    rbp, debug_stackwalker_stack_top, debug_stackwalker_stack_bottom, debug_stackwalker_text_start, debug_stackwalker_text_end);
    walk_stack (rbp, debug_stackwalker_stack_top, debug_stackwalker_stack_bottom, debug_stackwalker_max_frames, print_frame);
}

static int compare_syms (const struct Elf64_Sym *a, const struct Elf64_Sym *b)
{
    return (a->st_value <  b->st_value ? -1 :
	    a->st_value == b->st_value ? 0  :
	    1);
}
#endif

/*
 * Stack walker public API
 */
#define _unused __attribute__((unused))

void debug_setup_stackwalker (_unused uint64_t stack_top, _unused uint64_t stack_bottom, _unused uint64_t text_start, _unused uint64_t text_end, _unused struct Elf64_Sym *dynsyms, _unused char *dynstr, _unused int nsyms)
{
#if defined (CONFIG_KERNEL_STACK_TRACE)
    uint64_t rbp = rdrbp (), rsp;
    rsp = (uint64_t) &rsp;

    if (! stack_pointer_valid_p (rbp, stack_top, stack_bottom)) {
	printf ("WARNING:  refusing to initialize stackwalker with rbp=0x%lx (rsp=0x%lx) outside of specified 0x%lx-0x%lx\n"
		, rbp, rsp, stack_top, stack_bottom);
	return;
    }

    debug_stackwalker_stack_top    = stack_top;
    debug_stackwalker_stack_bottom = stack_bottom;
    debug_stackwalker_text_start   = text_start;
    debug_stackwalker_text_end     = text_end;
    debug_stackwalker_dynsyms      = dynsyms;
    debug_stackwalker_dynstr       = dynstr;
    debug_stackwalker_nsyms        = nsyms;

    printf ("Initialized stack walker with stack 0x%lx-0x%lx, text 0x%lx-0x%lx (rbp=%lx, rsp=%lx)\n"
	    , stack_top, stack_bottom, text_start, text_end, rbp, rsp);
#else
    printf ("WARNING:  refusing to initialize stackwalker: disabled by CONFIG_KERNEL_STACK_TRACE\n");
#endif
}

void debug_sort_dynsyms (struct Elf64_Sym *dynsyms, int n)
{
#if defined (CONFIG_KERNEL_STACK_TRACE)
    qsort (dynsyms, n, sizeof (struct Elf64_Sym), (int (*)(const void *, const void *)) compare_syms);
#endif
}

void debug_relocate_dynsyms (struct Elf64_Sym *dynsyms, int n, uint64_t offset)
{
    for (int i = 0; i < n; i++)
	dynsyms[i].st_value += offset;
}

void dump_stack (void)
{
#if defined (CONFIG_KERNEL_STACK_TRACE)
    __dump_stack (rdrbp ());
#else
    printf ("WARNING:  not dumping stack: disabled by CONFIG_KERNEL_STACK_TRACE\n");
#endif
}
