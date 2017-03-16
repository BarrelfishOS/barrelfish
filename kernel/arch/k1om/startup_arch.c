/**
 * \file
 * \brief x86_64 kernel bootup code.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <string.h>
#include <paging_kernel_arch.h>
#include <elf/elf.h>
#include <kernel_multiboot.h>
#include <irq.h>
#include <init.h>
#include <barrelfish_kpi/cpu.h>
#include <exec.h>
#include <getopt/getopt.h>
#include <dispatch.h>
#include <barrelfish_kpi/init.h>
#include <arch/x86/apic.h>
#include <barrelfish_kpi/paging_arch.h>
#include <barrelfish_kpi/syscalls.h>
#include <target/x86/barrelfish_kpi/coredata_target.h>
#include <kputchar.h>
#include <startup.h>
#include <arch/x86/startup_x86.h>
#include <arch/x86/start_aps.h>

#include <xeon_phi.h>
#include <xeon_phi/xeon_phi.h>

/// Quick way to find the base address of a cnode capability
#define CNODE(cte)     get_address(&(cte)->cap)

/**
 * init's needed boot pages.
 */
#define INIT_PDPT_SIZE          X86_64_PDPT_ENTRIES(K1OM_INIT_SPACE_LIMIT)
#define INIT_PDIR_SIZE          X86_64_PDIR_ENTRIES(K1OM_INIT_SPACE_LIMIT)
#define INIT_PTABLE_SIZE        X86_64_PTABLE_ENTRIES(K1OM_INIT_SPACE_LIMIT)
#define INIT_PAGE_BITMAP        X86_64_PTABLE_PRESENT

/// Pointer to bootinfo structure for init
static struct bootinfo *bootinfo = (struct bootinfo *)BOOTINFO_BASE;

struct spawn_state spawn_state;

/**
 * Page map level 4 table for init user address space.
 */
static union x86_64_pdir_entry *init_pml4; //[PTABLE_SIZE]

/**
 * Page directory pointer table for init user address space.
 */
static union x86_64_pdir_entry *init_pdpt; //[INIT_PDPT_SIZE][PTABLE_SIZE]

/**
 * Page directory for init user address space.
 */
static union x86_64_pdir_entry *init_pdir; //[INIT_PDPT_SIZE][INIT_PDIR_SIZE][PTABLE_SIZE]

/**
 * Page tables for init user address space.
 */
static union x86_64_ptable_entry *init_ptable; //[INIT_PDPT_SIZE][INIT_PDIR_SIZE][INIT_PTABLE_SIZE][PTABLE_SIZE]

/**
 * \brief Convert elf flags to page flags
 *
 * \param flags ELF64 program segment flags.
 *
 * \return page flags.
 *
 * Not all combinations may be supported by an architecture
 */
static uint64_t paging_elf_to_page_flags(uint32_t flags)
{
    uint64_t pageflags = 0;

    pageflags |= flags & PF_R ? PTABLE_USER_SUPERVISOR : 0;
    pageflags |= flags & PF_W ? PTABLE_READ_WRITE : 0;
    pageflags |= flags & PF_X ? 0 : PTABLE_EXECUTE_DISABLE;

    return pageflags;
}

/**
 * \brief Map init user-space memory.
 *
 * This function maps pages of the init user-space module. It expects
 * the virtual base address 'vbase' of a program segment of the init executable,
 * its size 'size' and its ELF64 access control flags. It maps pages
 * to the sequential area of physical memory, given by 'base'. If you
 * want to allocate physical memory frames as you go, you better use
 * startup_alloc_init().
 *
 * \param vbase Virtual base address of program segment.
 * \param base  Physical base address of program segment.
 * \param size  Size of program segment in bytes.
 * \param flags ELF64 access control flags of program segment.
 */
errval_t startup_map_init(lvaddr_t vbase, lpaddr_t base, size_t size,
                          uint32_t flags)
{
    lvaddr_t vaddr;

    paging_align(&vbase, &base, &size, BASE_PAGE_SIZE);

    assert(vbase + size - K1OM_INIT_VBASE < K1OM_INIT_SPACE_LIMIT);

    // Map pages
    for(vaddr = vbase; vaddr < vbase + size;
        vaddr += BASE_PAGE_SIZE, base += BASE_PAGE_SIZE) {
        lvaddr_t baddr = vaddr - K1OM_INIT_VBASE;
        union x86_64_ptable_entry *ptable_base = &init_ptable[
                    X86_64_PML4_BASE(baddr) * X86_64_PTABLE_SIZE *
                    X86_64_PTABLE_SIZE * X86_64_PTABLE_SIZE +
                    X86_64_PDPT_BASE(baddr) * X86_64_PTABLE_SIZE *
                    X86_64_PTABLE_SIZE + X86_64_PDIR_BASE(baddr) *
                    X86_64_PTABLE_SIZE + X86_64_PTABLE_BASE(vaddr)];

        debug(SUBSYS_PAGING, "Mapping 4K page: vaddr = 0x%lx, base = 0x%lx, "
              "PML4_BASE = %lu, PDPT_BASE = %lu, PDIR_BASE = %lu, "
              "PTABLE_BASE = %lu -- ", vaddr, base, X86_64_PML4_BASE(baddr),
              X86_64_PDPT_BASE(baddr), X86_64_PDIR_BASE(baddr),
              X86_64_PTABLE_BASE(baddr));

        if(!X86_64_IS_PRESENT(ptable_base)) {
            debug(SUBSYS_PAGING, "mapped!\n");
            paging_x86_64_map(ptable_base, base,
                              INIT_PAGE_BITMAP | paging_elf_to_page_flags(flags));
        } else {
            debug(SUBSYS_PAGING, "already existing!\n");
        }
    }

    return SYS_ERR_OK;
}

/// Create physical address range or RAM caps to unused physical memory
static void create_phys_caps(lpaddr_t init_alloc_addr)
{
    errval_t err;

    // map first meg of RAM, which contains lots of crazy BIOS tables
    err = create_caps_to_cnode(0, K1OM_START_KERNEL_PHYS,
                               RegionType_PlatformData, &spawn_state, bootinfo);
    assert(err_is_ok(err));

    char *mmap_addr = MBADDR_ASSTRING(glbl_core_data->mmap_addr);
    lpaddr_t last_end_addr = 0;

    struct multiboot_mmap *mbi_mmaps = (struct multiboot_mmap *)mmap_addr;
    uint8_t swapped;
    do {
        swapped = false;
        for (uint32_t i = 1; i < glbl_core_data->mmap_length / sizeof(struct multiboot_mmap); ++i) {
            if (mbi_mmaps[i-1].base_addr > mbi_mmaps[i].base_addr) {
                struct multiboot_mmap tmp = mbi_mmaps[i-1];
                mbi_mmaps[i-1] = mbi_mmaps[i];
                mbi_mmaps[i] = tmp;
                swapped = true;
            }
        }
    } while(swapped);

    for(char *m = mmap_addr; m < mmap_addr + glbl_core_data->mmap_length;) {
        struct multiboot_mmap *mmap = (struct multiboot_mmap * SAFE)TC(m);

        debug(SUBSYS_STARTUP, "MMAP %lx--%lx Type %u\n",
              mmap->base_addr, mmap->base_addr + mmap->length,
              mmap->type);

        if (last_end_addr >= init_alloc_addr
            && mmap->base_addr > last_end_addr) {
            /* we have a gap between regions. add this as a physaddr range */
            debug(SUBSYS_STARTUP, "physical address range %lx--%lx\n",
                  last_end_addr, mmap->base_addr);

            err = create_caps_to_cnode(last_end_addr,
                                       mmap->base_addr - last_end_addr,
                                       RegionType_PhyAddr, &spawn_state, bootinfo);
            assert(err_is_ok(err));
        }

        if (mmap->type == MULTIBOOT_MEM_TYPE_RAM) {
            genpaddr_t base_addr = mmap->base_addr;
            genpaddr_t end_addr = base_addr + mmap->length;

            // only map the rest of RAM which is greater than init_alloc_addr
            if (end_addr > local_phys_to_gen_phys(init_alloc_addr)) {
                if (base_addr < local_phys_to_gen_phys(init_alloc_addr)) {
                    base_addr = local_phys_to_gen_phys(init_alloc_addr);
                }
                debug(SUBSYS_STARTUP, "RAM %lx--%lx\n", base_addr, end_addr);
                err = create_caps_to_cnode(base_addr, end_addr - base_addr,
                                           RegionType_Empty, &spawn_state, bootinfo);
                if (err_no(err) == SYS_ERR_SLOTS_IN_USE) {
                    printk(LOG_WARN, "not able to create RAM caps for all physical memory in the system, CNode full\n");
                }
                assert(err_is_ok(err));
            }
        } else if (mmap->base_addr > local_phys_to_gen_phys(init_alloc_addr)) {
            /* XXX: The multiboot spec just says that mapping types other than
             * RAM are "reserved", but GRUB always maps the ACPI tables as type
             * 3, and things like the IOAPIC tend to show up as type 2 or 4,
             * so we map all these regions as platform data
             */
            debug(SUBSYS_STARTUP, "platform %lx--%lx\n", mmap->base_addr,
                  mmap->base_addr + mmap->length);
            assert(mmap->base_addr > local_phys_to_gen_phys(init_alloc_addr));
            err = create_caps_to_cnode(mmap->base_addr, mmap->length,
                                       RegionType_PlatformData, &spawn_state, bootinfo);
            assert(err_is_ok(err));
        }

        last_end_addr = mmap->base_addr + mmap->length;
        m += mmap->size;
    }

    assert(last_end_addr != 0);
}

#define NEEDED_KERNEL_SPACE \
    ((SIZE_KERNEL_IMAGE & 0x1000 ) == SIZE_KERNEL_IMAGE ? \
    SIZE_KERNEL_IMAGE : \
    (SIZE_KERNEL_IMAGE & 0xfffffffffffff000) + 0x1000)

#define OBJSPERPAGE_CTE         (1UL << (BASE_PAGE_BITS - OBJBITS_CTE))

static void init_page_tables(struct spawn_state *st, alloc_phys_func alloc_phys)
{
    /* Allocate memory for init's page tables */
    init_pml4 = (void *)local_phys_to_mem(
                alloc_phys(X86_64_PTABLE_SIZE * sizeof(union x86_64_pdir_entry)));
    init_pdpt = (void *)local_phys_to_mem(
                alloc_phys(X86_64_PTABLE_SIZE * INIT_PDPT_SIZE
                           * sizeof(union x86_64_pdir_entry)));
    init_pdir = (void *)local_phys_to_mem(
                alloc_phys(X86_64_PTABLE_SIZE * INIT_PDPT_SIZE * INIT_PDIR_SIZE
                           * sizeof(union x86_64_pdir_entry)));
    init_ptable = (void *)local_phys_to_mem(
                alloc_phys(X86_64_PTABLE_SIZE * INIT_PDPT_SIZE * INIT_PDIR_SIZE
                           * INIT_PTABLE_SIZE * sizeof(union x86_64_ptable_entry)));

    /* Page table setup */
    /* Initialize init page tables */
    for(size_t i = 0; i < INIT_PDPT_SIZE; i++) {
        paging_x86_64_clear_pdir(&init_pdpt[i]);
        for(size_t j = 0; j < INIT_PDIR_SIZE; j++) {
            paging_x86_64_clear_pdir(&init_pdir[i * PTABLE_SIZE + j]);
            for(size_t k = 0; k < INIT_PTABLE_SIZE; k++) {
                paging_x86_64_clear_ptable(
                &init_ptable[i * PTABLE_SIZE * PTABLE_SIZE + j * PTABLE_SIZE + k]);
            }
        }
    }
    /* Map pagetables into pageCN */
    int     pagecn_pagemap = 0;
    // Map PML4 (slot 0 in pagecn)
    caps_create_new(ObjType_VNode_x86_64_pml4, mem_to_local_phys((lvaddr_t)init_pml4),
                    BASE_PAGE_SIZE, 0, my_core_id,
                    caps_locate_slot(CNODE(st->pagecn), pagecn_pagemap++));
    // Map PDPT into successive slots in pagecn
    for(size_t i = 0; i < INIT_PDPT_SIZE; i++) {
        caps_create_new(ObjType_VNode_x86_64_pdpt,
                        mem_to_local_phys((lvaddr_t)init_pdpt) + i * BASE_PAGE_SIZE,
                        BASE_PAGE_SIZE, 0, my_core_id,
                        caps_locate_slot(CNODE(st->pagecn), pagecn_pagemap++));
    }
    // Map PDIR into successive slots in pagecn
    for(size_t i = 0; i < INIT_PDIR_SIZE; i++) {
        caps_create_new(ObjType_VNode_x86_64_pdir,
                        mem_to_local_phys((lvaddr_t)init_pdir) + i * BASE_PAGE_SIZE,
                        BASE_PAGE_SIZE, 0, my_core_id,
                        caps_locate_slot(CNODE(st->pagecn), pagecn_pagemap++));
    }
    // Map page tables into successive slots in pagecn
    for(size_t i = 0; i < INIT_PTABLE_SIZE; i++) {
        caps_create_new(ObjType_VNode_x86_64_ptable,
                        mem_to_local_phys((lvaddr_t)init_ptable) + i * BASE_PAGE_SIZE,
                        BASE_PAGE_SIZE, 0, my_core_id,
                        caps_locate_slot(CNODE(st->pagecn), pagecn_pagemap++));
    }
    // Connect all page tables to page directories.
    // init's memory manager expects page tables within the pagecn to
    // already be connected to the corresponding directories. To avoid
    // unneccessary special cases, we connect them here.
    for(lvaddr_t vaddr = K1OM_INIT_VBASE; vaddr < K1OM_INIT_SPACE_LIMIT;
        vaddr += BASE_PAGE_SIZE) {
        lvaddr_t baddr = vaddr - K1OM_INIT_VBASE;
        union x86_64_pdir_entry *pml4_base, *pdpt_base, *pdir_base;
        union x86_64_ptable_entry *ptable_base;
        pml4_base = &init_pml4[X86_64_PML4_BASE(vaddr)];
        pdpt_base = &init_pdpt[X86_64_PML4_BASE(baddr) * X86_64_PTABLE_SIZE +
                               X86_64_PDPT_BASE(vaddr)];
        pdir_base = &init_pdir[X86_64_PML4_BASE(baddr) * X86_64_PTABLE_SIZE *
                               X86_64_PTABLE_SIZE
                               + X86_64_PDPT_BASE(baddr) * X86_64_PTABLE_SIZE
                               + X86_64_PDIR_BASE(vaddr)];
        ptable_base = &init_ptable[X86_64_PML4_BASE(baddr) * X86_64_PTABLE_SIZE *
                                   X86_64_PTABLE_SIZE * X86_64_PTABLE_SIZE +
                                   X86_64_PDPT_BASE(baddr) * X86_64_PTABLE_SIZE *
                                   X86_64_PTABLE_SIZE + X86_64_PDIR_BASE(baddr) *
                                   X86_64_PTABLE_SIZE + X86_64_PTABLE_BASE(vaddr)];

        paging_x86_64_map_table(pml4_base, mem_to_local_phys((lvaddr_t)pdpt_base));
        paging_x86_64_map_table(pdpt_base, mem_to_local_phys((lvaddr_t)pdir_base));
        paging_x86_64_map_table(pdir_base, mem_to_local_phys((lvaddr_t)ptable_base));
    }

    /* Initialize and switch to init's PML4 */
    paging_x86_64_make_good_pml4(mem_to_local_phys((lvaddr_t)init_pml4));
    paging_x86_64_context_switch(mem_to_local_phys((lvaddr_t)init_pml4));

    /***** VSpace available now *****/
}

static struct dcb *spawn_init_common(struct spawn_state *st, const char *name,
                                     int argc, const char *argv[],
                                     lpaddr_t bootinfo_phys,
                                     alloc_phys_func alloc_phys,
                                     alloc_phys_aligned_func alloc_phys_aligned)
{
    errval_t err;

    /* Perform arch-independent spawn */
    lvaddr_t paramaddr;
    struct dcb *init_dcb = spawn_module(st, name, argc, argv, bootinfo_phys,
                                        ARGS_BASE, alloc_phys, alloc_phys_aligned,
                                        &paramaddr);

    /* Init page tables */
    init_page_tables(st, alloc_phys);

    /* Map cmdline args R/W into VSpace at ARGS_BASE */
    paging_x86_64_map_table(&init_pml4[X86_64_PML4_BASE(ARGS_BASE)],
                            mem_to_local_phys((lvaddr_t)init_pdpt));
    paging_x86_64_map_table(&init_pdpt[X86_64_PDPT_BASE(ARGS_BASE)],
                            mem_to_local_phys((lvaddr_t)init_pdir));
    paging_x86_64_map_table(&init_pdir[X86_64_PDIR_BASE(ARGS_BASE)],
                            mem_to_local_phys((lvaddr_t)init_ptable));
    for (int i = 0; i < ARGS_SIZE / BASE_PAGE_SIZE; i++) {
        paging_x86_64_map(&init_ptable[X86_64_PTABLE_BASE(ARGS_BASE) + i],
                          st->args_page + i * BASE_PAGE_SIZE,
                          INIT_PAGE_BITMAP | paging_elf_to_page_flags(PF_R|PF_W));
    }

    /* Map dispatcher frame R/W into VSpace */
    paging_x86_64_map_table(&init_pml4[X86_64_PML4_BASE(DISPATCHER_BASE)],
                            mem_to_local_phys((lvaddr_t)init_pdpt));
    paging_x86_64_map_table(&init_pdpt[X86_64_PDPT_BASE(DISPATCHER_BASE)],
                            mem_to_local_phys((lvaddr_t)init_pdir));
    paging_x86_64_map_table(&init_pdir[X86_64_PDIR_BASE(DISPATCHER_BASE)],
                            mem_to_local_phys((lvaddr_t)init_ptable));
    for (int i = 0; i < (1 << DISPATCHER_FRAME_BITS) / BASE_PAGE_SIZE; i++) {
        paging_x86_64_map(&init_ptable[X86_64_PTABLE_BASE(DISPATCHER_BASE) + i],
                          mem_to_local_phys(init_dcb->disp) + i * BASE_PAGE_SIZE,
                          INIT_PAGE_BITMAP | paging_elf_to_page_flags(PF_R|PF_W));
    }

    struct dispatcher_shared_generic *init_disp =
        get_dispatcher_shared_generic(init_dcb->disp);
    struct dispatcher_shared_x86_64 *init_disp_x86_64 =
        get_dispatcher_shared_x86_64(init_dcb->disp);

    registers_set_param(&init_disp_x86_64->enabled_save_area, paramaddr);

    // Map IO cap in task cnode
    struct cte *iocap = caps_locate_slot(CNODE(st->taskcn), TASKCN_SLOT_SYSMEM);
    err = caps_create_new(ObjType_DevFrame, XEON_PHI_SYSMEM_BASE,
                          1UL << XEON_PHI_SYSMEM_SIZE_BITS,
                          1UL << XEON_PHI_SYSMEM_SIZE_BITS,
                          my_core_id, iocap);
    /*
     * XXX: there is no IO on the xeon phi, we use this slot to put in the
     *      capability to the host memory, as this can be seen as IO
     */
    struct cte *mmiocap = caps_locate_slot(CNODE(st->taskcn), TASKCN_SLOT_IO);
    err = caps_create_new(ObjType_DevFrame, XEON_PHI_SBOX_BASE,
                          1UL << XEON_PHI_SBOX_SIZE_BITS,
                          1UL << XEON_PHI_SBOX_SIZE_BITS,
                          my_core_id, mmiocap);

    struct cte *coreboot = caps_locate_slot(CNODE(st->taskcn), TASKCN_SLOT_COREBOOT);
    // XXX: make the 64k below a named constant
    err = caps_create_new(ObjType_DevFrame, 0, 65536, 65536, my_core_id, coreboot);

    assert(err_is_ok(err));

    /* Set fields in DCB */
    // Set Vspace
    init_dcb->vspace = mem_to_local_phys((lvaddr_t)init_pml4);

    // init dispatcher
    init_disp->disabled = true;
    strncpy(init_disp->name, argv[0], DISP_NAME_LEN);

    /* tell init the vspace addr of its dispatcher */
    init_disp->udisp = DISPATCHER_BASE;

    init_disp->xeon_phi_id = glbl_core_data->xeon_phi_id;

    init_disp_x86_64->disabled_save_area.rdi = DISPATCHER_BASE;
    init_disp_x86_64->disabled_save_area.fs = 0;
    init_disp_x86_64->disabled_save_area.gs = 0;
    init_disp_x86_64->disabled_save_area.eflags = USER_EFLAGS;

    return init_dcb;
}

struct dcb *spawn_bsp_init(const char *name)
{
    errval_t err;

    /* Only the first core can run this code */
    assert(apic_is_bsp());

    /* Allocate bootinfo */
    lpaddr_t bootinfo_phys = bsp_alloc_phys(BOOTINFO_SIZE);
    memset((void *)local_phys_to_mem(bootinfo_phys), 0, BOOTINFO_SIZE);

    /* Construct cmdline args */
    char bootinfochar[16];
    snprintf(bootinfochar, sizeof(bootinfochar), "%lu", BOOTINFO_BASE);
    const char *argv[] = { "init", bootinfochar };

    struct dcb *init_dcb = spawn_init_common(&spawn_state, name,
                                             ARRAY_LENGTH(argv), argv,
                                             bootinfo_phys, bsp_alloc_phys,
                                             bsp_alloc_phys_aligned);

    /* Map bootinfo R/W into VSpace at vaddr BOOTINFO_BASE */
    paging_x86_64_map_table(&init_pml4[0], mem_to_local_phys((lvaddr_t)init_pdpt));
    paging_x86_64_map_table(&init_pdpt[0], mem_to_local_phys((lvaddr_t)init_pdir));
    paging_x86_64_map_table(&init_pdir[1], mem_to_local_phys((lvaddr_t)init_ptable));
    for (int i = 0; i < BOOTINFO_SIZE / BASE_PAGE_SIZE; i++) {
        paging_x86_64_map(&init_ptable[i], bootinfo_phys + i * BASE_PAGE_SIZE,
                   INIT_PAGE_BITMAP | paging_elf_to_page_flags(PF_R|PF_W));
    }

    /* Load init ELF64 binary from multiboot */
    struct multiboot_modinfo *module = multiboot_find_module(name);
    if (module == NULL) {
        panic("Could not find init module!");
    }
    lvaddr_t init_ep;
    err = elf_load(EM_K1OM, startup_alloc_init, &spawn_state,
                   local_phys_to_mem(module->mod_start),
                   MULTIBOOT_MODULE_SIZE(*module), &init_ep);
    if (err_is_fail(err)) {
        //err_print_calltrace(err);
        panic("ELF load of init module failed!");
    }

    struct dispatcher_shared_x86_64 *init_disp_x86_64 =
        get_dispatcher_shared_x86_64(init_dcb->disp);
    init_disp_x86_64->disabled_save_area.rip = init_ep;

    /* Create caps for init to use */
    create_module_caps(&spawn_state);
    lpaddr_t init_alloc_end = bsp_alloc_phys(0);
    create_phys_caps(init_alloc_end);

    /* Fill bootinfo struct */
    bootinfo->mem_spawn_core = NEEDED_KERNEL_SPACE; // Size of kernel

    bootinfo->host_msg = glbl_core_data->bp->msg_base;
    bootinfo->host_msg_bits = glbl_core_data->bp->msg_size_bits;

    return init_dcb;
}

struct dcb *spawn_app_init(struct x86_core_data *core_data, const char *name)
{
    errval_t err;

    /* Construct cmdline args */
    // Core id of the core that booted this core
    char coreidchar[16];
    snprintf(coreidchar, sizeof(coreidchar), "%d", core_data->src_core_id);

    // IPI channel id of core that booted this core
    char chanidchar[30];
    snprintf(chanidchar, sizeof(chanidchar), "chanid=%d", core_data->chan_id);

    // Arch id of the core that booted this core
    char archidchar[30];
    snprintf(archidchar, sizeof(archidchar), "archid=%" PRIuHWID,
             core_data->src_arch_id);

    const char *argv[] = { name, coreidchar, chanidchar, archidchar };

    struct dcb *init_dcb = spawn_init_common(&spawn_state, name,
                                             ARRAY_LENGTH(argv), argv,
                                             0, app_alloc_phys,
                                             app_alloc_phys_aligned);

    // Urpc frame cap
    struct cte *urpc_frame_cte = caps_locate_slot(CNODE(spawn_state.taskcn),
                                                  TASKCN_SLOT_MON_URPC);
    // use fact that cap is foreign to avoid zeroing it
    assert(core_data->src_core_id != my_core_id);
    err = caps_create_new(ObjType_Frame, core_data->urpc_frame_base,
                          1UL << core_data->urpc_frame_bits,
                          1UL << core_data->urpc_frame_bits, core_data->src_core_id,
                          urpc_frame_cte);
    assert(err_is_ok(err));
    lpaddr_t urpc_ptr = gen_phys_to_local_phys(urpc_frame_cte->cap.u.frame.base);

    /* Map urpc frame at MON_URPC_BASE */
    paging_x86_64_map_table(&init_pml4[X86_64_PML4_BASE(MON_URPC_BASE)],
                            mem_to_local_phys((lvaddr_t)init_pdpt));
    paging_x86_64_map_table(&init_pdpt[X86_64_PDPT_BASE(MON_URPC_BASE)],
                            mem_to_local_phys((lvaddr_t)init_pdir));
    paging_x86_64_map_table(&init_pdir[X86_64_PDIR_BASE(MON_URPC_BASE)],
                            mem_to_local_phys((lvaddr_t)init_ptable));
    for (int i = 0; i < MON_URPC_SIZE / BASE_PAGE_SIZE; i++) {
        paging_x86_64_map(&init_ptable[X86_64_PTABLE_BASE(MON_URPC_BASE) + i],
                          urpc_ptr + i * BASE_PAGE_SIZE,
                          INIT_PAGE_BITMAP | paging_elf_to_page_flags(PF_R | PF_W));
    }

    // elf load the domain
    genvaddr_t entry_point;
    err = elf_load(EM_K1OM, startup_alloc_init, &spawn_state,
                   local_phys_to_mem(core_data->monitor_binary),
                   core_data->monitor_binary_size, &entry_point);
    if (err_is_fail(err)) {
        //err_print_calltrace(err);
        panic("ELF load of init module failed!");
    }

    struct dispatcher_shared_x86_64 *init_disp_x86_64 =
        get_dispatcher_shared_x86_64(init_dcb->disp);
    init_disp_x86_64->disabled_save_area.rip = entry_point;

    return init_dcb;
}
