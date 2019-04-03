/**
 * \file
 * \brief Architecture-independent bootstrap code.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include <stdio.h>
#include <kernel.h>
#include <startup.h>
#include <exec.h>
#include <dispatch.h>
#include <barrelfish_kpi/init.h>
#include <barrelfish_kpi/paging_arch.h>
#include <barrelfish_kpi/domain_params.h>
#include <kcb.h>
#include <mdb/mdb_tree.h>
#include <trace/trace.h>

struct kcb *kcb_current = NULL;

coreid_t my_core_id;

/// Quick way to find the base address of a cnode capability
#define CNODE(cte)     get_address(&(cte)->cap)

/**
 * \brief Create caps in 'cnode'
 *
 * This function creates untyped caps to the RAM at physical address 'base_addr'
 * and size 'size' and adds them to a cnode for the init task. The bootinfo is
 * updated accordingly.
 *
 * \param base_addr The physical base address of the RAM for which caps have to
 *                  be created
 * \param size      The size of the physical region
 * \param type      Region type to create
 * \param st        spawn_state structure to update
 * \param bootinfo  bootinfo structure to update
 */
errval_t create_caps_to_cnode(lpaddr_t base_addr, size_t size,
                              enum region_type type,
                              struct spawn_state *st, struct bootinfo *bootinfo)
{
    struct mem_region *regions = bootinfo->regions;
    size_t *regions_index = &bootinfo->regions_length;
    struct capability *cnode;
    cslot_t *slot;
    enum objtype cap_type;
    errval_t err;

    // determine destination and cap type
    switch(type) {
    case RegionType_Empty:
        cap_type = ObjType_RAM;
        cnode = &st->supercn->cap;
        slot = &st->supercn_slot;
        break;

    case RegionType_PhyAddr:
    case RegionType_PlatformData:
        cap_type = ObjType_PhysAddr;
        cnode = &st->physaddrcn->cap;
        slot = &st->physaddrcn_slot;
        break;

    case RegionType_RootTask:
        cap_type = ObjType_Frame;
        cnode = &st->segcn->cap;
        slot = &st->segcn_slot;
        break;

    default:
        panic("Cannot handle bootinfo region type!");
    }

    if (*slot >= cnode_get_slots(cnode)) {
        printk(LOG_WARN, "create_caps_to_cnode: Cannot create more caps "
               "in CNode\n");
        return SYS_ERR_SLOTS_IN_USE;
    }
    /* Cannot insert anymore into the mem_region */
    if (*regions_index >= MAX_MEM_REGIONS) {
        printk(LOG_WARN, "create_caps_to_cnode: mem_region out of space\n");
        return -1;
    }

    /* create the capability */
    err = caps_create_new(cap_type, base_addr, size, size, my_core_id,
            caps_locate_slot(get_address(cnode), (*slot)++));
    if (err_is_fail(err)) {
        return err;
    }

    /* record region */
    assert(regions != NULL);
    regions[*regions_index].mr_base = base_addr;
    regions[*regions_index].mr_type = type;
    regions[*regions_index].mr_bytes = size;
    regions[*regions_index].mr_consumed = false;
    regions[*regions_index].mrmod_size = 0;
    regions[*regions_index].mrmod_data = 0;
    (*regions_index)++;

    return SYS_ERR_OK;
}

struct dcb *spawn_module(struct spawn_state *st,
                         const char *name, int argc, const char** argv,
                         lpaddr_t bootinfo, lvaddr_t args_base,
                         alloc_phys_func alloc_phys,
                         alloc_phys_aligned_func alloc_phys_aligned,
                         lvaddr_t *retparamaddr)
{
    errval_t err;

    printf("spawn module: %s\n", name);

    // check for reuse of static state
#ifndef NDEBUG
    static bool once_only;
    assert(!once_only);
    once_only = true;
#endif

    /* Set up root cnode and the caps it contains */
    // Has to be valid after leaving this stack frame, because this CTE will
    // be entered into the MDB!
    // Don't want this to be part of the data section, as the memory backing
    // the data section of the kernel can and will disappear when we reboot a
    // core with a different kernel but want to restore the state
    struct cte *rootcn = &kcb_current->init_rootcn;
    mdb_init(kcb_current);
    kcb_current->is_valid = true;
#if defined(CONFIG_SCHEDULER_RR)
    kcb_current->sched = SCHED_RR;
#elif defined(CONFIG_SCHEDULER_RBED)
    kcb_current->sched = SCHED_RBED;
#else
#error invalid scheduler
#endif

    /* create root cnode */
    err = caps_create_new(ObjType_L1CNode, alloc_phys(OBJSIZE_L2CNODE),
                          OBJSIZE_L2CNODE, OBJSIZE_L2CNODE, my_core_id,
                          rootcn);
    assert(err_is_ok(err));

    // on BSP core: Add BSP KCB to rootcn
    if (arch_core_is_bsp()) {
        // cannot use caps_create_new() here, as that would zero out KCB, so
        // we replicate the cap initialization here.
        struct capability bspkcb_cap;
        memset(&bspkcb_cap, 0, sizeof(struct capability));
        bspkcb_cap.type = ObjType_KernelControlBlock;
        bspkcb_cap.rights = CAPRIGHTS_ALLRIGHTS;
        bspkcb_cap.u.kernelcontrolblock.kcb = kcb_current;
        // find slot in init rootcn
        struct cte *bspkcb = caps_locate_slot(CNODE(rootcn), ROOTCN_SLOT_BSPKCB);
        assert(bspkcb && bspkcb->cap.type == ObjType_Null);
        memcpy(&bspkcb->cap, &bspkcb_cap, sizeof(struct capability));
    }

    // Task cnode in root cnode
    st->taskcn = caps_locate_slot(CNODE(rootcn), ROOTCN_SLOT_TASKCN);
    err = caps_create_new(ObjType_L2CNode, alloc_phys(OBJSIZE_L2CNODE),
                          OBJSIZE_L2CNODE, OBJSIZE_L2CNODE, my_core_id,
                          st->taskcn);
    assert(err_is_ok(err));

    // Page cnode in root cnode
    st->pagecn = caps_locate_slot(CNODE(rootcn), ROOTCN_SLOT_PAGECN);
    err = caps_create_new(ObjType_L2CNode,
                          alloc_phys(OBJSIZE_L2CNODE), OBJSIZE_L2CNODE,
                          OBJSIZE_L2CNODE, my_core_id, st->pagecn);
    assert(err_is_ok(err));

    // Base page cnode in root cnode
    st->basepagecn = caps_locate_slot(CNODE(rootcn), ROOTCN_SLOT_BASE_PAGE_CN);
    err = caps_create_new(ObjType_L2CNode, alloc_phys(OBJSIZE_L2CNODE),
                          OBJSIZE_L2CNODE, OBJSIZE_L2CNODE, my_core_id,
                          st->basepagecn);
    assert(err_is_ok(err));

    // Early cnode alloc cnode in root cnode
    st->earlycncn = caps_locate_slot(CNODE(rootcn), ROOTCN_SLOT_EARLY_CN_CN);
    err = caps_create_new(ObjType_L2CNode, alloc_phys(OBJSIZE_L2CNODE),
                          OBJSIZE_L2CNODE, OBJSIZE_L2CNODE, my_core_id,
                          st->earlycncn);
    assert(err_is_ok(err));

    // Super cnode in root cnode
    st->supercn = caps_locate_slot(CNODE(rootcn), ROOTCN_SLOT_SUPERCN);
    err = caps_create_new(ObjType_L2CNode,
                          alloc_phys(OBJSIZE_L2CNODE),
                          OBJSIZE_L2CNODE, OBJSIZE_L2CNODE, my_core_id, st->supercn);
    assert(err_is_ok(err));

    // slot_alloc cnodes in root cnode. assumes SLOT_SLOT_ALLOC0,1,2 are
    // consecutive slots in root cnode.
    assert(ROOTCN_SLOT_SLOT_ALLOC0 + 1 == ROOTCN_SLOT_SLOT_ALLOC1);
    assert(ROOTCN_SLOT_SLOT_ALLOC1 + 1 == ROOTCN_SLOT_SLOT_ALLOC2);
    assert(ROOTCN_SLOT_SLOT_ALLOC2 + 1 == ROOTCN_SLOT_ROOT_MAPPING);
    st->slot_alloc_cn0 = caps_locate_slot(CNODE(rootcn), ROOTCN_SLOT_SLOT_ALLOC0);
    err = caps_create_new(ObjType_L2CNode,
                          alloc_phys(4*OBJSIZE_L2CNODE), 4*OBJSIZE_L2CNODE,
                          OBJSIZE_L2CNODE, my_core_id, st->slot_alloc_cn0);
    assert(err_is_ok(err));

    // Seg cnode in root cnode
    st->segcn = caps_locate_slot(CNODE(rootcn), ROOTCN_SLOT_SEGCN);
    err = caps_create_new(ObjType_L2CNode, alloc_phys(OBJSIZE_L2CNODE),
                          OBJSIZE_L2CNODE, OBJSIZE_L2CNODE, my_core_id,
                          st->segcn);
    assert(err_is_ok(err));

    // Physaddr cnode in root cnode
    st->physaddrcn = caps_locate_slot(CNODE(rootcn), ROOTCN_SLOT_PACN);
    err = caps_create_new(ObjType_L2CNode,
                          alloc_phys(OBJSIZE_L2CNODE), OBJSIZE_L2CNODE, OBJSIZE_L2CNODE,
                          my_core_id, st->physaddrcn);
    assert(err_is_ok(err));

    if (arch_core_is_bsp()) {
        // Cnode for Boot loaded modules
        st->modulecn = caps_locate_slot(CNODE(rootcn), ROOTCN_SLOT_MODULECN);
        err = caps_create_new(ObjType_L2CNode,
                              alloc_phys(OBJSIZE_L2CNODE), OBJSIZE_L2CNODE,
                              OBJSIZE_L2CNODE, my_core_id, st->modulecn);
        assert(err_is_ok(err));
    }

    /* Managing caps in task cnode */
    // Dcb cap
    struct cte *init_dcb_cte = caps_locate_slot(CNODE(st->taskcn),
                                                TASKCN_SLOT_DISPATCHER);
    err = caps_create_new(ObjType_Dispatcher,
                          alloc_phys(OBJSIZE_DISPATCHER),
                          OBJSIZE_DISPATCHER, 0, my_core_id, init_dcb_cte);
    assert(err_is_ok(err));
    struct dcb *init_dcb = init_dcb_cte->cap.u.dispatcher.dcb;

    // Copy root cnode to task cnode
    err = caps_copy_to_cnode(st->taskcn, TASKCN_SLOT_ROOTCN, rootcn, 0, 0, 0);
    assert(err_is_ok(err));

    // Dispatcher frame in task cnode
    struct cte *init_dispframe_cte = caps_locate_slot(CNODE(st->taskcn),
                                                      TASKCN_SLOT_DISPFRAME);
    err = caps_create_new(ObjType_Frame,
                          alloc_phys_aligned(DISPATCHER_FRAME_SIZE, DISPATCHER_FRAME_SIZE),
                          DISPATCHER_FRAME_SIZE,
                          DISPATCHER_FRAME_SIZE,
                          my_core_id, init_dispframe_cte);
    assert(err_is_ok(err));

    // Copy dispatcher frame to the dcb struct
    err = caps_copy_to_cte(&init_dcb->disp_cte, init_dispframe_cte, false, 0, 0);
    assert(err_is_ok(err));

    // Argspage in task cnode
    struct cte *init_args_cte = caps_locate_slot(CNODE(st->taskcn),
                                                 TASKCN_SLOT_ARGSPAGE);
    err = caps_create_new(ObjType_Frame,
                          alloc_phys_aligned(ARGS_SIZE, ARGS_SIZE),
                          1UL << ARGS_FRAME_BITS, 1UL << ARGS_FRAME_BITS, my_core_id,
                          init_args_cte);
    st->args_page = gen_phys_to_local_phys(init_args_cte->cap.u.frame.base);

    if (arch_core_is_bsp()) {
        assert(bootinfo != 0);

        // Map bootinfo (in task cnode)
        struct cte *bootinfo_cte = caps_locate_slot(CNODE(st->taskcn),
                                                    TASKCN_SLOT_BOOTINFO);
        /* DevFrame to prevent zeroing! */
        /* Note: Since this is only done in the bsp, we can safely assume we
         * own the bootinfo memory */
        err = caps_create_new(ObjType_DevFrame, bootinfo, 1UL << BOOTINFO_SIZEBITS,
                              1UL << BOOTINFO_SIZEBITS, my_core_id, bootinfo_cte);
        assert(err_is_ok(err));
    }

    // Map kernel Cap in task cnode
    struct cte *kernelcap_cte = caps_locate_slot(CNODE(st->taskcn),
                                                 TASKCN_SLOT_KERNELCAP);
    err = caps_create_new(ObjType_Kernel, 0, 0, 0, my_core_id, kernelcap_cte);
    assert(err_is_ok(err));

    // Create capability for performance monitoring
    struct cte *perfmoncap_cte = caps_locate_slot(CNODE(st->taskcn),
                                                   TASKCN_SLOT_PERF_MON);
    err = caps_create_new(ObjType_PerfMon, 0, 0, 0, my_core_id, perfmoncap_cte);
    assert(err_is_ok(err));

    // Map IRQ table in task cnode
    err = caps_create_new(ObjType_IRQTable, 0, 0, 0, my_core_id,
                          caps_locate_slot(CNODE(st->taskcn), TASKCN_SLOT_IRQ));
    assert(err_is_ok(err));

    // Create capability for IPI sending
    struct cte *ipicap_cte = caps_locate_slot(CNODE(st->taskcn),
                                              TASKCN_SLOT_IPI);
    err = caps_create_new(ObjType_IPI, 0, 0, 0, my_core_id, ipicap_cte);
    assert(err_is_ok(err));

    // Create process manager capability
    struct cte *procmngcap_cte = caps_locate_slot(CNODE(st->taskcn),
                                                  TASKCN_SLOT_PROC_MNG);
    err = caps_create_new(ObjType_ProcessManager, 0, 0, 0, my_core_id,
                          procmngcap_cte);
    assert(err_is_ok(err));

    /* Initialize dispatcher */
    dispatcher_handle_t init_handle
        = local_phys_to_mem(init_dispframe_cte->cap.u.frame.base);
    struct dispatcher_shared_generic *init_disp =
        get_dispatcher_shared_generic(init_handle);
    init_disp->disabled = true;
    init_disp->curr_core_id = my_core_id;
    strncpy(init_disp->name, argv[0], DISP_NAME_LEN);

    /* Set fields in DCB */
    // Set cspace
    err = caps_copy_to_cte(&init_dcb->cspace, rootcn, 0, 0, 0);
    assert(err_is_ok(err));

    // Set disp and add to run queue
    init_dcb->disp = init_handle;
    init_dcb->disabled = true;
    make_runnable(init_dcb);

    // XXX: hack for 1:1 mapping
    if (args_base == 0) {
        args_base = st->args_page;
    }

    /* Construct args page */
    struct spawn_domain_params *params = (void *)local_phys_to_mem(st->args_page);
    memset(params, 0, sizeof(*params));
    char *buf = (char *)local_phys_to_mem(st->args_page
                                          + sizeof(struct spawn_domain_params));
    size_t buflen = ARGS_SIZE - sizeof(struct spawn_domain_params);
    assert(argc < MAX_CMDLINE_ARGS);
    params->argc = argc;
    for (int i = 0; i < argc; i++) {
        size_t arglen = strlen(argv[i]);
        assert(arglen < buflen);
        params->argv[i] = (void *)(args_base + mem_to_local_phys((lvaddr_t)buf)
                                   - st->args_page);
        strcpy(buf, argv[i]);
        buf += arglen + 1;
        buflen -= arglen + 1;
    }

    assert(retparamaddr != NULL);
    *retparamaddr = args_base;

    /* Fill up base page CN (pre-allocated 4K pages) */
    err = caps_create_new(ObjType_RAM, alloc_phys(L2_CNODE_SLOTS * BASE_PAGE_SIZE),
            L2_CNODE_SLOTS * BASE_PAGE_SIZE, BASE_PAGE_SIZE, my_core_id,
            caps_locate_slot(CNODE(st->basepagecn), 0));
    assert(err_is_ok(err));

    /* Fill up early cnode alloc CN (pre-allocated 16K pages) */
    err = caps_create_new(ObjType_RAM, alloc_phys(EARLY_CNODE_ALLOCATED_SLOTS * OBJSIZE_L2CNODE),
            EARLY_CNODE_ALLOCATED_SLOTS * OBJSIZE_L2CNODE, OBJSIZE_L2CNODE, my_core_id,
            caps_locate_slot(CNODE(st->earlycncn), 0));
    assert(err_is_ok(err));

    // Store the application in the boot applications.
	trace_new_boot_application((char*) name, (uintptr_t) init_dcb);

    return init_dcb;
}


// Physical memory allocator for spawn_app_init
lpaddr_t app_alloc_phys_start, app_alloc_phys_end;

/**
 * Allocate physical memory during kernel startup for application cores.
 * Allocations are always rounded up to multiple pages.
 *
 * \param size The number of bytes to allocate.
 *
 * \return An lpaddr to the newly allocated physical memory.
 */
lpaddr_t app_alloc_phys(size_t size)
{
    uint32_t npages = (size + BASE_PAGE_SIZE - 1) / BASE_PAGE_SIZE;

    lpaddr_t addr = app_alloc_phys_start;
    app_alloc_phys_start += npages * BASE_PAGE_SIZE;

    if (app_alloc_phys_start >= app_alloc_phys_end) {
        panic("Out of memory, increase CORE_DATA_PAGES");
    }

    memset((void*)local_phys_to_mem(addr), 0, npages * BASE_PAGE_SIZE);

    return addr;
}

lpaddr_t app_alloc_phys_aligned(size_t size, size_t align)
{
    app_alloc_phys_start = ROUND_UP(app_alloc_phys_start, align);
    return app_alloc_phys(size);
}

/**
 * The address from where bsp_alloc_phys will start allocating memory
 */
lpaddr_t bsp_init_alloc_addr = 0;

/**
 * \brief Linear physical memory allocator.
 *
 * This function allocates a linear region of addresses of size 'size' from
 * physical memory.
 *
 * \param size  Number of bytes to allocate.
 *
 * \return Base physical address of memory region.
 */
lpaddr_t bsp_alloc_phys(size_t size)
{
    // round to base page size
    uint32_t npages = (size + BASE_PAGE_SIZE - 1) / BASE_PAGE_SIZE;

    assert(bsp_init_alloc_addr != 0);
    lpaddr_t addr = bsp_init_alloc_addr;

    bsp_init_alloc_addr += npages * BASE_PAGE_SIZE;

    memset((void*)local_phys_to_mem(addr), 0, npages * BASE_PAGE_SIZE);

    return addr;
}

lpaddr_t bsp_alloc_phys_aligned(size_t size, size_t align)
{
    bsp_init_alloc_addr = ROUND_UP(bsp_init_alloc_addr, align);
    return bsp_alloc_phys(size);
}
