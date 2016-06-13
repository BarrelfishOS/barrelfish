/*
 * Copyright (c) 2009, 2010 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <dispatch.h>
#include <string.h>
#include <stdio.h>

#include <barrelfish_kpi/init.h>
#include <barrelfish_kpi/syscalls.h>
#include <elf/elf.h>

#include <arm_hal.h>
#include <paging_kernel_arch.h>
#include <exceptions.h>
#include <cpiobin.h>
#include <init.h>
#include <barrelfish_kpi/paging_arch.h>
#include <barrelfish_kpi/arm_core_data.h>
#include <kernel_multiboot.h>
#include <offsets.h>
#include <startup_arch.h>
#include <global.h>
#include <kcb.h>

#define CNODE(cte)              (cte)->cap.u.cnode.cnode
#define UNUSED(x)               (x) = (x)

#define STARTUP_PROGRESS()      debug(SUBSYS_STARTUP, "%s:%d\n",          \
                                      __FUNCTION__, __LINE__);

#ifdef __ARM_ARCH_7M__//armv7-M : cortex-M3 processor on pandaboard
#define BSP_INIT_MODULE_NAME    BF_BINARY_PREFIX "armv7-m/sbin/init"
#define APP_INIT_MODULE_NAME	BF_BINARY_PREFIX "armv7-m/sbin/monitor"
#else//"normal" armv7-A
#define BSP_INIT_MODULE_NAME    BF_BINARY_PREFIX "armv7/sbin/init"
#define APP_INIT_MODULE_NAME	BF_BINARY_PREFIX "armv7/sbin/monitor"
#endif


//static phys_mmap_t* g_phys_mmap;        // Physical memory map
static union arm_l1_entry * init_l1;              // L1 page table for init
static union arm_l2_entry * init_l2;              // L2 page tables for init

static struct spawn_state spawn_state;

/// Pointer to bootinfo structure for init
struct bootinfo* bootinfo = (struct bootinfo*)INIT_BOOTINFO_VBASE;

/**
 * Each kernel has a local copy of global and locks. However, during booting and
 * kernel relocation, these are set to point to global of the pristine kernel,
 * so that all the kernels can share it.
 */
//static  struct global myglobal;
struct global *global = (struct global *)GLOBAL_VBASE;

static inline uintptr_t round_up(uintptr_t value, size_t unit)
{
    assert(0 == (unit & (unit - 1)));
    size_t m = unit - 1;
    return (value + m) & ~m;
}

static inline uintptr_t round_down(uintptr_t value, size_t unit)
{
    assert(0 == (unit & (unit - 1)));
    size_t m = unit - 1;
    return value & ~m;
}

// Physical memory allocator for spawn_app_init
static lpaddr_t app_alloc_phys_start, app_alloc_phys_end;
static lpaddr_t app_alloc_phys(size_t size)
{
    uint32_t npages = (size + BASE_PAGE_SIZE - 1) / BASE_PAGE_SIZE;


    lpaddr_t addr = app_alloc_phys_start;
    app_alloc_phys_start += npages * BASE_PAGE_SIZE;

    if (app_alloc_phys_start >= app_alloc_phys_end) {
        panic("Out of memory, increase CORE_DATA_PAGES");
    }

    return addr;
}

static lpaddr_t app_alloc_phys_aligned(size_t size, size_t align)
{
    app_alloc_phys_start = round_up(app_alloc_phys_start, align);
    return app_alloc_phys(size);
}

/**
 * The address from where bsp_alloc_phys will start allocating memory
 */
static lpaddr_t bsp_init_alloc_addr = PHYS_MEMORY_START;

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
lpaddr_t bsp_alloc_phys(size_t);
lpaddr_t bsp_alloc_phys(size_t size)
{
    // round to base page size
    uint32_t npages = (size + BASE_PAGE_SIZE - 1) / BASE_PAGE_SIZE;

    assert(bsp_init_alloc_addr != 0);

    lpaddr_t addr = bsp_init_alloc_addr;

    bsp_init_alloc_addr += npages * BASE_PAGE_SIZE;
    return addr;
}

static lpaddr_t bsp_alloc_phys_aligned(size_t size, size_t align)
{
    bsp_init_alloc_addr = round_up(bsp_init_alloc_addr, align);
    return bsp_alloc_phys(size);
}

/**
 * Map frames into init process address space. Init has a contiguous set of
 * l2 entries so this is straightforward.
 *
 * @param l2_table      pointer to init's L2 table.
 * @param l2_base       virtual address represented by first L2 table entry
 * @param va_base       virtual address to map.
 * @param pa_base       physical address to associate with virtual address.
 * @param bytes         number of bytes to map.
 * @param l2_flags      ARM L2 small page flags for mapped pages.
 */
static void
spawn_init_map(union arm_l2_entry* l2_table,
               lvaddr_t   l2_base,
               lvaddr_t   va_base,
               lpaddr_t   pa_base,
               size_t     bytes,
               uintptr_t  l2_flags)
{
    assert(va_base >= l2_base);
    assert(0 == (va_base & (BASE_PAGE_SIZE - 1)));
    assert(0 == (pa_base & (BASE_PAGE_SIZE - 1)));
    assert(0 == (bytes & (BASE_PAGE_SIZE - 1)));

    int bi = (va_base - l2_base) / BASE_PAGE_SIZE;
    int li = bi + bytes / BASE_PAGE_SIZE;

    while (bi < li)
    {
        paging_set_l2_entry((uintptr_t *)&l2_table[bi], pa_base, l2_flags);
        pa_base += BASE_PAGE_SIZE;
        bi++;
    }
}

static uint32_t elf_to_l2_flags(uint32_t eflags)
{
#ifdef __ARM_ARCH_7M__//the cortex-m3 does not actually understand these flags yet
//XXX: if we ever allow all these flags, then remove the ifdef again
    return 0;
#else   //normal case, __ARM_ARCH_7A__
    switch (eflags & (PF_W|PF_R))
    {
      case PF_W|PF_R:
        return (ARM_L2_SMALL_USR_RW |
                ARM_L2_SMALL_CACHEABLE |
                ARM_L2_SMALL_BUFFERABLE);
      case PF_R:
        return (ARM_L2_SMALL_USR_RO |
                ARM_L2_SMALL_CACHEABLE |
                ARM_L2_SMALL_BUFFERABLE);
      default:
        panic("Unknown ELF flags combination.");
    }
#endif
}

struct startup_l2_info
{
    union arm_l2_entry* l2_table;
    lvaddr_t   l2_base;
};

static errval_t
startup_alloc_init(
    void*      state,
    genvaddr_t gvbase,
    size_t     bytes,
    uint32_t   flags,
    void**     ret
    )
{
    const struct startup_l2_info* s2i = (const struct startup_l2_info*)state;

    lvaddr_t sv = round_down((lvaddr_t)gvbase, BASE_PAGE_SIZE);
    size_t   off = (lvaddr_t)gvbase - sv;
    lvaddr_t lv = round_up((lvaddr_t)gvbase + bytes, BASE_PAGE_SIZE);
    lpaddr_t pa;

    //STARTUP_PROGRESS();
    if(hal_cpu_is_bsp())
    	pa = bsp_alloc_phys_aligned((lv - sv), BASE_PAGE_SIZE);
    else
    	pa = app_alloc_phys_aligned((lv - sv), BASE_PAGE_SIZE);

    if (lv > sv && (pa != 0))
    {
        spawn_init_map(s2i->l2_table, s2i->l2_base, sv,
                       pa, lv - sv, elf_to_l2_flags(flags));
        *ret = (void*)(local_phys_to_mem(pa) + off);
    }
    else
    {
        *ret = 0;
    }
    return SYS_ERR_OK;
}

static void
load_init_image(
    struct startup_l2_info* l2i,
    const char *name,
    genvaddr_t* init_ep,
    genvaddr_t* got_base
    )
{
    lvaddr_t elf_base;
    size_t elf_bytes;
    errval_t err;


    *init_ep = *got_base = 0;

    /* Load init ELF32 binary */
    struct multiboot_modinfo *module = multiboot_find_module(name);
    if (module == NULL) {
    	panic("Could not find init module!");
    }

    elf_base =  local_phys_to_mem(module->mod_start);
    elf_bytes = MULTIBOOT_MODULE_SIZE(*module);

    debug(SUBSYS_STARTUP, "load_init_image %p %08x\n", elf_base, elf_bytes);
    //printf("load_init_image %p %08x\n", elf_base, elf_bytes);

    err = elf_load(EM_ARM, startup_alloc_init, l2i,
    		elf_base, elf_bytes, init_ep);
    if (err_is_fail(err)) {
    	//err_print_calltrace(err);
    	panic("ELF load of " BSP_INIT_MODULE_NAME " failed!\n");
    }

    // TODO: Fix application linkage so that it's non-PIC.
    struct Elf32_Shdr* got_shdr =
        elf32_find_section_header_name((lvaddr_t)elf_base, elf_bytes, ".got");
    if (got_shdr)
    {
        *got_base = got_shdr->sh_addr;
    }
}

/// Setup the module cnode, which contains frame caps to all multiboot modules
void create_module_caps(struct spawn_state *st)
{
    errval_t err;

    /* Create caps for multiboot modules */
    struct multiboot_modinfo *module =
        (struct multiboot_modinfo *)local_phys_to_mem(glbl_core_data->mods_addr);

    // Allocate strings area
    lpaddr_t mmstrings_phys = bsp_alloc_phys(BASE_PAGE_SIZE);
    lvaddr_t mmstrings_base = local_phys_to_mem(mmstrings_phys);
    lvaddr_t mmstrings = mmstrings_base;

    // create cap for strings area in first slot of modulecn
    assert(st->modulecn_slot == 0);
    err = caps_create_new(ObjType_Frame, mmstrings_phys, BASE_PAGE_SIZE,
                          BASE_PAGE_SIZE, my_core_id,
                          caps_locate_slot(CNODE(st->modulecn),
                                           st->modulecn_slot++));
    assert(err_is_ok(err));

    /* Walk over multiboot modules, creating frame caps */
    for (int i = 0; i < glbl_core_data->mods_count; i++) {
        struct multiboot_modinfo *m = &module[i];

        // Set memory regions within bootinfo
        struct mem_region *region =
            &bootinfo->regions[bootinfo->regions_length++];

        genpaddr_t remain = MULTIBOOT_MODULE_SIZE(*m);
        genpaddr_t base_addr = local_phys_to_gen_phys(m->mod_start);

        region->mr_type = RegionType_Module;
        region->mr_base = base_addr;
        region->mrmod_slot = st->modulecn_slot;  // first slot containing caps
        region->mrmod_size = remain;  // size of image _in bytes_
        region->mrmod_data = mmstrings - mmstrings_base; // offset of string in area

        // round up to page size for caps
        remain = ROUND_UP(remain, BASE_PAGE_SIZE);
        assert((base_addr & BASE_PAGE_MASK) == 0);
        assert((remain & BASE_PAGE_MASK) == 0);

        assert(st->modulecn_slot < (1U << st->modulecn->cap.u.cnode.bits));
        // create as DevFrame cap to avoid zeroing memory contents
        err = caps_create_new(ObjType_DevFrame, base_addr, remain,
                              remain, my_core_id,
                              caps_locate_slot(CNODE(st->modulecn),
                                               st->modulecn_slot++));
        assert(err_is_ok(err));

        // Copy multiboot module string to mmstrings area
        strcpy((char *)mmstrings, MBADDR_ASSTRING(m->string));
        mmstrings += strlen(MBADDR_ASSTRING(m->string)) + 1;
        assert(mmstrings < mmstrings_base + BASE_PAGE_SIZE);
    }
}

/// Create physical address range or RAM caps to unused physical memory
static void create_phys_caps(lpaddr_t init_alloc_addr)
{
    errval_t err;

    /* Walk multiboot MMAP structure, and create appropriate caps for memory */
    char *mmap_addr = MBADDR_ASSTRING(glbl_core_data->mmap_addr);
    genpaddr_t last_end_addr = 0;

    for(char *m = mmap_addr; m < mmap_addr + glbl_core_data->mmap_length;) {
        struct multiboot_mmap *mmap = (struct multiboot_mmap * SAFE)TC(m);

        debug(SUBSYS_STARTUP, "MMAP %llx--%llx Type %"PRIu32"\n",
                mmap->base_addr, mmap->base_addr + mmap->length,
                mmap->type);

        if (last_end_addr >= init_alloc_addr
                && mmap->base_addr > last_end_addr) {
            /* we have a gap between regions. add this as a physaddr range */
            debug(SUBSYS_STARTUP, "physical address range %llx--%llx\n",
                    last_end_addr, mmap->base_addr);

            err = create_caps_to_cnode(last_end_addr,
                    mmap->base_addr - last_end_addr,
                    RegionType_PhyAddr, &spawn_state, bootinfo);
            assert(err_is_ok(err));
        }

        if (mmap->type == MULTIBOOT_MEM_TYPE_RAM) {
            genpaddr_t base_addr = mmap->base_addr;
            genpaddr_t end_addr  = base_addr + mmap->length;

            // only map RAM which is greater than init_alloc_addr
            if (end_addr > local_phys_to_gen_phys(init_alloc_addr))
            {
                if (base_addr < local_phys_to_gen_phys(init_alloc_addr)) {
                    base_addr = local_phys_to_gen_phys(init_alloc_addr);
                }
                debug(SUBSYS_STARTUP, "RAM %llx--%llx\n", base_addr, end_addr);

                assert(end_addr >= base_addr);
                err = create_caps_to_cnode(base_addr, end_addr - base_addr,
                        RegionType_Empty, &spawn_state, bootinfo);
                assert(err_is_ok(err));
            }
        } else if (mmap->base_addr > local_phys_to_gen_phys(init_alloc_addr)) {
            /* XXX: The multiboot spec just says that mapping types other than
             * RAM are "reserved", but GRUB always maps the ACPI tables as type
             * 3, and things like the IOAPIC tend to show up as type 2 or 4,
             * so we map all these regions as platform data
             */
            debug(SUBSYS_STARTUP, "platform %llx--%llx\n", mmap->base_addr,
                    mmap->base_addr + mmap->length);
            assert(mmap->base_addr > local_phys_to_gen_phys(init_alloc_addr));
            err = create_caps_to_cnode(mmap->base_addr, mmap->length,
                    RegionType_PlatformData, &spawn_state, bootinfo);
            assert(err_is_ok(err));
        }
        last_end_addr = mmap->base_addr + mmap->length;
        m += mmap->size + 4;
    }

    // Assert that we have some physical address space
    assert(last_end_addr != 0);

    if (last_end_addr < PADDR_SPACE_SIZE)
    {
    	/*
    	 * FIXME: adding the full range results in too many caps to add
    	 * to the cnode (and we can't handle such big caps in user-space
    	 * yet anyway) so instead we limit it to something much smaller
    	 */
    	genpaddr_t size = PADDR_SPACE_SIZE - last_end_addr;
    	const genpaddr_t phys_region_limit = 1ULL << 32; // PCI implementation limit
    	if (last_end_addr > phys_region_limit) {
    		size = 0; // end of RAM is already too high!
    	} else if (last_end_addr + size > phys_region_limit) {
    		size = phys_region_limit - last_end_addr;
    	}
    	debug(SUBSYS_STARTUP, "end physical address range %llx--%llx\n",
    			last_end_addr, last_end_addr + size);
    	err = create_caps_to_cnode(last_end_addr, size,
    			RegionType_PhyAddr, &spawn_state, bootinfo);
    	assert(err_is_ok(err));
    }
}

/*
 * \brief Initialzie page tables
 *
 * This includes setting up page tables for the init process.
 */
static void init_page_tables(void)
{
    // Create page table for init
    if(hal_cpu_is_bsp()) {
        init_l1 =  (union arm_l1_entry *)local_phys_to_mem(bsp_alloc_phys_aligned(INIT_L1_BYTES, ARM_L1_ALIGN));
        memset(init_l1, 0, INIT_L1_BYTES);

        init_l2 = (union arm_l2_entry *)local_phys_to_mem(bsp_alloc_phys_aligned(INIT_L2_BYTES, ARM_L2_ALIGN));
        memset(init_l2, 0, INIT_L2_BYTES);
    } else {
        init_l1 =  (union arm_l1_entry *)local_phys_to_mem(app_alloc_phys_aligned(INIT_L1_BYTES, ARM_L1_ALIGN));
        memset(init_l1, 0, INIT_L1_BYTES);

        init_l2 = (union arm_l2_entry *)local_phys_to_mem(app_alloc_phys_aligned(INIT_L2_BYTES, ARM_L2_ALIGN));
        memset(init_l2, 0, INIT_L2_BYTES);
    }

    printf("init_page_tables done: init_l1=%p init_l2=%p\n",
            init_l1, init_l2);

    /* Map pagetables into page CN */
    int pagecn_pagemap = 0;

    /*
     * ARM has:
     *
     * L1 has 4096 entries (16KB).
     * L2 Coarse has 256 entries (256 * 4B = 1KB).
     *
     * CPU driver currently fakes having 1024 entries in L1 and
     * L2 with 1024 entries by treating a page as 4 consecutive
     * L2 tables and mapping this as a unit in L1.
     */
    caps_create_new(ObjType_VNode_ARM_l1,
                    mem_to_local_phys((lvaddr_t)init_l1),
                    vnode_objsize(ObjType_VNode_ARM_l1), 0, my_core_id,
                    caps_locate_slot(CNODE(spawn_state.pagecn),
                        pagecn_pagemap++)
                    );

    //STARTUP_PROGRESS();

    // Map L2 into successive slots in pagecn
    size_t i;
    for (i = 0; i < INIT_L2_BYTES / BASE_PAGE_SIZE; i++) {
        size_t objsize_vnode = vnode_objsize(ObjType_VNode_ARM_l2);
        assert(objsize_vnode == BASE_PAGE_SIZE);
        caps_create_new(
                        ObjType_VNode_ARM_l2,
                        mem_to_local_phys((lvaddr_t)init_l2) + i*objsize_vnode,
                        objsize_vnode, 0, my_core_id,
                        caps_locate_slot(CNODE(spawn_state.pagecn),
                            pagecn_pagemap++)
                        );
    }

    /*
     * Initialize init page tables - this just wires the L1
     * entries through to the corresponding L2 entries.
     */
    STATIC_ASSERT(0 == (INIT_VBASE % ARM_L1_SECTION_BYTES), "");
    for (lvaddr_t vaddr = INIT_VBASE;
         vaddr < INIT_SPACE_LIMIT;
         vaddr += ARM_L1_SECTION_BYTES) {
        uintptr_t section = (vaddr - INIT_VBASE) / ARM_L1_SECTION_BYTES;
        uintptr_t l2_off = section * ARM_L2_TABLE_BYTES;
        lpaddr_t paddr = mem_to_local_phys((lvaddr_t)init_l2) + l2_off;
        paging_map_user_pages_l1((lvaddr_t)init_l1, vaddr, paddr);
    }

    printf("Calling paging_context_switch with address = %"PRIxLVADDR"\n",
           mem_to_local_phys((lvaddr_t) init_l1));
    paging_context_switch(mem_to_local_phys((lvaddr_t)init_l1));
}

static struct dcb *spawn_init_common(const char *name,
                                     int argc, const char *argv[],
                                     lpaddr_t bootinfo_phys,
                                     alloc_phys_func alloc_phys)
{
    printf("spawn_init_common %s\n", name);

    lvaddr_t paramaddr;
    struct dcb *init_dcb = spawn_module(&spawn_state, name,
                                        argc, argv,
                                        bootinfo_phys, INIT_ARGS_VBASE,
                                        alloc_phys, &paramaddr);

    init_page_tables();

    printf("about to call mem_to_local_phys with lvaddr=%"PRIxLVADDR"\n",
           init_l1);

    init_dcb->vspace = mem_to_local_phys((lvaddr_t)init_l1);

    spawn_init_map(init_l2, INIT_VBASE, INIT_ARGS_VBASE,
                   spawn_state.args_page, ARGS_SIZE, INIT_PERM_RW);


    // Map dispatcher
    spawn_init_map(init_l2, INIT_VBASE, INIT_DISPATCHER_VBASE,
                   mem_to_local_phys(init_dcb->disp), DISPATCHER_SIZE,
                   INIT_PERM_RW);


    /*
     * we create the capability to the devices at this stage and store it
     * in the TASKCN_SLOT_IO, where on x86 the IO capability is stored for
     * device access on PCI. PCI is not available on the pandaboard so this
     * should not be a problem.
     */
    struct cte *iocap = caps_locate_slot(CNODE(spawn_state.taskcn), TASKCN_SLOT_IO);
    errval_t  err = caps_create_new(ObjType_DevFrame, 0x40000000, 1UL << 30,
                                    1UL << 30, my_core_id, iocap);
        assert(err_is_ok(err));

    struct dispatcher_shared_generic *disp
        = get_dispatcher_shared_generic(init_dcb->disp);
    struct dispatcher_shared_arm *disp_arm
        = get_dispatcher_shared_arm(init_dcb->disp);

    /* Initialize dispatcher */
    disp->disabled = true;
    strncpy(disp->name, argv[0], DISP_NAME_LEN);

    /* tell init the vspace addr of its dispatcher */
    disp->udisp = INIT_DISPATCHER_VBASE;

    disp_arm->enabled_save_area.named.r0   = paramaddr;
#ifndef __ARM_ARCH_7M__ //the armv7-m profile does not have such a mode field
    disp_arm->enabled_save_area.named.cpsr = ARM_MODE_USR | CPSR_F_MASK;
#endif
    disp_arm->enabled_save_area.named.rtls = INIT_DISPATCHER_VBASE;
    disp_arm->disabled_save_area.named.rtls = INIT_DISPATCHER_VBASE;

    printf("spawn_init_common: starting from=%"PRIxLVADDR"\n");

    return init_dcb;
}

struct dcb *spawn_bsp_init(const char *name, alloc_phys_func alloc_phys)
{
    printf("spawn_bsp_init\n");

    /* Only the first core can run this code */
    assert(hal_cpu_is_bsp());

    /* Allocate bootinfo */
    lpaddr_t bootinfo_phys = alloc_phys(BOOTINFO_SIZE);
    memset((void *)local_phys_to_mem(bootinfo_phys), 0, BOOTINFO_SIZE);

    /* Construct cmdline args */
    char bootinfochar[16];
    snprintf(bootinfochar, sizeof(bootinfochar), "%u", INIT_BOOTINFO_VBASE);
    const char *argv[] = { "init", bootinfochar };
    int argc = 2;

    struct dcb *init_dcb = spawn_init_common(name, argc, argv, bootinfo_phys,
            alloc_phys);

    // Map bootinfo
    spawn_init_map(init_l2, INIT_VBASE, INIT_BOOTINFO_VBASE,
                   bootinfo_phys, BOOTINFO_SIZE, INIT_PERM_RW);

    struct startup_l2_info l2_info = { init_l2, INIT_VBASE };

    genvaddr_t init_ep, got_base;
    load_init_image(&l2_info, BSP_INIT_MODULE_NAME, &init_ep, &got_base);

    struct dispatcher_shared_arm *disp_arm
        = get_dispatcher_shared_arm(init_dcb->disp);
    disp_arm->enabled_save_area.named.r10  = got_base;
    disp_arm->got_base = got_base;

    disp_arm->disabled_save_area.named.pc   = init_ep;
#ifndef __ARM_ARCH_7M__ //the armv7-m profile does not have such a mode field
    disp_arm->disabled_save_area.named.cpsr = ARM_MODE_USR | CPSR_F_MASK;
#endif
    disp_arm->disabled_save_area.named.r10  = got_base;

    /* Create caps for init to use */
    create_module_caps(&spawn_state);
    lpaddr_t init_alloc_end = alloc_phys(0); // XXX
    create_phys_caps(init_alloc_end);

    /* Fill bootinfo struct */
    bootinfo->mem_spawn_core = KERNEL_IMAGE_SIZE; // Size of kernel

    return init_dcb;
}

struct dcb *spawn_app_init(struct arm_core_data *core_data,
                           const char *name, alloc_phys_func alloc_phys)
{
    errval_t err;

    /* Construct cmdline args */
    // Core id of the core that booted this core
    char coreidchar[10];
    snprintf(coreidchar, sizeof(coreidchar), "%d", core_data->src_core_id);

    // IPI channel id of core that booted this core
    char chanidchar[30];
    snprintf(chanidchar, sizeof(chanidchar), "chanid=%"PRIu32, core_data->chan_id);

    // Arch id of the core that booted this core
    char archidchar[30];
    snprintf(archidchar, sizeof(archidchar), "archid=%d",
            core_data->src_arch_id);

    const char *argv[5] = { name, coreidchar, chanidchar, archidchar };
    int argc = 4;

    struct dcb *init_dcb = spawn_init_common(name, argc, argv,0, alloc_phys);

    // Urpc frame cap
    struct cte *urpc_frame_cte = caps_locate_slot(CNODE(spawn_state.taskcn),
    		TASKCN_SLOT_MON_URPC);
    // XXX: Create as devframe so the memory is not zeroed out
    err = caps_create_new(ObjType_DevFrame, core_data->urpc_frame_base,
            core_data->urpc_frame_bits,
            core_data->urpc_frame_bits, my_core_id, urpc_frame_cte);
    assert(err_is_ok(err));
    urpc_frame_cte->cap.type = ObjType_Frame;
    lpaddr_t urpc_ptr = gen_phys_to_local_phys(urpc_frame_cte->cap.u.frame.base);

    /* Map urpc frame at MON_URPC_BASE */
    spawn_init_map(init_l2, INIT_VBASE, MON_URPC_VBASE, urpc_ptr, MON_URPC_SIZE,
    			   INIT_PERM_RW);

    struct startup_l2_info l2_info = { init_l2, INIT_VBASE };

    // elf load the domain
    genvaddr_t entry_point, got_base=0;
    err = elf_load(EM_ARM, startup_alloc_init, &l2_info,
    		local_phys_to_mem(core_data->monitor_binary),
                core_data->monitor_binary_size, &entry_point);
    if (err_is_fail(err)) {
    	//err_print_calltrace(err);
    	panic("ELF load of init module failed!");
    }

    // TODO: Fix application linkage so that it's non-PIC.
    struct Elf32_Shdr* got_shdr =
    		elf32_find_section_header_name(local_phys_to_mem(core_data->monitor_binary),
    									   core_data->monitor_binary_size, ".got");
    if (got_shdr)
    {
    	got_base = got_shdr->sh_addr;
    }

    struct dispatcher_shared_arm *disp_arm =
    		get_dispatcher_shared_arm(init_dcb->disp);
    disp_arm->enabled_save_area.named.r10  = got_base;
    disp_arm->got_base = got_base;

    disp_arm->disabled_save_area.named.pc   = entry_point;
#ifndef __ARM_ARCH_7M__ //the armv7-m profile does not have such a mode field
    disp_arm->disabled_save_area.named.cpsr = ARM_MODE_USR | CPSR_F_MASK;
#endif
    disp_arm->disabled_save_area.named.r10  = got_base;
    //disp_arm->disabled_save_area.named.rtls = INIT_DISPATCHER_VBASE;

    return init_dcb;
}

void play_with_fdif(void);

void arm_kernel_startup(void)
{
    printf("arm_kernel_startup entered \n");
    struct dcb *init_dcb;

    if (hal_cpu_is_bsp()) {
        printf("Doing BSP related bootup \n");

    	/* Initialize the location to allocate phys memory from */
    	bsp_init_alloc_addr = glbl_core_data->start_free_ram;

        /* allocate initial KCB */
        kcb_current = (struct kcb *) local_phys_to_mem(bsp_alloc_phys(sizeof(*kcb_current)));
        memset(kcb_current, 0, sizeof(*kcb_current));
        assert(kcb_current);

        // Bring up init
        init_dcb = spawn_bsp_init(BSP_INIT_MODULE_NAME, bsp_alloc_phys);

        // Not available on PandaBoard?        pit_start(0);

    } else {
        printf("Doing non-BSP related bootup \n");

        kcb_current = (struct kcb *)
            local_phys_to_mem((lpaddr_t) kcb_current);

        /* Initialize the allocator with the information passed to us */
        app_alloc_phys_start = glbl_core_data->memory_base_start;
        app_alloc_phys_end   = app_alloc_phys_start
                               + ((lpaddr_t)1 <<glbl_core_data->memory_bits);

        init_dcb = spawn_app_init(glbl_core_data, APP_INIT_MODULE_NAME, app_alloc_phys);

#ifndef __ARM_ARCH_7M__ //armv7-m does not use a gic and can not acknowledge interrupts
    	uint32_t irq = gic_get_active_irq();
    	gic_ack_irq(irq);
#endif
    }

    /* printf("Trying to enable interrupts\n"); */
    /* __asm volatile ("CPSIE aif"); // Enable interrups */
    /* printf("Done enabling interrupts\n"); */

    /* printf("HOLD BOOTUP - SPINNING\n"); */
    /* while (1); */
    /* printf("THIS SHOULD NOT HAPPEN\n"); */

    // enable interrupt forwarding to cpu
    // FIXME: PS: enable this as it is needed for multicore setup.
    // gic_cpu_interface_enable();

    // Should not return
    printf("Calling dispatch from arm_kernel_startup, start address is=%"PRIxLVADDR"\n",
           get_dispatcher_shared_arm(init_dcb->disp)->enabled_save_area.named.r0);
    dispatch(init_dcb);
    panic("Error spawning init!");

}
