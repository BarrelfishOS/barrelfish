/*
 * Copyright (c) 2009, 2010 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <bitmacros.h>
#include <dispatch.h>
#include <string.h>
#include <stdio.h>

#include <barrelfish_kpi/init.h>
#include <barrelfish_kpi/syscalls.h>
#include <elf/elf.h>

#include <platform.h>
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
#include <gic.h>
#include <arch/arm/startup_arm.h>

#define CNODE(cte)              get_address(&cte->cap)

#define STARTUP_PROGRESS()      debug(SUBSYS_STARTUP, "%s:%d\n",          \
                                      __FUNCTION__, __LINE__);

#define BSP_INIT_MODULE_NAME    BF_BINARY_PREFIX "armv7/sbin/init"
#define APP_INIT_MODULE_NAME    BF_BINARY_PREFIX "armv7/sbin/monitor"

#define MSG(format, ...) printk( LOG_NOTE, "ARMv7-A: "format, ## __VA_ARGS__ )

//static phys_mmap_t* g_phys_mmap;        // Physical memory map
static union arm_l1_entry * init_l1;              // L1 page table for init
static union arm_l2_entry * init_l2;              // L2 page tables for init

static struct spawn_state spawn_state;

/// Pointer to bootinfo structure for init
struct bootinfo* bootinfo = (struct bootinfo*)INIT_BOOTINFO_VBASE;

/* There is only one copy of the global locks, which is allocated alongside
 * the BSP kernel.  All kernels have their pointers set to the BSP copy. */
struct global *global= NULL;

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

    while (bi < li) {
        paging_set_l2_entry((uintptr_t *)&l2_table[bi], pa_base, l2_flags);
        pa_base += BASE_PAGE_SIZE;
        bi++;
    }
}

static uint32_t elf_to_l2_flags(uint32_t eflags)
{
    switch (eflags & (PF_W|PF_R)) {
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

    lvaddr_t sv = ROUND_DOWN((lvaddr_t)gvbase, BASE_PAGE_SIZE);
    size_t   off = (lvaddr_t)gvbase - sv;
    lvaddr_t lv = ROUND_UP((lvaddr_t)gvbase + bytes, BASE_PAGE_SIZE);
    lpaddr_t pa;

    //STARTUP_PROGRESS();
    if(cpu_is_bsp()) {
        pa = bsp_alloc_phys_aligned((lv - sv), BASE_PAGE_SIZE);
    } else {
        pa = app_alloc_phys_aligned((lv - sv), BASE_PAGE_SIZE);
    }
    if (lv > sv && (pa != 0)) {
        spawn_init_map(s2i->l2_table, s2i->l2_base, sv,
                       pa, lv - sv, elf_to_l2_flags(flags));
        *ret = (void*)(local_phys_to_mem(pa) + off);
    } else {
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
    if (got_shdr) {
        *got_base = got_shdr->sh_addr;
    }
}

/// Setup the module cnode, which contains frame caps to all multiboot modules
void create_module_caps(struct spawn_state *st)
{
    struct multiboot_info *mb=
        (struct multiboot_info *)core_data->multiboot_header;
    errval_t err;

    /* Create caps for multiboot modules */
    struct multiboot_modinfo *module =
        (struct multiboot_modinfo *)local_phys_to_mem(mb->mods_addr);

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
    for (int i = 0; i < mb->mods_count; i++) {
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

        assert(st->modulecn_slot < cnode_get_slots(&st->modulecn->cap));
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

/* Create physical address range or RAM caps to unused physical memory.
   init_alloc_addr is the last address allocated for the init process, plus
   one. */
static void create_phys_caps(lpaddr_t init_alloc_addr)
{
    struct multiboot_info *mb=
        (struct multiboot_info *)core_data->multiboot_header;
    errval_t err;

    /* Walk multiboot MMAP structure, and create appropriate caps for memory.
       This function assumes that the memory map is sorted by base address,
       and contains no overlaps.  We also assume that the kernel, and init,
       have been allocated at the beginning of the first RAM region, and thus
       that init_alloc_addr represents the lowest unallocated RAM address. */
    genpaddr_t last_end_addr= 0;
    genpaddr_t first_free_byte= local_phys_to_gen_phys(init_alloc_addr);
    debug(SUBSYS_STARTUP, "First free byte is PA:0x%"PRIxGENPADDR".\n",
                          first_free_byte);

    lvaddr_t mmap_vaddr= local_phys_to_mem((lpaddr_t)mb->mmap_addr);
    for(uint32_t i= 0; i < mb->mmap_length; i++) {
        struct multiboot_mmap *mmap = (struct multiboot_mmap *)mmap_vaddr;

        genpaddr_t base_addr = mmap->base_addr;
        genpaddr_t end_addr  = base_addr + (mmap->length - 1);

        debug(SUBSYS_STARTUP, "MMAP PA:0x%"PRIxGENPADDR"-0x%"
                              PRIxGENPADDR" type %"PRIu32"\n",
                              base_addr, end_addr, mmap->type);

        switch(mmap->type) {
            case MULTIBOOT_MEM_TYPE_RAM:
                /* Only map RAM which is greater than init_alloc_addr. */
                if (end_addr >= first_free_byte) {
                    if(base_addr < first_free_byte)
                        base_addr= first_free_byte;
                    debug(SUBSYS_STARTUP, "RAM PA:0x%"PRIxGENPADDR"-0x%"
                                          PRIxGENPADDR"\n",
                                          base_addr, end_addr);

                    assert(end_addr >= base_addr);
                    err= create_caps_to_cnode(base_addr,
                            (end_addr - base_addr) + 1,
                            RegionType_Empty, &spawn_state, bootinfo);
                    assert(err_is_ok(err));
                }
                break;

            case MULTIBOOT_MEM_TYPE_DEVICE:
                /* Device memory will be handled explicitly later. */
                break;

            default:
                if (mmap->base_addr >= first_free_byte) {
                    /* XXX: The multiboot spec just says that mapping types
                     * other than RAM are "reserved", but GRUB always maps the
                     * ACPI tables as type 3, and things like the IOAPIC tend
                     * to show up as type 2 or 4, so we map all these regions
                     * as platform data.  */
                    debug(SUBSYS_STARTUP, "Platform data PA:0x%"PRIxGENPADDR
                                          "-0x%"PRIxGENPADDR"\n", base_addr,
                                          end_addr);
                    assert(base_addr >= first_free_byte);
                    err = create_caps_to_cnode(base_addr, mmap->length,
                            RegionType_PlatformData, &spawn_state, bootinfo);
                    assert(err_is_ok(err));
                }
        }

        last_end_addr= end_addr;
        mmap_vaddr+= mmap->size;
    }

    // Assert that we have some physical address space
    assert(last_end_addr != 0);
}

/*
 * \brief Initialzie page tables
 *
 * This includes setting up page tables for the init process.
 */
static void init_page_tables(void)
{
    // Create page table for init
    if(cpu_is_bsp()) {
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

    MSG("init_page_tables done: init_l1=%p init_l2=%p\n",
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
    for (i = 0; i < INIT_L2_BYTES / ARM_L2_TABLE_BYTES; i++) {
        size_t objsize_vnode = vnode_objsize(ObjType_VNode_ARM_l2);
        assert(objsize_vnode == ARM_L2_TABLE_BYTES);
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

    MSG("Calling paging_context_switch with address = %"PRIxLVADDR"\n",
           mem_to_local_phys((lvaddr_t) init_l1));
    paging_context_switch(mem_to_local_phys((lvaddr_t)init_l1));
}

/* Locate the first device region below 4GB listed in the multiboot memory
 * map, and truncate it to fit. */
static void
first_device_region(lpaddr_t *base, lpaddr_t *length) {
    struct multiboot_info *mb=
        (struct multiboot_info *)core_data->multiboot_header;

    lvaddr_t mmap_vaddr= local_phys_to_mem((lpaddr_t)mb->mmap_addr);
    for(uint32_t i= 0; i < mb->mmap_length; i++) {
        struct multiboot_mmap *mmap= (struct multiboot_mmap *)mmap_vaddr;

        if(mmap->type == MULTIBOOT_MEM_TYPE_DEVICE) {
            uint64_t base64=   mmap->base_addr;
            uint64_t length64= mmap->length;

            if(base64 > (uint64_t)UINT32_MAX) {
                MSG("device region %"PRIu32" lies above 4GB.\n", i);
            }
            else if(base64 + (length64 - 1) > (uint64_t)UINT32_MAX) {
                MSG("device region %"PRIu32" extends beyond 4GB, "
                    "truncating it.\n", i);
                length64= ((uint64_t)UINT32_MAX - base64) + 1;
            }

            *base=   (lpaddr_t)base64;
            *length= (lpaddr_t)length64;
            return;
        }

        mmap_vaddr+= mmap->size;
    }

    panic("No device regions specified in multiboot memory map.\n");
}

static struct dcb *
spawn_init_common(const char *name, int argc, const char *argv[],
                  lpaddr_t bootinfo_phys, alloc_phys_func alloc_phys,
                  alloc_phys_aligned_func alloc_phys_aligned)
{
    MSG("spawn_init_common %s\n", name);

    lvaddr_t paramaddr;
    struct dcb *init_dcb = spawn_module(&spawn_state, name,
                                        argc, argv,
                                        bootinfo_phys, INIT_ARGS_VBASE,
                                        alloc_phys, alloc_phys_aligned,
                                        &paramaddr);

    init_page_tables();

    MSG("about to call mem_to_local_phys with lvaddr=%"PRIxLVADDR"\n",
           init_l1);

    init_dcb->vspace = mem_to_local_phys((lvaddr_t)init_l1);

    spawn_init_map(init_l2, INIT_VBASE, INIT_ARGS_VBASE,
                   spawn_state.args_page, ARGS_SIZE, INIT_PERM_RW);


    // Map dispatcher
    spawn_init_map(init_l2, INIT_VBASE, INIT_DISPATCHER_VBASE,
                   mem_to_local_phys(init_dcb->disp), DISPATCHER_SIZE,
                   INIT_PERM_RW);

    /* Locate the memory-mapped device region. */
    lpaddr_t device_base, device_length;
    first_device_region(&device_base, &device_length);
    MSG("Using device region at PA:0x%"PRIx32"-0x%"PRIx32"\n",
            device_base, device_base + (device_length - 1));
    if((1UL << log2ceil(device_length)) != device_length) {
        panic("Device region isn't a power of two in size.\n");
    }

    /*
     * We create the capability to the devices at this stage and store it
     * in the TASKCN_SLOT_IO, where on x86 the IO capability is stored for
     * device access on PCI.
     *
     * PCI is not available on our existing ARMv7 platforms, but this may be a
     * problem in future.
     */
    struct cte *iocap=
        caps_locate_slot(CNODE(spawn_state.taskcn), TASKCN_SLOT_IO);
    errval_t err=
        caps_create_new(ObjType_DevFrame, device_base, device_length,
                        device_length, my_core_id, iocap);
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

    /* Write the context ID for init - see arch/arm/dispatch.c. */
    cp15_write_contextidr(((uint32_t)init_dcb) & ~MASK(8));

    disp_arm->enabled_save_area.named.r0   = paramaddr;
    disp_arm->enabled_save_area.named.cpsr = ARM_MODE_USR | CPSR_F_MASK;
    arch_set_thread_register(INIT_DISPATCHER_VBASE);

    MSG("spawn_init_common: starting from=%"PRIxLVADDR"\n");

    dump_dispatcher(disp);

    return init_dcb;
}


struct dcb *
spawn_bsp_init(const char *name)
{
    MSG("spawn_bsp_init\n");

    /* Only the first core can run this code */
    assert(cpu_is_bsp());

    /* Allocate bootinfo */
    lpaddr_t bootinfo_phys = bsp_alloc_phys_aligned(BOOTINFO_SIZE, BASE_PAGE_SIZE);
    memset((void *)local_phys_to_mem(bootinfo_phys), 0, BOOTINFO_SIZE);

    /* Construct cmdline args */
    char bootinfochar[16];
    snprintf(bootinfochar, sizeof(bootinfochar), "%u", INIT_BOOTINFO_VBASE);
    const char *argv[] = { "init", bootinfochar };
    int argc = 2;

    struct dcb *init_dcb =
        spawn_init_common(name, argc, argv, bootinfo_phys,
                          bsp_alloc_phys, bsp_alloc_phys_aligned);

    // Map bootinfo
    spawn_init_map(init_l2, INIT_VBASE, INIT_BOOTINFO_VBASE,
                   bootinfo_phys, BOOTINFO_SIZE, INIT_PERM_RW);

    struct startup_l2_info l2_info = { init_l2, INIT_VBASE };

    genvaddr_t init_ep, got_base;
    load_init_image(&l2_info, BSP_INIT_MODULE_NAME, &init_ep, &got_base);

    struct dispatcher_shared_arm *disp_arm =
        get_dispatcher_shared_arm(init_dcb->disp);
    disp_arm->enabled_save_area.named.r9   = got_base;
    disp_arm->got_base = got_base;

    disp_arm->disabled_save_area.named.pc   = init_ep;
    disp_arm->disabled_save_area.named.cpsr = ARM_MODE_USR | CPSR_F_MASK;
    disp_arm->disabled_save_area.named.r9   = got_base;

    /* Create caps for init to use */
    create_module_caps(&spawn_state);
    lpaddr_t init_alloc_end = bsp_alloc_phys(0); // XXX
    create_phys_caps(init_alloc_end);

    /* Fill bootinfo struct */
    //bootinfo->mem_spawn_core = KERNEL_IMAGE_SIZE; // Size of kernel

    return init_dcb;
}

struct dcb *spawn_app_init(struct arm_core_data *new_core_data, const char *name)
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

    struct dcb *init_dcb=
        spawn_init_common(name, argc, argv, 0, app_alloc_phys, app_alloc_phys_aligned);

    // Urpc frame cap
    struct cte *urpc_frame_cte =
        caps_locate_slot(CNODE(spawn_state.taskcn), TASKCN_SLOT_MON_URPC);
    // XXX: Create as devframe so the memory is not zeroed out
    err = caps_create_new(ObjType_DevFrame, 
                          core_data->urpc_frame_base,
                          core_data->urpc_frame_size,
                          core_data->urpc_frame_size,
                          my_core_id,
                          urpc_frame_cte);
    assert(err_is_ok(err));
    urpc_frame_cte->cap.type = ObjType_Frame;
    lpaddr_t urpc_ptr = gen_phys_to_local_phys(urpc_frame_cte->cap.u.frame.base);

    /* Map urpc frame at MON_URPC_BASE */
    spawn_init_map(init_l2, INIT_VBASE, MON_URPC_VBASE, urpc_ptr, MON_URPC_SIZE,
                           INIT_PERM_RW);

    struct startup_l2_info l2_info = { init_l2, INIT_VBASE };

    // elf load the domain
    lvaddr_t monitor_binary=
        local_phys_to_mem(core_data->monitor_module.mod_start);
    size_t monitor_binary_size=
        core_data->monitor_module.mod_end -
        core_data->monitor_module.mod_start + 1;
    genvaddr_t entry_point, got_base=0;
    err = elf_load(EM_ARM, startup_alloc_init, &l2_info,
                monitor_binary, monitor_binary_size, &entry_point);
    if (err_is_fail(err)) {
        //err_print_calltrace(err);
        panic("ELF load of init module failed!");
    }

    // TODO: Fix application linkage so that it's non-PIC.
    struct Elf32_Shdr* got_shdr =
        elf32_find_section_header_name(monitor_binary, monitor_binary_size,
                                       ".got");
    if (got_shdr) {
        got_base = got_shdr->sh_addr;
    }

    struct dispatcher_shared_arm *disp_arm =
        get_dispatcher_shared_arm(init_dcb->disp);
    disp_arm->enabled_save_area.named.r9   = got_base;
    disp_arm->got_base = got_base;

    disp_arm->disabled_save_area.named.pc   = entry_point;
    disp_arm->disabled_save_area.named.cpsr = ARM_MODE_USR | CPSR_F_MASK;
    disp_arm->disabled_save_area.named.r9   = got_base;
    arch_set_thread_register(INIT_DISPATCHER_VBASE);

    return init_dcb;
}

void arm_kernel_startup(void)
{
    MSG("arm_kernel_startup entered \n");
    struct dcb *init_dcb;

    if (cpu_is_bsp()) {
        MSG("Doing BSP related bootup \n");

        struct multiboot_info *mb=
            (struct multiboot_info *)core_data->multiboot_header;
        size_t max_addr = max(multiboot_end_addr(mb),
                              (uintptr_t)&kernel_final_byte);

        /* Initialize the location to allocate phys memory from */
        bsp_init_alloc_addr = mem_to_local_phys(max_addr);

        /* Initial KCB was allocated by the boot driver. */
        assert(kcb_current);

        // Bring up init
        init_dcb = spawn_bsp_init(BSP_INIT_MODULE_NAME);
    } else {
        MSG("Doing non-BSP related bootup \n");

        kcb_current = (struct kcb *)
            local_phys_to_mem((lpaddr_t) kcb_current);

        /* Initialize the allocator with the information passed to us */
        app_alloc_phys_start = core_data->memory_base_start;
        app_alloc_phys_end   = app_alloc_phys_start + core_data->memory_bytes;

        init_dcb = spawn_app_init(core_data, APP_INIT_MODULE_NAME);

        uint32_t irq = gic_get_active_irq();
        gic_ack_irq(irq);
    }

    /* XXX - this really shouldn't be necessary. */
    MSG("Trying to enable interrupts\n"); 
    // __asm volatile ("CPSIE aif"); 
    MSG("Done enabling interrupts\n");

    /* printf("HOLD BOOTUP - SPINNING\n"); */
    /* while (1); */
    /* printf("THIS SHOULD NOT HAPPEN\n"); */

    // enable interrupt forwarding to cpu
    // FIXME: PS: enable this as it is needed for multicore setup.
    //gic_cpu_interface_enable();

    // Should not return
    MSG("Calling dispatch from arm_kernel_startup, start address is=%"PRIxLVADDR"\n",
           get_dispatcher_shared_arm(init_dcb->disp)->enabled_save_area.named.r0);
    dispatch(init_dcb);
    panic("Error spawning init!");

}
