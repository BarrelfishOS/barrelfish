/*
 * Copyright (c) 2009,2010,2015, ETH Zurich.
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
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
#include <sysreg.h>
#include <cpiobin.h>
#include <init.h>
#include <barrelfish_kpi/arm_core_data.h>
#include <kernel_multiboot2.h>
#include <offsets.h>
#include <startup_arch.h>
#include <timers.h>
#include <platform.h>

#include <arch/arm/startup_arm.h>

#include <target/aarch64/barrelfish_kpi/paging_arm_v8.h>

#include <global.h>
#include <kcb.h>

#include <efi.h>
#include <arch/arm/gic.h>

#define CNODE(cte)              get_address(&(cte)->cap)

#define STARTUP_PROGRESS()      debug(SUBSYS_STARTUP, "%s:%d\n",          \
                                      __FUNCTION__, __LINE__);

#define MSG(format, ...) printk( LOG_NOTE, "ARMv8-A: "format, ## __VA_ARGS__ )


#if !defined(BF_BINARY_PREFIX)
#   define BF_BINARY_PREFIX
#endif

#define BSP_INIT_MODULE_NAME    BF_BINARY_PREFIX "armv8/sbin/init"
#define APP_INIT_MODULE_NAME    BF_BINARY_PREFIX "armv8/sbin/monitor"


//static phys_mmap_t* g_phys_mmap;        // Physical memory map
static union armv8_ttable_entry *init_l0; // L1 page table for init
static union armv8_ttable_entry *init_l1; // L1 page table for init
static union armv8_ttable_entry *init_l2; // L2 page tables for init
static union armv8_ttable_entry *init_l3; // L3 page tables for init

static struct spawn_state spawn_state;

/// Pointer to bootinfo structure for init
struct bootinfo* bootinfo = NULL;

/**
 * Each kernel has a local copy of global and locks. However, during booting and
 * kernel relocation, these are set to point to global of the pristine kernel,
 * so that all the kernels can share it.
 */
//static  struct global myglobal;
struct global *global;

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

/**
 * Map frames into init process address space. Init has a contiguous set of
 * l3 entries so this is straightforward.
 *
 * @param l3_table      pointer to init's L3 table.
 * @param l3_base       virtual address represented by first L3 table entry
 * @param va_base       virtual address to map.
 * @param pa_base       physical address to associate with virtual address.
 * @param bytes        number of bytes to map.
 * @param l3_flags      ARM L3 small page flags for mapped pages.
 */
static void spawn_init_map(union armv8_ttable_entry *l3_table, lvaddr_t l3_base,
                           lvaddr_t va_base, lpaddr_t pa_base, size_t bytes,
                           uintptr_t  l3_flags)
{
    assert(va_base >= l3_base);
    assert(0 == (va_base & (BASE_PAGE_SIZE - 1)));
    assert(0 == (pa_base & (BASE_PAGE_SIZE - 1)));
    assert(0 == (bytes & (BASE_PAGE_SIZE - 1)));

    long bi = (va_base - l3_base) / BASE_PAGE_SIZE;
    long li = bi + bytes / BASE_PAGE_SIZE;

    while (bi < li) {
        /* XXX: we should check not to overrun here */
        paging_set_l3_entry(&l3_table[bi], pa_base, l3_flags);
        pa_base += BASE_PAGE_SIZE;
        bi++;
    }
}


static uint32_t elf_to_l3_flags(uint32_t eflags)
{
    switch (eflags & (PF_W|PF_R))
    {
      case PF_W|PF_R:
        return (VMSAv8_64_L3_USR_RW |
                VMSAv8_64_L3_CACHEABLE |
                VMSAv8_64_L3_BUFFERABLE);
      case PF_R:
        return (VMSAv8_64_L3_USR_RO |
                VMSAv8_64_L3_CACHEABLE |
                VMSAv8_64_L3_BUFFERABLE);
      default:
        panic("Unknown ELF flags combination.");
    }
}

struct startup_l3_info
{
    union armv8_ttable_entry *l3_table;
    lvaddr_t l3_base;
};

static errval_t startup_alloc_init(void* state, genvaddr_t gvbase, size_t bytes,
                                   uint32_t flags, void **ret)
{
    const struct startup_l3_info* s2i = (const struct startup_l3_info*)state;

    lvaddr_t sv = round_down((lvaddr_t)gvbase, BASE_PAGE_SIZE);
    size_t   off = (lvaddr_t)gvbase - sv;
    lvaddr_t lv = round_up((lvaddr_t)gvbase + bytes, BASE_PAGE_SIZE);
    lpaddr_t pa;

    //STARTUP_PROGRESS();
    if(cpu_is_bsp())
        pa = bsp_alloc_phys_aligned((lv - sv), BASE_PAGE_SIZE);
    else
        pa = app_alloc_phys_aligned((lv - sv), BASE_PAGE_SIZE);

    if (lv > sv && (pa != 0))
    {
        spawn_init_map(s2i->l3_table, s2i->l3_base, sv,
                       pa, lv - sv, elf_to_l3_flags(flags));
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
    struct startup_l3_info* l3i,
    const char *name,
    genvaddr_t* init_ep,
    genvaddr_t* got_base
    )
{
    lvaddr_t elf_base;
    size_t elf_bytes;
    errval_t err;

    *init_ep = *got_base = 0;

    /* Load init ELF64 binary */
    struct multiboot_header_tag *multiboot =
            (struct multiboot_header_tag *) local_phys_to_mem(
                    armv8_glbl_core_data->multiboot_image.base);
    struct multiboot_tag_module_64 *module = multiboot2_find_module_64(
            multiboot, armv8_glbl_core_data->multiboot_image.length, name);
    if (module == NULL) {
        panic("Could not find init module!");
    }

    elf_base =  local_phys_to_mem(module->mod_start);
    elf_bytes = MULTIBOOT_MODULE_SIZE(*module);

    debug(SUBSYS_STARTUP, "load_init_image %p %08x\n", elf_base, elf_bytes);
    printf("load_init_image %p %08x\n", elf_base, elf_bytes);

    err = elf_load(EM_AARCH64, startup_alloc_init, l3i,
            elf_base, elf_bytes, init_ep);
    if (err_is_fail(err)) {
        //err_print_calltrace(err);
        panic("ELF load of " BSP_INIT_MODULE_NAME " failed!\n");
    }

    // TODO: Fix application linkage so that it's non-PIC.
    struct Elf64_Shdr* got_shdr =
        elf64_find_section_header_name((lvaddr_t)elf_base, elf_bytes, ".got");
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
    struct multiboot_header_tag *multiboot =
        (struct multiboot_header_tag *)local_phys_to_mem(armv8_glbl_core_data->multiboot_image.base);

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

    //Nag
    bootinfo->regions_length = 0;

    /* Walk over multiboot modules, creating frame caps */
    size_t position = 0;
    size_t size = armv8_glbl_core_data->multiboot_image.length;

    struct mem_region *region;

    lpaddr_t acpi_base = (lpaddr_t)-1;
    /* add the ACPI regions */
    struct multiboot_tag_new_acpi *acpi_new;
    acpi_new = (struct multiboot_tag_new_acpi *)
           multiboot2_find_header(multiboot, size, MULTIBOOT_TAG_TYPE_ACPI_NEW);
    if (acpi_new) {
        acpi_base = mem_to_local_phys((lvaddr_t)&acpi_new->rsdp[0]);
    } else {
        struct multiboot_tag_old_acpi *acpi_old;
        acpi_old = (struct multiboot_tag_old_acpi *)
           multiboot2_find_header(multiboot, size, MULTIBOOT_TAG_TYPE_ACPI_OLD);
        if (acpi_old) {
            acpi_base = mem_to_local_phys((lvaddr_t)&acpi_old->rsdp[0]);
        }
    }

    if (acpi_base != (lpaddr_t)-1) {
        region = &bootinfo->regions[bootinfo->regions_length++];
        region->mr_base = acpi_base;
        region->mr_type = RegionType_ACPI_TABLE;
    }

    /* add the module regions */
    position = 0;
    struct multiboot_tag_module_64 *module = (struct multiboot_tag_module_64 *)
            multiboot2_find_header(multiboot, size, MULTIBOOT_TAG_TYPE_MODULE_64);
    while (module) {
        // Set memory regions within bootinfo
        region = &bootinfo->regions[bootinfo->regions_length++];

        genpaddr_t remain = module->mod_end - module->mod_start;
        genpaddr_t base_addr = local_phys_to_gen_phys(module->mod_start);
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
        strcpy((char *)mmstrings, module->cmdline);
        mmstrings += strlen(module->cmdline) + 1;
        assert(mmstrings < mmstrings_base + BASE_PAGE_SIZE);

        module = ((void *) module) + module->size;
        position += module->size;
        module = (struct multiboot_tag_module_64 *) multiboot2_find_header(
                (struct multiboot_header_tag *) module, size - position,
                MULTIBOOT_TAG_TYPE_MODULE_64);
    }
}

static void
create_phys_caps_region(lpaddr_t reserved_start, lpaddr_t reserved_end, lpaddr_t region_base,
        size_t region_size, enum region_type region_type) {
    errval_t err = SYS_ERR_OK;
    if (reserved_start <= region_base + region_size && region_base <= reserved_end) {
        // reserved overlaps with region
        if (region_base < reserved_start) {
            err = create_caps_to_cnode(region_base, reserved_start - region_base, region_type, &spawn_state, bootinfo);
        }
        assert(err_is_ok(err));
        if (region_base + region_size > reserved_end) {
            err = create_caps_to_cnode(reserved_end, region_base + region_size - reserved_end, region_type, &spawn_state, bootinfo);
        }
    } else {
        err = create_caps_to_cnode(region_base, region_size, region_type, &spawn_state, bootinfo);
    }
    assert(err_is_ok(err));
}

/// Create physical address range or RAM caps to unused physical memory
static void create_phys_caps(lpaddr_t reserved_start, lpaddr_t reserved_end)
{
    /* Walk multiboot MMAP structure, and create appropriate caps for memory */
    struct multiboot_tag_efi_mmap *mmap = (struct multiboot_tag_efi_mmap *)
            local_phys_to_mem(armv8_glbl_core_data->efi_mmap);

    lpaddr_t last_end_addr = 0;
    for (size_t i = 0; i < (mmap->size - sizeof(struct multiboot_tag_efi_mmap)) / mmap->descr_size; i++) {
        efi_memory_descriptor *desc = (efi_memory_descriptor *)(mmap->efi_mmap + mmap->descr_size * i);

        enum region_type region_type = RegionType_Max;
        switch(desc->Type) {
            case EfiConventionalMemory:
               region_type = RegionType_Empty;
               break;
            case EfiPersistentMemory :
                region_type = RegionType_Empty;
                break;
            case EfiACPIReclaimMemory :
                region_type = RegionType_PlatformData;
                break;
            default:
               region_type = RegionType_PlatformData;
           break;
        };

        if (last_end_addr < desc->PhysicalStart) {
            // create cap for gap in mmap
            create_phys_caps_region(reserved_start, reserved_end, last_end_addr, desc->PhysicalStart - last_end_addr, RegionType_PhyAddr);
        }
        last_end_addr = desc->PhysicalStart + desc->NumberOfPages * BASE_PAGE_SIZE;

        create_phys_caps_region(reserved_start, reserved_end, desc->PhysicalStart, desc->NumberOfPages * BASE_PAGE_SIZE, region_type);
    }

    size_t size = (1UL << 48) - last_end_addr;


    create_phys_caps_region(reserved_start, reserved_end, last_end_addr, size, RegionType_PhyAddr);
}

static void init_page_tables(void)
{
    lpaddr_t (*alloc_phys_aligned)(size_t size, size_t align);
    if (cpu_is_bsp()) {
        alloc_phys_aligned = bsp_alloc_phys_aligned;
    } else {
        alloc_phys_aligned = app_alloc_phys_aligned;
    }

    // Create page table for init
    const size_t l0_size = VMSAv8_64_PTABLE_NUM_ENTRIES * INIT_L0_SIZE * sizeof(union armv8_ttable_entry);
    init_l0 = (void *) local_phys_to_mem(alloc_phys_aligned(l0_size, VMSAv8_64_PTABLE_SIZE));
    memset(init_l0, 0, l0_size);

    const size_t l1_size = l0_size * INIT_L1_SIZE;
    init_l1 = (void *) local_phys_to_mem(alloc_phys_aligned(l1_size, VMSAv8_64_PTABLE_SIZE));
    memset(init_l1, 0, l1_size);

    const size_t l2_size = l1_size * INIT_L2_SIZE;
    init_l2 = (void *) local_phys_to_mem(alloc_phys_aligned(l2_size, VMSAv8_64_PTABLE_SIZE));
    memset(init_l2, 0, l2_size);

    const size_t l3_size = l2_size * INIT_L3_SIZE;
    init_l3 = (void *) local_phys_to_mem(alloc_phys_aligned(l3_size, VMSAv8_64_PTABLE_SIZE));
    memset(init_l3, 0, l3_size);

    /* Map pagetables into page CN */
    int pagecn_pagemap = 0;

    /*
     * AARCH64 has:
     *
     * L0 has 1 entry.
     * L1 has 1 entry.
     * L2 Coarse has 16 entries (512 * 8B = 4KB).
     * L3 Coarse has 16*512 entries (512 * 8B = 4KB).
     *
     */

    printk(LOG_NOTE, "init page tables: l0=%p, l1=%p, l2=%p, l3=%p\n",
            init_l0, init_l1, init_l2, init_l3);

    caps_create_new(
            ObjType_VNode_AARCH64_l0,
            mem_to_local_phys((lvaddr_t)init_l0),
            vnode_objsize(ObjType_VNode_AARCH64_l0), 0,
                        my_core_id,
            caps_locate_slot(CNODE(spawn_state.pagecn), pagecn_pagemap++)
    );

    for (size_t i = 0; i < INIT_L1_SIZE; i++) {
        size_t objsize_vnode = vnode_objsize(ObjType_VNode_AARCH64_l1);
        assert(objsize_vnode == BASE_PAGE_SIZE);
        caps_create_new(
                ObjType_VNode_AARCH64_l1,
                mem_to_local_phys((lvaddr_t)init_l1) + (i * objsize_vnode),
                objsize_vnode, 0, my_core_id,
                caps_locate_slot(CNODE(spawn_state.pagecn), pagecn_pagemap++)
        );
    }

    //STARTUP_PROGRESS();
    for(size_t i = 0; i < INIT_L2_SIZE; i++) {
        size_t objsize_vnode = vnode_objsize(ObjType_VNode_AARCH64_l2);
        assert(objsize_vnode == BASE_PAGE_SIZE);
        caps_create_new(
                ObjType_VNode_AARCH64_l2,
                mem_to_local_phys((lvaddr_t)init_l2) + (i * objsize_vnode),
                objsize_vnode, 0, my_core_id,
                caps_locate_slot(CNODE(spawn_state.pagecn), pagecn_pagemap++)
        );
    }

    // Map L3 into successive slots in pagecn
    for(size_t i = 0; i < INIT_L3_SIZE; i++) {
        size_t objsize_vnode = vnode_objsize(ObjType_VNode_AARCH64_l3);
        assert(objsize_vnode == BASE_PAGE_SIZE);
        caps_create_new(
                ObjType_VNode_AARCH64_l3,
                mem_to_local_phys((lvaddr_t)init_l3) + (i * objsize_vnode),
                objsize_vnode, 0,
                my_core_id,
                caps_locate_slot(CNODE(spawn_state.pagecn), pagecn_pagemap++)
        );
    }

    /*
     * Initialize init page tables - this just wires the L0
     * entries through to the corresponding L1 entries.
     */
    for(lvaddr_t vaddr = ARMV8_INIT_VBASE;
        vaddr < ARMV8_INIT_SPACE_LIMIT;
        vaddr += VMSAv8_64_L0_SIZE)
    {
        uintptr_t section = (vaddr - ARMV8_INIT_VBASE) / VMSAv8_64_L0_SIZE;
        uintptr_t l1_off = section * VMSAv8_64_PTABLE_SIZE;
        lpaddr_t paddr = mem_to_local_phys((lvaddr_t)init_l1) + l1_off;
        paging_map_table_l0(init_l0, vaddr, paddr);
    }
    /*
     * Initialize init page tables - this just wires the L1
     * entries through to the corresponding L2 entries.
     */
    for(lvaddr_t vaddr = ARMV8_INIT_VBASE;
        vaddr < ARMV8_INIT_SPACE_LIMIT;
        vaddr += VMSAv8_64_L1_BLOCK_SIZE)
    {
        uintptr_t section = (vaddr - ARMV8_INIT_VBASE) / VMSAv8_64_L1_BLOCK_SIZE;
        uintptr_t l2_off = section * VMSAv8_64_PTABLE_SIZE;
        lpaddr_t paddr = mem_to_local_phys((lvaddr_t)init_l2) + l2_off;
        paging_map_table_l1(init_l1, vaddr, paddr);
    }

    /*
     * Initialize init page tables - this just wires the L2
     * entries through to the corresponding L3 entries.
     */
    STATIC_ASSERT(0 == (ARMV8_INIT_VBASE % VMSAv8_64_L2_BLOCK_SIZE), "");
    for(lvaddr_t vaddr = ARMV8_INIT_VBASE;
        vaddr < ARMV8_INIT_SPACE_LIMIT;
        vaddr += VMSAv8_64_L2_BLOCK_SIZE)
    {
        uintptr_t section = (vaddr - ARMV8_INIT_VBASE) / VMSAv8_64_L2_BLOCK_SIZE;
        uintptr_t l3_off = section * VMSAv8_64_PTABLE_SIZE;

        lpaddr_t paddr = mem_to_local_phys((lvaddr_t)init_l3) + l3_off;

        paging_map_table_l2(init_l2, vaddr, paddr);
    }

}

static struct dcb *spawn_init_common(const char *name,
                                     int argc, const char *argv[],
                                     lpaddr_t bootinfo_phys,
                                     alloc_phys_func alloc_phys,
                                     alloc_phys_aligned_func alloc_phys_aligned)
{
    struct dispatcher_shared_generic *disp;
    struct dispatcher_shared_aarch64 *disp_aarch64;

    MSG("spawn_init_common %s\n", name);

    lvaddr_t paramaddr;
    struct dcb *init_dcb = spawn_module(&spawn_state, name, argc, argv,
                                        bootinfo_phys, INIT_ARGS_VBASE,
                                        alloc_phys, alloc_phys_aligned,
                                        &paramaddr);
    /* initialize page tables */
    init_page_tables();

    init_dcb->vspace = mem_to_local_phys((lvaddr_t)init_l0);

    spawn_init_map(init_l3, ARMV8_INIT_VBASE, INIT_ARGS_VBASE,
                       spawn_state.args_page, ARGS_SIZE, INIT_PERM_RW);

    /* Map dispatcher */
    spawn_init_map(init_l3, ARMV8_INIT_VBASE, INIT_DISPATCHER_VBASE,
                   mem_to_local_phys(init_dcb->disp), DISPATCHER_SIZE,
                   INIT_PERM_RW);

    disp = get_dispatcher_shared_generic(init_dcb->disp);
    disp_aarch64 = get_dispatcher_shared_aarch64(init_dcb->disp);

    /* Initialize dispatcher */
    disp->disabled = true;
    strncpy(disp->name, argv[0], DISP_NAME_LEN);

    /* Tell init the vspace addr of its dispatcher. */
    disp->udisp = INIT_DISPATCHER_VBASE;

    /* TODO: write the contet ID for init */

    /* Set the thread ID register to point to the shared structure. */

    disp_aarch64->enabled_save_area.named.x0   = paramaddr;
    disp_aarch64->enabled_save_area.named.spsr = AARCH64_MODE_USR | CPSR_I_MASK;
    sysreg_write_tpidrro_el0((uint64_t)disp->udisp);

    dump_dispatcher(disp);

    return init_dcb;
}

struct dcb *spawn_bsp_init(const char *name)
{
    MSG("spawning '%s' on BSP core\n", name);
    /* Only the first core can run this code */
    assert(cpu_is_bsp());

    /* Allocate bootinfo */
    lpaddr_t bootinfo_phys = bsp_alloc_phys_aligned(BOOTINFO_SIZE, BASE_PAGE_SIZE);
    memset((void *)local_phys_to_mem(bootinfo_phys), 0, BOOTINFO_SIZE);

    /* store pointer to bootinfo in kernel virtual memory */
    bootinfo = (struct bootinfo *) local_phys_to_mem(bootinfo_phys);

    /* Construct cmdline args */
    char bootinfochar[16];
    snprintf(bootinfochar, sizeof(bootinfochar), "%u", INIT_BOOTINFO_VBASE);
    const char *argv[] = { "init", bootinfochar };
    int argc = 2;

    /* perform common spawning of init domain */
    struct dcb *init_dcb = spawn_init_common(name, argc, argv,bootinfo_phys,
            bsp_alloc_phys, bsp_alloc_phys_aligned);

    /* map boot info into init's VSPACE */
    spawn_init_map(init_l3, ARMV8_INIT_VBASE, INIT_BOOTINFO_VBASE, bootinfo_phys,
                   BOOTINFO_SIZE, INIT_PERM_RW);

    /* load the image */
    genvaddr_t init_ep, got_base;
    struct startup_l3_info l3_info = { init_l3, ARMV8_INIT_VBASE };
    load_init_image(&l3_info, BSP_INIT_MODULE_NAME, &init_ep, &got_base);

    MSG("init loaded with entry=0x%" PRIxGENVADDR " and GOT=0x%" PRIxGENVADDR "\n",
         init_ep, got_base);

    struct dispatcher_shared_aarch64 *disp_aarch64 =
            get_dispatcher_shared_aarch64(init_dcb->disp);

    /* setting GOT pointers */
    disp_aarch64->got_base = got_base;
    /* XXX - Why does the kernel do this? -DC */
    disp_aarch64->enabled_save_area.named.x10  = got_base;
    disp_aarch64->disabled_save_area.named.x10  = got_base;

    /* setting entry points */
    disp_aarch64->disabled_save_area.named.pc   = init_ep;
    disp_aarch64->disabled_save_area.named.spsr = AARCH64_MODE_USR | CPSR_F_MASK;

    /* Create caps for init to use */
    create_module_caps(&spawn_state);
    lpaddr_t init_alloc_end = bsp_alloc_phys(0);
    create_phys_caps(armv8_glbl_core_data->start_kernel_ram, init_alloc_end);

    /* Fill bootinfo struct */
    bootinfo->mem_spawn_core = KERNEL_IMAGE_SIZE; // Size of kernel

    return init_dcb;
}

struct dcb *spawn_app_init(struct armv8_core_data *core_data,
                           const char *name)
{
    errval_t err;

    MSG("spawning '%s' on APP core\n", name);

    /* Only the app core can run this code */
    assert(!cpu_is_bsp());

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



    struct dcb *init_dcb= spawn_init_common(name, argc, argv, 0, app_alloc_phys,
                                            app_alloc_phys_aligned);


    MSG("creating monitor URPC frame cap\n");
    // Urpc frame cap
    struct cte *urpc_frame_cte = caps_locate_slot(CNODE(spawn_state.taskcn),
                                                  TASKCN_SLOT_MON_URPC);

    // XXX: Create as devframe so the memory is not zeroed out
    err = caps_create_new(ObjType_DevFrame,
                          core_data->urpc_frame.base,
                          core_data->urpc_frame.length,
                          core_data->urpc_frame.length,
                          my_core_id,
                          urpc_frame_cte);
    assert(err_is_ok(err));
    urpc_frame_cte->cap.type = ObjType_Frame;
    lpaddr_t urpc_ptr = gen_phys_to_local_phys(urpc_frame_cte->cap.u.frame.base);


    /* Map urpc frame at MON_URPC_BASE */
    MSG("mapping URPC frame cap %" PRIxLPADDR" \n",urpc_ptr );
    spawn_init_map(init_l3, ARMV8_INIT_VBASE, MON_URPC_VBASE, urpc_ptr,
                   MON_URPC_SIZE, INIT_PERM_RW);

    struct startup_l3_info l3_info = { init_l3, ARMV8_INIT_VBASE };

    // elf load the domain
    genvaddr_t entry_point, got_base=0;

    MSG("loading elf '%s' @ %" PRIxLPADDR "\n", name,
        local_phys_to_mem(core_data->monitor_binary.base));

    err = elf_load(EM_AARCH64, startup_alloc_init, &l3_info,
            local_phys_to_mem(core_data->monitor_binary.base),
            core_data->monitor_binary.length, &entry_point);
    if (err_is_fail(err)) {
        //err_print_calltrace(err);
        panic("ELF load of init module failed!");
    }

    // TODO: Fix application linkage so that it's non-PIC.
    struct Elf64_Shdr* got_shdr;
    got_shdr = elf64_find_section_header_name(local_phys_to_mem(core_data->monitor_binary.base),
                                           core_data->monitor_binary.length, ".got");
    if (got_shdr)
    {
        got_base = got_shdr->sh_addr;
    }

    MSG("init loaded with entry=0x%" PRIxGENVADDR " and GOT=0x%" PRIxGENVADDR "\n",
        entry_point, got_base);

    struct dispatcher_shared_aarch64 *disp_aarch64 =
            get_dispatcher_shared_aarch64(init_dcb->disp);

    disp_aarch64->got_base = got_base;
    disp_aarch64->enabled_save_area.named.x10  = got_base;
    disp_aarch64->disabled_save_area.named.x10  = got_base;

    /* setting entry points */
    disp_aarch64->disabled_save_area.named.pc   = entry_point;
    disp_aarch64->disabled_save_area.named.spsr = AARCH64_MODE_USR | CPSR_F_MASK;
    //arch_set_thread_register(INIT_DISPATCHER_VBASE);

    MSG("init dcb set up\n");

    return init_dcb;

}

void arm_kernel_startup(void *pointer)
{
    /* Initialize the core_data */
    /* Used when bringing up other cores, must be at consistent global address
     * seen by all cores */

    // Initialize system timers
    timers_init(config_timeslice);

    struct dcb *init_dcb;

    if(cpu_is_bsp()) {
        MSG("Doing BSP related bootup \n");

        /* Initialize the location to allocate phys memory from */
        printf("start_free_ram = 0x%lx\n", armv8_glbl_core_data->start_free_ram);
        bsp_init_alloc_addr = armv8_glbl_core_data->start_free_ram;

        /* allocate initial KCB */
        kcb_current= (struct kcb *)local_phys_to_mem(
                bsp_alloc_phys(sizeof(*kcb_current)));
        assert(kcb_current);
        memset(kcb_current, 0, sizeof(*kcb_current));

        init_dcb = spawn_bsp_init(BSP_INIT_MODULE_NAME);

//        pit_start(0);
    } else {
        MSG("Doing non-BSP related bootup \n");
        struct armv8_core_data *core_data = (struct armv8_core_data *)pointer;

        my_core_id = core_data->dst_core_id;

        /* Initialize the allocator */


        app_alloc_phys_start = (core_data->memory.base);
        app_alloc_phys_end   = (core_data->memory.length + app_alloc_phys_start);

        MSG("Memory: %lx, %lx, size=%zu kB\n", app_alloc_phys_start, app_alloc_phys_end,
            (app_alloc_phys_end - app_alloc_phys_start + 1) >> 10);

        kcb_current= (struct kcb *)local_phys_to_mem(core_data->kcb);

        init_dcb = spawn_app_init(core_data, APP_INIT_MODULE_NAME);

       // uint32_t irq = gic_get_active_irq();
       // gic_ack_irq(irq);
    }


    // enable interrupt forwarding to cpu
    platform_gic_cpu_interface_enable();


    MSG("Calling dispatch from arm_kernel_startup, entry point %#"PRIxLVADDR"\n",
            get_dispatcher_shared_aarch64(init_dcb->disp)->disabled_save_area.named.pc);

    // Should not return
    dispatch(init_dcb);

    panic("Error spawning init!");

}
