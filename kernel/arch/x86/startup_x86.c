/**
 * \file
 * \brief x86 kernel bootup code.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>

#include <dispatch.h>
#include <elf/elf.h>
#include <exec.h>
#include <init.h>
#include <getopt/getopt.h>
#include <kcb.h>
#include <kernel_multiboot.h>
#include <irq.h>
#include <kputchar.h>
#include <mdb/mdb_tree.h>
#ifdef CONFIG_MICROBENCHMARKS
#include <microbenchmarks.h>
#endif
#include <paging_kernel_arch.h>
#include <startup.h>
#include <string.h>
#include <wakeup.h>
#include <barrelfish_kpi/cpu.h>
#include <barrelfish_kpi/init.h>
#include <barrelfish_kpi/paging_arch.h>
#include <barrelfish_kpi/syscalls.h>
#include <arch/x86/apic.h>
#include <target/x86/barrelfish_kpi/coredata_target.h>
#include <arch/x86/startup_x86.h>
#include <dev/ia32_dev.h>

#ifdef __scc__
#       include <rck.h>
#endif

/// Optional core ID to use for the BSP core (command-line argument)
static int bsp_coreid;

/// Quick way to find the base address of a cnode capability
#define CNODE(cte)     (cte)->cap.u.cnode.cnode

/// Pointer to bootinfo structure for init
static struct bootinfo *bootinfo = (struct bootinfo *)BOOTINFO_BASE;

/**
 * Each kernel has a local copy of global and locks. However, during booting and
 * kernel relocation, these are set to point to global of the pristine kernel,
 * so that all the kernels can share it.
 */
static  struct global myglobal;
struct global *global = &myglobal;

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

    memset((void*)local_phys_to_mem(addr), 0, npages * BASE_PAGE_SIZE);

    return addr;
}

/**
 * The address from where bsp_alloc_phys will start allocating memory
 */
static lpaddr_t bsp_init_alloc_addr = 0;

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
static lpaddr_t bsp_alloc_phys(size_t size)
{
    // round to base page size
    uint32_t npages = (size + BASE_PAGE_SIZE - 1) / BASE_PAGE_SIZE;

    assert(bsp_init_alloc_addr != 0);
    lpaddr_t addr = bsp_init_alloc_addr;

    bsp_init_alloc_addr += npages * BASE_PAGE_SIZE;

    memset((void*)local_phys_to_mem(addr), 0, npages * BASE_PAGE_SIZE);

    return addr;
}

/**
 * \brief Map init user-space memory.
 *
 * This function maps pages of the init user-space module. It expects
 * the virtual base address 'vbase' of a program segment of the init executable,
 * its size 'size' and its ELF64 access control flags. It maps pages
 * into physical memory that is allocated on the fly and puts
 * corresponding frame caps into init's segcn.
 *
 * \param vbase Virtual base address of program segment.
 * \param size  Size of program segment in bytes.
 * \param flags ELF64 access control flags of program segment.
 * \param ret   Used to return base region pointer
 */
errval_t startup_alloc_init(void *state, genvaddr_t gvbase, size_t size,
                            uint32_t flags, void **ret)
{

    errval_t err;

    struct spawn_state *spawn_state = state;

    lvaddr_t vbase = (lvaddr_t)gvbase; /* XXX */
    lvaddr_t offset = BASE_PAGE_OFFSET(vbase);

    /* Page align the parameters */
    paging_align(&vbase, NULL, &size, BASE_PAGE_SIZE);

    lpaddr_t pbase = 0, paddr = 0;
    for(lvaddr_t i = vbase; i < vbase + size; i += BASE_PAGE_SIZE) {
        if (apic_is_bsp()) {
            paddr = bsp_alloc_phys(BASE_PAGE_SIZE);
        } else {
            paddr = app_alloc_phys(BASE_PAGE_SIZE);
        }

        if(pbase == 0) {
            pbase = paddr;
        }

        err = startup_map_init(i, paddr, BASE_PAGE_SIZE, flags);
        assert(err_is_ok(err));
    }

    if (apic_is_bsp()) {
        // Create frame caps for segcn
        paddr += BASE_PAGE_SIZE;

        debug(SUBSYS_STARTUP,
              "Allocated physical memory [0x%"PRIxLPADDR", 0x%"PRIxLPADDR"]\n",
              pbase, paddr - pbase);

        err = create_caps_to_cnode(pbase, paddr - pbase,
                                   RegionType_RootTask, spawn_state, bootinfo);
        if (err_is_fail(err)) {
            return err;
        }
    }

    assert(ret != NULL);
    *ret = (void *)(vbase + offset);

    return SYS_ERR_OK;
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
    err = caps_create_new(ObjType_Frame, mmstrings_phys, BASE_PAGE_BITS,
                          BASE_PAGE_BITS, my_core_id,
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

        // Create max-sized caps to multiboot module in module cnode
        while (remain > 0) {
            assert((base_addr & BASE_PAGE_MASK) == 0);
            assert((remain & BASE_PAGE_MASK) == 0);

            // determine size of next chunk
            uint8_t block_size = bitaddralign(remain, base_addr);

            assert(st->modulecn_slot < (1UL << st->modulecn->cap.u.cnode.bits));
            // create as DevFrame cap to avoid zeroing memory contents
            err = caps_create_new(ObjType_DevFrame, base_addr, block_size,
                                  block_size, my_core_id,
                                  caps_locate_slot(CNODE(st->modulecn),
                                                   st->modulecn_slot++));
            assert(err_is_ok(err));

            // Advance by that chunk
            base_addr += ((genpaddr_t)1 << block_size);
            remain -= ((genpaddr_t)1 << block_size);
        }

        // Copy multiboot module string to mmstrings area
        strcpy((char *)mmstrings, MBADDR_ASSTRING(m->string));
        mmstrings += strlen(MBADDR_ASSTRING(m->string)) + 1;
        assert(mmstrings < mmstrings_base + BASE_PAGE_SIZE);
    }
}

void cleanup_bios_regions(char *mmap_addr, char **new_mmap_addr,
                          uint32_t *new_mmap_length)
{
    assert(new_mmap_addr);
    assert(new_mmap_length);
#define PRINT_REGIONS(map, map_length) do {\
        for(char * printcur = map; printcur < map + map_length;) {\
            struct multiboot_mmap * printcurmmap = (struct multiboot_mmap * SAFE)TC(printcur);\
            printf("\t0x%08"PRIx64" - 0x%08"PRIx64" Type: %"PRIu32" Length: 0x%"PRIx64"\n", printcurmmap->base_addr, printcurmmap->base_addr + printcurmmap->length, printcurmmap->type, printcurmmap->length);\
            printcur += printcurmmap->size + 4;\
        }\
    } while (0)

    printf("Raw MMAP from BIOS\n");
    PRINT_REGIONS(mmap_addr, glbl_core_data->mmap_length);

    // normalize memory regions
    lpaddr_t clean_base = bsp_alloc_phys(glbl_core_data->mmap_length);
    char *clean_mmap_addr = (char *)local_phys_to_mem(clean_base);
    uint32_t clean_mmap_length = glbl_core_data->mmap_length;
    memcpy(clean_mmap_addr, mmap_addr, glbl_core_data->mmap_length);

    // first of all, sort regions by base address
    // yes, it's a bubble sort, but the dataset is small and usually in the right order
    bool swapped;
    do {
        swapped = false;

        for(char * cur = clean_mmap_addr; cur < clean_mmap_addr + clean_mmap_length;) {
            struct multiboot_mmap * curmmap = (struct multiboot_mmap * SAFE)TC(cur);
            if (cur + curmmap->size + 4 >= clean_mmap_addr + clean_mmap_length)
                break; // do not try to move this check into the forloop as entries do not have to be the same length

            struct multiboot_mmap * nextmmap = (struct multiboot_mmap * SAFE)TC(cur + curmmap->size + 4);

            if (nextmmap->base_addr < curmmap->base_addr ||
                (nextmmap->base_addr == curmmap->base_addr && nextmmap->length > curmmap->length)) {
                // swap
                assert(curmmap->size == 20); // FIXME: The multiboot specification does not require this size
                assert(nextmmap->size == 20);

                struct multiboot_mmap tmp;
                tmp = *curmmap;
                *curmmap = *nextmmap;
                *nextmmap = tmp;

                swapped = true;
            }

            cur += curmmap->size + 4;
        }
    } while(swapped);

    printf("Sorted MMAP\n");
    PRINT_REGIONS(clean_mmap_addr, clean_mmap_length);

    // now merge consecutive memory regions of the same or lower type
    for(char * cur = clean_mmap_addr; cur < clean_mmap_addr + clean_mmap_length;) {
        struct multiboot_mmap * curmmap = (struct multiboot_mmap * SAFE)TC(cur);
        if (cur + curmmap->size + 4 >= clean_mmap_addr + clean_mmap_length)
            break; // do not try to move this check into the forloop as entries do not have to be the same length

        struct multiboot_mmap * nextmmap = (struct multiboot_mmap * SAFE)TC(cur + curmmap->size + 4);

        /* On some machines (brie1) the IOAPIC region is only 1kB.
         * Currently we're not able to map regions that are <4kB so we
         * make sure that every region (if there is no problematic overlap)
         * is at least BASE_PAGE_SIZEd (==4kB) here.
         */
        if ((curmmap->length < BASE_PAGE_SIZE) && (curmmap->base_addr + BASE_PAGE_SIZE <= nextmmap->base_addr)) {
            curmmap->length = BASE_PAGE_SIZE;
        }

#define DISCARD_NEXT_MMAP do {\
    uint32_t discardsize = nextmmap->size + 4;\
    memmove(cur + curmmap->size + 4, cur + curmmap->size + 4 + discardsize, clean_mmap_length - (cur - clean_mmap_addr) - curmmap->size - 4 - discardsize);\
    clean_mmap_length -= discardsize;\
    } while (0)

#define BUBBLE_NEXT_MMAP do {\
    for (char * bubblecur = cur + curmmap->size + 4; bubblecur < clean_mmap_addr + clean_mmap_length;){\
        struct multiboot_mmap * bubblecur_mmap = (struct multiboot_mmap * SAFE)TC(bubblecur);\
        if (bubblecur + bubblecur_mmap->size + 4 >= clean_mmap_addr + clean_mmap_length)\
            break;\
        struct multiboot_mmap * bubblenext_mmap = (struct multiboot_mmap * SAFE)TC(bubblecur + bubblecur_mmap->size + 4);\
        if (bubblenext_mmap->base_addr < bubblecur_mmap->base_addr || (bubblecur_mmap->base_addr == bubblenext_mmap->base_addr && bubblenext_mmap->length > bubblecur_mmap->length)) {\
            struct multiboot_mmap bubbletmp; bubbletmp = *bubblecur_mmap; *bubblecur_mmap = *bubblenext_mmap; *bubblenext_mmap = bubbletmp;\
        } else break;\
    }} while(0)


        bool reduced = false;
        do {
            reduced = false;

            if (curmmap->base_addr == nextmmap->base_addr) {
                // regions start at the same location
                if (curmmap->length == nextmmap->length) {
                    // trivial case. They are the same. Choose higher type and discard next
                    curmmap->type = max(curmmap->type, nextmmap->type);

                    DISCARD_NEXT_MMAP;

                    reduced = true;
                    continue;
                } else {
                    // next region is smaller (we sorted that way)
                    if (nextmmap->type <= curmmap->type) {
                        // next regions type is the same or smaller. discard
                        DISCARD_NEXT_MMAP;

                        reduced = true;
                        continue;
                    } else {
                        // next regions type is higher, so it gets priority
                        // change type of current region and shrink next
                        uint32_t tmptype = curmmap->type;
                        uint64_t newlength = curmmap->length - nextmmap->length;
                        curmmap->type = nextmmap->type;
                        curmmap->length = nextmmap->length;
                        nextmmap->type = tmptype;
                        nextmmap->base_addr += nextmmap->length;
                        nextmmap->length = newlength;

                        // now we need to bubble next to the right place to restore order
                        BUBBLE_NEXT_MMAP;

                        reduced = true;
                        continue;
                    }
                }
            }

            // regions overlap
            if (nextmmap->base_addr > curmmap->base_addr && nextmmap->base_addr < curmmap->base_addr + curmmap->length) {
                // same type
                if (curmmap->type == nextmmap->type) {
                    // simple. just extend if necessary and discard next
                    if (nextmmap->base_addr + nextmmap->length > curmmap->base_addr + curmmap->length)
                        curmmap->length = (nextmmap->base_addr + nextmmap->length) - curmmap->base_addr;

                    DISCARD_NEXT_MMAP;

                    reduced = true;
                    continue;
                } else {
                    // type is not the same
                    if (nextmmap->base_addr + nextmmap->length < curmmap->base_addr + curmmap->length) {
                        // there is a chunk at the end. create a new region
                        struct multiboot_mmap tmpmmap;
                        tmpmmap.size = 20;
                        tmpmmap.base_addr = nextmmap->base_addr + nextmmap->length;
                        tmpmmap.length = (curmmap->base_addr + curmmap->length) - (nextmmap->base_addr + nextmmap->length);
                        tmpmmap.type = curmmap->type;

                        // move everything to make room
                        assert(clean_mmap_length + tmpmmap.length + 4 < BOOTINFO_SIZE);
                        memmove(cur + curmmap->size + 4 + tmpmmap.size + 4, cur + curmmap->size + 4, clean_mmap_length - ((cur - clean_mmap_addr) + curmmap->size + 4));
                        clean_mmap_length += tmpmmap.size + 4;

                        // insert new
                        *nextmmap = tmpmmap;

                        // restore order
                        BUBBLE_NEXT_MMAP;

                        reduced = true;
                    }

                    // after the previous step, the next region either ends
                    // at the same location as the current or is longer
                    uint64_t overlap = (curmmap->base_addr + curmmap->length) - nextmmap->base_addr;

                    if (curmmap-> type > nextmmap->type) {
                        // current has priority, shrink next and extend current
                        nextmmap->length -= overlap;
                        nextmmap->base_addr += overlap;
                        curmmap->length += overlap;

                        if (nextmmap->length == 0)
                            DISCARD_NEXT_MMAP;

                        reduced = true;
                        continue;
                    } else {
                        // next has priority, shrink current and extend next
                        nextmmap->length += overlap;
                        nextmmap->base_addr -= overlap;
                        curmmap->length -= overlap;

                        reduced = true;
                        continue;
                    }
                }
            }
        } while (reduced);

        cur += curmmap->size + 4;

#undef DISCARD_NEXT_MMAP
#undef BUBBLE_NEXT_MMAP
    }

    printf("Preprocessed MMAP\n");
    PRINT_REGIONS(clean_mmap_addr, clean_mmap_length);

    // we can only map pages. Therefore page align regions
    for(char * cur = clean_mmap_addr; cur < clean_mmap_addr + clean_mmap_length;) {
        struct multiboot_mmap * curmmap = (struct multiboot_mmap * SAFE)TC(cur);
        if (cur + curmmap->size + 4 >= clean_mmap_addr + clean_mmap_length)
            break; // do not try to move this check into the forloop as entries do not have to be the same length

        struct multiboot_mmap * nextmmap = (struct multiboot_mmap * SAFE)TC(cur + curmmap->size + 4);

        if (nextmmap->base_addr & BASE_PAGE_MASK) {
            uint64_t offset = nextmmap->base_addr - ((nextmmap->base_addr >> BASE_PAGE_BITS) << BASE_PAGE_BITS);

            // round in favour of higher type
            if (curmmap->type > nextmmap->type) {
                curmmap->length += BASE_PAGE_SIZE - offset;
                nextmmap->base_addr += BASE_PAGE_SIZE - offset;
                nextmmap->length -= BASE_PAGE_SIZE - offset;
            } else {
                curmmap->length -= offset;
                nextmmap->base_addr -= offset;
                nextmmap->length += offset;
            }
        }

        cur += curmmap->size + 4;
    }

    printf("Pagealigned MMAP\n");
    PRINT_REGIONS(clean_mmap_addr, clean_mmap_length);

#undef PRINT_REGIONS
    *new_mmap_addr = clean_mmap_addr;
    *new_mmap_length = clean_mmap_length;
    return;
}

// XXX from serial.c
extern int serial_portbase;

static struct cmdarg cmdargs[] = {
    {"loglevel", ArgType_Int, { .integer = &kernel_loglevel }},
    {"logmask", ArgType_Int, { .integer = &kernel_log_subsystem_mask }},
    {"ticks", ArgType_Bool, { .boolean = &kernel_ticks_enabled }},
    {"timeslice", ArgType_Int, { .integer = &kernel_timeslice }},
#ifndef __scc__ // FIXME: why not?
    {"serial", ArgType_Int, { .integer = &serial_portbase }},
#endif
    {"bsp_coreid", ArgType_Int, { .integer = &bsp_coreid }},
    {NULL, 0, {NULL}}
};

/**
 * Name of multiboot module containing program for init domains.
 */
#if defined(__k1om__)
#       define BSP_INIT_MODULE_PATH     BF_BINARY_PREFIX "k1om/sbin/init"
#elif defined(__x86_64__)
#       define BSP_INIT_MODULE_PATH     BF_BINARY_PREFIX "x86_64/sbin/init"
#elif defined(__scc__)
#       define BSP_INIT_MODULE_PATH     BF_BINARY_PREFIX "scc/sbin/init"
#elif defined(__i386__)
#       define BSP_INIT_MODULE_PATH     BF_BINARY_PREFIX "x86_32/sbin/init"
#else
#       error "Unknown x86"
#endif
#define BSP_INIT_PROG_NAME       "init"
#define APP_INIT_PROG_NAME       "monitor"

/**
 * \brief Kernel's early startup code, called from arch-specific bootstrap.
 */
void kernel_startup_early(void)
{
    const char *cmdline;
    assert(glbl_core_data != NULL);
    cmdline = MBADDR_ASSTRING(glbl_core_data->cmdline);
    parse_commandline(cmdline, cmdargs);
}

/**
 * \brief Kernel's main startup code, called from arch-specific bootstrap.
 *
 * This function never returns.
 */
extern bool verbose_dispatch;
void kernel_startup(void)
{
#ifdef CONFIG_MICROBENCHMARKS
    printk(LOG_NOTE, "\nRunning microbenchmarks...\n");
    microbenchmarks_run_all();
#endif

    /* Initialize the core_data */
    /* Used when bringing up other cores, must be at consistent global address
     * seen by all cores */
    struct x86_core_data *core_data
        = (void *)((lvaddr_t)&_start_kernel - BASE_PAGE_SIZE);

    struct dcb *init_dcb;
    if (apic_is_bsp()) {
        if (bsp_coreid != 0) {
            my_core_id = bsp_coreid;
        }

        /* Initialize the location to allocate phys memory from */
        bsp_init_alloc_addr = glbl_core_data->start_free_ram;

        /* allocate initial KCB */
        kcb_current = (struct kcb *) local_phys_to_mem(bsp_alloc_phys(sizeof(*kcb_current)));
        memset(kcb_current, 0, sizeof(*kcb_current));
        assert(kcb_current);

        /* spawn init */
        init_dcb = spawn_bsp_init(BSP_INIT_MODULE_PATH, bsp_alloc_phys);
    } else {
        kcb_current = (struct kcb *)
            local_phys_to_mem((lpaddr_t) kcb_current);

        start_ap_signal();
        // if we have a kernel control block, use it
        if (kcb_current && kcb_current->is_valid) {
            debug(SUBSYS_STARTUP, "have valid kcb, restoring state\n");
            print_kcb();

            // restore mdb
            errval_t err = mdb_init(kcb_current);
            if (err_is_fail(err)) {
                panic("couldn't restore mdb");
            }
            // figure out if we need to convert scheduler state
#ifdef CONFIG_SCHEDULER_RR
            if (kcb_current->sched != SCHED_RR) {
                printf("converting scheduler state to RR\n");
                scheduler_convert();
            }
#elif CONFIG_SCHEDULER_RBED
            if (kcb_current->sched != SCHED_RBED) {
                printf("converting scheduler state to RBED\n");
                scheduler_convert();
            }
#else
#error must define scheduler
#endif
            // update core id of domains
            kcb_update_core_id(kcb_current);
            // set queue pointers
            scheduler_restore_state();
            // restore wakeup queue state
            printk(LOG_DEBUG, "%s:%s:%d: kcb_current->wakeup_queue_head = %p\n",
                   __FILE__, __FUNCTION__, __LINE__, kcb_current->wakeup_queue_head);
            wakeup_set_queue_head(kcb_current->wakeup_queue_head);

            printk(LOG_DEBUG, "%s:%s:%d: dcb_current = %p\n",
                   __FILE__, __FUNCTION__, __LINE__, dcb_current);
            struct dcb *next = schedule();
            debug(SUBSYS_STARTUP, "next = %p\n", next);
            if (next != NULL) {
                assert (next->disp);
                struct dispatcher_shared_generic *dst =
                    get_dispatcher_shared_generic(next->disp);
                debug(SUBSYS_STARTUP, "scheduling '%s' from restored state\n",
                      dst->name);
            }
            // interrupt state should be fine, as it's used directly from the
            // kcb.
            dispatch(next);
            panic("should not get here!");
        }
        my_core_id = core_data->dst_core_id;

        /* Initialize the allocator */
        app_alloc_phys_start = core_data->memory_base_start;
        app_alloc_phys_end   = ((lpaddr_t)1 << core_data->memory_bits) +
                                    app_alloc_phys_start;

        init_dcb = spawn_app_init(core_data, APP_INIT_PROG_NAME, app_alloc_phys);
    }

    // Should not return
    //if (apic_is_bsp()) {
    dispatch(init_dcb);
    //}
    panic("Error spawning init!");
}

/*
 * Configure the IA32_PAT_MSR register such that PA4 is write-combining and
 * PA5 is write-protect.
 */
void configure_page_attribute_table(void)
{
    ia32_t ia32;
    ia32_cr_pat_t pat;

    ia32_initialize(&ia32);

    pat = ia32_cr_pat_rd(&ia32);

    pat = ia32_cr_pat_pa4_insert(pat, ia32_wc);
    pat = ia32_cr_pat_pa5_insert(pat, ia32_wp);

    ia32_cr_pat_wr(&ia32, pat);

    debug(SUBSYS_STARTUP, "Configured IA32_PAT_MSR.\n");
}
