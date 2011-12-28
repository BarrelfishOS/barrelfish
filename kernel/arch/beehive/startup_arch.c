/**
 * \file
 * \brief Architecture-dependent kernel bootup code.
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
#include <stdio.h>
#include <stdlib.h>
#include <string.h> // for strcpy
#include <barrelfish_kpi/init.h>
#include <capabilities.h> // not to be confused with the next one
#include <barrelfish_kpi/capabilities.h> // before capbits.h
#include <barrelfish_kpi/capbits.h> // in build directory
#include <dispatch.h>
#include <startup.h>
#include <barrelfish_kpi/paging_arch.h> // for BASE_PAGE_SIZE
#include <target/beehive/barrelfish_kpi/coredata_target.h>
#include "beekernel.h"

/// Quick way to find the base address of a cnode capability
#define CNODE(cte)     (cte)->cap.u.cnode.cnode

// findbits.S

extern int find_least_clear32(uint32_t value);
//extern int find_least_set32(uint32_t value);
extern int find_least_clear64(uint64_t value);
extern int find_least_set64(uint64_t value);
extern int find_highest_clear32(uint32_t value);
//extern int find_highest_set32(uint32_t value);
extern int find_highest_clear64(uint64_t value);
extern int find_highest_set64(uint64_t value);
extern int population_count(uint32_t value);


void kernel_bsp_startup(void* initaddr, pstringtable_t strings);
void kernel_app_startup(struct beehive_core_data *bcd);
void kernel_alloc_init(lpaddr_t base, lpaddr_t limit);

static struct spawn_state spawn_state;

// Kernel static data
#define MAX_IMAGES 16
bexec_t *MovedImages[MAX_IMAGES];
int MovedImageCount;



/**
 * The address from where alloc_phys will start allocating memory
 */
static lpaddr_t init_alloc_addr;
static lpaddr_t init_alloc_limit;
static int alloc_phys_pages = 0;

/**
 * \brief Initialise linear physical memory allocator.
 *
 * This function initialises the allocator 
 *
 * \param base  Start of allocation region
 * \param limit End of allocation region
 *
 */
void kernel_alloc_init(lpaddr_t base, lpaddr_t limit)
{
    init_alloc_addr = base;
    init_alloc_limit = limit;
}

/**
 * \brief Linear physical memory allocator.
 *
 * This function allocates a linear region of addresses of size 'size' from
 * physical memory. XXX Performs no checking on whether it is safe to do so
 *
 * \param size  Number of bytes to allocate.
 *
 * \return Base physical address of memory region.
 */
static inline lpaddr_t alloc_phys(size_t size)
{
    // round to base page size
    size_t npages = (size + BASE_PAGE_SIZE - 1) / BASE_PAGE_SIZE;

    assert(init_alloc_addr != 0);
    lpaddr_t addr = init_alloc_addr;

    init_alloc_addr += npages * BASE_PAGE_SIZE;
    alloc_phys_pages += npages;

    if(init_alloc_addr > init_alloc_limit) {
        printf("Out of memory: %d pages allocated\n", alloc_phys_pages);
    }
    assert(init_alloc_addr <= init_alloc_limit);

    //printf("alloc_phys(%x) => %x\n", size, addr);

    return addr;
}

// On most systems this is called _end, but on beehive it is a
// word/code address, and must be shifted for data accesses.
extern void _code_ibefore(void);
extern void _code_iafter(void);
extern void _data_iafter(void);

#define ROUNDUP(x,y) (((x)+(y)-1) & ~((y)-1))

// </This section should be in a arch header file>


#define AddRegionStr(_base, _size, _type, _string) \
    do { \
        uintptr_t base = (_base); \
        size_t size = (_size); \
        __typeof__((_type)) type = (_type); \
	assert((base) == location); \
        assert((size) > 0); \
	assert(bootinfo->regions_length < MAX_MEM_REGIONS); \
	bootinfo->regions[bootinfo->regions_length] = (struct mem_region){ .mr_base = (base), .mr_type = (type), .mr_bits = find_highest_set32(size) + (population_count(size) > 1 ? 1 : 0), .mrmod_size = (size), .mrmod_data = (_string) }; \
	bootinfo->regions_length++; \
	location = (base) + (size); \
        printf("Region 0x%08X size %lu to 0x%08X type %d\n", (base), (size), (location), (type)); \
    } while(0)

#define AddRegion(_base, _size, _type) AddRegionStr(_base, _size, _type, 0)

static void create_regions_from_moved_images(struct bootinfo *const bootinfo, pstringtable_t strings)
{
    assert(bootinfo != NULL);
    assert(strings != NULL);
    printf("strings->soffset[0] = %d\n", strings->soffset[0]);
    printf("strings[0] = \"%s\"\n", 
	   (char*)strings + strings->soffset[0]);
    assert(strings->soffset[0] < strings->slength);

    uintptr_t location = 0;

    // First: create the module information from the MovedImages
    // Kernel at 0x2000
    assert((((uintptr_t)&_code_ibefore)<< 2) == 0x8000); // if not check this code

    AddRegion(0, 0x1000, RegionType_PlatformData); // 4kB magic code at 0
    AddRegion(0x1000, 0x3000, RegionType_PlatformData); // 12kB reserved
    AddRegion(0x4000, 0x2000, RegionType_PlatformData); // 16*512B debug areas
    AddRegion(0x6000, 0x2000, RegionType_PlatformData); // 8k hypervisor
    AddRegionStr(0x8000,
	      ROUNDUP(
		  ((((uintptr_t)&_data_iafter)<<2)-0x8000), BASE_PAGE_SIZE),
	      RegionType_Module, strings->soffset[0]); // Kernel

    for(int i=0; i<MovedImageCount; i++) {
	bexec_t *bexec = MovedImages[i];
	if (location != (bexec->btorg << 2))
	    panic("Bsplice logic failure in boot image");
        printf("strings->soffset[%d] = %d\n", i+1, strings->soffset[i+1]);
        printf("strings[%d] = \"%s\"\n", 
               i+1, (char*)strings + strings->soffset[i+1]);
	assert(strings->soffset[i+1] < strings->slength);
	AddRegionStr((bexec->btorg << 2), 
		     ROUNDUP(
			 ((bexec->bborg + bexec->bbsize - bexec->btorg) << 2), BASE_PAGE_SIZE),
		     RegionType_Module,
		     strings->soffset[i+1]);
    }

    // Need to check whether this is the simulator or the real HW

    // Again we add a Consumed region just to fill the gap - 
    // it should be ignored by init.
#if 0
    AddRegion(location, ((size_t)2 << 30) - 4096 -location, RegionType_Consumed);
    AddRegion(((size_t)2<<30) - 4096, 4096, RegionType_PlatformData); // I cache flush
#endif
}



/// Setup the module cnode, which contains frame caps to all boot loaded modules
static void create_module_caps(struct bootinfo * bootinfo, pstringtable_t strings)
{
    errval_t err;
    int first_slot = 0;

    /* Walk over the memoty regions creating frame caps */
    for(int i=0; i<bootinfo->regions_length; i++) {
	struct mem_region *region = &bootinfo->regions[i];
	if (region->mr_type != RegionType_Module)
	    continue;

        first_slot = spawn_state.modulecn_slot;

#if 0
	printf("create_module_caps: for %" PRIxGENPADDR " to +%#x = %" PRIxGENPADDR " from slot %d\n",
	       region->mr_base, region->mrmod_size, 
               region->mr_base + region->mrmod_size, first_slot);
#endif

	// Create a single cap per module
	size_t remain = region->mrmod_size;
	lpaddr_t base_addr = region->mr_base;

	while (remain > 0) {
	    assert((base_addr & BASE_PAGE_MASK) == 0);
	    assert((remain & BASE_PAGE_MASK) == 0);

	    uint8_t block_size = bitaddralign(remain, base_addr);

            assert(spawn_state.modulecn_slot
                   < (1UL << spawn_state.modulecn->cap.u.cnode.bits));
            // create as DevFrame cap to avoid zeroing memory contents
            err = caps_create_new(ObjType_DevFrame, base_addr, block_size,
                        block_size,
                        caps_locate_slot(spawn_state.modulecn->cap.u.cnode.cnode,
                                         spawn_state.modulecn_slot++));
            assert(err_is_ok(err));

	    // Advance by that chunk
	    base_addr += (1UL << block_size);
	    remain -= (1UL << block_size);
	}

#if 0	    
        for (lpaddr_t pa = region->mr_base; 
             pa < region->mr_base + region->mrmod_size;
             pa += BASE_PAGE_SIZE) {

            assert((pa & BASE_PAGE_MASK) == 0);
            assert(modulecn_slot < (1UL << modulecn->cap.u.cnode.bits));
            // create as DevFrame cap to avoid zeroing memory contents
            err = caps_create_new(ObjType_DevFrame, pa, BASE_PAGE_BITS,
                        BASE_PAGE_BITS,
                        caps_locate_slot(modulecn->cap.u.cnode.cnode,
                                         modulecn_slot++));
            assert(err_is_ok(err));
        }
#endif // 0

        // Replace mr_base with the modulecn slot number for the caps
        region->mrmod_slot = first_slot;
    }
}


/// Create physical address range or RAM caps to unused physical memory
static void create_phys_caps(struct bootinfo *bootinfo)
{
    /* XXX PBAR - now we need to create caps for all usable physical
       memory so that init can hand these to mem_serv.  

       This will probably involve walking the boot images just like
       MoveImages() did and record the current state of the address
       space into the mem_regions datatructure (which in turn is
       passed to init in the bootinfo so it can initilize the
       mem_serv).  

       MoveImages currently fiddles with the memregions - but this is
       most likely best done here since we also need to deal with any
       reserved physical addresses such as the 'context save areas'
       for each core and any datastrcutres residing at 'well-known'
       addresses (i.e. not allocated using alloc_phys()).  Not 100%
       sure how we record the memory which alloc_phys has handed out
       ... perhaps look at current value of init_alloc_addr */

    // XXX GUESS.  Create a cap from where init_alloc_addr got to
    // until the end of RAM.
    errval_t err;
    err = create_caps_to_cnode(init_alloc_addr,
			       ((size_t)(0x80000000U - 4096) - init_alloc_addr),
			       RegionType_Empty, &spawn_state, bootinfo);
    assert(err_is_ok(err));
    // init_alloc_addr is not valid any more since we created caps for
    // memory after that.
    init_alloc_addr = 0;
}

static struct dcb *bee_spawn_module(struct bootinfo *bootinfo,
                                pstringtable_t strings,
                                bexec_t *module, 
                                const char *name,
                                int argc, const char** argv)
{
    errval_t err;

    printf("bee_spawn_module\n");

    lvaddr_t paramaddr;
    struct dcb *init_dcb = spawn_module(&spawn_state, name, argc, argv,
                                        (lpaddr_t)bootinfo, 0, alloc_phys,
                                        &paramaddr);

    // Create BMP table in task cnode
    err = caps_create_new(ObjType_BMPTable, 0, 0, 0,
			  caps_locate_slot(CNODE(spawn_state.taskcn),
                          TASKCN_SLOT_BMP_TABLE));
    assert(err_is_ok(err));

    /* Set fields in DCB */
    // Set Vspace (undefined concept since 1:1 SAS)
    init_dcb->vspace = 0xBAD23200;

    // create cap for strings area in first slot of modulecn
    if (strings != NULL) {
	assert(spawn_state.modulecn_slot == 0);
	assert(strings->slength < BASE_PAGE_SIZE);
        // create as DevFrame cap to avoid zeroing memory contents
	err = caps_create_new(ObjType_DevFrame, (uintptr_t)strings,
			      BASE_PAGE_BITS, BASE_PAGE_BITS,
			      caps_locate_slot(spawn_state.modulecn->cap.u.cnode.cnode,
					       spawn_state.modulecn_slot++));
	assert(err_is_ok(err));
    }

    /* Initialize dispatcher */
    struct dispatcher_shared_generic *init_disp =
        get_dispatcher_shared_generic(init_dcb->disp);
    struct dispatcher_shared_beehive *init_disp_beehive =
        get_dispatcher_shared_beehive(init_dcb->disp);
    init_disp->disabled = true;
    strncpy(init_disp->name, argv[0], DISP_NAME_LEN);

    /* Set up args in appropriate regs */
    registers_set_param(&init_disp_beehive->enabled_save_area, paramaddr);

    /* tell init the vspace addr of its dispatcher (1:1 V:P) */
    init_disp->udisp = (lvaddr_t)init_dcb->disp;

    init_disp_beehive->disabled_save_area.named.pc = module->btorg;
    init_disp_beehive->disabled_save_area.named.p1 = init_dcb->disp;

    return init_dcb;
}

/**
 * Name of multiboot module containing program for init domains.
 */
#define BSP_INIT_MODULE_NAME     "/beehive/sbin/init"
#define BSP_MONITOR_MODULE_NAME  "/beehive/sbin/monitor"

static bexec_t *find_module(char *name, pstringtable_t strings) 
{
    bexec_t *module = NULL;

    printf("name=%s\n", name);
    for (int i=0; i<MovedImageCount; i++) {
	char *str = (char*)strings + strings->soffset[i+1]; // +1 since kernel not moved
	printf("str=%s\n", str);
	if (strcmp(name, str))
	    continue;
	module = MovedImages[i];
	break;
    }
    return module;
}

void kernel_app_startup(struct beehive_core_data *bcd)
{
    my_core_id = arch_get_core_id();

    bexec_t *monitor = bcd->monitor_module;
    if (monitor == NULL) {
        panic("Could not find monitor module!");
    }

    // Setup args for monitor here
    // First arg is coreid of monitor which booted us
    // Second is the address of a page used for URPC (XXX replace with BMP?)
    char corearg[8];
    snprintf(corearg, sizeof(corearg), "%d", bcd->src_core_id);

    char chanarg[16];
    snprintf(chanarg, sizeof(chanarg), "chanid=%d", bcd->chanid);

    const char *argv[3] = { "monitor", corearg, chanarg };

    struct dcb *mon_dcb = bee_spawn_module(bcd->bootinfo, 
                                       bcd->strings,
                                       bcd->monitor_module, 
                                       BSP_MONITOR_MODULE_NAME,
                                       3, argv);

    printf("alloc_phys_pages=%d\n", alloc_phys_pages);

    make_runnable(mon_dcb);
    
    printf("Going to dispatch monitor...\n");
    dispatch(mon_dcb);
    
    panic("MONITOR RETURNED!\n");
}

void kernel_bsp_startup(void* initaddr, pstringtable_t strings)
{
    my_core_id = arch_get_core_id();

    bexec_t *init = find_module(BSP_INIT_MODULE_NAME, strings);
    if (init == NULL) {
        panic("Could not find init module!");
    }
    /* Fill bootinfo struct to tell init where everything is */
    // TODO XXX Need to integrate this with MoveImages
    struct bootinfo *bootinfo = (void*)alloc_phys(BOOTINFO_SIZE);
    memset(bootinfo, 0, BOOTINFO_SIZE);
    printf("BOOTINFO at %p\n", bootinfo);
    bootinfo->regions_length = 0;

    /* Fill in modules from MovedImage data */
    printf("create_regions_from_moved_images...\n");
    create_regions_from_moved_images(bootinfo, strings);

    char bootinfoarg[16];
    snprintf(bootinfoarg, sizeof(bootinfoarg), "%d", (lvaddr_t)bootinfo);

    const char *argv[2] = { "init", bootinfoarg };
    struct dcb *init_dcb = bee_spawn_module(bootinfo, strings, 
                                        init, BSP_INIT_MODULE_NAME,
                                        2, argv);

    /* Create caps for init to use */
    printf("create_module_caps...\n");
    create_module_caps(bootinfo, strings);

    printf("create_phys_caps...\n");
    create_phys_caps(bootinfo);

    printf("alloc_phys_pages=%d\n", alloc_phys_pages);

    make_runnable(init_dcb);
    
    printf("Going to dispatch init...\n");
    dispatch(init_dcb);
    
    panic("INIT RETURNED!\n");

}
