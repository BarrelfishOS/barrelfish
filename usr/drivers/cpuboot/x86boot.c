/**
 * \file
 * \brief Boot driver for x86-64 CPUs
 */
/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/spawn_client.h>
#include <elf/elf.h>
#include <target/x86/barrelfish_kpi/coredata_target.h>
#include <target/x86_32/barrelfish_kpi/paging_target.h>
#include <target/x86_64/barrelfish_kpi/paging_target.h>

#include <if/monitor_defs.h>
#include <if/monitor_blocking_rpcclient_defs.h>
#include <spawndomain/spawndomain.h>
#include <if/intermon_defs.h>
#include <acpi_client/acpi_client.h>
#include <vfs/vfs.h>

#include <barrelfish/invocations_arch.h>

// From kernels start_aps.c
#include <kernel.h>
#include <arch/x86/apic.h>
#include <arch/x86/start_aps.h>
#include <x86.h>
#include <arch/x86/cmos.h>
#include <init.h>
#include <dev/xapic_dev.h>
#include <target/x86_64/offsets_target.h>

#define MON_URPC_CHANNEL_LEN  (32 * UMP_MSG_BYTES)

#pragma GCC diagnostic ignored "-Wunused-function"

struct elf_allocate_state {
    void *vbase;
    genvaddr_t elfbase;
};

static bool done = false;
static bool do_reboot = false;

/**
 * Start_ap and start_ap_end mark the start end the
 * end point of the assembler startup code to be copied
 */
extern uint64_t x86_64_start_ap;
extern uint64_t x86_64_start_ap_end;
extern uint64_t x86_64_init_ap_absolute_entry;
extern uint64_t x86_64_init_ap_wait;
extern uint64_t x86_64_init_ap_lock;
extern uint64_t x86_64_start;
extern uint64_t x86_64_init_ap_global;

static coreid_t my_arch_id;
static struct capref kernel_cap;

static errval_t elfload_allocate(void *state, genvaddr_t base,
                                         size_t size, uint32_t flags,
                                         void **retbase)
{
    struct elf_allocate_state *s = state;

    *retbase = (char *)s->vbase + base - s->elfbase;
    return SYS_ERR_OK;
}

/**
 * \brief Spawn a new core.
 *
 * \param core_id    APIC ID of the core to try booting
 * \param cpu_type   Type of core to boot
 * \param entry      Kernel entry point in physical memory
 */
static inline errval_t
invoke_spawn_core(coreid_t core_id, enum cpu_type cpu_type,
                  forvaddr_t entry)
{

    /*struct capref task_cap_kernel;
    task_cap_kernel.cnode = cnode_task;
    task_cap_kernel.slot = TASKCN_SLOT_KERNELCAP;*/

    return cap_invoke4(kernel_cap, KernelCmd_Spawn_core, core_id, cpu_type,
                       entry).error;
}

static inline errval_t invoke_send_init_ipi(coreid_t core_id)
{
    return cap_invoke2(kernel_cap, KernelCmd_Init_IPI_Send,
                       core_id).error;
}

static inline errval_t invoke_send_start_ipi(coreid_t core_id, forvaddr_t entry)
{
    return cap_invoke3(kernel_cap, KernelCmd_Start_IPI_Send,
                       core_id, entry).error;
}

static inline errval_t invoke_get_global_paddr(genpaddr_t* global)
{
    struct sysret sr = cap_invoke1(kernel_cap, KernelCmd_GetGlobalPhys);
    if (err_is_ok(sr.error)) {
        *global = sr.value;
    }

    return sr.error;
}

static inline errval_t invoke_start_core(void)
{
    struct sysret sr = cap_invoke1(kernel_cap, KernelCmd_StartCore);
    return sr.error;
}

/**
 * \brief Boot a app core of x86_64 type
 *
 * The processors are started by a sequency of INIT and STARTUP IPIs
 * which are sent by this function.
 * CMOS writes to the shutdown status byte are used to execute
 * different memory locations.
 *
 * \param core_id   APIC ID of the core to try booting
 * \param entry     Entry address for new kernel in the destination
 *                  architecture's lvaddr_t given in genvaddr_t
 *
 * \returns Zero on successful boot, non-zero (error code) on failure
 */
int start_aps_x86_64_start(uint8_t core_id, genvaddr_t entry)
{
    printf("%s:%d: start_aps_x86_64_start\n", __FILE__, __LINE__);

    // Copy the startup code to the real-mode address
    uint8_t *real_src = (uint8_t *) &x86_64_start_ap;
    uint8_t *real_end = (uint8_t *) &x86_64_start_ap_end;

    struct capref bootcap;
    struct acpi_rpc_client* acl = get_acpi_rpc_client();
    errval_t error_code;
    errval_t err = acl->vtbl.mm_realloc_range_proxy(acl, 16, 0x0,
                                                    &bootcap, &error_code);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "mm_alloc_range_proxy failed.");
    }
    if (err_is_fail(error_code)) {
        USER_PANIC_ERR(error_code, "mm_alloc_range_proxy return failed.");
    }

    void* real_base;
    err = vspace_map_one_frame(&real_base, 1<<16, bootcap, NULL, NULL);
    uint8_t* real_dest = (uint8_t*)real_base + X86_64_REAL_MODE_LINEAR_OFFSET;

/*
    printf("%s:%d: X86_64_REAL_MODE_LINEAR_OFFSET=%p\n", __FILE__, __LINE__, (void*)X86_64_REAL_MODE_LINEAR_OFFSET);
    printf("%s:%d: real_dest=%p\n", __FILE__, __LINE__, real_dest);
    printf("%s:%d: real_src=%p\n", __FILE__, __LINE__, real_src);
    printf("%s:%d: real_end=%p\n", __FILE__, __LINE__, real_end);
    printf("%s:%d: size=%lu\n", __FILE__, __LINE__, (uint64_t)real_end-(uint64_t)real_src);
*/

    memcpy(real_dest, real_src, real_end - real_src);

    /* Pointer to the entry point called from init_ap.S */
    volatile uint64_t *absolute_entry_ptr = (volatile uint64_t *)
                                            ((
                                             (lpaddr_t) &x86_64_init_ap_absolute_entry -
                                             (lpaddr_t) &x86_64_start_ap
                                            )
                                            +
                                            real_dest);
    //copy the address of the function start (in boot.S) to the long-mode
    //assembler code to be able to perform an absolute jump
    *absolute_entry_ptr = entry;

    // pointer to the shared global variable amongst all kernels
    volatile uint64_t *ap_global = (volatile uint64_t *)
                                   ((
                                    (lpaddr_t) &x86_64_init_ap_global -
                                    (lpaddr_t) &x86_64_start_ap
                                   )
                                   + real_dest);


    genpaddr_t global;
    err = invoke_get_global_paddr(&global);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "invoke spawn core");
        return err_push(err, MON_ERR_SPAWN_CORE);
    }
    *ap_global = (uint64_t)(genpaddr_t)global;

    // pointer to the pseudo-lock used to detect boot up of new core
    volatile uint32_t *ap_wait = (volatile uint32_t *)
                                         ((lpaddr_t) &x86_64_init_ap_wait -
                                         ((lpaddr_t) &x86_64_start_ap) +
                                         real_dest);

    // Pointer to the lock variable in the realmode code
    volatile uint8_t *ap_lock = (volatile uint8_t *)
                                        ((lpaddr_t) &x86_64_init_ap_lock -
                                        ((lpaddr_t) &x86_64_start_ap) +
                                        real_dest);

    *ap_wait = AP_STARTING_UP;


    err = invoke_send_init_ipi(1);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "invoke send init ipi");
        return err;
    }

    err = invoke_send_start_ipi(1, entry);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "invoke sipi");
        return err;
    }

    /*err = invoke_send_start_ipi(1, entry);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "invoke sipi");
        return err;
    }*/


    printf("%s:%d: \n", __FILE__, __LINE__);
    //give the new core a bit time to start-up and set the lock
    for (uint64_t i = 0; i < STARTUP_TIMEOUT; i++) {
        if (*ap_lock != 0) {
            break;
        }
    }
    printf("%s:%d: \n", __FILE__, __LINE__);

    // If the lock is set, the core has been started, otherwise assume, that
    // a core with this APIC ID doesn't exist.
    if (*ap_lock != 0) {
        while (*ap_wait != AP_STARTED);
        trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_CORE_START_REQUEST_ACK, core_id);
        *ap_lock = 0;
        return 0;
    }

    assert(!"badness");
    return -1;
}

static errval_t get_architecture_config(enum cpu_type type,
                                        genpaddr_t* arch_page_size,
                                        const char** monitor_binary,
                                        const char** cpu_binary)
{
    switch (type) {
    case CPU_X86_64:
        *arch_page_size = X86_64_BASE_PAGE_SIZE;
        *monitor_binary = "/x86_64/sbin/monitor";
        *cpu_binary = "/x86_64/sbin/cpu";
        break;

    case CPU_X86_32:
        *arch_page_size = X86_32_BASE_PAGE_SIZE;
        *monitor_binary = "x86_32/sbin/monitor";
        *cpu_binary = "x86_32/sbin/cpu";
        break;

    default:
        return SPAWN_ERR_UNKNOWN_TARGET_ARCH;
    }

    return SYS_ERR_OK;
}

static inline errval_t
invoke_monitor_cap_remote(capaddr_t cap, int bits, bool is_remote,
                          bool * has_descendents)
{
    struct sysret r = cap_invoke4(kernel_cap, KernelCmd_Remote_cap, cap, bits,
                                  is_remote);
    if (err_is_ok(r.error)) {
        *has_descendents = r.value;
    }
    return r.error;
}

static errval_t cap_mark_remote(struct capref cap)
{
    // TODO(gz): Re-enable, this does not much right now but
    // will be interesting with Mark nevills libmdb

    bool has_descendants;

    uint8_t vbits = get_cap_valid_bits(cap);
    capaddr_t caddr = get_cap_addr(cap) >> (CPTR_BITS - vbits);
    return invoke_monitor_cap_remote(caddr, vbits, true, &has_descendants);
}

/**
 * \brief Same as frame_alloc but also identify the capability.
 */
static errval_t frame_alloc_identify(struct capref *dest, size_t bytes,
                                     size_t *retbytes, struct frame_identity* id)
{
    errval_t err = frame_alloc(dest, bytes, retbytes);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "frame_alloc failed.");
        return err;
    }

    if (id != NULL) {
        err = invoke_frame_identify(*dest, id);
    }

    return err;
}

static errval_t lookup_module(const char* module_name, lvaddr_t* binary_virt,
                              genpaddr_t* binary_phys, size_t* binary_size)
{
    /*struct mem_region *module_region = multiboot_find_module(bi, module_name);
    if (module_region == NULL) {
        USER_PANIC("multiboot module not found?");
        return SPAWN_ERR_FIND_MODULE;
    }*/

    vfs_handle_t handle;
    struct vfs_fileinfo info;

    errval_t err = vfs_open(module_name, &handle);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "vfs_open could not open module?");
        return err;
    }

    err = vfs_stat(handle, &info);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "vfs_stat for binary failed.");
        return err;
    }
    *binary_size = info.size;

    struct capref binary_image_cap;
    struct frame_identity id;
    err = frame_alloc_identify(&binary_image_cap, info.size, NULL, &id);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Could not allocate space for binary");
        return err;
    }
    *binary_phys = id.base;
    printf("%s:%d: id.base=0x%"PRIxGENPADDR"\n", __FILE__, __LINE__, id.base);

    err = vspace_map_one_frame((void**)binary_virt, info.size, binary_image_cap,
                               NULL, NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Could not map frame");
        return err;
    }

    size_t bytes_read = 0;
    err = vfs_read(handle, (void*)*binary_virt, info.size, &bytes_read);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Can not read binary from vfs");
        return err;
    }
    assert(bytes_read == info.size); // TODO(gz): If this fails, need to loop vfs_read


    return SYS_ERR_OK;
}

/**
 * Allocates memory for kernel binary.
 *
 * For x86, the app kernel can only be loaded in the first 4GB
 * of memory. Further, it must not overlap the integer
 * boundaries, i.e. 0-1, 1-2, 2-3, or 3-4.
 *
 * Probably because we identity map this region during boot-phase
 * so we can't access anything higher. Not sure about overlap tough.
 */
static errval_t allocate_kernel_memory(lvaddr_t cpu_binary, genpaddr_t page_size,
                                       struct capref* cpu_memory_cap, size_t* cpu_memory,
                                       struct frame_identity* id)
{
    errval_t err;
#ifdef __scc__
    *cpu_memory = X86_32_BASE_PAGE_SIZE;
    err = frame_alloc_identify(cpu_memory_cap, *cpu_memory, cpu_memory, id);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_FRAME_ALLOC);
    }
#else
    *cpu_memory = elf_virtual_size(cpu_binary) + page_size;

    uint64_t old_minbase;
    uint64_t old_maxlimit;
    ram_get_affinity(&old_minbase, &old_maxlimit);
    printf("%s:%d: \n", __FILE__, __LINE__);
    for (uint64_t minbase = 0, maxlimit = (uint64_t)1 << 30;
            minbase < (uint64_t)4 << 30;
            minbase += (uint64_t)1 << 30, maxlimit += (uint64_t)1 << 30) {

        ram_set_affinity(minbase, maxlimit);
        err = frame_alloc_identify(cpu_memory_cap, *cpu_memory, cpu_memory, id);
        if (err_is_fail(err)) {
            continue;
        } else {
            goto done;
        }
    }

    USER_PANIC("No memory in the first 4GB, cannot continue booting cores");

done:
    ram_set_affinity(old_minbase, old_maxlimit);
#endif

    return SYS_ERR_OK;
}

static errval_t relocate_cpu_binary(lvaddr_t cpu_binary,
                                    struct Elf64_Ehdr *cpu_head,
                                    struct elf_allocate_state state,
                                    struct frame_identity frameid,
                                    genpaddr_t arch_page_size)
{
    switch (cpu_head->e_machine) {
    case EM_X86_64: {
        struct Elf64_Shdr *rela, *symtab, *symhead =
            (struct Elf64_Shdr *)(cpu_binary + (uintptr_t)cpu_head->e_shoff);

        assert(cpu_head->e_shoff != 0);
        rela = elf64_find_section_header_type(symhead, cpu_head->e_shnum, SHT_RELA);
        assert(rela != NULL);
        symtab = elf64_find_section_header_type(symhead, cpu_head->e_shnum, SHT_DYNSYM);
        assert(symtab != NULL);
        elf64_relocate(frameid.base + arch_page_size, state.elfbase,
                       (struct Elf64_Rela *)(uintptr_t)(cpu_binary + rela->sh_offset),
                       rela->sh_size,
                       (struct Elf64_Sym *)(uintptr_t)(cpu_binary + symtab->sh_offset),
                       symtab->sh_size,
                       state.elfbase, state.vbase);
        break;
    }
    case EM_386: {
        struct Elf32_Ehdr *head32 = (struct Elf32_Ehdr *)cpu_binary;

        struct Elf32_Shdr *rel, *symtab, *symhead =
            (struct Elf32_Shdr *)(cpu_binary + (uintptr_t)head32->e_shoff);

        rel = elf32_find_section_header_type(symhead, head32->e_shnum, SHT_REL);
        assert(rel != NULL);
        symtab = elf32_find_section_header_type(symhead, head32->e_shnum,
                                                SHT_DYNSYM);
        assert(symtab != NULL);
        elf32_relocate(frameid.base + arch_page_size, state.elfbase,
                       (struct Elf32_Rel *)(uintptr_t)(cpu_binary + rel->sh_offset),
                       rel->sh_size,
                       (struct Elf32_Sym *)(uintptr_t)(cpu_binary + symtab->sh_offset),
                       symtab->sh_size,
                       state.elfbase, state.vbase);
        break;
    }
    default:
        return SPAWN_ERR_UNKNOWN_TARGET_ARCH;
    }

    return SYS_ERR_OK;
}

static errval_t spawn_xcore_monitor(coreid_t coreid, int hwid,
                                    enum cpu_type cpu_type,
                                    const char *cmdline,
                                    struct intermon_binding **ret_binding,
                                    struct capref* frame)
{
    const char *monitorname = NULL, *cpuname = NULL;
    genpaddr_t arch_page_size;
    errval_t err;

    err = get_architecture_config(cpu_type, &arch_page_size,
                                  &monitorname, &cpuname);
    assert(err_is_ok(err));

    // compute size of frame needed and allocate it
    struct frame_identity urpc_frame_id = {.base = 0, .bits = 0};
    size_t framesize;
    err = frame_alloc_identify(frame, MON_URPC_SIZE, &framesize, &urpc_frame_id);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_FRAME_ALLOC);
    }
    printf("%s:%s:%d: urpc_frame_id.base=%"PRIxGENPADDR"\n",
           __FILE__, __FUNCTION__, __LINE__, urpc_frame_id.base);
    printf("%s:%s:%d: urpc_frame_id.size=%d\n",
           __FILE__, __FUNCTION__, __LINE__, urpc_frame_id.bits);

    err = cap_mark_remote(*frame);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Can not mark cap remote.");
        return err;
    }

    static size_t cpu_binary_size;
    static lvaddr_t cpu_binary = 0;
    static genpaddr_t cpu_binary_phys;
    static const char* cached_cpuname = NULL;
    if (cpu_binary == 0) {
        cached_cpuname = cpuname;
        // XXX: Caching these for now, until we have unmap
        err = lookup_module(cpuname, &cpu_binary, &cpu_binary_phys,
                            &cpu_binary_size);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Can not lookup module");
            return err;
        }
    }
    // Ensure caching actually works and we're
    // always loading same binary. If this starts to fail, get rid of caching.
    assert (strcmp(cached_cpuname, cpuname) == 0);

    static size_t monitor_binary_size;
    static lvaddr_t monitor_binary = 0;
    static genpaddr_t monitor_binary_phys;
    static const char* cached_monitorname = NULL;
    if (monitor_binary == 0) {
        cached_monitorname = monitorname;
        // XXX: Caching these for now, until we have unmap
        err = lookup_module(monitorname, &monitor_binary,
                            &monitor_binary_phys, &monitor_binary_size);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Can not lookup module");
            return err;
        }
    }
    // Again, ensure caching actually worked (see above)
    assert (strcmp(cached_monitorname, monitorname) == 0);

    struct capref cpu_memory_cap;
    struct frame_identity frameid;
    size_t cpu_memory;
    err = allocate_kernel_memory(cpu_binary, arch_page_size,
                                 &cpu_memory_cap, &cpu_memory, &frameid);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Can not allocate space for new app kernel.");
        return err;
    }

    err = cap_mark_remote(cpu_memory_cap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Can not mark cap remote.");
        return err;
    }

    void *cpu_buf_memory;
    err = vspace_map_one_frame(&cpu_buf_memory, cpu_memory, cpu_memory_cap,
                               NULL, NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VSPACE_MAP);
    }

    /* Chunk of memory to load monitor on the app core */
    struct capref spawn_memory_cap;
    struct frame_identity spawn_memory_identity;

    err = frame_alloc_identify(&spawn_memory_cap,
                               X86_CORE_DATA_PAGES * arch_page_size,
                               NULL, &spawn_memory_identity);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_FRAME_ALLOC);
    }

    err = cap_mark_remote(spawn_memory_cap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Can not mark cap remote.");
        return err;
    }

    /* Load cpu */
    struct elf_allocate_state state;
    state.vbase = (char *)cpu_buf_memory + arch_page_size;
    assert(sizeof(struct x86_core_data) <= arch_page_size);
    state.elfbase = elf_virtual_base(cpu_binary);

    struct Elf64_Ehdr *cpu_head = (struct Elf64_Ehdr *)cpu_binary;
    genvaddr_t cpu_entry;

    err = elf_load(cpu_head->e_machine, elfload_allocate, &state,
                   cpu_binary, cpu_binary_size, &cpu_entry);
    if (err_is_fail(err)) {
        return err;
    }

    err = relocate_cpu_binary(cpu_binary, cpu_head, state, frameid, arch_page_size);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Can not relocate new kernel.");
        return err;
    }

    genvaddr_t cpu_reloc_entry = cpu_entry - state.elfbase
                                 + frameid.base + arch_page_size;
    /* Compute entry point in the foreign address space */
    forvaddr_t foreign_cpu_reloc_entry = (forvaddr_t)cpu_reloc_entry;

    /* Setup the core_data struct in the new kernel */
    struct x86_core_data *core_data = (struct x86_core_data *)cpu_buf_memory;
    switch (cpu_head->e_machine) {
    case EM_X86_64:
        core_data->elf.size = sizeof(struct Elf64_Shdr);
        core_data->elf.addr = cpu_binary_phys + (uintptr_t)cpu_head->e_shoff;
        core_data->elf.num  = cpu_head->e_shnum;
        break;
    case EM_386:
        core_data->elf.size = sizeof(struct Elf32_Shdr);
        struct Elf32_Ehdr *head32 = (struct Elf32_Ehdr *)cpu_binary;
        core_data->elf.addr = cpu_binary_phys + (uintptr_t)head32->e_shoff;
        core_data->elf.num  = head32->e_shnum;
        break;
    default:
        return SPAWN_ERR_UNKNOWN_TARGET_ARCH;
    }
    core_data->module_start = cpu_binary_phys;
    core_data->module_end   = cpu_binary_phys + cpu_binary_size;
    core_data->urpc_frame_base = urpc_frame_id.base;
    core_data->urpc_frame_bits = urpc_frame_id.bits;
    core_data->monitor_binary   = monitor_binary_phys;
    core_data->monitor_binary_size = monitor_binary_size;
    core_data->memory_base_start = spawn_memory_identity.base;
    core_data->memory_bits       = spawn_memory_identity.bits;
    core_data->src_core_id       = disp_get_core_id();
    core_data->src_arch_id       = my_arch_id;
    core_data->dst_core_id       = coreid;
#ifdef CONFIG_FLOUNDER_BACKEND_UMP_IPI
    core_data->chan_id           = chanid;
#endif

    if (cmdline != NULL) {
        // copy as much of command line as will fit
        strncpy(core_data->kernel_cmdline, cmdline,
                sizeof(core_data->kernel_cmdline));
        // ensure termination
        core_data->kernel_cmdline[sizeof(core_data->kernel_cmdline) - 1] = '\0';
    }

    /* Invoke kernel capability to boot new core */
    start_aps_x86_64_start(hwid, foreign_cpu_reloc_entry);

    /* Clean up */
    // XXX: Should not delete the remote caps?
    err = cap_destroy(spawn_memory_cap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "cap_destroy failed");
    }
    err = vspace_unmap(cpu_buf_memory);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "vspace unmap CPU driver memory failed");
    }
    err = cap_destroy(cpu_memory_cap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "cap_destroy failed");
    }

    return SYS_ERR_OK;
}

static void boot_core_reply(struct monitor_binding *st, errval_t msgerr)
{
    if (err_is_fail(msgerr)) {
        USER_PANIC_ERR(msgerr, "msgerr in boot_core_reply, exiting\n");
    }
    printf("%s:%d: got boot_core_reply.\n", __FILE__, __LINE__);
    done = true;
}

static void power_down_response(struct monitor_binding *st, coreid_t target)
{
    printf("%s:%s:%d: Got power_down_response. target=%"PRIuCOREID"\n", __FILE__, __FUNCTION__, __LINE__, target);
    printf("%s:%s:%d: Is the core really down now?\n", __FILE__, __FUNCTION__, __LINE__);

    done = true;
}

int main(int argc, char** argv)
{
    errval_t err;
    if (argc < 4) {
        printf("%s:%s:%d: Not enough arguments\n", __FILE__, __FUNCTION__, __LINE__);
        return 1;
    }

    vfs_init();

    struct monitor_binding *st = get_monitor_binding();
    st->rx_vtbl.boot_core_reply = boot_core_reply;
    st->rx_vtbl.power_down_response = power_down_response;

    err = connect_to_acpi();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "connect to acpi failed.");
    }

    struct monitor_blocking_rpc_client *mc = get_monitor_blocking_rpc_client();
    err = mc->vtbl.get_arch_core_id(mc, (uintptr_t*)&my_arch_id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "get_arch_core_id failed.");
    }
    printf("%s:%d: my_arch_id is %"PRIuCOREID"\n", __FILE__, __LINE__, my_arch_id);

    err = mc->vtbl.get_kernel_cap(mc, &kernel_cap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "get_kernel_cap failed.");
    }

    for (size_t i = 0; i < argc; i++) {
        printf("%s:%d: argv[i]=%s\n", __FILE__, __LINE__, argv[i]);
    }

    coreid_t destination = (coreid_t) atoi(argv[3]);
    assert(destination < MAX_COREID);

    if(!strcmp(argv[3], "again")) {
        printf("%s:%s:%d: we're doing a reboot\n",
               __FILE__, __FUNCTION__, __LINE__);
        do_reboot = true;
    } else {
        printf("%s:%s:%d: we're not doing a reboot\n",
               __FILE__, __FUNCTION__, __LINE__);
    }

    //enum cpu_type type = (enum cpu_type) atoi(argv[4]);
    //assert(type < CPU_TYPE_NUM);

    if (!strcmp(argv[2], "up")) {
        struct intermon_binding *new_binding = NULL;
        struct capref frame;
        err = spawn_xcore_monitor(destination, destination, CPU_X86_64, "", &new_binding, &frame);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "spawn xcore monitor failed.");
        }

        struct monitor_binding *mb = get_monitor_binding();
        err = mb->tx_vtbl.boot_core_request(mb, NOP_CONT, destination, frame);
    }
    else if (!strcmp(argv[2], "down")) {
        printf("%s:%d: Power it down...\n", __FILE__, __LINE__);
        err = st->tx_vtbl.power_down(st, NOP_CONT, destination);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "power_down failed.");
        }
    }
    else if (!strcmp(argv[2], "resume")) {
        printf("%s:%s:%d: Resume...\n", __FILE__, __FUNCTION__, __LINE__);
        err = invoke_start_core();
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "resume core failed.");
        }
        done = true;
    }
    else {
        printf("%s:%s:%d: unknown cmd = %s\n",
               __FILE__, __FUNCTION__, __LINE__, argv[2]);
        done = true;
    }

    while(!done) {
        err = event_dispatch(get_default_waitset());
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "error in event_dispatch");
        }
    }

    printf("%s:%s:%d: We're done here...\n", __FILE__, __FUNCTION__, __LINE__);

    return 0;
}