/**
 * \file
 * \brief Code for handling booting additional cores
 */

/*
 * Copyright (c) 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "monitor.h"
#include <inttypes.h>
#include <elf/elf.h>
#include <target/x86/barrelfish_kpi/coredata_target.h>
#include <barrelfish_kpi/shared_mem_arch.h>
#include <notify_ipi.h>

#ifdef RCK_EMU
struct monitor_allocate_state {
    void          *vbase;
    genvaddr_t     elfbase;
};

static errval_t monitor_elfload_allocate(void *state, genvaddr_t base,
                                         size_t size, uint32_t flags,
                                         void **retbase)
{
    struct monitor_allocate_state *s = state;

    *retbase = s->vbase + base - s->elfbase;
    return SYS_ERR_OK;
}

struct xcore_bind_handler {
    coreid_t                    coreid;
    enum cpu_type               cputype;
    struct monitor_binding      *binding;
};
#endif

errval_t spawn_xcore_monitor(coreid_t id, int hwid, enum cpu_type cpu_type,
                             const char *cmdline /* XXX: currently ignored */)
{
#ifdef RCK_EMU
    const char *monitorname = NULL, *cpuname = NULL;
#endif
    genpaddr_t arch_page_size;
    errval_t err;

    switch(cpu_type) {
    case CPU_SCC:
        arch_page_size = X86_32_BASE_PAGE_SIZE;
#ifdef RCK_EMU
        monitorname = "scc/sbin/monitor";
        cpuname = "scc/sbin/cpu";
#endif
        break;

    default:
        return SPAWN_ERR_UNKNOWN_TARGET_ARCH;
    }

    // Setup new inter-monitor connection
    struct intermon_ump_ipi_binding *binding = malloc(sizeof(struct intermon_ump_ipi_binding));
    assert(binding != NULL);

    // compute size of frame needed and allocate it
    struct capref frame;
    size_t framesize = MON_URPC_CHANNEL_LEN * 2;
#ifndef RCK_EMU
    ram_set_affinity(SHARED_MEM_MIN + (PERCORE_MEM_SIZE * my_core_id),
                     SHARED_MEM_MIN + (PERCORE_MEM_SIZE * (my_core_id + 1)));
#endif
    err = frame_alloc(&frame, framesize, &framesize);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_FRAME_ALLOC);
    }
#ifndef RCK_EMU
    ram_set_affinity(0, 0);
#endif

    // Mark it remote
    bool has_descendants;
    err = monitor_cap_remote(frame, true, &has_descendants);
    if (err_is_fail(err)) {
        return err;
    }

    // map it in
    void *buf;
    err = vspace_map_one_frame_attr(&buf, framesize, frame,
                                    VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);
    if (err_is_fail(err)) {
        cap_destroy(frame);
        return err_push(err, LIB_ERR_VSPACE_MAP);
    }

    memset(buf, 0, framesize);

    /* printf("vaddr of frame: %p\n", buf); */

    // Bootee's notify channel ID is always 0
    struct capref notify_cap;
    err = notification_create_cap(0, hwid, &notify_cap);
    assert(err == SYS_ERR_OK);

    // Allocate my own notification caps
    struct capref ep, my_notify_cap;
    struct lmp_endpoint *iep;
    int chanid;
    err = endpoint_create(LMP_RECV_LENGTH, &ep, &iep);
    assert(err_is_ok(err));
    err = notification_allocate(ep, &chanid);
    assert(err == SYS_ERR_OK);
    err = notification_create_cap(chanid, my_core_id, &my_notify_cap);
    assert(err == SYS_ERR_OK);

    // init our end of the binding and channel
    err = intermon_ump_ipi_init(binding, get_default_waitset(),
                                buf, MON_URPC_CHANNEL_LEN,
                                buf + MON_URPC_CHANNEL_LEN,
                                MON_URPC_CHANNEL_LEN, notify_cap,
                                my_notify_cap, ep, iep);
    if (err_is_fail(err)) {
        cap_destroy(frame);
        return err_push(err, LIB_ERR_UMP_CHAN_BIND);
    }

    // Identify UMP frame for tracing
    struct frame_identity umpid;
    err = invoke_frame_identify(frame, &umpid);
    assert(err_is_ok(err));
    binding->ump_state.chan.recvid = (uintptr_t)umpid.base;
    binding->ump_state.chan.sendid =
        (uintptr_t)(umpid.base + MON_URPC_CHANNEL_LEN);

    err = intermon_init(&binding->b, id);
    assert(err_is_ok(err));

    err = intern_set(&binding->b, false, id);
    assert(err_is_ok(err));

#ifdef RCK_EMU
    /* Look up modules */
    struct mem_region *cpu_region = multiboot_find_module(bi, cpuname);
    if (cpu_region == NULL) {
        return SPAWN_ERR_FIND_MODULE;
    }
    size_t cpu_binary_size;
    lvaddr_t cpu_binary;
    genpaddr_t cpu_binary_phys;
    err = spawn_map_module(cpu_region, &cpu_binary_size, &cpu_binary,
                           &cpu_binary_phys);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_MAP_MODULE);
    }
    struct Elf64_Ehdr *cpu_head = (struct Elf64_Ehdr *)cpu_binary;

    struct mem_region *monitor_region = multiboot_find_module(bi, monitorname);
    if (monitor_region == NULL) {
        return SPAWN_ERR_FIND_MODULE;
    }
    size_t monitor_binary_size;
    lvaddr_t monitor_binary;
    genpaddr_t monitor_binary_phys;
    err = spawn_map_module(monitor_region, &monitor_binary_size, &monitor_binary,
                           &monitor_binary_phys);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_MAP_MODULE);
    }

    /* Memory for cpu */
    size_t cpu_memory = elf_virtual_size(cpu_binary) + arch_page_size;
#else
    size_t cpu_memory = X86_32_BASE_PAGE_SIZE;
#endif

    struct capref cpu_memory_cap;
    err = frame_alloc(&cpu_memory_cap, cpu_memory, &cpu_memory);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_FRAME_ALLOC);
    }
    // Mark memory as remote
    err = monitor_cap_remote(cpu_memory_cap, true, &has_descendants);
    if (err_is_fail(err)) {
        return err;
    }
    void *cpu_buf_memory;
    err = vspace_map_one_frame(&cpu_buf_memory, cpu_memory, cpu_memory_cap, NULL, NULL);
    if(err_is_fail(err)) {
        return err_push(err, LIB_ERR_VSPACE_MAP);
    }

#ifdef RCK_EMU
    /* Chunk of memory to load monitor on the app core */
    struct capref spawn_memory_cap;
    err = frame_alloc(&spawn_memory_cap, X86_CORE_DATA_PAGES * arch_page_size, NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_FRAME_ALLOC);
    }
    // Mark memory as remote
    err = monitor_cap_remote(spawn_memory_cap, true, &has_descendants);
    if (err_is_fail(err)) {
        return err;
    }
    struct frame_identity spawn_memory_identity = { .base = 0, .bits = 0 };
    err = invoke_frame_identify(spawn_memory_cap, &spawn_memory_identity);
    assert(err_is_ok(err));

    /* Load cpu */
    struct monitor_allocate_state state;
    state.vbase = cpu_buf_memory + arch_page_size;
    state.elfbase = elf_virtual_base(cpu_binary);
    genvaddr_t cpu_entry;
    err = elf_load(EM_386, monitor_elfload_allocate, &state, cpu_binary,
                   cpu_binary_size, &cpu_entry);
    if (err_is_fail(err)) {
        return err;
    }

    // Relocate cpu to new physical base address
    struct frame_identity id = { .base = 0, .bits = 0 };
    err = invoke_frame_identify(cpu_memory_cap, &id);
    if(err_is_fail(err)) {
        return err_push(err, LIB_ERR_FRAME_IDENTIFY);
    }
    switch(cpu_head->e_machine) {
    case EM_386: {
        struct Elf32_Ehdr *head32 = (struct Elf32_Ehdr *)cpu_binary;

        struct Elf32_Shdr *rel, *symtab, *symhead =
            (struct Elf32_Shdr *)(cpu_binary + (uintptr_t)head32->e_shoff);

        rel = elf32_find_section_header_type(symhead, head32->e_shnum, SHT_REL);
        assert(rel != NULL);
        symtab = elf32_find_section_header_type(symhead, head32->e_shnum,
                                                SHT_DYNSYM);
        assert(symtab != NULL);
        elf32_relocate(id.base + arch_page_size, state.elfbase,
                       (struct Elf32_Rel *)(uintptr_t)(cpu_binary + rel->sh_offset),
                       rel->sh_size,
                       (struct Elf32_Sym *)(uintptr_t)(cpu_binary + symtab->sh_offset),
                       symtab->sh_size,
                       state.elfbase, state.vbase);

        // XXX: QEMU hack to be able to boot there
        cpu_entry += 0x5b;
        break;
    }
    default:
        return SPAWN_ERR_UNKNOWN_TARGET_ARCH;
    }
    genvaddr_t cpu_reloc_entry = cpu_entry - state.elfbase
        + id.base + arch_page_size;
#endif

    /* Look up information on the urpc_frame cap */
    struct frame_identity urpc_frame_id  = { .base = 0, .bits = 0 };
    err = invoke_frame_identify(frame, &urpc_frame_id);
    assert(err_is_ok(err));

#ifdef RCK_EMU
    /* Compute entry point in the foreign address space */
    // XXX: Confusion address translation about l/gen/addr
    forvaddr_t foreign_cpu_reloc_entry = (forvaddr_t)cpu_reloc_entry;

    /* Setup the core_data struct in the new kernel */
    struct x86_core_data *core_data = (struct x86_core_data*)cpu_buf_memory;
    switch(cpu_head->e_machine) {
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
    core_data->multiboot_flags = MULTIBOOT_INFO_FLAG_HAS_ELF_SYMS;
    core_data->urpc_frame_base = urpc_frame_id.base;
    core_data->urpc_frame_bits = urpc_frame_id.bits;
    core_data->monitor_binary   = monitor_binary_phys;
    core_data->monitor_binary_size = monitor_binary_size;
    core_data->memory_base_start = spawn_memory_identity.base;
    core_data->memory_bits       = spawn_memory_identity.bits;
#else
    struct x86_core_data *core_data = (struct x86_core_data*)cpu_buf_memory;
    core_data->urpc_frame_base = urpc_frame_id.base;
    /* printf("URPC frame base of booter: 0x%" PRIxGENPADDR "\n", core_data->urpc_frame_base); */
    core_data->urpc_frame_bits = urpc_frame_id.bits;
#endif
    core_data->src_core_id       = my_core_id;
    core_data->dst_core_id       = id;
    core_data->chan_id           = chanid;

    /* Invoke kernel capability to boot new core */
#ifdef RCK_EMU
    err = invoke_monitor_spawn_core(hwid, cpu_type, foreign_cpu_reloc_entry);
#else
    // XXX: Passing vaddr of core_data + arch_page size here! Dirty hack!
    err = invoke_monitor_spawn_core(hwid, cpu_type,
                                    ((forvaddr_t)(uintptr_t)core_data) + arch_page_size);
#endif
    if (err_is_fail(err)) {
        return err_push(err, MON_ERR_SPAWN_CORE);
    }

#ifdef RCK_EMU
    /* Clean up */ // XXX: Should not delete the remote cap
    err = cap_destroy(cpu_memory_cap);
    assert(err_is_ok(err));
    err = cap_destroy(spawn_memory_cap);
    assert(err_is_ok(err));
#endif

    return SYS_ERR_OK;
}

errval_t boot_arch_app_core(int argc, char *argv[])
{
    errval_t err;
    int argn = 1;

#ifndef RCK_EMU
    assert(argc == 5);

    // First argument contains the bootinfo location
    bi = (struct bootinfo*)strtol(argv[argn++], NULL, 10);
#else
    assert(argc == 4);
#endif

    // core_id of the core that booted this core
    coreid_t core_id = strtol(argv[argn++], NULL, 10);

    // other monitor's channel id
    assert(strncmp("chanid", argv[argn], strlen("chanid")) == 0);
    int chan_id = strtol(strchr(argv[argn++], '=') + 1, NULL, 10);

    // other monitor's frame base
    assert(strncmp("frame", argv[argn], strlen("frame")) == 0);
    uint64_t chanbase = strtoul(strchr(argv[argn++], '=') + 1, NULL, 10);

#ifndef RCK_EMU
    err = monitor_client_setup_mem_serv();
    assert(err_is_ok(err));

    /* Wait for mem_serv to advertise its iref to us */
    while (mem_serv_iref == 0) {
        messages_wait_and_handle_next();
    }

    /* Can now connect to and use mem_serv */
    err = ram_alloc_set(NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_RAM_ALLOC_SET);
    }
#endif

    printf("frame base at 0x%llx -- 0x%llx\n", chanbase, chanbase + BASE_PAGE_SIZE);

#ifndef RCK_EMU
    assert(MON_URPC_CHANNEL_LEN * 2 < BASE_PAGE_SIZE);
    ram_set_affinity(chanbase, chanbase + BASE_PAGE_SIZE);
    struct capref frame;
    err = frame_alloc(&frame, MON_URPC_CHANNEL_LEN * 2, NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "frame_alloc failed");
        return err; // FIXME: cleanup
    }
    ram_set_affinity(0, 0);     // Reset affinity
#else
    struct capref frame = {
        .cnode = cnode_task,
        .slot  = TASKCN_SLOT_MON_URPC,
    };
#endif

    struct frame_identity frameid = { .base = 0, .bits = 0 };
    err = invoke_frame_identify(frame, &frameid);
    assert(err == SYS_ERR_OK);

    printf("URPC physical frame at 0x%llx\n", frameid.base);

    // Map frame locally
    // XXX: Remove this and use URPC_BASE when spawning from other core
    void *buf;
    err = vspace_map_one_frame_attr(&buf, MON_URPC_CHANNEL_LEN * 2, frame,
                                    VREGION_FLAGS_READ_WRITE_NOCACHE, NULL,
                                    NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VSPACE_MAP);
    }

    // Create notify cap to other monitor
    struct capref notify_cap;
    err = notification_create_cap(chan_id, core_id, &notify_cap);
    assert(err == SYS_ERR_OK);

    // Allocate my own notification caps
    struct capref ep, my_notify_cap;
    struct lmp_endpoint *iep;
    int chanid;
    err = endpoint_create(LMP_RECV_LENGTH, &ep, &iep);
    assert(err_is_ok(err));
    err = notification_allocate(ep, &chanid);
    assert(err == SYS_ERR_OK);
    assert(chanid == 0);        // Make sure it's channel 0
    err = notification_create_cap(chanid, my_core_id, &my_notify_cap);
    assert(err == SYS_ERR_OK);

    // setup our side of the binding
    struct intermon_ump_ipi_binding *rckb;
    rckb = malloc(sizeof(struct intermon_ump_ipi_binding));
    assert(rckb != NULL);
    err = intermon_ump_ipi_init(rckb, get_default_waitset(),
                                buf + MON_URPC_CHANNEL_LEN,
                                MON_URPC_CHANNEL_LEN,
                                buf, MON_URPC_CHANNEL_LEN, notify_cap,
                                my_notify_cap, ep, iep);
    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_UMP_CHAN_BIND);
        return err;
    }

    // Identify UMP frame for tracing
    rckb->ump_state.chan.sendid = (uintptr_t)frameid.base;
    rckb->ump_state.chan.recvid =
        (uintptr_t)(frameid.base + MON_URPC_CHANNEL_LEN);

    // connect it to our request handlers
    intermon_init(&rckb->b, core_id);

    err = intern_set(&rckb->b, true, core_id);
    assert(err_is_ok(err));

    /* Request memserv and nameserv iref */
#ifdef RCK_EMU
    err = request_mem_serv_iref(&rckb->b);
    assert(err_is_ok(err));
#endif
    err = request_name_serv_iref(&rckb->b);
    assert(err_is_ok(err));

#ifdef RCK_EMU
    /* initialize self ram alloc */
    err = mon_ram_alloc_init(core_id);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_RAM_ALLOC_SET);
    }
#endif

    return SYS_ERR_OK;
}
