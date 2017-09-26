/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "init.h"

/**
 * Initialize mem_serv while spawning it.
 */
errval_t initialize_mem_serv(struct spawninfo *si)
{
    errval_t err;

    /* copy supercn to memory server */;
    struct capref init_supercn_cap = {
        .cnode = cnode_root,
        .slot  = ROOTCN_SLOT_SUPERCN
    };
    struct capref child_supercn_cap = {
        .cnode = si->rootcn,
        .slot  = ROOTCN_SLOT_SUPERCN
    };
    err = cap_copy(child_supercn_cap, init_supercn_cap);
    if (err_is_fail(err)) {
        return err_push(err, INIT_ERR_COPY_SUPERCN_CAP);
    }

    return SYS_ERR_OK;
}

errval_t initialize_monitor(struct spawninfo *si)
{
    errval_t err;

    /* Give monitor the kernel capability */
    struct capref dest, src;
    dest.cnode = si->taskcn;
    dest.slot  = TASKCN_SLOT_KERNELCAP;
    src.cnode = cnode_task;
    src.slot  = TASKCN_SLOT_KERNELCAP;
    err = cap_copy(dest, src);
    if (err_is_fail(err)) {
        return err_push(err, INIT_ERR_COPY_KERNEL_CAP);
    }

    /* Give monitor.0 the BSP KCB capability */
    dest.cnode = si->rootcn;
    dest.slot  = ROOTCN_SLOT_BSPKCB;
    src.cnode = cnode_root;
    src.slot  = ROOTCN_SLOT_BSPKCB;
    err = cap_copy(dest, src);
    if (err_is_fail(err)) {
        return err_push(err, INIT_ERR_COPY_BSP_KCB);
    }

    /* Give monitor the perfmon capability */
    dest.cnode = si->taskcn;
    dest.slot = TASKCN_SLOT_PERF_MON;
    src.cnode = cnode_task;
    src.slot = TASKCN_SLOT_PERF_MON;
    err = cap_copy(dest, src);
    if (err_is_fail(err)) {
        return err_push(err, INIT_ERR_COPY_PERF_MON);
    }

    /* Give monitor the IPI capability */
    dest.cnode = si->taskcn;
    dest.slot = TASKCN_SLOT_IPI;
    src.cnode = cnode_task;
    src.slot = TASKCN_SLOT_IPI;
    err = cap_copy(dest, src);
    if (err_is_fail(err)) {
        return err_push(err, INIT_ERR_COPY_IPI);
    }

    /* Give monitor the ProcessManager capability */
    dest.cnode = si->taskcn;
    dest.slot = TASKCN_SLOT_PROC_MNG;
    src.cnode = cnode_task;
    src.slot = TASKCN_SLOT_PROC_MNG;
    err = cap_copy(dest, src);
    if (err_is_fail(err)) {
        return err_push(err, INIT_ERR_COPY_PROC_MNG_CAP);
    }

    /* Give monitor modulecn */
    dest.cnode = si->rootcn;
    dest.slot  = ROOTCN_SLOT_MODULECN;
    src.cnode = cnode_root;
    src.slot  = ROOTCN_SLOT_MODULECN;
    err = cap_copy(dest, src);
    if (err_is_fail(err)) {
        return err_push(err, INIT_ERR_COPY_MODULECN_CAP);
    }

    /* Give monitor physaddr cn */
    dest.cnode = si->rootcn;
    dest.slot  = ROOTCN_SLOT_PACN;
    src.cnode = cnode_root;
    src.slot  = ROOTCN_SLOT_PACN;
    err = cap_copy(dest, src);
    if (err_is_fail(err)) {
        return err_push(err, INIT_ERR_COPY_PACN_CAP);
    }

    /* Give monitor IRQ */
    dest.cnode = si->taskcn;
    dest.slot  = TASKCN_SLOT_IRQ;
    src.cnode = cnode_task;
    src.slot  = TASKCN_SLOT_IRQ;
    err = cap_copy(dest, src);
    if (err_is_fail(err)) {
        return err_push(err, INIT_ERR_COPY_IRQ_CAP);
    }

#if !defined(__ARM_ARCH_8A__)
    /* Give monitor IO */
    dest.cnode = si->taskcn;
    dest.slot  = TASKCN_SLOT_IO;
    src.cnode = cnode_task;
    src.slot  = TASKCN_SLOT_IO;
    err = cap_copy(dest, src);
    if (err_is_fail(err)) {
        return err_push(err, INIT_ERR_COPY_IO_CAP);
    }
#endif

#ifdef __k1om__
    /* Give monitor system memory cap */
    dest.cnode = si->taskcn;
    dest.slot  = TASKCN_SLOT_SYSMEM;
    src.cnode = cnode_task;
    src.slot  = TASKCN_SLOT_SYSMEM;
    err = cap_copy(dest, src);
    if (err_is_fail(err)) {
        return err_push(err, INIT_ERR_COPY_IO_CAP);
    }

    dest.cnode = si->taskcn;
    dest.slot  = TASKCN_SLOT_COREBOOT;
    src.cnode = cnode_task;
    src.slot  = TASKCN_SLOT_COREBOOT;
    err = cap_copy(dest, src);
    if (err_is_fail(err)) {
        return err_push(err, INIT_ERR_COPY_IO_CAP);
    }
#endif

    return SYS_ERR_OK;
}
