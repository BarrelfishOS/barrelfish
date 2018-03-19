/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef DRIVERKIT_H
#define DRIVERKIT_H

#include <barrelfish/types.h>
#include <errors/errno.h>
#include <collections/list.h>

struct bfdriver;


/**
 * Kaluga passes a CNode with capabilities to the pci driver. The offset
 * in this CNode are defined here
 */
#define DRIVERKIT_ARGCN_SLOT_DEVID  0
#define DRIVERKIT_ARGCN_SLOT_INT    1
#define DRIVERKIT_ARGCN_SLOT_EP     2
#define DRIVERKIT_ARGCN_SLOT_BAR0   3
#define DRIVERKIT_ARGCN_SLOT_BAR1   4
#define DRIVERKIT_ARGCN_SLOT_BAR2   5
#define DRIVERKIT_ARGCN_SLOT_BAR3   6
#define DRIVERKIT_ARGCN_SLOT_BAR4   7
#define DRIVERKIT_ARGCN_SLOT_BAR5   8
#define DRIVERKIT_ARGCN_SLOT_BAR6   9
#define DRIVERKIT_ARGCN_SLOT_MAX    10

// Generic struc to track all driver instance state.
struct bfdriver_instance {
    char name[256]; //< This is owned by driverkit 'modules.c'.
    struct bfdriver* driver; //< This is owned by driverkit 'modules.c'.
    iref_t control; //< This is initialized by driverkit 'dcontrol_service.c'.

    struct capref caps[6];
    uint8_t capc;
    struct cnoderef argcn;
    struct capref   argcn_cap;
    char *argv[4];
    char _argv[4][256];
    uint8_t argc;

    iref_t device; //< Driver state. This is owned by the driver implementation.
    void* dstate;  //< Driver state. This is owned by the driver implementation.
};

typedef errval_t(*driver_init_fn)(struct bfdriver_instance*, uint64_t flags, iref_t*);
typedef errval_t(*driver_attach_fn)(struct bfdriver_instance*);
typedef errval_t(*driver_detach_fn)(struct bfdriver_instance*);
typedef errval_t(*driver_set_sleep_level_fn)(struct bfdriver_instance*, uint32_t level);
typedef errval_t(*driver_destroy_fn)(struct bfdriver_instance*);

struct bfdriver {
    char name[256];
    driver_init_fn init;
    driver_attach_fn attach;
    driver_detach_fn detach;
    driver_set_sleep_level_fn set_sleep_level;
    driver_destroy_fn destroy;
};

errval_t driverkit_create_driver(const char* cls, struct bfdriver_instance *bfi,
                                 uint64_t flags, iref_t* dev, iref_t* ctrl);
errval_t driverkit_destroy(const char* name);
void driverkit_list(struct bfdriver**, size_t*);
struct bfdriver* driverkit_lookup_cls(const char*);


errval_t driverkit_get_ep_cap(struct bfdriver_instance *bfi, struct capref *cap);
errval_t driverkit_get_interrupt_cap(struct bfdriver_instance *bfi, struct capref *cap);
errval_t driverkit_get_devid_cap(struct bfdriver_instance *bfi, struct capref *cap);
errval_t driverkit_get_bar_cap(struct bfdriver_instance *bfi, uint8_t idx,
                               struct capref *cap);


/** driver domain flounder interface */
struct domain_instance {
    uint64_t service_id;
    collections_listnode* to_spawn;
    collections_listnode* spawned;
    struct ddomain_binding *b;
};

struct driver_instance {
    char* driver_name;
    char* inst_name;
    size_t arg_idx;
    char** args;
    size_t cap_idx;
    struct capref* caps;
    uint64_t flags;
    iref_t dev;
    iref_t control;
    struct capref argcn_cap;
    struct cnoderef argcn;
};
errval_t ddomain_communication_init(iref_t kaluga_iref, uint64_t id);
errval_t ddomain_controller_init(void);
struct domain_instance* ddomain_create_domain_instance(uint64_t id);
struct driver_instance* ddomain_create_driver_instance(char* driver_name, char* inst_name);
errval_t ddomain_instantiate_driver(struct domain_instance* di, struct driver_instance* drv);
void ddomain_free_driver_inst(void* arg);
void ddomain_free_domain_inst(void* arg);
errval_t ddomain_driver_add_cap(struct driver_instance* drv, struct capref cap);
errval_t ddomain_driver_add_arg(struct driver_instance* drv, char *str);
void ddomain_wait_for_id(void);

/** driver control flounder interface */
errval_t dcontrol_service_init(struct bfdriver_instance* bfi, struct waitset* ws);

errval_t map_device_register(lpaddr_t, size_t, lvaddr_t*);
errval_t map_device_cap(struct capref, lvaddr_t *);

errval_t driverkit_local_service_register(char* name, void* tbl);
void* driverkit_local_service_lookup(char* name);

#define __bfdrivers		__attribute__((__section__(".bfdrivers")))
#define __visible       __attribute__((__externally_visible__))
#define __waligned       __attribute__((__aligned__(sizeof(size_t))))
#define __used          __attribute__((__used__))
#define ___PASTE(a,b) a##b
#define __PASTE(a,b) ___PASTE(a,b)
#define __UNIQUE_ID(prefix) __PASTE(__PASTE(__UNIQUE_ID_, prefix), __COUNTER__)

#define DEFINE_MODULE(name, init_fn, attach_fn, detach_fn, sleep_fn, destroy_fn) \
    struct bfdriver __UNIQUE_ID(name)               \
        __used                                      \
        __visible                                   \
        __bfdrivers                                 \
        __waligned =  {                             \
        #name,                                      \
        init_fn,                                    \
        attach_fn,                                  \
        detach_fn,                                  \
        sleep_fn,                                   \
        destroy_fn                                  \
    };


#endif // DRIVERKIT_H
