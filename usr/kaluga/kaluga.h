#ifndef KALUGA_H_
#define KALUGA_H_

#include <barrelfish/barrelfish.h>
#include <dist2/dist2.h>

#include "queue.h"
#include "debug.h"

#define TRIGGER_ALWAYS (DIST_PERSIST | DIST_ON_SET | DIST_ON_DEL | DIST_ALWAYS_SET)
#define BSP_CORE_ID 0

struct module_info {
    char* complete_line;
    char* path;
    char* binary;

    char* cmdargs; // Used for pointers in argv
    int argc;
    char* argv[MAX_CMDLINE_ARGS + 1];

    domainid_t did;
};

extern coreid_t my_core_id;
extern uint32_t my_arch_id;


errval_t trigger_existing_and_watch(const char*,
        trigger_handler_fn,  dist2_trigger_id_t*);

errval_t watch_for_cores(void);
errval_t watch_for_pci_root_bridge(void);
errval_t watch_for_pci_devices(void);

void init_environ(void);
errval_t init_boot_modules(void);
struct module_info* find_module(char*);
bool is_auto_driver(struct module_info* mi);

#endif /* KALUGA_H_ */
