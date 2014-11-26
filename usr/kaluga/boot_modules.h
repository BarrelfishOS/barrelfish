#ifndef BOOT_MODULES_H_
#define BOOT_MODULES_H_

#include <barrelfish/barrelfish.h>

struct module_info;
typedef errval_t(*module_start_fn)(coreid_t where, struct module_info* mi,
        char* record);

#define MAX_DRIVER_INSTANCES 16

struct module_info {
    char* complete_line;
    char* path;
    char* binary;

    char* args; ///< cmd args as a single string

    char* cmdargs; ///< Used for pointers in argv
    int argc;
    char* argv[MAX_CMDLINE_ARGS + 1];

    module_start_fn start_function;
    uint8_t allow_multi;    ///< allow multiple driver instances
    uint8_t num_started;    ///< keeps track of the number of started domains
    coreid_t coreoffset;     ///< next coreid to start the new instance on
    domainid_t did[MAX_DRIVER_INSTANCES];
};


void init_environ(void);
errval_t init_boot_modules(void);
struct module_info* find_module(char*);

bool is_started(struct module_info*);
bool can_start(struct module_info*);
bool is_auto_driver(struct module_info*);
void set_start_function(char*, module_start_fn);
void set_started(struct module_info*);
void set_multi_instance(struct module_info*, uint8_t);
void set_core_id_offset(struct module_info*, coreid_t);
domainid_t *get_did_ptr(struct module_info *);
coreid_t get_core_id_offset(struct module_info*);

#endif /* BOOT_MODULES_H_ */
