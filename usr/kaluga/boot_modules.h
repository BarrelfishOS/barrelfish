#ifndef BOOT_MODULES_H_
#define BOOT_MODULES_H_

#include <barrelfish/barrelfish.h>
#include <int_route/int_model.h>

struct module_info;
struct int_startup_argument;

struct driver_argument {
    struct capref arg_caps;
    struct int_startup_argument int_arg;
    struct cnoderef argnode_ref;

};
typedef errval_t(*module_start_fn)(coreid_t where, struct module_info* mi,
        char* record, struct driver_argument * int_arg);

#define MAX_DRIVER_INSTANCES 16

struct module_info {
    char* complete_line;
    char* path;
    char* binary;

    char* args; ///< cmd args as a single string

    char* cmdargs; ///< Used for pointers in argv
    int argc;
    char* argv[MAX_CMDLINE_ARGS + 1];

    struct domain_instance* driverinstance;

    module_start_fn start_function;
    uint8_t allow_multi;    ///< allow multiple driver instances
    uint8_t num_started;    ///< keeps track of the number of started domains
    coreid_t coreoffset;     ///< next coreid to start the new instance on
    struct capref did[MAX_DRIVER_INSTANCES];
};


errval_t init_driver_argument(struct driver_argument *arg);

void init_environ(void);
errval_t init_boot_modules(void);
struct module_info* find_module(char*);
struct module_info* find_corectrl_for_cpu_type(enum cpu_type cpu_type);

bool is_started(struct module_info*);
bool can_start(struct module_info*);
bool is_auto_driver(struct module_info*);
void set_start_function(char*, module_start_fn);
void set_started(struct module_info*);
void set_multi_instance(struct module_info*, uint8_t);
void set_core_id_offset(struct module_info*, coreid_t);
struct capref *get_did_ptr(struct module_info *);
coreid_t get_core_id_offset(struct module_info*);

#endif /* BOOT_MODULES_H_ */
