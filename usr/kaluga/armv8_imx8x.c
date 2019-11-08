#include <armv8_imx8x.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <skb/skb.h>
#include <barrelfish_kpi/platform.h>
#include <if/monitor_blocking_defs.h>
#include "kaluga.h"

static errval_t armv8_startup_common_noacpi(void)
{
    errval_t err = SYS_ERR_OK;

    // We need to run on core 0
    // (we are responsible for booting all the other cores)
    assert(my_core_id == BSP_CORE_ID);

    err = skb_client_connect();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Connect to SKB.");
    }

    // Make sure the driver db is loaded
    err = skb_execute("[device_db].");
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Device DB not loaded.");
    }

    KALUGA_DEBUG("Kaluga: watch_for_cores\n");

    err = watch_for_cores();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Watching cores.");
    }

    KALUGA_DEBUG("Kaluga: pci_root_bridge\n");

    err = watch_for_pci_root_bridge();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Watching PCI root bridges.");
    }

    KALUGA_DEBUG("Kaluga: pci_devices\n");

    err = watch_for_pci_devices();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Watching PCI devices.");
    }

    KALUGA_DEBUG("Kaluga: wait for spawnd\n");
    err = nameservice_blocking_lookup("spawn.0", NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "wait for spawnd");
    }
    err = oct_set("all_spawnds_up { iref: 0 }");
    assert(err_is_ok(err));
    return SYS_ERR_OK;
}

static inline errval_t
invoke_monitor_create_cap(uint64_t *raw, capaddr_t caddr, int level, capaddr_t slot, coreid_t owner)
{
    struct capref my_cap_kernel = {
        .cnode = cnode_task,
        .slot  = TASKCN_SLOT_KERNELCAP
    };
    return cap_invoke6(my_cap_kernel, KernelCmd_Create_cap, caddr, level, slot,
                       owner, (uintptr_t)raw).error;
}

static errval_t imx8x_get_device_cap(lpaddr_t address, size_t size, struct capref *devframe) {
    KALUGA_DEBUG("HACK: Forging cap directly in kaluga....\n");
    errval_t err;
    err = slot_alloc(devframe);
    assert(err_is_ok(err));
    capaddr_t caddr = get_cnode_addr(*devframe);
    uint8_t level = get_cnode_level(*devframe);
    size_t  slot  = devframe->slot;

    assert(address % 4096 == 0);
    assert(size % 4096 == 0);

    struct capability the_cap = {
        .type = ObjType_DevFrame,
        .rights = CAPRIGHTS_ALLRIGHTS,
        .u.devframe.base = address,
        .u.devframe.bytes = size
    };

    return invoke_monitor_create_cap((uint64_t*)&the_cap, caddr, level, slot, disp_get_core_id());
}



__attribute__((used))
static errval_t start_gpio(char*name, lpaddr_t address)

{   errval_t err;
    struct module_info *mi;
    mi = find_module("imx8x_gpio");
    if(mi == NULL){
        KALUGA_DEBUG("imx8x_gpio not found, not starting\n");
        return KALUGA_ERR_MODULE_NOT_FOUND;
    }
    struct driver_argument arg;
    init_driver_argument(&arg);
    arg.module_name = "imx8x_gpio_module";
    struct capref device_frame;
    err = imx8x_get_device_cap(address, 0x1000, &device_frame);
    if(err_is_fail(err)){
        USER_PANIC_ERR(err, "get_device_cap");
    }
    KALUGA_DEBUG("get_device_cap worked\n");
    //transfer destination
    struct capref cap = {
            .cnode = (&arg)->argnode_ref,
            .slot = DRIVERKIT_ARGCN_SLOT_BAR0
    };
    err = cap_copy(cap, device_frame);
    if(err_is_fail(err)){
        USER_PANIC_ERR(err, "get_device_cap");
    }
    err = default_start_function_pure(0, mi, name, &arg);
    if(err_is_fail(err)){
        USER_PANIC_ERR(err, "get_device_cap");
    }
    return err;
} 

__attribute__((used))
static errval_t imx8x_serial_kernel(void)

{   errval_t err;
    struct module_info *mi;
    mi = find_module("serial_kernel");
    struct driver_argument arg;
    init_driver_argument(&arg);
    arg.flags = 1; // disable interrupt
    arg.module_name = "serial_kernel";
    err = default_start_function_pure(0, mi,"serial_kernel {}", &arg);
    return err;
} 
//serial_lpuart
__attribute__((used))
static errval_t start_serial_lpuart(lpaddr_t address)

{   errval_t err;
    struct module_info *mi;
    mi = find_module("serial_lpuart");
    if(mi == NULL){
        KALUGA_DEBUG("serial_lpuart not found, not starting");
        return KALUGA_ERR_MODULE_NOT_FOUND;
    }
    struct driver_argument arg;
    init_driver_argument(&arg);
    arg.module_name = "serial_lpuart";
    struct capref device_frame;
    err = imx8x_get_device_cap(address, 0x00010000, &device_frame);
    if(err_is_fail(err)){
        USER_PANIC_ERR(err, "get_device_cap");
    }
    KALUGA_DEBUG("get_device_cap worked\n");
    //transfer destination
    struct capref cap = {
            .cnode = (&arg)->argnode_ref,
            .slot = DRIVERKIT_ARGCN_SLOT_BAR0
    };
    err = cap_copy(cap, device_frame);
    if(err_is_fail(err)){
        USER_PANIC_ERR(err, "get_device_cap");
    }
    // Store capability in driver_argument: See add_mem_args in start_pci.c lines 197 and following
    err = default_start_function_pure(0, mi, "serial_lpuart {}", &arg);
    if(err_is_fail(err)){
        USER_PANIC_ERR(err, "get_device_cap");
    }
    return err;
} 

//serial_lpuart
__attribute__((used))
static errval_t start_network(lpaddr_t address)

{   errval_t err;
    struct module_info *mi;
    mi = find_module("enet");
    if(mi == NULL){
        KALUGA_DEBUG("enet not found, not starting");
        return KALUGA_ERR_MODULE_NOT_FOUND;
    }
    struct driver_argument arg;
    init_driver_argument(&arg);
    arg.module_name = "enet_module";
    struct capref device_frame;
    err = imx8x_get_device_cap(address, 0x00010000, &device_frame);
    if(err_is_fail(err)){
        USER_PANIC_ERR(err, "get_device_cap");
    }
    KALUGA_DEBUG("get_device_cap worked\n");
    //transfer destination
    struct capref cap = {
            .cnode = (&arg)->argnode_ref,
            .slot = DRIVERKIT_ARGCN_SLOT_BAR0
    };
    err = cap_copy(cap, device_frame);
    if(err_is_fail(err)){
        USER_PANIC_ERR(err, "get_device_cap");
    }
    // Store capability in driver_argument: See add_mem_args in start_pci.c lines 197 and following
    err = default_start_function_pure(0, mi, "enet {}", &arg);
    if(err_is_fail(err)){
        USER_PANIC_ERR(err, "get_device_cap");
    }
    return err;
} 

errval_t imx8x_startup(void)
{
    errval_t err;
    err = armv8_startup_common_noacpi();
    if(err_is_fail(err)){
        USER_PANIC_ERR(err, "startup common");
    }
    err = start_gpio("imx8x.gpio1 {}", 0x5D090000);
    if(err_is_fail(err) && err_no(err) != KALUGA_ERR_MODULE_NOT_FOUND) {
        USER_PANIC_ERR(err, "gpio1 start");
    }
    err = start_gpio("imx8x.gpio2 {}", 0x5D0B0000);
    if(err_is_fail(err) && err_no(err) != KALUGA_ERR_MODULE_NOT_FOUND) {
        USER_PANIC_ERR(err, "gpio2 start");
    }

    err = start_serial_lpuart(0x5A090000);
    if(err_is_fail(err) && err_no(err) != KALUGA_ERR_MODULE_NOT_FOUND) {
        USER_PANIC_ERR(err, "imx8x serial lpuart");
    }
    
    err = start_network(0x5B040000);
    if(err_is_fail(err) && err_no(err) != KALUGA_ERR_MODULE_NOT_FOUND) {
        USER_PANIC_ERR(err, "imx8x network");
    }
    return SYS_ERR_OK;
}
