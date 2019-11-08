#include <armv8_imx8x.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/spawn_client.h>
#include <skb/skb.h>
#include <barrelfish_kpi/platform.h>
#include <if/monitor_blocking_defs.h>
#include "kaluga.h"
#include <pci/pci.h>

// For booting cores
#include <hw_records_arch.h>
#include <barrelfish/cpu_arch.h>

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

    err = skb_execute("[plat_imx8x].");
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Plat imx8x not loaded.");
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

{
    errval_t err;
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

/* IMX8X Reference Manual page 65 */
#define UART0_INT 257
#define UART1_INT 258
#define UART2_INT 259
#define UART3_INT 260
#define DIST_OFFSET 1000  /* First interrupt index on the distributor */

__attribute__((used))
static errval_t start_serial_lpuart(lpaddr_t address, uint32_t irq)

{   errval_t err;
    struct module_info *mi;
    
    // get module
    mi = find_module("serial_lpuart");
    if(mi == NULL){
        KALUGA_DEBUG("serial_lpuart not found, not starting");
        return KALUGA_ERR_MODULE_NOT_FOUND;
    }
    struct driver_argument arg;
    init_driver_argument(&arg);
    arg.module_name = "serial_lpuart";

    // get device frame
    struct capref device_frame;
    err = imx8x_get_device_cap(address, 0x00010000, &device_frame);
    if(err_is_fail(err)){
        USER_PANIC_ERR(err, "get_device_cap");
    }
    KALUGA_DEBUG("got device frame for lpuart\n");


    struct capref cap = {
            .cnode = arg.argnode_ref,
            .slot = DRIVERKIT_ARGCN_SLOT_BAR0
    };
    err = cap_copy(cap, device_frame);
    if(err_is_fail(err)){
        USER_PANIC_ERR(err, "get_device_cap");
    }

    // get irq src for lpuart
    struct capref irq_src;
    err = slot_alloc(&irq_src);
    assert(err_is_ok(err));

    err = sys_debug_create_irq_src_cap(irq_src, irq + DIST_OFFSET, irq + DIST_OFFSET);
    assert(err_is_ok(err));

    struct capref irq_dst = {
        .cnode = arg.argnode_ref,
        .slot = PCIARG_SLOT_INT 
    };
    err = cap_copy(irq_dst, irq_src);
    if(err_is_fail(err)){
        DEBUG_ERR(err, "cap_copy\n");
        return err;
    }
    KALUGA_DEBUG("got irq src cap frame for lpuart\n");
     
    err = default_start_function_pure(0, mi, "serial_lpuart {}", &arg);
    if(err_is_fail(err)){
        USER_PANIC_ERR(err, "get_device_cap");
    }
    return err;
} 


#define USDHC1_BASE 0x5B010000
#define USDHC2_BASE 0x5B020000
#define USDHC0_INT 264
#define USDHC1_INT 265
#define USDHC2_INT 266

__attribute__((used))
static errval_t start_sdhc(lpaddr_t address, uint32_t irq)

{   errval_t err;
    struct module_info *mi;
    
    // get module
    mi = find_module("imx8x_sdhc");
    if(mi == NULL){
        KALUGA_DEBUG("imx8x_sdhc not found, not starting");
        return KALUGA_ERR_MODULE_NOT_FOUND;
    }
    struct driver_argument arg;
    init_driver_argument(&arg);
    arg.module_name = "sdhc";

    // get device frame
    struct capref device_frame;
    err = imx8x_get_device_cap(address, 0x00010000, &device_frame);
    if(err_is_fail(err)){
        USER_PANIC_ERR(err, "get_device_cap");
    }
    KALUGA_DEBUG("got device frame for sdhc\n");


    struct capref cap = {
            .cnode = arg.argnode_ref,
            .slot = DRIVERKIT_ARGCN_SLOT_BAR0
    };
    err = cap_copy(cap, device_frame);
    if(err_is_fail(err)){
        USER_PANIC_ERR(err, "get_device_cap");
    }

    // get irq src for lpuart
    struct capref irq_src;
    err = slot_alloc(&irq_src);
    assert(err_is_ok(err));

    err = sys_debug_create_irq_src_cap(irq_src, irq + DIST_OFFSET, irq + DIST_OFFSET);
    assert(err_is_ok(err));

    struct capref irq_dst = {
        .cnode = arg.argnode_ref,
        .slot = PCIARG_SLOT_INT 
    };
    err = cap_copy(irq_dst, irq_src);
    if(err_is_fail(err)){
        DEBUG_ERR(err, "cap_copy\n");
        return err;
    }
    KALUGA_DEBUG("got irq src cap frame for lpuart\n");
     
    err = default_start_function_pure(0, mi, "sdhc {}", &arg);
    if(err_is_fail(err)){
        USER_PANIC_ERR(err, "get_device_cap");
    }
    return err;
} 

static lpaddr_t platform_gic_distributor_base = 0x51a00000;
//static lpaddr_t platform_gic_redistributor_base = 0x51b00000;

static errval_t
start_int_route_domains(void)
{
    errval_t err;
    // Int route server
    char *argv[] = {NULL};
    err = spawn_program(0, "int_route", argv, NULL, 0, NULL); 
    if(err_is_fail(err)){
         DEBUG_ERR(err,"int_route start");
         return err;
    }

    //Distributor driver
    struct module_info *mi;
    mi = find_module("pl390_dist");
    if(mi == NULL){
        KALUGA_DEBUG("pl390_dist not found, not starting\n");
        return KALUGA_ERR_MODULE_NOT_FOUND;
    }
    struct driver_argument arg;
    init_driver_argument(&arg);
    arg.module_name = "pl390_dist";
    struct capref device_frame;

    err = imx8x_get_device_cap(platform_gic_distributor_base, 0x1000, &device_frame);
    if(err_is_fail(err)){
        USER_PANIC_ERR(err, "get_device_cap");
    }
    KALUGA_DEBUG("get_device_cap worked\n");
    //transfer destination
    struct capref cap = {
            .cnode = arg.argnode_ref,
            .slot = 0
    };
    err = cap_copy(cap, device_frame);
    if(err_is_fail(err)){
        USER_PANIC_ERR(err, "get_device_cap");
    }
    err = default_start_function_pure(0, mi, "gic.dist {}", &arg);
    if(err_is_fail(err)){
        USER_PANIC_ERR(err, "get_device_cap");
    }
    return err;

   
}

extern size_t cpu_count;

static errval_t start_cores(void){
    /* We can't discover cores an ARMv8 embedded platforms. This 
     * works similar to armv7 startup. */
    errval_t err;
    KALUGA_DEBUG("Kaluga: start_cores\n");

    err = skb_execute_query("findall(mpid(X), arm_mpid(X), Li), write(Li).");
    if (err_is_fail(err)) {
        USER_PANIC_SKB_ERR(err, "Finding cores.");
    }
    int id;
    struct list_parser_status skb_list;
    skb_read_list_init(&skb_list);
    size_t skb_cpus = 0;
    while(skb_read_list(&skb_list, "mpid(%d)", &id)) {
        skb_cpus++;
        err = oct_set(HW_PROCESSOR_ARMV8_RECORD_FORMAT, id, 1 /*enabled*/,
                id, id, CURRENT_CPU_TYPE, id, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, id);
        if(err_is_fail(err)){
            USER_PANIC_ERR(err, "oct_set core");
        }
    }
    cpu_count = skb_cpus;

    err = watch_for_cores();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Watching cores.");
    }

    err = wait_for_all_spawnds(0);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Unable to wait for spawnds failed.");
    }
    return err;
}

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

    err = start_cores();
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "start cores");
    }

    err = start_int_route_domains();
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "start int_route");
    }

    err = start_gpio("imx8x.gpio1 {}", 0x5D090000);
    if(err_is_fail(err) && err_no(err) != KALUGA_ERR_MODULE_NOT_FOUND) {
        USER_PANIC_ERR(err, "gpio1 start");
    }
    err = start_gpio("imx8x.gpio2 {}", 0x5D0B0000);
    if(err_is_fail(err) && err_no(err) != KALUGA_ERR_MODULE_NOT_FOUND) {
        USER_PANIC_ERR(err, "gpio2 start");
    }

    err = start_serial_lpuart(0x5A090000, UART3_INT);
    if(err_is_fail(err) && err_no(err) != KALUGA_ERR_MODULE_NOT_FOUND) {
        USER_PANIC_ERR(err, "imx8x serial lpuart");
    }

    // SDHC2 is the one that is broken out on the carrier board.
    err = start_sdhc(USDHC2_BASE, USDHC2_INT);
    if(err_is_fail(err) && err_no(err) != KALUGA_ERR_MODULE_NOT_FOUND) {
        USER_PANIC_ERR(err, "imx8x sdhc");
    }

    err = start_network(0x5B040000);
    if(err_is_fail(err) && err_no(err) != KALUGA_ERR_MODULE_NOT_FOUND) {
        USER_PANIC_ERR(err, "imx8x network");
    }

    return SYS_ERR_OK;
}
