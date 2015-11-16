#include <kernel.h>
#include <serial.h>
#include <uefi_mmap.h>
#include <sysreg.h>
#include <multiboot2.h>
#include <paging_kernel_arch.h>

/* XXX - shouldn't be here. */
bool is_bsp;

__attribute__((noreturn))
void plat_init(uint32_t magic, void *pointer);

/* Hagfish guarantees us the following on calling plat_init():
    * Single core running (not guaranteed to be core 0)
    * CPU is in highest non-secure privilege mode: EL2 or EL1
    * MMU enabled, 4k translation granule, 1:1 mapping of all RAM, using TTBR0.
    * Little-endian mode
    * Core caches (L1&L2) and TLB enabled
    * Non-architectural caches disabled
    * Interrupts enabled
    * Generic timer initialized and enabled
    * >= 128KiB stack
    * ACPI tables available
    * Register x0 contains handle to ACPI root table
    * Register x1 contains a pointer to the UEFI memory map
 */

void plat_init(uint32_t magic, void *pointer) {
    struct multiboot_header *mb= NULL;

    /* Uncomment the line below to wait here for GDB. */
    /* __asm volatile ("wfi":::); */

    /* Set both console ports: UART0 is the one that's connected to the DB9
       connector on the back of the Mustang boxes. */
    /* XXX - we should get EFI to tell us this. */
    serial_console_port= 0;
    serial_debug_port=   0;

    /* Initialise the serial console. Skip hardware initialisation, as the
       port is guaranteed to have been initialized by UEFI. */
    serial_console_init(false);

    switch(magic) {
    case MULTIBOOT2_BOOTLOADER_MAGIC:
        is_bsp= true;
        mb= (struct multiboot_header *)pointer;
        break;
    default:
        is_bsp= false;
        panic("Implement AP booting!");
        break;
    }

    if (is_bsp) {
        // TODO: finish BSP core init
    } else {
        // TODO: AP core init
        panic("AP init");
    }

    printf("Barrelfish APM88xxxx CPU driver starting at addr 0x%"
            PRIxLVADDR" on core %"PRIuCOREID"\n",
            local_phys_to_mem((lpaddr_t)&kernel_first_byte), my_core_id);

    while(1) {
        __asm volatile ("wfi":::);
    }
}
