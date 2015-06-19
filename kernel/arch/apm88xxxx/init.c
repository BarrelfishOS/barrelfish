#include <kernel.h>
#include <serial.h>
#include <uefi_mmap.h>
#include <sysreg.h>
#include <multiboot.h>

/*
 * Create kernel page tables (high 256G)
 * We use GB sections (level 1 entries that point to memory)
 */
static void paging_init(void)
{
    return;
}

static void paging_dump(void)
{
    lvaddr_t lvl0 = sysreg_read_ttbr0();
    lvl0 = lvl0;
}

bool is_bsp = true;

__attribute__((noreturn))
void arch_init(void *pointer, EFI_MEMORY_DESCRIPTOR *uefi_mmap);
// Currently
void arch_init(void *pointer, EFI_MEMORY_DESCRIPTOR *uefi_mmap)
{
    // break to attach gdb here
    __asm volatile ("wfi":::);

    // set both console ports: UART0 is the one that's connected to the DB9
    // connector on the back of the mustang boxes.
    serial_console_port = 0;
    serial_debug_port   = 0;

    // init serial console, skip hwinit, as the port is guaranteed to be
    // initialized by UEFI.
    serial_console_init(false);

    if (is_bsp) {
        printf("ACPI root table (RSDP):  %p\n", pointer);
        printf("UEFI memory map pointer: %p\n", uefi_mmap);

        printf("First memory map entry:\n");
        printf(" Type:      %x\n", uefi_mmap->Type);
        printf(" PhysStart: 0x%lx\n", uefi_mmap->PhysicalStart);
        printf(" VirtStart: 0x%lx\n", uefi_mmap->VirtualStart);
        printf(" #pages:    %lu\n", uefi_mmap->NumberOfPages);
        printf(" Attrs:     %lx\n", uefi_mmap->Attribute);

        struct multiboot_info *mb = pointer;
        mb = mb;

        // TODO: finish BSP core init
    } else {
        // TODO: AP core init
    }

    // print something
    printf("Barrelfish APM88xxxx CPU driver starting at addr 0x%"
            PRIxLVADDR" on core %"PRIuCOREID"\n",
            local_phys_to_mem((lpaddr_t)&kernel_first_byte), my_core_id);

    paging_dump();
    paging_init();

    while(1) {
        __asm volatile ("wfi":::);
    }
}
