#include <kernel.h>
#include <serial.h>
#include <uefi_mmap.h>
#include <sysreg.h>
#include <multiboot.h>
#include <paging_kernel_arch.h>

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
    union armv8_ttable_entry *lvl0 =
        (union armv8_ttable_entry *)sysreg_read_ttbr0();
    for (int i = 0; i < 512; i++) {
        union armv8_ttable_entry *entry0 = lvl0 + i;
        if (entry0->d.valid && entry0->d.mb1) {
            printf("%d: level 1 table @%lx\n", i, (entry0->d.base)<<BASE_PAGE_BITS);
            union armv8_ttable_entry *lvl1 =
                (union armv8_ttable_entry *)((uint64_t)(entry0->d.base)<<BASE_PAGE_BITS);
            for (int j = 0; j < 512; j++) {
                union armv8_ttable_entry *entry1 = lvl1 + j;
                if (entry1->d.valid && entry1->d.mb1) {
                    printf("  %d: level 2 table @%lx\n", j, (entry1->d.base)<<BASE_PAGE_BITS);
                } else if (entry1->block_l1.valid) {
                    printf("  %d: level 1 block @%lx\n", j,
                            (entry1->block_l1.base) << HUGE_PAGE_BITS);
                }
            }
        }
    }
}

bool is_bsp = true;

__attribute__((noreturn))
void arch_init(void *pointer, EFI_MEMORY_DESCRIPTOR *uefi_mmap);
// Currently
void arch_init(void *pointer, EFI_MEMORY_DESCRIPTOR *uefi_mmap)
{
    // uncomment line below to force wait to attach gdb here
    // __asm volatile ("wfi":::);

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
