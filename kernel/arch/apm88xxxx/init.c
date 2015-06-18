#include <kernel.h>
#include <serial.h>
#include <uefi_mmap.h>

/**
 * \brief Kernel stack.
 *
 * This is the one and only kernel stack for a kernel instance.
 */
uintptr_t kernel_stack[KERNEL_STACK_SIZE / sizeof(uintptr_t)] __attribute__ ((aligned(16)));


__attribute__((noreturn))
void arch_init(void *arg1, EFI_MEMORY_DESCRIPTOR *uefi_mmap);
// Currently
void arch_init(void *arg1, EFI_MEMORY_DESCRIPTOR *uefi_mmap)
{
    // set console port: UART0 is the one that's connected to the DB9
    // connector on the back of the mustang boxes.
    serial_console_port = 0;

    // init serial console, skip hwinit, as the port is guaranteed to be
    // initialized by UEFI.
    serial_console_init(false);

    // print something
    printf("Barrelfish APM88xxxx CPU driver starting at addr 0x%"
            PRIxLVADDR" on core %"PRIuCOREID" xxxxxxxxxx\n",
            local_phys_to_mem((lpaddr_t)&kernel_first_byte), my_core_id);

    printf("ACPI root table (RSDP):  %p\n", arg1);
    printf("UEFI memory map pointer: %p\n", uefi_mmap);

    uint32_t *mmap_ptr = (uint32_t *)uefi_mmap;

    printf("Test 1: %p\n", mmap_ptr);
    printf("Test 2: %p\n", mmap_ptr+1);
    printf("Test 3: %p\n", mmap_ptr+2);
    printf("Test 4: %p\n", mmap_ptr+3);
    printf("Test 5: %p\n", mmap_ptr+4);
#if 0
    for (int i = 0; i < 8; i++) {
        printf("%016lx: 0x%08x\n", (uint64_t)(mmap_ptr+i), mmap_ptr[i]);
    }

    printf("First memory map entry:\n");
    printf(" Type:      %x\n", uefi_mmap->Type);
    printf(" PhysStart: 0x%lx\n", uefi_mmap->PhysicalStart);
    printf(" VirtStart: 0x%lx\n", uefi_mmap->VirtualStart);
    printf(" #pages:    %lu\n", uefi_mmap->NumberOfPages);
    printf(" Attrs:     %lx\n", uefi_mmap->Attribute);

#endif

    *(uint32_t*)0x1c020000 = 'a';

    while(1) {
        __asm volatile ("nop":::);
    }
}
