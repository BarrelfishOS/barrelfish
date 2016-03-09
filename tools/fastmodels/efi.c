#include <stdio.h>

#include "efi.h"
#include "util.h"

void
print_mmap(efi_memory_descriptor *mmap, size_t mmap_len) {
    size_t i;

    printf("EFI Memory Map:\n");
    for(i= 0; i < mmap_len; i++) {
        printf("%016lx-%016lx    ",
               mmap[i].PhysicalStart,
               mmap[i].PhysicalStart + (mmap[i].NumberOfPages * PAGE_4k) - 1);
        switch(mmap[i].Type) {
            case EfiConventionalMemory:
                printf("EfiConventionalMemory");
                break;
            case EfiBarrelfishCPUDriver:
                printf("EfiBarrelfishCPUDriver");
                break;
            case EfiBarrelfishCPUDriverStack:
                printf("EfiBarrelfishCPUDriverStack");
                break;
            case EfiBarrelfishMultibootData:
                printf("EfiBarrelfishMultibootData");
                break;
            case EfiBarrelfishELFData:
                printf("EfiBarrelfishELFData");
                break;
            case EfiBarrelfishBootPageTable:
                printf("EfiBarrelfishBootPageTable");
                break;
            default:
                printf("Unrecognised");
        }
        printf(" (%lukB)\n", mmap[i].NumberOfPages * 4);
    }
}
