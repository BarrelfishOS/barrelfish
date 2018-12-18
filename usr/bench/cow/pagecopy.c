#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <unistd.h>
#include <stdint.h>
#include <string.h>
#include <sys/mman.h> 
#include <inttypes.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "bench_rdtsc.h"
typedef uint64_t cycles_t;

void* allocate_page() {
    void* page;
    page = mmap(NULL, getpagesize(), PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
    if (page == MAP_FAILED) {
        assert(!"mmap");
    }

    return page;
}

int main(int argc, char** argv)
{
    
    for (size_t i=0; i<200; i++) {

        void* p1 = allocate_page();
        void* p2 = allocate_page();

        cycles_t start = bench_tsc();
        memcpy(p1, p2, getpagesize());
        cycles_t end = bench_tsc();
        printf("%s:%s:%d: copy = %"PRIu64"\n", 
               __FILE__, __FUNCTION__, __LINE__, end-start);

    }

    return 0;
}