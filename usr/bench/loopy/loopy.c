#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <bench/bench.h>

#define ITERATIONS 100000000

int main(int argc, char *argv[])
{
    bench_init();
    int k = 300;
    while(k--) {

        uint64_t start = bench_tsc();
        for (volatile int i = 0; i < ITERATIONS; i++);
        uint64_t end = bench_tsc();

        printf("%"PRIu64"\n", end - start);
    }

  return EXIT_SUCCESS;
}
