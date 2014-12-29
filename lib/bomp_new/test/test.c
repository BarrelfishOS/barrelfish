
#include <barrelfish/barrelfish.h>
#include <omp.h>

#define ITERATIONS 10000

int main(int arc, char *argv[])
{
	debug_printf("Bomp New Test started\n");

	debug_printf("==========================\n");

	bomp_init(BOMP_THREADS_ALL);

	debug_printf("==========================\n");
	debug_printf("==========================\n");

	uint32_t array[10];
	memset(array, 0, sizeof(array));


	uint64_t counter = 0;

#pragma omp parallel for
	for (uint32_t i = 0; i < ITERATIONS; ++i) {
	    array[omp_get_thread_num()]++;
	    if ((i % 5000) == 0) {
	        debug_printf("loop %u\n", i);
	    }
	    counter++;
	}

	assert(counter == ITERATIONS);
    debug_printf("array: %u %u %u %u", array[0], array[1], array[2], array[3]);

	debug_printf("==========================\n");
	debug_printf("==========================\n");
	counter = 0;
	memset(array, 0, sizeof(array));

#pragma omp parallel for
    for (uint32_t i = 0; i < 10000; ++i) {
        array[omp_get_thread_num()]++;
        if ((i % 5000) == 0) {
            debug_printf("loop %u\n", i);
        }
        counter++;
    }
    debug_printf("array: %u %u %u %u", array[0], array[1], array[2], array[3]);
    assert(counter == ITERATIONS);

    debug_printf("==========================\n");
    debug_printf("==========================\n");

    counter = 0;
    memset(array, 0, sizeof(array));

#pragma omp parallel for
    for (uint32_t i = 0; i < 12; ++i) {
#pragma omp parallel for
        for (uint32_t j = 0; j < 10; ++j) {
                debug_printf("loop %u.%u\n", i, j);
            array[omp_get_thread_num()]++;
            __sync_fetch_and_add(&counter, 1);
        }
    }

    debug_printf("array: %u %u %u %u", array[0], array[1], array[2], array[3]);
    if (counter != 120) {
        debug_printf("%lu %u\n\n", counter, 120);
    }

    while(1)
        ;
    assert(counter == 120);


    debug_printf("==========================\n");
    debug_printf("==========================\n");


    debug_printf("==========================\n");

	debug_printf("Bomp New Test terminated\n");

	while(1)
	    ;
	return 0;
}
