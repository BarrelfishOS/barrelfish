#include "ahcid.h"
#include "test.h"

#include <stdarg.h>
#include <barrelfish/deferred.h>
#include <bench/bench.h>
#include <devif/backends/blk/ahci_devq.h>
#include <devif/queue_interface.h>


static uint64_t finish_counter = 0;

struct dma_mem {
    lvaddr_t vaddr;         ///< virtual address of the mapped region
    lpaddr_t paddr;         ///< physical address of the underlying frame
    uint64_t bytes;         ///< size of the region in bytes
    uint64_t requested;     ///< requested size of the region in bytes (<= bytes)
    struct capref frame;    ///< frame capability backing this region
};

void test_runner(int n, ...)
{
    va_list arguments;
    va_start(arguments, n);
    
    // sleep for 5 sec
    barrelfish_usleep(5000*1000);

    for (size_t i=0; i<n; i++) {
        enum AhciTest test = va_arg(arguments, enum AhciTest);
        switch (test) {
            case AhciTest_READ:
                ahci_perf_sequential(1024*1024*1024, 512, false);
                ahci_perf_sequential(1024*1024*1024, 1024, false);
                ahci_perf_sequential(1024*1024*1024, 2048, false);
                ahci_perf_sequential(1024*1024*1024, 4096, false);
                ahci_perf_sequential(1024*1024*1024, 8192, false);
                ahci_perf_sequential(1024*1024*1024, 16384, false);
                ahci_perf_sequential(1024*1024*1024, 32768, false);
                ahci_perf_sequential(1024*1024*1024, 65536, false);
                ahci_perf_sequential(1024*1024*1024, 131072, false);
                ahci_perf_sequential(1024*1024*1024, 262144, false);
                ahci_perf_sequential(1024*1024*1024, 524288, false);
                ahci_perf_sequential(1024*1024*1024, 1048576, false);
                ahci_perf_sequential(1024*1024*1024, 1048576*2, false);
                //ahci_perf_sequential(1024*1024*1024, 1048576*4, false); // ERROR: tilsiter1 OOM
            break;

            case AhciTest_WRITE:
                ahci_perf_sequential(1024*1024*256, 512, true);
                ahci_perf_sequential(1024*1024*256, 1024, true);
                ahci_perf_sequential(1024*1024*256, 2048, true);
                ahci_perf_sequential(1024*1024*256, 4096, true);
                ahci_perf_sequential(1024*1024*256, 8192, true);
                ahci_perf_sequential(1024*1024*256, 16384, true);
                ahci_perf_sequential(1024*1024*256, 32768, true);
                ahci_perf_sequential(1024*1024*256, 65536, true);
                ahci_perf_sequential(1024*1024*256, 131072, true);
                ahci_perf_sequential(1024*1024*256, 262144, true);
                ahci_perf_sequential(1024*1024*256, 524288, true);
                ahci_perf_sequential(1024*1024*256, 1048576, true);
                ahci_perf_sequential(1024*1024*256, 1048576*2, true);
                //ahci_perf_sequential(1024*1024*256, 1048576*4, true); // ERROR: tilsiter1 OOM
            break;

            case AhciTest_VERIFY:
                ahci_verify_sequential(1024*1024*256, 512);
                ahci_verify_sequential(1024*1024*256, 1024);
                ahci_verify_sequential(1024*1024*256, 2048);
                ahci_verify_sequential(1024*1024*256, 4096);
                ahci_verify_sequential(1024*1024*256, 8192);
                ahci_verify_sequential(1024*1024*256, 16384);
                ahci_verify_sequential(1024*1024*256, 32768);
                ahci_verify_sequential(1024*1024*256, 65536);
                ahci_verify_sequential(1024*1024*256, 131072);
                ahci_verify_sequential(1024*1024*256, 262144);
                ahci_verify_sequential(1024*1024*256, 524288);
                ahci_verify_sequential(1024*1024*256, 1048576);
                ahci_verify_sequential(1024*1024*256, 1048576*2);
                ahci_verify_sequential(1024*1024*256, 1048576*4);
            break;

            case AhciTest_BASIC:
                ahci_simple_test();
            break;

            default:
                USER_PANIC("Unknown test?");
            break;
        }
    }
    // Harness line
    printf("AHCI testing completed.\n");
}

static void frame_alloc_identify(size_t size, struct capref *frame, 
                                 struct frame_identity *id)
{
    errval_t err;
    size_t retbytes;

    err = frame_alloc(frame, size, &retbytes);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "frame_alloc");
    }

    err = invoke_frame_identify(*frame, id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "invoke_frame_identify");
    }
}

static void wait_for_interrupt(void)
{
    errval_t err = event_dispatch(&disk_ws);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error in event_dispatch for wait_for_interrupt");
    }
}
void ahci_simple_test(void)
{
    errval_t err;
    regionid_t region_id = 0;
    genoffset_t offset = 0;
    genoffset_t length = 0;
    genoffset_t valid_data = 0;
    genoffset_t valid_length = 0;

    // Allocate a buffer:
    struct dma_mem mem;

    struct capref frame;
    struct frame_identity id;
    //void* va;

    err = frame_alloc(&frame, 4096, NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "frame alloc");
    }

    /*
    err = vspace_map_one_frame_attr(&va, 4096, frame, VREGION_FLAGS_READ_WRITE,
                                    NULL, NULL); 
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "map frame");
    }
    */
    
    err = invoke_frame_identify(frame, &id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "frame identify");
    }

    err = devq_register(dq, frame, &region_id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "devq register");
    }

    uint64_t flags = 0x0;
    devq_enqueue(dq, region_id, 0, 512, 0, 512, flags);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "devq enqueue");
    }

    do {
        err = devq_dequeue(dq, &region_id, &offset, &length, &valid_data, 
                           &valid_length, &flags);
        if (err_is_ok(err)) {
            break;
        }
        if (err_is_fail(err) && err_no(err) != DEVQ_ERR_QUEUE_EMPTY) {
            USER_PANIC_ERR(err, "devq dequeue");
        }
        wait_for_interrupt();
    } while (err_no(err) == DEVQ_ERR_QUEUE_EMPTY);

    assert (offset == 0);
    assert (length == 512);

    err = devq_deregister(dq, region_id, &mem.frame);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "devq_deregister failed.");
    }

    printf("[%s]: DONE\n", __FUNCTION__);
}

static void blocking_dequeue(void* q, regionid_t* region_id, 
                             genoffset_t* offset, genoffset_t* length,
                             genoffset_t* valid_data, genoffset_t* valid_length)
{
    uint64_t flags;
    errval_t err;
    do {
        err = devq_dequeue(q, region_id, offset, length, valid_data,
                           valid_length, &flags);
        if (err_is_ok(err)) {
            break;
        }
        if (err_is_fail(err) && err_no(err) != DEVQ_ERR_QUEUE_EMPTY) {
            USER_PANIC_ERR(err, "devq dequeue");
        }

        assert(err_no(err) == DEVQ_ERR_QUEUE_EMPTY);
        wait_for_interrupt();
    } while (err_no(err) == DEVQ_ERR_QUEUE_EMPTY);
}

static void receive_block(void)
{
    regionid_t rid = 0;
    genoffset_t offset = 0;
    genoffset_t length = 0;
    genoffset_t valid_data = 0;
    genoffset_t valid_length = 0;
    blocking_dequeue(dq, &rid, &offset, &length, &valid_data,
                     &valid_length);
    finish_counter++;
}

void ahci_perf_sequential(size_t buffer_size, size_t block_size, bool write)
{
    finish_counter = 0;
    bench_init();
    errval_t err;
    assert(buffer_size % block_size == 0);

    size_t read_requests = buffer_size / block_size;
    regionid_t region_id = 0;

    static struct capref frame;
    struct frame_identity id;
    frame_alloc_identify(buffer_size, &frame, &id);

    err = devq_register(dq, frame, &region_id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "devq register");
    }

    uint64_t write_flag = (write) ? (1ULL << 63) : 0;
    bufferid_t *received = calloc(1, sizeof(bufferid_t) * read_requests);
    cycles_t t1 = bench_tsc();

    for (size_t i=0; i < read_requests; i++) {
        uint64_t disk_block = write_flag | i;
        do {
            err = devq_enqueue(dq, region_id, (i*block_size), 
                               block_size, 0,
                               block_size, disk_block);
            if (err_is_ok(err)) {
                break;
            }
            else if (err_no(err) == DEVQ_ERR_QUEUE_FULL) {
                receive_block();
            }
            else {
                USER_PANIC_ERR(err, "Can't receive block.");
            }
        } while (true);
    }

    // Make sure we have all requests:
    while (finish_counter < read_requests) {
        receive_block();
    }

    cycles_t t2 = bench_tsc();
    cycles_t result = (t2 - t1 - bench_tscoverhead());

    double result_ms = (double)bench_tsc_to_ms(result);
    double bw = buffer_size / result_ms / 1000; // 1e3 to sec, 10e6 to MB
    char* cmd = write ? "Write" : "Read";
    printf("[%s] %s sequential size %zu bs %zu: %.2f [MB/s]\n", __FUNCTION__, cmd, buffer_size, block_size, bw);

    err = devq_deregister(dq, region_id, &frame);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "devq_deregister failed.");
    }

    free(received);

    cap_destroy(frame);
}
void ahci_verify_sequential(size_t buffer_size, size_t block_size)
{
    finish_counter = 0;
    bench_init();
    errval_t err;
    assert(buffer_size % block_size == 0);

    size_t requests = buffer_size / block_size;
    regionid_t region_id = 0;

    struct capref frame;
    struct frame_identity id;
    frame_alloc_identify(buffer_size, &frame, &id);
    err = devq_register(dq, frame, &region_id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "devq register");
    }

    struct capref fcopy;
    err = slot_alloc(&fcopy);
    assert(err_is_ok(err));
    err = cap_copy(fcopy, frame);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "copy failed.");
    }
    void* retaddr;
    err = vspace_map_one_frame(&retaddr, id.bytes, fcopy, NULL, NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "map copy failed.");
    }

    uint8_t rbyte = (uint8_t) rdtsc();
    if (rbyte == 0) rbyte++;
    memset(retaddr, rbyte, buffer_size);

    uint64_t write_flag = (1ULL << 63);
    bufferid_t *received = calloc(1, sizeof(bufferid_t) * requests);
    for (size_t i=0; i < requests; i++) {
        uint64_t disk_block = write_flag | i;
        do {
            err = devq_enqueue(dq, region_id, (i*block_size), block_size, 
                               0, block_size, disk_block);
            if (err_is_ok(err)) {
                break;
            }
            else if (err_no(err) == DEVQ_ERR_QUEUE_FULL) {
                receive_block();
            }
            else {
                USER_PANIC_ERR(err, "Can't receive block.");
            }
        } while (true);
    }
    // Make sure we have all requests:
    while (finish_counter < requests) {
        receive_block();
    }

    memset(retaddr, 0x00, id.bytes);
    memset((void*)received, 0x0, sizeof(bufferid_t)*requests);
    finish_counter = 0;

    for (size_t i=0; i < requests; i++) {
        //printf("%s:%s:%d: i: %zu requests: %zu\n", __FILE__, __FUNCTION__, __LINE__, i, requests);
        uint64_t disk_block = i;
        do {
            err = devq_enqueue(dq, region_id, (i*block_size), 
                               block_size, 0, block_size,
                               disk_block);
            if (err_is_ok(err)) {
                break;
            }
            else if (err_no(err) == DEVQ_ERR_QUEUE_FULL) {
                receive_block();
            }
            else {
                USER_PANIC_ERR(err, "Can't receive block.");
            }
        } while (true);
    }

    // Make sure we have all requests:
    while (finish_counter < requests) {
        receive_block();
    }

    for (size_t i=0; i < buffer_size; i++) {
        uint8_t* carr = retaddr;
        if (carr[i] != rbyte) {
            printf("%s:%s:%d: carr[%zu]=%d != rbyte=%d\n", __FILE__, __FUNCTION__, __LINE__, i, carr[i], rbyte);
        }
        assert(carr[i] == rbyte);
    }

    printf("[%s] SUCCESS (%zu %zu)\n", __FUNCTION__, buffer_size, block_size);
    cap_destroy(fcopy);

    err = devq_deregister(dq, region_id, &frame);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "devq_deregister failed.");
    }
}
