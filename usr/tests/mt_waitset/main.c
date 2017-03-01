#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <if/mt_waitset_defs.h>
#include <if/mt_waitset_defs.h>
#include <barrelfish/deferred.h>
#include <barrelfish/inthandler.h>
#include <bench/bench.h>
#include <sys/time.h>
#include "../lib/barrelfish/include/threads_priv.h"
#include <barrelfish/debug.h>
#include <barrelfish/spawn_client.h>
#include <barrelfish/event_mutex.h>

const static char *service_name = "mt_waitset_service";
coreid_t my_core_id, num_cores;
struct thread *threads[256];

static int server_threads = 10;
static int client_threads = 1;
static int iteration_count = 1000;

static int client_counter = 0;
static int64_t server_calls[256];
static int64_t client_calls[256][256];

static void show_stats(void)
{
    debug_printf("Stats: %zd %zd %zd %zd %zd %zd %zd %zd %zd %zd\n",
        server_calls[0], server_calls[1], server_calls[2], server_calls[3],
        server_calls[4], server_calls[5], server_calls[6], server_calls[7],
        server_calls[8], server_calls[9]);
}

static void show_client_stats(void)
{
    int i, j, s;
    char text[256];

    for (i = 0; i < num_cores; i++) {
        s = sprintf(text, "Core %d:", i);
        for (j = 0; j < 16; j++)
            s += sprintf(text + s, "\t%zd", client_calls[i][j]);
        s += sprintf(text + s, "\n");
        debug_printf("%s", text);
    }
}

static int client_thread(void * arg)
{
    struct mt_waitset_binding *binding;
    errval_t err;
    binding = arg;
    int i, j, k, l;
    uint64_t payload[512];
    uint64_t result[512];
    size_t result_size;
    uint64_t o1;
    uint32_t o2;
    uint32_t i1 = my_core_id << 8 | thread_self()->id;
    uint64_t mmm = ((uint64_t)my_core_id << 56) | ((uint64_t)thread_self()->id << 48);

    debug_printf("Start\n");

    for (k = 0; k < iteration_count; k++) {
        uint64_t i2 = (rdtsc() & 0xffffffff) | mmm | (((uint64_t)k & 0xffffL) << 32);

        j = ((i2 >> 5) & 511) + 1;

        i2 &= 0xfffffffffffff000;

        for (i = 0; i < j; i++)
            payload[i] = i2 + i;
        err = binding->rpc_tx_vtbl.rpc_method(binding, i2, (uint8_t *)payload, 8 * j, i1, &o1, (uint8_t *)result, &result_size, &o2);

        assert(err == SYS_ERR_OK);
        l = 0;
        for (i = 0; i < j; i++) {
            if (result[i] == payload[i] + i)
                l++;
        }
        if (!(i2 + 1 == o1) || result_size != (8 * j) || l != j) {
            debug_printf("%d: wrong %016lx != %016lx  %d %zd    %d %d\n", k, i2 + 1, o1, 8 * j, result_size, j, l);
            for (i = 0; i < j; i++)
                debug_printf("\t%d: %016lx %016lx\n", i, payload[i], result[i]);
        }
        server_calls[o2]++;
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "error sending message\n");
        }
    }

    dispatcher_handle_t handle = disp_disable();

    client_counter--;
    debug_printf("Done, threads left:%d\n", client_counter);

    if (client_counter == 0) {
        disp_enable(handle);
        // all threads have finished, we're done, inform the server
        payload[0] = mmm;
        err = binding->rpc_tx_vtbl.rpc_method(binding, mmm, (uint8_t *)payload, 8, 65536, &o1, (uint8_t *)result, &result_size, &o2);
        show_stats();
    } else
        disp_enable(handle);
    return 0;
}

static void bind_cb(void *st, errval_t err, struct mt_waitset_binding *b)
{
    int i = (long int)st;

    mt_waitset_rpc_client_init(b);

    client_counter = client_threads;
    for (i = 1; i < client_threads; i++)
        thread_create(client_thread, b);
    client_thread(b);
}

static void start_client(void)
{
    char name[64];
    errval_t err;
    iref_t iref;

    debug_printf("Start client\n");
    sprintf(name, "%s%d", service_name, 0);
    err = nameservice_blocking_lookup(service_name, &iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_blocking_lookup failed");
    }
    err = mt_waitset_bind(iref, bind_cb,  (void *)0, get_default_waitset(), IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "bind failed");
    }
}


// server

static void export_cb(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export failed");
    }
    err = nameservice_register(service_name, iref);
    if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "nameservice_register failed");
    }
}

static errval_t server_rpc_method_call(struct mt_waitset_binding *b, uint64_t i1, const uint8_t *s, size_t ss, uint32_t i2, uint64_t *o1, uint8_t *r, size_t *rs, uint32_t *o2)
{
    int i, j, k, me;
    static int count = 0;
    static uint64_t calls = 0;
    uint64_t *response = (uint64_t *)r;

    for (i = 0;; i++) {
        if (thread_self() == threads[i]) {
            server_calls[i]++;
            me = i;
            break;
        }
    }

    if (i2 == 65536) {
        count++;    // client has finished
    } else
        client_calls[i2 >> 8][i2 & 255]++;

    j = ss / 8;
    k = 0;
    for (i = 0; i < j; i++) {
        response[i] = ((uint64_t *)s)[i];
        if (response[i] == i1 + i)
            k++;
        response[i] += i;
    }
    if (k != j && i2 != 65536)
        debug_printf("%s: binding:%p %08x %08x  %d %d   %016lx:%d\n", __func__, b, i2, b->incoming_token, k, j, response[0], me);
    if (count == num_cores) {
        bool failed = false;

        debug_printf("Final statistics\n");
        show_stats();
        show_client_stats();
        for (i = 0; i < num_cores; i++) {
            for (j = 0; j < client_threads; j++) {
                if (client_calls[i][j] != iteration_count) {
                    failed = true;
                    goto out;
                }
            }
        }
out:
        if (failed)
            debug_printf("Test FAILED\n");
        else
            debug_printf("Test PASSED\n");
    }
    calls++;
    if ((calls % 10000) == 0) {
        show_stats();
    }

    *o1 = i1 + 1;
    *rs = 8 * j;
    *o2 = me;

    return SYS_ERR_OK;
}

static struct mt_waitset_rpc_rx_vtbl rpc_rx_vtbl = {
    .rpc_method_call = server_rpc_method_call
};

static errval_t connect_cb(void *st, struct mt_waitset_binding *b)
{
    b->rpc_rx_vtbl = rpc_rx_vtbl;
    return SYS_ERR_OK;
}

static int run_server(void * arg)
{
    int i = (uintptr_t)arg;
    struct waitset *ws = get_default_waitset();
    errval_t err;


    debug_printf("Server dispatch loop %d\n", i);
    threads[i] = thread_self();

    for (;;) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
    }
    return SYS_ERR_OK;
}

static void start_server(void)
{
    struct waitset *ws = get_default_waitset();
    errval_t err;
    int i;

    debug_printf("Start server\n");

    err = mt_waitset_export(NULL, export_cb, connect_cb, ws,
                            IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export failed");
    }
    for (i = 1; i < server_threads; i++) {
        thread_create(run_server, (void *)(uintptr_t)i);
    }
}

int main(int argc, char *argv[])
{
    errval_t err;
    char *my_name = strdup(argv[0]);

    my_core_id = disp_get_core_id();

    memset(server_calls, 0, sizeof(server_calls));
    memset(client_calls, 0, sizeof(client_calls));

    if (argc == 1) {
        debug_printf("Usage: %s server_threads client_threads iteration_count\n", argv[0]);
    } else if (argc == 4) {
        char *xargv[] = {my_name, argv[2], argv[3], NULL};

        server_threads = atoi(argv[1]);
        client_threads = atoi(argv[2]);
        iteration_count = atoi(argv[3]);

        err = spawn_program_on_all_cores(true, xargv[0], xargv, NULL,
            SPAWN_FLAGS_DEFAULT, NULL, &num_cores);
        debug_printf("spawn program on all cores (%d)\n", num_cores);
        assert(err_is_ok(err));

        start_server();

        struct waitset *ws = get_default_waitset();

        threads[0] = thread_self();
        for (;;) {
            err = event_dispatch(ws);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "in event_dispatch");
                break;
            }
        }
    } else {
        client_threads = atoi(argv[1]);
        iteration_count = atoi(argv[2]);

        struct waitset *ws = get_default_waitset();
        start_client();
        debug_printf("Client process events\n");
        for (;;) {
            err = event_dispatch(ws);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "in event_dispatch");
                break;
            }
        }
    }
    return EXIT_FAILURE;
}
