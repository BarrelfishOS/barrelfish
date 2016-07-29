#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <if/test_defs.h>

#define PANIC_IF_ERR(err, msg...) \
do { \
    if (err_is_fail(err)) { \
        printf("distops_retype: got %s, expected SYS_ERR_OK\n", err_getcode(err)); \
        USER_PANIC_ERR(err, msg); \
    } \
} while(0)

#define RAM_BITS LARGE_PAGE_BITS
#define RAM_SIZE (1UL << RAM_BITS)

static const char *service_name = "distops_test";

enum server_op {
    SERVER_OP_CREATE_COPY,
    // Create a BASE_PAGE_SIZEd Frame cap of last page in RAM
    SERVER_OP_RETYPE_1,
    // Exit server
    SERVER_OP_EXIT,
};

enum client_op {
    // do a retype on client while server holds copies of cap but no
    // descendants
    CLIENT_OP_RETYPE_1,
    // Do retype (overlapping and non-overlapping) on client while server
    // holds copies of cap and Frame of last page in region
    CLIENT_OP_RETYPE_2,
};

//{{{1 Unused message handlers
static void rx_str(struct test_binding *b, uint32_t arg, char *s)
{
    printf("rx_str %"PRIu32" '%s'\n", arg, s);
    free(s);
}

static void rx_buf(struct test_binding *b, uint8_t *buf, size_t buflen)
{
    printf("rx_buf buflen=%zu\n", buflen);
    free(buf);
}

//{{{1 Server-side cap operations

struct server_state {
    struct capref ram;
    struct capref copy;
    struct capref frame1;
};

__attribute__((unused))
static void debug_capref(const char *prefix, struct capref cap)
{
    char buf[128];
    debug_print_capref(buf, 128, cap);
    printf("%s capref = %s\n", prefix, buf);
}

static void server_do_test(struct test_binding *b, uint32_t test, struct capref cap)
{
    errval_t err;
    struct server_state *st = b->st;

    switch(test) {
        case SERVER_OP_CREATE_COPY:
            printf("server: creating a copy\n");
            err = slot_alloc(&st->copy);
            PANIC_IF_ERR(err, "slot alloc for copy in server");

            err = cap_copy(st->copy, cap);
            PANIC_IF_ERR(err, "copy in server");

            err = test_basic__tx(b, NOP_CONT, CLIENT_OP_RETYPE_1);
            PANIC_IF_ERR(err, "server: triggering 1st retype on client");
            break;

        case SERVER_OP_RETYPE_1:
            printf("server: retype #1\n");
            err = slot_alloc(&st->frame1);
            PANIC_IF_ERR(err, "slot alloc for retype in server");

            err = cap_retype(st->frame1, st->copy, RAM_SIZE - BASE_PAGE_SIZE,
                             ObjType_Frame, BASE_PAGE_SIZE, 1);
            PANIC_IF_ERR(err, "retype last page in server");

            printf("server: triggering 2nd set of retypes in client\n");
            err = test_basic__tx(b, NOP_CONT, CLIENT_OP_RETYPE_2);
            PANIC_IF_ERR(err, "server: triggering 2nd set of retypes on client");
            break;

        case SERVER_OP_EXIT:
            printf("server: exit\n");
            exit(0);

        default:
            USER_PANIC("server: Unknown test %"PRIu32"\n", test);
    }
}

//{{{1 Server-side message handlers

static void server_rx_basic(struct test_binding *b, uint32_t arg)
{
    printf("server rx_basic %"PRIu32"\n", arg);
    server_do_test(b, arg, NULL_CAP);
}

static void server_rx_caps(struct test_binding *b, uint32_t arg, struct capref cap1,
        struct capref cap2)
{
    struct server_state *st = b->st;
    st->ram = cap1;
    server_do_test(b, arg, cap1);
}

static struct test_rx_vtbl server_rx_vtbl = {
    .basic = server_rx_basic,
    .str = rx_str,
    .caps = server_rx_caps,
    .buf = rx_buf,
};

//{{{1 Server

static void export_cb(void *st, errval_t err, iref_t iref)
{
    PANIC_IF_ERR(err, "export failed");

    printf("service exported at iref %"PRIuIREF"\n", iref);

    // register this iref with the name service
    err = nameservice_register(service_name, iref);
    PANIC_IF_ERR(err, "nameservice_register failed");
}

static errval_t connect_cb(void *st, struct test_binding *b)
{
    printf("service got a connection!\n");

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = server_rx_vtbl;

    // Create a server state struct for this connection
    b->st = malloc(sizeof(struct server_state));
    assert(b->st);

    // accept the connection (we could return an error to refuse it)
    return SYS_ERR_OK;
}

static void run_server(void)
{
    errval_t err;

    err = test_export(NULL /* state pointer for connect/export callbacks */,
            export_cb, connect_cb, get_default_waitset(),
            IDC_EXPORT_FLAGS_DEFAULT);
    PANIC_IF_ERR(err, "export failed");
}

//{{{1 Tests
struct client_state {
    struct capref ram;
};

static void client_test_retype(struct capref src, gensize_t offset,
                               gensize_t size, size_t count, errval_t expected_err)
{
    errval_t err;
    struct capref result;
    err = slot_alloc(&result);
    PANIC_IF_ERR(err, "in client: slot_alloc for retype result");

    // Tests come here
    err = cap_retype(result, src, offset, ObjType_Frame, size, count);
    if (err != expected_err) {
        printf("in client: retype(offset=%"PRIuGENSIZE", size=%"PRIuGENSIZE
                     ", count=%zu) to Frame returned %s, expected %s\n",
                     offset, size, count, err_getcode(err), err_getcode(expected_err));
    }
    if (err_is_ok(err)) {
        // Cap delete only necessary if retype successful
        err = cap_delete(result);
        PANIC_IF_ERR(err, "cap_delete in client_test_retype");
    }
    err = slot_free(result);
    PANIC_IF_ERR(err, "slot_free in client_test_retype");
}

static void client_do_test(struct test_binding *b, uint32_t test, struct capref cap)
{
    errval_t err;
    struct client_state *st = b->st;
    assert(st);

    switch(test) {
        // Do retype while copies exist on other core
        case CLIENT_OP_RETYPE_1:
            printf("client: retype #1\n");
            printf("   retype first page: should succeed\n");
            client_test_retype(st->ram, 0, BASE_PAGE_SIZE, 1, SYS_ERR_OK);
            err = test_basic__tx(b, NOP_CONT, SERVER_OP_RETYPE_1);
            PANIC_IF_ERR(err, "client: trigger first retype on server");
            break;

        case CLIENT_OP_RETYPE_2:
            printf("client: retype #2\n");
            printf("   retype last page: should fail\n");
            client_test_retype(st->ram, RAM_SIZE - BASE_PAGE_SIZE, BASE_PAGE_SIZE, 1,
                    SYS_ERR_REVOKE_FIRST);

            printf("   retype first page: should succeed\n");
            client_test_retype(st->ram, 0, BASE_PAGE_SIZE, 1, SYS_ERR_OK);

            printf("   retype 2nd last page: should succeed\n");
            client_test_retype(st->ram, RAM_SIZE - 2*BASE_PAGE_SIZE,
                               BASE_PAGE_SIZE, 1, SYS_ERR_OK);

            err = test_basic__tx(b, NOP_CONT, SERVER_OP_EXIT);
            PANIC_IF_ERR(err, "client: trigger exit on server");
            printf("distops_retype: client done\n");
            exit(0);

        default:
            USER_PANIC("client: Unknown test %"PRIu32"\n", test);
    }
}

//{{{1 Client-side message handlers

// We use test.basic to signal that server has done it's retypes
static void client_rx_basic(struct test_binding *b, uint32_t arg)
{
    printf("client rx_basic %"PRIu32": b->st = %p\n", arg, b->st);

    client_do_test(b, arg, NULL_CAP);
}

static void client_rx_caps(struct test_binding *b, uint32_t arg, struct capref cap1,
        struct capref cap2)
{
    printf("client rx_caps: arg=%"PRIu32"\n", arg);
    cap_destroy(cap1);
    cap_destroy(cap2);
}

static struct test_rx_vtbl client_rx_vtbl = {
    .basic = client_rx_basic,
    .str = rx_str,
    .caps = client_rx_caps,
    .buf = rx_buf,
};

//{{{1 Client

static void bind_cb(void *st, errval_t err, struct test_binding *b)
{
    PANIC_IF_ERR(err, "bind failed");

    printf("client bound!\n");

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = client_rx_vtbl;

    // allocate a client state struct
    b->st = malloc(sizeof(struct client_state));
    assert(b->st);
    struct client_state *cst = b->st;

    err = ram_alloc(&cst->ram, RAM_BITS);
    PANIC_IF_ERR(err, "in client: ram_alloc");

    err = test_caps__tx(b, NOP_CONT, SERVER_OP_CREATE_COPY, cst->ram, NULL_CAP);
    PANIC_IF_ERR(err, "in client: test_caps__tx");
}

static void run_client(void)
{
    errval_t err;
    iref_t iref;

    printf("client looking up '%s' in name service...\n", service_name);
    err = nameservice_blocking_lookup(service_name, &iref);
    PANIC_IF_ERR(err, "nameservice_blocking_lookup failed");

    printf("client binding to %"PRIuIREF"...\n", iref);

    err = test_bind(iref, bind_cb, NULL, get_default_waitset(), IDC_BIND_FLAGS_DEFAULT);
    PANIC_IF_ERR(err, "bind failed");
}

//{{{1 Main
int main(int argc, char *argv[])
{
    char *role;
    if (argc < 2) {
        printf("remote retype test needs an argument of \"server\" or \"client\"\n");
    }
    if (strncmp(argv[1], "server", 6) == 0) {
        printf("we are the server\n");
        role = "server";
        run_server();
    }
    if (strncmp(argv[1], "client", 6) == 0) {
        printf("we are the client\n");
        role = "client";
        run_client();
    }

    errval_t err;
    struct waitset *ws = get_default_waitset();
    while (true) {
        err = event_dispatch(ws);
        PANIC_IF_ERR(err, "in %s: event_dispatch", role);
    }

    return 0;
}
