#include "common.h"

#include <barrelfish/nameservice_client.h>
#include <ipv4/lwip/inet.h>

#include <bulk_transfer/bulk_local.h>
#include <bulk_transfer/bulk_net_proxy.h>
#include <bulk_transfer/bulk_sm.h>

#define BENCH_TRUST BULK_TRUST_FULL
//#define BENCH_TRUST BULK_TRUST_NONE

#define FORCE_NO_COPY_SCENARIO false

#define BENCH_NET_MAX_QUEUES 5
#define BENCH_NET_BUFFER_SIZE 0x1000
#define BENCH_NET_BUFFER_COUNT 512

bool is_no_copy = false;

/** Checks if the string s has a particular prefix */
static inline bool has_prefix(const char *s, const char *prefix)
{
    return !strncmp(s, prefix, strlen(prefix));
}


static void proxy_connected(struct bulk_net_proxy *p)
{
    bool *done = p->user_state;
    debug_printf("proxy_connected()\n");
    *done = true;
}

errval_t cb_bind_received(struct bulk_channel *channel)
{
    bool *done = channel->user_state;
    debug_printf("Bound: %p\n", done);
    if (done != NULL) {
        *done = true;
    }
    return SYS_ERR_OK;
}

/* TODO: these functions are leaking memory all over the place, but since we are
 * only using them for benchmark initializations. */

// npl:card:queue:port
static void net_proxy_listen(char *str, struct bulk_channel *channel,
        struct bulk_channel_callbacks *cb, struct waitset *ws,
        enum bulk_channel_direction dir,  size_t bufsz, size_t metasz,
        bool *done)
{
    errval_t err;
    struct bulk_net_proxy *proxy = calloc(1, sizeof(*proxy));
    struct bulk_local_endpoint *adesc = malloc(sizeof(*adesc));
    struct bulk_local_endpoint *pdesc = malloc(sizeof(*pdesc));
    struct bulk_channel_setup setup = {
        .direction = dir,
        .role = BULK_ROLE_MASTER,
        .trust = BENCH_TRUST,
        .meta_size = metasz,
        .waitset = ws,
    };

    // Parse string
    char *card = strtok(str, ":");
    uint8_t queue = atoi(strtok(NULL, ":"));
    uint16_t port = atoi(strtok(NULL, ":"));

    // Prepare local channel
    bulk_local_init_endpoint(adesc, NULL);
    err = bulk_channel_create(channel, &adesc->generic, cb, &setup);
    assert(err_is_ok(err));
    channel->user_state = NULL;

    // Initialize proxy
    bulk_local_init_endpoint(pdesc, channel);
    proxy->user_state = done;
    err = bulk_net_proxy_listen(proxy, &pdesc->generic, ws, bufsz,
            card, queue, port, proxy_connected);
    if (!err_is_ok(err)) {
        printf("err_string= %s\n", err_getstring(err));
        err_print_calltrace(err);
    }
}

// npc:card:queue:ip:port
static void net_proxy_connect(char *str, struct bulk_channel *channel,
        struct bulk_channel_callbacks *cb, struct waitset *ws,
        enum bulk_channel_direction dir,  size_t bufsz, size_t metasz,
        bool *done)
{
    errval_t err;
    struct bulk_net_proxy *proxy = calloc(1, sizeof(*proxy));
    struct bulk_local_endpoint *adesc = malloc(sizeof(*adesc));
    struct bulk_local_endpoint *pdesc = malloc(sizeof(*pdesc));
    struct bulk_channel_setup setup = {
        .direction = dir,
        .role = BULK_ROLE_MASTER,
        .trust = BENCH_TRUST,
        .meta_size = metasz,
        .waitset = ws,
    };

    // Parse string
    char *card = strtok(str, ":");
    uint8_t queue = atoi(strtok(NULL, ":"));
    uint32_t ip = ntohl(inet_addr(strtok(NULL, ":")));
    uint16_t port = atoi(strtok(NULL, ":"));

    // Prepare local channel
    bulk_local_init_endpoint(adesc, NULL);
    err = bulk_channel_create(channel, &adesc->generic, cb, &setup);
    assert(err_is_ok(err));
    channel->user_state = NULL;

    // Initialize proxy
    bulk_local_init_endpoint(pdesc, channel);
    proxy->user_state = done;
    err = bulk_net_proxy_connect(proxy, &pdesc->generic, ws, bufsz,
            card, queue, ip, port, proxy_connected);
    if (!err_is_ok(err)) {
        printf("err_string= %s\n", err_getstring(err));
        err_print_calltrace(err);
    }
}

// ntl:card:queue:port
static void net_transparent_listen(char *str, struct bulk_channel *channel,
        struct bulk_channel_callbacks *cb, struct waitset *ws,
        enum bulk_channel_direction dir,  size_t bufsz, size_t metasz,
        bool *done, bool nocopy)
{
    struct bulk_net_endpoint_descriptor *epd = malloc(sizeof(*epd));

    is_no_copy = nocopy;

    char *card_str = strtok(str, ":");
    char *queue_str = strtok(NULL, ":");
    char *port_str = strtok(NULL, ":");

    struct bulk_net_ep_setup epsetup = {
        .port = atoi(port_str),
        .queue = atoi(queue_str),
        .max_queues = BENCH_NET_MAX_QUEUES,
        .buffer_size = BENCH_NET_BUFFER_SIZE,
        .buffer_count = BENCH_NET_BUFFER_COUNT,
        .cardname = card_str,
        .no_copy = nocopy,
    };

    is_no_copy = nocopy || FORCE_NO_COPY_SCENARIO;

    expect_success(bulk_net_ep_create(epd, &epsetup));
    struct bulk_channel_setup setup = {
        .direction = dir,
        .role = (dir == BULK_DIRECTION_TX ? BULK_ROLE_MASTER : BULK_ROLE_SLAVE),
        .trust = BENCH_TRUST,
        .meta_size = metasz,
        .waitset = ws,
        .user_state = done,
    };
    channel->user_state = done;
    expect_success(bulk_channel_create(channel, &epd->ep_generic, cb, &setup));
}

static void bind_done_cb(void *arg, errval_t err, struct bulk_channel *channel)
{
    bool *done = arg;
    assert(err_is_ok(err));
    debug_printf("bind_done_cb()\n");
    *done = true;
}

// ntc:card:queue:ip:port
static void net_transparent_connect(char *str, struct bulk_channel *channel,
        struct bulk_channel_callbacks *cb, struct waitset *ws,
        enum bulk_channel_direction dir,  size_t bufsz, size_t metasz,
        bool *done, bool nocopy)
{
    struct bulk_net_endpoint_descriptor *epd = malloc(sizeof(*epd));

    is_no_copy = nocopy;

    char *card_str = strtok(str, ":");
    char *queue_str = strtok(NULL, ":");
    char *ip_str = strtok(NULL, ":");
    char *port_str = strtok(NULL, ":");

    struct bulk_net_ep_setup epsetup = {
        .ip.addr = inet_addr(ip_str),
        .port = atoi(port_str),
        .queue = atoi(queue_str),
        .max_queues = BENCH_NET_MAX_QUEUES,
        .buffer_size = BENCH_NET_BUFFER_SIZE,
        .buffer_count = BENCH_NET_BUFFER_COUNT,
        .cardname = card_str,
        .no_copy = nocopy,
    };

    expect_success(bulk_net_ep_create_remote(epd, &epsetup));
    struct bulk_channel_bind_params setup = {
        .role = (dir == BULK_DIRECTION_TX ? BULK_ROLE_MASTER : BULK_ROLE_SLAVE),
        .trust = BENCH_TRUST,
        .waitset = ws,
    };

    is_no_copy = nocopy || FORCE_NO_COPY_SCENARIO;

    expect_success(bulk_channel_bind(channel, &epd->ep_generic, cb, &setup,
            MK_BULK_CONT(bind_done_cb, done)));
}

// sml:name
static void sm_listen(char *str, struct bulk_channel *channel,
        struct bulk_channel_callbacks *cb, struct waitset *ws,
        enum bulk_channel_direction dir,  size_t bufsz, size_t metasz,
        bool *done)
{
    char *name = str;

    struct bulk_sm_endpoint_descriptor *epd = malloc(sizeof(*epd));
    expect_success(bulk_sm_ep_create(epd));
    struct bulk_channel_setup setup = {
        .direction = dir,
        .role = (dir == BULK_DIRECTION_TX ? BULK_ROLE_MASTER : BULK_ROLE_SLAVE),
        .trust = BENCH_TRUST,
        .meta_size = metasz,
        .waitset = ws,
        .user_state = done,
    };
    channel->user_state = done;
    expect_success(bulk_channel_create(channel, &epd->ep_generic, cb, &setup));
    expect_success(nameservice_register(name, epd->iref));
}

// smc:name
static void sm_connect(char *str, struct bulk_channel *channel,
        struct bulk_channel_callbacks *cb, struct waitset *ws,
        enum bulk_channel_direction dir,  size_t bufsz, size_t metasz,
        bool *done)
{
    char *name = str;

    struct bulk_sm_endpoint_descriptor *epd = malloc(sizeof(*epd));
    iref_t iref;

    expect_success(nameservice_blocking_lookup(name, &iref));
    expect_success(bulk_sm_ep_create_remote(epd, iref));

    struct bulk_channel_bind_params setup = {
        .role = (dir == BULK_DIRECTION_TX ? BULK_ROLE_MASTER : BULK_ROLE_SLAVE),
        .trust = BENCH_TRUST,
        .waitset = ws,
    };

    expect_success(bulk_channel_bind(channel, &epd->ep_generic, cb, &setup,
            MK_BULK_CONT(bind_done_cb, done)));
}


void initialize_channel(const char *str, struct bulk_channel *channel,
        struct bulk_channel_callbacks *cb, struct waitset *ws,
        enum bulk_channel_direction dir,  size_t bufsz, size_t metasz,
        bool *done)
{
    char *s = malloc(strlen(str) + 1);
    strcpy(s, str);

    memset(channel, 0, sizeof(*channel));

    if (has_prefix(s, "npl:")) {
        net_proxy_listen(s + 4, channel, cb, ws, dir, bufsz, metasz, done);
    } else if (has_prefix(s, "npc:")) {
        net_proxy_connect(s + 4, channel, cb, ws, dir, bufsz, metasz, done);
    } else if (has_prefix(s, "ntl:")) {
        net_transparent_listen(s + 4, channel, cb, ws, dir, bufsz, metasz,
                done, false);
    } else if (has_prefix(s, "ntc:")) {
        net_transparent_connect(s + 4, channel, cb, ws, dir, bufsz, metasz,
                done, false);
    } else if (has_prefix(s, "ncl:")) {
        net_transparent_listen(s + 4, channel, cb, ws, dir, bufsz, metasz,
                done, true);
    } else if (has_prefix(s, "ncc:")) {
        net_transparent_connect(s + 4, channel, cb, ws, dir, bufsz, metasz,
                done, true);
    } else if (has_prefix(s, "sml:")) {
        sm_listen(s + 4, channel, cb, ws, dir, bufsz, metasz, done);
    } else if (has_prefix(s, "smc:")) {
        sm_connect(s + 4, channel, cb, ws, dir, bufsz, metasz, done);
    } else {
        USER_PANIC("Invalid channel description prefix");
    }
}
