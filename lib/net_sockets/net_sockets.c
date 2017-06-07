#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <if/net_sockets_defs.h>
#include <net_sockets/net_sockets.h>

#include <barrelfish/waitset_chan.h>
#include <barrelfish/waitset.h>

#include <devif/queue_interface.h>
#include <devif/backends/descq.h>


static struct net_sockets_binding *binding;
static bool bound_done = false;

static struct capref buffer_frame;
struct descq* descq_queue;
static void *buffer_start;
static regionid_t regionid;
static uint64_t queue_id;

#define NO_OF_BUFFERS 128
#define BUFFER_SIZE 16384

void *buffers[NO_OF_BUFFERS];
uint64_t next_free, next_used;
struct net_socket *sockets = NULL;

/// Dequeue the element from the net_socket queue
static void dequeue(struct net_socket **queue,
                            struct net_socket *element)
{
    if (element->next == element) {
        assert(element->prev == element);
        assert(*queue == element);
        *queue = NULL;
    } else {
        element->prev->next = element->next;
        element->next->prev = element->prev;
        if (*queue == element) {
            *queue = element->next;
        }
    }
    element->prev = element->next = NULL;
}

/// Enqueue the element on the net_socket queue
static void enqueue(struct net_socket **queue,
                            struct net_socket *element)
{
    if (*queue == NULL) {
        *queue = element;
        element->next = element->prev = element;
    } else {
        element->next = *queue;
        element->prev = (*queue)->prev;
        element->next->prev = element;
        element->prev->next = element;
    }
}

struct net_socket * net_udp_socket(void)
{
    errval_t err;
    struct net_socket *socket;
    uint32_t descriptor;

    err = binding->rpc_tx_vtbl.new_udp_socket(binding, &descriptor);
    assert(err_is_ok(err));

    socket = malloc(sizeof(struct net_socket));
    assert(socket);

    socket->descriptor = descriptor;
    socket->received = NULL;
    socket->connected = NULL;
    socket->accepted = NULL;
    socket->user_state = NULL;
    enqueue(&sockets, socket);
    return socket;
}

struct net_socket * net_tcp_socket(void)
{
    errval_t err;
    struct net_socket *socket;
    uint32_t descriptor;

    err = binding->rpc_tx_vtbl.new_tcp_socket(binding, &descriptor);
    assert(err_is_ok(err));

    socket = malloc(sizeof(struct net_socket));
    assert(socket);

    socket->descriptor = descriptor;
    socket->received = NULL;
    socket->sent = NULL;
    socket->connected = NULL;
    socket->accepted = NULL;
    socket->user_state = NULL;
    enqueue(&sockets, socket);
    return socket;
}

static struct net_socket * get_socket(uint32_t descriptor)
{
    struct net_socket *socket = sockets;
    
    while (socket) {
        if (socket->descriptor == descriptor)
            return socket;
        socket = socket->next;
        if (socket == sockets)
            break;
    }
    assert(0);
    return NULL;
}

void net_set_user_state(struct net_socket *socket, void *user_state)
{
    socket->user_state = user_state;
}

void net_close(struct net_socket *socket)
{
    errval_t err, error;

    err = binding->rpc_tx_vtbl.delete_socket(binding, socket->descriptor, &error);
    assert(err_is_ok(err));
    assert(err_is_ok(error));
    dequeue(&sockets, socket);
    free(socket);
}

errval_t net_bind(struct net_socket *socket, host_address_t ip_address, uint16_t port)
{
    errval_t err, error;

    err = binding->rpc_tx_vtbl.bind(binding, socket->descriptor, ip_address, port, &error);
    assert(err_is_ok(err));

    return error;
}

errval_t net_listen(struct net_socket *socket, uint8_t backlog)
{
    errval_t err, error;

    err = binding->rpc_tx_vtbl.listen(binding, socket->descriptor, backlog, &error);
    assert(err_is_ok(err));

    return error;
}

void * net_alloc(size_t size)
{
    void *buffer = buffers[next_free];
    assert(buffer);
    buffers[next_free] = NULL;
    next_free = (next_free + 1) % NO_OF_BUFFERS;
    // debug_printf("%s: %ld:%p  %ld:%p\n", __func__, next_free, buffers[next_free], next_used, buffers[next_used]);
    return buffer + sizeof(struct net_buffer);
}

void net_free(void *buffer)
{
    assert(!buffers[next_used]);
    buffers[next_used] = buffer - sizeof(struct net_buffer);
    next_used = (next_used + 1) % NO_OF_BUFFERS;
    // debug_printf("%s: %ld:%p  %ld:%p\n", __func__, next_free, buffers[next_free], next_used, buffers[next_used]);
}

errval_t net_send(struct net_socket *socket, void *data, size_t size)
{
    errval_t err, error;

    void *buffer = data - sizeof(struct net_buffer);
    struct net_buffer *nb = buffer;
    // debug_printf("%s(%d): %ld -> %p\n", __func__, descriptor, size, buffer);

    nb->size = size;
    nb->descriptor = socket->descriptor;
    nb->host_address = 0;
    nb->port = 0;
    // debug_printf("%s: enqueue 2 %lx:%ld\n", __func__, buffer - buffer_start, sizeof(struct net_buffer) + size);
    err = devq_enqueue((struct devq *)descq_queue, regionid, buffer - buffer_start, sizeof(struct net_buffer) + size,
                       0, 0, 2);
    assert(err_is_ok(err));
    err = devq_notify((struct devq *)descq_queue);
    assert(err_is_ok(err));

    error = SYS_ERR_OK;
    return error;
}

errval_t net_send_to(struct net_socket *socket, void *data, size_t size, host_address_t ip_address, uint16_t port)
{
    errval_t err, error;

    void *buffer = data - sizeof(struct net_buffer);
    struct net_buffer *nb = buffer;
    // debug_printf("%s(%d): %ld -> %p\n", __func__, descriptor, size, buffer);

    nb->size = size;
    nb->descriptor = socket->descriptor;
    nb->host_address = ip_address;
    nb->port = port;
    // debug_printf("%s: enqueue 2 %lx:%ld\n", __func__, buffer - buffer_start, sizeof(struct net_buffer) + size);
    err = devq_enqueue((struct devq *)descq_queue, regionid, buffer - buffer_start, sizeof(struct net_buffer) + size,
                       0, 0, 2);
    assert(err_is_ok(err));
    err = devq_notify((struct devq *)descq_queue);
    assert(err_is_ok(err));

    error = SYS_ERR_OK;
    return error;
}

errval_t net_connect(struct net_socket *socket, host_address_t ip_address, uint16_t port, net_connected_callback_t cb)
{
    errval_t err, error;

    socket->connected = cb;
    err = binding->rpc_tx_vtbl.connect(binding, socket->descriptor, ip_address, port, &error);
    assert(err_is_ok(err));
    assert(err_is_ok(error));

    return error;
}

static void net_connected(struct net_sockets_binding *b, uint32_t descriptor, errval_t error)
{
    struct net_socket *socket = get_socket(descriptor);
    assert(socket->descriptor == descriptor);
    assert(err_is_ok(error));

    assert(socket->connected);
    socket->connected(socket->user_state, socket);
}

void net_accept(struct net_socket *socket, net_accepted_callback_t cb)
{
    socket->accepted = cb;
}

static void net_accepted(struct net_sockets_binding *b, uint32_t descriptor,
    uint32_t accepted_descriptor, uint32_t host_address, uint16_t port, errval_t error)
{
    struct net_socket *socket = get_socket(descriptor);
    assert(socket->descriptor == descriptor);
    assert(err_is_ok(error));

    struct net_socket *accepted_socket = malloc(sizeof(struct net_socket));
    assert(accepted_socket);

    accepted_socket->descriptor = accepted_descriptor;
    accepted_socket->received = NULL;
    accepted_socket->sent = NULL;
    accepted_socket->connected = NULL;
    accepted_socket->accepted = NULL;
    accepted_socket->user_state = NULL;
    enqueue(&sockets, accepted_socket);

    assert(socket->accepted);
    socket->accepted(socket->user_state, accepted_socket, host_address, port);
}


void net_recv(struct net_socket *socket, net_received_callback_t cb)
{
    socket->received = cb;
}

void net_set_sent(struct net_socket *socket, net_sent_callback_t cb)
{
    socket->sent = cb;
}

static void bind_cb(void *st, errval_t err, struct net_sockets_binding *b)
{
    binding = b;
    net_sockets_rpc_client_init(binding);
    bound_done = true;
}

static void alloc_mem(struct capref *frame, void** virt, size_t size)
{
    errval_t r;
    vregion_flags_t flags;

    r = frame_alloc(frame, size, NULL);
    assert(err_is_ok(r));

    flags = VREGION_FLAGS_READ_WRITE;
    r = vspace_map_one_frame_attr(virt, size, *frame, flags, NULL, NULL);
    assert(err_is_ok(r));
    memset(*virt, 0, size);
}

static errval_t q_notify(struct descq* q)
{
    assert(descq_queue == q);
    errval_t err = SYS_ERR_OK;
    //errval_t err2 = SYS_ERR_OK;
    regionid_t rid;
    genoffset_t offset;
    genoffset_t length;
    genoffset_t valid_data;
    genoffset_t valid_length;
    uint64_t flags;
    bool notify = 0;

    // debug_printf("%s: \n", __func__);
    for (;;) {
        err = devq_dequeue((struct devq *)descq_queue, &rid, &offset, &length,
                           &valid_data, &valid_length, &flags);
        if (err_is_fail(err)) {
            break;
        } else {
            void *buffer = buffer_start + offset;
            struct net_buffer *nb = buffer;
            struct net_socket *socket = get_socket(nb->descriptor);
            void *shb_data = buffer + sizeof(struct net_buffer);

            // debug_printf("%s: dequeue %lx:%ld %ld  %p socket:%p\n", __func__, offset, length, flags, nb, socket);
            if (flags == 1) { // receiving buffer
                // debug_printf("%s: enqueue 1> %lx:%d\n", __func__, offset, nb->size);
                if (socket->received)
                    socket->received(socket->user_state, socket, shb_data, nb->size, nb->host_address, nb->port);
                // debug_printf("%s: enqueue 1< %lx:%d\n", __func__, offset, 2048);

                err = devq_enqueue((struct devq *)descq_queue, rid, offset, 2048, 0, 0, 1);
                assert(err_is_ok(err));
                notify = 1;
            } else if (flags == 2) { // transmitting buffer
                if (socket->sent)
                    socket->sent(socket->user_state, socket, shb_data);
    // debug_printf("%s: %ld:%p  %ld:%p\n", __func__, next_free, buffers[next_free], next_used, buffers[next_used]);
                // assert(!buffers[next_used]);
                // buffers[next_used] = buffer_start + offset;
                // next_used = (next_used + 1) % NO_OF_BUFFERS;
            }
        }
    }

    if (notify) {
        // debug_printf("notify>\n");
        err = devq_notify((struct devq *)descq_queue);
        assert(err_is_ok(err));
        // debug_printf("notify<\n");
    }

    return SYS_ERR_OK;
}

errval_t net_sockets_init(void)
{
    errval_t err;
    iref_t iref;

    memset(buffers, 0, sizeof(buffers));
    next_free = 0;
    next_used = 0;

    alloc_mem(&buffer_frame, &buffer_start, 2 * BUFFER_SIZE * NO_OF_BUFFERS);
 
    struct descq_func_pointer f;
    f.notify = q_notify;

    debug_printf("Descriptor queue test started \n");
    err = descq_create(&descq_queue, DESCQ_DEFAULT_SIZE, "net_sockets_queue",
                       false, true, true, &queue_id, &f);
    assert(err_is_ok(err));

    err = nameservice_blocking_lookup("net_sockets", &iref);
    assert(err_is_ok(err));
    err = net_sockets_bind(iref, bind_cb, NULL, get_default_waitset(), IDC_BIND_FLAGS_DEFAULT);
    assert(err_is_ok(err));

    while (!bound_done) {
        event_dispatch(get_default_waitset());
    }
    debug_printf("%s: initialized\n", __func__);
    binding->rx_vtbl.connected = net_connected;
    binding->rx_vtbl.accepted = net_accepted;

    err = binding->rpc_tx_vtbl.register_queue(binding, queue_id);
    assert(err_is_ok(err));

    err = devq_register((struct devq *)descq_queue, buffer_frame, &regionid);
    assert(err_is_ok(err));

    for (int i = 0; i < NO_OF_BUFFERS; i++) {
        err = devq_enqueue((struct devq *)descq_queue, regionid, i * BUFFER_SIZE, BUFFER_SIZE,
                           0, 0, 1);
        if (!err_is_ok(err))
            debug_printf("%s: %d:%d\n", __func__, i, NO_OF_BUFFERS);
        assert(err_is_ok(err));
        buffers[i] = i * BUFFER_SIZE + buffer_start + BUFFER_SIZE * NO_OF_BUFFERS;
    }

    err = devq_notify((struct devq *)descq_queue);
    assert(err_is_ok(err));

    return SYS_ERR_OK;
}
