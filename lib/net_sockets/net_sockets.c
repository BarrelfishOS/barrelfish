#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <if/net_sockets_defs.h>
#include <net_sockets/net_sockets.h>
#include <arpa/inet.h>

#include <barrelfish/waitset_chan.h>
#include <barrelfish/waitset.h>
#include <barrelfish/deferred.h>
#include <queue_service/client.h>

#include <if/octopus_defs.h>
#include <octopus/octopus.h>
#include <octopus/getset.h>

#include <devif/queue_interface.h>
#include <devif/backends/descq.h>
#include <debug_log/debug_log.h>

static struct net_sockets_binding *binding;
static bool bound_done = false;

static struct capref buffer_frame;
struct descq* descq_queue;
static void *buffer_start;
static regionid_t regionid;
static uint64_t queue_id;
static struct capref ep;

#define NO_OF_BUFFERS 128
#define BUFFER_SIZE 16384
#define NETSOCKET_LOOP_ITER 100
#define NUM_TRY_TIMEOUT 20

#define NAMESERVICE_ENTRY "r'net\\_sockets\\_service\\_.*' { iref: _ }"

void *buffers[NO_OF_BUFFERS];
uint64_t next_free, next_used;
struct net_socket *sockets = NULL;

static struct queue_service_client* qs_cl;

//#define DEBUG_MODE

#if defined(DEBUG_MODE) 
#define DEBUG_NETSOCK(x...) do { printf("lib_netsocket:%s.%d:%s:%d: ", \
            disp_name(), disp_get_core_id(), __func__, __LINE__); \
                printf(x);\
        } while (0)
#else
#define DEBUG_NETSOCK(x...) ((void)0)
#endif 

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

static struct net_socket * allocate_socket(uint32_t descriptor)
{
    struct net_socket *socket;

    socket = calloc(1, sizeof(struct net_socket));
    assert(socket);

    socket->descriptor = descriptor;
    socket->is_closing = false;
    socket->received = NULL;
    socket->connected = NULL;
    socket->accepted = NULL;
    socket->user_state = NULL;
    socket->bound_address.s_addr = 0;
    socket->bound_port = 0;
    socket->connected_address.s_addr = 0;
    socket->connected_port = 0;
    enqueue(&sockets, socket);
    return socket;
}

struct net_socket * net_udp_socket(void)
{
    errval_t err;
    struct net_socket *socket;
    uint32_t descriptor;

    err = binding->rpc_tx_vtbl.new_udp_socket(binding, &descriptor);
    assert(err_is_ok(err));

    socket = allocate_socket(descriptor);
    return socket;
}

struct net_socket * net_tcp_socket(void)
{
    errval_t err;
    struct net_socket *socket;
    uint32_t descriptor;

    err = binding->rpc_tx_vtbl.new_tcp_socket(binding, &descriptor);
    assert(err_is_ok(err));

    socket = allocate_socket(descriptor);
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
    return NULL;
}

void net_set_user_state(struct net_socket *socket, void *user_state)
{
    socket->user_state = user_state;
}

void net_set_on_closed(struct net_socket *socket, net_closed_callback_t cb)
{
    socket->closed = cb;
}

void net_close(struct net_socket *socket)
{
    errval_t err;

// DEBUG_NETSOCK_to_log("%s(%d): %ld:%p  %ld:%p", __func__, socket->descriptor, next_free, buffers[next_free], next_used, buffers[next_used]);
// DEBUG_NETSOCK("%s(%d): %d\n", __func__, socket->descriptor, socket->is_closing);
    if (socket->is_closing)
        debug_print_log();
    assert(!socket->is_closing);

    struct net_buffer *nb = net_alloc(0) - sizeof(struct net_buffer);
    nb->descriptor = socket->descriptor;
    socket->is_closing = true;
// DEBUG_NETSOCK_to_log("%s(%d): enqueue %lx:%ld", __func__, socket->descriptor, (void *)nb - buffer_start, sizeof(struct net_buffer));
    err = devq_enqueue((struct devq *)descq_queue, regionid, (void *)nb - buffer_start, sizeof(struct net_buffer),
                       0, 0, NET_EVENT_CLOSE);
    assert(err_is_ok(err));
    err = devq_notify((struct devq *)descq_queue);
    assert(err_is_ok(err));
}

errval_t net_bind(struct net_socket *socket, struct in_addr ip_address, uint16_t port)
{
    errval_t err, error;
    uint16_t bound_port;

    err = binding->rpc_tx_vtbl.bind(binding, socket->descriptor, ip_address.s_addr, port, &error, &bound_port);
    assert(err_is_ok(err));
    socket->bound_address = ip_address;
    socket->bound_port = bound_port;

    return error;
}

errval_t net_listen(struct net_socket *socket, uint8_t backlog)
{
    errval_t err, error;

    err = binding->rpc_tx_vtbl.listen(binding, socket->descriptor, backlog, &error);
    assert(err_is_ok(err));

    return error;
}

errval_t net_print_log(void)
{
    errval_t err, error;

    err = binding->rpc_tx_vtbl.listen(binding, -1, 0, &error);
    assert(err_is_ok(err));

    return error;
}

void * net_alloc(size_t size)
{
    assert(size < (BUFFER_SIZE - sizeof(struct net_buffer)));
    void *buffer = buffers[next_free];
    if (!buffer) {
        // DEBUG_NETSOCK_to_log("%s: %ld:%p  %ld:%p", __func__, next_free, buffers[next_free], next_used, buffers[next_used]);
        errval_t err, error;
        err = binding->rpc_tx_vtbl.listen(binding, -1, 0, &error);
        debug_print_log();
        assert(0);
    }
    assert(buffer);
    buffers[next_free] = NULL;
    next_free = (next_free + 1) % NO_OF_BUFFERS;
    // DEBUG_NETSOCK("%s: %p:%zd  %ld:%p  %ld:%p  %p\n", __func__, buffer + sizeof(struct net_buffer), size, next_free, buffers[next_free], next_used, buffers[next_used], __builtin_return_address(0));
    return buffer + sizeof(struct net_buffer);
}

void net_free(void *buffer)
{
    assert(!buffers[next_used]);
    buffers[next_used] = buffer - sizeof(struct net_buffer);
    next_used = (next_used + 1) % NO_OF_BUFFERS;
    // DEBUG_NETSOCK("%s: %p  %ld:%p  %ld:%p  %p\n", __func__, buffer, next_free, buffers[next_free], next_used, buffers[next_used], __builtin_return_address(0));
}

errval_t net_send(struct net_socket *socket, void *data, size_t size)
{
    errval_t err, error;

    void *buffer = data - sizeof(struct net_buffer);
    struct net_buffer *nb = buffer;
    // DEBUG_NETSOCK("%s(%d): %ld -> %p\n", __func__, socket->descriptor, size, buffer);
    if (socket->is_closing)
        debug_print_log();
    assert(!socket->is_closing);

    nb->size = size;
    nb->descriptor = socket->descriptor;
    nb->host_address.s_addr = INADDR_NONE;
    nb->port = 0;
// DEBUG_NETSOCK_to_log("%s(%d): enqueue %lx:%ld", __func__, socket->descriptor, buffer - buffer_start, sizeof(struct net_buffer) + size);
    err = devq_enqueue((struct devq *)descq_queue, regionid, buffer - buffer_start, sizeof(struct net_buffer) + size,
                       0, 0, NET_EVENT_SEND);
    assert(err_is_ok(err));
    err = devq_notify((struct devq *)descq_queue);
    assert(err_is_ok(err));

    error = SYS_ERR_OK;
    return error;
}

errval_t net_send_to(struct net_socket *socket, void *data, size_t size, struct in_addr ip_address, uint16_t port)
{
    errval_t err, error;

    void *buffer = data - sizeof(struct net_buffer);
    struct net_buffer *nb = buffer;
    // DEBUG_NETSOCK("%s(%d): %ld -> %p\n", __func__, descriptor, size, buffer);

    nb->size = size;
    nb->descriptor = socket->descriptor;
    nb->host_address = ip_address;
    nb->port = port;
    // DEBUG_NETSOCK_to_log("%s: enqueue %ld  %lx:%ld\n", __func__, size, buffer - buffer_start, sizeof(struct net_buffer) + size);
    err = devq_enqueue((struct devq *)descq_queue, regionid, buffer - buffer_start, sizeof(struct net_buffer) + size,
                       0, 0, NET_EVENT_SEND);
    assert(err_is_ok(err));
    err = devq_notify((struct devq *)descq_queue);
    assert(err_is_ok(err));

    error = SYS_ERR_OK;
    return error;
}

errval_t net_connect(struct net_socket *socket, struct in_addr ip_address, uint16_t port, net_connected_callback_t cb)
{
    errval_t err, error;

    socket->connected = cb;
    err = binding->rpc_tx_vtbl.connect(binding, socket->descriptor, ip_address.s_addr, port, &error);
    assert(err_is_ok(err));
    assert(err_is_ok(error));

    return error;
}

static void net_connected(struct net_sockets_binding *b, uint32_t descriptor, errval_t error, uint32_t connected_address, uint16_t connected_port)
{
    struct net_socket *socket = get_socket(descriptor);
    if (!socket) {
        DEBUG_NETSOCK("%s: socket not found %d\n", __func__, descriptor);
        // debug_print_log();
        assert(0);
    }
    assert(socket->descriptor == descriptor);
    assert(err_is_ok(error));

    socket->connected_address.s_addr = connected_address;
    socket->connected_port = connected_port;
    assert(socket->connected);
// DEBUG_NETSOCK_to_log("%s: %ld:%p  %ld:%p", __func__, next_free, buffers[next_free], next_used, buffers[next_used]);
    socket->connected(socket->user_state, socket);
}

void net_set_on_accepted(struct net_socket *socket, net_accepted_callback_t cb)
{
    socket->accepted = cb;
}

static void net_accepted(uint32_t descriptor, uint32_t accepted_descriptor, struct in_addr host_address, uint16_t port)
{
    struct net_socket *socket = get_socket(descriptor);
    if (!socket) {
        DEBUG_NETSOCK("%s: socket not found %d\n", __func__, descriptor);
        // debug_print_log();
        assert(0);
    }
    assert(socket->descriptor == descriptor);

    struct net_socket *accepted_socket = allocate_socket(accepted_descriptor);
    accepted_socket->connected_address = host_address;
    accepted_socket->connected_port = port;
// DEBUG_NETSOCK_to_log("%s(%d): %ld:%p  %ld:%p", __func__, accepted_descriptor, next_free, buffers[next_free], next_used, buffers[next_used]);
    socket->accepted(socket->user_state, accepted_socket);
}


void net_set_on_received(struct net_socket *socket, net_received_callback_t cb)
{
    socket->received = cb;
}

void net_set_on_sent(struct net_socket *socket, net_sent_callback_t cb)
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
    uint64_t event;
    bool notify = 0;

    // DEBUG_NETSOCK("%s: \n", __func__);
    for (int i = 0; i < NETSOCKET_LOOP_ITER; i++) {
        err = devq_dequeue((struct devq *)descq_queue, &rid, &offset, &length,
                           &valid_data, &valid_length, &event);
        if (err_is_fail(err)) {
            break;
        } else {
            void *buffer = buffer_start + offset;
            struct net_buffer *nb = buffer;
            // DEBUG_NETSOCK_to_log("%s: dequeue %lx:%ld %ld  %d:%d", __func__, offset, length, event, nb->descriptor, nb->size);
            // DEBUG_NETSOCK("%s: dequeue %lx:%ld %ld  %p socket:%d asocket:%d\n", __func__, offset, length, event, nb, nb->descriptor, nb->accepted_descriptor);
            void *shb_data = buffer + sizeof(struct net_buffer);

            if (event == NET_EVENT_RECEIVED) { // receiving buffer
                // DEBUG_NETSOCK("%s: enqueue 1> %lx:%d\n", __func__, offset, nb->size);
                struct net_socket *socket = get_socket(nb->descriptor);
                if (socket && !socket->is_closing) {
                    if (socket->received) {
                        // DEBUG_NETSOCK("net_received(%d): %d\n", nb->descriptor, nb->size);
                        // DEBUG_NETSOCK_to_log("%s: dequeue %ld  %lx:%ld\n", __func__, nb->size, offset, length);
                        socket->received(socket->user_state, socket, shb_data, nb->size, nb->host_address, nb->port);
                    // DEBUG_NETSOCK("%s: enqueue 1< %lx:%d\n", __func__, offset, 2048);
                    }
                }
                err = devq_enqueue((struct devq *)descq_queue, rid, offset, length, 0, 0, NET_EVENT_RECEIVE);
                assert(err_is_ok(err));
                notify = 1;
            } else if (event == NET_EVENT_ACCEPT) {
                uint32_t descriptor = nb->descriptor;
                uint32_t accepted_descriptor = nb->accepted_descriptor;
                struct in_addr host_address = nb->host_address;
                uint16_t port = nb->port;

                // DEBUG_NETSOCK("%s(%d): accept\n", __func__, accepted_descriptor);
                err = devq_enqueue((struct devq *)descq_queue, rid, offset, length, 0, 0, NET_EVENT_RECEIVE);
                assert(err_is_ok(err));
                notify = 1;
                net_accepted(descriptor, accepted_descriptor, host_address, port);
            } else if (event == NET_EVENT_SENT) { // transmitting buffer
                struct net_socket *socket = get_socket(nb->descriptor);
                assert(socket);

                    // DEBUG_NETSOCK_to_log("%s(%d): dequeue sent %lx:%ld %p  %p", __func__, socket->descriptor, offset, length, shb_data, socket->sent);
                if (socket->sent) {
                    socket->sent(socket->user_state, socket, shb_data, nb->size);
                }
                // assert(!buffers[next_used]);
                // buffers[next_used] = buffer_start + offset;
                // next_used = (next_used + 1) % NO_OF_BUFFERS;
            } else if (event == NET_EVENT_CLOSED) {
                struct net_socket *socket = get_socket(nb->descriptor);
                
                net_free(shb_data);
                // DEBUG_NETSOCK("%s(%d): closed\n", __func__, nb->descriptor);
                // DEBUG_NETSOCK_to_log("%s(%d): closed %ld:%p  %ld:%p", __func__, nb->descriptor, next_free, buffers[next_free], next_used, buffers[next_used]);

                if (socket->closed) {
                    socket->closed(socket->user_state, socket);
                }
                dequeue(&sockets, socket);
                free(socket);
            } else {
                DEBUG_NETSOCK("%s: unknown event %ld!", __func__, event);
                assert(0);
            }
        }
    }

    if (notify) {
        // DEBUG_NETSOCK("notify>\n");
        err = devq_notify((struct devq *)descq_queue);
        assert(err_is_ok(err));
        // DEBUG_NETSOCK("notify<\n");
    }

    return SYS_ERR_OK;
}


static errval_t net_sockets_init_internal(struct capref endpoint)
{
    errval_t err;

    memset(buffers, 0, sizeof(buffers));
    next_free = 0;
    next_used = 0;
    ep = endpoint;

    alloc_mem(&buffer_frame, &buffer_start, 2 * BUFFER_SIZE * NO_OF_BUFFERS);
 
    struct descq_func_pointer f;
    f.notify = q_notify;

    DEBUG_NETSOCK("net socket client started \n");

    DEBUG_NETSOCK("Connect to net_sockets_server using EP\n");
    err = net_sockets_bind_to_endpoint(ep, bind_cb, NULL, get_default_waitset(), 
                                       IDC_BIND_FLAGS_DEFAULT);
    assert(err_is_ok(err));
    while (!bound_done) {
        event_dispatch(get_default_waitset());
    }

    DEBUG_NETSOCK("Bound to net_sockets_server\n");

    struct capref descq_ep;
    err = slot_alloc(&descq_ep);
    assert(err_is_ok(err));

    binding->rpc_tx_vtbl.request_descq_ep(binding, disp_get_core_id(), &descq_ep);
    assert(err_is_ok(err));

    err = descq_create_with_ep(&descq_queue, DESCQ_DEFAULT_SIZE, descq_ep, 
                               &queue_id, &f);
    assert(err_is_ok(err));


    DEBUG_NETSOCK("%s: initialized\n", __func__);
    binding->rx_vtbl.connected = net_connected;

    err = binding->rpc_tx_vtbl.register_queue(binding, queue_id);
    assert(err_is_ok(err));

    err = devq_register((struct devq *)descq_queue, buffer_frame, &regionid);
    assert(err_is_ok(err));

    for (int i = 0; i < NO_OF_BUFFERS; i++) {
        err = devq_enqueue((struct devq *)descq_queue, regionid, i * BUFFER_SIZE, BUFFER_SIZE,
                           0, 0, NET_EVENT_RECEIVE);
        if (!err_is_ok(err))
            DEBUG_NETSOCK("%s: %d:%d\n", __func__, i, NO_OF_BUFFERS);
        assert(err_is_ok(err));
        buffers[i] = i * BUFFER_SIZE + buffer_start + BUFFER_SIZE * NO_OF_BUFFERS;
    }

    err = devq_notify((struct devq *)descq_queue);
    assert(err_is_ok(err));

    return SYS_ERR_OK;
}

errval_t net_sockets_init_with_ep(struct capref endpoint) 
{
    return net_sockets_init_internal(endpoint);
}

errval_t net_sockets_init_with_card(char* cardname) 
{
    errval_t err;

    // init queue service client
    if (qs_cl == NULL) {
        err = queue_service_client_init(&qs_cl);   
        if (err_is_fail(err)) {
            return err;
        }
    }
    
    char* record;
    // lookup cards that are available
    err = oct_init();
    if (err_is_fail(err)) {
        return err;
    }

    //Wait for an entry in the namserver to pop up TODO might make this more specific
    err = oct_wait_for(&record, NAMESERVICE_ENTRY);
    if (err_is_fail(err)) {
        return err;
    }
    free(record);

    // get endpoint to any net_sockets_server connectio
    // there is a race condition that the service is exported but the 
    // endpoint factory notyet added -> QSERVICE_ERR_NOT_FOUND
    // if there is no such endpoint factory, fail after some tries
    char cname[256];
    if (strncmp(cardname, "net_sockets_server", strlen("net_sockets_server")) == 0) {   
        strncpy(cname, cardname, strlen(cardname) < 256 ? strlen(cardname): 256);
    } else {
        sprintf(cname, "net_sockets_server:%s", cardname);
    }

    DEBUG_NETSOCK("Netsockets server connecting using name %s \n", cname);
    err = slot_alloc(&ep);
    if (err_is_fail(err)) {
        return err;
    }

    err = QSERVICE_ERR_NOT_FOUND;
    uint8_t timeout = 0;
    while(err == QSERVICE_ERR_NOT_FOUND && timeout < NUM_TRY_TIMEOUT) {
        err = queue_service_client_request_ep_by_name(qs_cl, cname,
                                                      &ep);
        if (err_is_fail(err) && err != QSERVICE_ERR_NOT_FOUND) {
            return err;
        }
        barrelfish_usleep(500*1000);
        timeout++;
    }
    
    if (err_is_fail(err)) {
        slot_free(ep);
        return err;
    }

    net_sockets_init_with_ep(ep); 
    if (err_is_fail(err)) {
        slot_free(ep);
        return err;
    }

    return SYS_ERR_OK;
}

errval_t net_sockets_init(void) 
{
    return net_sockets_init_with_card("net_sockets_server");
}

