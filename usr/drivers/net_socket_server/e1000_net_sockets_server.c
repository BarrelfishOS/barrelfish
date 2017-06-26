/**
 * @brief
 *  E1000 net socket server
 */

/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/deferred.h>
#include <barrelfish/nameservice_client.h>

#include <net/net.h>

#include <barrelfish/waitset_chan.h>

#include <devif/queue_interface.h>
#include <devif/backends/descq.h>

#include <if/net_sockets_defs.h>
#include <net_sockets/net_sockets_types.h>

#include <lwip/ip.h>
#include <lwip/udp.h>
#include <lwip/tcp.h>
#include <lwip/pbuf.h>

#define NO_OF_BUFFERS 128
#define BUFFER_SIZE 16384

struct socket_connection;

struct network_connection {
    struct network_connection *next;

    struct capref buffer_cap;
    struct descq *queue;
    uint64_t queue_id;
    regionid_t region_id;
    void *buffer_start;
    uint64_t buffer_size;

    void *buffers[NO_OF_BUFFERS];
    uint64_t next_free, next_used;

    struct net_sockets_binding *binding;

    struct descq *buffer_queue;
    struct socket_connection *sockets;
};

#define MAX_SEND_FRAMES 2

struct send_frame {
    genoffset_t offset;
    genoffset_t sent, length;
};

struct socket_connection {
    struct socket_connection *next;
    struct network_connection *connection;
    uint32_t descriptor;
    struct send_frame send_frames[MAX_SEND_FRAMES];

    struct udp_pcb *udp_socket;
    struct tcp_pcb *tcp_socket;
};

static struct network_connection *network_connections = NULL;

static struct socket_connection * allocate_socket(struct network_connection *nc)
{
    struct socket_connection *last, *socket;
    uint32_t last_descriptor = 0;

    last = NULL;
    socket = nc->sockets;
    while (socket) {
        if (socket->descriptor != last_descriptor + 1)
            break;
        last = socket;
        last_descriptor = last->descriptor;
        socket = socket->next;
    }

    socket = malloc(sizeof(struct socket_connection));
    assert(socket);
    memset(socket, 0, sizeof(struct socket_connection));

    if (last) {
        socket->next = last->next;
        last->next = socket;
    } else {
        nc->sockets = socket;
    }

    socket->descriptor = last_descriptor + 1;
    
    socket->connection = nc;
    socket->udp_socket = NULL;
    socket->tcp_socket = NULL;

    memset(socket->send_frames, 0, sizeof(socket->send_frames));

    return socket;
}

static void net_udp_receive(void *arg, struct udp_pcb *pcb, struct pbuf *p, const ip_addr_t *addr, u16_t port)
{
    struct socket_connection *connection = arg;
    struct network_connection *nc = connection->connection;
    errval_t err;

    assert(p->tot_len + sizeof(struct net_buffer) <= BUFFER_SIZE);

    uint32_t length = p->tot_len;
    void *buffer = nc->buffers[nc->next_free];

    if (!buffer) {
        debug_printf("%s: drop\n", __func__);
        pbuf_free(p);
        return;
    }

    assert(buffer);
    nc->buffers[nc->next_free] = NULL;
    nc->next_free = (nc->next_free + 1) % NO_OF_BUFFERS;
    assert(sizeof(struct net_buffer) + length <= BUFFER_SIZE);
    struct net_buffer *nb = buffer;

    nb->size = length;
    nb->descriptor = connection->descriptor;
    nb->host_address.s_addr = addr->addr;
    nb->port = port;
    // debug_printf("%s(%d): %p -> %p %p %d\n", __func__, connection->descriptor, buffer, nb->user_callback, nb->user_state, nb->size);

    void *shb_data = buffer + sizeof(struct net_buffer);
    
    struct pbuf *it;
    uint32_t pos;
    
    it = p;
    for (pos = 0; pos < length; ) {
        assert(it);
        memcpy((void *)shb_data + pos, it->payload, it->len);
        pos += it->len;
        it = it->next;
    }
    pbuf_free(p);
    // debug_printf("%s.%d: enqueue 1 %lx:%ld\n", __func__, __LINE__, buffer - nc->buffer_start, sizeof(struct net_buffer) + length);
    err = devq_enqueue((struct devq *)nc->queue, nc->region_id, buffer - nc->buffer_start, sizeof(struct net_buffer) + length,
                       0, 0, 1);
    assert(err_is_ok(err));
    err = devq_notify((struct devq *)nc->queue);
    assert(err_is_ok(err));

    // debug_printf("%s: notifing\n", __func__);
    // struct net_sockets_binding *binding = connection->connection->binding;
    // debug_printf("%s: done\n", __func__);
}


static err_t net_tcp_receive(void *arg, struct tcp_pcb *pcb, struct pbuf *p, err_t error)
{
    struct socket_connection *socket = arg;
    struct network_connection *nc = socket->connection;
    errval_t err;
    uint32_t length = 0;
    void *buffer = nc->buffers[nc->next_free];
    struct net_buffer *nb = buffer;

    // debug_printf("%s: pcb:%p  p:%p\n", __func__, pcb, p);
    if (p) {
        // debug_printf("%s(%d): %d\n", __func__, socket->descriptor, p->tot_len);
        assert(p->len == p->tot_len);
        length = p->tot_len;
        if (!buffer) {
            debug_printf("%s: drop\n", __func__);
            pbuf_free(p);
            return ERR_OK;
        }
        assert(buffer);
        nb->size = length;
        nb->descriptor = socket->descriptor;
        nb->accepted_descriptor = 0;
        nb->host_address.s_addr = 0;
        nb->port = 0;
        // debug_printf("%s(%d): %p -> %d\n", __func__, socket->descriptor, buffer, nb->size);

        void *shb_data = buffer + sizeof(struct net_buffer);
        memcpy((void *)shb_data, p->payload, length);
        tcp_recved(pcb, p->tot_len);
        pbuf_free(p);
    } else {
        assert(buffer);
        nb->size = 0;
        nb->descriptor = socket->descriptor;
        nb->accepted_descriptor = 0;
        nb->host_address.s_addr = 0;
        nb->port = 0;
        debug_printf("%s(%d): close\n", __func__, socket->descriptor);
        // debug_printf("%s(%d): %p -> %p %p %d\n", __func__, connection->descriptor, buffer, nb->user_callback, nb->user_state, nb->size);
    }
    nc->buffers[nc->next_free] = NULL;
    nc->next_free = (nc->next_free + 1) % NO_OF_BUFFERS;
    assert(sizeof(struct net_buffer) + length <= BUFFER_SIZE);
    
    debug_printf("%s.%d: enqueue 1 %lx:%ld %d\n", __func__, __LINE__, buffer - nc->buffer_start, sizeof(struct net_buffer) + length, nb->descriptor);
    err = devq_enqueue((struct devq *)nc->queue, nc->region_id, buffer - nc->buffer_start, sizeof(struct net_buffer) + length,
                       0, 0, 1);
    assert(err_is_ok(err));
    err = devq_notify((struct devq *)nc->queue);
    assert(err_is_ok(err));

    // debug_printf("%s: notifing\n", __func__);
    // struct net_sockets_binding *binding = connection->connection->binding;
    // debug_printf("%s: done\n", __func__);
    return ERR_OK;
}

static void net_tcp_error(void *arg, err_t err)
{
    struct socket_connection *socket = arg;
    // struct network_connection *nc = socket->connection;
    
    debug_printf("%s(%d): error %d\n", __func__, socket->descriptor, err);
    socket->tcp_socket = NULL; // invalidate

    // uint32_t length = 0;
    // void *buffer = nc->buffers[nc->next_free];
    // struct net_buffer *nb = buffer;
    //
    // assert(buffer);
    // nb->size = 0;
    // nb->descriptor = socket->descriptor;
    // nb->accepted_descriptor = 0;
    // nb->host_address.s_addr = 0;
    // nb->port = 0;
    // debug_printf("%s(%d): close on error\n", __func__, socket->descriptor);
    // nc->buffers[nc->next_free] = NULL;
    // nc->next_free = (nc->next_free + 1) % NO_OF_BUFFERS;
    // assert(sizeof(struct net_buffer) + length <= BUFFER_SIZE);
    //
    // // debug_printf("%s.%d: enqueue 1 %lx:%ld %d\n", __func__, __LINE__, buffer - nc->buffer_start, sizeof(struct net_buffer) + length, nb->descriptor);
    // err = devq_enqueue((struct devq *)nc->queue, nc->region_id, buffer - nc->buffer_start, sizeof(struct net_buffer) + length,
    //                    0, 0, 1);
    // assert(err_is_ok(err));
    // err = devq_notify((struct devq *)nc->queue);
    // assert(err_is_ok(err));
}

static err_t net_tcp_sent(void *arg, struct tcp_pcb *pcb, uint16_t len)
{
    struct socket_connection *socket = arg;
    struct network_connection *nc = socket->connection;
    bool notify = false;
    errval_t err;

    while (len > 0) {
        // debug_printf("%s(%d): %d  %zx:%zd:%zd %zx:%zd:%zd\n", __func__, socket->descriptor, len,
        //     socket->send_frames[0].offset, socket->send_frames[0].sent, socket->send_frames[0].length,
        //     socket->send_frames[1].offset, socket->send_frames[1].sent, socket->send_frames[1].length);
        assert(socket->send_frames[0].length);
        if (len < (socket->send_frames[0].length - socket->send_frames[0].sent)) {
            socket->send_frames[0].sent += len;
            len = 0;
        } else {
            len -= socket->send_frames[0].length - socket->send_frames[0].sent;
            
            socket->send_frames[0].length += sizeof(struct net_buffer);
            // debug_printf("%s.%d: enqueue 2 %lx:%zd\n", __func__, __LINE__, socket->send_frames[0].offset, socket->send_frames[0].length);
            err = devq_enqueue((struct devq *)nc->queue, nc->region_id, socket->send_frames[0].offset, socket->send_frames[0].length, 0, 0, 2);
            if (!err_is_ok(err))
                debug_printf("%s: err %zd\n", __func__, err);
            assert(err_is_ok(err));
            notify = true;
            socket->send_frames[0] = socket->send_frames[1];
            socket->send_frames[1].sent = 0;
            socket->send_frames[1].length = 0;
            socket->send_frames[1].offset = 0;
        }
    }
    if (notify) {
        err = devq_notify((struct devq *)nc->queue);
        assert(err_is_ok(err));
    }
    return ERR_OK;
}

static err_t net_tcp_accepted(void *arg, struct tcp_pcb *newpcb, err_t error)
{
    struct socket_connection *socket = arg;
    struct network_connection *nc = socket->connection;
    struct socket_connection *accepted_socket;

    accepted_socket = allocate_socket(nc);
    accepted_socket->udp_socket = NULL;
    accepted_socket->tcp_socket = newpcb;
    tcp_arg(accepted_socket->tcp_socket, accepted_socket);
    tcp_recv(accepted_socket->tcp_socket, net_tcp_receive);
    tcp_err(accepted_socket->tcp_socket, net_tcp_error);
    tcp_sent(accepted_socket->tcp_socket, net_tcp_sent);

    debug_printf("%s(%d): -> %d\n", __func__, socket->descriptor, accepted_socket->descriptor);
    // errval_t err = nc->binding->tx_vtbl.accepted(nc->binding, BLOCKING_CONT, socket->descriptor, accepted_socket->descriptor, 0, 0, SYS_ERR_OK);
    // assert(err_is_ok(err));

    errval_t err;
    uint32_t length = 0;
    void *buffer = nc->buffers[nc->next_free];
    struct net_buffer *nb = buffer;

    nb->size = 0;
    nb->descriptor = socket->descriptor;
    nb->accepted_descriptor = accepted_socket->descriptor;
    nb->host_address.s_addr = 0;
    nb->port = 0;
    debug_printf("%s(%d): accepted %p\n", __func__, socket->descriptor, newpcb);

    nc->buffers[nc->next_free] = NULL;
    nc->next_free = (nc->next_free + 1) % NO_OF_BUFFERS;
    assert(sizeof(struct net_buffer) + length <= BUFFER_SIZE);
    
    // debug_printf("%s.%d: enqueue 1 %lx:%ld %d\n", __func__, __LINE__, buffer - nc->buffer_start, sizeof(struct net_buffer) + length, nb->descriptor);
    err = devq_enqueue((struct devq *)nc->queue, nc->region_id, buffer - nc->buffer_start, sizeof(struct net_buffer) + length,
                       0, 0, 1);
    assert(err_is_ok(err));
    err = devq_notify((struct devq *)nc->queue);
    assert(err_is_ok(err));

    return ERR_OK;
}


static errval_t net_register_queue(struct net_sockets_binding *binding, uint64_t queue_id)
{
    struct network_connection *nc;

    nc = network_connections;
    while (nc) {
        if (nc->queue_id == queue_id)
            break;
        nc = nc->next;
    }
    assert(nc);

    binding->st = nc;
    nc->binding = binding;
    devq_set_state((struct devq *)nc->queue, nc);

    return SYS_ERR_OK;
}

static errval_t net_udp_socket(struct net_sockets_binding *binding, uint32_t *descriptor)
{
    struct network_connection *nc;
    struct socket_connection *socket;

    nc = binding->st;
    socket = allocate_socket(nc);
    *descriptor = socket->descriptor;

    struct udp_pcb *pcb = udp_new();
    assert(pcb);
    socket->udp_socket = pcb;
    udp_recv(socket->udp_socket, net_udp_receive, socket);

    return SYS_ERR_OK;
}

static errval_t net_tcp_socket(struct net_sockets_binding *binding, uint32_t *descriptor)
{
    struct network_connection *nc;
    struct socket_connection *socket;

    nc = binding->st;
    socket = allocate_socket(nc);
    *descriptor = socket->descriptor;

    struct tcp_pcb *pcb = tcp_new();
    assert(pcb);
    socket->tcp_socket = pcb;
    tcp_arg(pcb, socket);
    tcp_recv(socket->tcp_socket, net_tcp_receive);

    return SYS_ERR_OK;
}

static errval_t net_bind(struct net_sockets_binding *binding, uint32_t descriptor, uint32_t ip_address, uint16_t port, errval_t *error)
{
    struct network_connection *nc;
    struct socket_connection *socket;

    nc = binding->st;
    socket = nc->sockets;
    while (socket) {
        if (socket->descriptor == descriptor)
            break;
        socket = socket->next;
    }
    assert(socket);

    if (socket->udp_socket) {
        ip_addr_t ip;

        ip.addr = ip_address;
        *error = udp_bind(socket->udp_socket, &ip, port);
        assert(err_is_ok(*error));
        *error = SYS_ERR_OK;

        debug_printf("UDP ECHO bind done.\n");
    } else if (socket->tcp_socket) {
        ip_addr_t ip;

        ip.addr = ip_address;
        debug_printf("%s(%d): %x %d\n", __func__, socket->descriptor, ip.addr, port);
        *error = tcp_bind(socket->tcp_socket, &ip, port);
        assert(err_is_ok(*error));
        *error = SYS_ERR_OK;

        debug_printf("TCP ECHO bind done.\n");
    }
    return SYS_ERR_OK;
}

static errval_t net_listen(struct net_sockets_binding *binding, uint32_t descriptor, uint8_t backlog, errval_t *error)
{
    struct network_connection *nc;
    struct socket_connection *socket;

    nc = binding->st;
    socket = nc->sockets;
    while (socket) {
        if (socket->descriptor == descriptor)
            break;
        socket = socket->next;
    }
    assert(socket);
    assert(socket->tcp_socket);
    socket->tcp_socket = tcp_listen(socket->tcp_socket);
    debug_printf("%s(%d): listen %p\n", __func__, descriptor, socket->tcp_socket);
    tcp_accept(socket->tcp_socket, net_tcp_accepted);
    tcp_err(socket->tcp_socket, net_tcp_error);
    // socket->tcp_socket = tcp_listen_with_backlog(socket->tcp_socket, backlog);
    assert(socket->tcp_socket);

    *error = SYS_ERR_OK;
    return SYS_ERR_OK;
}

static err_t net_tcp_connected(void *arg, struct tcp_pcb *tpcb, err_t error)
{
    struct socket_connection *socket = arg;
    struct network_connection *nc = socket->connection;

    errval_t err = nc->binding->tx_vtbl.connected(nc->binding, BLOCKING_CONT, socket->descriptor, SYS_ERR_OK);
    assert(err_is_ok(err));

    return SYS_ERR_OK;
}

static errval_t net_connect(struct net_sockets_binding *binding, uint32_t descriptor, uint32_t ip_address, uint16_t port, errval_t *error)
{
    struct network_connection *nc;
    struct socket_connection *socket;

    nc = binding->st;
    socket = nc->sockets;
    while (socket) {
        if (socket->descriptor == descriptor)
            break;
        socket = socket->next;
    }
    assert(socket);

    if (socket->udp_socket) {
        ip_addr_t addr;
        addr.addr = ip_address;
        assert(udp_connect(socket->udp_socket, &addr, port) == ERR_OK);
        *error = SYS_ERR_OK;
    } else if (socket->tcp_socket) {
        ip_addr_t addr;
        addr.addr = ip_address;
        assert(tcp_connect(socket->tcp_socket, &addr, port, net_tcp_connected) == ERR_OK);
        *error = SYS_ERR_OK;
    }

    return SYS_ERR_OK;
}

static errval_t net_delete_socket(struct net_sockets_binding *binding, uint32_t descriptor, errval_t *error)
{
    struct network_connection *nc;
    struct socket_connection *socket, *last;

    nc = binding->st;
    socket = nc->sockets;
    last = NULL;
    while (socket) {
        if (socket->descriptor == descriptor)
            break;
        last = socket;
        socket = socket->next;
    }
    assert(socket);
    if (socket->udp_socket) {
        udp_recv(socket->udp_socket, NULL, NULL);
        udp_remove(socket->udp_socket);
    } else if (socket->tcp_socket) {
        tcp_recv(socket->tcp_socket, NULL); // you can receive packets after you close the socket
        // tcp_accept(socket->tcp_socket, NULL);
        err_t e;
        debug_printf("%s(%d): tcp_close %p\n", __func__, descriptor, socket->tcp_socket);
        e = tcp_close(socket->tcp_socket);
        assert(e == ERR_OK);
    }

    debug_printf("%s(%d):\n", __func__, descriptor);
    debug_printf("%s: %ld:%p  %ld:%p\n", __func__, nc->next_free, nc->buffers[nc->next_free], nc->next_used, nc->buffers[nc->next_used]);
    if (last)
        last->next = socket->next;
    else
        nc->sockets = socket->next;
    free(socket);

    *error = SYS_ERR_OK;
    return SYS_ERR_OK;
}

static errval_t q_create(struct descq* q, bool notifications, uint8_t role,
                       uint64_t* queue_id)
{
    struct network_connection *nc;
    static uint64_t qid = 1;

    nc = malloc(sizeof(struct network_connection));
    assert(nc);
    nc->next = network_connections;
    network_connections = nc;

    nc->sockets = NULL;
    nc->binding = NULL;
    nc->queue = q;
    *queue_id = qid++;
    nc->queue_id = *queue_id;
    memset(nc->buffers, 0, sizeof(nc->buffers));
    nc->next_free = 0;
    nc->next_used = 0;
    return SYS_ERR_OK;
}

static errval_t q_destroy(struct descq* q)
{
    return SYS_ERR_OK;
}


static errval_t q_notify(struct descq* q)
{
    struct devq* queue = (struct devq *)q;
    errval_t err = SYS_ERR_OK;
    //errval_t err2 = SYS_ERR_OK;
    regionid_t rid;
    genoffset_t offset;
    genoffset_t length;
    genoffset_t valid_data;
    genoffset_t valid_length;
    uint64_t flags;
    struct network_connection *nc;
    bool notify = 0;

    // debug_printf("%s: \n", __func__);
    nc = devq_get_state(queue);
    for (;;) {
        err = devq_dequeue(queue, &rid, &offset, &length,
                           &valid_data, &valid_length, &flags);
        if (err_is_fail(err)) {
            break;
        } else {
            // debug_printf("%s: dequeue %lx:%ld %ld\n", __func__, offset, length, flags);
            if (flags == 1) { // receiving buffer
                // debug_printf("%s: dequeue %lx:%ld %ld\n", __func__, offset, length, flags);
                assert(!nc->buffers[nc->next_used]);
                nc->buffers[nc->next_used] = nc->buffer_start + offset;
                nc->next_used = (nc->next_used + 1) % NO_OF_BUFFERS;
            } else if (flags == 2) { // transmitting buffer
                struct socket_connection *socket;
                void *buffer;
                buffer = offset + nc->buffer_start;
                struct net_buffer *nb = buffer;
                void *shb_data = (void *)buffer + sizeof(struct net_buffer);

                // debug_printf("%s: %p\n", __func__, buffer);

                socket = nc->sockets;
                while (socket) {
                    if (socket->descriptor == nb->descriptor)
                        break;
                    socket = socket->next;
                }
                assert(socket);

                // debug_printf("buffer: %d %d %x %d  %p %p\n", nb->size, nb->descriptor, nb->host_address, nb->port, socket->udp_socket, socket->tcp_socket);
                if (socket->udp_socket) {
                    struct udp_pcb *pcb = socket->udp_socket;
                    struct pbuf *p;

                    p = pbuf_alloc(PBUF_TRANSPORT, nb->size, PBUF_RAM);
                    assert(p);
                    memcpy(p->payload, shb_data, nb->size);

                    ip_addr_t addr;
                    uint16_t port;
                    port = nb->port;
                    addr.addr = nb->host_address.s_addr;
                    // debug_printf("%s.%d: enqueue 2 %lx:%d\n", __func__, __LINE__, offset, nb->size);
                    err = devq_enqueue(queue, rid, offset, length, 0, 0, 2);
                    assert(err_is_ok(err));
                    notify = 1;
                    // debug_printf("%s(%d): %d\n", __func__, socket->descriptor, p->tot_len);
                    if (port && addr.addr) {
                        assert(udp_sendto(pcb, p, &addr, port) == ERR_OK);
                    } else {
                        assert(udp_send(pcb, p) == ERR_OK);
                    }
                    pbuf_free(p);
                } else if (socket->tcp_socket) {
                    err_t e;
                    // debug_printf("%s: dequeue %lx:%ld %ld\n", __func__, offset, length, flags);
                    
                    if (socket->send_frames[0].length == 0) {
                        
                    } else {
                        assert(socket->send_frames[1].length == 0);
                    }
                    // debug_printf("%s: tcp_write %d %d\n", __func__, tcp_sndbuf(socket->tcp_socket), nb->size);
                    e = tcp_write(socket->tcp_socket, shb_data, nb->size, TCP_WRITE_FLAG_COPY);
                    if (e != ERR_OK)
                        debug_printf("%s: e=%d\n", __func__, e);
                    assert(e == ERR_OK);
                    e = tcp_output(socket->tcp_socket);
                    assert(e == ERR_OK);
                    
                    if (socket->send_frames[0].length == 0) {
                        socket->send_frames[0].offset = offset;
                        socket->send_frames[0].length = length - sizeof(struct net_buffer);
                    } else {
                        socket->send_frames[1].offset = offset;
                        socket->send_frames[1].length = length - sizeof(struct net_buffer);
                    }
                    // debug_printf("%s.%d: enqueue 2 %lx:%zd\n", __func__, __LINE__, offset, length);
                    // err = devq_enqueue(queue, rid, offset, length, 0, 0, 2);
                    // assert(err_is_ok(err));
                    // notify = 1;
                }
            }
        }
    }

    if (notify) {
        // debug_printf("notify>\n");
        err = devq_notify(queue);
        // debug_printf("notify<\n");
        assert(err_is_ok(err));
    }

    return SYS_ERR_OK;
}

static errval_t q_reg(struct descq* q, struct capref cap,
                    regionid_t rid)
{
    struct frame_identity pa;
    struct network_connection *nc;

    nc = devq_get_state((struct devq *)q);

    errval_t err = frame_identify(cap, &pa);
    assert(err_is_ok(err));
    nc->buffer_cap = cap;
    nc->region_id = rid;

    nc->buffer_size = pa.bytes;
    err = vspace_map_one_frame(&nc->buffer_start, pa.bytes, cap, NULL, NULL);
    assert(err_is_ok(err));

    return SYS_ERR_OK;
}


static errval_t q_dereg(struct descq* q, regionid_t rid)
{
    return SYS_ERR_OK;
}
    

static errval_t q_control(struct descq* q, uint64_t cmd, uint64_t value, uint64_t* res)
{
    return SYS_ERR_OK;
}


static struct net_sockets_rpc_rx_vtbl rpc_rx_vtbl = {
    .register_queue_call = net_register_queue,
    .new_udp_socket_call = net_udp_socket,
    .new_tcp_socket_call = net_tcp_socket,
    .bind_call = net_bind,
    .connect_call = net_connect,
    .delete_socket_call = net_delete_socket,
    .listen_call = net_listen,
};


static errval_t connect_cb(void *st, struct net_sockets_binding *binding)
{
    binding->rpc_rx_vtbl = rpc_rx_vtbl;
    return SYS_ERR_OK;
}


static void export_cb(void *st, errval_t err, iref_t iref)
{
    assert(err_is_ok(err));
    err = nameservice_register("net_sockets", iref);
    assert(err_is_ok(err));
}

int main(int argc, char *argv[])
{
    errval_t err;

    debug_printf("Net socket server started for e1000 %s.\n", argv[2]);

    char servicename[64];
    snprintf(servicename, sizeof(servicename), "e1000:%s", argv[2]);

    /* connect to the network */
    err = networking_init(servicename, 0);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to initialize the network");
    }


    struct descq *exp_queue;
    struct descq_func_pointer f;

    f.notify = q_notify;
    f.create = q_create;
    f.destroy = q_destroy;
    f.reg = q_reg;
    f.dereg = q_dereg;
    f.control = q_control;

    err = descq_create(&exp_queue, DESCQ_DEFAULT_SIZE, "net_sockets_queue",
                       true, true, 0, NULL, &f);
    assert(err_is_ok(err));


    err = net_sockets_export(NULL, export_cb, connect_cb, get_default_waitset(),
                            IDC_EXPORT_FLAGS_DEFAULT);
    assert(err_is_ok(err));

    while(1) {
        event_dispatch(get_default_waitset());

        // networking_poll();
    }

    debug_printf("UDP ECHO termiated.\n");

    return 0;
}
