/**
 * @brief
 *  Net socket server
 */

/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <getopt.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/deferred.h>
#include <barrelfish/nameservice_client.h>
#include <driverkit/driverkit.h>

#include <arpa/inet.h>

#include <net/net.h>
#include <net/dhcp.h>

#include <barrelfish/waitset_chan.h>

#include <octopus/octopus.h>

#include <devif/queue_interface.h>
#include <devif/backends/descq.h>

#include <if/net_sockets_defs.h>
#include <net_sockets/net_sockets_types.h>
#include <debug_log/debug_log.h>

#include <lwip/ip.h>
#include <lwip/udp.h>
#include <lwip/tcp.h>
#include <lwip/pbuf.h>

#include "netss.h"

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

struct netss_state {
    struct network_connection* ns;
    char service_name[128];
    bool exported;
};

static struct network_connection *network_connections = NULL;
static struct descq *exp_queue;

static struct socket_connection * find_socket_connection(struct network_connection *nc, uint32_t descriptor)
{
    struct socket_connection *socket;

    socket = nc->sockets;
    while (socket) {
        if (socket->descriptor == descriptor)
            break;
        socket = socket->next;
    }
    return socket;
}

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
        NET_SOCK_DEBUG("%s: drop\n", __func__);
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
    NET_SOCK_DEBUG("%s(%d): %p -> %p %p %d\n", __func__, connection->descriptor, 
                   buffer, nb->user_callback, nb->user_state, nb->size);

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
    err = devq_enqueue((struct devq *)nc->queue, nc->region_id, buffer - nc->buffer_start, 
                       sizeof(struct net_buffer) + length, 0, 0, NET_EVENT_RECEIVED);
    assert(err_is_ok(err));
    err = devq_notify((struct devq *)nc->queue);
    assert(err_is_ok(err));
}


static err_t net_tcp_receive(void *arg, struct tcp_pcb *pcb, struct pbuf *p, err_t error)
{
    struct socket_connection *socket = arg;
    struct network_connection *nc = socket->connection;
    errval_t err;
    uint32_t length = 0;
    void *buffer = nc->buffers[nc->next_free];
    struct net_buffer *nb = buffer;

    NET_SOCK_DEBUG("%s(%d): pcb:%p  p:%p\n", __func__, socket->descriptor, pcb, p);
    if (p) {
        NET_SOCK_DEBUG("%s(%d): %d\n", __func__, socket->descriptor, p->tot_len);
        assert(p->len == p->tot_len);
        length = p->tot_len;

        if (!buffer) {
            NET_SOCK_DEBUG("%s: drop\n", __func__);
            pbuf_free(p);
            return ERR_OK;
        }
        assert(buffer);
        nb->size = length;
        nb->descriptor = socket->descriptor;
        nb->accepted_descriptor = 0;
        nb->host_address.s_addr = 0;
        nb->port = 0;
        NET_SOCK_DEBUG("%s(%d): %p -> %d\n", __func__, socket->descriptor, buffer, 
                       nb->size);

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
        tcp_err(socket->tcp_socket, NULL);
    }
    nc->buffers[nc->next_free] = NULL;
    nc->next_free = (nc->next_free + 1) % NO_OF_BUFFERS;
    assert(sizeof(struct net_buffer) + length <= BUFFER_SIZE);

    NET_SOCK_DEBUG("%s.%d: enqueue 1 %lx:%ld %d\n", __func__, __LINE__, 
                   buffer - nc->buffer_start, sizeof(struct net_buffer) + length, 
                   nb->descriptor);
    err = devq_enqueue((struct devq *)nc->queue, nc->region_id, buffer - nc->buffer_start, 
                       sizeof(struct net_buffer) + length, 0, 0, NET_EVENT_RECEIVED);

    assert(err_is_ok(err));
    err = devq_notify((struct devq *)nc->queue);
    assert(err_is_ok(err));

    return ERR_OK;
}

static void net_tcp_error(void *arg, err_t tcp_err)
{
    struct socket_connection *socket = arg;
    struct network_connection *nc = socket->connection;
    errval_t err;

    NET_SOCK_DEBUG("%s(%d): error %d\n", __func__, socket->descriptor, tcp_err);
    tcp_sent(socket->tcp_socket, NULL);
    tcp_recv(socket->tcp_socket, NULL);
    socket->tcp_socket = NULL; // invalidate

    void *buffer = nc->buffers[nc->next_free];
    assert(buffer);
    struct net_buffer *nb = buffer;
    nb->size = 0;
    nb->descriptor = socket->descriptor;
    nb->accepted_descriptor = 0;
    nb->host_address.s_addr = 0;
    nb->port = 0;
    nc->buffers[nc->next_free] = NULL;
    nc->next_free = (nc->next_free + 1) % NO_OF_BUFFERS;

    err = devq_enqueue((struct devq *)nc->queue, nc->region_id, buffer - nc->buffer_start, sizeof(struct net_buffer),
                       0, 0, NET_EVENT_RECEIVED);
    assert(err_is_ok(err));

    err = devq_notify((struct devq *)nc->queue);
    assert(err_is_ok(err));
}

static err_t net_tcp_sent(void *arg, struct tcp_pcb *pcb, uint16_t len)
{
    struct socket_connection *socket = arg;
    struct network_connection *nc = socket->connection;
    bool notify = false;
    errval_t err;

    while (len > 0) {
        if (!socket->send_frames[0].length)
            debug_print_log();
        assert(socket->send_frames[0].length);
        if (len < (socket->send_frames[0].length - socket->send_frames[0].sent)) {
            socket->send_frames[0].sent += len;
            len = 0;
        } else {
            len -= socket->send_frames[0].length - socket->send_frames[0].sent;

            socket->send_frames[0].length += sizeof(struct net_buffer);
            err = devq_enqueue((struct devq *)nc->queue, nc->region_id, 
                               socket->send_frames[0].offset, 
                               socket->send_frames[0].length, 0, 0, 
                               NET_EVENT_SENT);
            if (!err_is_ok(err))
                NET_SOCK_DEBUG("%s: err %zd\n", __func__, err);
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

    newpcb->flags |= TF_NODELAY;

    accepted_socket = allocate_socket(nc);
    accepted_socket->udp_socket = NULL;
    accepted_socket->tcp_socket = newpcb;
    tcp_arg(accepted_socket->tcp_socket, accepted_socket);
    tcp_recv(accepted_socket->tcp_socket, net_tcp_receive);
    tcp_err(accepted_socket->tcp_socket, net_tcp_error);
    tcp_sent(accepted_socket->tcp_socket, net_tcp_sent);

    NET_SOCK_DEBUG("%s(%d): -> %d\n", __func__, socket->descriptor, accepted_socket->descriptor);

    errval_t err;
    uint32_t length = 0;
    void *buffer = nc->buffers[nc->next_free];
    struct net_buffer *nb = buffer;

    nb->size = 0;
    nb->descriptor = socket->descriptor;
    nb->accepted_descriptor = accepted_socket->descriptor;
    nb->host_address.s_addr = newpcb->remote_ip.addr;
    nb->port = newpcb->remote_port;

    nc->buffers[nc->next_free] = NULL;
    nc->next_free = (nc->next_free + 1) % NO_OF_BUFFERS;
    assert(sizeof(struct net_buffer) + length <= BUFFER_SIZE);

    NET_SOCK_DEBUG("%s.%d: enqueue 1 %lx:%ld %d\n", __func__, __LINE__, 
                   buffer - nc->buffer_start, sizeof(struct net_buffer) + length, 
                   nb->descriptor);
    err = devq_enqueue((struct devq *)nc->queue, nc->region_id, buffer - nc->buffer_start, 
                       sizeof(struct net_buffer) + length,
                       0, 0, NET_EVENT_ACCEPT);
    assert(err_is_ok(err));
    err = devq_notify((struct devq *)nc->queue);
    assert(err_is_ok(err));

    return ERR_OK;
}

static errval_t net_request_descq_ep_rpc(struct net_sockets_binding *binding, uint16_t core, 
                                         struct capref* ep)
{
    errval_t err;
    err = slot_alloc(ep);
    if (err_is_fail(err)) {
        return err;
    }

    err = descq_create_ep(exp_queue, core, ep);
    if (err_is_fail(err)) {
        slot_free(*ep);
    }
    return err;
}

static void net_request_descq_ep(struct net_sockets_binding *binding, uint16_t core)
{
    struct capref ep;
    errval_t err;

    err = net_request_descq_ep_rpc(binding, core, &ep);
    err = binding->tx_vtbl.request_descq_ep_response(binding, NOP_CONT, ep);
    assert(err_is_ok(err));
}

static errval_t net_register_queue_rpc(struct net_sockets_binding *binding, uint64_t queue_id)
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

static void net_register_queue(struct net_sockets_binding *binding, uint64_t queue_id)
{
    errval_t err;
    err = net_register_queue_rpc(binding, queue_id);
    err = binding->tx_vtbl.register_queue_response(binding, NOP_CONT);
    assert(err_is_ok(err));
}

static errval_t net_udp_socket_rpc(struct net_sockets_binding *binding, 
                                   uint32_t *descriptor)
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

static void net_udp_socket(struct net_sockets_binding *binding)
{
    errval_t err;
    uint32_t desc;
    err = net_udp_socket_rpc(binding, &desc);
    err = binding->tx_vtbl.new_udp_socket_response(binding, NOP_CONT, desc);
    assert(err_is_ok(err));
}

static errval_t net_tcp_socket_rpc(struct net_sockets_binding *binding, uint32_t *descriptor)
{
    struct network_connection *nc;
    struct socket_connection *socket;

    nc = binding->st;
    socket = allocate_socket(nc);
    *descriptor = socket->descriptor;

    struct tcp_pcb *pcb = tcp_new();
    assert(pcb);
    pcb->flags |= TF_NODELAY;
    socket->tcp_socket = pcb;
    tcp_arg(pcb, socket);
    tcp_recv(socket->tcp_socket, net_tcp_receive);

    return SYS_ERR_OK;
}

static void net_tcp_socket(struct net_sockets_binding *binding)
{
    errval_t err;
    uint32_t desc;
    err = net_tcp_socket_rpc(binding, &desc);
    err = binding->tx_vtbl.new_tcp_socket_response(binding, NOP_CONT, desc);
    assert(err_is_ok(err));
}

static errval_t net_bind_rpc(struct net_sockets_binding *binding, uint32_t descriptor, 
                             uint32_t ip_address, uint16_t port, errval_t *error, 
                             uint16_t *bound_port)
{
    struct network_connection *nc;
    struct socket_connection *socket;

    nc = binding->st;
    socket = find_socket_connection(nc, descriptor);
    assert(socket);

    if (socket->udp_socket) {
        ip_addr_t ip;

        ip.addr = ip_address;
        *error = udp_bind(socket->udp_socket, &ip, port);
        assert(err_is_ok(*error));
        *bound_port = socket->udp_socket->local_port;
        *error = SYS_ERR_OK;
    } else if (socket->tcp_socket) {
        ip_addr_t ip;

        ip.addr = ip_address;
        NET_SOCK_DEBUG("%s(%d): %x %d\n", __func__, socket->descriptor, ip.addr, port);
        *error = tcp_bind(socket->tcp_socket, &ip, port);
        assert(err_is_ok(*error));
        *bound_port = socket->tcp_socket->local_port;
        *error = SYS_ERR_OK;
    }
    return SYS_ERR_OK;
}

static void net_bind(struct net_sockets_binding *binding, uint32_t descriptor, 
                     uint32_t ip_address, uint16_t port)
{
    errval_t err;
    uint16_t bound_port;
    err = net_bind_rpc(binding, descriptor, ip_address , port, &err, &bound_port);
    err = binding->tx_vtbl.bind_response(binding, NOP_CONT, err, bound_port);
    assert(err_is_ok(err));
}

static errval_t net_listen_rpc(struct net_sockets_binding *binding, 
                               uint32_t descriptor, uint8_t backlog, 
                               errval_t *error)
{
    struct network_connection *nc;
    struct socket_connection *socket;

    if (descriptor == -1) {
        debug_print_log();
        return SYS_ERR_OK;
    }
    nc = binding->st;
    socket = find_socket_connection(nc, descriptor);
    assert(socket);
    assert(socket->tcp_socket);
    socket->tcp_socket = tcp_listen(socket->tcp_socket);
    NET_SOCK_DEBUG("%s(%d): listen %p\n", __func__, descriptor, socket->tcp_socket);
    tcp_accept(socket->tcp_socket, net_tcp_accepted);
    tcp_err(socket->tcp_socket, net_tcp_error);
    // socket->tcp_socket = tcp_listen_with_backlog(socket->tcp_socket, backlog);
    assert(socket->tcp_socket);

    *error = SYS_ERR_OK;
    return SYS_ERR_OK;
}

static void net_listen(struct net_sockets_binding *binding, 
                       uint32_t descriptor, uint8_t backlog)
{
    errval_t err, err2;
    err = net_listen_rpc(binding, descriptor, backlog, &err2);
    err = binding->tx_vtbl.listen_response(binding, NOP_CONT, err2);
    assert(err_is_ok(err));
}


static err_t net_tcp_connected(void *arg, struct tcp_pcb *tpcb, err_t error)
{
    struct socket_connection *socket = arg;
    struct network_connection *nc = socket->connection;

    errval_t err = nc->binding->tx_vtbl.connected(nc->binding, BLOCKING_CONT, socket->descriptor, SYS_ERR_OK, tpcb->remote_ip.addr, tpcb->remote_port);
    assert(err_is_ok(err));

    return SYS_ERR_OK;
}

static errval_t net_connect_rpc(struct net_sockets_binding *binding, 
                                uint32_t descriptor, uint32_t ip_address, 
                                uint16_t port, errval_t *error)
{
    struct network_connection *nc;
    struct socket_connection *socket;

    nc = binding->st;
    socket = find_socket_connection(nc, descriptor);
    assert(socket);

    if (socket->udp_socket) {
        ip_addr_t addr;
        err_t e;

        addr.addr = ip_address;
        e = udp_connect(socket->udp_socket, &addr, port);
        assert(e == ERR_OK);
        *error = SYS_ERR_OK;
    } else if (socket->tcp_socket) {
        ip_addr_t addr;
        err_t e;

        addr.addr = ip_address;
        e = tcp_connect(socket->tcp_socket, &addr, port, net_tcp_connected);
        assert(e == ERR_OK);
        *error = SYS_ERR_OK;
    }

    return SYS_ERR_OK;
}

static void net_connect(struct net_sockets_binding *binding, 
                        uint32_t descriptor, uint32_t ip_address, 
                        uint16_t port)
{
    errval_t err, err2;
    err = net_connect_rpc(binding, descriptor, ip_address, port, &err2);
    err = binding->tx_vtbl.connect_response(binding, NOP_CONT, err2);
    assert(err_is_ok(err));
}

static void net_delete_socket(struct network_connection *nc, uint32_t descriptor)
{
    struct socket_connection *socket, *last;
    errval_t err;

    NET_SOCK_DEBUG("%s(%d): tcp_close", __func__, descriptor);
    socket = nc->sockets;
    last = NULL;
    while (socket) {
        if (socket->descriptor == descriptor)
            break;
        last = socket;
        socket = socket->next;
    }
    if (!socket)
        debug_print_log();
    assert(socket);
    if (socket->udp_socket) {
        udp_recv(socket->udp_socket, NULL, NULL);
        udp_remove(socket->udp_socket);
    } else if (socket->tcp_socket) {
        tcp_recv(socket->tcp_socket, NULL); // you can receive packets after you close the socket
        tcp_sent(socket->tcp_socket, NULL);
        // tcp_accept(socket->tcp_socket, NULL);
        err_t e;
        e = tcp_close(socket->tcp_socket);
        assert(e == ERR_OK);
    }

    while (socket->send_frames[0].length) { // invaldate all sent frames
        void *buffer;
        struct net_buffer *nb;

        socket->send_frames[0].length += sizeof(struct net_buffer);
        buffer = socket->send_frames[0].offset + nc->buffer_start;
        nb = buffer;
        nb->descriptor = descriptor;
        err = devq_enqueue((struct devq *)nc->queue, nc->region_id, socket->send_frames[0].offset, socket->send_frames[0].length, 0, 0, NET_EVENT_SENT);
        assert(err_is_ok(err));
        socket->send_frames[0] = socket->send_frames[1];
        socket->send_frames[1].sent = 0;
        socket->send_frames[1].length = 0;
        socket->send_frames[1].offset = 0;
    }
    NET_SOCK_DEBUG("%s: %ld:%p  %ld:%p\n", __func__, nc->next_free, 
                   nc->buffers[nc->next_free], nc->next_used, 
                   nc->buffers[nc->next_used]);
    if (last)
        last->next = socket->next;
    else
        nc->sockets = socket->next;
    free(socket);
}

static uint64_t qid = 1;

static errval_t q_create(struct descq* q, uint64_t* queue_id)
{
    struct network_connection *nc;

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
    uint64_t event;
    struct network_connection *nc;
    bool notify = 0;

    nc = devq_get_state(queue);
    for (int i = 0; i < NETSOCKET_LOOP_ITER; i++) {
        err = devq_dequeue(queue, &rid, &offset, &length,
                           &valid_data, &valid_length, &event);
        if (err_is_fail(err)) {
            break;
        } else {
            void *buffer;
            buffer = offset + nc->buffer_start;
            struct net_buffer *nb = buffer;

            NET_SOCK_DEBUG(" offset %lu length %lu \n", offset, length);
            if (event == NET_EVENT_RECEIVE) {
                assert(!nc->buffers[nc->next_used]);
                nc->buffers[nc->next_used] = nc->buffer_start + offset;
                nc->next_used = (nc->next_used + 1) % NO_OF_BUFFERS;
            } else if (event == NET_EVENT_SEND) {
                struct socket_connection *socket;
                void *shb_data = (void *)buffer + sizeof(struct net_buffer);

                NET_SOCK_DEBUG("%s: %p\n", __func__, buffer);
                // find the socket
                socket = find_socket_connection(nc, nb->descriptor);
                if (!socket)
                    NET_SOCK_DEBUG("%s(%d): %p\n", __func__, nb->descriptor, socket);
                assert(socket);

                NET_SOCK_DEBUG("buffer: %d %d %x %d  %p %p\n", nb->size, nb->descriptor, 
                               nb->host_address, nb->port, socket->udp_socket, 
                               socket->tcp_socket);
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
                    NET_SOCK_DEBUG("%s.%d: enqueue 2 %lx:%d\n", __func__, __LINE__, offset, nb->size);
                    err = devq_enqueue(queue, rid, offset, length, 0, 0, NET_EVENT_SENT);
                    assert(err_is_ok(err));
                    notify = 1;
                    NET_SOCK_DEBUG("%s(%d): %d\n", __func__, socket->descriptor, p->tot_len);
                    if (port && addr.addr) {
                        err_t e;

                        e = udp_sendto(pcb, p, &addr, port);
                        if (e != ERR_OK)
                            NET_SOCK_DEBUG("%s(%d): err:%d\n", __func__, socket->descriptor, e);
                        assert(e == ERR_OK);
                    } else {
                        err_t e;

                        e = udp_send(pcb, p);
                        if (e != ERR_OK)
                            NET_SOCK_DEBUG("%s(%d): err:%d\n", __func__, socket->descriptor, e);
                        assert(e == ERR_OK);
                    }
                    pbuf_free(p);
                } else if (socket->tcp_socket) {
                    err_t e;
                    NET_SOCK_DEBUG("%s: dequeue %lx:%ld %ld\n", __func__, offset, length, event);

                    if (socket->send_frames[0].length == 0) {

                    } else {
                        assert(socket->send_frames[1].length == 0);
                    }
                    NET_SOCK_DEBUG("%s: tcp_write %d %d\n", __func__, tcp_sndbuf(socket->tcp_socket), nb->size);
                    e = tcp_write(socket->tcp_socket, shb_data, nb->size, TCP_WRITE_FLAG_COPY);
                    if (e != ERR_OK)
                        NET_SOCK_DEBUG("%s: e=%d\n", __func__, e);
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
                } else {
                    err = devq_enqueue(queue, rid, offset, length, 0, 0, NET_EVENT_SENT);
                    assert(err_is_ok(err));
                    notify = 1;
                }
            } else if (event == NET_EVENT_CLOSE) {
                net_delete_socket(nc, nb->descriptor);
                err = devq_enqueue(queue, rid, offset, length, 0, 0, NET_EVENT_CLOSED);
                assert(err_is_ok(err));
                notify = 1;
            } else {
                NET_SOCK_DEBUG("%s: unknown event %ld!", __func__, event);
                assert(0);
            }
        }
    }

    if (notify) {
        // NET_SOCK_DEBUG("notify>\n");
        err = devq_notify(queue);
        // NET_SOCK_DEBUG("notify<\n");
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
    .request_descq_ep_call = net_request_descq_ep_rpc,
    .register_queue_call = net_register_queue_rpc,
    .new_udp_socket_call = net_udp_socket_rpc,
    .new_tcp_socket_call = net_tcp_socket_rpc,
    .bind_call = net_bind_rpc,
    .connect_call = net_connect_rpc,
    .listen_call = net_listen_rpc,
};

static struct net_sockets_rx_vtbl rx_vtbl = {
    .request_descq_ep_call = net_request_descq_ep,
    .register_queue_call = net_register_queue,
    .new_udp_socket_call = net_udp_socket,
    .new_tcp_socket_call = net_tcp_socket,
    .bind_call = net_bind,
    .connect_call = net_connect,
    .listen_call = net_listen,
};

static errval_t connect_cb(void *st, struct net_sockets_binding *binding)
{
    binding->rx_vtbl = rx_vtbl;
    binding->rpc_rx_vtbl = rpc_rx_vtbl;
    return SYS_ERR_OK;
}


static void export_cb(void *st, errval_t err, iref_t iref)
{
    struct netss_state* state = (struct netss_state* ) st;
    debug_printf("service_name %s: err %s \n", state->service_name, err_getstring(err));
    assert(err_is_ok(err));
    err = nameservice_register(state->service_name, iref);
    debug_printf("nameservice register err %s \n", err_getstring(err));
    assert(err_is_ok(err));
    state->exported = true;
}

/**
 * Driver initialization function. This function is called by the driver domain
 * (see also 'create_handler' in ddomain_service.c).
 * Typically through a request from the device manager.
 *
 * The init function is supposed to set `dev` to the exported service iref.
 * The init function may use `bfi->dstate` to store additional state about the device.
 *
 * \param[in]   bfi   The instance of this driver.
 * \param[in]   name  The name of this driver instance.
 * \param[in]   flags Additional flags (The exact flags supported is device/driver specific).
 * \param[in]   c     Capabilities (for registers etc.) as provided by the device manager.
 *                    The exact layout of the `c` is device specific.
 * \param[out]  dev   The service iref over which the device can be contacted.
 *
 * \retval SYS_ERR_OK Device initialized successfully.
 * \retval LIB_ERR_MALLOC_FAIL Unable to allocate memory for the driver.
 */
static errval_t init(struct bfdriver_instance *bfi, uint64_t flags, iref_t *dev)
{
    //barrelfish_usleep(10*1000*1000);
    NET_SOCK_DEBUG("Netsock init\n");

    errval_t err;

    if (bfi->argc < 4) {
        printf("%s: missing arguments! \n", bfi->argv[0]);
        return -1;
    }

    debug_printf("Net socket server started for %s.\n", bfi->argv[2]);

    char card_name[64];
    snprintf(card_name, sizeof(card_name), "%s:%s", bfi->argv[2], bfi->argv[bfi->argc - 1]);

    char *ip = NULL;
    char *netmask = NULL;
    char *gw = NULL;

    while (1) {
        int option_index = 0;
        int c;
        static struct option long_options[] = {
            {"ip",  required_argument,  0,  1},
            {"netmask",  required_argument,  0,  2},
            {"gw",  required_argument,  0,  3},
            {0, 0,  0,  0}
        };

        c = getopt_long_only(bfi->argc, bfi->argv, "", long_options, &option_index);
        if (c == -1)
            break;

        switch (c) {
        case 1:
            ip = optarg;
            break;
        case 2:
            netmask = optarg;
            break;
        case 3:
            gw = optarg;
            break;
        default:
            break;
        }
    }

    if (ip)
        printf("option ip [%s]\n", ip);
    if (netmask)
        printf("option nm [%s]\n", netmask);
    if (gw)
        printf("option gw [%s]\n", gw);

    if (ip) { // setting static IP, no DHCP
        err = oct_init();
        assert(err_is_ok(err));
        err = oct_set(NET_CONFIG_STATIC_IP_RECORD_FORMAT, inet_addr(ip), 
                      gw ? inet_addr(gw): 0, netmask ? inet_addr(netmask): 0);
        assert(err_is_ok(err));
    }

    /* connect to the network */
#ifdef POLLING
    debug_printf("Net socket server polling \n");

    err = networking_init_with_ep(card_name, bfi->caps[0], (!ip ? NET_FLAGS_DO_DHCP: 0)
                                  | NET_FLAGS_DEFAULT_QUEUE | NET_FLAGS_BLOCKING_INIT
                                  | NET_FLAGS_POLLING );
#else
    debug_printf("Net socket server using interrupts \n");
    err = networking_init_with_ep(card_name, bfi->caps[0], (!ip ? NET_FLAGS_DO_DHCP: 0)
                                  | NET_FLAGS_DEFAULT_QUEUE | NET_FLAGS_BLOCKING_INIT);
#endif
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to initialize the network");
    }

    struct descq_func_pointer f;

    f.notify = q_notify;
    f.create = q_create;
    f.destroy = q_destroy;
    f.reg = q_reg;
    f.dereg = q_dereg;
    f.control = q_control;

    char queue_name[128];
    struct netss_state* st = (struct netss_state*) malloc(sizeof(struct netss_state));

    sprintf(queue_name, "net_sockets_queue_%s", bfi->argv[2]);
    sprintf(st->service_name, "net_sockets_service_%s", bfi->argv[2]);

    err = descq_create(&exp_queue, DESCQ_DEFAULT_SIZE, queue_name,
                       true, NULL, &f);
    assert(err_is_ok(err));
    
    st->exported = false;    

    err = net_sockets_export(st, export_cb, connect_cb, get_default_waitset(),
                            IDC_EXPORT_FLAGS_DEFAULT);
    assert(err_is_ok(err));
    
    while(!st->exported) {
        event_dispatch(get_default_waitset());
    }

    return SYS_ERR_OK;
}


/**
 * Instructs driver to attach to the device.
 * This function is only called if the driver has previously detached
 * from the device (see also detach).
 *
 * \note After detachment the driver can not assume anything about the
 * configuration of the device.
 *
 * \param[in]   bfi   The instance of this driver.
 * \retval SYS_ERR_OK Device initialized successfully.
 */
static errval_t attach(struct bfdriver_instance* bfi) {
    return SYS_ERR_OK;
}

/**
 * Instructs driver to detach from the device.
 * The driver must yield any control over to the device after this function returns.
 * The device may be left in any state.
 *
 * \param[in]   bfi   The instance of this driver.
 * \retval SYS_ERR_OK Device initialized successfully.
 */
static errval_t detach(struct bfdriver_instance* bfi) {
    return SYS_ERR_OK;
}

/**
 * Instructs the driver to go in a particular sleep state.
 * Supported states are platform/device specific.
 *
 * \param[in]   bfi   The instance of this driver.
 * \retval SYS_ERR_OK Device initialized successfully.
 */
static errval_t set_sleep_level(struct bfdriver_instance* bfi, uint32_t level) {
    return SYS_ERR_OK;
}

/**
 * Destroys this driver instance. The driver will yield any
 * control over the device and free any state allocated.
 *
 * \param[in]   bfi   The instance of this driver.
 * \retval SYS_ERR_OK Device initialized successfully.
 */
static errval_t destroy(struct bfdriver_instance* bfi) {
    USER_PANIC("NIY \n");
    return SYS_ERR_OK;
}

static errval_t get_ep(struct bfdriver_instance* bfi, bool lmp, struct capref* ret_cap)
{
    debug_printf("Endpoint was requested \n");
    errval_t err;
    struct net_sockets_binding* b;
    err = net_sockets_create_endpoint(lmp? IDC_ENDPOINT_LMP: IDC_ENDPOINT_UMP, 
                                      &rx_vtbl, NULL,
                                      get_default_waitset(),
                                      IDC_ENDPOINT_FLAGS_DUMMY,
                                      &b, *ret_cap);
    
    return err;
}
/**
 * Registers the driver module with the system.
 *
 * To link this particular module in your driver domain,
 * add it to the addModules list in the Hakefile.
 */
DEFINE_MODULE(net_sockets_server_module, init, attach, detach, set_sleep_level, destroy, get_ep);
