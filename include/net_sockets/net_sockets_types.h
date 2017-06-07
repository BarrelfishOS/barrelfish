#ifndef NET_SOCKETS_TYPES_H
#define NET_SOCKETS_TYPES_H

#include <barrelfish/barrelfish.h>

typedef uint32_t host_address_t;

struct net_socket;

typedef void (*net_received_callback_t)(void *user_state, struct net_socket *socket, void *data, size_t size, host_address_t ip_address, uint16_t port);
typedef void (*net_sent_callback_t)(void *user_state, struct net_socket *socket, void *data);
typedef void (*net_connected_callback_t)(void *user_state, struct net_socket *socket);
typedef void (*net_accepted_callback_t)(void *user_state, struct net_socket *accepted_socket, host_address_t ip_address, uint16_t port);

struct net_buffer {
    net_received_callback_t user_callback;
    uint64_t user_state;
    uint32_t size;
    uint32_t descriptor;
    host_address_t host_address;
    uint16_t port;
};

#endif
