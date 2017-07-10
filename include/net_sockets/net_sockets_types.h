#ifndef NET_SOCKETS_TYPES_H
#define NET_SOCKETS_TYPES_H

#include <barrelfish/barrelfish.h>
#include <netinet/in.h>

struct net_socket;

typedef void (*net_received_callback_t)(void *user_state, struct net_socket *socket, void *data, size_t size, struct in_addr ip_address, uint16_t port);
typedef void (*net_sent_callback_t)(void *user_state, struct net_socket *socket, void *data, size_t size);
typedef void (*net_connected_callback_t)(void *user_state, struct net_socket *socket);
typedef void (*net_accepted_callback_t)(void *user_state, struct net_socket *accepted_socket);
typedef void (*net_closed_callback_t)(void *user_state, struct net_socket *socket);

typedef enum {
    NET_EVENT_RECEIVE = 1,  // to a server to receive data
    NET_EVENT_RECEIVED, // to a client with received data
    NET_EVENT_SEND, // to a server with data to send
    NET_EVENT_SENT, // to a client with sent data
    NET_EVENT_ACCEPT, // to a client with a new accepted descriptor
    NET_EVENT_CLOSE, // to a server requesting closing, no more frames will be sent to a server
    NET_EVENT_CLOSED // to a client confirming closing, no more frames will be sent to a client
} net_buffer_event_t;

struct net_buffer {
    uint32_t size;
    uint32_t descriptor, accepted_descriptor;
    struct in_addr host_address;
    uint16_t port;
};

#endif
