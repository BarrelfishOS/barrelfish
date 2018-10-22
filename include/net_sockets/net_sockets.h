#ifndef NET_SOCKETS_H
#define NET_SOCKETS_H

#include <net_sockets/net_sockets_types.h>

void * net_alloc(size_t size);
void net_free(void *buffer);

struct net_socket {
    uint32_t descriptor;
    bool is_closing;
    net_received_callback_t received;
    net_sent_callback_t sent;
    net_connected_callback_t connected;
    net_accepted_callback_t accepted;
    net_closed_callback_t closed;
    void *user_state;

    struct in_addr bound_address;
    uint16_t bound_port;
    struct in_addr connected_address;
    uint16_t connected_port;

    struct net_socket *prev, *next;
};

struct net_socket * net_udp_socket(void);
struct net_socket * net_tcp_socket(void);
void net_set_user_state(struct net_socket *socket, void *user_state);
void net_close(struct net_socket *socket);

errval_t net_bind(struct net_socket *socket, struct in_addr ip_address, uint16_t port);
errval_t net_listen(struct net_socket *socket, uint8_t backlog);
errval_t net_print_log(void);

// data must be allocated using net_alloc, it can be freed, when on_sent is called (i.e. actually sent)
errval_t net_send(struct net_socket *socket, void *data, size_t size);
errval_t net_send_to(struct net_socket *socket, void *data, size_t size, struct in_addr ip_address, uint16_t port);

errval_t net_connect(struct net_socket *socket, struct in_addr ip_address, uint16_t port, net_connected_callback_t cb);

void net_set_on_received(struct net_socket *socket, net_received_callback_t cb);
void net_set_on_sent(struct net_socket *socket, net_sent_callback_t cb);
void net_set_on_accepted(struct net_socket *socket, net_accepted_callback_t cb);
void net_set_on_closed(struct net_socket *socket, net_closed_callback_t cb);


errval_t net_sockets_init(void);
errval_t net_sockets_init_with_card(const char* cardname);
errval_t net_sockets_init_with_ep(struct capref ep);

#endif
