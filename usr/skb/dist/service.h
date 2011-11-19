#ifndef SERVICE_H_
#define SERVICE_H_

#include <barrelfish/barrelfish.h>
#include <if/skb_defs.h>

void get_handler(struct skb_binding*, char*);
void set_handler(struct skb_binding*, char*);
void del_handler(struct skb_binding*, char*);

void subscribe(struct skb_binding*, char*, uint64_t);
void publish(struct skb_binding*, char*);
void unsubscribe(struct skb_binding*, uint64_t);

void lock_handler(struct skb_binding*, char*);
void unlock_handler(struct skb_binding*, char*);

void identify_rpc_binding(struct skb_binding*, uint64_t);
void get_identifier(struct skb_binding*);

void event_server_init(void);

#endif /* SERVICE_H_ */
