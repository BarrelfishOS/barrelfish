#ifndef SERVICE_H_
#define SERVICE_H_

#include <barrelfish/barrelfish.h>
#include <if/skb_defs.h>


void get_object(struct skb_binding*, char*);
void set_object(struct skb_binding*, char*);

void subscribe(struct skb_binding*, char*, uint64_t);
void publish(struct skb_binding*, char*);

void event_server_init(void);
void identify_rpc_binding(struct skb_binding*, uint64_t);
void unsubscribe(struct skb_binding*, uint64_t);

#endif /* SERVICE_H_ */
