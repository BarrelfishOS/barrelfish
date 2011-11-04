#ifndef SERVICE_H_
#define SERVICE_H_

#include <barrelfish/barrelfish.h>
#include <if/skb_defs.h>

void get_object(struct skb_binding *b, char *query);
void set_object(struct skb_binding *b, char *query);

#endif /* SERVICE_H_ */
