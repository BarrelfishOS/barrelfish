#ifndef DIST2_PUBSUB_H_
#define DIST2_PUBSUB_H_

#define MAX_SUBSCRIPTIONS 255

#include <stdint.h>
#include <if/dist2_defs.h>

typedef uint64_t subscription_t;
typedef void(*subscription_handler_fn)(subscription_t id, char* object, void* state);

errval_t dist_subscribe(subscription_handler_fn, void*, subscription_t*, char*, ...);
errval_t dist_unsubscribe(subscription_t);
errval_t dist_publish(char*, ...);

//errval_t dist_publish_set(char*, ...);
//errval_t dist_get_subscribe(char*, char**);

void subscribed_message_handler(struct dist2_binding*, subscription_t, char*);


#endif /* DIST2_PUBSUB_H_ */
