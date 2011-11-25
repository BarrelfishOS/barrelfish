#ifndef DIST2_SERVICE_H_
#define DIST2_SERVICE_H_

#include <barrelfish/barrelfish.h>
#include <if/dist_defs.h>
#include <if/dist_event_defs.h>

#define BUFFER_SIZE (32 * 1024)

struct dist_reply_state;

struct dist_query_state {
    char output_buffer[BUFFER_SIZE];
    char error_buffer[BUFFER_SIZE];
    int output_length;
    int error_output_length;
    int exec_res;
};

typedef void(*dist_reply_handler_fn)(struct dist_binding*, struct dist_reply_state*);

struct dist_reply_state {
    struct dist_query_state skb;
    dist_reply_handler_fn rpc_reply;
    errval_t error;

    struct dist_reply_state *next;
};

void get_names_handler(struct dist_binding*, char*);
void get_handler(struct dist_binding*, char*);
void set_handler(struct dist_binding*, char*);
void del_handler(struct dist_binding*, char*);
void exists_handler(struct dist_binding*, char*, bool, bool);

void subscribe_handler(struct dist_binding*, char*, uint64_t);
void publish_handler(struct dist_binding*, char*);
void unsubscribe_handler(struct dist_binding*, uint64_t);

void lock_handler(struct dist_binding*, char*);
void unlock_handler(struct dist_binding*, char*);

void identify_rpc_binding(struct dist_binding*, uint64_t);
void get_identifier(struct dist_binding*);
void identify_events_binding(struct dist_event_binding*, uint64_t);

#endif /* DIST2_SERVICE_H_ */
