#ifndef DIST2_SERVICE_H_
#define DIST2_SERVICE_H_

#include <barrelfish/barrelfish.h>
#include <if/dist2_defs.h>
#include <if/dist_event_defs.h>

#define BUFFER_SIZE (32 * 1024)

struct dist_reply_state;

struct skb_writer {
    char buffer[BUFFER_SIZE];
    size_t length;
};

struct dist_query_state {
    struct skb_writer stdout;
    struct skb_writer stderr;
    int exec_res;
};

typedef void(*dist_reply_handler_fn)(struct dist2_binding*, struct dist_reply_state*);

struct dist_reply_state {
    struct dist2_binding* binding;
    dist_reply_handler_fn reply;

    struct dist_query_state query_state;
    bool return_record;
    errval_t error;

    dist2_trigger_t trigger;
    // For watch()
    uint64_t watch_id;
    uint64_t client_id;

    struct dist_reply_state *next;
};

errval_t new_dist_reply_state(struct dist_reply_state**, dist_reply_handler_fn);

void get_names_handler(struct dist2_binding*, char*);
void get_handler(struct dist2_binding*, char*);
void set_handler(struct dist2_binding*, char*, uint64_t, bool);
void del_handler(struct dist2_binding*, char*);
void exists_handler(struct dist2_binding*, char*, dist2_trigger_t);
void exists_not_handler(struct dist2_binding* b, char*, dist2_trigger_t);
void watch_handler(struct dist2_binding* b, char* query, uint64_t mode, dist2_binding_type_t type, uint64_t client_id);

void subscribe_handler(struct dist2_binding*, char*, uint64_t);
void publish_handler(struct dist2_binding*, char*);
void unsubscribe_handler(struct dist2_binding*, uint64_t);

void get_identifier(struct dist2_binding*);
void identify_binding(struct dist2_binding*, uint64_t, dist2_binding_type_t);

#endif /* DIST2_SERVICE_H_ */
