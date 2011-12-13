#ifndef DIST2_TRIGGER_H_
#define DIST2_TRIGGER_H_

#include <barrelfish/barrelfish.h>

#define MAX_TRIGGERS 256

typedef void(*trigger_handler_fn)(char* object, void* state);

struct dist_trigger {
    trigger_handler_fn trigger;
    void* state;
    uint64_t version;
};

errval_t dist_register_trigger(trigger_handler_fn, void*, size_t*);
errval_t dist_unregister_trigger(size_t);


#endif /* DIST2_TRIGGER_H_ */
