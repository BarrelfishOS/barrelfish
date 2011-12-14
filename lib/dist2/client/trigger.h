#ifndef DIST2_TRIGGER_H_
#define DIST2_TRIGGER_H_

#include <barrelfish/barrelfish.h>

#include <if/dist2_defs.h>

#define MAX_TRIGGERS 256

typedef void(*trigger_handler_fn)(char* object, void* state);

void trigger_handler(struct dist2_binding*, uint64_t, uint64_t, char*);

#endif /* DIST2_TRIGGER_H_ */
