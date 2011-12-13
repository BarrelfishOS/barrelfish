#include "trigger.h"

static struct dist_trigger trigger_table[MAX_TRIGGERS] = {{ NULL, NULL, 0 }};

static struct dist_trigger* find_free_slot(size_t* id)
{
    for(size_t i=0; i <  MAX_TRIGGERS; i++) {
        if(trigger_table[i].trigger == NULL) {
            return &trigger_table[i];
        }
    }

    *id = 0;
    return NULL;
}

// TODO locking for concurrent access!
errval_t dist_register_trigger(trigger_handler_fn trigger, void* st, size_t* id)
{
    assert(trigger != NULL);

    struct dist_trigger* t = find_free_slot(id);
    if(t == NULL) {
        return DIST2_ERR_NO_TRIGGER_SLOT;
    }

    t->state = st;
    t->trigger = trigger;
    t->version++;

    return SYS_ERR_OK;
}

errval_t dist_unregister_trigger(size_t id)
{
    assert(id < MAX_TRIGGERS);

    trigger_table[id].state = NULL;
    trigger_table[id].trigger = NULL;

    return SYS_ERR_OK;
}
