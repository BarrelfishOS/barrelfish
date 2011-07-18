/**
 * \file
 * \brief Chips framework implementation
 */

/*
 * Copyright (c) 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#include <stdio.h>
#include <stdlib.h>
#include <barrelfish/barrelfish.h>
#include <trace/trace.h>
#include <if/nameservice_defs.h>
#include <if/monitor_defs.h>

#include "hashtable.h"
#include "multimap.h"
#include "filter.h"
#include "framework.h"

// enable to check for regression
// in hashtable and filter implementation
//#define CHIPS_TESTS_ENABLED
//#define CHIPS_DEBUG_REGISTRY

// the service registry
static struct multimap *registry;

// pending (blocked) lookups on the registry
struct pending_lookup {
    char *iface;
    struct nameservice_binding *b;
    struct pending_lookup *next;
};

static struct pending_lookup *pending_lookups = NULL;

static inline struct service_reference*
get_service_reference(char* key)
{
    void *val;
    registry->get_first(registry, key, &val);
    return (struct service_reference *) val;
}

// walk queue of pending lookups, sending replies for any that now match
static void process_pending_lookups(void)
{
    struct pending_lookup *p, *prevp, *nextp;
    struct service_reference *ref;
    errval_t err;

    for (p = pending_lookups, prevp = NULL; p != NULL; p = nextp) {
        nextp = p->next;

        ref = get_service_reference(p->iface);
        if (ref != NULL) { // found entry: reply and remove from queue
            assert(p != NULL);
            printf("chips: notifying client about %s\n", p->iface);
            err = p->b->tx_vtbl.wait_for_service_reference_response(p->b,
                                NOP_CONT, (nameservice_srvref_t)(uintptr_t)ref);
            assert(err_is_ok(err)); // XXX

            if (p == pending_lookups) {
                assert(prevp == NULL);
                pending_lookups = nextp;
            } else {
                assert(prevp != NULL);
                prevp->next = nextp;
            }

            free(p->iface);
            free(p);
        } else {
            prevp = p;
        }
    }
}


static void register_service_handler(struct nameservice_binding *b,
                                     iref_t iref, char *iface)
{
    struct dictionary *dict = NULL; // XXX: not yet supported by flounder
    struct service_registration *reg;
    struct service_reference *ref;
    errval_t err;
    int r;

#ifdef CHIPS_DEBUG_REGISTRY
    printf("registering %s with properties\n", iface);
    print_hashtable(stderr, (struct hashtable_t *) dict);
#endif

    reg = malloc(sizeof(struct service_registration));
    assert(reg != NULL);

    ref = malloc(sizeof(struct service_reference));
    assert(ref != NULL);

    // populate service reference
    ref->iface = iface;
    ref->dict = dict;
    ref->service = iref;

    // populate service registration
    reg->ref = ref;

    r = registry->put(registry, iface, ref);
    assert(r == SYS_ERR_OK);

#ifdef CHIPS_DEBUG_REGISTRY
    printf("registry after registration:\n");
    print_hashtable(stdout, (struct hashtable_t *) registry);
#endif

    // send back the service registration handle
    // XXX: unsafe to send pointer to our local state!
    err = b->tx_vtbl.register_service_response(b, NOP_CONT,
                                    (nameservice_reghandle_t)(uintptr_t)reg);
    assert(err_is_ok(err)); // XXX

    process_pending_lookups();
}

static void unregister_service_handler(struct nameservice_binding *b,
                                       nameservice_reghandle_t reghandle)
{
    struct service_registration *reg = (struct service_registration *)(uintptr_t)reghandle;
    errval_t err;
    int r;

    // remove from the registry
    r = registry->remove(registry, reg->ref->iface, (void*) reg->ref);

    free(reg->ref);
    free(reg);

#ifdef CHIPS_DEBUG_REGISTRY
    printf("registry after deregistration:\n");
    print_hashtable(stdout, (struct hashtable_t *) registry);
#endif

    err = b->tx_vtbl.unregister_service_response(b, NOP_CONT);
    assert(err_is_ok(err)); // XXX
}

static void get_service_reference_handler(
    struct nameservice_binding *b, char *iface)
{
    struct service_reference *ref;
    errval_t err;

    // get the service reference from the registry
    // can be a NULL pointer if there is no such service
    ref = get_service_reference(iface);
    free(iface);

    err = b->tx_vtbl.get_service_reference_response(b, NOP_CONT,
                                        (nameservice_srvref_t)(uintptr_t)ref);
    assert(err_is_ok(err)); // XXX
}

static void wait_for_service_reference_handler(
    struct nameservice_binding *b, char *iface)
{
    struct service_reference *ref;
    errval_t err;

    // get the service reference from the registry
    // can be a NULL pointer if there is no such service
    ref = get_service_reference(iface);

    // if we didn't find anything, add it to the pending lookups queue
    if (ref == NULL) {
        printf("chips: client waiting for %s\n", iface);
        struct pending_lookup *pending = malloc(sizeof(struct pending_lookup));
        assert(pending != NULL);
        pending->iface = iface;
        assert(b != NULL);
        pending->b = b;
        pending->next = pending_lookups;
        pending_lookups = pending;
    } else {
        // reply with existing entry
        free(iface);
        err = b->tx_vtbl.wait_for_service_reference_response(b, NOP_CONT,
                                        (nameservice_srvref_t)(uintptr_t)ref);
        assert(err_is_ok(err)); // XXX
    }
}

#if 0
static void getservice_references_handler(
    struct nameservice_binding *b, char *iface, char *filter_str,
    uint64_t max_count)

{
    filter_t filter = create_filter(filter_str);

#ifdef CHIPS_DEBUG_REGISTRY
    printf("looking for %ld %s services with fulfilling %s\n", max_count, iface,
           filter_str);
#endif
    free(filter_str);

    struct service_reference **refs = malloc(max_count * sizeof(struct service_reference*));
    assert(refs != NULL);

    // get the candidate list
    // TODO: this is not correct. Cut the result set and not the candidate set!
    uint64_t candidate_count = registry->get_all(registry, iface, (void**) refs, max_count);
#ifdef CHIPS_DEBUG_REGISTRY
    printf("I have %ld candidates\n", candidate_count);
#endif

    for (uint64_t i=0; i<candidate_count; i++) {
        ref = refs[i];

#ifdef CHIPS_DEBUG_REGISTRY
        printf("checking %s with properties\n", iface);
        printf("filter ");
        print_filter(stdout, filter);
        print_hashtable(stdout, (struct hashtable_t *) registry);
#endif

        assert(ref->dict != NULL);
        assert(filter != NULL);
        if (match_filter(ref->dict, filter)) {
#ifdef CHIPS_DEBUG_REGISTRY
            printf("->match\n");
#endif
            XXX: add ref to reply set
        }
    }

    free(iface);
    destroy_filter(filter);
    free(refs);

    err = b->tx_vtbl.get_services_references(st->conn, XXX);
    assert(err_is_ok(err)); // XXX
}
#endif

static void get_service_handler(struct nameservice_binding *b,
                                nameservice_srvref_t srvref)
{
    struct service_reference *ref = (void *)(uintptr_t)srvref;
    errval_t err;

    // XXX TODO: check that the ref is still valid
    // is easier when the service ID is implemented as
    // a default attribute
    err = b->tx_vtbl.get_service_response(b, NOP_CONT, ref->service);
    assert(err_is_ok(err)); // XXX
}

static struct nameservice_rx_vtbl nameservice_rx_vtbl = {
    .register_service_call = register_service_handler,
    .unregister_service_call = unregister_service_handler,
    .get_service_reference_call = get_service_reference_handler,
    .wait_for_service_reference_call = wait_for_service_reference_handler,
    .get_service_call = get_service_handler,
};

static void export_handler(void *st, errval_t err, iref_t iref)
{
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "nameservice export failed!");
        abort();
    }

    trace_event(TRACE_SUBSYS_CHIPS, TRACE_EVENT_CHIPS_LISTENCB, 0);

    struct monitor_binding *mb = get_monitor_binding();
    err = mb->tx_vtbl.set_name_iref_request(mb, NOP_CONT, iref);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "failed to send set_name_iref_request to monitor");
        // XXX: cleanup
    }
}

static errval_t connect_handler(void *st, struct nameservice_binding *b)
{
    b->rx_vtbl = nameservice_rx_vtbl;
    return SYS_ERR_OK;
}

#ifdef CHIPS_TESTS_ENABLED
static void registry_tests(void)
{
    // put into the ht
    int r = registry->h.d.put_string((struct dictionary *) registry, "test", "value");
    assert(r == 0);
    assert(registry->h.entry_count == 1);

    // get from the ht
    char* val;
    uint8_t type = registry->h.d.get((struct dictionary *) registry, "test", (void**) &val);
    assert(val != NULL);
    assert(type == TYPE_STRING);
    assert(strcmp(val, "value") == 0);

    filter_t filter1 = create_filter("(|(a>=10)(test=value))");
    filter_t filter2 = create_filter("(test=foo)");


    print_filter(stderr, filter1);
    print_filter(stderr, filter2);
    assert(match_filter((struct dictionary *) registry, filter1) == 1);
    assert(match_filter((struct dictionary *) registry, filter2) == 0);

    destroy_filter(filter1);
    destroy_filter(filter2);

    r = registry->h.d.put_word((struct dictionary *) registry, "test2", 4711);
    assert(r == 0);
    assert(registry->h.entry_count == 2);

    uintptr_t val2;
    type = registry->h.d.get((struct dictionary *) registry, "test2", (void**) &val2);
    assert(type == TYPE_WORD);
    assert(val2 == 4711);

    r = registry->h.d.remove((struct dictionary *) registry, "test");
    assert(r == 0);

    type = registry->h.d.get((struct dictionary *) registry, "test", (void**) &val);
    assert(val == NULL);


    filter_t filter3 = create_filter("(|(a>=10)(test2<=5))");
    filter_t filter4 = create_filter("(&(test2=*)(test2>=5))");

    assert(match_filter((struct dictionary *) registry, filter3) == 0);
    assert(match_filter((struct dictionary *) registry, filter4) == 1);

    destroy_filter(filter3);
    destroy_filter(filter4);

    r = registry->h.d.remove((struct dictionary *) registry, "test2");
    assert(r == 0);

    type = registry->h.d.get((struct dictionary *) registry, "test2", (void**) &val);
    assert(val == NULL);
}
#endif

int main(int argc, char **argv)
{
    errval_t err;

    // create the service registry
    registry = create_multimap();
    assert(registry != NULL);

#ifdef CHIPS_TESTS_ENABLED
    registry_tests();
#endif

    err = nameservice_export(NULL, export_handler, connect_handler,
                             get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);
    assert(err_is_ok(err));

    messages_handler_loop();
    return EXIT_FAILURE;
}
