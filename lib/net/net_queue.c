/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <net/net_queue.h>
#include <pci/pci_types.h>
#include <bench/bench.h>
#include <if/if_types.h>
#include "networking_internal.h"
#include "net_queue_internal.h"

static errval_t create_loopback_queue(const char* cardname, inthandler_t interrupt, 
                                      struct capref* ep, uint64_t *queueid,
                                      bool default_q, bool poll, struct capref* filter_ep,
                                      struct devq **retqueue)
{
    errval_t err;

    debug_printf("net: creating loopback queue.\n");

    *queueid = 0;
    err = loopback_queue_create((struct loopback_queue **)retqueue);
    if (err_is_fail(err)) {
        return err;
    }
    *filter_ep = NULL_CAP;
    return SYS_ERR_OK;
}

static errval_t create_driver_queue(const char* cardname, inthandler_t interrupt, 
                                    struct capref* ep, uint64_t *queueid,
                                    bool default_q, bool poll, struct capref* filter_ep,
                                    struct devq **retqueue)
{
    *filter_ep = NULL_CAP;
    *queueid = 0;
    return SYS_ERR_OK;
}

static errval_t create_e1000_queue(const char* cardname, inthandler_t interrupt, 
                                   struct capref* ep, uint64_t *queueid,
                                   bool default_q, bool poll, struct capref* filter_ep,
                                   struct devq **retqueue)
{
    errval_t err;
    if (strncmp(cardname, "", strlen("")) == 0) {
        if (cardname[6] != ':') {
            return DEVQ_ERR_INIT_QUEUE;
        }
    }

    struct pci_addr addr;
    struct pci_id id;
    struct pci_class cls;

    err = pci_deserialize_octet((char*) cardname+7, &addr, &id, &cls);     
    if (err_is_fail(err)) {
        printf("%s \n", cardname+7);
        return DEVQ_ERR_INIT_QUEUE;
    }

    struct net_state* st = get_default_net_state();
    // disable HW filter since the card does not have them
    st->hw_filter = false;
    *filter_ep = NULL_CAP;

    return e1000_queue_create((struct e1000_queue**)retqueue, ep, id.vendor, id.device,
                              addr.bus, addr.device, addr.function, poll? 0: 1, interrupt);
}

// cardname - "mlx4:vendor:deviceid:bus:device:function"
static errval_t create_mlx4_queue(const char* cardname, inthandler_t interrupt, 
                                  struct capref* ep, uint64_t *queueid,
                                  bool default_q, bool poll, struct capref* filter_ep, 
                                  struct devq **retqueue)
{
    errval_t err;

    struct pci_addr addr;
    struct pci_id id;
    struct pci_class cls;

    if (strncmp(cardname, "", strlen("")) != 0) {
        if (cardname[4] != ':') {
            return DEVQ_ERR_INIT_QUEUE;
        }

        err = pci_deserialize_octet((char*) cardname+5, &addr, &id, &cls);     
        if (err_is_fail(err)) {
            return DEVQ_ERR_INIT_QUEUE;
        }
    } else {
        return DEVQ_ERR_INIT_QUEUE;
    }


    struct net_state* st = get_default_net_state();
    // disable HW filter since the card does not have them
    st->hw_filter = false;
    *filter_ep = NULL_CAP;
    return mlx4_queue_create((struct mlx4_queue**)retqueue, id.vendor, id.device,
                              addr.bus, addr.device, addr.function, 1, interrupt);
}

static errval_t create_e10k_queue(const char* cardname, inthandler_t interrupt, 
                                  struct capref* ep, uint64_t *queueid,
                                  bool default_q, bool poll, struct capref* filter_ep,
                                  struct devq **retqueue)
{
    errval_t err;
    struct net_state* st = get_default_net_state();
    // enable HW filter since they are enabled by default by the driver
    st->hw_filter = true;

    uint32_t vendor, deviceid, bus, device, function;
    if (strncmp(cardname, "", strlen("")) != 0) {
        if (cardname[4] != ':') {
            return DEVQ_ERR_INIT_QUEUE;
        }
        unsigned parsed = sscanf(cardname + 5, "%x:%x:%x:%x:%x", &vendor,
                                 &deviceid, &bus, &device, &function);
        if (parsed != 5) {
            return DEVQ_ERR_INIT_QUEUE;
        }

    }

    if (driverkit_iommu_present(NULL)) {
        err = e10k_queue_create((struct e10k_queue**)retqueue, interrupt,
                                ep, bus, function, deviceid, device, 
                                true/*virtual functions*/,
                                !poll, /* user interrupts*/
                                default_q);
    } else {
        printf("Create queue no iommu EP \n");
        err = e10k_queue_create((struct e10k_queue**)retqueue, interrupt,
                                ep, bus, function, deviceid, device, 
                                false/*virtual functions*/,
                                !poll, /* user interrupts*/
                                default_q);
    }
    if (err_is_fail(err)) {
        return err;
    }

    assert(retqueue != NULL);
    *queueid = e10k_queue_get_id((struct e10k_queue*)*retqueue);
    e10k_queue_get_netfilter_ep((struct e10k_queue*)*retqueue, filter_ep);
    return err;
}

static errval_t create_sfn5122f_queue(const char* cardname, inthandler_t interrupt, 
                                      struct capref* ep, uint64_t *queueid, 
                                      bool default_q, bool poll, struct capref* filter_ep,
                                      struct devq **retqueue)
{
    errval_t err;
    struct net_state* st = get_default_net_state();
    // enable HW filter since they are enabled by default by the driver
    st->hw_filter = true;
    err = sfn5122f_queue_create((struct sfn5122f_queue**)retqueue, interrupt,
                                ep, false /*userlevel network feature*/,
                                !poll /* user interrupts*/,
                                default_q);
    if (err_is_fail(err)) {
        return err;
    }

    *queueid = sfn5122f_queue_get_id((struct sfn5122f_queue*)*retqueue);
    sfn5122f_queue_get_netfilter_ep((struct sfn5122f_queue*)*retqueue, filter_ep);
    return err;
}


typedef errval_t (*queue_create_fn)(const char*, inthandler_t, struct capref*, 
                                    uint64_t*, bool, bool, struct capref* filter_ep, struct devq **);

typedef struct bench_ctl* (*get_bench_data_fn)(struct devq*, uint8_t);

struct networking_card
{
    char *cardname;
    queue_create_fn createfn;
    get_bench_data_fn benchfn;
    enum endpoint_types iftype;
} networking_cards [] = {
    { "loopback", create_loopback_queue, NULL, IF_TYPE_DUMMY},
    { "driver", create_driver_queue, NULL, IF_TYPE_DUMMY},
    { "e1000n", create_e1000_queue, NULL, IF_TYPE_E1000_DEVIF},
    { "mlx4", create_mlx4_queue, NULL, IF_TYPE_DUMMY},
    { "e10k", create_e10k_queue, e10k_get_benchmark_data, IF_TYPE_E10K_VF},
    { "sfn5122f", create_sfn5122f_queue, sfn5122f_get_benchmark_data, IF_TYPE_SFN5122F_DEVIF},
    { NULL, NULL, NULL, IF_TYPE_DUMMY}
};

/**
 * @brief creates a queue to the given card and the queueid
 *
 * @param interrupt interrupt handler
 * @param cardname  network card to create the queue for
 * @param ep        endpoint to nic, possibly Null
 * @param queueid   queueid of the network card
 * @param default_q get the default queue (most of the time queue 0)
 * @param poll      Is the queue polled or are interrupts used
 * @param filter_ep returns the endpoint to the netfilter interface of this queue
 * @param retqueue  returns the pointer to the queue
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t net_queue_internal_create(inthandler_t interrupt, const char *cardname,
                                   struct capref* ep, uint64_t* queueid, bool default_q, 
                                   bool poll, struct capref* filter_ep, struct devq **retqueue)
{
    errval_t err;
    struct networking_card *nc = networking_cards;

    struct endpoint_identity epid;
    if (ep != NULL) {
        err = invoke_endpoint_identify(*ep, &epid);
        if (err_is_ok(err)) {
            while(nc->cardname != NULL) {
                if (nc->iftype == epid.iftype) {
                    debug_printf("Init queue %s using EP \n", nc->cardname);
                    return nc->createfn(cardname, interrupt, ep, queueid, default_q,
                                        poll, filter_ep, retqueue);
                }
                nc++;
            }
            
        }
    
    }   

    if (cardname != NULL) {
        nc = networking_cards;
        while(nc->cardname != NULL) {
            if (strncmp(cardname, nc->cardname, strlen(nc->cardname)) == 0) {
                return nc->createfn(cardname, interrupt, ep, queueid, default_q,
                                    poll, filter_ep, retqueue);
            }
            nc++;
        }

        debug_printf("net: ERROR unknown queue. card='%s', queueid=%" PRIu64 "\n",
                    cardname, *queueid);
        return -1;
    }

    debug_printf("net: ERROR unknown queue type \n");

    return -1;
}


/**
 * @brief creates a queue to the given card and the queueid
 *
 * @param interrupt interrupt handler
 * @param cardname  network card to create the queue for
 * @param ep        endpoint to NIC driver
 * @param queueid   queueid of the network card
 * @param poll      Is the queue polled or are interrupts used
 * @param filter_ep returns the endpoint to the netfilter interface of this queue
 * @param retqueue  returns the pointer to the queue
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t net_queue_create(inthandler_t interrupt, const char *cardname, struct capref* ep,
                          uint64_t* queueid, bool poll, struct capref* filter_ep, struct devq **retqueue)
{
    return net_queue_internal_create(interrupt, cardname, ep, queueid, false, poll, filter_ep, retqueue);
}

struct bench_ctl* net_queue_get_bench_data(struct devq* q, const char* name, uint8_t type)
{
    struct networking_card *nc = networking_cards;
    while(nc->cardname != NULL) {
        if (strncmp(name, nc->cardname, strlen(nc->cardname)) == 0) {
            if (nc->benchfn != NULL) {
                return nc->benchfn(q, type);
            }
        }
        nc++;
    }

    return NULL;
}
