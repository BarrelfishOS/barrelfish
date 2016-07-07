/**
 * \file
 * \brief Contains handler functions for server-side octopus interface RPC call.
 */

/*
 * Copyright (c) 2009, 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <inttypes.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <skb/skb.h> // read list
#include <int_route/int_route_server.h>
#include <int_route/int_route_debug.h>

#include <if/int_route_service_defs.h>

struct controller_driver {
   char * lbl; // Label used in the SKB
   void * rpc_client; // RPC client to talk to the instance
   struct controller_driver * next;
};

struct controller_driver * controller_head;

//static struct controller_driver * find_controller(char * lbl, struct controller_driver *d){
//    if(d == NULL){
//        return NULL;
//    } else if(strcmp(d->lbl, lbl) == 0){
//        return d;
//    } else {
//        return find_controller(lbl, d->next);
//    }
//}

static struct controller_driver * add_controller(char * lbl, struct controller_driver * d){
    struct controller_driver * cur;
    if(controller_head == NULL){
        controller_head = malloc(sizeof(struct controller_driver));
        cur = controller_head;
    } else if(d != NULL && d->next != NULL){
        return add_controller(lbl, d->next);
    } else {
        assert(d != NULL);
        d->next = malloc(sizeof(struct controller_driver));
        cur = d->next;
    }

    // Initialize cur
    cur->next = NULL;
    cur->rpc_client = NULL;
    cur->lbl = malloc(strlen(lbl) + 1);
    strcpy(cur->lbl, lbl);
    return cur;
}

static void add_controller_call(struct int_route_service_binding *b, int_route_service_pci_address_t addr,
        int_route_service_controller_type_t type) {
    INT_DEBUG("add_controller_call enter: addr(%d,%d,%d) type: %s\n", addr.bus,
            addr.device, addr.function,
            type == int_route_service_CONTROLLER_MSI ? "MSI" : "MSI-x");

    uint16_t intbase = 0;
    INT_DEBUG("Returning intbase=%"PRIu16"\n",intbase);
    b->rx_vtbl.add_controller_response(b, intbase, SYS_ERR_OK);
}

static void route_call(struct int_route_service_binding *b,
        struct capref intsource, struct capref intdest){
    INT_DEBUG("route_call enter\n");
    b->rx_vtbl.route_response(b, SYS_ERR_OK);
}

static struct int_route_service_rx_vtbl rx_vtbl = {
        .add_controller_call = add_controller_call,
        .route_call = route_call

};

static errval_t rpc_connect_cb(void *st, struct int_route_service_binding *b) {
    INT_DEBUG("rpc_connect_cb");
    b->st = NULL;
    b->rx_vtbl = rx_vtbl;
    return SYS_ERR_OK;

}

static void export_cb(void *st, errval_t err, iref_t iref){
    INT_DEBUG("export_cb\n");
    assert(err_is_ok(err));

    err = nameservice_register("int_route_service", iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_register failed");
    };
}

//static bool all_driver_connected(void){
//    struct controller_driver * cur = controller_head;
//    while(cur != NULL){
//        if(cur->rpc_client == NULL){
//            return false;
//        }
//    }
//    return true;
//}

// This function must be called after ACPI discovery is done
// it instantiates controller in the skb.
errval_t int_route_service_init_controller(void){
    INT_DEBUG("int_route_service_init_controller. No-op.\n");
    if(0) add_controller(NULL,NULL);
    return SYS_ERR_OK;

    //skb_execute("print_controller_driver.");
    //char * out = skb_get_output();
    //INT_DEBUG("skb output: %s\n", out);

    //// Parse output and start controller
    //char binary[255];
    //char lbl[255];
    //char class[255];
    //int inlo,inhi,outlo,outhi;
    //char remainder[1024];

    //for(char * pos = out; pos - 1 != NULL && *pos != 0; pos = strchr(pos,'\n')+1 ) {
    //    int res = sscanf(pos, "%[^,],%[^,],%[^,],%d,%d,%d,%d,%s",
    //            binary, lbl, class, &inlo, &inhi, &outlo, &outhi, remainder);
    //    if(res != 8) {
    //        printf("WARNING: Invalid SKB response. (%d)\n", __LINE__);
    //        continue;
    //    }
    //    printf("Scanned %d args: %s %s %s  remainder: %s\n", res, binary, lbl, class, remainder);
    //    if(strcmp(class,"ioapic_iommu") == 0 || strcmp(class,"ioapic") == 0){
    //        // parse ioapic remainder
    //        uint64_t mem_base;
    //        res = sscanf(remainder, "%"SCNu64, &mem_base);
    //        if(res != 1) {
    //            printf("WARNING: Invalid SKB response. (%d)\n", __LINE__);
    //            continue;
    //        }

    //        add_controller(lbl, controller_head);

    //    } else if (strcmp(class,"iommu") == 0){
    //        // parse iommu remainder
    //        uint64_t mem_base;
    //        res = sscanf(remainder, "%*d,%*d,%"SCNu64, &mem_base);
    //        if(res != 1) {
    //            printf("WARNING: Invalid SKB response. (%d)\n", __LINE__);
    //            continue;
    //        }
    //        add_controller(lbl, controller_head);
    //    }
    //}

    //return 0;
}


// The main function of this service
errval_t int_route_service_init(void)
{
    INT_DEBUG("int_route_service_init\n");
    // We need skb connection
    skb_client_connect();

    // Export our service
    int_route_service_export(NULL, export_cb, rpc_connect_cb, get_default_waitset(),
        IDC_EXPORT_FLAGS_DEFAULT);
    return SYS_ERR_OK;
}
