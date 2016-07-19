/**
 * \file
 * \brief Interrupt Service Controller client. Registers itself as a controller for
 * pci link devices on the int_route server.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "pcilnk_controller_client.h"

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/cpu_arch.h>
#include <barrelfish/nameservice_client.h>
#include <acpi.h>
#include <mm/mm.h>

#include <skb/skb.h>
#include <octopus/getset.h>
#include <trace/trace.h>

#include "acpi_debug.h"
#include "acpi_shared.h"

#include "interrupts.h"

#include <if/int_route_controller_defs.h>


static void add_mapping(struct int_route_controller_binding *b, char *label, size_t l1,
        char *class, size_t l2,
        int_route_controller_int_message_t from,
        int_route_controller_int_message_t to) {

    errval_t err = SYS_ERR_OK;

    label[l1] = '\0';
    class[l2] = '\0';

    ACPI_DEBUG("pcilnk add_mapping: label:%s, class:%s (%"PRIu64", %"PRIu64") to "
            "(%"PRIu64", %"PRIu64")\n", label, class, from.addr, from.msg, to.addr, to.msg);


    // Get acpiName from SKB
    char acpiName[255];
    char acpiName2[255];
    skb_execute_query("pcilnk_index(AcpiName, %s), writeln(AcpiName).", label);
    if(strlen(skb_get_output()) > sizeof(acpiName)){
        debug_printf("Buffer overflow\n");
        err = SYS_ERR_INVALID_USER_BUFFER;
        goto out;
    }
    strncpy(acpiName, skb_get_output(), sizeof(acpiName));
    *strchr(acpiName,'\n') = '\0';
    // Escape backslash
    for(int i=0, desti=0; i<strlen(acpiName); i++, desti++){
        acpiName2[desti] = acpiName[i];
        if(acpiName[i] == '\\'){
            acpiName2[++desti] = '\\';
        }
    }


    // Get GSI base for this ACPI name
    char query[1024];
    snprintf(query, sizeof(query), "findall(X,pir(\"%s\",X),LiU), sort(LiU,Li), Li=[X|_], writeln(X).",
            acpiName2);
    err = skb_execute(query);
    if(err_is_fail(err)){
        skb_read_error_code();
        DEBUG_ERR(err, "skb_execute_query failed. error_code=%d\nstdout=%s\nstderr=%s\n",
                skb_read_error_code(), skb_get_output(), skb_get_error_output());
    }
    assert(err_is_ok(err));

    int gsiBase;
    err = skb_read_output("%d", &gsiBase);
    assert(err_is_ok(err));

    ACPI_DEBUG("add_mapping: GsiBase:%d, AcpiName:%s, addr: %"PRIu64"\n",
            gsiBase, acpiName, to.addr);
    err = set_device_irq(acpiName, gsiBase + to.addr);


out:
    if(err_is_fail(err)){
        DEBUG_ERR(err, "add_mapping failed");
    }

}

static void pcilnk_route_controller_bind_cb(void *st, errval_t err, struct int_route_controller_binding *b) {
    if(!err_is_ok(err)){
        debug_err(__FILE__,__FUNCTION__,__LINE__, err, "Bind failure\n");
        return;
    }

    b->rx_vtbl.add_mapping = add_mapping;

    // Register this binding for all controllers with class pcilnk
    const char * label = "";
    const char * ctrl_class = "pcilnk";
    b->tx_vtbl.register_controller(b, NOP_CONT, label, strlen(label), ctrl_class,
            strlen(ctrl_class));


}




errval_t pcilnk_controller_client_init(void){
    // Connect to int route service
    iref_t int_route_service;
    errval_t err;
    err = nameservice_blocking_lookup("int_ctrl_service", &int_route_service);
    if(!err_is_ok(err)){
        debug_err(__FILE__,__FUNCTION__,__LINE__, err, "Could not lookup int_route_service\n");
        return err;
    }

    err = int_route_controller_bind(int_route_service, pcilnk_route_controller_bind_cb, NULL, get_default_waitset(),
            IDC_BIND_FLAGS_DEFAULT);

    if(!err_is_ok(err)){
        debug_err(__FILE__,__FUNCTION__,__LINE__, err, "Could not bind int_route_service\n");
        return err;
    }

    return SYS_ERR_OK;
}
