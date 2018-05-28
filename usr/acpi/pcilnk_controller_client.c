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

#include <if/int_route_controller_defs.h>


static int get_device_irq(const char* device){
    ACPI_HANDLE source;
    ACPI_STATUS as = AcpiGetHandle(NULL, (CONST_CAST)device, &source);
    if (ACPI_FAILURE(as)) {
        debug_printf("  failed lookup: %s\n", AcpiFormatException(as));
        return -1;
    }

    uint8_t data[2*sizeof(ACPI_RESOURCE) + 1];
    ACPI_BUFFER buf = { .Length = sizeof(data), .Pointer = &data };
    as = AcpiGetCurrentResources(source, &buf);
    if (ACPI_FAILURE(as)) {
        debug_printf("  failed getting _CRS: %s\n", AcpiFormatException(as));
        return -1;
    }

    assert(buf.Pointer == data);
    ACPI_RESOURCE * res = buf.Pointer;
    switch(res->Type) {
    case ACPI_RESOURCE_TYPE_IRQ:
        return res->Data.Irq.Interrupts[0];
        break;

    case ACPI_RESOURCE_TYPE_EXTENDED_IRQ:
        return res->Data.ExtendedIrq.Interrupts[0];
        break;

    default:
        printf("Unknown resource type: %"PRIu32"\n", res->Type);
        ACPI_DEBUG("NYI");
        return -1;
        break;
    }

}

#ifdef ACPI_BF_DEBUG
void AcpiRsDumpResourceList (ACPI_RESOURCE *Resource);
static void debug_device_irq(const char* device) {
    ACPI_HANDLE source;
    ACPI_STATUS as = AcpiGetHandle(NULL, (CONST_CAST)device, &source);
    if (ACPI_FAILURE(as)) {
        debug_printf("  failed lookup: %s\n", AcpiFormatException(as));
        return;
    }

    uint8_t data[32 * sizeof(ACPI_RESOURCE)];
    ACPI_BUFFER buf = { .Length = sizeof(data), .Pointer = &data };
    as = AcpiGetCurrentResources(source, &buf);
    if (ACPI_FAILURE(as)) {
        debug_printf("  failed getting _CRS: %s\n", AcpiFormatException(as));
        return;
    }


    printf("-- DUMP RESOURCES OF %s --", device);
    ACPI_RESOURCE *res = buf.Pointer;
    AcpiRsDumpResourceList(res);
}
#endif

/**
 * TODO: Once everything uses the interrupt routing service,
 * this method should become static and should be called only from add_mapping
 */
errval_t set_device_irq(const char* device, uint32_t irq) {
    ACPI_DEBUG("Setting link device '%s' to GSI %"PRIu32"\n", device, irq);

    ACPI_HANDLE source;
    ACPI_STATUS as = AcpiGetHandle(NULL, (CONST_CAST)device, &source);
    if (ACPI_FAILURE(as)) {
        ACPI_DEBUG("  failed lookup: %s\n", AcpiFormatException(as));
        return ACPI_ERR_INVALID_PATH_NAME;
    }

    // Space for IRQ resource + end tag
    uint8_t data[sizeof(ACPI_RESOURCE) * 2 + 1];
    ACPI_BUFFER buf = { .Length = sizeof(data), .Pointer = &data };
    as = AcpiGetCurrentResources(source, &buf);
    if (ACPI_FAILURE(as)) {
        ACPI_DEBUG("  failed getting _CRS: %s\n", AcpiFormatException(as));
        return ACPI_ERR_GET_RESOURCES;
    }

    // set chosen IRQ in first IRQ resource type
    ACPI_RESOURCE *res = buf.Pointer;
    switch(res->Type) {
    case ACPI_RESOURCE_TYPE_IRQ:
        res->Data.Irq.Interrupts[0] = irq;
        res->Data.Irq.InterruptCount = 1;
        res->Data.Irq.Sharable = ACPI_EXCLUSIVE;
        break;

    case ACPI_RESOURCE_TYPE_EXTENDED_IRQ:
        res->Data.ExtendedIrq.Interrupts[0] = irq;
        res->Data.ExtendedIrq.InterruptCount = 1;
        res->Data.ExtendedIrq.Sharable = ACPI_EXCLUSIVE;
        break;

    default:
        printf("Unknown resource type: %"PRIu32"\n", res->Type);
        ACPI_DEBUG("NYI");
        break;
    }

    ACPI_RESOURCE* res1 = ACPI_NEXT_RESOURCE(res);
    if(res1->Type != ACPI_RESOURCE_TYPE_END_TAG){
        debug_printf("unexpected ACPI RESOURCE TYPE ??? : %d\n", res1->Type);
    }

    //pcie_enable(); // XXX
    as = AcpiSetCurrentResources(source, &buf);
    if (ACPI_FAILURE(as)) {
        ACPI_DEBUG("  failed setting current IRQ: %s\n",
                  AcpiFormatException(as));
        return ACPI_ERR_SET_IRQ;
    }

    return SYS_ERR_OK;
}

static void add_mapping(struct int_route_controller_binding *b,
        const char *label,
        const char *class,
        int_route_controller_int_message_t from,
        int_route_controller_int_message_t to) {

    errval_t err = SYS_ERR_OK;

    ACPI_DEBUG("pcilnk add_mapping: label:%s, class:%s (%"PRIu64", %"PRIu64") to "
            "(%"PRIu64", %"PRIu64")\n", label, class, from.addr, from.msg, to.addr, to.msg);


    // Get acpiName from SKB
    char acpiName[255];
    char acpiName2[255];
    err = skb_execute_query("pcilnk_index(AcpiName, %s), writeln(AcpiName).", label);
    if(strlen(skb_get_output()) > sizeof(acpiName)){
        debug_printf("Buffer overflow: acpiName\n");
        err = SYS_ERR_INVALID_USER_BUFFER;
        goto out;
    }
    strncpy(acpiName, skb_get_output(), sizeof(acpiName));
    char * end_of_line = strchr(acpiName,'\n');
    if(!end_of_line){
        debug_printf("Buffer too small: acpiName\n");
        err = SYS_ERR_INVALID_USER_BUFFER;
        goto out;
    }
    *end_of_line = '\0';
    // Escape backslash
    for(int i=0, desti=0; i<strlen(acpiName)+1; i++, desti++){
        if(desti >= sizeof(acpiName2)){
            debug_printf("Buffer overflow: acpiName2\n");
            err = SYS_ERR_INVALID_USER_BUFFER;
            goto out;
        }
        acpiName2[desti] = acpiName[i];
        if(acpiName[i] == '\\'){
            acpiName2[++desti] = '\\';
        }
    }

    // Get GSI base for this ACPI name
    err = skb_execute_query("findall(X,pir(\"%s\",X),LiU), sort(LiU,Li), Li=[X|_],"
            "writeln(X).", acpiName2);
    if(err_is_fail(err)){
        DEBUG_SKB_ERR(err, "skb_execute_query failed.");
    }
    assert(err_is_ok(err));

    int gsiBase;
    err = skb_read_output("%d", &gsiBase);
    assert(err_is_ok(err));

    ACPI_DEBUG("add_mapping: GsiBase:%d, AcpiName:%s, port: %"PRIu64"\n",
            gsiBase, acpiName, to.port);
    err = set_device_irq(acpiName, gsiBase + to.port);

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
    b->tx_vtbl.register_controller(b, NOP_CONT, label, ctrl_class);

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
