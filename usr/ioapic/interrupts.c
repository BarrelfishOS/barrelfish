/**
 * \file
 * \brief Interrupt management (Local and IOAPICs) and routing
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <acpi.h>
#include <mm/mm.h>

#include <skb/skb.h>
#include <dist2/getset.h>

#include "pci.h"
#include "pci_acpi.h"
#include "ioapic.h"
#include "pci_debug.h"
#include "ioapic_debug.h"

//from documentation
#define APIC_BITS 11

/// Assume size of the I/O APIC is one page
#define IOAPIC_PAGE_SIZE        BASE_PAGE_SIZE

/// Maximum number of supported I/O APICs
#define IOAPIC_MAX      5

/// Room for all supported I/O APICs
static struct ioapic ioapics[IOAPIC_MAX];

/// Interrupt override table (maps ISA interrupts to GSIs)
#define N_ISA_INTERRUPTS 16
static int interrupt_overrides[N_ISA_INTERRUPTS];

/// I/O APIC redirection table entry template for ISA bus
static lpc_ioapic_redir_tbl_t ioapic_redir_tmpl_isa = {
    .mode = lpc_ioapic_fixed,
    .destmode = lpc_ioapic_physical,
    .polarity = lpc_ioapic_active_high,
    .trigger = lpc_ioapic_edge,
    .mask = 1
};

/// I/O APIC redirection table entry template for PCI bus
static lpc_ioapic_redir_tbl_t ioapic_redir_tmpl_pci = {
    .mode = lpc_ioapic_fixed,
    .destmode = lpc_ioapic_physical,
    .polarity = lpc_ioapic_active_low,
    .trigger = lpc_ioapic_edge, // XXX: Barrelfish cannot handle level
                                // triggered interrupts
    .mask = 1
};

static struct ioapic *find_ioapic(uint32_t gsi)
{
    for(int i = 0; i < IOAPIC_MAX; i++) {
        struct ioapic *a = &ioapics[i];

        if(a->irqbase <= gsi && gsi < a->irqbase + a->nintis) {
            return a;
        }
    }

    return NULL;
}

static errval_t init_one_ioapic(uint64_t id,  uint64_t address, uint64_t irqbase)
{
    errval_t            err;
    struct capref       devmem, devframe;
    lvaddr_t            vaddr;
    static int          ioapic_nr = 0;

    assert(ioapic_nr < IOAPIC_MAX);

    assert(IOAPIC_PAGE_SIZE == BASE_PAGE_SIZE);

    // allocate memory backing IOAPIC
    err = mm_realloc_range(&pci_mm_physaddr, BASE_PAGE_BITS, address, &devmem);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Failed to allocate I/O APIC register page at 0x%x\n",
                address);
        return err_push(err, MM_ERR_REALLOC_RANGE);
    }

    err = devframe_type(&devframe, devmem, BASE_PAGE_BITS);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_DEVFRAME_TYPE);
    }

    // Map registers
    err = vspace_map_one_frame_attr((void**)&vaddr, BASE_PAGE_SIZE, devframe,
                                    VREGION_FLAGS_READ_WRITE_NOCACHE,
                                    NULL, NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VSPACE_MAP);
    }

    // Initialize driver
    struct ioapic *ioapic = &ioapics[ioapic_nr++];

    err = ioapic_init(ioapic, vaddr, id, irqbase);
    if (err_is_fail(err)) {
        PCI_DEBUG("I/O APIC init failed!\n");
        return err_push(err, PCI_ERR_IOAPIC_INIT);
    }

    // Set redirection table entry defaults for managed buses
    for(int i = 0; i < ioapic->nintis; i++) {
        if(irqbase + i < N_ISA_INTERRUPTS) {
            // ISA interrupts go to GSIs 0 .. 15
            ioapic_setup_inti(ioapic, i, ioapic_redir_tmpl_isa);
        } else {
            // Assume the rest is PCI
            ioapic_setup_inti(ioapic, i, ioapic_redir_tmpl_pci);
        }
    }

    return SYS_ERR_OK;
}

static void add_interrupt_override(uint64_t bus, uint64_t source,
        uint64_t irq, uint64_t flags)
{
    APIC_DEBUG("add_interrupt_override bus:%lu source: %lu, irq: %lu, flags: %lu\n",
            bus, source, irq, flags);
    assert(source < N_ISA_INTERRUPTS);
    interrupt_overrides[source] = irq;

    if (flags == 0) {
        return;
    }

    lpc_ioapic_redir_tbl_t entry = ioapic_redir_tmpl_isa;
    struct ioapic *a = find_ioapic(irq);
    if (a == NULL) {
        APIC_DEBUG("Warning: unknown I/O APIC for GSI %lu, ignored"
                     " interrupt override flags.\n", irq);
        return;
    }

    // Set polarity
    assert((flags & ACPI_MADT_POLARITY_MASK) != ACPI_MADT_POLARITY_RESERVED);
    switch(flags & ACPI_MADT_POLARITY_MASK) {

    case ACPI_MADT_POLARITY_ACTIVE_HIGH:
        entry.polarity = lpc_ioapic_active_high;
        break;

    case ACPI_MADT_POLARITY_ACTIVE_LOW:
        entry.polarity = lpc_ioapic_active_low;
        break;

    }

    // Set trigger mode
    assert((flags & ACPI_MADT_TRIGGER_MASK) != ACPI_MADT_TRIGGER_RESERVED);
    switch(flags & ACPI_MADT_TRIGGER_MASK) {

    case ACPI_MADT_TRIGGER_EDGE:
        entry.trigger = lpc_ioapic_edge;
        break;

    case ACPI_MADT_TRIGGER_LEVEL:
        // XXX: should be lpc_ioapic_level
        entry.trigger = lpc_ioapic_edge;
        break;

    }

    ioapic_setup_inti(a, irq - a->irqbase, entry);
}

errval_t setup_interupt_override(void)
{
    errval_t err = SYS_ERR_OK;
    // Now we need to deal with interrupt overrides
    for (int i = 0; i < N_ISA_INTERRUPTS; i++) {
        // Default Overrides
        interrupt_overrides[i] = i;
    }

    err = skb_execute("findall(X, (interrupt_override(A,B,C,D), "
                      "X = interrupt_override(A,B,C,D)), L), writeln(L).");
    if (err_is_fail(err)) {
        return err;
    }

    APIC_DEBUG("SKB returned: %s\n", skb_get_output());

    err = skb_read_error_code();
    if (err_is_ok(err)) {
        struct list_parser_status status;
        skb_read_list_init(&status);

        uint64_t bus, source, irq, flags;
        static char* format = "interrupt_override(%lu,%lu,%lu,%lu)";
        while(skb_read_list(&status, format, &bus, &source, &irq, &flags)) {
            add_interrupt_override(bus, source, irq, flags);
        }
    }

    return err;
}

errval_t init_all_apics(void)
{
    errval_t err = SYS_ERR_OK;

    char** names = NULL;
    size_t len = 0;
    err = dist_get_names(&names, &len,
            "r'hw.ioapic.[0-9]+' { id: _, address: _, irqbase: _ }");
    if (err_is_fail(err)) {
        return err;
    }

    for (size_t i=0; i<len; i++) {
        char* record = NULL;
        err = dist_get(&record, names[i]);
        if (err_is_ok(err)) {
            APIC_DEBUG("Found I/O APIC record: %s\n", record);
            uint64_t id, address, irqbase;
            err = dist_read(record, "_ { id: %d, address: %d, irqbase: %d }",
                    &id, &address, &irqbase);
            free(record);
            if (err_is_fail(err)) {
                goto out;
            }
            err = init_one_ioapic(id, address, irqbase);
            if(err_is_fail(err)) {
                DEBUG_ERR(err, "Unable to initialize I/O APIC (ID = %lu)", id);
                abort();
            }
        }
        // else: the record has been removed; ignore
    }

out:
    dist_free_names(names, len);
    return err;
}

errval_t enable_and_route_interrupt(int gsi, coreid_t dest, int vector)
{
    /* sanity-check vector */
    // XXX: this check matches the use of vectors in the kernel's irq.c
    if (vector < 0 || vector >= (250 - 32)) {
        return PCI_ERR_INVALID_VECTOR;
    }

    /* convert to CPU vector */
    vector += 32;

    /* lookup override table */
    int gsi_mapped;
    if (gsi < N_ISA_INTERRUPTS) {
        gsi_mapped = interrupt_overrides[gsi];
    } else {
        gsi_mapped = gsi;
    }

    /* find the correct IOAPIC */
    struct ioapic *i = find_ioapic(gsi_mapped);
    if (i == NULL) {
        return PCI_ERR_UNKNOWN_GSI;
    }

    // Resolve destination core ID to APIC ID
    char *result = NULL, *str_error = NULL, query[256];
    int int_error = 0;
    int r = snprintf(query, 256, "corename(%d, _, apic(A)), write(A).", dest);
    assert(r >= 0 && r < 256);
    errval_t err = skb_evaluate(query, &result, &str_error, &int_error);
    assert(err_is_ok(err));
    assert(result != NULL);
    free(str_error);

    // If SKB didn't have an answer, we assume we're not done setting up
    // mappings yet. In this case, we can only resolve our own core ID.
    uint8_t dest_apicid;
    if(*result == '\0') {
        if(dest != disp_get_core_id()) {
            USER_PANIC("SKB couldn't resolve core ID and it's not this core's "
                       "ID, giving up.");
        }

        dest_apicid = my_apic_id;
    } else {
        dest_apicid = strtol(result, &str_error, 10);
        assert(*str_error == '\0');
    }
    free(result);

    /* route to the given core */
    int inti = gsi_mapped - i->irqbase;
    ioapic_route_inti(i, inti, vector, dest_apicid);

    PCI_DEBUG("routing GSI %d -> %d -> INTI %d -> APIC %d (coreid %d) "
              "vector %d\n", gsi, gsi_mapped, inti, dest_apicid, dest, vector);

    /* enable */
    ioapic_toggle_inti(i, inti, true);

    return SYS_ERR_OK;
}
