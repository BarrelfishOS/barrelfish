/******************************************************************************
 *
 * Module Name: osbarrelfishxf - Barrelfish OSL interfaces
 *
 *****************************************************************************/

/*
 * Copyright (c) 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*****************************************************************
 * Debug printer and its power-switch:
 *****************************************************************/

#include "acpi_shared.h"
#include "acpi_debug.h"
#include <barrelfish/deferred.h>


/******************************************************************************
 *
 * 1. Copyright Notice
 *
 * Some or all of this work - Copyright (c) 1999 - 2008, Intel Corp.
 * All rights reserved.
 *
 * 2. License
 *
 * 2.1. This is your license from Intel Corp. under its intellectual property
 * rights.  You may have additional license terms from the party that provided
 * you this software, covering your right to use that party's intellectual
 * property rights.
 *
 * 2.2. Intel grants, free of charge, to any person ("Licensee") obtaining a
 * copy of the source code appearing in this file ("Covered Code") an
 * irrevocable, perpetual, worldwide license under Intel's copyrights in the
 * base code distributed originally by Intel ("Original Intel Code") to copy,
 * make derivatives, distribute, use and display any portion of the Covered
 * Code in any form, with the right to sublicense such rights; and
 *
 * 2.3. Intel grants Licensee a non-exclusive and non-transferable patent
 * license (with the right to sublicense), under only those claims of Intel
 * patents that are infringed by the Original Intel Code, to make, use, sell,
 * offer to sell, and import the Covered Code and derivative works thereof
 * solely to the minimum extent necessary to exercise the above copyright
 * license, and in no event shall the patent license extend to any additions
 * to or modifications of the Original Intel Code.  No other license or right
 * is granted directly or by implication, estoppel or otherwise;
 *
 * The above copyright and patent license is granted only if the following
 * conditions are met:
 *
 * 3. Conditions
 *
 * 3.1. Redistribution of Source with Rights to Further Distribute Source.
 * Redistribution of source code of any substantial portion of the Covered
 * Code or modification with rights to further distribute source must include
 * the above Copyright Notice, the above License, this list of Conditions,
 * and the following Disclaimer and Export Compliance provision.  In addition,
 * Licensee must cause all Covered Code to which Licensee contributes to
 * contain a file documenting the changes Licensee made to create that Covered
 * Code and the date of any change.  Licensee must include in that file the
 * documentation of any changes made by any predecessor Licensee.  Licensee
 * must include a prominent statement that the modification is derived,
 * directly or indirectly, from Original Intel Code.
 *
 * 3.2. Redistribution of Source with no Rights to Further Distribute Source.
 * Redistribution of source code of any substantial portion of the Covered
 * Code or modification without rights to further distribute source must
 * include the following Disclaimer and Export Compliance provision in the
 * documentation and/or other materials provided with distribution.  In
 * addition, Licensee may not authorize further sublicense of source of any
 * portion of the Covered Code, and must include terms to the effect that the
 * license from Licensee to its licensee is limited to the intellectual
 * property embodied in the software Licensee provides to its licensee, and
 * not to intellectual property embodied in modifications its licensee may
 * make.
 *
 * 3.3. Redistribution of Executable. Redistribution in executable form of any
 * substantial portion of the Covered Code or modification must reproduce the
 * above Copyright Notice, and the following Disclaimer and Export Compliance
 * provision in the documentation and/or other materials provided with the
 * distribution.
 *
 * 3.4. Intel retains all right, title, and interest in and to the Original
 * Intel Code.
 *
 * 3.5. Neither the name Intel nor any other trademark owned or controlled by
 * Intel shall be used in advertising or otherwise to promote the sale, use or
 * other dealings in products derived from or relating to the Covered Code
 * without prior written authorization from Intel.
 *
 * 4. Disclaimer and Export Compliance
 *
 * 4.1. INTEL MAKES NO WARRANTY OF ANY KIND REGARDING ANY SOFTWARE PROVIDED
 * HERE.  ANY SOFTWARE ORIGINATING FROM INTEL OR DERIVED FROM INTEL SOFTWARE
 * IS PROVIDED "AS IS," AND INTEL WILL NOT PROVIDE ANY SUPPORT,  ASSISTANCE,
 * INSTALLATION, TRAINING OR OTHER SERVICES.  INTEL WILL NOT PROVIDE ANY
 * UPDATES, ENHANCEMENTS OR EXTENSIONS.  INTEL SPECIFICALLY DISCLAIMS ANY
 * IMPLIED WARRANTIES OF MERCHANTABILITY, NONINFRINGEMENT AND FITNESS FOR A
 * PARTICULAR PURPOSE.
 *
 * 4.2. IN NO EVENT SHALL INTEL HAVE ANY LIABILITY TO LICENSEE, ITS LICENSEES
 * OR ANY OTHER THIRD PARTY, FOR ANY LOST PROFITS, LOST DATA, LOSS OF USE OR
 * COSTS OF PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES, OR FOR ANY INDIRECT,
 * SPECIAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THIS AGREEMENT, UNDER ANY
 * CAUSE OF ACTION OR THEORY OF LIABILITY, AND IRRESPECTIVE OF WHETHER INTEL
 * HAS ADVANCE NOTICE OF THE POSSIBILITY OF SUCH DAMAGES.  THESE LIMITATIONS
 * SHALL APPLY NOTWITHSTANDING THE FAILURE OF THE ESSENTIAL PURPOSE OF ANY
 * LIMITED REMEDY.
 *
 * 4.3. Licensee shall not export, either directly or indirectly, any of this
 * software or system incorporating such software without first obtaining any
 * required license or other approval from the U. S. Department of Commerce or
 * any other agency or department of the United States Government.  In the
 * event Licensee exports any such software from the United States or
 * re-exports any such software from a foreign destination, Licensee shall
 * ensure that the distribution and export/re-export of the software is in
 * compliance with all laws, regulations, orders, or other restrictions of the
 * U.S. Export Administration Regulations. Licensee agrees that neither it nor
 * any of its subsidiaries will export/re-export any technical data, process,
 * software, or service, directly or indirectly, to any country for which the
 * United States government or any agency thereof requires an export license,
 * other governmental approval, or letter of assurance, without first obtaining
 * such license, approval or letter.
 *
 *****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <barrelfish/inthandler.h>

#include <acpi.h>
#include <accommon.h>
#include <amlcode.h>
#include <acparser.h>
#include <acdebug.h>

#include <pci/confspace/pci_confspace.h>
#include <pci/confspace/mackerelpci.h>

#include <mm/mm.h>

#define _COMPONENT          ACPI_OS_SERVICES
        ACPI_MODULE_NAME    ("osbarrelfishxf")

//extern FILE *AcpiGbl_DebugFile;
static FILE *AcpiGbl_OutputFile;

/******************************************************************************
 *
 * FUNCTION:    AcpiOsInitialize, AcpiOsTerminate
 *
 * PARAMETERS:  None
 *
 * RETURN:      Status
 *
 * DESCRIPTION: Init and terminate.  Nothing to do.
 *
 *****************************************************************************/

ACPI_STATUS
AcpiOsInitialize (void)
{
    AcpiGbl_OutputFile = stdout;
    return AE_OK;
}


ACPI_STATUS
AcpiOsTerminate (void)
{
    return AE_OK;
}


/******************************************************************************
 *
 * FUNCTION:    AcpiOsGetRootPointer
 *
 * PARAMETERS:  None
 *
 * RETURN:      RSDP physical address
 *
 * DESCRIPTION: Gets the root pointer (RSDP)
 *
 *****************************************************************************/

static ACPI_PHYSICAL_ADDRESS acpi_root_pointer = 0;

ACPI_PHYSICAL_ADDRESS
AcpiOsGetRootPointer (
    void)
{
    if (acpi_root_pointer != 0) {
        return acpi_root_pointer;
    }
    ACPI_PHYSICAL_ADDRESS physaddr;
    ACPI_STATUS as = AcpiFindRootPointer(&physaddr);
    if (as == AE_OK) {
        return physaddr;
    } else {
        return 0;
    }
}

void
AcpiOsSetRootPointer (
        ACPI_PHYSICAL_ADDRESS physaddr)
{
    acpi_root_pointer = physaddr;
}


/******************************************************************************
 *
 * FUNCTION:    AcpiOsPredefinedOverride
 *
 * PARAMETERS:  InitVal     - Initial value of the predefined object
 *              NewVal      - The new value for the object
 *
 * RETURN:      Status, pointer to value.  Null pointer returned if not
 *              overriding.
 *
 * DESCRIPTION: Allow the OS to override predefined names
 *
 *****************************************************************************/

ACPI_STATUS
AcpiOsPredefinedOverride (
    const ACPI_PREDEFINED_NAMES *InitVal,
    ACPI_STRING                 *NewVal)
{

    if (!InitVal || !NewVal)
    {
        return (AE_BAD_PARAMETER);
    }

    *NewVal = NULL;
    return (AE_OK);
}


/******************************************************************************
 *
 * FUNCTION:    AcpiOsTableOverride
 *
 * PARAMETERS:  ExistingTable   - Header of current table (probably firmware)
 *              NewTable        - Where an entire new table is returned.
 *
 * RETURN:      Status, pointer to new table.  Null pointer returned if no
 *              table is available to override
 *
 * DESCRIPTION: Return a different version of a table if one is available
 *
 *****************************************************************************/

ACPI_STATUS
AcpiOsTableOverride (
    ACPI_TABLE_HEADER       *ExistingTable,
    ACPI_TABLE_HEADER       **NewTable)
{

    if (!ExistingTable || !NewTable)
    {
        return (AE_BAD_PARAMETER);
    }

    *NewTable = NULL;

#ifdef ACPI_EXEC_APP

    AeTableOverride (ExistingTable, NewTable);
    return (AE_OK);
#else
    return AE_NO_ACPI_TABLES;
#endif
}


/******************************************************************************
 *
 * FUNCTION:    AcpiOsReadable
 *
 * PARAMETERS:  Pointer             - Area to be verified
 *              Length              - Size of area
 *
 * RETURN:      TRUE if readable for entire length
 *
 * DESCRIPTION: Verify that a pointer is valid for reading
 *
 *****************************************************************************/

BOOLEAN
AcpiOsReadable (
    void                    *Pointer,
    ACPI_SIZE               Length)
{

    return (TRUE);
}


/******************************************************************************
 *
 * FUNCTION:    AcpiOsWritable
 *
 * PARAMETERS:  Pointer             - Area to be verified
 *              Length              - Size of area
 *
 * RETURN:      TRUE if writable for entire length
 *
 * DESCRIPTION: Verify that a pointer is valid for writing
 *
 *****************************************************************************/

BOOLEAN
AcpiOsWritable (
    void                    *Pointer,
    ACPI_SIZE               Length)
{

    return (TRUE);
}


/******************************************************************************
 *
 * FUNCTION:    AcpiOsRedirectOutput
 *
 * PARAMETERS:  Destination         - An open file handle/pointer
 *
 * RETURN:      None
 *
 * DESCRIPTION: Causes redirect of AcpiOsPrintf and AcpiOsVprintf
 *
 *****************************************************************************/

void
AcpiOsRedirectOutput (
    void                    *Destination)
{

    AcpiGbl_OutputFile = Destination;
}


/******************************************************************************
 *
 * FUNCTION:    AcpiOsPrintf
 *
 * PARAMETERS:  fmt, ...            Standard printf format
 *
 * RETURN:      None
 *
 * DESCRIPTION: Formatted output
 *
 *****************************************************************************/

void ACPI_INTERNAL_VAR_XFACE
AcpiOsPrintf (
    const char              *Fmt,
    ...)
{
    va_list                 Args;


    va_start (Args, Fmt);

#ifdef ACPI_BF_DEBUG
    AcpiOsVprintf (Fmt, Args);
#endif

    va_end (Args);
    return;
}


/******************************************************************************
 *
 * FUNCTION:    AcpiOsVprintf
 *
 * PARAMETERS:  fmt                 Standard printf format
 *              args                Argument list
 *
 * RETURN:      None
 *
 * DESCRIPTION: Formatted output with argument list pointer
 *
 *****************************************************************************/

void
AcpiOsVprintf (
    const char              *Fmt,
    va_list                 Args)
{
    INT32                   Count = 0;
    UINT8                   Flags;


    Flags = AcpiGbl_DbOutputFlags;
    if (Flags & ACPI_DB_REDIRECTABLE_OUTPUT)
    {
        /* Output is directable to either a file (if open) or the console */

#if 0
        if (AcpiGbl_DebugFile)
        {
            /* Output file is open, send the output there */

            Count = vfprintf (AcpiGbl_DebugFile, Fmt, Args);
        }
        else
#endif
        {
            /* No redirection, send output to console (once only!) */

            Flags |= ACPI_DB_CONSOLE_OUTPUT;
        }
    }

#ifdef ACPI_BF_DEBUG

    if (Flags & ACPI_DB_CONSOLE_OUTPUT)
    {
        Count = vfprintf (AcpiGbl_OutputFile, Fmt, Args);
    }

#else

    Count = 0;

#endif

    return;
}


/******************************************************************************
 *
 * FUNCTION:    AcpiOsGetLine
 *
 * PARAMETERS:  fmt                 Standard printf format
 *              args                Argument list
 *
 * RETURN:      Actual bytes read
 *
 * DESCRIPTION: Formatted input with argument list pointer
 *
 *****************************************************************************/

UINT32
AcpiOsGetLine (
    char                    *Buffer,
    UINT32                  BufferLength,
    UINT32                  *BytesRead)
{
    assert(!"NYI: AcpiOsGetLine");
    return 0;
#if 0
    UINT8                   Temp;
    UINT32                  i;


    for (i = 0; ; i++)
    {
        scanf ("%1c", &Temp);
        if (!Temp || Temp == '\n')
        {
            break;
        }

        Buffer [i] = Temp;
    }

    /* Null terminate the buffer */

    Buffer [i] = 0;

    /* Return the number of bytes in the string */

    return (i);
#endif
}

/******************************************************************************
 *
 * FUNCTION:    AcpiOsMapMemory
 *
 * PARAMETERS:  where               Physical address of memory to be mapped
 *              length              How much memory to map
 *
 * RETURN:      Pointer to mapped memory.  Null on error.
 *
 * DESCRIPTION: Map physical memory into caller's address space
 *
 *****************************************************************************/

struct AcpiMapping {
    struct memobj_anon *memobj;
    struct vregion *vregion;
    lpaddr_t pbase;
    ACPI_SIZE length;
    unsigned refcount;
    struct capref *caps;
    size_t num_caps;
    struct AcpiMapping *next;
};

static struct AcpiMapping *head = NULL;

#if 0
static void dump_map_list(void)
{
    for (struct AcpiMapping *walk = head; walk; walk = walk->next)
    {
        printf("mapped region: pbase = 0x%"PRIxLPADDR"\n"
               "               vbase = 0x%"PRIxGENVADDR"\n"
               "               size  = %zd\n"
               "               refc  = %u\n",
               walk->pbase,
               vregion_get_base_addr(walk->vregion),
               walk->length,
               walk->refcount);
    }
}
#endif

void *
AcpiOsMapMemory (
    ACPI_PHYSICAL_ADDRESS   where,  /* not page aligned */
    ACPI_SIZE               length) /* in bytes, not page-aligned */
{
    errval_t err;

    ACPI_DEBUG("AcpiOsMapMemory where=0x%016lx, length=%lu\n", where, length);

    //printf("AcpiOsMapMemory: 0x%"PRIxLPADDR", %lu\n", where, length);

    /* round the addresses to page boundary */
    lpaddr_t pbase = where & (~BASE_PAGE_MASK);
    length += where - pbase;
    length = ROUND_UP(length, BASE_PAGE_SIZE);
    size_t npages = DIVIDE_ROUND_UP(length, BASE_PAGE_SIZE);

    lpaddr_t pend  = pbase + length - 1;

    ACPI_DEBUG("AcpiOsMapMemory: aligned request: 0x%" PRIxLPADDR "..0x%"
                PRIxLPADDR", %d\n", pbase, pend, npages);

    struct capref am_pages[npages];
    memset(&am_pages, 0, npages*sizeof(struct capref));

    /* find existing mappings */
    for (struct AcpiMapping *walk = head; walk; walk = walk->next) {
        lpaddr_t walk_end = walk->pbase + walk->length - 1;

        ACPI_DEBUG("%s: walk=0x%" PRIxLPADDR "...0x%" PRIxLPADDR ", region=0x%"
                   PRIxLPADDR "...0x%" PRIxLPADDR "\n", __FUNCTION__,
                   walk->pbase, walk_end, pbase, pend);

        /*
         * The walk region starts before the requested region.
         *   Walk   |------------------
         *   Region         |-----------------
         */
        if (walk->pbase <= pbase) {

            /*
             * The walk region ends after the requested region.
             *   Walk   |------------------|
             *   Region         |--------|
             *
             *   We have found the overlapping region, return this.
             *   Increase the refcount and return
             */
            if (walk_end >= pend) {
                walk->refcount++;
                ACPI_DEBUG("%s: found region for request (new refcount=%d), "
                           "mapped at %#"PRIxLVADDR"\n", __FUNCTION__,
                           walk->refcount, vregion_get_base_addr(walk->vregion));

                return (void*)(uintptr_t)vregion_get_base_addr(walk->vregion)
                                                   + (where-walk->pbase);
            }

            /*
             * The walk region ends before the requested region.
             *   Walk   |----|
             *   Region         |--------|
             *
             *   We have found the overlapping region, return this.
             *   Increase the refcount and return
             */

            if (walk_end < pbase) {
                continue;
            }

            /*
             * The walk region ends before the requested region ends
             *   Walk   |------------------|
             *   Region         |--------------|
             *
             * We can use the caps of the parts of this walk region. From
             * where the requested region starts until the end.
             */
            size_t walk_offset = (walk->pbase - pbase) / BASE_PAGE_SIZE;
            size_t idx = 0;
            for (size_t i = walk_offset; i < walk->num_caps; i++) {
                ACPI_DEBUG("%s: using am_pages[%zu/%zu] = walk->caps[%zu/%zu] with paddr=0x%"
                           PRIxLPADDR "\n", __FUNCTION__, idx, npages, i,
                           walk->num_caps,  walk->pbase + i * BASE_PAGE_SIZE);
                assert(i < walk->num_caps);
                assert(idx < npages);
                am_pages[idx++] = walk->caps[i];


            }
        } else {

            /*
             * The walk region starts after the requested element.
             *   Walk                |------------------|
             *   Region   |------|
             *
             *   walk->pbase > pbase
             *
             *   the region is outside of the walk.
             */
            if (walk->pbase > pend) {
                continue;
            }

            /*
             * The walk region starts after the requested element.
             *   Walk          |------------------|
             *   Region   |------|
             *
             *   walk->pbase > pbase
             *
             *   the region is outside of the walk.
             */
            size_t region_offset = (walk->pbase - pbase) / BASE_PAGE_SIZE;
            size_t idx = 0;
            for (lpaddr_t cur = walk->pbase; cur < pend; cur += BASE_PAGE_SIZE) {
                ACPI_DEBUG("%s: using am_pages[%zu/%zu] = walk->caps[%zu/%zu] with paddr=0x%"
                           PRIxLPADDR "\n", __FUNCTION__, region_offset, npages, idx,
                           walk->num_caps,  walk->pbase + idx * BASE_PAGE_SIZE);
                assert(region_offset < npages);
                assert(idx < walk->num_caps);
                am_pages[region_offset++] = walk->caps[idx++];
            }
        }
    }

    struct memobj_anon *memobj = malloc(sizeof(struct memobj_anon));
    assert(memobj);
    struct vregion *vregion = malloc(sizeof(struct vregion));
    assert(vregion);
    err = memobj_create_anon(memobj, length, 0);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "memobj_create_anon failed");
        return NULL;
    }
    err = vregion_map(vregion, get_current_vspace(), &memobj->m, 0, length,
                      VREGION_FLAGS_READ_WRITE);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "vregion_map failed");
        return NULL;
    }
    ACPI_DEBUG("%s: Mapping %#"PRIxLPADDR" at %#"PRIxGENVADDR"\n",
            __FUNCTION__, pbase, vregion_get_base_addr(vregion));

    ACPI_DEBUG("%s: Mapping %#"PRIxLPADDR" at %#"PRIxGENVADDR"\n",
            __FUNCTION__, pbase, vregion_get_base_addr(vregion));

    struct AcpiMapping *new = malloc(sizeof(struct AcpiMapping));
    new->num_caps = npages;
    new->caps = calloc(npages, sizeof(struct capref));
    for (int page = 0; page < npages; page++) {
        struct capref frame_cap;
        lpaddr_t paddr = pbase + page * BASE_PAGE_SIZE;

        if (capref_is_null(am_pages[page])) {
            ACPI_DEBUG("%s: allocating page for %#"PRIxLPADDR"\n",
                    __FUNCTION__, paddr);

            err = mm_realloc_range(&pci_mm_physaddr, BASE_PAGE_BITS, paddr,
                                   &frame_cap);
            if (err_is_fail(err)) {
                free(new->caps);
                free(new);
                DEBUG_ERR(err, "AcpiOsMapMemory: allocating RAM at %lx failed\n",
                          paddr);
                return NULL;
            }
            /* result of mm_realloc_range is already DevFrame */
        }
        else {
            ACPI_DEBUG("%s: using existing page for %#"PRIxLPADDR"\n",
                    __FUNCTION__, paddr);
            frame_cap = am_pages[page];
        }

        err = slot_alloc(&new->caps[page]);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "AcpiOsMapMemory: could not allocate slot for capability: %s.",
                    err_getstring(err_no(err)));
            return NULL;
        }
        err = cap_copy(new->caps[page], frame_cap);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "AcpiOsMapMemory: cap_copy failed: %s.",
                    err_getstring(err_no(err)));
            return NULL;
        }
        err = memobj->m.f.fill(&memobj->m, page * BASE_PAGE_SIZE, new->caps[page],
                           BASE_PAGE_SIZE);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "AcpiOsMapMemory: memobj fill failed: %s.",
                    err_getstring(err_no(err)));
            // XXX: TODO: cleanup
            return NULL;
        }
        assert(err == 0);
        err = memobj->m.f.pagefault(&memobj->m, vregion, page * BASE_PAGE_SIZE, 0);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "AcpiOsMapMemory: memobj pagefault failed: %s.",
                    err_getstring(err_no(err)));
            // XXX: TODO: cleanup
            return NULL;
        }
        assert(err == 0);
    }

    // add new mapping to tracking list
    new->memobj = memobj;
    new->vregion = vregion;
    new->pbase = pbase;
    new->length = length;
    new->refcount = 1;
    new->next = head;
    head = new;

    return (void*)(uintptr_t)vregion_get_base_addr(vregion) + (where - pbase);
}


/******************************************************************************
 *
 * FUNCTION:    AcpiOsUnmapMemory
 *
 * PARAMETERS:  where               Logical address of memory to be unmapped
 *              length              How much memory to unmap
 *
 * RETURN:      None.
 *
 * DESCRIPTION: Delete a previously created mapping.  Where and Length must
 *              correspond to a previous mapping exactly.
 *
 *****************************************************************************/

void
AcpiOsUnmapMemory (
    void                    *where,
    ACPI_SIZE               length)
{
    ACPI_DEBUG("%s: where=%p length=%zd\n",
            __FUNCTION__, where, (size_t)length);

    uintptr_t vbase = (uintptr_t)where & (~BASE_PAGE_MASK);
    length = ROUND_UP(length, BASE_PAGE_SIZE);

    ACPI_DEBUG("%s: aligned request: %#"PRIxPTR", %zu pages\n",
            __FUNCTION__, vbase, length / BASE_PAGE_SIZE);

    assert(head); // there should be a mapped region if Unmap is called

    struct AcpiMapping *prev = NULL;
    for (struct AcpiMapping *walk = head; walk; prev = walk, walk = walk->next) {
        genvaddr_t walk_vaddr = vregion_get_base_addr(walk->vregion);
        genvaddr_t walk_end   = walk_vaddr + walk->length;
        if (walk_vaddr <= vbase && walk_end >= vbase + length) {
            ACPI_DEBUG("region: %#"PRIxGENVADDR", %zu pages\n",
                    walk_vaddr, walk->length / BASE_PAGE_SIZE);
            walk->refcount--;
            if (!walk->refcount) {
                ACPI_DEBUG("%s: last reference to region, cleaning up\n", __FUNCTION__);
                errval_t err;
                // Delete vregion; do this first because vnode_unmap complains
                // about missing caps if we mm_free() the backing caps first
                err = vregion_destroy(walk->vregion);
                if (err_is_fail(err)) {
                    DEBUG_ERR(err, "%s: vregion_destroy", __FUNCTION__);
                }

                // Destroy memobj without destroying caps, as we've passed
                // uncopied caps from MM to memobj->fill().
                err = memobj_destroy_anon((struct memobj *)walk->memobj, false);
                if (err_is_fail(err)) {
                    DEBUG_ERR(err, "%s: memobj_destroy_anon", __FUNCTION__);
                }

                // Remove from list
                if (prev) {
                    prev->next = walk->next;
                }
                else { // we were head
                    head = walk->next;
                }

                for (int i = 0; i < walk->num_caps; i++) {
                    // XXX: ensure that this never deletes a last copy?
                    cap_destroy(walk->caps[i]);
                }

                // Free malloc'd memory for element
                free(walk->vregion);
                free(walk->memobj);
                free(walk->caps);
                free(walk);

                return;
            }
        }
    }
}


/******************************************************************************
 *
 * FUNCTION:    AcpiOsAllocate
 *
 * PARAMETERS:  Size                Amount to allocate, in bytes
 *
 * RETURN:      Pointer to the new allocation.  Null on error.
 *
 * DESCRIPTION: Allocate memory.  Algorithm is dependent on the OS.
 *
 *****************************************************************************/

void *
AcpiOsAllocate (
    ACPI_SIZE               size)
{
    void                    *Mem;


    Mem = (void *) malloc ((size_t) size);

    return Mem;
}


/******************************************************************************
 *
 * FUNCTION:    AcpiOsFree
 *
 * PARAMETERS:  mem                 Pointer to previously allocated memory
 *
 * RETURN:      None.
 *
 * DESCRIPTION: Free memory allocated via AcpiOsAllocate
 *
 *****************************************************************************/

void
AcpiOsFree (
    void                    *mem)
{
    free(mem);
}


/******************************************************************************
 *
 * FUNCTION:    AcpiOsCreateSemaphore
 *
 * PARAMETERS:  InitialUnits        - Units to be assigned to the new semaphore
 *              OutHandle           - Where a handle will be returned
 *
 * RETURN:      Status
 *
 * DESCRIPTION: Create an OS semaphore
 *
 *****************************************************************************/

ACPI_STATUS
AcpiOsCreateSemaphore (
    UINT32              MaxUnits,
    UINT32              InitialUnits,
    ACPI_HANDLE         *OutHandle)
{
    struct thread_sem *sem = malloc(sizeof(struct thread_sem));
    assert(sem != NULL);
    thread_sem_init(sem, InitialUnits);
    *OutHandle = sem;
    return AE_OK;
}

/******************************************************************************
 *
 * FUNCTION:    AcpiOsDeleteSemaphore
 *
 * PARAMETERS:  Handle              - Handle returned by AcpiOsCreateSemaphore
 *
 * RETURN:      Status
 *
 * DESCRIPTION: Delete an OS semaphore
 *
 *****************************************************************************/

ACPI_STATUS
AcpiOsDeleteSemaphore (
    ACPI_HANDLE         Handle)
{

    if (!Handle)
    {
        return AE_BAD_PARAMETER;
    }

    free(Handle);
    return AE_OK;
}


/******************************************************************************
 *
 * FUNCTION:    AcpiOsWaitSemaphore
 *
 * PARAMETERS:  Handle              - Handle returned by AcpiOsCreateSemaphore
 *              Units               - How many units to wait for
 *              Timeout             - How long to wait
 *
 * RETURN:      Status
 *
 * DESCRIPTION: Wait for units
 *
 *****************************************************************************/

ACPI_STATUS
AcpiOsWaitSemaphore (
    ACPI_HANDLE         Handle,
    UINT32              Units,
    UINT16              Timeout)
{
    if(Units != 1) {
        USER_PANIC("AcpiOsWaitSemaphore called with Units != 1");
    }

    if (Timeout == ACPI_DO_NOT_WAIT) {
        bool ok = thread_sem_trywait(Handle);
        return ok ? AE_OK : AE_TIME;
    } else {
        assert(Timeout == ACPI_WAIT_FOREVER); // no timeout
        thread_sem_wait(Handle);
        return AE_OK;
    }
}


/******************************************************************************
 *
 * FUNCTION:    AcpiOsSignalSemaphore
 *
 * PARAMETERS:  Handle              - Handle returned by AcpiOsCreateSemaphore
 *              Units               - Number of units to send
 *
 * RETURN:      Status
 *
 * DESCRIPTION: Send units
 *
 *****************************************************************************/

ACPI_STATUS
AcpiOsSignalSemaphore (
    ACPI_HANDLE         Handle,
    UINT32              Units)
{
    if(Units != 1) {
        USER_PANIC("AcpiOsSignalSemaphore called with Units != 1");
    }
    thread_sem_post(Handle);
    return AE_OK;
}


ACPI_STATUS
AcpiOsCreateLock (
    ACPI_SPINLOCK           *OutHandle)
{
    spinlock_t *lock = malloc(sizeof(spinlock_t));
    assert(lock != NULL);
    *lock = 0;
    *OutHandle = (void *)lock;
    return AE_OK;
}

void
AcpiOsDeleteLock (
    ACPI_SPINLOCK           Handle)
{
    free((void *)Handle);
}


ACPI_CPU_FLAGS
AcpiOsAcquireLock (
    ACPI_HANDLE             Handle)
{
    acquire_spinlock((spinlock_t *)Handle);
    return (0);
}


void
AcpiOsReleaseLock (
    ACPI_SPINLOCK           Handle,
    ACPI_CPU_FLAGS          Flags)
{
    release_spinlock(Handle);
}


/******************************************************************************
 *
 * FUNCTION:    AcpiOsInstallInterruptHandler
 *
 * PARAMETERS:  InterruptNumber     Level handler should respond to.
 *              Isr                 Address of the ACPI interrupt handler
 *              ExceptPtr           Where status is returned
 *
 * RETURN:      Handle to the newly installed handler.
 *
 * DESCRIPTION: Install an interrupt handler.  Used to install the ACPI
 *              OS-independent handler.
 *
 *****************************************************************************/

struct interrupt_closure {
    ACPI_OSD_HANDLER handler;
    void *context;
};

static void interrupt_wrapper(void *arg)
{
    struct interrupt_closure *ic = arg;
    printf("ACPI: interrupt!\n");
    ic->handler(ic->context); /* discards return value... what's it for? */
}

UINT32
AcpiOsInstallInterruptHandler (
    UINT32                  InterruptNumber,
    ACPI_OSD_HANDLER        ServiceRoutine,
    void                    *Context)
{
    ACPI_DEBUG("AcpiOsInstallInterruptHandler(%"PRIu32")\n", (uint32_t)InterruptNumber);

    struct interrupt_closure *ic = malloc(sizeof(struct interrupt_closure));
    assert(ic != NULL);

    ic->handler = ServiceRoutine;
    ic->context = Context;

    uint64_t vector;
    errval_t e = inthandler_setup(interrupt_wrapper, ic, &vector);
    ACPI_DEBUG("Allocated local vec %"PRIu64"\n", vector);
    if (err_is_fail(e)) {
        DEBUG_ERR(e, "failed to setup handler function/vector");
        return AE_ERROR;
    }
    
    // The enable and route interrupt call expects interrupt number that are
    // based on 32 (which is the first int number above the exceptions)
    e = enable_and_route_interrupt(InterruptNumber, disp_get_core_id(), vector-32);
    if (err_is_fail(e)) {
        DEBUG_ERR(e, "failed to route interrupt");
        return AE_ERROR;
    }

    return AE_OK;
}

/******************************************************************************
 *
 * FUNCTION:    AcpiOsRemoveInterruptHandler
 *
 * PARAMETERS:  Handle              Returned when handler was installed
 *
 * RETURN:      Status
 *
 * DESCRIPTION: Uninstalls an interrupt handler.
 *
 *****************************************************************************/

ACPI_STATUS
AcpiOsRemoveInterruptHandler (
    UINT32                  InterruptNumber,
    ACPI_OSD_HANDLER        ServiceRoutine)
{
    assert(!"NYI: AcpiOsRemoveInterruptHandler");
    return AE_OK;
}


/******************************************************************************
 *
 * FUNCTION:    AcpiOsExecute
 *
 * PARAMETERS:  Type            - Type of execution
 *              Function        - Address of the function to execute
 *              Context         - Passed as a parameter to the function
 *
 * RETURN:      Status.
 *
 * DESCRIPTION: Execute a new thread
 *
 *****************************************************************************/

ACPI_STATUS
AcpiOsExecute (
    ACPI_EXECUTE_TYPE       Type,
    ACPI_OSD_EXEC_CALLBACK  Function,
    void                    *Context)
{
    struct thread *thread = thread_create((thread_func_t)Function, Context);
    errval_t err = thread_detach(thread);
    assert(err_is_ok(err));
    return (thread == NULL);
}


/******************************************************************************
 *
 * FUNCTION:    AcpiOsBreakpoint
 *
 * PARAMETERS:  Msg                 Message to print
 *
 * RETURN:      Status
 *
 * DESCRIPTION: Print a message and break to the debugger.
 *
 *****************************************************************************/

#if 0 /* unused, undefined -AB */
ACPI_STATUS
AcpiOsBreakpoint (
    char                    *Msg)
{

    if (Msg)
    {
        AcpiOsPrintf ("AcpiOsBreakpoint: %s ****\n", Msg);
    }
    else
    {
        AcpiOsPrintf ("At AcpiOsBreakpoint ****\n");
    }

    return AE_OK;
}
#endif


/******************************************************************************
 *
 * FUNCTION:    AcpiOsStall
 *
 * PARAMETERS:  microseconds        To sleep
 *
 * RETURN:      Blocks until sleep is completed.
 *
 * DESCRIPTION: Sleep at microsecond granularity
 *
 *****************************************************************************/

void
AcpiOsStall (
    UINT32                  microseconds)
{

    if (microseconds)
    {
        errval_t err;
        err = barrelfish_usleep(microseconds);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "barrelfish_usleep(%"PRIu32") in %s",
                    microseconds, __FUNCTION__);
        }
    }
    return;
}


/******************************************************************************
 *
 * FUNCTION:    AcpiOsSleep
 *
 * PARAMETERS:  milliseconds        To sleep
 *
 * RETURN:      Blocks until sleep is completed.
 *
 * DESCRIPTION: Sleep at millisecond granularity
 *
 *****************************************************************************/

void
AcpiOsSleep (
    ACPI_INTEGER            milliseconds)
{

    if (milliseconds > 0) {
        assert(!"NYI: AcpiOsSleep");
    }

    return;
}

/******************************************************************************
 *
 * FUNCTION:    AcpiOsGetTimer
 *
 * PARAMETERS:  None
 *
 * RETURN:      Current time in 100 nanosecond units
 *
 * DESCRIPTION: Get the current system time
 *
 *****************************************************************************/

UINT64
AcpiOsGetTimer (void)
{
    assert(!"NYI: AcpiOsGetTimer");
    return 0;
#if 0
    struct timeval  time;

    gettimeofday(&time, NULL);

    /* Seconds * 10^7 = 100ns(10^-7), Microseconds(10^-6) * 10^1 = 100ns */

    return (((UINT64) time.tv_sec * 10000000) + ((UINT64) time.tv_usec * 10));
#endif
}



/******************************************************************************
 *
 * FUNCTION:    AcpiOsReadPciConfiguration
 *
 * PARAMETERS:  PciId               Seg/Bus/Dev
 *              Register            Device Register
 *              Value               Buffer where value is placed
 *              Width               Number of bits
 *
 * RETURN:      Status
 *
 * DESCRIPTION: Read data from PCI configuration space
 *
 *****************************************************************************/

ACPI_STATUS
AcpiOsReadPciConfiguration (
    ACPI_PCI_ID             *PciId,
    UINT32                  Register,
    UINT64                  *Value,
    UINT32                  Width)
{
    mackerel_pci_t addr = {
        .bus = PciId->Bus,
        .device = PciId->Device,
        .function = PciId->Function,
    };

    //printf("AcpiOsReadPciConfiguration(%d %d %d %d %d)\n",
    //       addr.bus, addr.device, addr.function, Register, Width);

    switch (Width) {
    case 32:
        *(uint32_t *)Value = mackerel_read_pci_32(addr, Register);
        break;

    case 16:
        *(uint16_t *)Value = mackerel_read_pci_16(addr, Register);
        break;

    case 8:
        *(uint8_t *)Value = mackerel_read_pci_8(addr, Register);
        break;

    default:
        return AE_ERROR;
    }

    return AE_OK;
}


/******************************************************************************
 *
 * FUNCTION:    AcpiOsWritePciConfiguration
 *
 * PARAMETERS:  PciId               Seg/Bus/Dev
 *              Register            Device Register
 *              Value               Value to be written
 *              Width               Number of bits
 *
 * RETURN:      Status.
 *
 * DESCRIPTION: Write data to PCI configuration space
 *
 *****************************************************************************/

ACPI_STATUS
AcpiOsWritePciConfiguration (
    ACPI_PCI_ID             *PciId,
    UINT32                  Register,
    ACPI_INTEGER            Value,
    UINT32                  Width)
{
    mackerel_pci_t addr = {
        .bus = PciId->Bus,
        .device = PciId->Device,
        .function = PciId->Function,
    };

    switch (Width) {
    case 32:
        mackerel_write_pci_32(addr, Register, Value);
        break;

    case 16:
        mackerel_write_pci_16(addr, Register, Value);
        break;

    case 8:
        mackerel_write_pci_8(addr, Register, Value);
        break;

    default:
        return AE_ERROR;
    }

    return AE_OK;
}


/******************************************************************************
 *
 * FUNCTION:    AcpiOsReadMemory
 *
 * PARAMETERS:  Address             Physical Memory Address to read
 *              Value               Where value is placed
 *              Width               Number of bits
 *
 * RETURN:      Value read from physical memory address
 *
 * DESCRIPTION: Read data from a physical memory address
 *
 *****************************************************************************/

ACPI_STATUS
AcpiOsReadMemory (
    ACPI_PHYSICAL_ADDRESS   Address,
    UINT64                  *Value,
    UINT32                  Width)
{
    assert(!"NYI: AcpiOsReadMemory");

    switch (Width)
    {
    case 8:
    case 16:
    case 32:
        *Value = 0;
        break;

    default:
        return (AE_BAD_PARAMETER);
        break;
    }
    return (AE_OK);
}


/******************************************************************************
 *
 * FUNCTION:    AcpiOsWriteMemory
 *
 * PARAMETERS:  Address             Physical Memory Address to write
 *              Value               Value to write
 *              Width               Number of bits
 *
 * RETURN:      None
 *
 * DESCRIPTION: Write data to a physical memory address
 *
 *****************************************************************************/

ACPI_STATUS
AcpiOsWriteMemory (
    ACPI_PHYSICAL_ADDRESS   Address,
    UINT64                  Value,
    UINT32                  Width)
{
    assert(!"NYI: AcpiOsWriteMemory");
    return (AE_OK);
}


ACPI_THREAD_ID
AcpiOsGetThreadId(void)
{
    return (ACPI_THREAD_ID)(uintptr_t)thread_self();
}


/******************************************************************************
 *
 * FUNCTION:    AcpiOsSignal
 *
 * PARAMETERS:  Function            ACPI CA signal function code
 *              Info                Pointer to function-dependent structure
 *
 * RETURN:      Status
 *
 * DESCRIPTION: Miscellaneous functions
 *
 *****************************************************************************/

ACPI_STATUS
AcpiOsSignal (
    UINT32                  Function,
    void                    *Info)
{
    assert(!"NYI: AcpiOsSignal");

    switch (Function)
    {
    case ACPI_SIGNAL_FATAL:
        break;

    case ACPI_SIGNAL_BREAKPOINT:

        if (Info)
        {
            AcpiOsPrintf ("AcpiOsBreakpoint: %s ****\n", Info);
        }
        else
        {
            AcpiOsPrintf ("At AcpiOsBreakpoint ****\n");
        }

        break;
    }


    return (AE_OK);
}


/*-
 * Copyright (c) 2000 Michael Smith
 * Copyright (c) 2000 BSDi
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */


/* Section 5.2.9.1:  global lock acquire/release functions */
#define GL_ACQUIRED     (-1)
#define GL_BUSY         0
#define GL_BIT_PENDING  0x1
#define GL_BIT_OWNED    0x2
#define GL_BIT_MASK     (GL_BIT_PENDING | GL_BIT_OWNED)

/*
 * Acquire the global lock.  If busy, set the pending bit.  The caller
 * will wait for notification from the BIOS that the lock is available
 * and then attempt to acquire it again.
 */
int acpi_acquire_global_lock(uint32_t *lock)
{
    uint32_t new, old;

    do {
        old = *lock;
        new = ((old & ~GL_BIT_MASK) | GL_BIT_OWNED) |
               ((old >> 1) & GL_BIT_PENDING);
    } while (!__sync_bool_compare_and_swap(lock, old, new));

    return ((new < GL_BIT_MASK) ? GL_ACQUIRED : GL_BUSY);
}

/*
 * Release the global lock, returning whether there is a waiter pending.
 * If the BIOS set the pending bit, OSPM must notify the BIOS when it
 * releases the lock.
 */
int acpi_release_global_lock(uint32_t *lock)
{
    uint32_t new, old;

    do {
        old = *lock;
        new = old & ~GL_BIT_MASK;
    } while (!__sync_bool_compare_and_swap(lock, old, new));

    return (old & GL_BIT_PENDING);
}
