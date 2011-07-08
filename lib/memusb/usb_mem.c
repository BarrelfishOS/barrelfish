/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <stdint.h>
#include <stdlib.h>
#include <timer/timer.h>
#include <skb/skb.h>


#include <usb/mem/usb_mem.h>

#if defined(EHCI_MEM_DEBUG)
#define dprintf(x...) printf("\nUSB_MEM: " x)
#else
#define dprintf(x...) ((void)0)
#endif

//#define SIZE_QH 48
/* XXX: The queue head size is 48 bytes but it has to be 
 * 32 bytes aligned. So stright forward way to allow this 
 * with out simple scheme is to have internal fragmentation of 16 bytes
 * per queue head. 
 */

//FIXME: This number is allocated statically (for nos4)
//but should be retrived from SKB. 

#define NO_OF_CORES 4

typedef struct mem_aff {
    uint64_t base;
    uint64_t limit;
    int valid;
} mem_aff;

static mem_aff mem_affinity_table[NO_OF_CORES];

/*
 * \brief Updates the mem affinity structure by contacing the SKB. 
 */

static void update_list(struct timer *t, void *data)
{
    dprintf(" updating the list \n");
    struct mem_aff {
        uint64_t base, limit;
    };
    struct mem_aff memory_affinities[20];
    uintptr_t idx = 0;

    char query[128];
    uint64_t core_id;

    for (int cpu_id = 0; cpu_id < NO_OF_CORES; cpu_id++) {
        idx = 0;

        core_id = cpu_id;       //disp_get_core_id();

        sprintf(query, "local_memory_affinity(%lu, List)"
                ",length(List,Len),"
                "write(output,len(Len)),write(output,List).", core_id);

        int error = 10;         //initialize with some number != 0
        dprintf("\nexecute query...\n\n %s \n\n", query);
        int no_of_attempts = 10;

        while (error != 0) {
            skb_execute(query);
            while (skb_read_error_code() == SKB_PROCESSING)
                messages_wait_and_handle_next();
            error = skb_read_error_code();
            thread_yield();
            no_of_attempts--;
            dprintf("while error !=0 loop ...error [%x]\n", error);
            if (no_of_attempts < 0)
                break;
        }
        if (error != 0) {       /* Failed communication */
            dprintf(" Error condition with code [%x] and core[%llu]",
                    error, (unsigned long long int)core_id);
            goto end_processing;
        }


        dprintf("\nquery executed.\n");
        dprintf("\nresult = %s\n", skb_get_output());
        char *output = skb_get_output();
        while (!((output[0] == 'l') &&
                 (output[1] == 'e') && (output[2] == 'n')))
            output++;
        output += 4;
        int len = atoi(output);
        dprintf("\nprocess %u elements.\n", len);
        for (int i = 0; i < len; i++) {
            while (!(output[0] == 'r'))
                output++;
            while (!((output[0] >= '0') && (output[0] <= '9')))
                output++;
            uint64_t base = atol(output);
            while (!(output[0] == ','))
                output++;
            while (!((output[0] >= '0') && (output[0] <= '9')))
                output++;
            uint64_t limit = atol(output);
            memory_affinities[idx].base = base;
            memory_affinities[idx].limit = limit;
            idx++;
        }
        for (int i = 0; i < idx; i++) {
            dprintf("found affinity %d: [%lx, %lx]\n", i,
                    memory_affinities[i].base, memory_affinities[i].limit);
        }
        mem_affinity_table[core_id].base = memory_affinities[0].base;
        mem_affinity_table[core_id].limit = memory_affinities[idx - 1].limit;
        mem_affinity_table[core_id].valid = 1;
    }

  end_processing:
    dprintf("\n Ending the control flow in timer ...\n\n");

}

/*
 * \brief It init the mem affinity struct and schedule the periodic update 
 *        by poking SKB. 
 */

static void init_numa(void)
{

    for (int i = 0; i < NO_OF_CORES; i++) {
        mem_affinity_table[i].base = 0;
        mem_affinity_table[i].limit = 0;
        mem_affinity_table[i].valid = -1;
    }

    dprintf(" %s : %s \n", __FILE__, __func__);
    errval_t err = timer_init();
    dprintf(" timer is init \n");
    assert(err_is_ok(err));

    skb_client_connect();
    dprintf("\n Before creating the bufder \n");
    skb_create_buffer();
    dprintf("\n After creating the bufder \n");

    struct timer *timer = timer_create(1000, false, update_list, 0);
    dprintf(" timer is scheduled \n");
    timer_start(timer);
    dprintf(" timer is started \n");

}

#define SIZE_QH 64
#define SIZE_QE 32

#define TOTAL_QH (BASE_PAGE_SIZE/SIZE_QH)
#define TOTAL_QE (BASE_PAGE_SIZE/SIZE_QE)

#define GB (1024 * 1024 * 1024)

static uint64_t qh_count = 0, qe_count = 0, qh_total = 0, qe_total =
    0, usb_pages = 0, qh_free = 0, qe_free = 0;

//Assuming that 0xFFF...FF is invalid value 
static uint64_t ehci_core_id = -1;
static uint64_t self_core_id = -1;

typedef struct qh_ll {
    usb_page page;
    int total;
    int free;
    int used;
    int next_free;              // Index into the array;
    usb_mem arr[TOTAL_QH];
    struct qh_ll *next;
    uint64_t id;
} qh_ll;


typedef struct qe_ll {
    usb_page page;
    int total;
    int free;
    int used;
    int next_free;              // Index into the array;
    usb_mem arr[TOTAL_QE];
    struct qe_ll *next;
    uint64_t id;
} qe_ll;

typedef struct page_list_t {
    usb_page page;
    uint32_t no_of_frames;
    struct page_list_t *next;
} page_list_t;


struct qh_ll *qh_head = NULL;
struct qe_ll *qe_head = NULL;
struct page_list_t *page_head = NULL;


uint64_t qh_capacity = 0, qe_capacity = 0;

/*
 * \brief Prints the USB page link list dump
 */

static void print_usb_page_dump(void)
{
    page_list_t *node = page_head;
    while (node != NULL) {
        dprintf("\nnode mapped at va[%p] pa[%p] frame_count[%u]\n",
                node->page.va, node->page.pa, node->no_of_frames);
        node = node->next;
    }
}

/* 
 * \brief  Inserts a node into the internal USB pages list. 
 *
 * \param page usb_page struct to be insterted in list
 * \param num  This cap represents how many pages 
 */

static void insert_page(usb_page page, uint32_t num)
{
    page_list_t *node = (page_list_t *) malloc(sizeof (page_list_t));
    node->page = page;
    node->no_of_frames = num;

    if (page_head == NULL)
        node->next = NULL;
    else
        node->next = page_head;

    page_head = node;
}

/*
 * \brief Removes and update internal USB pages list
 *
 * \param node Node to removed
 */

static void remove_node(page_list_t * node)
{
    print_usb_page_dump();
    page_list_t *prev = page_head;
    if (page_head == node) {
        page_head = node->next;
        dprintf("\n Everything looks good. Head removed");
        usb_pages -= node->no_of_frames;
        free(node);
        return;
    }

    while (prev) {
        if (prev->next == node)
            break;

        prev = prev->next;
    }

    assert(prev != NULL);
    prev->next = prev->next->next;
    dprintf("\n Everything looks good. Updating the counter");
    usb_pages -= node->no_of_frames;
    free(node);
}


/*
 * \brief Resets the memory alloc range to whole 8 GB.
 *        We have to stick with 8 GB because controller is 32 bit. 
 */

static void reset_range(void)
{
    // Use whole ram (less than 4 GB)
    // because controller is 32 bit
    uint64_t GB4 = 4;
    GB4 *= GB;
    ram_set_affinity(0, GB4);
}


/*
 * \brief Sets the memory range near the given core_id. 
 *        
 * \param on_core Core_id for which caller want to set the range 
 */

static void set_range(uint8_t on_core)
{
    assert(on_core < NO_OF_CORES);

    if (mem_affinity_table[on_core].valid == -1) {
        reset_range();
        return;
    }


    ram_set_affinity(mem_affinity_table[on_core].base,
                     mem_affinity_table[on_core].limit);

}

/*
 * \brief Sets the memory range to near EHCI. Caller 
 *        does not have to be aware of on which core ehci is running. 
 *        This information is passed to lib at the boot time in 
 *        usb_mem_init function. 
 */

static void set_range_to_ehci(void)
{
    set_range(ehci_core_id);
}

/*
 *  \brief Maps a new capability into the domain. 
 *
 *  \param num  how many frames. 
 *  \param flag allocation type. Could be any of these values  
 *               (USB_NEAR_EHCI, USB_NEAR_SELF or USB_DONT_CARE
 */

static struct usb_page map_new_frame(int num, uint8_t flag)
{
    int r = 0;
    struct capref frame;
    struct frame_identity frame_id = { .base = 0, .bits = 0 };
    struct usb_page map;

    if (flag == USB_NEAR_EHCI && ehci_core_id != -1)
        set_range_to_ehci();
    else if (flag == USB_NEAR_SELF && self_core_id != -1)
        set_range(self_core_id);

    // In case DONOT_CARE...we just fall through 

    // XXX: IMPORTANT - Assuming that num pages will be contigious 
    //      otherwise ...:-( !! 
    int total_size = BASE_PAGE_SIZE * num;
    r = frame_alloc(&frame, total_size, NULL);
    assert(r == 0);
    r = invoke_frame_identify(frame, &frame_id);
    assert(r == 0);
    void *va;
    r = vspace_map_one_frame_attr(&va, total_size, frame,
                                  VREGION_FLAGS_READ_WRITE_NOCACHE,
                                  NULL, NULL);
    assert(r == 0);
    map.va = va;
    map.frame = frame;
    map.frame_id = frame_id;
    map.valid = 1;
    map.pa = (void *)frame_id.base;

    insert_page(map, num);
    usb_pages += num;

    dprintf("\n EHCI: A new frame is allocated   PADDR %lx     VADDR %lx ",
            (uint64_t) map.frame_id.base, (uint64_t) map.va);

    return map;
}

/*
 * \brief Maps a given capability into caller's domain.  
 */

void *map_cap(struct capref cap, uint32_t sz)
{
    void *retval;
    errval_t err = vspace_map_one_frame(&retval, sz, cap, NULL, NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "vspace_map_one_frame failed");
        return NULL;
    }
    return retval;
}

/*
 * \brief Allocates an I/O buffer of size sz. 
 *
 * \param sz   size of the I/O buffer. 
 * \param flag for NUMA aware allocation 
 */

usb_mem malloc_iobuff(uint32_t sz, uint8_t flag)
{
    // FIXME: This is very poor memory managment 
    // code for EHCI. Even for a small io buffer request 
    // it allocates a whole new frame. 
    int no_frames = (sz / BASE_PAGE_SIZE);
    if (sz % BASE_PAGE_SIZE != 0)       //spilled data into next frame
        no_frames++;

    usb_page map = map_new_frame(no_frames, flag);
    //reset the range for neq requests
    set_range(self_core_id);

    usb_mem mem;
    mem.va = map.va;
    mem.pa = map.pa;
    mem.type = EHCI_MEM_TYPE_IO;
    mem.free = 1;
    mem.size = sz;
    mem.cap = map.frame;

    return mem;
}

/*
 * \brief Initiates the mapping of queue head or queue element on a newly 
 *        mapped frame. 
 *
 * \param arr      To be mapped usb memory array. 
 * \param size     To differentiate between queue head and queue element 
 * \param start_va Starting virtual address 
 * \param start_pa Starting physical address 
 */

static void init_map(usb_mem * arr, int size, void *start_va, void *start_pa)
{
    int i = 0;

    // TODO: Fix this to support iTDs.
    int typ = (size == SIZE_QH) ? EHCI_MEM_TYPE_QH : EHCI_MEM_TYPE_QE;
    int to = (size == SIZE_QH) ? TOTAL_QH : TOTAL_QE;
    if (size == SIZE_QH)
        qh_total += TOTAL_QH;
    else
        qe_total += TOTAL_QE;

    for (i = 0; i < to; i++) {
        arr[i].va = start_va + (i * size);
        arr[i].pa = start_pa + (i * size);
        arr[i].type = typ;
        arr[i].free = EHCI_MEM_FREE;
        arr[i].size = size;
    }
}

/* 
 * \brief Adds a new node(node contains a new page) to queue head 
 *        link list. 
 */

static void add_mem_qh(void)
{
    struct qh_ll *temp = (qh_ll *) malloc(sizeof (struct qh_ll));
    temp->page = map_new_frame(1, USB_NEAR_SELF);
    temp->total = TOTAL_QH;
    temp->free = TOTAL_QH;
    temp->used = 0;
    temp->next_free = 0;
    temp->id = qh_head->id + 1;
    init_map(temp->arr, SIZE_QH, temp->page.va, temp->page.pa);

    // Linking 
    temp->next = qh_head;
    qh_head = temp;

    // add to capacity
    qh_capacity = qh_capacity + TOTAL_QH;

    dprintf("\n EHCI_MEM: adding a new page to usb memory\
                                ...of queue head type, id [%lu]", qh_head->id);
}


/* 
 * \brief Adds a new node(node contains a new page) to queue element 
 *        link list. 
 */
static void add_mem_qe(void)
{
    struct qe_ll *temp = (qe_ll *) malloc(sizeof (struct qe_ll));
    temp->page = map_new_frame(1, USB_NEAR_SELF);
    temp->total = TOTAL_QE;
    temp->free = TOTAL_QE;
    temp->used = 0;
    temp->next_free = 0;
    temp->id = qe_head->id + 1;

    init_map(temp->arr, SIZE_QE, temp->page.va, temp->page.pa);
    dprintf("\n EHCI_MEM: Queue head is at [%lx] \
                        and its next is [%lx]", (uint64_t) qe_head, (uint64_t) qe_head->next);

    // Linking 
    temp->next = qe_head;
    qe_head = temp;
    dprintf("\n EHCI_MEM: adding memory to queue element \
                        link list temp node=[%lx] temp page=[%lx]", (uint64_t) temp, (uint64_t) temp->page.va);
    dprintf("\n EHCI_MEM: now qe_head->next [%lx] \
                        queue_head is at [%lx]", (uint64_t) qe_head->next, (uint64_t) qe_head);

    // add to capacity
    qe_capacity = qe_capacity + TOTAL_QE;
}


/*
 * \brief Inits the memory lib
 */

void usb_mem_init(void)
{
    static bool init = false;
    if (!init) {
        // This function initiates 2 parallel link lists
        // 1. For queue heads, which is represented by qh_head
        // 2. For queue elements, which is represented by qe_head

        //All allocation of local data should be done on near
        //memory
        init_numa();
        self_core_id = disp_get_core_id();
        set_range(self_core_id);

        // Queue head (64 bytes) list 

        if (qh_head == NULL)
            qh_head = (qh_ll *) malloc(sizeof (struct qh_ll));
        qh_capacity = 0;

        qh_head->page = map_new_frame(1, USB_NEAR_SELF);
        qh_head->total = TOTAL_QH;
        qh_head->free = TOTAL_QH;
        qh_head->used = 0;
        qh_head->next_free = 0;
        qh_head->id = 0;
        qh_head->next = NULL;
        init_map(qh_head->arr, SIZE_QH, qh_head->page.va, qh_head->page.pa);

        qh_capacity = qh_capacity + TOTAL_QH;

        // Queue element (32 bytes) list 
        if (qe_head == NULL)
            qe_head = (qe_ll *) malloc(sizeof (struct qe_ll));

        qe_capacity = 0;
        qe_head->page = map_new_frame(1, USB_NEAR_SELF);
        qe_head->total = TOTAL_QE;
        qe_head->free = TOTAL_QE;
        qe_head->used = 0;
        qe_head->id = 0;
        qe_head->next_free = 0;
        qe_head->next = NULL;
        init_map(qe_head->arr, SIZE_QE, qe_head->page.va, qe_head->page.pa);

        qe_capacity = qe_capacity + TOTAL_QE;
        init = true;
    }
}

/*
 * \brief Adds more memory to the nodes. 
 */
static void add_mem(int type)
{
    if (type == EHCI_MEM_TYPE_QH)
        add_mem_qh();
    else if (type == EHCI_MEM_TYPE_QE)
        add_mem_qe();
    else
        assert(!"EHCI: Undefined type of call to \
                                add memory in EHCI malloc utility");
}

/*
 *  \brief Updates the next free pointer in a link list. 
 */
static int update_next(usb_mem * arr, int size)
{
    int i = 0;
    dprintf("\n EHCI_MEM: updating the next free ...");
    while (i < size) {
        if (arr[i].free == 1)
            return i;

        i++;
    }

    // To indicate that we have consumed 
    // all blocks from this node
    return -1;
}

/*
 * \brief Allocates the memory on the passed node. 
 *
 * \param node queue head node on which allocation is to be done.
 */
static struct usb_mem allocate_on_node_qh(qh_ll * node)
{
    dprintf("\n EHCI_MEM: Node [%lu] for allocation is selected\
                                ....next_free to be at [%d]", node->id, node->next_free);
    int to_return;
    usb_mem mem;
    assert(node->free != 0);
    node->free--;
    node->used++;

    assert(node->next_free != -1);

    to_return = node->next_free;
    node->arr[to_return].free = 0;      // Marked it as used 
    node->next_free = update_next(node->arr, TOTAL_QH);

    qh_capacity--;

    mem = node->arr[to_return];

    mem.free = 0;               // set slot as used 
    qh_count++;

    return mem;
}

/*
 * \brief Allocates the memory on the passed node. 
 *
 * \param node queue element node on which allocation is to be done.
 */

static struct usb_mem allocate_on_node_qe(qe_ll * node)
{
    dprintf("\n EHCI_MEM: Node [%lu] for allocation is selected \
                        ....next_free to be at [%d]", node->id, node->next_free);
    int to_return;
    usb_mem mem;
    assert(node->free != 0);
    node->free--;
    node->used++;

    assert(node->next_free != -1);

    to_return = node->next_free;
    node->arr[to_return].free = 0;      // Marked it as used 
    node->next_free = update_next(node->arr, TOTAL_QE);

    qe_capacity--;

    mem = node->arr[to_return];

    mem.free = 0;               // set slot as used 
    qe_count++;

    return mem;
}

/*
 * \brief Locates the node for a given usb_mem element in queue element 
 *        link list. 
 */

static void *locate_qe(usb_mem mem)
{
    qe_ll *temp = qe_head;
    dprintf("\n before locates while loop temp[%lx] \
                        page address [%lx]", (uint64_t) temp, (uint64_t) temp->page.va);

    while (temp != NULL) {
        if (mem.va >= temp->page.va && mem.va <= temp->page.va + BASE_PAGE_SIZE) {
            dprintf("\n EHCI_MEM: locate returned non \
                                        NULL node %p", temp);
            return temp;
        }

        temp = temp->next;
        dprintf("\n temp update [%lx] page address [%lx]",
                (uint64_t) temp, (uint64_t) temp->page.va);
    }

    return NULL;
}


/*
 * \brief Locates the node for a given usb_mem element in queue head 
 *        link list. 
 */

static void *locate_qh(usb_mem mem)
{
    qh_ll *temp = qh_head;

    dprintf("\n before locates while loop temp[%lx] \
                        page address [%lx]", (uint64_t) temp, (uint64_t) temp->page.va);
    while (temp != NULL) {
        if (mem.va >= temp->page.va && mem.va <= temp->page.va + BASE_PAGE_SIZE) {
            dprintf("\n EHCI_MEM: locate returned non NULL\
                                        node %p", temp);
            return temp;
        }

        temp = temp->next;
        dprintf("\n temp update [%lx] page address [%lx]",
                (uint64_t) temp, (uint64_t) temp->page.va);
    }

    return NULL;

}

/*
 * \brief Locates a given usb_mem element in either link list.
 */

static void *locate(usb_mem mem, int size)
{
    if (size == SIZE_QH)
        return locate_qh(mem);
    else
        return locate_qe(mem);
}


/*
 * \brief Frees a slot containing the given usb mem element on node 
 *        (queue head list). 
 */

static void free_on_node_qh(struct qh_ll *node, usb_mem mem)
{
    uint64_t diff = mem.va - node->page.va;
    uint64_t index = diff / SIZE_QH;

    dprintf("\n EHCI_MEM: Node [%lu] Index to free found to be at [%lu]",
            node->id, index);

    node->arr[index].free = 1;  // Mark slot as free and invalid

    node->free++;
    node->used--;

    qh_capacity++;
    qh_free++;

    // XXX: Sort of HACK, so the last freed slot will be used again 
    // in next allocation. We do not have to update this. 
    node->next_free = index;
}

/*
 * \brief Frees a slot containing the given usb mem element on node 
 *        (queue element list). 
 */
static void free_on_node_qe(struct qe_ll *node, usb_mem mem)
{
    uint64_t diff = mem.va - node->page.va;
    uint64_t index = diff / SIZE_QE;

    dprintf("\n EHCI_MEM: Node [%lu] Index to free found to be at [%lu]",
            node->id, index);

    node->arr[index].free = 1;  // Mark slot as free and invalid

    node->free++;
    node->used--;

    qe_capacity++;
    qe_free++;

    // XXX: Sort of HACK, so the last freed slot will be used again 
    // in next allocation. We do not have to update this. 
    node->next_free = index;    // update_next(node->arr, TOTAL_QE);
}


/*
 * ----- External interface functions -----
 */

/*
 * \brief Sets the ehci core id on which the EHCI is running. Use for 
 *        NUMA aware allocation. 
 */

void set_ehci_core_id(uint64_t core)
{
    ehci_core_id = core;
}

/*
 * \brief Allocates a queue head worth of memory chunk. (64 Bytes)
 */

struct usb_mem malloc_qh(void)
{
    qh_ll *temp = NULL;

    if (qh_capacity == 0)
        add_mem(EHCI_MEM_TYPE_QH);

    dprintf("\n EHCI_MEM: searching for proper node to allocate ");

    temp = qh_head;

    // Search first block to be free
    while (temp->free <= 0 && temp != NULL)
        temp = temp->next;

    assert(temp != NULL);       // To test sanity 

    return allocate_on_node_qh(temp);
}


/*
 * \brief Allocates N queue heads worth of memory chunk. (64*N Bytes)
 *
 * \param n How many queue heads. 
 */
struct usb_mem *malloc_qh_n(int n)
{
    int i;
    usb_mem *mem_arr = (usb_mem *) malloc(n * sizeof (usb_mem));

    for (i = 0; i < n; i++) {
        mem_arr[i] = malloc_qh();
    }

    return mem_arr;
}

/*
 * \brief Allocates a queue element worth of memory chunk. (32 Bytes)
 */

struct usb_mem malloc_qe(void)
{
    qe_ll *temp = NULL;

    if (qe_capacity == 0)
        add_mem(EHCI_MEM_TYPE_QE);

    dprintf("\n EHCI_MEM: searching for proper node to allocate ");

    temp = qe_head;

    while (temp->free <= 0 && temp != NULL)     // Search first block to be free
        temp = temp->next;

    assert(temp != NULL);       // To test sanity 

    return allocate_on_node_qe(temp);
}


/*
 * \brief Allocates N queue elements worth of memory chunk. (32*N Bytes)
 *
 * \param n How many queue elements. 
 */

struct usb_mem *malloc_qe_n(int n)
{
    int i;
    usb_mem *mem_arr = (usb_mem *) malloc(n * sizeof (usb_mem));

    for (i = 0; i < n; i++) {
        mem_arr[i] = malloc_qe();
    }

    return mem_arr;
}

/*
 * \brief Free equivalent of the malloc function. Frees up the slot of 
 *        passed mememory element in the queue head. 
 *
 * \param mem Memory element. 
 */

void free_qh(usb_mem mem)
{
    qh_ll *node = (qh_ll *) locate(mem, SIZE_QH);

    if (node == NULL) {
        dprintf("\n EHCI_MEM: passed queue head element\
                                [pa=%p] [va=%p] does not belongs to the \
                                interanl malloc utility\n\
                                Ignoring the freeing request", mem.pa, mem.va);
    } else
        free_on_node_qh(node, mem);
}

/*
 * \brief Free equivalent of the malloc function. Frees up the slot of 
 *        passed mememory element in the queue element. 
 *
 * \param mem Memory element. 
 */

void free_qe(usb_mem mem)
{
    qe_ll *node = (qe_ll *) locate(mem, SIZE_QE);
    if (node == NULL) {
        dprintf("\n EHCI_MEM: passed queue element \
                                [pa=%p] [va=%p] does not belongs to the\
                                interanl malloc utility\n\
                                Ignoring the freeing request", mem.pa, mem.va);
    } else
        free_on_node_qe(node, mem);
}

/*
 *  \brief Free a allocated I/O buffer
 *
 *  \param iobuff I/O buffer allocated through malloc_iobuff function
 */

void free_iobuff(usb_mem iobuff)
{
    errval_t err;
    if (iobuff.type != EHCI_MEM_TYPE_IO || page_head == NULL) {
        printf("\n USB_MEM: False type memory passed. Ignoring !!\n");
        // False type of memory passed, ignore
        return;
    }
    //Locate the internal node in list to be freed 
    page_list_t *node = page_head;
    while (node != NULL) {
        if (node->page.va == iobuff.va && node->page.pa == iobuff.pa)
            break;
        node = node->next;
    }
    assert(node != NULL);
    dprintf(" node found to be mapped at pa[%p] va[%p]", node->page.pa,
            node->page.va);
    remove_node(node);
    err = cap_destroy(iobuff.cap);
}

/*
 * \brief Wrapper funtion for 64 bytes allocation. 
 */

usb_mem malloc_64(void)
{
    return malloc_qh();
}

/*
 * \brief Wrapper funtion for 32 bytes allocation. 
 */


usb_mem malloc_32(void)
{
    return malloc_qe();
}


/*
 * \brief Wrapper funtion for freeing up 64 bytes allocation. 
 */

void free_64(usb_mem mem)
{
    free_qh(mem);
}

/*
 * \brief Wrapper funtion for freeing up 32 bytes allocation. 
 */
void free_32(usb_mem mem)
{
    free_qe(mem);
}

/*
 * \brief Debugging function to print current queue head list status
 */

void print_qh_list(void)
{
    qh_ll *temp = qh_head;
    while (temp != NULL) {
        printf
            ("\nEHCI_MEM: Node [%lu], total=[%d] free=[%d] used=[%d] next_free=[%d]",
             temp->id, temp->total, temp->free, temp->used, temp->next_free);
        temp = temp->next;
    }
}

/*
 * \brief Prints out the paramets of passed usb memory element. 
 */
void print_usb_mem(usb_mem mem)
{
    printf("\n -------- EHCI_MEM node {%lx]------- ", (uint64_t) & mem);
    printf("\n [VA = %lx] [PA = %lx]", (uint64_t) mem.va, (uint64_t) mem.pa);
    printf("\n [type = %x] [free = %x]", mem.type, mem.free);
    printf("\n -------- END EHCI_MEM node ------- ");
}

/*
 * \brief Prints current memory status
 */

void print_memory_stats(void)
{
    printf("\n\n------- EHCI MEMORY STATS-----------\n");
    printf("\n Total frames in use                                : %lu ",
           usb_pages);
    printf
        ("\n Total queue heads  (64 B) in used/freed/capacity   : %lu / %lu / %lu",
         qh_count, qh_free, qh_total);
    printf
        ("\n Total queue elements (32 B) in used/freed/capacity : %lu / %lu / %lu",
         qe_count, qe_free, qe_total);
    printf("\n\n------- ---------------- -----------\n");
}
