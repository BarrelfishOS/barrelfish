/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/* 
 * Contains logic for I/O requests 
 * preparation logic
 */

#include <stdio.h>
#include <stdlib.h>

#include <usb/utility.h>
#include "error_codes.h"

#include <usb/mem/usb_mem.h>
#include <usb/usb_device.h>

#include "async_queue.h"
#include "ehci_status.h"
#include "ehci_core.h"

//#define EHCI_LOCAL_DEBUG 
#include "ehci_debug.h"


#include "context.h"
#include "toggle_state.h"

//FIXME: make an array and do lookup 
static struct ehci_service_response *resp = NULL;

static uint32_t get_dbug_flag(void)
{
    static uint32_t x = 0xABCDEF11;
    return x++;
}

void set_context(struct ehci_service_response *rsp)
{
    resp = rsp;
}

struct ehci_service_response *get_context(void)
{
    return resp;
}

static uint8_t get_setup_data_direction(usb_device_request req)
{
    uint8_t flag = (req.bmRequestType >> 7) && 1;

    if (flag == 1)
        return USB_IN_TOKEN;
    else
        return USB_OUT_TOKEN;

}

static uint8_t get_expected_data_size(usb_device_request req)
{
    return req.wLength;

}


static void print_qTDs_dump(qhead_wrapper_t * node)
{
    // Enable it selectively, creates a big dump 
    // in case of large I/O requests. 
    return;
    dprintf("Total number of queue elements [%d] \n", node->n);
    qTD_wrapper_t *elem = node->qe_next;
    while (elem != NULL) {
        dprintf("PA is [%lx]  & >> 5 [%lx]\n",
                (uintptr_t) elem->mem.pa, (uintptr_t) elem->mem.pa >> 5);
        print_qTD(elem->q_elem);
        elem = elem->next;
    }

}

/*
 * \brief Wapper function which allocates and wrappes 
 *        queue elements in a particular queue, specified by
 *        queue head wrapper. 
 *
 * \param mem_arr Array containing queue elements 
 * \param qh_wrp  Queue head wrapper where new element wrapper are put
 */

static void wrap_q_elements(usb_mem * mem_arr, int n, qhead_wrapper_t * qh_wrp)
{
    dprintf(" -->> ??\n");
    qTD_wrapper_t *data_wrp_arr =
        (qTD_wrapper_t *) malloc(sizeof (struct qTD_wrapper_t) * n);
    dprintf(" --->> !!\n");

    // We have wrappers, assign them
    dprintf("%s: Wrapper assignment loop\n", __func__);
    for (int i = 0; i < n; i++) {
        data_wrp_arr[i].mem = mem_arr[i];
        data_wrp_arr[i].q_elem = (qTD *) mem_arr[i].va;
        if (i < n - 1)
            data_wrp_arr[i].next = &data_wrp_arr[i + 1];
        data_wrp_arr[i].qhw = qh_wrp;
    }

    dprintf("%s: Prev link setup\n", __func__);
    //set prev links 
    for (int i = n - 1; i > 0; i--)
        data_wrp_arr[i].prev = &data_wrp_arr[i - 1];

    // set terminal links
    data_wrp_arr[0].prev = data_wrp_arr[n - 1].next = NULL;
    dprintf("%s: terminal prev, next set to NULL\n", __func__);

    //set qh_wrp pointers
    qh_wrp->qe_last = &data_wrp_arr[n - 1];
    qh_wrp->qe_next = &data_wrp_arr[0];

    dprintf("%s: qe_last and qe_next are set\n ", __func__);

}


/*
 * \brief Allocates and assign queue elements array and does linkage 
 *
 * \param sz Size of the request 
 * \param pa I/O buffer 
 * \param pid type of request 
 * \param status status of request 
 * \param  qhw Queue head wrapper 
 */

static void malloc_qTDs(uint32_t sz, void *pa,
                        uint8_t pid, uint8_t status, qhead_wrapper_t * qhw)
{
    //XXX: This function links the alternate next 
    //pointers. Not sure about implication when linked 
    // via next pointers. 
    void *pa_buff = pa;
    //calculate how many will be needed 
    int num = sz / (BASE_PAGE_SIZE * N_QUEUE_BUFF);
    int spill = sz % (BASE_PAGE_SIZE * N_QUEUE_BUFF);
    if (spill != 0)
        num++;

    dprintf("Going to allocate [%d] QE with spill [%d] for [%u] bytes\n",
            num, spill, sz);
    usb_mem *mem_arr = malloc_qe_n(num);
    qTD *node = NULL;

    // perform linkage of structure
    dprintf("Performing linkage\n");

    for (int i = 0; i < num; i++) {
        node = (qTD *) mem_arr[i].va;
        node->alt.str.ptr = 0x0;
        node->alt.str.t = 0x1;  // terminate

        if (i < num - 1)
            node->next_qTD.str.ptr =
                HIGHER_ADD((uintptr_t) mem_arr[i + 1].pa, 5);

        node->next_qTD.str.t = 0;       // Valid next 

        node->q_token.str.pid = pid;
        node->q_token.str.status = status;
        // For a hi speed devices this is automatically set to 3. 
        // No other values are accepted   
        node->q_token.str.cerr = 3;

        // No toggle input 
        node->q_token.str.dt = 0;
        // first page is the request page
        node->q_token.str.c_page = 0;
        for (int j = 0; j < N_QUEUE_BUFF; j++) {
            node->buff_list[j].str.buff_ptr =
                HIGHER_ADD(((uintptr_t) pa_buff), 12LU);
            node->buff_list[j].str.offset = GET_OFFSET((uint64_t) pa_buff);
            pa_buff += BASE_PAGE_SIZE;
        }
        //no interrupt on internal elements.
        node->q_token.str.ioc = 0;
        node->q_token.str.total_bytes = BASE_PAGE_SIZE * N_QUEUE_BUFF;
        //print_qTD(node);
        //dprintf("\n EHCI: Iteration [%d] done", i);
    }

    // Now take care of last element 
    // it also might not get full 
    // i will be num-1

    node = (qTD *) mem_arr[num - 1].va;

    node->next_qTD.str.ptr = 0x0;
    node->next_qTD.str.t = 0x1; // terminate 

    //YES interrupt on last the element.
    node->q_token.str.ioc = 1;
    if (spill != 0)
        node->q_token.str.total_bytes = spill;


    //Now wrap it in q_elements wrappers 
    qhw->n = num;
    dprintf("%s: Calling wrapper funtion\n", __func__);
    wrap_q_elements(mem_arr, num, qhw);
    print_qTDs_dump(qhw);

}

/*
 * enqueue_usb_cmd_req: Primary message pipe for executing the 
 *                      control related requests on default ep. 
 *  XXX: This is an internal "old" function. New interface should be used. 
 *
 *  Args: (usb_device_request)    usb_request         
 *        (void *)                I/O buff location 
 *        (uint64_t)              expected Bytes 
 *        (int)                   is there is DATA stage
 *        (uint8_t)               device address 
 *        (uint8_t)               end point number (always =0)
 *        (int)                   debugging flag
 */

static int enqueue_usb_cmd_req(usb_device_request usb_req, void *p_buff,
                               uint64_t sz, int flag, uint8_t device,
                               uint8_t ep_num, int debug)
{
    assert(ep_num == 0);
    int ret_val;                //, req_pros_status;
    usb_device_request *udr_ptr = NULL;
    //struct thread_sem sem_init = THREAD_SEM_INITIALIZER;
    usb_shared_state_t *dev_arr = get_dev_arr_add();

    if (dev_arr[device].status != CONN_RH)      // Device is not connected 
    {
        dprintf("######## DEVICE DETACHEMDDDD ######## add[%u]\n", device);
        return DEV_REMOVED;
    }
    // Allocate location for queue head and elements 
    usb_mem req_data_mem;       //, resp_data_mem; 
    usb_mem req_qe, data_qe, status_qe;
    usb_mem qhead_mem;

    req_qe = malloc_qe();
    data_qe = malloc_qe();
    status_qe = malloc_qe();

    req_data_mem = malloc_qe();

    qhead_mem = malloc_qh();

    // Allocate wrappers 

    qhead_wrapper_t *qh_wrp =
        (qhead_wrapper_t *) malloc(sizeof (struct qhead_wrapper_t));
    qTD_wrapper_t *qTD_setup_wrp =
        (qTD_wrapper_t *) malloc(sizeof (struct qTD_wrapper_t));
    qTD_wrapper_t *qTD_data_wrp =
        (qTD_wrapper_t *) malloc(sizeof (struct qTD_wrapper_t));
    qTD_wrapper_t *qTD_status_wrp =
        (qTD_wrapper_t *) malloc(sizeof (struct qTD_wrapper_t));

    //semaphore assignment 
    //qh_wrp->wait_flag = 
    //(struct thread_sem *)malloc (sizeof(struct thread_sem)); 
    //*(qh_wrp->wait_flag) = sem_init;

    // Do assignments

    struct qTD *setup, *data, *status;
    setup = (struct qTD *)req_qe.va;
    data = (struct qTD *)data_qe.va;
    status = (struct qTD *)status_qe.va;

    struct q_head *v_qhead_ptr = (struct q_head *)qhead_mem.va;

    // Physcial linkage of the queue elements 

    if (flag == 1)              // Expected data stage 
    {
        setup->next_qTD.str.ptr = HIGHER_ADD(((uintptr_t) data_qe.pa), 5);
        setup->next_qTD.str.t = 0;      // Valid pointer
        setup->alt.str.t = 1;   // Mark this as invalid so that on queue advancemnet, HC will select next ptr

        // DATA
        data->next_qTD.str.ptr = HIGHER_ADD(((uintptr_t) status_qe.pa), 5);
        data->next_qTD.str.t = 0;
        data->alt.str.t = 1;    // Marked as invalid 

        // STATUS
        status->next_qTD.str.ptr = 0x0;
        status->next_qTD.str.t = 1;     // Termination, Not a valid pointer  
        status->alt.str.t = 1;  // Termination 
    } else {                    // No data stage
        setup->next_qTD.str.ptr = HIGHER_ADD(((uintptr_t) status_qe.pa), 5);
        setup->next_qTD.str.t = 0;      // Valid pointer
        setup->alt.str.t = 1;
        // Mark this as invalid so that on 
        // queue advancemnet, HC will select next ptr

        status->next_qTD.str.ptr = 0x0;
        status->next_qTD.str.t = 1;     // Termination, Not a valid pointer  
        status->alt.str.t = 1;  // Termination 
    }


    /* Process
     * Step 1. Setting up the SETUP packet 
     */

    // Mark this as active 
    setup->q_token.str.status = USB_TRAN_STATUS_ACTIVE;
    // SETUP token type
    setup->q_token.str.pid = USB_SETUP_TOKEN;
    // Number of retries;
    setup->q_token.str.cerr = 3;
    // first page is the request page;
    setup->q_token.str.c_page = 0;
    // SETUP is always DATA0
    setup->q_token.str.dt = 0;

    // copy the request to the res_data_mem 
    udr_ptr = (usb_device_request *) req_data_mem.va;
    *(udr_ptr) = usb_req;

    // Setup the page for a given physcial address PA 
    // we have ...page = PA >> 12;
    // offset = PA 

    // This will be page address >> 12
    setup->buff_list[0].str.buff_ptr =
        HIGHER_ADD(((uintptr_t) req_data_mem.pa), 12LU);
    setup->buff_list[0].str.offset = GET_OFFSET((uint64_t) req_data_mem.pa);

    setup->q_token.str.total_bytes = sizeof (usb_device_request);

    if (flag == 1) {
        /* Process 
         * Step 2. Setup the data stage 
         */

        data->q_token.str.status = USB_TRAN_STATUS_ACTIVE;
        data->q_token.str.pid = get_setup_data_direction(usb_req);
        data->q_token.str.cerr = 3;
        // first page is the request page;
        data->q_token.str.c_page = 0;

        //data->buff_list[0].str.buff_ptr = 
        //HIGHER_ADD(((uintptr_t) resp_data_mem.pa), 12LU);
        //data->buff_list[0].str.offset = 
        //GET_OFFSET((uint64_t) resp_data_mem.pa);

        data->buff_list[0].str.buff_ptr =
            HIGHER_ADD(((uintptr_t) p_buff), 12LU);
        data->buff_list[0].str.offset = GET_OFFSET((uint64_t) p_buff);

        data->q_token.str.ioc = 0;
        // Put ioc on status packet 
        data->q_token.str.total_bytes = get_expected_data_size(usb_req);

        //FIXME: There might be more than one data stage so this 
        //should be properly init (rare case ..in case of strings) 
        data->q_token.str.dt = 1;
    }


    /* Process
     * Step 3. Setup the status packet
     */

    status->q_token.str.status = USB_TRAN_STATUS_ACTIVE;
    if (flag == 1)
        status->q_token.str.pid =
            data->q_token.str.pid == USB_IN_TOKEN ?
            USB_OUT_TOKEN : USB_IN_TOKEN;
    else
        status->q_token.str.pid = USB_IN_TOKEN; // If there is no data stage 

    status->q_token.str.cerr = 3;
    status->q_token.str.c_page = 0;     // first page is the request page;

    status->buff_list[0].str.buff_ptr = 0x0;
    status->buff_list[0].str.offset = 0x0;

    status->q_token.str.ioc = 1;
    // Put ioc on status packet 

    status->q_token.str.total_bytes = 0;
    status->q_token.str.dt = 1; // STATUS is always DATA1 

    /*
     * Process.
     * Step 4. Setting up the queue head 
     */

    v_qhead_ptr->ep_char.str.dev_addr = device; // Default device
    v_qhead_ptr->ep_char.str.i = 0;     // 
    v_qhead_ptr->ep_char.str.endpt_num = ep_num;        // Default ep, control pipe
    v_qhead_ptr->ep_char.str.eps = EP_HI_SPEED; // Hi speed device
    v_qhead_ptr->ep_char.str.dtc = 1;   // USE the toggle from the respective qTDs

    // Set in the queuing logic 
    v_qhead_ptr->ep_char.str.head = 0;

    // FIXME: control ep is in device descriptor use it (But linux has 64)
    // Doc vs implementation issue !! 
    v_qhead_ptr->ep_char.str.max_packet_length = 64;

    v_qhead_ptr->ep_char.str.ctrl_endpt_flag = 0;       // as told on page 47

    //XXX: Now using 4 becasue Linux uses 4. 
    //No peculiar reason for using 4  
    v_qhead_ptr->ep_char.str.nack_reload = 4;

    // Dword 3 
    v_qhead_ptr->ep_cap.str.s_mask = 0;
    v_qhead_ptr->ep_cap.str.c_mask = 0;
    v_qhead_ptr->ep_cap.str.hub_addr = 0;
    v_qhead_ptr->ep_cap.str.port_num = 0;
    v_qhead_ptr->ep_cap.str.mult = 0x01;        // 1 transaction per u-frame

    // TRANSFER OVERLAY STARTS HERE > .. 
    // as said on page #80 of EHCI, zero out whole area and set a 
    // valid next_qTD;

    v_qhead_ptr->current.raw = 0x0;
    v_qhead_ptr->next.raw = 0x0;
    v_qhead_ptr->alt.raw = 0x0;
    v_qhead_ptr->q_token.raw = 0x0;
    v_qhead_ptr->buff_list[0].raw = 0x0;
    v_qhead_ptr->buff_list[1].raw = 0x0;
    v_qhead_ptr->buff_list[2].raw = 0x0;
    v_qhead_ptr->buff_list[3].raw = 0x0;
    v_qhead_ptr->buff_list[4].raw = 0x0;

    v_qhead_ptr->next.str.ptr = HIGHER_ADD(((uintptr_t) req_qe.pa), 5);
    v_qhead_ptr->next.str.t = 0;        // Valid 

    if (debug == 1) {
        printf("%s: %s: Before enqueuing the data", __FILE__, __func__);
        printf("...here are the status\n");
        print_usb_mem(req_qe);
        print_usb_mem(data_qe);
        print_usb_mem(status_qe);
        print_usb_mem(req_data_mem);
        print_usb_mem(qhead_mem);
        printf("Dumping of usb_mem nodes finished\n");

        print_qhead(v_qhead_ptr);
        print_qTD(setup);
        print_qTD(data);
        print_qTD(status);
    }
    // All normal allocation is done to this point. 
    // Now it is time to allocate wrappers to track 
    // and manage request. 

    // Step 1. Assign SETUP

    qTD_setup_wrp->q_elem = setup;
    qTD_setup_wrp->prev = NULL;
    if (flag == 1)              // there is a data stage 
        qTD_setup_wrp->next = qTD_data_wrp;
    else
        qTD_setup_wrp->next = qTD_status_wrp;

    qTD_setup_wrp->qhw = qh_wrp;
    qTD_setup_wrp->mem = req_qe;

    // Step 2. Assign DATA 
    if (flag == 1) {
        qTD_data_wrp->q_elem = data;
        qTD_data_wrp->prev = qTD_setup_wrp;
        qTD_data_wrp->next = qTD_status_wrp;
        qTD_data_wrp->qhw = qh_wrp;
        qTD_data_wrp->mem = data_qe;
        qh_wrp->req_type = REQ_TYPE_DCTRL;
    } else
        qh_wrp->req_type = REQ_TYPE_CTRL;

    // Step 3. Assign Status 
    qTD_status_wrp->q_elem = status;
    qTD_status_wrp->next = NULL;
    if (flag == 1)              // there is a data stage 
        qTD_status_wrp->prev = qTD_data_wrp;
    else
        qTD_status_wrp->prev = qTD_setup_wrp;

    qTD_status_wrp->qhw = qh_wrp;
    qTD_status_wrp->mem = status_qe;

    // Queue element wrapping finished, move to the 
    // queue head 
    qh_wrp->qh = v_qhead_ptr;
    // Both self referencing, new value will be assigned in queuing logic 
    qh_wrp->next = qh_wrp->prev = qh_wrp;
    qh_wrp->qe_last = qTD_status_wrp;
    qh_wrp->qe_next = qTD_setup_wrp;
    qh_wrp->mem = qhead_mem;

    qh_wrp->status_flag = FLAG_ACTIVE;
    qh_wrp->type = NODE_TYPE_NONE;
    qh_wrp->rsp = get_context();
    qh_wrp->len = sz;
    dprintf("Putting request to the internal queuing logic ...\n");
    qh_wrp->dbug = 0xDEB0DEB1;
    qh_wrp->packet_sz = v_qhead_ptr->ep_char.str.max_packet_length;

    internal_queue_req(qh_wrp);

    // Old code which was used before new message notification 
    // mechanism was implemented. Client (1 domain was waiting on 
    // semaphore.

#if 0

    thread_sem_wait(qh_wrp->wait_flag);

    // Remove busy loop 
    //while(qh_wrp->wait_flag != RELEASE);

    dprintf
        ("\n EHCI_CORE: SETUP status %x IN status %x STATUS status %x \n\n\n",
         setup->q_token.str.status, data->q_token.str.status,
         status->q_token.str.status);

    if (debug == 1) {
        print_qhead(v_qhead_ptr);
        print_qTD(setup);
        print_qTD(data);
        print_qTD(status);
    }

    req_pros_status =
        get_return_status(setup->q_token.str.status, data->q_token.str.status,
                          status->q_token.str.status);

    if (req_pros_status == 0)   // All went OK.
    {

        if (flag == 1)          // There was DATA stage and DATA is expected back
        {
            //data_read = *((uint8_t *) resp_data_mem.va);
            //sz = data_bytes(usb_req, data_read, sz);
            //dprintf("\n EHCI_CORE: Number of bytes read are %lu", sz);
            //copy_data(resp_data_mem.va, buff, sz);
            //ret_val = sz;
            dprintf("\n EHCI: Data stage");
            ret_val = sz;
        } else {
            ret_val = 0;        // No DATA stage, but command was completed successfully 
        }

        ret_val = 0;            // Data interpretation is done by USB 
    } else {
        ret_val = req_pros_status;      // Falied, request   
    }

    // Free all consumed resources 
    free_qe(req_qe);
    free_qe(data_qe);
    free_qe(status_qe);
    free_qe(req_data_mem);

    //free_qh(resp_data_mem);
    free_qh(qhead_mem);

    free(qh_wrp);
    free(qTD_setup_wrp);
    free(qTD_data_wrp);
    free(qTD_status_wrp);
#endif

    ret_val = 0;
    return ret_val;

}


/*
 * \brief This is primary function used to handle all I/O data tranfer 
 *        on bulk pipe. 
 *
 * \param pipe Bulk pipe on which transaction has to be done
 * \param io_buff I/O buffer location 
 * \param len Length of the transfer
 * \param debug Selective debug for a request
 */


int usb_bulk_exe(usb_pipe_t pipe, void *io_buff, uint32_t len, int debug)
{
    // Allocate location for queue head and queue elements 
    // Allocate required queue data on ehci memory 

    dprintf("In starting of bulk-- [%llu]\n", (long long unsigned int)rdtsc());
    printf("%llu\n", (long long unsigned int)rdtsc());

    usb_mem qhead_mem;
    usb_shared_state_t *dev_arr = get_dev_arr_add();

    if (dev_arr[pipe.dev].status != CONN_RH)    // Device is not connected 
        return DEV_REMOVED;

    int i;
    qhead_mem = malloc_qh();

    // Allocate wrappers 
    qhead_wrapper_t *qh_wrp =
        (qhead_wrapper_t *) malloc(sizeof (struct qhead_wrapper_t));

    // Do assignments
    struct q_head *v_qhead_ptr = (struct q_head *)qhead_mem.va;


    if (debug == 1)
        printf("%s: %s: The physical address of the I/O buffer %lx",
               __FILE__, __func__, (uintptr_t) io_buff);


    uint8_t pid = (pipe.ep_dir == EP_TYPE_IN) ? USB_IN_TOKEN : USB_OUT_TOKEN;
    uint8_t status =
        (pipe.ep_dir == EP_TYPE_IN) ?
        USB_TRAN_STATUS_ACTIVE : (USB_TRAN_STATUS_ACTIVE |
                                  USB_TRAN_STATUS_PERR);

    malloc_qTDs(len, io_buff, pid, status, qh_wrp);
    print_qTDs_dump(qh_wrp);

    uint32_t toggle_qhead = USB_GET_TOGGLE(pipe.dev, pipe.ep_number, pipe.ep_dir);      // USE data toggle in bulk in-out transfers   
    // check how many toggles are required 

    int toggle_times = len / pipe.ep_psz;
    int toggle_mod = len % pipe.ep_psz;

    if (toggle_mod != 0)
        toggle_times++;
    qh_wrp->dev = pipe.dev;
    qh_wrp->ep_num = pipe.ep_number;
    qh_wrp->dir = pipe.ep_dir;
    if (toggle_times % 2 == 1) {
        // Do data toggle    
        USB_DO_TOGGLE(pipe.dev, pipe.ep_number, pipe.ep_dir);
        qh_wrp->toggle = true;
    } else
        qh_wrp->toggle = false;


    //else ignore the toggle becuase end output will be the same 

    dprintf("%s: Current toggle %u, next toggle %u, toggle_times %d, len %u\n",
            __func__, toggle_qhead,
            USB_GET_TOGGLE(pipe.dev, pipe.ep_number, pipe.ep_dir),
            toggle_times, len);

    if (debug == 1) {
        //print_usb_mem(data_qe);
        //print_qTD(data);
        dprintf("\n %s", __func__);
    }


    /*
     * Process.
     * Step 2. Setting up the queue head 
     */

    // Dword 1, this bit is ignored, this connection is setup during the 
    // enqueung phase of async request   
    v_qhead_ptr->next_qhead.str.t = 1;
    v_qhead_ptr->next_qhead.str.ptr = 0x0;

    // Dword 2
    v_qhead_ptr->ep_char.str.dev_addr = pipe.dev & 0x007F;      // 7 bit address
    v_qhead_ptr->ep_char.str.i = 0;     // 
    v_qhead_ptr->ep_char.str.endpt_num = pipe.ep_number;
    // Hi speed device, hi speed ep
    v_qhead_ptr->ep_char.str.eps = EP_HI_SPEED;

    // Allcate all qTD properly, beacuae this will set from incoming qTD  
    // 0 = preserve dt in queue head 
    // 1 = take input from queue element 
    v_qhead_ptr->ep_char.str.dtc = 0;

    // XXX: This is fixed as 1 in enqueue_logic 
    v_qhead_ptr->ep_char.str.head = 0;

    v_qhead_ptr->ep_char.str.max_packet_length = pipe.ep_psz;

    v_qhead_ptr->ep_char.str.ctrl_endpt_flag = 0;       // as told on page 47
    v_qhead_ptr->ep_char.str.nack_reload = 4;

    // Dword 3 
    v_qhead_ptr->ep_cap.str.s_mask = 0;
    v_qhead_ptr->ep_cap.str.c_mask = 0;
    v_qhead_ptr->ep_cap.str.hub_addr = 0;
    v_qhead_ptr->ep_cap.str.port_num = 0;
    v_qhead_ptr->ep_cap.str.mult = pipe.multi;

    // TRANSFER OVERLAY STARTS HERE > .. 
    // as said on page #80, zerout whole area and set a valid next_qTD;

    // To mark this as invalid pointer 
    v_qhead_ptr->current.raw = 0x1;
    v_qhead_ptr->next.raw = 0x1;
    v_qhead_ptr->alt.raw = 0x1;

    // GOOD. Preserve the toggle here in the queue head and init it also. 
    // In coming qTD's dt will be ignored   
    if (toggle_qhead == 1)
        v_qhead_ptr->q_token.raw = 1 << 31;
    else
        v_qhead_ptr->q_token.raw = 0x0;

    dprintf("head_token %u\n", v_qhead_ptr->q_token.raw);
    v_qhead_ptr->buff_list[0].raw = 0x0;
    v_qhead_ptr->buff_list[1].raw = 0x0;
    v_qhead_ptr->buff_list[2].raw = 0x0;
    v_qhead_ptr->buff_list[3].raw = 0x0;
    v_qhead_ptr->buff_list[4].raw = 0x0;

    v_qhead_ptr->next.str.ptr =
        HIGHER_ADD(((uintptr_t) qh_wrp->qe_next->mem.pa), 5);
    v_qhead_ptr->next.str.t = 0;        // Valid 

    // Part 2.2 queue head
    qh_wrp->qh = v_qhead_ptr;
    qh_wrp->mem = qhead_mem;
    qh_wrp->next = qh_wrp->prev = qh_wrp;
    qh_wrp->status_flag = FLAG_ACTIVE;
    qh_wrp->type = NODE_TYPE_NONE;
    qh_wrp->req_type = REQ_TYPE_BULK;
    qh_wrp->rsp = get_context();
    qh_wrp->len = len;
    qh_wrp->packet_sz = v_qhead_ptr->ep_char.str.max_packet_length;

    if (debug == 1) {
        printf("%s: %s: Before enqueuing the head and data ...\n",
               __FILE__, __func__);

        print_qhead(v_qhead_ptr);
        printf("Call to internal queuee....\n");
    }

    qh_wrp->dbug = get_dbug_flag();
    dprintf("Wait begins .....for[%x] add[%p]\n", qh_wrp->dbug, qh_wrp);

    i = 0;

    //Put request into the internal hardware quque 
    internal_queue_req(qh_wrp);

    //Code used when we used semaphore ..used in 1-domain system 
#if 0

    // Wait 
    //thread_sem_wait(qh_wrp->wait_flag, 1);


    int try = 0;
    while (qh_wrp->wait_flag != RELEASE) {
        if (debug == 1 && try % 199 == 0) {
            dprintf("\n\n EHCI_CORE_DATA_MOVE: data status %x",
                    data->q_token.str.status);
            //print_qhead(v_qhead_ptr);
            //print_qTD(data);
        }

        if (debug == 1 && try == 1000) {
            print_qhead(v_qhead_ptr);
            print_qTD(data);
            break;
        } else {
            try++;
        }


    }
    */dprintf("\n EHCI_CORE_DATA_MOVE: Wait ends ...");

    dprintf("\n\n EHCI_CORE_DATA_MOVE: usb_move_data data status %x \n\n\n",
            data->q_token.str.status);
    if (debug == 1) {
        print_qhead(v_qhead_ptr);
        print_qTD(data);

    }

    if (switch_error(data->q_token.str.status) == REQ_FINISHED)
        len = len;
    else {
        if (switch_error(data->q_token.str.status) == REQ_PERR && data->q_token.str.total_bytes == 0)   // Though were are in PING stage but no OUT data
            len = len;
        else
            len = switch_error(data->q_token.str.status);
    }


    free_qe(data_qe);
    free_qh(qhead_mem);

    free(qh_wrp);
    free(data_wrp);
#endif


    return len;
}



/*
 * Clean wrapper utilities
 * Assumes that control transactions are done on default (zero) endpoint  
 */

/*
 * \brief Calls enqueue_usb_cmd_req internally with all valid arguments. 
 *        It is used for control transactions which have data stage. 
 */

int usb_dctrl_exe(usb_device_request usb_req, void *buff, uint64_t sz,
                  uint8_t device, int debug)
{
    return enqueue_usb_cmd_req(usb_req, buff, sz, 1, device, 0, debug);
}

/*
 * \brief Calls enqueue_usb_cmd_req internally with all valid arguments. 
 *        It is used for control transactions which DON't have data stage. 
 */
int usb_ctrl_exe(usb_device_request usb_req, uint8_t device, int debug)
{
    return enqueue_usb_cmd_req(usb_req, NULL, 0, 0, device, 0, debug);
}
