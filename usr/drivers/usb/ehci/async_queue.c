/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>

#include <timer/timer.h>
#include <usb/utility.h>

#include <dev/ehci_dev.h>

#include <usb/mem/usb_mem.h>
#include "ehci.h"
#include "ehci_status.h"
#include "async_queue.h"

#define TESTING 0

//XXX: Enabale this macro for debugging local file specific debugging
//#define EHCI_LOCAL_DEBUG
#include "ehci_debug.h"

#include "toggle_state.h"


static struct thread_sem do_remove = THREAD_SEM_INITIALIZER;
static struct thread_sem do_scan = THREAD_SEM_INITIALIZER;

static struct thread_mutex list_update;
static struct thread_mutex safe_register_access;
static struct thread *mark_thread = NULL;
static struct thread *remove_thread = NULL;

static volatile int free_all;

static qhead_wrapper_t *start_async_node = NULL;

/*
 * \brief Upon receiving an queue head wrapper this function 
 *        cleans up the whole queue element queue and calculates 
 *        how much data transaction has been done.
 */

static void clean_and_post(qhead_wrapper_t * qhw)
{
    dprintf("%s\n", __func__);

    qTD_wrapper_t *qTDw = NULL;
    qTD_wrapper_t *temp = NULL;
    uint32_t total_bytes = 0;
    uint8_t error_code = 0;
    uint32_t succ = 0, failed = 0;
    int count = 0;
    uint8_t last_status = -1;

    assert(qhw != NULL);

    // scan for errors
    qTDw = qhw->qe_next;

    //print_qhead(qhw->qh);
    //dprintf("clean_and_post 1 %u", total_bytes);

    while (qTDw != NULL) {
        count++;
        //XXX: For control requests this will be status
        last_status = qTDw->q_elem->q_token.str.status;

        if (qTDw->q_elem->q_token.str.status ==
            USB_TRAN_STATUS_FINISHED
            || qTDw->q_elem->q_token.str.status == USB_TRAN_STATUS_PERR) {
            //print_qTD(qTDw->q_elem);
            total_bytes += qTDw->q_elem->q_token.str.total_bytes;

            // XXX: Generated big output dump, enabale it selectively 
            /*
               dprintf("status - > %x bytes %u ioc %x \n", 
               qTDw->q_elem->q_token.str.status, 
               total_bytes,
               qTDw->q_elem->q_token.str.ioc);
             */
            succ++;             // maintained for partial transaction


        } else {
            failed++;
            //print_qTD(qTDw->q_elem);
            dprintf("current count %d and total_bytes %u\n",
                    count, total_bytes);
            error_code = qTDw->q_elem->q_token.str.status;
            total_bytes += qTDw->q_elem->q_token.str.total_bytes;
            break;
            // Assuming that no further transactions are executed 
        }

        qTDw = qTDw->next;
    }

    //dprintf("\n clean_and_post 3");
    qTDw = qhw->qe_next;
    while (qTDw != NULL) {
        //dprintf("\n list free step 1");
        free_qe(qTDw->mem);
        //dprintf("\n list free step 2");
        temp = qTDw;
        //dprintf("\n list free step 3");
        qTDw = qTDw->next;
        //dprintf("\n list free step 4");
        free(temp);
        //dprintf("\n list free step 5");
    }

    //dprintf("\n clean_and_post 4");

    // Now we have head left
    struct ehci_service_response *rsp = qhw->rsp;
    uint8_t type = qhw->req_type;

    //XXX: If total bytes was zero then all elements were executed 
    //sucessfully so thats good.
    if (total_bytes == 0) {
        total_bytes = qhw->len;
    } else {
        //XXX: We have partial failure or termination
        int times, mod;
        total_bytes = (succ + 1) * (N_QUEUE_BUFF * BASE_PAGE_SIZE)
            - total_bytes;
        times = total_bytes / qhw->packet_sz;
        mod = total_bytes % qhw->packet_sz;
        if (mod != 0)
            times++;
        dprintf("times = %d bool %d \n", times, qhw->toggle);
        usb_shared_state_t *dev_arr = get_dev_arr_add();
        if ((times % 2 == 0 && qhw->toggle == true) ||
            (times % 2 == 1 && qhw->toggle == false))
            USB_DO_TOGGLE(qhw->dev, qhw->ep_num, qhw->dir);
        // Do data toggle    

    }

    //dprintf("\n clean_and_post 4.1");
    free_qh(qhw->mem);
    //dprintf("\n clean_and_post 4.2 %x add[%p]", qhw->dbug, qhw);
    //free(qhw);
    //dprintf("\n clean_and_post 4.3");
    assert(rsp != NULL);

    switch (type) {
    case REQ_TYPE_DCTRL:
        if (last_status != 0)
            total_bytes = -last_status;
        dprintf("DCTRL done [%u]\n", total_bytes);
        rsp->f->dctrl_done(rsp, total_bytes);
        break;
    case REQ_TYPE_CTRL:
        if (last_status != 0)
            total_bytes = -last_status;
        dprintf("CTRL done [%u]\n", total_bytes);
        rsp->f->ctrl_done(rsp, total_bytes);
        break;
    case REQ_TYPE_BULK:
        dprintf("Bulk done [%u]\n", total_bytes);
        dprintf("Bulk done IPC -- [%llu]\n", (long long unsigned int)rdtsc());
        printf("%llu\n", (long long unsigned int)rdtsc());
        rsp->f->bulk_done(rsp, total_bytes);
        break;
    default:
        USER_PANIC("\n Uknown tranfer type in queue head\n");
    }
}

/*
 * \brief This function enables or disbales the asynchronous schedule 
 *
 * \param dev   EHCI operational device 
 * \param from  debugging char string from caller function
 */

void enable_async_schedule(ehci_t dev, char *from)
{
    thread_mutex_lock(&safe_register_access);
    volatile uint8_t new_val, status, timeout = 100;
    int i;
    status = ehci_usb_status_rd(&dev).as_s;
    new_val = ehci_usb_cmd_rd(&dev).as_e;

    dprintf("(ENABLING %s) async schedule status before"
            "ENABLING status [%d] cmd [%d]\n", from, status, new_val);

    if (status != 0x1)          // Schedule is not already enabaled 
    {
        new_val = 0x1;
        // Enable the schedule 
        ehci_usb_cmd_as_e_wrf(&dev, new_val);
        dprintf("(ENABLING %s) Wrote %d to"
                "USB_COMMAND register\n", from, new_val);
        // Repeated attemps untill we see status change 
        while (timeout > 0) {
            i = 0;

            status = ehci_usb_status_rd(&dev).as_s;
            new_val = ehci_usb_cmd_rd(&dev).as_e;

            dprintf("(ENABLING %s)"
                    "periodic status check"
                    "status [%d] cmd [%d] \n", from, status, new_val);
            timeout--;

            // Command to enable list is 
            // executed and list is enabled
            if (status == 0x1)
                break;
            else
                continue;
        }
    }

    dprintf("\n");

    //XXX: Somehow we failed to enable schedule 
    // Hardware issues ? This should never happen
    if (timeout == 0 && status != 0x1)
        USER_PANIC("\n\nFailed to enable async schedule in time\n\n");

    thread_mutex_unlock(&safe_register_access);
}


/*
 *  \brief Equivalent function to disable asynchronous schedule.
 *
 *  \param dev  EHCI operation device 
 *  \param from debugging string from caller function
 */

void disable_async_schedule(ehci_t dev, char *from)
{
    thread_mutex_lock(&safe_register_access);
    volatile uint8_t new_val, status, timeout = 100;
    status = ehci_usb_status_rd(&dev).as_s;
    new_val = ehci_usb_cmd_rd(&dev).as_e;
    int i;

    dprintf("(DISABLING %s)"
            "async schedule status"
            "before DISABLING status [%d] cmd (?) [%d] \n",
            from, status, new_val);

    if (status != 0x0)          // Schedule is not already disbaled 
    {
        new_val = 0x0;
        // Disable the schedule 
        ehci_usb_cmd_as_e_wrf(&dev, new_val);
        dprintf("(DISABLING %s)"
                "Wrote %d to USB_COMMAND"
                "register to disbale the queue \n", from, new_val);
        while (timeout > 0) {
            i = 0;
            status = ehci_usb_status_rd(&dev).as_s;
            new_val = ehci_usb_cmd_rd(&dev).as_e;
            dprintf("EHCI: (DISABLING %s)"
                    "periodic status check"
                    "status [%d] cmd [%d]\n", from, status, new_val);
            timeout--;

            // Command to disable list is 
            // executed and list is disabled 
            if (status == 0x0)
                break;
            else
                continue;
        }
    }
    //XXX: hardware issues ? Mackerel bug ?
    if (timeout == 0 && status != 0x0)
        assert(!"Failed to disable async schedule in time\n");

    thread_mutex_unlock(&safe_register_access);
}

/* 
 * \brief function to set async advancement bell on EHCI
 *
 * \param dev EHCI operation device 
 */
static void set_doorbell_flag(ehci_t dev)
{
    ehci_usb_cmd_iaad_wrf(&dev, 0x1);        // Set the door bell
}


/*
 * \brief This function is called after we receive async advancement 
 *        bell. It indicates that controller does not have any more 
 *        cached entries of queue elements and we can free them. This 
 *        is precisely what it does. It removes all marked (aka completed)
 *        transaction from the queue. 
 *        This code executes in a different thread 
 */

static int scan_and_release_qheads(void *args)
{
    // This will never be called to remove a
    // queue head marked H =1, only if all 
    // have to be released 

    qhead_wrapper_t *temp, *new_start = NULL;
    int count, assert_test;
    int total, freed;
    while (1) {
        thread_sem_wait(&do_remove);
        {
            //thread_mutex_lock(&list_update);
            dprintf(" do_remove...");
            temp = start_async_node;
            new_start = NULL;
            count = 0;
            // XXX: WHY did we receive an interrupt 
            // on a NULL list ?
            if (temp == NULL) {
                disable_async_schedule(ehci_device, "NULL_INT_NEVER");
                USER_PANIC("\n\n scan_and_release called on NULL head\n\n");
                break;
            }                   //temp == NULL

            if (free_all == 0x1) {
                total = 0;
                freed = 0;
                while (temp->next != temp) {
                    temp->prev->next = temp->next;
                    temp->next->prev = temp->prev;
                    temp->status_flag = FLAG_REMOVED;
                    clean_and_post(temp);
                    total++;

                }
                // Now only single node is left 

                total++;
                dprintf("EHCI_RELEASE:"
                        "Total of %d nodes are"
                        "released dbug id %x \n", total, temp->dbug);
                disable_async_schedule(ehci_device, "RELEASE_FREE_ALL");
                start_async_node = NULL;
                free_all = 0x0;
                clean_and_post(temp);
            } else {
                total = 0;
                freed = 0;
                while (count != 2)
                    // 2 time, becuase head is shifted 
                {
                    total++;
                    if (temp->status_flag == FLAG_MARK_REMOVE) {
                        // Node unlinked 
                        freed++;
                        temp->prev->next = temp->next;
                        temp->next->prev = temp->prev;
                        temp->status_flag = FLAG_REMOVED;

                        // This will set free the 
                        // locked thread
                        assert_test = 0;
                        clean_and_post(temp);


                    } else
                        assert_test = 1;

                    if (temp->type == NODE_TYPE_HEAD) {
                        assert(assert_test);
                        new_start = temp;
                        count++;
                    }

                    temp = temp->next;
                    // move to next item 
                }               // count != 2

                assert(new_start != NULL);
                start_async_node = new_start;
                dprintf("EHCI_RELEASE:"
                        "(not_all_free) %d"
                        "nodes are released"
                        "from total of %d...\n", freed, total);

            }                   // else free all 

            total = 0;
            thread_mutex_unlock(&list_update);

        }

    }

    thread_exit();
    return 0;
}

/*
 *  \brief This function physically removes the queue elements from HC 
 *         hardware queue but let them be in wrapper link list. Because 
 *         untill hardware flags that it is safe to free them we should not. 
 *         its OK to unlink them but nodes should not be freed.
 */

static void unlink_queue_head(volatile qhead_wrapper_t * now,
                              volatile qhead_wrapper_t * next)
{
    // This function will just remove the physical links 
    // of the HC queue but still keeps the node in 
    // the wrapper queue. 

    if (IS_HEAD(now->type)) {
        next->type = NODE_TYPE_HEAD;
        next->qh->ep_char.str.head = 1;

        now->type = NODE_TYPE_NONE;
        now->qh->ep_char.str.head = 0;
    }
    // Move the physical linking ...
    (now)->qh->next_qhead.raw = (next)->qh->next_qhead.raw;

    // Do not update the wrapper connection for node
    // That will be done in release at doorbell interrupt
}


/*
 *  \brief Check if a queue element is in a active state of transaction 
 */

static int is_active(uint8_t status)
{
    // This is MSB, will eval to 0 or 1 
    return ((status >> 7) & USB_TRAN_STATUS_ACTIVE);
}

/*
 *  \brief Check if a queue element is in a Ping ERR (PERR) state of 
 *         transaction.  
 */
static int is_perr(uint8_t status)
{
    // This is LSB, will eval to 0 or 1 
    return ((status & USB_TRAN_STATUS_PERR));
}


/*
 * \brief This is a thread entry point. It waits for interrupt on completion 
 *        (ioc) to start processing. Upon receving an interrupt notification 
 *        it scan the whole async list and marks all elements which have their 
 *        transaction done. It unlinks them from physical queue and set the
 *        door bell for async advancement. 
 */

static int scan_and_remove(void *args)
{
    volatile qhead_wrapper_t *temp = start_async_node;
    volatile qhead_wrapper_t *prev = NULL;
    volatile qhead_wrapper_t *now = NULL;
    volatile qhead_wrapper_t *next = NULL;

    uint32_t total, marked;
    int count = 0;
    while (1) {
        thread_sem_wait(&do_scan);
        {
            thread_mutex_lock(&list_update);
            dprintf("%s\n", __func__);
            temp = start_async_node;
            count = 0;          // Have not seen queue head yet

            // No elements in the queue
            // Then Why did we receive an interrupt ? 
            if (temp == NULL) {
                continue;
                assert(!"Interrupt received on NULL head?");
            }
            prev = start_async_node->prev;
            total = 0;
            marked = 0;
            dprintf("%s:No list is not empty\n", __func__);

            while (count != 1) {
                // Processing finished, 
                // either with success or failure 
                // we do not process same item twice 
                if (is_active(temp->qh->q_token.str.status) == 0
                    && temp->status_flag == FLAG_ACTIVE) {
                    if (is_perr(temp->qh->q_token.str.status) == 1);
                    /*{
                       temp->qh->q_token.str.status = temp->qh->q_token.str.status | USB_TRAN_STATUS_ACTIVE;
                       if(Q_DEBUG) 
                       //dprintf("\n HC in PING stage");
                       }else
                       { */

                    temp->status_flag = FLAG_MARK_REMOVE;       // Mark them to be removed 
                    marked++;
                    //}

                }

                temp = temp->next;
                total++;
                if (IS_HEAD(temp->type))
                    count++;
                // Will be 1 when we see head for 
                // the second time 
            }                   // List scan complete 

            // Now perform actual unlinking ... 

            dprintf("%s: total nodes = %d,  to_remove = %d\n",
                    __func__, total, marked);
            count = 0;
            temp = start_async_node;

            if (marked < total) {
                while (marked > 0) {
                    now = temp;
                    while (temp->status_flag == FLAG_MARK_REMOVE)
                        temp = temp->next;
                    now = temp;
                    while (temp->status_flag == FLAG_MARK_REMOVE)
                        temp = temp->next;
                    next = temp;

                    unlink_queue_head(now, next);

                    temp = next;
                    marked--;
                }
            } else {

                // ALL mark nodes were marked to be removed 
                dprintf("%s: Free all condition is reached\n", __func__);
                free_all = 0x1;
            }

            //thread_mutex_unlock(&list_update);

            dprintf("%s: setting up the doorbell...\n", __func__);
            set_doorbell_flag(ehci_device);
            dprintf("%s: should be now any minute ..\n", __func__);

        }                       // 1 Invocation complete, set it as false  

    }                           // The interrupt thread will set it to ture on receiving an IOC 

    thread_exit();
    return 0;

}

/*
 * \Brief Performs the null hardware list activation
 *
 * \param node node to be inserted into the deactivated list 
 */

static void null_list_activation(qhead_wrapper_t * node)
{
    struct ehci_asyn_list_reg_t reg;
    dprintf("%s: Node is found to be NULL ... inserting node\n", __func__);

    disable_async_schedule(ehci_device, "internal_queue_NULL");
    // Level 1. Internal book keeping
    // Mark this only entry as HEAD
    node->qh->ep_char.str.head = 1;

    // Recursive loop setting 
    node->qh->next_qhead.str.ptr = HIGHER_ADD(((uintptr_t) node->mem.pa), 5);
    // Level 2. Wrapper book-keeping doubly link list 
    // So we have an entry in queue 

    start_async_node = node;
    assert(start_async_node != NULL);
    node->next = node;
    node->prev = node;
    node->type = NODE_TYPE_HEAD;

    // Write register 
    // Physical address of a single queue head address 
    reg.lpl = HIGHER_ADD(((uintptr_t) (node->mem.pa)), 5);

    ehci_asyn_list_reg_wr(&ehci_device, reg);
    enable_async_schedule(ehci_device, "internal_queue_NULL");
    dprintf("%s: Node inserted and schedule is enabled ...\n", __func__);
    printf("%llu\n", (unsigned long long int)rdtsc());

}

static void insert_node(qhead_wrapper_t * node)
{
    // If async schedule is already enabled then 
    // NODE UPDATING .....

    // Level 2. For maintaining the wrapper link list 
    node->next = start_async_node->next;
    node->type = NODE_TYPE_NONE;
    node->prev = start_async_node;
    start_async_node->next->prev = node;
    start_async_node->next = node;

    // Level 1. Internal book-keeping 
    // Mark this node as internal queue head 

    node->qh->ep_char.str.head = 0;

    // physical linking of nodes ....
    // node and start_node is updated has been inserted here ...
    node->qh->next_qhead.str.ptr =
        HIGHER_ADD(((uintptr_t) start_async_node->next->next->mem.pa), 5);

    start_async_node->qh->next_qhead.str.ptr =
        HIGHER_ADD(((uintptr_t) node->mem.pa), 5);
}


void internal_queue_req(qhead_wrapper_t * node)
{
#if 0
    if (TESTING) {
        node->qh->ep_char.str.head = 1;
        // Recursive loop setting ..with one queue head only
        node->qh->next_qhead.str.ptr =
            HIGHER_ADD(((uintptr_t) node->mem.pa), 5);
        disable_async_schedule(ehci_device, "internal_queue_testing");
        reg.lpl = HIGHER_ADD(((uintptr_t) (node->mem.pa)), 5);
        // Physical address of a single queue head address 
        ehci_asyn_list_reg_wr(&ehci_device, reg);

        if (Q_DEBUG)
            dprintf("\n EHCI: async queue physical addresss\
                                                set to ASYNCLISTADDR register \
                                                %lx", (uint64_t) (ehci_asyn_list_reg_rd_raw(&ehci_device)));
        enable_async_schedule(ehci_device, "intern_queue_testing");
    } else {
    }
#endif

    thread_mutex_lock(&list_update);
    if (start_async_node == NULL)       // Async queue is not active 
    {
        null_list_activation(node);
    } else {
        insert_node(node);
    }
    thread_mutex_unlock(&list_update);

}


/*static void periodic_scan(struct timer *t, void *data)
{
        if(Q_DEBUG) 
                dprintf("\n Periodic scan for unfinished business at sync queue ...");
}*/

void notify_scan(void)
{
    //long long unsigned int end = rdtsc();
    printf("%llu\n", (unsigned long long int)rdtsc());
    //dprintf("RDTSC end -- %llu\n", end );
    dprintf("%s: Signaling the scan thread ..\n", __func__);
    thread_sem_post(&do_scan);
    dprintf("%s: Done \n", __func__);
}

void notify_remove(void)
{
    dprintf("%s: Signaling the remove thread ..\n", __func__);
    thread_sem_post(&do_remove);
    dprintf("%s: Done \n", __func__);
}

/*
 * Generic queue setup logic. The function performs following operations:
 * 1. Creates a periodic timer thread to remove dead requests. Dead requests
 *    are those which do not result in an interrupt. Often found in bulk 
 *    transaction when PING protocol is broken. 
 * 2. Creates two threads to mark and remove the completed requests. 
 * 3. Other misc mutex init. 
 */
void async_queue_init(void)
{
    start_async_node = NULL;
    free_all = 0x0;

    thread_mutex_init(&list_update);
    thread_mutex_init(&safe_register_access);

    /*timer_init();
       struct timer *timer = timer_create(1000, true, periodic_scan, 0);
       timer_start(timer); */

    mark_thread = thread_create(scan_and_remove, 0);
    assert(mark_thread != 0);
    remove_thread = thread_create(scan_and_release_qheads, 0);
    assert(remove_thread != 0);
    errval_t err = thread_detach(mark_thread);
    assert(err_is_ok(err));
    err = thread_detach(remove_thread);
    assert(err_is_ok(err));
}
