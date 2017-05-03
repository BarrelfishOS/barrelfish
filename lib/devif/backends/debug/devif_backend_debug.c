/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <devif/queue_interface.h>
#include "../../queue_interface_internal.h"
#include <devif/backends/debug.h>
#include "debug.h"

#define HIST_SIZE 128
#define MAX_STR_SIZE 128

/*
 * This is a debugging interface for the device queue interface that
 * can be used with any existing queue. It can be stacked on top 
 * to check for non valid buffer enqueues/deqeues that might happen.
 * A not valid enqueue of a buffer is when the endpoint that enqueues
 * the buffer does not own the buffer. 
 *
 * We keep track of the owned buffers as a list of regions which each
 * contains a list of memory chunks.
 * Each chunk specifies a offset within the region and its length.
 *
 * When a region is registered, we add one memory chunk that describes
 * the whole region i.e. offset=0 length= length of region
 *
 * If a a buffer is enqueued, it has to be contained in one of these
 * memory chunks. The memory chunk is then altered according how
 * the buffer is contained in the chunk. If it is at the beginning or
 * end of the chunk, the offset/length of the chunk is changed accordingly
 * If the buffer is in the middle of the chunk, we split the memory chunk
 * into two new memory chunks that do not contain the buffer.
 *
 * If a buffer is dequeued the buffer is added to the existing memory
 * chunks if possible, otherwise a new memory chunk is added to the
 * list of chunks. If a buffer is dequeued that is in between two
 * memory chunks, the memory chunks are merged to one big chunk. 
 *
 * When a region is deregistered, the list of chunks has to only 
 * contain a single chunk that descirbes the whole region. Otherwise
 * the call will fail since some of the buffers are still in use. 
 * 
 */

struct memory_ele {
    genoffset_t offset;
    genoffset_t length;
    struct memory_ele* next;
    struct memory_ele* prev;
};

struct memory_list {
    regionid_t rid;
    genoffset_t length;
    struct memory_ele* buffers;
    struct memory_list* next; // next in list of lists
};

struct operation {
    char str[MAX_STR_SIZE];
    genoffset_t offset;
    genoffset_t length;
};

struct debug_q {
    struct devq my_q;
    struct devq* q;
    struct memory_list* regions; // list of lists
    struct slab_allocator alloc;
    uint16_t hist_head;
    struct operation history[HIST_SIZE];
};

static void dump_list(struct memory_list* region)
{  
    struct memory_ele* ele = region->buffers;
    int index = 0;
    printf("================================================ \n");
    while (ele != NULL) {
        printf("Idx=%d offset=%lu length=%lu", index, ele->offset,
               ele->length);
        if (ele->prev != NULL) {

            printf(" prev->offset=%lu prev->length=%lu", ele->prev->offset,
                   ele->prev->length);
        }
        printf(" \n");
        ele = ele->next;
        index++;
    }   
    printf("================================================ \n");
}

// checks if there are elements in the list that should be merged
static void check_consistency(struct memory_list* region)
{
    struct memory_ele* ele = region->buffers;
    while (ele->next != NULL) {
        if (ele->offset + ele->length == ele->next->offset) {
            printf("offset=%lu length=%lu \n", ele->offset, ele->length);
            dump_list(region);
            USER_PANIC("Found entry that should be merged \n");
        }
        ele = ele->next;
    }   
}

static void add_to_history(struct debug_q* q, genoffset_t offset, 
                           genoffset_t length, char* s)
{
    q->history[q->hist_head].offset = offset;
    q->history[q->hist_head].length = length;
    strncpy(q->history[q->hist_head].str, s, MAX_STR_SIZE);
    q->hist_head = (q->hist_head + 1) % HIST_SIZE;

}

static void dump_history(struct debug_q* q)
{
    for (int i = 0; i < HIST_SIZE; i++) {
        printf("offset=%lu length=%lu %s\n", q->history[q->hist_head].offset, 
               q->history[q->hist_head].length, q->history[q->hist_head].str);
       
        q->hist_head = (q->hist_head + 1) % HIST_SIZE;
    }
}

/*
static bool in_list(struct memory_list* region, genoffset_t offset,
                    genoffset_t length)
{
    struct memory_ele* ele = region->buffers;
    while (ele != NULL) {
        if (offset >= ele->offset && 
            offset + length <= ele->offset+ele->length) {
            return true;
        }
        ele = ele->next;
    }   
    return false;
}
*/



static errval_t debug_register(struct devq* q, struct capref cap,
                               regionid_t rid) 
{
    errval_t err;
    struct debug_q* que = (struct debug_q*) q;
    DEBUG("Register \n");
    struct frame_identity id;
    
    err = invoke_frame_identify(cap, &id);
    if (err_is_fail(err)) {
        return err;
    }
        
    // queue of regions is empty
    if (que->regions == NULL) {
        // add the reigon
        err = que->q->f.reg(que->q, cap, rid);
        if (err_is_fail(err)) {
            return err;
        }

        que->regions = calloc(1, sizeof(struct memory_list));
        que->regions->rid = rid;
        que->regions->length = id.bytes;
        que->regions->next = NULL;
        // add the whole regions as a buffer
        que->regions->buffers = slab_alloc(&que->alloc);
        que->regions->buffers->offset = 0;
        que->regions->buffers->length = id.bytes;
        que->regions->buffers->next = NULL;
        DEBUG("Register rid=%"PRIu32" size=%"PRIu64" \n", rid, 
              id.bytes);
        return SYS_ERR_OK;
    }

    struct memory_list* ele = que->regions;
    while (ele->next != NULL) {
        ele = ele->next;
    }

    err = que->q->f.reg(que->q, cap, rid);
    if (err_is_fail(err)) {
        return err;
    }

    // add the reigon
    ele->next = calloc(1,sizeof(struct memory_list));
    ele = ele->next;

    ele->rid = rid;
    ele->next = NULL;
    ele->length = id.bytes;
    // add the whole regions as a buffer
    ele->buffers = slab_alloc(&que->alloc);
    ele->buffers->offset = 0;
    ele->buffers->length = id.bytes;
    ele->buffers->next = NULL;
    DEBUG("Register rid=%"PRIu32" size=%"PRIu64" \n", rid, 
          id.bytes);

    return SYS_ERR_OK;
}

static errval_t debug_deregister(struct devq* q, regionid_t rid) 
{
    DEBUG("Deregister \n");
    struct debug_q* que = (struct debug_q*) q;
    errval_t err;

    struct memory_list* ele = que->regions;
    if (ele == NULL) {
        return DEVQ_ERR_INVALID_REGION_ID;
    }    

    // remove head
    if (ele->rid == rid) {
        // there should only be a single element in the list
        // i.e. the whole region
        if (ele->buffers->offset == 0 &&
            ele->buffers->length == ele->length &&
            ele->buffers->next == NULL) {

            err = que->q->f.dereg(que->q, rid);
            if (err_is_fail(err)) {
                return err;
            }
            que->regions = ele->next;
        
            DEBUG("removed region rid=%"PRIu32" size=%"PRIu64" \n", rid, 
                  ele->length);

            slab_free(&que->alloc, ele->buffers);
            free(ele);

            return SYS_ERR_OK;
        } else {

            DEBUG("Destroy error rid=%d offset=%"PRIu64" length=%"PRIu64" "
                       "should be offset=0 length=%"PRIu64"\n",
                       ele->rid, ele->buffers->offset, 
                       ele->buffers->length, ele->length);
            dump_list(ele);
            return DEVQ_ERR_REGION_DESTROY;
        }   
    }
    
    while (ele->next != NULL) {
        if (ele->next->rid == rid) {
            if (ele->next->buffers->offset == 0 &&
                ele->next->buffers->length == ele->next->length &&
                ele->next->buffers->next == NULL) {
            
                err = que->q->f.dereg(que->q, rid);
                if (err_is_fail(err)) {
                    return err;
                }
                // remove from queue
                struct memory_list* next = ele->next;
                ele->next = ele->next->next;    

                DEBUG("removed region rid=%"PRIu32" size=%"PRIu64" \n", rid, 
                      next->length);
            
                slab_free(&que->alloc, next->buffers);
                free(next);

                return SYS_ERR_OK;
            } else {
                DEBUG("Destroy error rid=%d offset=%"PRIu64" length=%"PRIu64" "
                       "should be offset=0 length=%"PRIu64"\n",
                       ele->next->rid, ele->next->buffers->offset, 
                       ele->next->buffers->length, ele->next->length);
    
                dump_list(ele);
                return DEVQ_ERR_REGION_DESTROY;
            }   
        } else {
            ele = ele->next;
        }
    }
    

    return DEVQ_ERR_INVALID_REGION_ID;
}


static errval_t debug_control(struct devq* q, uint64_t cmd, uint64_t value,
                              uint64_t* result)
{
    DEBUG("control \n");
    struct debug_q* que = (struct debug_q*) q;
    return que->q->f.ctrl(que->q, cmd, value, result);
}


static errval_t debug_notify(struct devq* q)
{
    DEBUG("notify \n");
    struct debug_q* que = (struct debug_q*) q;
    return que->q->f.notify(que->q);
}


// is b1 in bounds of b2?
static bool buffer_in_bounds(genoffset_t offset_b1, genoffset_t len_b1,
                             genoffset_t offset_b2, genoffset_t len_b2)
{
    return (offset_b1 >= offset_b2) && (len_b1 <= len_b2) &&
           ((offset_b1 + len_b1) <= offset_b2 + len_b2);
}

// assumes that the buffer described by offset and length is contained
// in the buffer that is given as a struct
static void remove_split_buffer(struct debug_q* que,
                                struct memory_list* region,
                                struct memory_ele* buffer,
                                genoffset_t offset,
                                genoffset_t length) 
{
    // split the buffer 
    // insert half before the buffer

    DEBUG("enqueue offset=%"PRIu64" length=%"PRIu64" buf->offset=%lu "
          "buf->length %lu \n",
          offset, length, buffer->offset, buffer->length);

    // check if buffer at beginning of region
    if (buffer->offset == offset) {
        buffer->offset += length;
        buffer->length -= length;

        if (buffer->length == 0) {
            add_to_history(que, offset, length, "enq cut of beginning remove");
            DEBUG("enqueue remove buffer from list\n");
            // remove
            if (buffer->prev != NULL) {
                buffer->prev->next = buffer->next;
            } else {
                region->buffers = buffer->next;
            }
        
            if (buffer->next != NULL) {
                buffer->next->prev = buffer->prev;
            }
            slab_free(&que->alloc, buffer);
        } else {
            add_to_history(que, offset, length, "enq cut of beginning");
        }

        DEBUG("enqueue first cut off begining results in offset=%"PRIu64" "
              "length=%"PRIu64"\n",
              buffer->offset, buffer->length);
        return;
    }

    // check if buffer at end of region
    if ((buffer->offset+buffer->length) == (offset+length)) {
        buffer->length -= length;

        if (buffer->length == 0) {
            add_to_history(que, offset, length, "enq cut of end remove");
            // remove
            if (buffer->prev != NULL) {
                buffer->prev = buffer->next;
            }
        
            if (buffer->next != NULL) {
                buffer->next->prev = buffer->prev;
            }
            slab_free(&que->alloc, buffer);
        } else {
            add_to_history(que, offset, length, "enq cut of end");
        }
    
        DEBUG("enqueue first cut off end results in offset=%"PRIu64" "
              "length=%"PRIu64"\n",
              buffer->offset, buffer->length);
        return;
    }

    // now if this did not work need to split buffer that contains the
    // enqueued buffer into two buffers (might also result only in one)

    // inset half before buffer
    genoffset_t old_len = buffer->length;

    buffer->length = offset - buffer->offset;  

    struct memory_ele* after = NULL;
    after = slab_alloc(&que->alloc);
    after->offset = buffer->offset + buffer->length + length;
    after->length = old_len - buffer->length - length;

    // insert after buffer
    after->prev = buffer;
    after->next = buffer->next;

    if (buffer->next != NULL) {
        buffer->next->prev = after;
    }

    buffer->next = after;

    add_to_history(que, offset, length, "enq split buffer");

    DEBUG("Split buffer length=%lu to "
          "offset=%"PRIu64" length=%"PRIu64" and "
          "offset=%lu length=%lu \n",
          old_len, 
          buffer->offset, buffer->length,
          after->offset, after->length);

}

/*
 * Inserts a buffer either before or after an existing buffer into the queue
 * Checks for merges of prev/next buffer in the queue
 */
static void insert_merge_buffer(struct debug_q* que,
                                struct memory_list* region,
                                struct memory_ele* buffer,
                                genoffset_t offset,
                                genoffset_t length) 
{
    assert(buffer != NULL);
    assert(region != NULL);

    if (offset >= buffer->offset+buffer->length) {// insert after
        // buffer is on lower boundary
        //
        if (buffer->offset+length == offset) {
            buffer->length += length;
            DEBUG("dequeue merge after "
                  "offset=%"PRIu64" length=%"PRIu64" to offset=%"PRIu64" "
                  "length=%"PRIu64"\n",
                  offset, length, buffer->offset, buffer->length);
            // check other boundary for merge
            if (buffer->next != NULL &&
                (buffer->offset + buffer->length == buffer->next->offset)) {
                buffer->length += buffer->next->length;
                struct memory_ele* next = buffer->next;
                if (buffer->next->next != NULL) {
                    buffer->next = buffer->next->next;
                    buffer->next->next->prev = buffer;
                }

                DEBUG("dequeue merge after more offset=%"PRIu64" "
                      "length=%"PRIu64" to offset=%"PRIu64" length=%"PRIu64" \n ",
                      next->offset, next->length,
                      buffer->offset, buffer->length);

                add_to_history(que, offset, length, "deq insert after"
                               " on lower boundary and merge");

                slab_free(&que->alloc, next);
            } else {
                add_to_history(que, offset, length, "deq insert after on lower boundary");
            }
        } else { 
            // check higher boundary
            if (buffer->next != NULL && 
                buffer->next->offset == offset+length) {

                buffer->next->offset = offset;
                buffer->next->length += length;

                DEBUG("dequeue merge after more offset=%"PRIu64" "
                      "length=%"PRIu64" to offset=%"PRIu64" length=%"PRIu64" \n ",
                      offset, length,
                      buffer->next->offset, buffer->next->length);
                add_to_history(que, offset, length, "deq insert after"
                               " on higer boundary");
            
            } else { 
                // buffer->next can be null and the newly inserted buffer
                // is the inserted at the end -> check boundary
                if (buffer->next == NULL && 
                    buffer->offset + buffer->length == offset) {

                    buffer->length += length;

                    add_to_history(que, offset, length, "deq insert after"
                                   " on higer boundary end");

                    DEBUG("dequeue insert after merged offset=%"PRIu64" "
                          "length=%"PRIu64" "
                          "to offset=%"PRIu64" length=%"PRIu64" \n",
                          offset, length, buffer->offset, buffer->length);
                } else {
                    // insert in between
                    struct memory_ele* ele = slab_alloc(&que->alloc);
                    assert(ele != NULL);
                    
                    ele->offset = offset;
                    ele->length = length;
                    ele->next = buffer->next;
                    ele->prev = buffer;            

                    if (buffer->next != NULL) {
                        buffer->next->prev = ele;
                    }

                    buffer->next = ele;

                    add_to_history(que, offset, length, "deq insert after"
                                   " in between");
                    DEBUG("dequeue insert after offset=%"PRIu64" length=%"PRIu64" "
                          "after offset=%"PRIu64" length=%"PRIu64" \n",
                          offset, length, buffer->offset, buffer->length);
                }
            }
        }
    } else { // insert before buffer
        // buffer is on lower boundary
        if (buffer->offset == offset+length) {
            buffer->length += length;
            buffer->offset = offset;

            // check other boundary
            if (buffer->prev != NULL &&
                (buffer->prev->offset+ buffer->prev->length == 
                buffer->offset)) {
                struct memory_ele* prev = buffer->prev;
                prev->length += buffer->length;
                prev->next = buffer->next;

                if (buffer->next != NULL) {
                    buffer->next->prev = prev;
                }

                slab_free(&que->alloc, buffer);

                add_to_history(que, offset, length, "deq insert buffer"
                               " before lower boundary merge");
                DEBUG("dequeue merge before more offset=%"PRIu64" "
                      "length=%"PRIu64" to offset=%"PRIu64" length=%"PRIu64" \n ",
                      offset, length, prev->offset, prev->length);
            } else {
                add_to_history(que, offset, length, "deq insert buffer"
                               " before lower boundary");
            }
        } else {
            // check lower boundary
            if (buffer->prev != NULL &&
                (buffer->prev->offset+ buffer->prev->length == 
                offset)) {
                if (length == 0) {
                    printf("Length is 0 \n");
                    buffer->prev->length += 2048;
                }

                buffer->prev->length += length;

                add_to_history(que, offset, length, "deq insert buffer"
                               " before prev lower boundary merge");
                DEBUG("dequeue merge before more offset=%"PRIu64" "
                      "length=%"PRIu64" to offset=%"PRIu64" length=%"PRIu64" \n ",
                      offset, length, buffer->prev->offset, buffer->prev->length);
            } else { // insert in between

                // insert in between
                struct memory_ele* ele = slab_alloc(&que->alloc);
                assert(ele != NULL);
                
                ele->offset = offset;
                ele->length = length;
                ele->next = buffer;
                ele->prev = buffer->prev;            

                if (buffer->prev != NULL) {
                    buffer->prev->next = ele;
                } else {
                    region->buffers = ele;
                }

                buffer->prev = ele;

                add_to_history(que, offset, length, "deq insert buffer"
                               " before in between");
                DEBUG("dequeue insert before offset=%"PRIu64" length=%"PRIu64" "
                      "next is offset=%"PRIu64" length=%"PRIu64" \n",
                      offset, length,
                      buffer->offset, buffer->length);
            }
        }
    }
}

static errval_t find_region(struct debug_q* que, struct memory_list** list,
                            regionid_t rid)
{
    // find region
    struct memory_list* region = que->regions;
   
    while (region != NULL) {
        if (region->rid == rid) {
            break;
        }        
        region = region->next;
    }
    
    // check if we found the region
    if (region == NULL) {
        return DEVQ_ERR_INVALID_REGION_ID;
    }

    *list = region;
    return SYS_ERR_OK;
}

static errval_t debug_enqueue(struct devq* q, regionid_t rid, 
                              genoffset_t offset, genoffset_t length,
                              genoffset_t valid_data, genoffset_t valid_length,
                              uint64_t flags)
{
    assert(length > 0);
    DEBUG("enqueue offset %"PRIu64" \n", offset);
    errval_t err;
    struct debug_q* que = (struct debug_q*) q;

    // find region
    struct memory_list* region = NULL;

    err = find_region(que, &region, rid);
    if (err_is_fail(err)){
        return err;
    }
    
    check_consistency(region);

    // find the buffer 
    struct memory_ele* buffer = region->buffers;
    
    if (region->buffers == NULL) {
        return DEVQ_ERR_BUFFER_ALREADY_IN_USE;
    }    

    // the only buffer
    if (buffer->next == NULL) {
        if (buffer_in_bounds(offset, length,
                             buffer->offset, buffer->length)) {
            err = que->q->f.enq(que->q, rid, offset, length, valid_data,
                                valid_length, flags);
            if (err_is_fail(err)) {
                return err;
            }   

            remove_split_buffer(que, region, buffer, offset, length);
            return SYS_ERR_OK;          
        } else {
            printf("Bounds check failed only buffer offset=%lu length=%lu " 
                  " buf->offset=%lu buf->len=%lu\n", offset, length,
                  buffer->offset, buffer->length);
            dump_history(que);
            dump_list(region);
            return DEVQ_ERR_INVALID_BUFFER_ARGS;
        }
    }


    // more than one buffer
    while (buffer != NULL) {
        if (buffer_in_bounds(offset, length, 
                             buffer->offset, buffer->length)){
            err = que->q->f.enq(que->q, rid, offset, length, valid_data,
                                valid_length, flags);
            if (err_is_fail(err)) {
                return err;
            }   

            remove_split_buffer(que, region, buffer, offset, length);
            return SYS_ERR_OK;          
        }
        buffer = buffer->next;
    }  
    
    printf("Did not find region offset=%ld length=%ld \n", offset, length);
    dump_history(que);
    dump_list(region);

    return DEVQ_ERR_INVALID_BUFFER_ARGS;
}

static errval_t debug_dequeue(struct devq* q, regionid_t* rid, genoffset_t* offset,
                              genoffset_t* length, genoffset_t* valid_data,
                              genoffset_t* valid_length, uint64_t* flags)
{
    errval_t err;
    struct debug_q* que = (struct debug_q*) q;
    err = que->q->f.deq(que->q, rid, offset, length, valid_data,
                        valid_length, flags);
    if (err_is_fail(err)) {
        return err;
    }

    DEBUG("dequeued offset=%lu \n", *offset);

    struct memory_list* region = NULL;

    err = find_region(que, &region, *rid);
    if (err_is_fail(err)){
        return err;
    }

    check_consistency(region);

    // find the buffer 
    struct memory_ele* buffer = region->buffers;
    if (buffer == NULL) {
        region->buffers = slab_alloc(&que->alloc);
        region->buffers->offset = *offset;
        region->buffers->length = *length;
        region->buffers->next = NULL;
        region->buffers->prev = NULL;
        return SYS_ERR_OK;
    }

    if (buffer->next == NULL) {
        if (!buffer_in_bounds(*offset, *length, buffer->offset,
                              buffer->length)) {
            insert_merge_buffer(que, region, buffer, *offset, *length);
            return SYS_ERR_OK;
        } else {
            return DEVQ_ERR_BUFFER_NOT_IN_USE;
        }
    }


    while (buffer->next != NULL) {
        if (*offset >= buffer->offset) {
            buffer = buffer->next;
        } else {
            if (!buffer_in_bounds(*offset, *length, buffer->offset, 
                buffer->length)) {
                insert_merge_buffer(que, region, buffer, *offset, *length);
                return SYS_ERR_OK;
            } else {
                return DEVQ_ERR_BUFFER_NOT_IN_USE;
            }
        }
    }

    // insert after the last buffer
    if (!buffer_in_bounds(*offset, *length, buffer->offset, 
        buffer->length)) {
        insert_merge_buffer(que, region, buffer, *offset, *length);
        return SYS_ERR_OK;
    }

    return DEVQ_ERR_BUFFER_NOT_IN_USE;
}

/**
 * Public functions
 *
 */

errval_t debug_create(struct debug_q** q, struct devq* other_q)
{
    errval_t err;
    struct debug_q* que;
    que = calloc(1, sizeof(struct debug_q));
    assert(que);

    slab_init(&que->alloc, sizeof(struct memory_ele),
              slab_default_refill);
   
    que->q = other_q;
    err = devq_init(&que->my_q, false);
    if (err_is_fail(err)) {
        return err;
    }   

    que->my_q.f.reg = debug_register;
    que->my_q.f.dereg = debug_deregister;
    que->my_q.f.ctrl = debug_control;
    que->my_q.f.notify = debug_notify;
    que->my_q.f.enq = debug_enqueue;
    que->my_q.f.deq = debug_dequeue;
    *q = que;
    return SYS_ERR_OK;
}

errval_t debug_destroy(struct debug_q* q, struct devq* devq)
{
    devq = q->q;
    free(q);    

    return SYS_ERR_OK;
}

