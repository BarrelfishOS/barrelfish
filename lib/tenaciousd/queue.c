/*
 * Copyright (c) 2014, University of Washington.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <alloca.h>
#include <stdint.h>
#include <unistd.h>
#include <fcntl.h>
#include <assert.h>
#include <string.h>
#include <errors/errno.h>
#include <tenaciousd/queue.h>
#include <storage/storage.h>

#define QUEUE_IDENTIFIER	"TenaciousD_Queue_structure_rev01"

#define QUEUE_FIRST_ELEMENT_OFFSET(queue) \
  STORAGE_VSIC_ROUND(queue->vsic, sizeof(struct queue_header))

#define QUEUE_MIN_ELEMENT_SIZE(queue) \
  STORAGE_VSIC_ROUND(queue->vsic, sizeof(struct tenaciousd_queue_element))

#define QUEUE_ELEMENT_VALID		0x01
#define QUEUE_ELEMENT_INVALID		0x00
#define QUEUE_ELEMENT_END_MARKER	0xff

struct queue_header {
  char		identifier[32];
  uint8_t	version;
  uint64_t	elements;
  uint32_t	blocksize;
  uint64_t	last;
  uint64_t	end;
} __attribute__ ((packed));

static struct tenaciousd_queue_element *tenaciousd_queue_read_element(struct tenaciousd_queue *queue,
								      off_t offset)
{
    // Allocate minimum sized log entry
    struct tenaciousd_queue_element *element =
      storage_malloc(queue->vsic, sizeof(struct tenaciousd_queue_element));
    assert(element != NULL);

    // Read minimum sized entry
    errval_t err = queue->vsic->ops.read(queue->vsic, queue->vsa, offset,
				       QUEUE_MIN_ELEMENT_SIZE(queue), element);
    assert(err_is_ok(err));
    err = queue->vsic->ops.wait(queue->vsic);
    assert(err_is_ok(err));

    if(err_no(err) == VFS_ERR_EOF) {
        // Invalid read
        storage_free(queue, element);
        return NULL;
    }

    if (element->valid != QUEUE_ELEMENT_VALID) {
      // Invalid element
      storage_free(queue, element);
      return NULL;
    }

    // If this was less or equal to the min size, we're done
    if(element->size + sizeof(struct tenaciousd_queue_element)
       <= QUEUE_MIN_ELEMENT_SIZE(queue)) {
      return element;
    }

    // Otherwise, we have to read the rest
    element = storage_realloc(queue->vsic, element,
			      element->size + sizeof(struct tenaciousd_queue_element));
    assert(element != NULL);
    err = queue->vsic->ops.read(queue->vsic, queue->vsa,
                              offset + QUEUE_MIN_ELEMENT_SIZE(queue),
                              element->size - QUEUE_MIN_ELEMENT_SIZE(queue) + sizeof(struct tenaciousd_queue_element),
       ((uint8_t *)element) + QUEUE_MIN_ELEMENT_SIZE(queue));
    assert(err_is_ok(err));
    err = queue->vsic->ops.wait(queue->vsic);
    assert(err_is_ok(err));

    // Is the end marker there?
    if(element->data[element->size] != QUEUE_ELEMENT_END_MARKER) {
        // This entry is invalid
        storage_free(queue, element);
        return NULL;
    }

    return element;
}


void tenaciousd_queue_delete_entry(struct tenaciousd_queue *queue,
				 struct tenaciousd_queue_element *element)
{
  storage_free(queue->vsic, element);
}


struct tenaciousd_queue *tenaciousd_queue_new(struct storage_vsa *vsa,
					  struct storage_vsic *vsic)
{
  assert(vsa != NULL);
  assert(vsic != NULL);

  struct tenaciousd_queue *queue = malloc(sizeof(struct tenaciousd_queue));
  assert(queue != NULL);
  memset(queue, 0, sizeof(queue));

  queue->vsa = vsa;
  queue->vsic = vsic;

  // Check if VSA already has a queue
  struct queue_header *header = storage_alloca(vsic, sizeof(struct queue_header));
  assert(header != NULL);
  errval_t err = vsic->ops.read(vsic, vsa, 0,
				sizeof(struct queue_header), header);
  assert(err_is_ok(err));
  err = vsic->ops.wait(vsic);
  assert(err_is_ok(err));

  if(!strncmp(header->identifier, QUEUE_IDENTIFIER, sizeof(header->identifier))) {
    // Queue already exists -- initialize from storage
    assert(header->blocksize == vsic->blocksize);
    queue->elements = 0;
    queue->end = STORAGE_VSIC_ROUND(vsic, sizeof(struct queue_header));
    queue->last = header->end;

    // Check whether header is up-to-date
    for(;;) {
        struct tenaciousd_queue_element *element = tenaciousd_queue_read_element(queue, queue->end);

        // Either we couldn't read (cause EOF) or there's no entry
        if(element == NULL || element->size == 0) {
            // Header up-to-date -- we're done
            return queue;
        }

        // More entries -- log header not up-to-date
        queue->elements++;
	// modified...
	//     queue->end += STORAGE_VSIC_ROUND(log->vsic, element->size + sizeof(struct tenaciousd_queue_element));
	queue->last = queue->end;
	queue->end = element->next;
        tenaciousd_queue_delete_element(queue, element);
    }
  } else {
    // Reset header
    memset(header, 0, sizeof(struct queue_header));
    memcpy(header->identifier, QUEUE_IDENTIFIER, 32);
    header->version = 0;
    header->blocksize = vsic->blocksize;
    header->end = STORAGE_VSIC_ROUND(vsic, sizeof(struct queue_header));
    header->last = header->end;
  }

  // Write new header
  err = vsic->ops.write(vsic, vsa, 0, sizeof(struct queue_header), header);
  assert(err_is_ok(err));
  err = vsic->ops.flush(vsic, vsa);
  assert(err_is_ok(err));
  err = vsic->ops.wait(vsic);
  assert(err_is_ok(err));

  queue->elements = header->elements;
  queue->end = header->end;
  queue->last = header->last;

  return queue;
}

errval_t tenaciousd_queue_delete(struct tenaciousd_queue *queue)
{
  // Flush out log
  errval_t err = queue->vsic->ops.flush(queue->vsic, queue->vsa);
  assert(err_is_ok(err));

  // Update header and flush again and wait for it to finish
  struct queue_header *header =
      storage_alloca(queue->vsic, sizeof(struct queue_header));
  assert(header != NULL);

  memset(header, 0, sizeof(struct queue_header));
  memcpy(header->identifier, QUEUE_IDENTIFIER, 32);
  header->version = 0;
  header->elements = queue->elements;
  header->blocksize = queue->vsic->blocksize;
  header->end = queue->end;
  header->last = queue->last;

  err = queue->vsic->ops.write(queue->vsic, queue->vsa, 0, sizeof(struct queue_header), header);
  assert(err_is_ok(err));
  err = queue->vsic->ops.flush(queue->vsic, queue->vsa);
  assert(err_is_ok(err));
  err = queue->vsic->ops.wait(queue->vsic);
  assert(err_is_ok(err));

  // Free memory and return
  free(queue);
  return SYS_ERR_OK;
}

struct tenaciousd_queue_element *
tenaciousd_queue_element_new(struct tenaciousd_queue *queue, size_t *size)
{
    assert(*size > 0);
    *size += sizeof(struct tenaciousd_queue_element);
    struct tenaciousd_queue_element *element = storage_malloc(queue->vsic, *size);
    assert(element != NULL);

    //    *size = STORAGE_VSIC_ROUND(queue->vsic, *size)
    //        - sizeof(struct tenaciousd_queue_element);
    *size -= sizeof(struct tenaciousd_queue_element);
    element->size = *size;
    return element;
}


errval_t tenaciousd_queue_add(struct tenaciousd_queue *queue,
                               struct tenaciousd_queue_element *element)
{
  struct storage_vsic *vsic = queue->vsic;
  off_t end = queue->end;

  element->prev = queue->last;
  queue->last = end;
  queue->end = element->next = end +
    STORAGE_VSIC_ROUND(vsic, element->size + sizeof(struct tenaciousd_queue_element));
  queue->elements++;
  element->valid = QUEUE_ELEMENT_VALID;
  element->data[element->size] = QUEUE_ELEMENT_END_MARKER;

  return queue->vsic->ops.
    write(vsic, queue->vsa, end,
	  element->size + sizeof(struct tenaciousd_queue_element), element);
}

struct tenaciousd_queue_element * tenaciousd_queue_remove(struct tenaciousd_queue *queue) {
  if (queue->last == queue->end) {
    return NULL;
  }

  uint64_t last = queue->last;

  struct tenaciousd_queue_element * element = tenaciousd_queue_read_element(queue, queue->last);

  assert(element != NULL);

  queue->last = element->prev;
  queue->end -= STORAGE_VSIC_ROUND(queue->vsic, element->size + sizeof(struct tenaciousd_queue_element));

  element->valid = QUEUE_ELEMENT_INVALID;
  queue->vsic->ops.write(queue->vsic, queue->vsa, last, sizeof(struct tenaciousd_queue_element), element);

  return element;
}


struct tenaciousd_queue_iter tenaciousd_queue_begin(struct tenaciousd_queue *queue)
{
  struct tenaciousd_queue_element *element =
      tenaciousd_queue_read_element(queue, QUEUE_FIRST_ELEMENT_OFFSET(queue));

  if(element == NULL) {
      queue->elements = 0;
  }

  // TODO: Could do some prefetching here...

  return (struct tenaciousd_queue_iter) {
      .element = element,
      .next = NULL,
  };
}

struct tenaciousd_queue_iter tenaciousd_queue_next(struct tenaciousd_queue *queue,
					       struct tenaciousd_queue_iter iter)
{
  if(iter.next != NULL) {
      // Cached
      iter = *iter.next;
  } else {
      // Not cached
    if (iter.element != NULL) {      
      uint64_t offset = (iter.element)->next;
      iter.element = tenaciousd_queue_read_element(queue, offset);
    }
  }

  // TODO: Prefetch another one

  return iter;
}
