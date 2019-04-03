/*
 * Copyright (c) 2014, University of Washington.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef TENACIOUSD_QUEUE_H
#define TENACIOUSD_QUEUE_H

struct storage_vsa;
struct storage_vsic;

struct tenaciousd_queue_element {
  uint8_t 	valid;
  uint64_t	size;
  uint64_t	prev;
  uint64_t	next;		// Only valid on disk
  uint8_t	data[0];
  uint8_t	marker;		// Never valid
} __attribute__ ((packed));

struct tenaciousd_queue_iter {
  struct tenaciousd_queue_element	*element;
  struct tenaciousd_queue_iter		*next;
};

struct tenaciousd_queue {
  struct storage_vsa *vsa;
  struct storage_vsic *vsic;
  uint64_t elements;
  uint64_t last;
  uint64_t end;
};

void tenaciousd_queue_delete_element(struct tenaciousd_queue *queue,
				     struct tenaciousd_queue_element *element);

struct tenaciousd_queue *tenaciousd_queue_new(struct storage_vsa *vsa,
					  struct storage_vsic *vsic);

errval_t tenaciousd_queue_delete(struct tenaciousd_queue *queue);

struct tenaciousd_queue_element * 
tenaciousd_queue_element_new(struct tenaciousd_queue *queue,
                         size_t *size);

errval_t tenaciousd_queue_add(struct tenaciousd_queue *log,
                               struct tenaciousd_queue_element *element);

struct tenaciousd_queue_element * 
tenaciousd_queue_remove(struct tenaciousd_queue *queue);

struct tenaciousd_queue_iter tenaciousd_queue_begin(struct tenaciousd_queue *log);

struct tenaciousd_queue_iter tenaciousd_queue_next(struct tenaciousd_queue *log,
						   struct tenaciousd_queue_iter iter);

static inline bool tenaciousd_queue_end(struct tenaciousd_queue_iter iter)
{
  return iter.element == NULL ? true : false;
}

static inline void *tenaciousd_queue_iter_data(struct tenaciousd_queue_iter iter)
{
    return (void *)iter.element->data;
}

#endif
