/**
 * @file
 * Packet buffer management
 */

/**
 * @defgroup pbuf Packet buffers (PBUF)
 * @ingroup infrastructure
 *
 * Packets are built from the pbuf data structure. It supports dynamic
 * memory allocation for packet contents or can reference externally
 * managed packet contents both in RAM and ROM. Quick allocation for
 * incoming packets is provided through pools with fixed sized pbufs.
 *
 * A packet may span over multiple pbufs, chained as a singly linked
 * list. This is called a "pbuf chain".
 *
 * Multiple packets may be queued, also using this singly linked list.
 * This is called a "packet queue".
 *
 * So, a packet queue consists of one or more pbuf chains, each of
 * which consist of one or more pbufs. CURRENTLY, PACKET QUEUES ARE
 * NOT SUPPORTED!!! Use helper structs to queue multiple packets.
 *
 * The differences between a pbuf chain and a packet queue are very
 * precise but subtle.
 *
 * The last pbuf of a packet has a ->tot_len field that equals the
 * ->len field. It can be found by traversing the list. If the last
 * pbuf of a packet has a ->next field other than NULL, more packets
 * are on the queue.
 *
 * Therefore, looping through a pbuf of a single packet, has an
 * loop end condition (tot_len == p->len), NOT (next == NULL).
 *
 * Example of custom pbuf usage for zero-copy RX:
  @code{.c}
typedef struct my_custom_pbuf
{
   struct pbuf_custom p;
   void* dma_descriptor;
} my_custom_pbuf_t;

LWIP_MEMPOOL_DECLARE(RX_POOL, 10, sizeof(my_custom_pbuf_t), "Zero-copy RX PBUF pool");

void my_pbuf_free_custom(void* p)
{
  my_custom_pbuf_t* my_puf = (my_custom_pbuf_t*)p;

  LOCK_INTERRUPTS();
  free_rx_dma_descriptor(my_pbuf->dma_descriptor);
  LWIP_MEMPOOL_FREE(RX_POOL, my_pbuf);
  UNLOCK_INTERRUPTS();
}

void eth_rx_irq()
{
  dma_descriptor*   dma_desc = get_RX_DMA_descriptor_from_ethernet();
  my_custom_pbuf_t* my_pbuf  = (my_custom_pbuf_t*)LWIP_MEMPOOL_ALLOC(RX_POOL);

  my_pbuf->p.custom_free_function = my_pbuf_free_custom;
  my_pbuf->dma_descriptor         = dma_desc;

  invalidate_cpu_cache(dma_desc->rx_data, dma_desc->rx_length);
  
  struct pbuf* p = pbuf_alloced_custom(PBUF_RAW,
     dma_desc->rx_length,
     PBUF_REF,
     &my_pbuf->p,
     dma_desc->rx_data,
     dma_desc->max_buffer_size);

  if(netif->input(p, netif) != ERR_OK) {
    pbuf_free(p);
  }
}
  @endcode
 */

/*
 * Copyright (c) 2001-2004 Swedish Institute of Computer Science.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT
 * SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
 * OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY
 * OF SUCH DAMAGE.
 *
 * This file is part of the lwIP TCP/IP stack.
 *
 * Author: Adam Dunkels <adam@sics.se>
 *
 */

#include "lwip/opt.h"

#include "lwip/stats.h"
#include "lwip/def.h"
#include "lwip/mem.h"
#include "lwip/memp.h"
#include "lwip/pbuf.h"
#include "lwip/sys.h"
#if LWIP_TCP && TCP_QUEUE_OOSEQ
#include "lwip/priv/tcp_priv.h"
#endif
#if LWIP_CHECKSUM_ON_COPY
#include "lwip/inet_chksum.h"
#endif

#include <string.h>

#define SIZEOF_STRUCT_PBUF        LWIP_MEM_ALIGN_SIZE(sizeof(struct pbuf))
/* Since the pool is created in memp, PBUF_POOL_BUFSIZE will be automatically
   aligned there. Therefore, PBUF_POOL_BUFSIZE_ALIGNED can be used here. */
#define PBUF_POOL_BUFSIZE_ALIGNED LWIP_MEM_ALIGN_SIZE(PBUF_POOL_BUFSIZE)




/**
 * Adjusts the payload pointer to hide or reveal headers in the payload.
 * @see pbuf_header.
 *
 * @param p pbuf to change the header size.
 * @param header_size_increment Number of bytes to increment header size.
 * @param force Allow 'header_size_increment > 0' for PBUF_REF/PBUF_ROM types
 *
 * @return non-zero on failure, zero on success.
 *
 */
static u8_t
pbuf_header_impl(struct pbuf *p, s16_t header_size_increment, u8_t force)
{
  u16_t type;
  void *payload;
  u16_t increment_magnitude;

  LWIP_ASSERT("p != NULL", p != NULL);
  if ((header_size_increment == 0) || (p == NULL)) {
    return 0;
  }

  if (header_size_increment < 0) {
    increment_magnitude = (u16_t)-header_size_increment;
    /* Check that we aren't going to move off the end of the pbuf */
    LWIP_ERROR("increment_magnitude <= p->len", (increment_magnitude <= p->len), return 1;);
  } else {
    increment_magnitude = (u16_t)header_size_increment;
#if 0
    /* Can't assert these as some callers speculatively call
         pbuf_header() to see if it's OK.  Will return 1 below instead. */
    /* Check that we've got the correct type of pbuf to work with */
    LWIP_ASSERT("p->type == PBUF_RAM || p->type == PBUF_POOL",
                p->type == PBUF_RAM || p->type == PBUF_POOL);
    /* Check that we aren't going to move off the beginning of the pbuf */
    LWIP_ASSERT("p->payload - increment_magnitude >= p + SIZEOF_STRUCT_PBUF",
                (u8_t *)p->payload - increment_magnitude >= (u8_t *)p + SIZEOF_STRUCT_PBUF);
#endif
  }

  type = p->type;
  /* remember current payload pointer */
  payload = p->payload;

  /* pbuf types containing payloads? */
  if (type == PBUF_RAM || type == PBUF_POOL) {
    /* set new payload pointer */
    p->payload = (u8_t *)p->payload - header_size_increment;
    /* boundary check fails? */
    if ((u8_t *)p->payload < (u8_t *)p + SIZEOF_STRUCT_PBUF) {
      LWIP_DEBUGF( PBUF_DEBUG | LWIP_DBG_TRACE,
        ("pbuf_header: failed as %p < %p (not enough space for new header size)\n",
        (void *)p->payload, (void *)((u8_t *)p + SIZEOF_STRUCT_PBUF)));
      /* restore old payload pointer */
      p->payload = payload;
      /* bail out unsuccessfully */
      return 1;
    }
  /* pbuf types referring to external payloads? */
  } else if (type == PBUF_REF || type == PBUF_ROM) {
    /* hide a header in the payload? */
    if ((header_size_increment < 0) && (increment_magnitude <= p->len)) {
      /* increase payload pointer */
      p->payload = (u8_t *)p->payload - header_size_increment;
    } else if ((header_size_increment > 0) && force) {
      p->payload = (u8_t *)p->payload - header_size_increment;
    } else {
      /* cannot expand payload to front (yet!)
       * bail out unsuccessfully */
      return 1;
    }
  } else {
    /* Unknown type */
    LWIP_ASSERT("bad pbuf type", 0);
    return 1;
  }
  /* modify pbuf length fields */
  p->len += header_size_increment;
  p->tot_len += header_size_increment;

  LWIP_DEBUGF(PBUF_DEBUG | LWIP_DBG_TRACE, ("pbuf_header: old %p new %p (%"S16_F")\n",
    (void *)payload, (void *)p->payload, header_size_increment));

  return 0;
}

/**
 * Adjusts the payload pointer to hide or reveal headers in the payload.
 *
 * Adjusts the ->payload pointer so that space for a header
 * (dis)appears in the pbuf payload.
 *
 * The ->payload, ->tot_len and ->len fields are adjusted.
 *
 * @param p pbuf to change the header size.
 * @param header_size_increment Number of bytes to increment header size which
 * increases the size of the pbuf. New space is on the front.
 * (Using a negative value decreases the header size.)
 * If hdr_size_inc is 0, this function does nothing and returns successful.
 *
 * PBUF_ROM and PBUF_REF type buffers cannot have their sizes increased, so
 * the call will fail. A check is made that the increase in header size does
 * not move the payload pointer in front of the start of the buffer.
 * @return non-zero on failure, zero on success.
 *
 */
u8_t
pbuf_header(struct pbuf *p, s16_t header_size_increment)
{
   return pbuf_header_impl(p, header_size_increment, 1);
}

/**
 * Same as pbuf_header but does not check if 'header_size > 0' is allowed.
 * This is used internally only, to allow PBUF_REF for RX.
 */
u8_t
pbuf_header_force(struct pbuf *p, s16_t header_size_increment)
{
   return pbuf_header_impl(p, header_size_increment, 1);
}



/**
 * Count number of pbufs in a chain
 *
 * @param p first pbuf of chain
 * @return the number of pbufs in a chain
 */
u16_t
pbuf_clen(const struct pbuf *p)
{
  u16_t len;

  len = 0;
  while (p != NULL) {
    ++len;
    p = p->next;
  }
  return len;
}

/**
 * @ingroup pbuf
 * Increment the reference count of the pbuf.
 *
 * @param p pbuf to increase reference counter of
 *
 */
void
pbuf_ref(struct pbuf *p)
{
  /* pbuf given? */
  if (p != NULL) {
    SYS_ARCH_INC(p->ref, 1);
    LWIP_ASSERT("pbuf ref overflow", p->ref > 0);
  }
}

/**
 * @ingroup pbuf
 * Concatenate two pbufs (each may be a pbuf chain) and take over
 * the caller's reference of the tail pbuf.
 *
 * @note The caller MAY NOT reference the tail pbuf afterwards.
 * Use pbuf_chain() for that purpose.
 *
 * @see pbuf_chain()
 */
void
pbuf_cat(struct pbuf *h, struct pbuf *t)
{
  struct pbuf *p;

  LWIP_ERROR("(h != NULL) && (t != NULL) (programmer violates API)",
             ((h != NULL) && (t != NULL)), return;);

  /* proceed to last pbuf of chain */
  for (p = h; p->next != NULL; p = p->next) {
    /* add total length of second chain to all totals of first chain */
    p->tot_len += t->tot_len;
  }
  /* { p is last pbuf of first h chain, p->next == NULL } */
  LWIP_ASSERT("p->tot_len == p->len (of last pbuf in chain)", p->tot_len == p->len);
  LWIP_ASSERT("p->next == NULL", p->next == NULL);
  /* add total length of second chain to last pbuf total of first chain */
  p->tot_len += t->tot_len;
  /* chain last pbuf of head (p) with first of tail (t) */
  p->next = t;
  /* p->next now references t, but the caller will drop its reference to t,
   * so netto there is no change to the reference count of t.
   */
}

/**
 * @ingroup pbuf
 * Chain two pbufs (or pbuf chains) together.
 *
 * The caller MUST call pbuf_free(t) once it has stopped
 * using it. Use pbuf_cat() instead if you no longer use t.
 *
 * @param h head pbuf (chain)
 * @param t tail pbuf (chain)
 * @note The pbufs MUST belong to the same packet.
 * @note MAY NOT be called on a packet queue.
 *
 * The ->tot_len fields of all pbufs of the head chain are adjusted.
 * The ->next field of the last pbuf of the head chain is adjusted.
 * The ->ref field of the first pbuf of the tail chain is adjusted.
 *
 */
void
pbuf_chain(struct pbuf *h, struct pbuf *t)
{
  pbuf_cat(h, t);
  /* t is now referenced by h */
  pbuf_ref(t);
  LWIP_DEBUGF(PBUF_DEBUG | LWIP_DBG_TRACE, ("pbuf_chain: %p references %p\n", (void *)h, (void *)t));
}

/**
 * Dechains the first pbuf from its succeeding pbufs in the chain.
 *
 * Makes p->tot_len field equal to p->len.
 * @param p pbuf to dechain
 * @return remainder of the pbuf chain, or NULL if it was de-allocated.
 * @note May not be called on a packet queue.
 */
struct pbuf *
pbuf_dechain(struct pbuf *p)
{
  struct pbuf *q;
  u8_t tail_gone = 1;
  /* tail */
  q = p->next;
  /* pbuf has successor in chain? */
  if (q != NULL) {
    /* assert tot_len invariant: (p->tot_len == p->len + (p->next? p->next->tot_len: 0) */
    LWIP_ASSERT("p->tot_len == p->len + q->tot_len", q->tot_len == p->tot_len - p->len);
    /* enforce invariant if assertion is disabled */
    q->tot_len = p->tot_len - p->len;
    /* decouple pbuf from remainder */
    p->next = NULL;
    /* total length of pbuf p is its own length only */
    p->tot_len = p->len;
    /* q is no longer referenced by p, free it */
    LWIP_DEBUGF(PBUF_DEBUG | LWIP_DBG_TRACE, ("pbuf_dechain: unreferencing %p\n", (void *)q));
    tail_gone = pbuf_free(q);
    if (tail_gone > 0) {
      LWIP_DEBUGF(PBUF_DEBUG | LWIP_DBG_TRACE,
                  ("pbuf_dechain: deallocated %p (as it is no longer referenced)\n", (void *)q));
    }
    /* return remaining tail or NULL if deallocated */
  }
  /* assert tot_len invariant: (p->tot_len == p->len + (p->next? p->next->tot_len: 0) */
  LWIP_ASSERT("p->tot_len == p->len", p->tot_len == p->len);
  return ((tail_gone > 0) ? NULL : q);
}

/**
 * @ingroup pbuf
 * Create PBUF_RAM copies of pbufs.
 *
 * Used to queue packets on behalf of the lwIP stack, such as
 * ARP based queueing.
 *
 * @note You MUST explicitly use p = pbuf_take(p);
 *
 * @note Only one packet is copied, no packet queue!
 *
 * @param p_to pbuf destination of the copy
 * @param p_from pbuf source of the copy
 *
 * @return ERR_OK if pbuf was copied
 *         ERR_ARG if one of the pbufs is NULL or p_to is not big
 *                 enough to hold p_from
 */
err_t
pbuf_copy(struct pbuf *p_to, const struct pbuf *p_from)
{
  u16_t offset_to=0, offset_from=0, len;

  LWIP_DEBUGF(PBUF_DEBUG | LWIP_DBG_TRACE, ("pbuf_copy(%p, %p)\n",
    (const void*)p_to, (const void*)p_from));

  /* is the target big enough to hold the source? */
  LWIP_ERROR("pbuf_copy: target not big enough to hold source", ((p_to != NULL) &&
             (p_from != NULL) && (p_to->tot_len >= p_from->tot_len)), return ERR_ARG;);

  /* iterate through pbuf chain */
  do
  {
    /* copy one part of the original chain */
    if ((p_to->len - offset_to) >= (p_from->len - offset_from)) {
      /* complete current p_from fits into current p_to */
      len = p_from->len - offset_from;
    } else {
      /* current p_from does not fit into current p_to */
      len = p_to->len - offset_to;
    }
    MEMCPY((u8_t*)p_to->payload + offset_to, (u8_t*)p_from->payload + offset_from, len);
    offset_to += len;
    offset_from += len;
    LWIP_ASSERT("offset_to <= p_to->len", offset_to <= p_to->len);
    LWIP_ASSERT("offset_from <= p_from->len", offset_from <= p_from->len);
    if (offset_from >= p_from->len) {
      /* on to next p_from (if any) */
      offset_from = 0;
      p_from = p_from->next;
    }
    if (offset_to == p_to->len) {
      /* on to next p_to (if any) */
      offset_to = 0;
      p_to = p_to->next;
      LWIP_ERROR("p_to != NULL", (p_to != NULL) || (p_from == NULL) , return ERR_ARG;);
    }

    if ((p_from != NULL) && (p_from->len == p_from->tot_len)) {
      /* don't copy more than one packet! */
      LWIP_ERROR("pbuf_copy() does not allow packet queues!",
                 (p_from->next == NULL), return ERR_VAL;);
    }
    if ((p_to != NULL) && (p_to->len == p_to->tot_len)) {
      /* don't copy more than one packet! */
      LWIP_ERROR("pbuf_copy() does not allow packet queues!",
                  (p_to->next == NULL), return ERR_VAL;);
    }
  } while (p_from);
  LWIP_DEBUGF(PBUF_DEBUG | LWIP_DBG_TRACE, ("pbuf_copy: end of chain reached.\n"));
  return ERR_OK;
}

/**
 * @ingroup pbuf
 * Copy (part of) the contents of a packet buffer
 * to an application supplied buffer.
 *
 * @param buf the pbuf from which to copy data
 * @param dataptr the application supplied buffer
 * @param len length of data to copy (dataptr must be big enough). No more
 * than buf->tot_len will be copied, irrespective of len
 * @param offset offset into the packet buffer from where to begin copying len bytes
 * @return the number of bytes copied, or 0 on failure
 */
u16_t
pbuf_copy_partial(const struct pbuf *buf, void *dataptr, u16_t len, u16_t offset)
{
  const struct pbuf *p;
  u16_t left;
  u16_t buf_copy_len;
  u16_t copied_total = 0;

  LWIP_ERROR("pbuf_copy_partial: invalid buf", (buf != NULL), return 0;);
  LWIP_ERROR("pbuf_copy_partial: invalid dataptr", (dataptr != NULL), return 0;);

  left = 0;

  if ((buf == NULL) || (dataptr == NULL)) {
    return 0;
  }

  /* Note some systems use byte copy if dataptr or one of the pbuf payload pointers are unaligned. */
  for (p = buf; len != 0 && p != NULL; p = p->next) {
    if ((offset != 0) && (offset >= p->len)) {
      /* don't copy from this buffer -> on to the next */
      offset -= p->len;
    } else {
      /* copy from this buffer. maybe only partially. */
      buf_copy_len = p->len - offset;
      if (buf_copy_len > len) {
        buf_copy_len = len;
      }
      /* copy the necessary parts of the buffer */
      MEMCPY(&((char*)dataptr)[left], &((char*)p->payload)[offset], buf_copy_len);
      copied_total += buf_copy_len;
      left += buf_copy_len;
      len -= buf_copy_len;
      offset = 0;
    }
  }
  return copied_total;
}

#if LWIP_TCP && TCP_QUEUE_OOSEQ && LWIP_WND_SCALE
/**
 * This method modifies a 'pbuf chain', so that its total length is
 * smaller than 64K. The remainder of the original pbuf chain is stored
 * in *rest.
 * This function never creates new pbufs, but splits an existing chain
 * in two parts. The tot_len of the modified packet queue will likely be
 * smaller than 64K.
 * 'packet queues' are not supported by this function.
 *
 * @param p the pbuf queue to be split
 * @param rest pointer to store the remainder (after the first 64K)
 */
void pbuf_split_64k(struct pbuf *p, struct pbuf **rest)
{
  *rest = NULL;
  if ((p != NULL) && (p->next != NULL)) {
    u16_t tot_len_front = p->len;
    struct pbuf *i = p;
    struct pbuf *r = p->next;

    /* continue until the total length (summed up as u16_t) overflows */
    while ((r != NULL) && ((u16_t)(tot_len_front + r->len) > tot_len_front)) {
      tot_len_front += r->len;
      i = r;
      r = r->next;
    }
    /* i now points to last packet of the first segment. Set next
       pointer to NULL */
    i->next = NULL;

    if (r != NULL) {
      /* Update the tot_len field in the first part */
      for (i = p; i != NULL; i = i->next) {
        i->tot_len -= r->tot_len;
        LWIP_ASSERT("tot_len/len mismatch in last pbuf",
                    (i->next != NULL) || (i->tot_len == i->len));
      }
      if (p->flags & PBUF_FLAG_TCP_FIN) {
        r->flags |= PBUF_FLAG_TCP_FIN;
      }

      /* tot_len field in rest does not need modifications */
      /* reference counters do not need modifications */
      *rest = r;
    }
  }
}
#endif /* LWIP_TCP && TCP_QUEUE_OOSEQ && LWIP_WND_SCALE */

/* Actual implementation of pbuf_skip() but returning const pointer... */
static const struct pbuf*
pbuf_skip_const(const struct pbuf* in, u16_t in_offset, u16_t* out_offset)
{
  u16_t offset_left = in_offset;
  const struct pbuf* q = in;

  /* get the correct pbuf */
  while ((q != NULL) && (q->len <= offset_left)) {
    offset_left -= q->len;
    q = q->next;
  }
  if (out_offset != NULL) {
    *out_offset = offset_left;
  }
  return q;
}

/**
 * @ingroup pbuf
 * Skip a number of bytes at the start of a pbuf
 *
 * @param in input pbuf
 * @param in_offset offset to skip
 * @param out_offset resulting offset in the returned pbuf
 * @return the pbuf in the queue where the offset is
 */
struct pbuf*
pbuf_skip(struct pbuf* in, u16_t in_offset, u16_t* out_offset)
{
  const struct pbuf* out = pbuf_skip_const(in, in_offset, out_offset);
  return LWIP_CONST_CAST(struct pbuf*, out);
}

/**
 * @ingroup pbuf
 * Copy application supplied data into a pbuf.
 * This function can only be used to copy the equivalent of buf->tot_len data.
 *
 * @param buf pbuf to fill with data
 * @param dataptr application supplied data buffer
 * @param len length of the application supplied data buffer
 *
 * @return ERR_OK if successful, ERR_MEM if the pbuf is not big enough
 */
err_t
pbuf_take(struct pbuf *buf, const void *dataptr, u16_t len)
{
  struct pbuf *p;
  u16_t buf_copy_len;
  u16_t total_copy_len = len;
  u16_t copied_total = 0;

  LWIP_ERROR("pbuf_take: invalid buf", (buf != NULL), return ERR_ARG;);
  LWIP_ERROR("pbuf_take: invalid dataptr", (dataptr != NULL), return ERR_ARG;);
  LWIP_ERROR("pbuf_take: buf not large enough", (buf->tot_len >= len), return ERR_MEM;);

  if ((buf == NULL) || (dataptr == NULL) || (buf->tot_len < len)) {
    return ERR_ARG;
  }

  /* Note some systems use byte copy if dataptr or one of the pbuf payload pointers are unaligned. */
  for (p = buf; total_copy_len != 0; p = p->next) {
    LWIP_ASSERT("pbuf_take: invalid pbuf", p != NULL);
    buf_copy_len = total_copy_len;
    if (buf_copy_len > p->len) {
      /* this pbuf cannot hold all remaining data */
      buf_copy_len = p->len;
    }
    /* copy the necessary parts of the buffer */
    MEMCPY(p->payload, &((const char*)dataptr)[copied_total], buf_copy_len);
    total_copy_len -= buf_copy_len;
    copied_total += buf_copy_len;
  }
  LWIP_ASSERT("did not copy all data", total_copy_len == 0 && copied_total == len);
  return ERR_OK;
}

/**
 * @ingroup pbuf
 * Same as pbuf_take() but puts data at an offset
 *
 * @param buf pbuf to fill with data
 * @param dataptr application supplied data buffer
 * @param len length of the application supplied data buffer
 * @param offset offset in pbuf where to copy dataptr to
 *
 * @return ERR_OK if successful, ERR_MEM if the pbuf is not big enough
 */
err_t
pbuf_take_at(struct pbuf *buf, const void *dataptr, u16_t len, u16_t offset)
{
  u16_t target_offset;
  struct pbuf* q = pbuf_skip(buf, offset, &target_offset);

  /* return requested data if pbuf is OK */
  if ((q != NULL) && (q->tot_len >= target_offset + len)) {
    u16_t remaining_len = len;
    const u8_t* src_ptr = (const u8_t*)dataptr;
    /* copy the part that goes into the first pbuf */
    u16_t first_copy_len = LWIP_MIN(q->len - target_offset, len);
    MEMCPY(((u8_t*)q->payload) + target_offset, dataptr, first_copy_len);
    remaining_len -= first_copy_len;
    src_ptr += first_copy_len;
    if (remaining_len > 0) {
      return pbuf_take(q->next, src_ptr, remaining_len);
    }
    return ERR_OK;
  }
  return ERR_MEM;
}

/**
 * @ingroup pbuf
 * Creates a single pbuf out of a queue of pbufs.
 *
 * @remark: Either the source pbuf 'p' is freed by this function or the original
 *          pbuf 'p' is returned, therefore the caller has to check the result!
 *
 * @param p the source pbuf
 * @param layer pbuf_layer of the new pbuf
 *
 * @return a new, single pbuf (p->next is NULL)
 *         or the old pbuf if allocation fails
 */
struct pbuf*
pbuf_coalesce(struct pbuf *p, pbuf_layer layer)
{
  struct pbuf *q;
  err_t err;
  if (p->next == NULL) {
    return p;
  }
  q = pbuf_alloc(layer, p->tot_len, PBUF_RAM);
  if (q == NULL) {
    /* @todo: what do we do now? */
    return p;
  }
  err = pbuf_copy(q, p);
  LWIP_UNUSED_ARG(err); /* in case of LWIP_NOASSERT */
  LWIP_ASSERT("pbuf_copy failed", err == ERR_OK);
  pbuf_free(p);
  return q;
}

#if LWIP_CHECKSUM_ON_COPY
/**
 * Copies data into a single pbuf (*not* into a pbuf queue!) and updates
 * the checksum while copying
 *
 * @param p the pbuf to copy data into
 * @param start_offset offset of p->payload where to copy the data to
 * @param dataptr data to copy into the pbuf
 * @param len length of data to copy into the pbuf
 * @param chksum pointer to the checksum which is updated
 * @return ERR_OK if successful, another error if the data does not fit
 *         within the (first) pbuf (no pbuf queues!)
 */
err_t
pbuf_fill_chksum(struct pbuf *p, u16_t start_offset, const void *dataptr,
                 u16_t len, u16_t *chksum)
{
  u32_t acc;
  u16_t copy_chksum;
  char *dst_ptr;
  LWIP_ASSERT("p != NULL", p != NULL);
  LWIP_ASSERT("dataptr != NULL", dataptr != NULL);
  LWIP_ASSERT("chksum != NULL", chksum != NULL);
  LWIP_ASSERT("len != 0", len != 0);

  if ((start_offset >= p->len) || (start_offset + len > p->len)) {
    return ERR_ARG;
  }

  dst_ptr = ((char*)p->payload) + start_offset;
  copy_chksum = LWIP_CHKSUM_COPY(dst_ptr, dataptr, len);
  if ((start_offset & 1) != 0) {
    copy_chksum = SWAP_BYTES_IN_WORD(copy_chksum);
  }
  acc = *chksum;
  acc += copy_chksum;
  *chksum = FOLD_U32T(acc);
  return ERR_OK;
}
#endif /* LWIP_CHECKSUM_ON_COPY */

/**
 * @ingroup pbuf
 * Get one byte from the specified position in a pbuf
 * WARNING: returns zero for offset >= p->tot_len
 *
 * @param p pbuf to parse
 * @param offset offset into p of the byte to return
 * @return byte at an offset into p OR ZERO IF 'offset' >= p->tot_len
 */
u8_t
pbuf_get_at(const struct pbuf* p, u16_t offset)
{
  int ret = pbuf_try_get_at(p, offset);
  if (ret >= 0) {
    return (u8_t)ret;
  }
  return 0;
}

/**
 * @ingroup pbuf
 * Get one byte from the specified position in a pbuf
 *
 * @param p pbuf to parse
 * @param offset offset into p of the byte to return
 * @return byte at an offset into p [0..0xFF] OR negative if 'offset' >= p->tot_len
 */
int
pbuf_try_get_at(const struct pbuf* p, u16_t offset)
{
  u16_t q_idx;
  const struct pbuf* q = pbuf_skip_const(p, offset, &q_idx);

  /* return requested data if pbuf is OK */
  if ((q != NULL) && (q->len > q_idx)) {
    return ((u8_t*)q->payload)[q_idx];
  }
  return -1;
}

/**
 * @ingroup pbuf
 * Put one byte to the specified position in a pbuf
 * WARNING: silently ignores offset >= p->tot_len
 *
 * @param p pbuf to fill
 * @param offset offset into p of the byte to write
 * @param data byte to write at an offset into p
 */
void
pbuf_put_at(struct pbuf* p, u16_t offset, u8_t data)
{
  u16_t q_idx;
  struct pbuf* q = pbuf_skip(p, offset, &q_idx);

  /* write requested data if pbuf is OK */
  if ((q != NULL) && (q->len > q_idx)) {
    ((u8_t*)q->payload)[q_idx] = data;
  }
}

/**
 * @ingroup pbuf
 * Compare pbuf contents at specified offset with memory s2, both of length n
 *
 * @param p pbuf to compare
 * @param offset offset into p at which to start comparing
 * @param s2 buffer to compare
 * @param n length of buffer to compare
 * @return zero if equal, nonzero otherwise
 *         (0xffff if p is too short, diffoffset+1 otherwise)
 */
u16_t
pbuf_memcmp(const struct pbuf* p, u16_t offset, const void* s2, u16_t n)
{
  u16_t start = offset;
  const struct pbuf* q = p;
  u16_t i;
 
  /* pbuf long enough to perform check? */
  if(p->tot_len < (offset + n)) {
    return 0xffff;
  }
 
  /* get the correct pbuf from chain. We know it succeeds because of p->tot_len check above. */
  while ((q != NULL) && (q->len <= start)) {
    start -= q->len;
    q = q->next;
  }
 
  /* return requested data if pbuf is OK */
  for (i = 0; i < n; i++) {
    /* We know pbuf_get_at() succeeds because of p->tot_len check above. */
    u8_t a = pbuf_get_at(q, start + i);
    u8_t b = ((const u8_t*)s2)[i];
    if (a != b) {
      return i+1;
    }
  }
  return 0;
}

/**
 * @ingroup pbuf
 * Find occurrence of mem (with length mem_len) in pbuf p, starting at offset
 * start_offset.
 *
 * @param p pbuf to search, maximum length is 0xFFFE since 0xFFFF is used as
 *        return value 'not found'
 * @param mem search for the contents of this buffer
 * @param mem_len length of 'mem'
 * @param start_offset offset into p at which to start searching
 * @return 0xFFFF if substr was not found in p or the index where it was found
 */
u16_t
pbuf_memfind(const struct pbuf* p, const void* mem, u16_t mem_len, u16_t start_offset)
{
  u16_t i;
  u16_t max = p->tot_len - mem_len;
  if (p->tot_len >= mem_len + start_offset) {
    for (i = start_offset; i <= max; i++) {
      u16_t plus = pbuf_memcmp(p, i, mem, mem_len);
      if (plus == 0) {
        return i;
      }
    }
  }
  return 0xFFFF;
}

/**
 * Find occurrence of substr with length substr_len in pbuf p, start at offset
 * start_offset
 * WARNING: in contrast to strstr(), this one does not stop at the first \0 in
 * the pbuf/source string!
 *
 * @param p pbuf to search, maximum length is 0xFFFE since 0xFFFF is used as
 *        return value 'not found'
 * @param substr string to search for in p, maximum length is 0xFFFE
 * @return 0xFFFF if substr was not found in p or the index where it was found
 */
u16_t
pbuf_strstr(const struct pbuf* p, const char* substr)
{
  size_t substr_len;
  if ((substr == NULL) || (substr[0] == 0) || (p->tot_len == 0xFFFF)) {
    return 0xFFFF;
  }
  substr_len = strlen(substr);
  if (substr_len >= 0xFFFF) {
    return 0xFFFF;
  }
  return pbuf_memfind(p, substr, (u16_t)substr_len, 0);
}
