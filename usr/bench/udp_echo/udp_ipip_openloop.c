/*
 * Copyright (c) 2014, University of Washington.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich. 
 * Attn: Systems Group.
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <netdb.h>
#include <inttypes.h>
#include <sys/types.h> 
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/time.h>
#include <strings.h>
#include <assert.h>
#include <pthread.h>
#include <linux/if_ether.h>
#include <sys/ioctl.h>
#define __USE_MISC
#include <net/if.h>
#include <netpacket/packet.h>
#include <net/ethernet.h>

#define IP_PROTO_IPENCAP 4
#define IP_PROTO_UDP     17

#define IP_HLEN 20

#ifndef BARRELFISH
struct ip_addr_packed {
  uint32_t addr;
} __attribute__ ((packed));

struct ip_addr {
  uint32_t addr;
};

typedef struct ip_addr ip_addr_t;
typedef struct ip_addr_packed ip_addr_p_t;
#endif

#define TCP_HLEN 20
#define ip4_addr_get_u32(src_ipaddr) ((src_ipaddr)->addr)

struct ip_hdr {
  /* version / header length */
  uint8_t _v_hl;
  /* type of service */
  uint8_t _tos;
  /* total length */
  uint16_t _len;
  /* identification */
  uint16_t _id;
  /* fragment offset field */
  uint16_t _offset;
#define IP_RF 0x8000U        /* reserved fragment flag */
#define IP_DF 0x4000U        /* dont fragment flag */
#define IP_MF 0x2000U        /* more fragments flag */
#define IP_OFFMASK 0x1fffU   /* mask for fragmenting bits */
  /* time to live */
  uint8_t _ttl;
  /* protocol*/
  uint8_t _proto;
  /* checksum */
  uint16_t _chksum;
  /* source and destination IP addresses */
  ip_addr_p_t src;
  ip_addr_p_t dest;
} __attribute__ ((packed));

#define IPH_V(hdr)  ((hdr)->_v_hl >> 4)
#define IPH_HL(hdr) ((hdr)->_v_hl & 0x0f)
#define IPH_TOS(hdr) ((hdr)->_tos)
#define IPH_LEN(hdr) ((hdr)->_len)
#define IPH_ID(hdr) ((hdr)->_id)
#define IPH_OFFSET(hdr) ((hdr)->_offset)
#define IPH_TTL(hdr) ((hdr)->_ttl)
#define IPH_PROTO(hdr) ((hdr)->_proto)
#define IPH_CHKSUM(hdr) ((hdr)->_chksum)

#define IPH_VHL_SET(hdr, v, hl) (hdr)->_v_hl = (((v) << 4) | (hl))
#define IPH_TOS_SET(hdr, tos) (hdr)->_tos = (tos)
#define IPH_LEN_SET(hdr, len) (hdr)->_len = (len)
#define IPH_ID_SET(hdr, id) (hdr)->_id = (id)
#define IPH_OFFSET_SET(hdr, off) (hdr)->_offset = (off)
#define IPH_TTL_SET(hdr, ttl) (hdr)->_ttl = (u8_t)(ttl)
#define IPH_PROTO_SET(hdr, proto) (hdr)->_proto = (u8_t)(proto)
#define IPH_CHKSUM_SET(hdr, chksum) (hdr)->_chksum = (chksum)

struct udp_hdr {
  uint16_t src;
  uint16_t dest;  /* src/dest UDP ports */
  uint16_t len;
  uint16_t chksum;
} __attribute__ ((packed));

#define	timersub(a, b, result)						      \
  do {									      \
    (result)->tv_sec = (a)->tv_sec - (b)->tv_sec;			      \
    (result)->tv_usec = (a)->tv_usec - (b)->tv_usec;			      \
    if ((result)->tv_usec < 0) {					      \
      --(result)->tv_sec;						      \
      (result)->tv_usec += 1000000;					      \
    }									      \
  } while (0)

#define BUFSIZE         1024
#define INBUFSIZE	2048
#define MAX_ROUNDS      10000000

#define MIN(a,b)        ((a) < (b) ? (a) : (b))
#define MAX(a,b)        ((a) > (b) ? (a) : (b))

static struct timeval tvs[MAX_ROUNDS], tst[MAX_ROUNDS];

/*
 * error - wrapper for perror
 */
static void error(char *msg) {
  perror(msg);
  exit(1);
}

static int sockfd; /* socket */
static struct sockaddr_ll clientaddr; /* client addr */
static socklen_t clientlen; /* byte size of client's address */
static int delay = 0;
static size_t rounds, packets = 0;
static uint8_t servmac[6] = "\xa0\x36\x9f\x10\x00\xa0";

static void *receiver_func(void *unused)
{
  char buf[INBUFSIZE]; /* message buf */

  for(int i = 0; i < rounds; i++) {
    /*
     * recvfrom: receive a UDP datagram from a client
     */
    struct sockaddr_ll recvaddr;
    int n;

    for(;;) {
      int socklen = sizeof(recvaddr);
      n = recvfrom(sockfd, buf, INBUFSIZE, 0, (struct sockaddr *)&recvaddr, &socklen);
      if (n < 0)
	error("ERROR in recvfrom");

      /* printf("socklen = %d, recvaddr = %d, halen = %d\n", socklen, sizeof(recvaddr), recvaddr.sll_halen); */
      //      assert(socklen == sizeof(recvaddr));
      if(recvaddr.sll_halen == 6 &&
	 !memcmp(recvaddr.sll_addr, servmac, 6)) {
	break;
      }
    }

    /* printf("server received %d bytes\n", n); */
    /* assert(n == BUFSIZE); */

    struct ip_hdr *outer_iphdr = (void *)buf;
    if(IPH_PROTO(outer_iphdr) == IP_PROTO_IPENCAP) {
      /* printf("IPIP packet\n"); */

      struct timeval *tstamp = (struct timeval *)&buf[40 + 8 + 8];
      gettimeofday(&tst[i], NULL);
      timersub(&tst[i], tstamp, &tvs[i]);
    } else {
      assert(IPH_PROTO(outer_iphdr) == IP_PROTO_UDP);
      /* printf("Decapsulated packet\n"); */

      struct timeval *tstamp = (struct timeval *)&buf[20 + 8 + 8];
      gettimeofday(&tst[i], NULL);
      timersub(&tst[i], tstamp, &tvs[i]);
    }

    /* uint64_t *cnt = (uint64_t *)buf; */
    packets++;
    /* if(*cnt != i) { */
    /*     printf("Packets reordered? %d != %" PRIu64 "\n", i, *cnt); */
    /*     exit(1); */
    /* } */
  }

  return NULL;
}

static uint16_t
lwip_standard_chksum(void *dataptr, uint16_t len)
{
  uint32_t acc;
  uint16_t src;
  uint8_t *octetptr;

  acc = 0;
  /* dataptr may be at odd or even addresses */
  octetptr = (uint8_t*)dataptr;
  while (len > 1) {
    /* declare first octet as most significant
       thus assume network order, ignoring host order */
    src = (*octetptr) << 8;
    octetptr++;
    /* declare second octet as least significant */
    src |= (*octetptr);
    octetptr++;
    acc += src;
    len -= 2;
  }
  if (len > 0) {
    /* accumulate remaining octet */
    src = (*octetptr) << 8;
    acc += src;
  }
  /* add deferred carry bits */
  acc = (acc >> 16) + (acc & 0x0000ffffUL);
  if ((acc & 0xffff0000UL) != 0) {
    acc = (acc >> 16) + (acc & 0x0000ffffUL);
  }
  /* This maybe a little confusing: reorder sum using htons()
     instead of ntohs() since it has a little less call overhead.
     The caller must invert bits for Internet sum ! */
  return htons((uint16_t)acc);
}

static uint16_t
inet_chksum(void *dataptr, uint16_t len)
{
  return ~lwip_standard_chksum(dataptr, len);
}

int main(int argc, char **argv) {
  int portno; /* port to listen on */
  int optval; /* flag value for setsockopt */
  char buf[BUFSIZE]; /* message buf */

  /* 
   * check command line arguments 
   */
  if (argc < 9) {
    fprintf(stderr, "usage: %s <port> <server IP> <delay us> <rounds> <start_val> <my_ip> <iface_name> <ttl=1_prob>\n", argv[0]);
    exit(1);
  }
  portno = atoi(argv[1]);
  delay = atoi(argv[3]);
  rounds = atoi(argv[4]);
  float ttlprob = atof(argv[8]);
  assert(rounds < MAX_ROUNDS);
  size_t start = atoi(argv[5]);

  // Get my source IP address from commandline
  in_addr_t srcaddr = inet_addr(argv[2]);
  ip_addr_t srcipaddr = {
    .addr = srcaddr,
  };
  in_addr_t srvaddr = inet_addr(argv[6]);

  /* 
   * socket: create the parent socket 
   */
  sockfd = socket(AF_PACKET, SOCK_DGRAM, htons(ETH_P_IP));
  if (sockfd < 0)
    error("ERROR opening socket");

  struct ifreq ifreq;

  for(int i = 0; i < 100; i++) {
    memset(&ifreq, 0, sizeof(struct ifreq));
    ifreq.ifr_ifindex = i;
    /* struct sockaddr_in sa; */
    /* memset(&sa, 0, sizeof(struct sockaddr_in)); */
    /* sa.sin_family = AF_INET; */
    /* /\* sa.sin_addr.s_addr = 0x80d006ff; *\/ */
    /* sa.sin_addr.s_addr = 0xff06d080; */
    /* memcpy(&ifreq.ifr_broadaddr, &sa, sizeof(struct sockaddr_in)); */

    int r = ioctl(sockfd, SIOCGIFNAME, &ifreq);
    if(r != 0) {
      continue;
    }
    assert(r == 0);

    if(!strcmp(ifreq.ifr_name, argv[7])) {
      printf("Found index = %d, name = '%s'\n", ifreq.ifr_ifindex, ifreq.ifr_name);
      break;
    }
  }

  static uint8_t clientmac[6] = "\xa0\x36\x9f\x10\x00\xa2";
  clientlen = sizeof(clientaddr);
  bzero((char *) &clientaddr, sizeof(clientaddr));
  clientaddr.sll_family = AF_PACKET;
  clientaddr.sll_protocol = htons(ETH_P_IP);
  clientaddr.sll_ifindex = ifreq.ifr_ifindex;
  clientaddr.sll_halen = 6;
  memcpy(&clientaddr.sll_addr, clientmac, 6);

  pthread_t sender;
  int ret = pthread_create(&sender, NULL, receiver_func, NULL);
  assert(ret == 0);

  struct ip_hdr outer_iphdr = {
    ._v_hl = 69,
    ._tos = 0,
    ._len = htons(IP_HLEN + IP_HLEN + sizeof(struct udp_hdr) + BUFSIZE),
    ._id = htons(3),
    ._offset = 0,
    ._ttl = 6,
    ._proto = IP_PROTO_IPENCAP,
    //    ._chksum = htons(0xa1e0),
    ._chksum = 0,
    .src.addr = srvaddr,
    .dest.addr = srcaddr,
  };

  // Calculate outer_iphdr checksum
  IPH_CHKSUM_SET(&outer_iphdr, inet_chksum(&outer_iphdr, IP_HLEN));

  struct ip_hdr outer_iphdr2 = {
    ._v_hl = 69,
    ._tos = 0,
    ._len = htons(IP_HLEN + IP_HLEN + sizeof(struct udp_hdr) + BUFSIZE),
    ._id = htons(3),
    ._offset = 0,
    ._ttl = 1,
    ._proto = IP_PROTO_IPENCAP,
    /* ._chksum = htons(0xa6e0), */
    ._chksum = 0,
    .src.addr = srvaddr,
    .dest.addr = srcaddr,
  };

  // Calculate outer_iphdr2 checksum
  IPH_CHKSUM_SET(&outer_iphdr2, inet_chksum(&outer_iphdr2, IP_HLEN));

  struct ip_hdr inner_iphdr = {
    ._v_hl = 69,
    ._tos = 0,
    ._len = htons(IP_HLEN + sizeof(struct udp_hdr) + BUFSIZE),
    ._id = htons(3),
    ._offset = 0,
    ._ttl = 64,
    ._proto = IP_PROTO_UDP,
    //    ._chksum = htons(0x67e7),
    ._chksum = htons(0),
    .src.addr = srcaddr,
    .dest.addr = srvaddr,
  };

  struct udp_hdr udphdr = {
    .src = htons(1234),
    .dest = htons(1234),
    .len = htons(1024 + sizeof(struct udp_hdr)),
    .chksum = 0,
  };

  uint64_t *cnt = (uint64_t *)buf;
  struct timeval *tstamp = (struct timeval *)&buf[8];
  memset(tstamp, 0, sizeof(struct timeval));

  for(*cnt = 0; *cnt < rounds; (*cnt)++) {
      struct timeval oldstamp, diff;

      oldstamp = *tstamp;

      do {
          gettimeofday(tstamp, NULL);
          timersub(tstamp, &oldstamp, &diff);
          /* printf("now = %lu, oldstamp = %lu, diff = %lu, delay = %d\n", */
          /*        tstamp->tv_sec * 1000000 + tstamp->tv_usec, */
          /*        oldstamp.tv_sec * 1000000 + oldstamp.tv_usec, */
          /*        diff.tv_sec * 1000000 + diff.tv_usec, delay); */
      } while((uint64_t)(diff.tv_sec * 1000000 + diff.tv_usec) < (uint64_t)delay);

      struct iovec out_iovec[4] = {
	{
	  .iov_base = &outer_iphdr,
	  .iov_len = sizeof(struct ip_hdr),
	},
	{
	  .iov_base = &inner_iphdr,
	  .iov_len = sizeof(struct ip_hdr),
	},
	{
	  .iov_base = &udphdr,
	  .iov_len = sizeof(udphdr),
	},
	{
	  .iov_base = buf,
	  .iov_len = BUFSIZE,
	},
      };
      struct msghdr outhdr = {
	.msg_name = &clientaddr,
	.msg_namelen = clientlen,
	.msg_iov = out_iovec,
	.msg_iovlen = 4,
	.msg_flags = 0,
      };

      if((float)rand() / (float)RAND_MAX < ttlprob) {
	out_iovec[0].iov_base = &outer_iphdr2;
      }

      int n = sendmsg(sockfd, &outhdr, 0);
      if (n < 0)
          error("ERROR in sendto");
  }

  ret = pthread_cancel(sender);
  assert(ret == 0);

  unsigned long sum = 0, min = 99999, max = 0;

  for(int i = 0; i < packets; i++) {
      unsigned long r = tvs[i].tv_sec * 1000000 + tvs[i].tv_usec;
      unsigned long t = tst[i].tv_sec * 1000000 + tst[i].tv_usec;
      if(t != 0) {
          printf("%zd %lu %lu %lu us\n", i + start, t - r, t, r);
          sum += r;
          min = MIN(min, r);
          max = MAX(max, r);
      }
  }

  printf("average %lu us, min %lu us, max %lu us\n",
         sum / packets, min, max);

  return 0;
}
