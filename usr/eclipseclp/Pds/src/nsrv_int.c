/* BEGIN LICENSE BLOCK
 * Version: CMPL 1.1
 *
 * The contents of this file are subject to the Cisco-style Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file except
 * in compliance with the License.  You may obtain a copy of the License
 * at www.eclipse-clp.org/license.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
 * the License for the specific language governing rights and limitations
 * under the License. 
 * 
 * The Original Code is  The ECLiPSe Constraint Logic Programming System. 
 * The Initial Developer of the Original Code is  Cisco Systems, Inc. 
 * Portions created by the Initial Developer are
 * Copyright (C) 1994-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): Kees Schuerman, ECRC
 * 
 * END LICENSE BLOCK */
/**********************************************************************
**      System: Parallel Distributed System
**        File: nsrv_int.c
**      Author: Kees Schuerman
**      SccsId: "@(#)nsrv_int.c	1.26 24 Nov 1995"
** Description: Name Service Internals 
***********************************************************************/

#include "machine.h"     /* architecture specific constant definitions */

#include <sys/types.h>
#include <sys/socket.h>
/*
#include <sys/uio.h>
*/
#include <sys/stat.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <rpc/rpc.h>
#include <fcntl.h>

#include "pds.types.h"
#include "pds.error.h"
#include "pds.mem.h"
#include "pds.mdt.h"
#include "pds.xdr.h"
#include "bmsg.msg.h"
#include "bmsg.xdr.h"
#include "amsg.msg.h"
#include "amsg.xdr.h"
#include "nsrv.h"
#include "nsrv.xdr.h"
#include "nsrv_int.h"


/**********************************************************************
** Type Definitions
***********************************************************************/

typedef pds_uint32 nsrv_id_t;

typedef struct ht_item {
    struct ht_item * next;
    void_ptr key;
    nsrv_name_t signature;
    void_ptr data;
} ht_item_t;

typedef struct {	    /* htd: hash table descriptor  */
    g_mutex_t mutex;	    /* protects hash table	   */
    unsigned int size;      /* size of hash table	   */
    ht_item_t * * item;     /* hash table		   */
    unsigned int count;	    /* # items in hash table  	   */
} htd_t;

#define HTD_Init(htd)   g_mutex_init(&(htd)->mutex)
#define HTD_Lock(htd)   g_mutex_lock(&(htd)->mutex)
#define HTD_Unlock(htd) g_mutex_unlock(&(htd)->mutex)

typedef struct {
    htd_t * domain;
    htd_t * bport;
    htd_t * aport;
    htd_t * domain_id;
    htd_t * bport_id;
} nsrv_t;

#define KEY_HT_SIZE             31
#define DOMAIN_HT_SIZE          31
#define BPORT_HT_SIZE           31
#define APORT_HT_SIZE           67
#define DOMAIN_ID_HT_SIZE       31
#define BPORT_ID_HT_SIZE        37



/**********************************************************************
** Global Variables
***********************************************************************/

static pds_heap_descriptor_t nsrv_shd;	/* shared heap descriptor      */
char * nsrv_data_start = (char *) 0; 	/* start of (shared) data area */
char * nsrv_msg_start = (char *) 0; 	/* start of (shared) msg area  */

static nsrv_t * * nsrv;			/* htd: hash table descriptors */
static htd_t * domain_htd;		/* domain htd cache	       */
static htd_t * bport_htd;		/* bport htd cache             */
static htd_t * aport_htd;		/* htd aport cache             */
static htd_t * domain_id_htd;		/* domain_id htd cache         */
static htd_t * bport_id_htd;		/* bport_id htd cache          */

static int nsrv_verbose = 0;

msg_type_t MDT_NSRVNAME;
msg_type_t MDT_NSRVVERSION;

msg_type_t MDT_APORT_REGISTER_REQUEST;
msg_type_t MDT_BPORT_REGISTER_REQUEST;
msg_type_t MDT_BDOMAIN_REGISTER_REQUEST;
msg_type_t MDT_DEREGISTER_REQUEST;
msg_type_t MDT_LOOK_UP_REQUEST;
msg_type_t MDT_BPORTID_REQUEST;
msg_type_t MDT_BDOMAINID_REQUEST;
msg_type_t MDT_VERSION_REQUEST;

msg_type_t MDT_SIMPLE_REPLY;
msg_type_t MDT_APORT_REPLY;
msg_type_t MDT_BPORT_REPLY;
msg_type_t MDT_BDOMAIN_REPLY;
msg_type_t MDT_BPORTID_REPLY;
msg_type_t MDT_BDOMAINID_REPLY;
msg_type_t MDT_VERSION_REPLY;



/**********************************************************************
** Debugging Support
***********************************************************************/

#if !defined(NDEBUG)

static void
print_bport_id_hash_table()
{
    ht_item_t * item;
    int i;

    printf("\n\n");
    printf("Bport Identifier Hash Table\n");
    printf("===========================\n");
    printf("\n");
    for (i=0;i<bport_id_htd->size;i++) {
	item = bport_id_htd->item[i];
	while (item) {
	    printf("      key: %d \n", * (int *) item->key);
	    printf("signature: %s \n", item->signature);
	    printf(" bport_id: 0x%x \n", * (int *) item->data);
	    item = item->next;
	    printf("\n");
	}
    }
}


static void
print_domain_id_hash_table()
{
    ht_item_t * item;
    int i;

    printf("\n\n");
    printf("Domain Identifier Hash Table\n");
    printf("============================\n");
    printf("\n");
    for (i=0;i<domain_id_htd->size;i++) {
	item = domain_id_htd->item[i];
	while (item) {
	    printf("       key: %d \n", * (int *) item->key);
	    printf(" signature: %s \n", item->signature);
	    printf("bdomain_id: 0x%x \n", * (int *) item->data);
	    item = item->next;
	    printf("\n");
	}
    }
}


static void
print_bport_hash_table()
{
    htd_t * htd;
    ht_item_t * item_0;
    ht_item_t * item_1;
    int i,j;

    printf("\n\n");
    printf("Bport Hash Table\n");
    printf("================\n");
    printf("\n");
    for (i=0;i<bport_htd->size;i++) {
	item_0 = bport_htd->item[i];
	if (item_0)
	    htd = (htd_t *) item_0->data;
	else
	    htd = (htd_t *) 0;
	while (htd) {
	    for (j=0;j<htd->size;j++) {
	        item_1 = htd->item[j];
		while (item_1) {
	            printf("                     key: %s \n", 
			    (char *) item_1->key);
	            printf("               signature: %s \n", 
			    item_1->signature);
	            printf("              bport.bpid: %d \n", 
		            ((bport_t *) item_1->data)->bpid);
	            printf("          bport.bport_id: 0x%x \n", 
		            ((bport_t *) item_1->data)->bport_id);
	            printf("        bport.bdomain_id: 0x%x \n", 
		            ((bport_t *) item_1->data)->bdomain_id);
	            printf("bport.bmsg_queue_address: 0x%lx \n", 
		            ((bport_t *) item_1->data)->bmsg_queue_address);
	            printf("      bport.bnet_address: %s \n", 
		            ((bport_t *) item_1->data)->bnet_address);
	            printf("      bport.bport_number: %u \n", 
			    ((bport_t *) item_1->data)->bport_number);
	            item_1 = item_1->next;
		    printf("\n");
	        }
	    }
	    item_0 = item_0->next;
	    if (item_0)
		htd = (htd_t *) item_0->data;
	    else
		htd = (htd_t *) 0;
	}
    }
}


static void
print_domain_hash_table()
{
    htd_t * htd;
    ht_item_t * item_0;
    ht_item_t * item_1;
    int i,j;

    printf("\n\n");
    printf("Domain Hash Table\n");
    printf("=================\n");
    printf("\n");
    for (i=0;i<domain_htd->size;i++) {
	item_0 = domain_htd->item[i];
	if (item_0)
	    htd = (htd_t *) item_0->data;
	else
	    htd = (htd_t *) 0;
	while (htd) {
	    for (j=0;j<htd->size;j++) {
	        item_1 = htd->item[j];
		while (item_1) {
	            printf("                     key: %s \n", 
			    (char *) item_1->key);
	            printf("               signature: %s \n", 
			    item_1->signature);
	            printf("      bdomain.bdomain_id: 0x%x \n", 
		            ((bdomain_t *) item_1->data)->bdomain_id);
	            printf("    bdomain.bdomain_file: %s \n", 
		            ((bdomain_t *) item_1->data)->bdomain_file);
	            printf("   bdomain.bdomain_start: 0x%lx \n", 
		            ((bdomain_t *) item_1->data)->bdomain_start);
	            printf("    bdomain.bdomain_size: 0x%x \n", 
		            ((bdomain_t *) item_1->data)->bdomain_size);
	            item_1 = item_1->next;
		    printf("\n");
	        }
	    }
	    item_0 = item_0->next;
	    if (item_0)
		htd = (htd_t *) item_0->data;
	    else
		htd = (htd_t *) 0;
	}
    }
}


static void
print_aport_hash_table()
{
    htd_t * htd;
    ht_item_t * item_0;
    ht_item_t * item_1;
    int i,j;

    printf("\n\n");
    printf("Aport Hash Table\n");
    printf("================\n");
    printf("\n");
    for (i=0;i<aport_htd->size;i++) {
	item_0 = aport_htd->item[i];
	if (item_0)
	    htd = (htd_t *) item_0->data;
	else
	    htd = (htd_t *) 0;
	while (htd) {
	    for (j=0;j<htd->size;j++) {
	        item_1 = htd->item[j];
		while (item_1) {
	            printf("                     key: %s \n", 
			    (char *) item_1->key);
	            printf("               signature: %s \n", 
			    item_1->signature);
	            printf("          aport.aport_id: 0x%x \n", 
		            ((aport_t *) item_1->data)->aport_id);
	            printf("          aport.bport_id: 0x%x \n", 
		            ((aport_t *) item_1->data)->bport_id);
	            printf("        aport.bdomain_id: 0x%x \n", 
		            ((aport_t *) item_1->data)->bdomain_id);
	            item_1 = item_1->next;
		    printf("\n");
	        }
	    }
	    item_0 = item_0->next;
	    if (item_0)
		htd = (htd_t *) item_0->data;
	    else
		htd = (htd_t *) 0;
	}
    }
}

#endif /* NDEBUG */



static void
print_nsrv_retcode(nret)
    nsrv_ret_t nret;
{
    switch (nret) {
	case NSRV_OK :
	    printf("NSRV_OK");
	    break;
	case NSRV_NYI :
	    printf("NSRV_NYI");
	    break;
	case NSRV_WARN :
	    printf("NSRV_WARN");
	    break;
	case NSRV_ERROR :
	    printf("NSRV_ERROR");
	    break;
	case NSRV_IMPLIM :
	    printf("NSRV_IMPLIM");
	    break;
	case NSRV_INVAL :
	    printf("NSRV_INVAL");
	    break;
	case NSRV_NORESOURCES :
	    printf("NSRV_NORESOURCES");
	    break;
	case NSRV_NOMEMORY :
	    printf("NSRV_NOMEMORY");
	    break;
	case NSRV_NOPORT :
	    printf("NSRV_NOPORT");
	    break;
	case NSRV_NOT_READY :
	    printf("NSRV_NOT_READY");
	    break;
	case NSRV_NOT_YOURS :
	    printf("NSRV_NOT_YOURS");
	    break;
	case NSRV_NOT_REGISTERED :
	    printf("NSRV_NOT_REGISTERED");
	    break;
	case NSRV_NODOMAIN :
	    printf("NSRV_NODOMAIN");
	    break;
	case NSRV_NOSERVER :
	    printf("NSRV_NOSERVER");
	    break;
	case NSRV_NOHOST :
	    printf("NSRV_NOHOST");
	    break;
	case NSRV_EVERSION :
	    printf("NSRV_EVERSION");
	    break;
	default :
	    printf("NSRV_???");
	    break;
    }
    printf("\n");
}



/**********************************************************************
** Basic Hash Table Primitives
***********************************************************************/

static nsrv_ret_t
ht_create(size,htd)
    unsigned int size;
    htd_t * * htd;
{
    int i;

    *htd = (htd_t *)
           pds_mem_alloc_size(&nsrv_shd,sizeof(htd_t));
    if (!*htd)
        return(NSRV_NOMEMORY);

    (*htd)->item = (ht_item_t * *)
                   pds_mem_alloc_size(&nsrv_shd,size * sizeof(ht_item_t *));
    if (!(*htd)->item) {
	pds_mem_free_size(&nsrv_shd,(void_ptr) *htd,sizeof(htd_t));
        return(NSRV_NOMEMORY);
    }

    for (i=0; i<size; i++) {
        (*htd)->item[i] = (ht_item_t *) 0;
    }

    (*htd)->size = size;
    (*htd)->count = 0;

    HTD_Init(*htd);

    return(NSRV_OK);
}


static void
ht_destroy(htd)
    htd_t * htd;
{
    nsrv_assert(htd->count == 0);
    pds_mem_free_size(&nsrv_shd,(void_ptr) htd->item,htd->size * sizeof(ht_item_t *));
    pds_mem_free_size(&nsrv_shd,(void_ptr) htd,sizeof(htd_t));
}


static nsrv_ret_t
ht_look_up(htd,key,index,match,item)
    htd_t * htd;
    void_ptr key;
    pds_uint32 (* index) ();
    int (* match) ();
    ht_item_t * * * item;
{
    pds_uint32 indx;
    ht_item_t * item_curr;
    ht_item_t * item_prev;

    indx = index(htd,key);

    item_curr = htd->item[indx];
    item_prev = (ht_item_t *) 0;
    while (item_curr && (!match(key,item_curr->key))) {
	item_prev = item_curr;
	item_curr = item_curr->next;
    }

    if (item_prev) 
	*item = &item_prev->next;
    else
        *item = &htd->item[indx];

    if (item_curr)
	return(NSRV_OK);
    else
	return(NSRV_NOT_REGISTERED);
}


static nsrv_ret_t
ht_delete(htd,key,signature,index,match,delete)
    htd_t * htd;
    void_ptr key;
    nsrv_name_t signature;
    pds_uint32 (* index) ();
    int (* match) ();
    void (* delete) ();
{
    ht_item_t * * item;

    if (ht_look_up(htd,key,index,match,&item) == NSRV_OK) {
	if (strcmp(signature,(*item)->signature))
	    return(NSRV_NOT_YOURS);
	else {
	    delete(item);
	    htd->count--;
	    return(NSRV_OK);
	}
    }
    else {
        return(NSRV_NOT_REGISTERED);
    }
}


static nsrv_ret_t
ht_insert(htd,key,signature,data,index,match,append,replace)
    htd_t * htd;
    void_ptr key;
    nsrv_name_t signature;
    void_ptr data;
    pds_uint32 (* index) ();
    int (* match) ();
    nsrv_ret_t (* append) ();
    nsrv_ret_t (* replace) ();
{
    nsrv_ret_t nret;
    ht_item_t * * item;

    if (ht_look_up(htd,key,index,match,&item) == NSRV_OK) {
	if (strcmp(signature,(*item)->signature))
	    return(NSRV_NOT_YOURS);
	else {
	    return(replace(*item,data));
	}
    }
    else {
	*item = (ht_item_t *) pds_mem_alloc_size(&nsrv_shd,sizeof(ht_item_t));
        if (!*item)
            return(NSRV_NOMEMORY);

        (*item)->next = (ht_item_t *) 0;

	(*item)->key = pds_mem_alloc_size(&nsrv_shd,NSRV_NAMELEN+1);
        if (!(*item)->key) {
            pds_mem_free_size(&nsrv_shd,(void_ptr) *item,sizeof(ht_item_t));
            return(NSRV_NOMEMORY);
        }
	strcpy((*item)->signature,signature);

	nret = append(*item,key,data);
	if (nret != NSRV_OK) {
	    pds_mem_free_size(&nsrv_shd,(void_ptr) (*item)->key,NSRV_NAMELEN+1);
            pds_mem_free_size(&nsrv_shd,(void_ptr) (*item),sizeof(ht_item_t));
	}
	else 
	    htd->count++;
	return(nret);
    }
}



/**********************************************************************
** More Local Primitives 
***********************************************************************/

static pds_uint32
nsrv_id_index(htd,key)
    htd_t * htd;
    void_ptr key;
{
    return((pds_uint32) (* (nsrv_id_t *) key % htd->size));
}


static int
nsrv_id_match(key1,key2)
    void_ptr key1;
    void_ptr key2;
{
    return ((* (nsrv_id_t *) key1) == (* (nsrv_id_t *) key2));
}


static nsrv_ret_t
nsrv_id_replace(item,data)
    ht_item_t * item;
    void_ptr data;
{
    return(NSRV_NOT_YOURS);
}


static void
nsrv_id_delete(item)
    ht_item_t * * item;
{
    ht_item_t * next;

    next = (*item)->next;
    pds_mem_free_size(&nsrv_shd,(void_ptr) (*item)->key,sizeof(nsrv_id_t));
    pds_mem_free_size(&nsrv_shd,(void_ptr) (*item)->data,sizeof(nsrv_id_t));
    pds_mem_free_size(&nsrv_shd,(void_ptr) (*item),sizeof(ht_item_t));
    *item = next;
}


static nsrv_ret_t
nsrv_id_append(item,key,data)
    ht_item_t * item;
    void_ptr key;
    void_ptr data;
{
    item->data = pds_mem_alloc_size(&nsrv_shd,sizeof(nsrv_id_t));
    if (!item->data) 
	return(NSRV_NOMEMORY);

    * (nsrv_id_t *) item->data = * (nsrv_id_t *) data;

    * (nsrv_id_t *) item->key = * (nsrv_id_t *) key;

    return(NSRV_OK);
}


static nsrv_ret_t
nsrv_claim_bport_id(signature,port_id)
    char * signature;
    bport_id_t port_id;
{
    nsrv_ret_t nret;
    nsrv_id_t data = port_id;

    HTD_Lock(bport_id_htd);

    nret = ht_insert(bport_id_htd,
                     (void_ptr) &data,
		     signature,
                     (void_ptr) &data,
                     nsrv_id_index,
                     nsrv_id_match,
                     nsrv_id_append,
                     nsrv_id_replace);

    HTD_Unlock(bport_id_htd);

    return(nret);
}


static nsrv_ret_t
nsrv_claim_domain_id(signature,domain_id)
    char * signature;
    bdomain_id_t domain_id;
{
    nsrv_ret_t nret;
    nsrv_id_t data = domain_id;

    HTD_Lock(domain_id_htd);

    nret = ht_insert(domain_id_htd,
                     (void_ptr) &data,
		     signature,
                     (void_ptr) &data,
                     nsrv_id_index,
                     nsrv_id_match,
                     nsrv_id_append,
                     nsrv_id_replace);

    HTD_Unlock(domain_id_htd);

    return(nret);
}


static pds_uint32 
nsrv_name_index(htd,key)
    htd_t * htd;
    void_ptr key;
{
    pds_uint32 indx = 0;
    unsigned char * name = (unsigned char *) key;
    int len = (int) strlen(key);
    int shift = 8 * (sizeof(indx) - sizeof(char));
    int i;

    for (i=0;i<len;i++) {
	indx += ((pds_uint32) name[i] << (i % shift));
    }
    
    return(indx % htd->size);
}


static int
nsrv_name_match(name1,name2)
    void_ptr name1;
    void_ptr name2;
{
    return (!strcmp((char *) name1,(char *) name2));
}


static nsrv_ret_t
bport_append(item,key,data)
    ht_item_t * item;
    void_ptr key;
    void_ptr data;
{
    item->data = pds_mem_alloc_size(&nsrv_shd,sizeof(bport_t));
    if (!item->data)
        return(NSRV_NOMEMORY);

    strcpy((char *) item->key,(char *) key);

    * (bport_t *) item->data = * (bport_t *) data;

    return(NSRV_OK);
}


static nsrv_ret_t
bport_replace(item,data)
    ht_item_t * item;
    void_ptr data;
{
    * (bport_t *) item->data = * (bport_t *) data;
    return(NSRV_OK);
}


static void
bport_delete(item)
    ht_item_t * * item;
{
    ht_item_t * next;

    next = (*item)->next;
    pds_mem_free_size(&nsrv_shd,(void_ptr) (*item)->key,NSRV_NAMELEN+1);
    pds_mem_free_size(&nsrv_shd,(void_ptr) (*item)->data,sizeof(bport_t));
    pds_mem_free_size(&nsrv_shd,(void_ptr) (*item),sizeof(ht_item_t));
    *item = next;
}


static nsrv_ret_t
domain_append(item,key,data)
    ht_item_t * item;
    void_ptr key;
    void_ptr data;
{
    item->data = pds_mem_alloc_size(&nsrv_shd,sizeof(bdomain_t));
    if (!item->data)
        return(NSRV_NOMEMORY);

    strcpy((char *) item->key,(char *) key);

    * (bdomain_t *) item->data = * (bdomain_t *) data;

    return(NSRV_OK);
}


static nsrv_ret_t
domain_replace(item,data)
    ht_item_t * item;
    void_ptr data;
{
    * (bdomain_t *) item->data = * (bdomain_t *) data;
    return(NSRV_OK);
}


static void
domain_delete(item)
    ht_item_t * * item;
{
    ht_item_t * next;

    next = (*item)->next;
    pds_mem_free_size(&nsrv_shd,(void_ptr) (*item)->key,NSRV_NAMELEN+1);
    pds_mem_free_size(&nsrv_shd,(void_ptr) (*item)->data,sizeof(bdomain_t));
    pds_mem_free_size(&nsrv_shd,(void_ptr) (*item),sizeof(ht_item_t));
    *item = next;
}


static nsrv_ret_t
aport_append(item,key,data)
    ht_item_t * item;
    void_ptr key;
    void_ptr data;
{
    item->data = pds_mem_alloc_size(&nsrv_shd,sizeof(aport_t));
    if (!item->data)
        return(NSRV_NOMEMORY);

    * (aport_t *) item->data = * (aport_t *) data;

    strcpy((char *) item->key,(char *) key);

    return(NSRV_OK);
}


static nsrv_ret_t
aport_replace(item,data)
    ht_item_t * item;
    void_ptr data;
{
    * (aport_t *) item->data = * (aport_t *) data;
    return(NSRV_OK);
}


static void
aport_delete(item)
    ht_item_t * * item;
{
    ht_item_t * next;

    next = (*item)->next;
    pds_mem_free_size(&nsrv_shd,(void_ptr) (*item)->key,NSRV_NAMELEN+1);
    pds_mem_free_size(&nsrv_shd,(void_ptr) (*item)->data,sizeof(aport_t));
    pds_mem_free_size(&nsrv_shd,(void_ptr) (*item),sizeof(ht_item_t));
    *item = next;
}


static nsrv_ret_t
nsrv_name_look_up(htd,key,name,item)
    htd_t * htd;
    nsrv_name_t key;
    nsrv_name_t name;
    ht_item_t * * item;
{
    nsrv_ret_t nret;
    htd_t * htd1;
    ht_item_t * * item0;
    ht_item_t * * item1;

    if (((int) strlen(key) > NSRV_NAMELEN) || 
	((int) strlen(name) > NSRV_NAMELEN))
	return(NSRV_INVAL);

    nret = ht_look_up(htd,
                      (void_ptr) key,
                      nsrv_name_index,
                      nsrv_name_match,
                      &item0);
    if (nret != NSRV_OK) 
	return(nret);
    htd1 = (htd_t *) (*item0)->data;

    nret = ht_look_up(htd1,
                      name,
                      nsrv_name_index,
                      nsrv_name_match,
                      &item1);
    *item = *item1;
    return(nret);
}


static nsrv_ret_t
nsrv_name_deregister(htd,key,name,signature,delete)
    htd_t * htd;
    nsrv_name_t key;
    nsrv_name_t name;
    nsrv_name_t signature;
    void (* delete) ();
{
    nsrv_ret_t nret;
    htd_t * htd1;
    ht_item_t * * item;
    ht_item_t * next;

    if (((int) strlen(key) > NSRV_NAMELEN) ||
	((int) strlen(name) > NSRV_NAMELEN) ||
	((int) strlen(signature) > NSRV_NAMELEN))
        return(NSRV_INVAL);

    nret = ht_look_up(htd,
                      (void_ptr) key,
                      nsrv_name_index,
                      nsrv_name_match,
                      &item);
    if (nret != NSRV_OK)
        return(nret);
    htd1 = (htd_t *) (*item)->data;

    nret = ht_delete(htd1,
                     (void_ptr) name,
                     signature,
                     nsrv_name_index,
                     nsrv_name_match,
                     delete);

    if ((nret == NSRV_OK) && (htd1->count == 0)) {
        next = (*item)->next;
        pds_mem_free_size(&nsrv_shd,(void_ptr) (*item)->key,NSRV_NAMELEN+1);
        ht_destroy(htd1);
        pds_mem_free_size(&nsrv_shd,(void_ptr) *item,sizeof(ht_item_t));
        *item = next;
	htd->count--;
    }

    return(nret);
}


static nsrv_ret_t
nsrv_name_register(htd,key,name,signature,data,bport_id,bdomain_id,size,append,replace)
    htd_t * htd;
    nsrv_name_t key;
    nsrv_name_t name;
    nsrv_name_t signature;
    void_ptr data;
    bport_id_t bport_id;
    bdomain_id_t bdomain_id;
    unsigned int size;
    nsrv_ret_t (* append) ();
    nsrv_ret_t (* replace) ();
{
    nsrv_ret_t nret;
    htd_t * htd1;
    ht_item_t * item;
    ht_item_t * * item_ext;

    if (((int) strlen(key) > NSRV_NAMELEN) ||
        ((int) strlen(name) > NSRV_NAMELEN) ||
        ((int) strlen(signature) > NSRV_NAMELEN))
        return(NSRV_INVAL); 

    if (bdomain_id != 0) {
        nret = nsrv_claim_domain_id(signature,bdomain_id);
        if ((nret != NSRV_NOT_YOURS) && (nret != NSRV_OK))
            return(nret);
    }
    if (bport_id != 0) {
        nret = nsrv_claim_bport_id(signature,bport_id);
        if ((nret != NSRV_NOT_YOURS) && (nret != NSRV_OK))
            return(nret);
    }

    nret = ht_look_up(htd,
                      (void_ptr) key,
                      nsrv_name_index,
                      nsrv_name_match,
                      &item_ext);
    if (nret != NSRV_OK) {
        item = (ht_item_t *)
               pds_mem_alloc_size(&nsrv_shd,sizeof(ht_item_t));
        if (!item)
            return(NSRV_NOMEMORY);
        item->next = (ht_item_t *) 0;
        item->key = pds_mem_alloc_size(&nsrv_shd,NSRV_NAMELEN+1);
        if (!item->key) {
            pds_mem_free_size(&nsrv_shd,(void_ptr) item,sizeof(ht_item_t));
            return(NSRV_NOMEMORY);
        }
        strcpy((char *) item->key, (char *) key);
        nret = ht_create(size,&htd1);
        if (nret != NSRV_OK) {
            pds_mem_free_size(&nsrv_shd,(void_ptr) item->key,NSRV_NAMELEN+1);
            pds_mem_free_size(&nsrv_shd,(void_ptr) item,sizeof(ht_item_t));
            return(nret);
	}
	item->signature[0] = '\0';
        item->data = (void_ptr) htd1;
	*item_ext = item;
	htd->count++;
    }
    else
        htd1 = (htd_t *) (*item_ext)->data;

    nret = ht_insert(htd1,
                     (void_ptr) name,
                     signature,
                     data,
                     nsrv_name_index,
                     nsrv_name_match,
                     append,
                     replace);

    return(nret);
}




/**********************************************************************
************************  Exported Primitives  ************************
***********************************************************************/

nsrv_ret_t
nsrv_types_init_i()
{
    pds_ret_t pret;

    msg_typedef_t mdt_name[5];
    msg_typedef_t mdt_version[6];
    msg_typedef_t mdt_request[10];
    msg_typedef_t mdt_reply[7];

    mdt_name[0] = MDT_BEGIN;
    mdt_name[1] = MDT_ARRAY_OF;
    mdt_name[2] = NSRV_NAMELEN+1;
    mdt_name[3] = MDT_CHAR;
    mdt_name[4] = MDT_END;

    if ((pret = pds_type_define(NSRV_INTFC,1,
				mdt_name,&MDT_NSRVNAME)) != PDS_OK)
	return((nsrv_ret_t) pret);

    mdt_version[0] = MDT_BEGIN;
    mdt_version[1] = MDT_STRUCT_OPEN;
    mdt_version[2] = MDT_INT32;
    mdt_version[3] = MDT_INT32;
    mdt_version[4] = MDT_STRUCT_CLOSE;
    mdt_version[5] = MDT_END;

    if ((pret = pds_type_define(NSRV_INTFC,2,
				mdt_version,&MDT_NSRVVERSION)) != PDS_OK)
	return((nsrv_ret_t) pret);

    mdt_request[0] = MDT_BEGIN;
    mdt_request[1] = MDT_STRUCT_OPEN;
    mdt_request[2] = MDT_NSRVNUMBER;
    mdt_request[3] = MDT_APORTID;
    mdt_request[4] = MDT_NSRVNAME;
    mdt_request[5] = MDT_NSRVNAME;
    mdt_request[6] = MDT_NSRVNAME;
    mdt_request[7] = MDT_APORT;
    mdt_request[8] = MDT_STRUCT_CLOSE;
    mdt_request[9] = MDT_END;

    if ((pret = pds_type_define(NSRV_INTFC,3,mdt_request,
			        &MDT_APORT_REGISTER_REQUEST)) != PDS_OK)
	return((nsrv_ret_t) pret);

    mdt_request[7] = MDT_BPORT;

    if ((pret = pds_type_define(NSRV_INTFC,4,mdt_request,
			        &MDT_BPORT_REGISTER_REQUEST)) != PDS_OK)
	return((nsrv_ret_t) pret);

    mdt_request[7] = MDT_BDOMAIN;

    if ((pret = pds_type_define(NSRV_INTFC,5,mdt_request,
			        &MDT_BDOMAIN_REGISTER_REQUEST)) != PDS_OK)
	return((nsrv_ret_t) pret);

    mdt_request[0] = MDT_BEGIN;
    mdt_request[1] = MDT_STRUCT_OPEN;
    mdt_request[2] = MDT_NSRVNUMBER;
    mdt_request[3] = MDT_APORTID;
    mdt_request[4] = MDT_NSRVNAME;
    mdt_request[5] = MDT_NSRVNAME;
    mdt_request[6] = MDT_NSRVNAME;
    mdt_request[7] = MDT_STRUCT_CLOSE;
    mdt_request[8] = MDT_END;

    if ((pret = pds_type_define(NSRV_INTFC,6,mdt_request,
			        &MDT_DEREGISTER_REQUEST)) != PDS_OK)
	return((nsrv_ret_t) pret);

    mdt_request[0] = MDT_BEGIN;
    mdt_request[1] = MDT_STRUCT_OPEN;
    mdt_request[2] = MDT_NSRVNUMBER;
    mdt_request[3] = MDT_APORTID;
    mdt_request[4] = MDT_NSRVNAME;
    mdt_request[5] = MDT_NSRVNAME;
    mdt_request[6] = MDT_STRUCT_CLOSE;
    mdt_request[7] = MDT_END;

    if ((pret = pds_type_define(NSRV_INTFC,7,mdt_request,
			        &MDT_LOOK_UP_REQUEST)) != PDS_OK)
	return((nsrv_ret_t) pret);

    mdt_request[0] = MDT_BEGIN;
    mdt_request[1] = MDT_STRUCT_OPEN;
    mdt_request[2] = MDT_NSRVNUMBER;
    mdt_request[3] = MDT_APORTID;
    mdt_request[4] = MDT_NSRVNAME;
    mdt_request[5] = MDT_BPORTID;
    mdt_request[6] = MDT_STRUCT_CLOSE;
    mdt_request[7] = MDT_END;

    if ((pret = pds_type_define(NSRV_INTFC,8,mdt_request,
			        &MDT_BPORTID_REQUEST)) != PDS_OK)
	return((nsrv_ret_t) pret);

    mdt_request[5] = MDT_BDOMAINID;

    if ((pret = pds_type_define(NSRV_INTFC,9,mdt_request,
			        &MDT_BDOMAINID_REQUEST)) != PDS_OK)
	return((nsrv_ret_t) pret);

    mdt_request[0] = MDT_BEGIN;
    mdt_request[1] = MDT_STRUCT_OPEN;
    mdt_request[2] = MDT_NSRVNUMBER;
    mdt_request[3] = MDT_APORTID;
    mdt_request[4] = MDT_STRUCT_CLOSE;
    mdt_request[5] = MDT_END;

    if ((pret = pds_type_define(NSRV_INTFC,10,mdt_request,
			        &MDT_VERSION_REQUEST)) != PDS_OK)
	return((nsrv_ret_t) pret);

    mdt_reply[0] = MDT_BEGIN;
    mdt_reply[1] = MDT_STRUCT_OPEN;
    mdt_reply[2] = MDT_NSRVRET;
    mdt_reply[3] = MDT_STRUCT_CLOSE;
    mdt_reply[4] = MDT_END;

    if ((pret = pds_type_define(NSRV_INTFC,11,mdt_reply,
			        &MDT_SIMPLE_REPLY)) != PDS_OK)
	return((nsrv_ret_t) pret);

    mdt_reply[0] = MDT_BEGIN;
    mdt_reply[1] = MDT_STRUCT_OPEN;
    mdt_reply[2] = MDT_NSRVRET;
    mdt_reply[3] = MDT_APORT;
    mdt_reply[4] = MDT_STRUCT_CLOSE;
    mdt_reply[5] = MDT_END;

    if ((pret = pds_type_define(NSRV_INTFC,12,mdt_reply,
			        &MDT_APORT_REPLY)) != PDS_OK)
	return((nsrv_ret_t) pret);

    mdt_reply[3] = MDT_BPORT;

    if ((pret = pds_type_define(NSRV_INTFC,13,mdt_reply,
			        &MDT_BPORT_REPLY)) != PDS_OK)
	return((nsrv_ret_t) pret);

    mdt_reply[3] = MDT_BDOMAIN;

    if ((pret = pds_type_define(NSRV_INTFC,14,mdt_reply,
			        &MDT_BDOMAIN_REPLY)) != PDS_OK)
	return((nsrv_ret_t) pret);

    mdt_reply[3] = MDT_BPORTID;

    if ((pret = pds_type_define(NSRV_INTFC,15,mdt_reply,
			        &MDT_BPORTID_REPLY)) != PDS_OK)
	return((nsrv_ret_t) pret);

    mdt_reply[3] = MDT_BDOMAINID;

    if ((pret = pds_type_define(NSRV_INTFC,16,mdt_reply,
			        &MDT_BDOMAINID_REPLY)) != PDS_OK)
	return((nsrv_ret_t) pret);

    mdt_reply[3] = MDT_NSRVVERSION;

    if ((pret = pds_type_define(NSRV_INTFC,17,mdt_reply,
			        &MDT_VERSION_REPLY)) != PDS_OK)
	return((nsrv_ret_t) pret);

    return(NSRV_OK);
}


void
nsrv_init_port_number(port_number,property)
    unsigned * port_number;
    unsigned * property;
{
    if (!*port_number) { 
        if (getenv(NSRV_PORT)) { /* use environment variable NSRV_PORT */
            int number;
            number = atoi(getenv(NSRV_PORT));
            if ((number < 1024) || (number > 0xffff)) {
                fprintf(stderr,
                        "\n nsrv: Invalid environment variable %s !\n",
                        NSRV_PORT);
                exit(-1);
            }
            else {
                *port_number = number;
		*property = NSRV_ENVIRONMENT;
	    }
        }
        else { /* use default port number NSRV_PORT_DEFAULT */
            *port_number = NSRV_PORT_DEFAULT;
	    *property = NSRV_DEFAULT;
	}
    }
    else
	*property = NSRV_PARAMETER;
}


void
nsrv_sock_noinherit(sock)
    int sock;
{
        nsrv_assert(sock > 0);

	/* don't inherit socket on exec() */
        if (fcntl(sock,F_SETFD,1) == -1) {
            nsrv_perror_and_assert_always("fcntl()");
	}
}


void
nsrv_sock_linger(sock)
    int sock;
{
    struct linger li;

    nsrv_assert(sock > 0);

    li.l_onoff = 1;    /* switch lingering on     */
    li.l_linger = 10;  /* linger up to 10 seconds */

    /* linger to ensure data delivery before closing socket */
    if (setsockopt(sock,SOL_SOCKET,SO_LINGER,
                   (char *) &li,sizeof(li)) == -1) {
        nsrv_perror_and_assert_always("setsockopt() ");
    }
}


void
nsrv_sock_nodelay(sock)
    int sock;
{
    int on = 1;

    nsrv_assert(sock > 0);

    /* disable the default delay of TCP data transmission */
    if (setsockopt(sock,IPPROTO_TCP,TCP_NODELAY,
                   (char *) &on,sizeof(on)) == -1) {
	nsrv_perror_and_assert_always("setsockopt() ");
    }
}


nsrv_ret_t
nsrv_send_sock(sock,message,size)
    int sock;
    char * message;
    pds_uint32 size;
{
    pds_uint32 buf;
    char * buffer;
    XDR xdrs;
    int nbytes,tbytes;
    int sbytes = sizeof(pds_uint32);

    nsrv_assert(sock > 0);

    buffer = (char *) &buf;

    xdrmem_create(&xdrs,(const caddr_t) buffer,
                  (const u_int) sizeof(pds_uint32),
                  XDR_ENCODE);

    if (!xdr_pds_u_int32(&xdrs,&size)) {
        xdr_destroy(&xdrs);
        return(NSRV_XDR_ENCODE);
    }
    nsrv_assert(xdr_getpos(&xdrs) == sizeof(pds_uint32));

    xdr_destroy(&xdrs);

    /* write size of message */
    tbytes = 0;
    while (tbytes < sbytes) {
        nbytes = write(sock,
                       buffer + tbytes,
                       sbytes - tbytes);
        if (nbytes >= 0) /* successful write */
            tbytes += nbytes;
        else if (errno != EINTR)
            return(NSRV_ERROR);
        /* else retry */
    }

    /* write message */
    tbytes = 0;
    while (tbytes < size) {
        nbytes = write(sock,
                       message + tbytes,
                       size - tbytes);
        if (nbytes >= 0) /* successful write */
            tbytes += nbytes;
        else if (errno != EINTR)
            return(NSRV_ERROR);
        /* else retry */
    }

    return(NSRV_OK);
}


nsrv_ret_t
nsrv_receive_sock(sock,message,size)
    int sock;
    char * message;
    pds_uint32 * size;
{
    pds_uint32 buf;
    char * buffer;
    XDR xdrs;
    int nbytes,tbytes;
    int rbytes = sizeof(pds_uint32);

    nsrv_assert(sock > 0);

    buffer = (char *) &buf;

    /* read size of message */
    tbytes = 0;
    while (tbytes < rbytes) {
        nbytes = read(sock,
                      buffer + tbytes,
                      rbytes - tbytes);
        if (nbytes > 0) /* successful read */
            tbytes += nbytes;
        else if (nbytes == 0) { /* End Of Stream */
	    if (tbytes == 0)
                return(NSRV_WARN);
	    else
                return(NSRV_ERROR);
	}
        else if (errno != EINTR)
            return(NSRV_ERROR);
        /* else retry */
    }

    xdrmem_create(&xdrs,(const caddr_t) buffer,
                  (const u_int) sizeof(pds_uint32),
                  XDR_DECODE);

    if (!xdr_pds_u_int32(&xdrs,size)) {
        xdr_destroy(&xdrs);
        return(NSRV_ERROR);
    }
    nsrv_assert(xdr_getpos(&xdrs) == sizeof(pds_uint32));

    xdr_destroy(&xdrs);

    rbytes = *size;

    /* read message */
    tbytes = 0;
    while (tbytes < rbytes) {
        nbytes = read(sock,
                      message + tbytes,
                      rbytes - tbytes);
        if (nbytes > 0) /* successful read */
            tbytes += nbytes;
        else if (nbytes == 0) /* End Of Stream */
            return(NSRV_ERROR);
        else if (errno != EINTR)
            return(NSRV_ERROR);
        /* else retry */
    }

    return(NSRV_OK);
}


void
nsrv_close_sock(sock)
    int sock;
{
    int ret;

    nsrv_assert(sock > 0);

    do {
        ret = close(sock);
    } while ((ret == -1) && (errno == EINTR));
}


void 
nsrv_init_server_i(datafile,verbose)
    char * datafile;
    int verbose;
{
    nsrv_verbose = verbose;

    if (nsrv_verbose)
	printf("\n\n");

    if ((sizeof(bdomain_id_t) > sizeof(nsrv_id_t)) ||
        (sizeof(bport_id_t) > sizeof(nsrv_id_t))) {
        fprintf(stderr, "nsrv: Implementation error !\n");
        exit(2);
    }

    /* initialise random number generator */
    srand((unsigned int) getpid());

    /* init shared memory heap */
    nsrv = (nsrv_t * *)
	   pds_mem_init(datafile,pds_mem_base(),NSRV_DATA_AREA_SIZE,&nsrv_shd,1);
    if (nsrv - (nsrv_t * *) 0 == -1) {
        fprintf(stderr, "nsrv: Not enough memory !\n");
        exit(2);
    }

    /* initialise nsrv_data_start */
    nsrv_data_start = nsrv_shd.shared_header->start;

    /* allocate hash table descriptors */
    *nsrv = (nsrv_t *)
            pds_mem_alloc_size(&nsrv_shd,sizeof(nsrv_t));

    /* create hash tables */
    if (!(*nsrv) ||
        (NSRV_OK != ht_create(KEY_HT_SIZE,&(*nsrv)->domain)) ||
        (NSRV_OK != ht_create(KEY_HT_SIZE,&(*nsrv)->bport)) ||
        (NSRV_OK != ht_create(KEY_HT_SIZE,&(*nsrv)->aport)) ||
        (NSRV_OK != ht_create(DOMAIN_ID_HT_SIZE,&(*nsrv)->domain_id)) ||
        (NSRV_OK != ht_create(BPORT_ID_HT_SIZE,&(*nsrv)->bport_id))) {
        pds_mem_release(&nsrv_shd);
        fprintf(stderr, "nsrv: Not enough memory !\n");
        exit(2);
    }

    /* get references to hash table descriptors */
    domain_htd = (*nsrv)->domain;
    bport_htd = (*nsrv)->bport;
    aport_htd = (*nsrv)->aport;
    domain_id_htd = (*nsrv)->domain_id;
    bport_id_htd = (*nsrv)->bport_id;
}


void
nsrv_exit_i()
{
    pds_mem_release(&nsrv_shd);
}


nsrv_ret_t 
nsrv_init_client_i(datafile)
    char * datafile;
{
    nsrv_assert((sizeof(bdomain_id_t) <= sizeof(nsrv_id_t)) &&
		(sizeof(bport_id_t) <= sizeof(nsrv_id_t)));

    /* initialise random number generator */
    srand((unsigned int) getpid());

    /* init shared memory heap */
    nsrv = (nsrv_t * *)
            pds_mem_init(datafile,(char *) 0,0,&nsrv_shd,0);
    if (nsrv - (nsrv_t * *) 0 == -1)
    	return(NSRV_NOMEMORY);

    /* initialise nsrv_data_start */
    nsrv_data_start = nsrv_shd.shared_header->start;

    /* get references to hash table descriptors */
    domain_htd = (*nsrv)->domain;
    bport_htd = (*nsrv)->bport;
    aport_htd = (*nsrv)->aport;
    domain_id_htd = (*nsrv)->domain_id;
    bport_id_htd = (*nsrv)->bport_id;

    return(NSRV_OK);
}


nsrv_ret_t 
nsrv_aport_register_i(key,name,signature,port)
    nsrv_name_t key;
    nsrv_name_t name;
    nsrv_name_t signature;
    aport_t * port;
{
    nsrv_ret_t nret;

    if (nsrv_verbose)
	printf("nsrv: nsrv_aport_register(%s,%s) ... ",key,name);

    if ((port->bdomain_id == (bdomain_id_t) 0) ||
        (port->bport_id == (bport_id_t) 0) ||
        (port->aport_id == (aport_id_t) 0)) {
	nret = NSRV_NOPORT;
        if (nsrv_verbose) print_nsrv_retcode(nret);
	return(nret);
    }

    HTD_Lock(aport_htd);

    nret = nsrv_name_register(aport_htd,
			      key,
			      name,
			      signature,
			      (void_ptr) port,
			      port->bport_id,
			      port->bdomain_id,
			      APORT_HT_SIZE,
			      aport_append,
			      aport_replace);

    HTD_Unlock(aport_htd);

    if (nsrv_verbose) print_nsrv_retcode(nret);

    return(nret);
}


nsrv_ret_t
nsrv_aport_deregister_i(key,name,signature)
    nsrv_name_t key;
    nsrv_name_t name;
    nsrv_name_t signature;
{
    nsrv_ret_t nret;

    if (nsrv_verbose)
	printf("nsrv: nsrv_aport_deregister(%s,%s) ... ",key,name);

    HTD_Lock(aport_htd);

    nret = nsrv_name_deregister(aport_htd,key,name,signature,aport_delete);

    HTD_Unlock(aport_htd);

    if (nsrv_verbose) print_nsrv_retcode(nret);

    return(nret);
}


nsrv_ret_t
nsrv_aport_look_up_i(key,port_name,port)
    nsrv_name_t key;
    nsrv_name_t port_name;
    aport_t * port;
{
    nsrv_ret_t nret;
    ht_item_t * item;

    if (nsrv_verbose)
	printf("nsrv: nsrv_aport_look_up(%s,%s) ... ",key,port_name);

    if (!port) {
	nret = NSRV_INVAL;
        if (nsrv_verbose) print_nsrv_retcode(nret);
	return(nret);
    }

    HTD_Lock(aport_htd);

    nret = nsrv_name_look_up(aport_htd,key,port_name,&item);

    if (nret == NSRV_OK)
	* port = * (aport_t *) item->data;

    HTD_Unlock(aport_htd);

    if (nsrv_verbose) print_nsrv_retcode(nret);

    return(nret);
}


nsrv_ret_t 
nsrv_bport_register_i(key,name,signature,port)
    nsrv_name_t key;
    nsrv_name_t name;
    nsrv_name_t signature;
    bport_t * port;
{
    nsrv_ret_t nret;

    if (nsrv_verbose)
	printf("nsrv: nsrv_bport_register(%s,%s) ... ",key,name);

    if ((port->bdomain_id == (bdomain_id_t) 0) ||
        (port->bport_id == (bport_id_t) 0)) {
	nret = NSRV_NOPORT;
        if (nsrv_verbose) print_nsrv_retcode(nret);
	return(nret);
    }

    HTD_Lock(bport_htd);

    nret = nsrv_name_register(bport_htd,
                              key,
                              name,
                              signature,
                              (void_ptr) port,
                              port->bport_id,
                              port->bdomain_id,
                              BPORT_HT_SIZE,
                              bport_append,
                              bport_replace);

    HTD_Unlock(bport_htd);

    if (nsrv_verbose) print_nsrv_retcode(nret);

    return(nret);
}


nsrv_ret_t 
nsrv_bport_deregister_i(key,name,signature)
    nsrv_name_t key;
    nsrv_name_t name;
    nsrv_name_t signature;
{
    nsrv_ret_t nret;

    if (nsrv_verbose) 
	printf("nsrv: nsrv_bport_deregister(%s,%s) ... ",key,name);

    HTD_Lock(bport_htd);

    nret = nsrv_name_deregister(bport_htd,key,name,signature,bport_delete);

    HTD_Unlock(bport_htd);

    if (nsrv_verbose) print_nsrv_retcode(nret);

    return(nret);
}


nsrv_ret_t 
nsrv_bport_look_up_i(key,port_name,port)
    nsrv_name_t key;
    nsrv_name_t port_name;
    bport_t * port;
{
    nsrv_ret_t nret;
    ht_item_t * item;

    if (nsrv_verbose) 
	printf("nsrv: nsrv_bport_look_up(%s,%s) ... ",key,port_name);

    if (!port) {
	nret = NSRV_INVAL;
        if (nsrv_verbose) print_nsrv_retcode(nret);
	return(nret);
    }

    HTD_Lock(bport_htd);

    nret = nsrv_name_look_up(bport_htd,key,port_name,&item);

    if (nret == NSRV_OK)
	*port = * (bport_t *) item->data;

    HTD_Unlock(bport_htd);

    if (nsrv_verbose) print_nsrv_retcode(nret);

    return(nret);
}


nsrv_ret_t
nsrv_bdomain_register_i(key,name,signature,domain)
    nsrv_name_t key;
    nsrv_name_t name;
    nsrv_name_t signature;
    bdomain_t * domain;
{
    nsrv_ret_t nret;

    if (nsrv_verbose)
	printf("nsrv: nsrv_bdomain_register(%s,%s) ... ",key,name);

    if (domain->bdomain_id == (bdomain_id_t) 0) {
	nret = NSRV_NODOMAIN;
        if (nsrv_verbose) print_nsrv_retcode(nret);
	return(nret);
    }

    HTD_Lock(domain_htd);

    nret = nsrv_name_register(domain_htd,
                              key,
                              name,
                              signature,
                              (void_ptr) domain,
                              0,
                              domain->bdomain_id,
                              DOMAIN_HT_SIZE,
                              domain_append,
                              domain_replace);

    HTD_Unlock(domain_htd);

    if (nsrv_verbose) print_nsrv_retcode(nret);

    return(nret);
}


nsrv_ret_t
nsrv_bdomain_deregister_i(key,name,signature)
    nsrv_name_t key;
    nsrv_name_t name;
    nsrv_name_t signature;
{
    nsrv_ret_t nret;

    if (nsrv_verbose)
	printf("nsrv: nsrv_bdomain_deregister(%s,%s) ... ",key,name);

    HTD_Lock(domain_htd);

    nret = nsrv_name_deregister(domain_htd,key,name,signature,domain_delete);

    HTD_Unlock(domain_htd);

    if (nsrv_verbose) print_nsrv_retcode(nret);

    return(nret);
}


nsrv_ret_t
nsrv_bdomain_look_up_i(key,domain_name,domain)
    nsrv_name_t key;
    nsrv_name_t domain_name;
    bdomain_t * domain;
{
    nsrv_ret_t nret;
    ht_item_t * item;

    if (nsrv_verbose) 
	printf("nsrv: nsrv_bdomain_look_up(%s,%s) ... ",key,domain_name);

    if (!domain) {
	nret = NSRV_INVAL;
        if (nsrv_verbose) print_nsrv_retcode(nret);
        return(nret);
    }

    HTD_Lock(domain_htd);

    nret = nsrv_name_look_up(domain_htd,key,domain_name,&item);

    if (nret == NSRV_OK) {
	* domain = * (bdomain_t *) item->data;
	strcpy(domain->bdomain_file,
	       ((bdomain_t *) item->data)->bdomain_file);
    }

    HTD_Unlock(domain_htd);

    if (nsrv_verbose) print_nsrv_retcode(nret);

    return(nret);
}


nsrv_ret_t
nsrv_new_bport_id_i(signature,port_id)
    nsrv_name_t signature;
    bport_id_t * port_id;	/* in/out */
{
    int i = 0;
    nsrv_id_t data;
    nsrv_ret_t nret;

    if (nsrv_verbose)
	printf("nsrv: nsrv_new_bport_id() ... ");

    if (((int) strlen(signature) > NSRV_NAMELEN) || (!port_id)) {
	nret = NSRV_INVAL;
        if (nsrv_verbose) print_nsrv_retcode(nret);
	return(nret);
    }

    /* first try to claim specified port_id */
    if ((*port_id > 0) && (*port_id <= BPORT_ID_MAX))
        data = (nsrv_id_t) *port_id;
    else
	data = ((nsrv_id_t) rand() % BPORT_ID_MAX + 1);

    HTD_Lock(bport_id_htd);

    while (((nret = ht_insert(bport_id_htd,
	       		      (void_ptr) &data,
		     	      signature,
			      (void_ptr) &data,
		  	      nsrv_id_index,
			      nsrv_id_match,
			      nsrv_id_append,
			      nsrv_id_replace)) == NSRV_NOT_YOURS) &&
	   (i < BPORT_ID_MAX)) {
	data = ((nsrv_id_t) rand() % BPORT_ID_MAX + 1);
	i++;
    }

    if (i < BPORT_ID_MAX) {
        *port_id = data;
        HTD_Unlock(bport_id_htd);
	nret = NSRV_OK;
    }
    else {
        *port_id = 0;
        HTD_Unlock(bport_id_htd);
        nret = NSRV_IMPLIM;
    }

    if (nsrv_verbose) print_nsrv_retcode(nret);

    return(nret);
}


nsrv_ret_t
nsrv_free_bport_id_i(signature,port_id)
    nsrv_name_t signature;
    bport_id_t port_id;
{
    nsrv_id_t data;
    nsrv_ret_t nret;

    if (nsrv_verbose) 
	printf("nsrv: nsrv_free_bport_id(%d) ... ",port_id);

    if ((int) strlen(signature) > NSRV_NAMELEN) {
	nret = NSRV_INVAL;
        if (nsrv_verbose) print_nsrv_retcode(nret);
	return(nret);
    }

    data = port_id;

    HTD_Lock(bport_id_htd);

    nret = ht_delete(bport_id_htd,
	      	     (void_ptr) &data,
		     signature,
		     nsrv_id_index,
		     nsrv_id_match,
		     nsrv_id_delete);

    HTD_Unlock(bport_id_htd);

    if (nret == NSRV_NOT_REGISTERED)
	nret = NSRV_NOT_YOURS;

    if (nsrv_verbose) print_nsrv_retcode(nret);

    return(nret);
}


nsrv_ret_t
nsrv_new_bdomain_id_i(signature,domain_id)
    nsrv_name_t signature;
    bdomain_id_t * domain_id;	/* in/out */
{
    int i = 0;
    nsrv_ret_t nret;
    nsrv_id_t data;

    if (nsrv_verbose)
	printf("nsrv: nsrv_new_bdomain_id() ... ");

    if (((int) strlen(signature) > NSRV_NAMELEN) || (!domain_id)) {
	nret = NSRV_INVAL;
        if (nsrv_verbose) print_nsrv_retcode(nret);
	return(nret);
    }

    /* first try to claim specified domain_id */
    if (*domain_id != 0)
        data = (nsrv_id_t) *domain_id;
    else
	data = ((nsrv_id_t) rand() % BDOMAIN_ID_MAX + 1);

    HTD_Lock(domain_id_htd);

    while (((nret = ht_insert(domain_id_htd,
	       		      (void_ptr) &data,
		     	      signature,
			      (void_ptr) &data,
		  	      nsrv_id_index,
			      nsrv_id_match,
			      nsrv_id_append,
			      nsrv_id_replace)) == NSRV_NOT_YOURS) &&
	   ((nsrv_id_t) i < BDOMAIN_ID_MAX)) {
	data = ((nsrv_id_t) rand() % BDOMAIN_ID_MAX + 1);
	i++;
    }

    if ((nsrv_id_t) i < BDOMAIN_ID_MAX) {
        *domain_id = data;
        HTD_Unlock(domain_id_htd);
	nret = NSRV_OK;
    }
    else {
        *domain_id = 0;
        HTD_Unlock(domain_id_htd);
        nret = NSRV_IMPLIM;
    }

    if (nsrv_verbose) print_nsrv_retcode(nret);

    return(nret);
}


nsrv_ret_t
nsrv_free_bdomain_id_i(signature,domain_id)
    nsrv_name_t signature;
    bdomain_id_t domain_id;
{
    nsrv_id_t data;
    nsrv_ret_t nret;

    if (nsrv_verbose)
	printf("nsrv: nsrv_free_bdomain_id(%d) ... ",domain_id);

    if ((int) strlen(signature) > NSRV_NAMELEN) {
	nret = NSRV_INVAL;
        if (nsrv_verbose) print_nsrv_retcode(nret);
	return(nret);
    }

    data = domain_id;

    HTD_Lock(domain_id_htd);

    nret = ht_delete(domain_id_htd,
	      	     (void_ptr) &data,
		     signature,
		     nsrv_id_index,
		     nsrv_id_match,
		     nsrv_id_delete);

    HTD_Unlock(domain_id_htd);

    if (nret == NSRV_NOT_REGISTERED)
	nret = NSRV_NOT_YOURS;

    if (nsrv_verbose) print_nsrv_retcode(nret);

    return(nret);
}


nsrv_ret_t
nsrv_version_i(version)
    nsrv_version_t * version;
{
    version->v_major = NSRV_VERSION_MAJOR;
    version->v_minor = NSRV_VERSION_MINOR;

    if (nsrv_verbose) {
	printf("nsrv: nsrv_version() ... ");
	print_nsrv_retcode(NSRV_OK);
    }

    return(NSRV_OK);
}

