/**
 * \file
 * \brief Barrelfish collections library list data structure
 */
/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _LIST_H_
#define _LIST_H_

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#ifdef WIN32
#include <malloc.h>
#include "stdint.h"
#endif // WIN32

#ifdef BARRELFISH
#include <stdint.h>
#include <barrelfish/barrelfish.h>
#endif // BARRELFISH


/*
 * Predicate function.
 * Should return zero for false, non-zero otherwise.
 */
typedef int32_t (*list_predicate)(void *data, void *arg);

/*
 * a function that can be used to release the user-supplied
 * data items.
 */
typedef void (*release_data)(void *data);

/*
 * structure of each element in the 
 * linked list.
 */
struct			_listnode;

typedef struct	_listnode {
	//
	// pointer to the previous node.
	//
    struct _listnode		*prev;

	//
	// pointer to the next node.
	//
    struct _listnode		*next;

	//
	// an abstract data value to store.
	//
    void					*data;
} listnode;

/* 
 * a header to the linked list 
 */
typedef struct	_header_data {
	// total number of elements.
    uint32_t				size;

	// comparison function provided by the user.
	release_data		    data_free;

	// a pointer to keep track of
	// traversing the list.
	listnode*				cur_item;
} header_data;


/*
 * functions ...
 */

void		list_create(listnode **start, release_data func);
void		list_release(listnode *start);
int32_t		list_insert(listnode *start, void *data);
void*       list_get_ith_item(listnode *start, uint32_t index);
void*		list_find_if(listnode *start, list_predicate p, void *arg);
void*       list_remove_if(listnode *start, list_predicate p, void *key);
uint32_t	list_size(listnode *start);
int32_t		list_traverse_start(listnode *start);
void*		list_traverse_next(listnode *start);
int32_t		list_traverse_end(listnode *start);

/*
 * Visitor function. Should return non-zero to continue iteration.
 */
typedef int (*list_visitor_func)(void *data, void *arg);
/* 
 * Visit elements in list with function f until f indicates stop or end of list reached.
 * Return non-zero if end of list reached, 0 otherwise.
 */
int        list_visit(listnode *start, list_visitor_func f, void *arg);

#endif

