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

#include "collections/list.h"

/******************************************************
 * a simple linked list implementation.
 ******************************************************/

/*
 * private functions.
 */

static void list_push(listnode* existing, listnode* insert)
{
    insert->next = existing->next;
    insert->prev = existing;
    existing->next = insert;
    insert->next->prev = insert;
}

static void* list_pop(listnode* n)
{
    void *data = n->data;
    n->next->prev = n->prev;
    n->prev->next = n->next;
    n->next = n->prev = NULL;
    return data;
}

/*
 * Insert the data at the head.
 */
static listnode *list_create_node(listnode *start, listnode *where, void *data)
{
    listnode *newnode = (listnode*) malloc(sizeof(listnode));
    newnode->data = data;

    list_push(where, newnode);
    ((header_data *)(start->data))->size ++;
    return newnode;
}

static void list_destroy_node(listnode *start, listnode *node)
{
    list_pop(node);
    free(node);
    ((header_data*)start->data)->size--;
}


/*
 * public functions.
 */

/*
 * Creates a new linked list.
 */
void list_create(listnode **start, release_data func)
{
    listnode *t;

	//
	// this is an empty list containing only the header.
	//
    t = (listnode *)malloc(sizeof(listnode));
    t->next = t->prev = t;
    header_data *h = (header_data *)malloc(sizeof(header_data));
    h->size = 0;
	h->data_free = func;
	h->cur_item = NULL;
    t->data = (void*)h;

    *start = t;
}

/* 
 * Releases all the nodes in the list.
 */
void list_release(listnode *start)
{
    release_data data_free = ((header_data*)start->data)->data_free;
    listnode* cur = start->next;

	// 
	// travel through the rest of the
	// list and release all the nodes.
	//
    while (cur != start)
    {
        void * data = cur->data;
        if (data != NULL && data_free)
        {
            data_free(data);
        }
        list_destroy_node(start, cur);
        cur = start->next;
    }

    //
	// release the header.
	//
    free(start->data);
    free(start);

    return;
}

/*
 * Inserts an element in the head of the list.
 */
int32_t list_insert(listnode *start, void *data)
{
	listnode *ret = list_create_node(start, start, data);
	if (ret) {
		// success ...
		return 0;
	}
	return 1;
}

/*
 * Returns the data that matches the user provided key.
 */
void *list_find_if(listnode *start, list_predicate p, void *arg)
{
    listnode *cur = start->next;
    while (cur != start)
    {
        if (p(cur->data, arg))
        {
            return cur->data;
        }

        cur = cur->next;
    }
	return NULL;
}

void * list_remove_if(listnode *start, list_predicate p, void *arg)
{
    listnode *cur = start->next;
    while (cur != start)
    {
        if (p(cur->data, arg))
        {
            void *data = cur->data;
            list_destroy_node(start, cur);
            return data;
        }
        cur = cur->next;
    }
    return NULL;
}

void *list_get_ith_item(listnode *start, uint32_t item)
{
    uint32_t n = list_size(start);
    if (item >= n) {
        return NULL;
    }
    else if (item <= n / 2)
    {
        listnode* cur = start->next;
        while (item != 0)
        {
            cur = cur->next;
            item--;
        }
        return cur->data;
    }
    else
    {
        listnode *cur = start;
        do {
            cur = cur->prev;
            item++;
        } while (item != n);
        return cur->data;
    }
}

/*
 * Return the total number of nodes in the list.
 */
uint32_t list_size(listnode *start)
{
    return ((header_data *)(start->data))->size;
}

#if 0
static void* list_front(listnode* start)
{
    return (start->next == start) ? NULL : start->next->data;
}

static void* list_back(listnode* start)
{
    return (start->prev == start) ? NULL : start->prev->data;
}
#endif

/*
 * A set of routines for iteratively traversing through
 * the linked list.
 */

/*
 * Opens the list for traversal.
 */
int32_t	list_traverse_start(listnode *start)
{
	header_data*	head = (header_data*) start->data;

	if (head->cur_item) {
		// if the next item is not null, a
		// traversal is already in progress.
		printf("Error: list is already opened for traversal.\n");
		return -1;
	}

	head->cur_item = start;

	return 1;
}

/*
 * Fetches the next item in the list.
 * If all the items have been fetched,
 * returns null.
 */
void* list_traverse_next(listnode *start)
{
	header_data*	head = (header_data*) start->data;

	if (!head->cur_item) {
		// asking for the next item without
		// starting the traversal.
		printf("Error: list must be opened for traversal.\n");
		return NULL;
	}

    head->cur_item = head->cur_item->next;
    if (head->cur_item == start)
    {
        return NULL;
    }
    else
    {
        return head->cur_item->data;
    }
}

/*
 * Finishes the list traversal.
 */
int32_t	list_traverse_end(listnode *start)
{
	header_data*	head = (header_data*) start->data;

	if (!head->cur_item) {
		// closing without
		// starting the traversal.
		printf("Error: list must be opened before ending.\n");
		return -1;
	}

	head->cur_item = NULL;

	return 1;
}

int list_visit(listnode *start, list_visitor_func func, void *arg)
{
    listnode* cur = start->next;
    while (cur != start && func(cur->data, arg))
    {
        cur = cur->next;
    }
    return cur == start;
}
