/**
 * \file
 * \brief Barrelfish collections library hash table
 */
/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _HASH_TABLE_H_
#define _HASH_TABLE_H_

#include "collections/list.h"

/*
 * a simple hash table. 
 */

typedef void (* hash_data_free)(void *);

typedef struct	_hash_table {
	// number of buckets in the table.
	int			num_buckets;

	// pointer to the buckets.
	listnode	**buckets;

	// total number of elements in the table.
	uint32_t	num_elems;

    // function that knows how to free inserted data resources
    hash_data_free data_free;

	// a pointer to keep track of 
	// traversing the hash table
	int32_t		cur_bucket_num;
} hash_table;

/*
 * Structure of a hash table element.
 */
typedef struct	_hash_elem {

	uint64_t	key;

	void	*data;
} hash_elem;

#define NUM_BUCKETS	1013

#ifdef __cplusplus
extern "C" {
#endif // __cplusplus


/*
 * functions ...
 */
void		hash_create(hash_table **t, hash_data_free f);
void		hash_create_with_buckets(hash_table **t, int num_buckets, hash_data_free f);
void		hash_release(hash_table *t);
void		hash_insert(hash_table *t, uint64_t key, void *data);
void*		hash_find(hash_table *t, uint64_t key);
void		hash_delete(hash_table *t, uint64_t key);
uint32_t	hash_size(hash_table *t);
int32_t		hash_traverse_start(hash_table* t);
void*		hash_traverse_next(hash_table* t, uint64_t *key);
int32_t		hash_traverse_end(hash_table* t);

/*
 * Visitor function: returns 0 when visit should be considered finish.
 */
typedef int (*hash_visitor_func)(uint64_t key, void *data, void *arg);

/*
 * Apply function to all elements in hash table or until function indicates
 * function application should stop.
 *
 * Returns non-zero if all elements in table visited, 0 otherwise.
 */
int    hash_visit(hash_table *t, hash_visitor_func hash_visitor, void *arg);

#ifdef __cplusplus
}
#endif // __cplusplus

#endif
