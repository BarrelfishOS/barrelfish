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

#include "collections/hash_table.h"
#include "inttypes.h"

/******************************************************
 * a simple hash table implementation
 ******************************************************/

/*
 * Function to identify the right element from the
 * linked list.
 */
static int32_t match_key(void *data, void *arg)
{
	hash_elem *elem = (hash_elem *) data;
	uint64_t key  = *((uint64_t *)arg);

    return (elem->key == key);
}

/*
 * Create a hash table.
 */
static void hash_create_core(hash_table **t, int num_buckets, hash_data_free data_free)
{
	int i;

	*t = (hash_table *) malloc (sizeof(hash_table));
	memset(*t, 0, sizeof(hash_table));

	(*t)->num_buckets = num_buckets;

	// create a linked list node for each bucket
	(*t)->buckets = (listnode **) malloc(sizeof(listnode *) * num_buckets);
	for (i = 0; i < num_buckets; i ++) {
		list_create(&(*t)->buckets[i], NULL);
	}

	(*t)->num_elems = 0;
    (*t)->data_free = data_free;

	// to keep track of traversing the hash table
	(*t)->cur_bucket_num = -1;
	return;
}

void hash_create(hash_table **t, hash_data_free elem_free)
{
	hash_create_core(t, NUM_BUCKETS, elem_free);
}

void hash_create_with_buckets(hash_table **t, int num_buckets, hash_data_free elem_free)
{
	hash_create_core(t, num_buckets, elem_free);
}

static int hash_release_elem(void* elem, void * arg)
{
    hash_table *t = (hash_table *)arg;
    hash_elem *he = (hash_elem *)elem;
    if (t->data_free)
    {
        t->data_free(he->data);
    }
    free(he);

	t->num_elems--;

    return 1;
}

// delete the entire hash table
void hash_release(hash_table *t)
{
	int bucket_num;
	int bucket_size;
	listnode *bucket;

	for (bucket_num = 0; bucket_num < t->num_buckets; bucket_num ++) {
        uint32_t before, after;
		bucket = t->buckets[bucket_num];
		bucket_size = list_size(bucket);
        
        before = t->num_elems;
        list_visit(bucket, hash_release_elem, t);
        after = t->num_elems;
        assert(before - after == bucket_size);

        list_release(bucket);
	}
    assert(t->num_elems == 0);

	free(t->buckets);
	free(t);
}

static hash_elem* hash_find_elem(hash_table *t, uint64_t key)
{
	uint32_t bucket_num;	
	listnode *bucket;	
	hash_elem *elem;

	bucket_num = key % t->num_buckets;
	bucket = t->buckets[bucket_num];
	elem = (hash_elem*) list_find_if(bucket, match_key, &key);
    return elem;
}

/*
 * Inserts an element into the hash table.
 */
void hash_insert(hash_table *t, uint64_t key, void *data)
{
	uint32_t bucket_num;
	listnode *bucket;
	hash_elem *elem;

    elem = hash_find_elem(t, key);
	if (elem != NULL) {
		printf("Error: key %" PRIu64 " already present in hash table %" PRIu64 "\n",
            key, elem->key);
		assert(0);
		return;
	}

	bucket_num = key % t->num_buckets;
	bucket = t->buckets[bucket_num];
	elem = (hash_elem *) malloc(sizeof(hash_elem));
	elem->key = key;
	elem->data = data;
	list_insert(bucket, (void *)elem);
	t->num_elems ++;
}

/*
 * Retrieves an element from the hash table.
 */
void *hash_find(hash_table *t, uint64_t key)
{
    hash_elem *he = hash_find_elem(t, key);
    return (he) ? he->data : NULL;
}

/*
 * Removes a specific element from the table.
 */
void hash_delete(hash_table *t, uint64_t key)
{	
	uint32_t bucket_num;	
	listnode *bucket;	
	hash_elem *elem;

	bucket_num = key % t->num_buckets;
	bucket = t->buckets[bucket_num];
	elem = (hash_elem*) list_remove_if(bucket, match_key, &key);
	if (elem) {
        uint32_t n = t->num_elems;
        hash_release_elem(elem, t);
        assert(1 == n - t->num_elems);
	}
    else
    {
	    printf("Error: cannot find the node with key %" PRIu64 " in hash_release\n", key);
    }
}

/*
 * Returns the number of elements in the hash table.
 */
uint32_t hash_size(hash_table *t)
{
	return (t->num_elems);
}

static listnode* hash_get_next_valid_bucket(hash_table* t)
{
	listnode* bucket;

	do {
		t->cur_bucket_num ++;
		if (t->cur_bucket_num < t->num_buckets) {
			if (!t->buckets[t->cur_bucket_num]) {
				continue;
			}
		} else {
			return NULL;
		}
	} while (list_size(t->buckets[t->cur_bucket_num]) <= 0);

	bucket = t->buckets[t->cur_bucket_num];
	list_traverse_start(bucket);

	return bucket;
}

int32_t hash_traverse_start(hash_table *t)
{
	if (t->cur_bucket_num != -1) {
		// if the cur_bucket_num is valid, a
		// traversal is already in progress.
		printf("Error: hash_table is already opened for traversal.\n");
		return -1;
	}

	hash_get_next_valid_bucket(t);

	return 1;
}

/*
 * Returns the next element in the hash table. If
 * a valid element is found, the key is set to the
 * key of the element. If there is no valid element,
 * returns null and key is not modified.
 */
void* hash_traverse_next(hash_table* t, uint64_t *key)
{
	if (t->cur_bucket_num == -1) {
		// if the cur_bucket_num is invalid, 
		// hash traversal has not been started.
		printf("Error: hash_table must be opened for traversal first.\n");
		return NULL;
	}
	
	if (t->cur_bucket_num >= t->num_buckets) {
		// all the buckets have been traversed.
		return NULL;
	} else {
		listnode*	bucket;
		hash_elem*	ret;

		if (t->buckets[t->cur_bucket_num]) {
			bucket = t->buckets[t->cur_bucket_num];
			ret = (hash_elem*) list_traverse_next(bucket);

			if (ret) {
				*key = ret->key;
				return ret->data;
			} else {
				// this list traversal is over.
				// let's close it.
				list_traverse_end(bucket);
			}
		}

		bucket = hash_get_next_valid_bucket(t);
		if (!bucket) {
			return NULL;
		} else {
			ret = (hash_elem*) list_traverse_next(bucket);
			assert(ret != NULL);
		}

		*key = ret->key;
		return ret->data;
	}
}

int32_t	hash_traverse_end(hash_table* t)
{
	if (t->cur_bucket_num == -1) {
		// if the cur_bucket_num is invalid, 
		// hash traversal has not been started.
		printf("Error: hash_table must be opened for traversal first.\n");
		return -1;
	}

	t->cur_bucket_num = -1;
	return 1;
}

struct hash_visitor_tuple
{
    hash_visitor_func func;
    void *arg;
};

static int hash_visit0(void* list_data, void* arg)
{
    struct hash_visitor_tuple *t = (struct hash_visitor_tuple *)arg;
    hash_elem *he = (hash_elem*)list_data;
    return t->func(he->key, he->data, t->arg);
}

int hash_visit(hash_table* t, hash_visitor_func func, void* arg)
{
    struct hash_visitor_tuple tuple = { func, arg };
    int i = 0;
    while (i < t->num_buckets)
    {
        if (list_visit(t->buckets[i], hash_visit0, &tuple) == 0) {
            break;
        }
        i++;
    }

    return (i == t->num_buckets);
}
