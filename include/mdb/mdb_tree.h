#ifndef LIBMDB_MDB_TREE_H
#define LIBMDB_MDB_TREE_H

#include <errors/errno.h>
#include <barrelfish/types.h>

struct cte;
struct capability;

enum {
    MDB_INVARIANT_OK = 0,
    MDB_INVARIANT_BOTHCHILDREN,
    MDB_INVARIANT_LEFT_LEVEL_LESS,
    MDB_INVARIANT_RIGHT_LEVEL_LEQ,
    MDB_INVARIANT_RIGHTRIGHT_LEVEL_LESS,
    MDB_INVARIANT_RIGHTLEFT_LEVEL_LESS,
    MDB_INVARIANT_END_IS_MAX,
    MDB_INVARIANT_LEFT_SMALLER,
    MDB_INVARIANT_RIGHT_GREATER,
};

enum {
    MDB_RANGE_NOT_FOUND = 0,
    MDB_RANGE_FOUND_SURROUNDING = 1,
    MDB_RANGE_FOUND_INNER = 2,
    MDB_RANGE_FOUND_PARTIAL = 3,
};

void mdb_dump(struct cte *cte, int indent);
void mdb_dump_all_the_things(void);
int mdb_check_invariants(void);
errval_t mdb_insert(struct cte *new_node);
errval_t mdb_remove(struct cte *node);
struct cte *mdb_predecessor(struct cte *current);
struct cte *mdb_successor(struct cte *current);
struct cte *mdb_find_equal(struct capability *cap);
struct cte *mdb_find_less(struct capability *cap, bool equal_ok);
struct cte *mdb_find_greater(struct capability *cap, bool equal_ok);
errval_t mdb_find_range(uint8_t root, genpaddr_t address, gensize_t size,
                        int max_result, struct cte **ret_node, int *result);

#endif // LIBMDB_MDB_TREE_H
