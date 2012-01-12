
#include "capabilities.h"
#include <errors/errno.h>

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

int mdb_check_invariants();
errval_t mdb_insert(struct cte *new_node);
errval_t mdb_remove(struct cte *node);
errval_t mdb_find_range(mdb_root_t root, genpaddr_t address, size_t size,
                        int max_result, struct cte **ret_node, int *result);
