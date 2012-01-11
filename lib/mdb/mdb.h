
// this is max log(n) so can easily fit in 1 byte
typedef uint8_t mdb_level_t;

struct mdbnode {
	struct cte *left, *right;
	genpaddr_t end;
	mdb_level_t level; 
};

struct cte {
	struct capability cap;
	struct mdbnode    mdbnode;

    char padding[(1UL << OBJBITS_CTE)
                 - sizeof(struct capability) - sizeof(struct mdbnode)];
}

#define MDB_ERR_DUPLICATE 1
#define MDB_ERR_NOTFOUND 2

enum {
	MDB_INVARIANT_OK = 0,
	MDB_INVARIANT_BOTHCHILDREN,
	MDB_INVARIANT_LEFT_LEVEL_LESS,
	MDB_INVARIANT_RIGHT_LEVEL_LEQ,
	MDB_INVARIANT_RIGHTRIGHT_LEVEL_LESS,
	MDB_INVARIANT_RIGHTLEFT_LEVEL_LESS,
	MDB_INVARIANT_END_IS_MAX,
};

enum {
	MDB_RANGE_NOT_FOUND = 0,
	MDB_RANGE_FOUND_SURROUNDING = 1,
	MDB_RANGE_FOUND_INNER = 2,
	MDB_RANGE_FOUND_PARTIAL = 3,
};

int check_invariants();

errval_t mdbtree_insert(struct cte *new_node);
errval_t mdbtree_remove(struct cte *node);
errval_t mdb_find_range(genpaddr_t address, size_t size, int max_result,
						struct cte **ret_node, int *result);
