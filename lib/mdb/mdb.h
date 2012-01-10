
// this ix max log(n) so can easily fit in 1 byte
typedef uint8_t mdb_level_t;

struct mdb_node {
	struct mdb_node *parent_object;
	struct mdb_node *left, *right;
	mdb_level_t level; 
};

#define MDB_ERR_DUPLICATE 1
#define MDB_ERR_NOTFOUND 2

enum {
	MDB_INVARIANT_OK = 0,
	MDB_INVARIANT_BOTHCHILDREN,
	MDB_INVARIANT_LEFT_LEVEL_LESS,
	MDB_INVARIANT_RIGHT_LEVEL_LEQ,
	MDB_INVARIANT_RIGHTRIGHT_LEVEL_LESS,
	MDB_INVARIANT_RIGHTLEFT_LEVEL_LESS,
}

int check_invariants();
