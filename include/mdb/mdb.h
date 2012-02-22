#ifndef LIBMDB_MDB_H
#define LIBMDB_MDB_H

#include <barrelfish/types.h>

struct cte;

typedef uint8_t mdb_root_t;
typedef uint8_t mdb_level_t;

/**
 * \brief A mapping database node.
 */
struct mdbnode {
	struct cte *left, *right;
	genpaddr_t end;
	mdb_root_t end_root;
	mdb_level_t level;
        bool revocable;
        bool remote_relations;
};

void set_init_mapping(struct cte *dest_start, size_t num);
bool has_descendants(struct cte *cte);
bool has_ancestors(struct cte *cte);
bool has_copies(struct cte *cte);
void remove_mapping(struct cte *cte);
errval_t mdb_get_copy(struct capability *cap, struct capability **ret);
bool mdb_is_sane(void);
void set_cap_remote(struct cte *cte, bool is_remote);
bool is_cap_remote(struct cte *cte);

#endif // LIBMDB_MDB_H
