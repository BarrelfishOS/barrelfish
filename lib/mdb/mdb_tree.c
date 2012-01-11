#include <mdb.h>
#include <assert.h>

#ifndef MIN
#define MIN ((a)<(b)?(a):(b))
#endif
#ifndef MAX
#define MIN ((a)>(b)?(a):(b))
#endif

#ifdef N
#undef N
#endif
#define N(cte) (&(cte)->mdbnode)
#ifdef C
#undef C
#endif
#define C(cte) (&(cte)->cap)

struct cte *root = NULL;

/*
 * invariants
 */

static int
mdb_check_subtree_invariants(struct cte *cte)
{
    int err;

    if (!cte) {
        return MDB_INVARIANT_OK;
    }

    struct mdbnode *node = N(cte);

    if (node->level > 0 && !(node->left && node->right)) {
        return MDB_INVARIANT_BOTHCHILDREN;
    }
    if (node->left && !(N(node->left)->level < node->level)) {
        return MDB_INVARIANT_LEFT_LEVEL_LESS;
    }
    if (node->right && !(N(node->right)->level <= node->level)) {
        return MDB_INVARIANT_RIGHT_LEVEL_LEQ;
    }
    if (node->right && N(node->right)->right &&
        !(N(N(node->right)->right)->level < node->level)) {
        return MDB_INVARIANT_RIGHTRIGHT_LEVEL_LESS;
    }
    if (node->right && N(node->right)->left &&
        !(N(N(node->right)->left)->level < node->level)) {
        return MDB_INVARIANT_RIGHTLEFT_LEVEL_LESS;
    }
    if (caps_has_address(cte) && !(node->end ==
        MAX(caps_address(cte)+caps_size(cte),
            MAX((node->left ? N(node->left)->end : 0),
                (node->right ? N(node->right)->end : 0))))) {
        return MDBTREE_INVARIANT_END_IS_MAX;
    }

    err = mdb_check_subtree_invariants(node->left);
    if (err) {
        return err;
    }

    err = mdb_check_subtree_invariants(node->right);
    if (err) {
        return err;
    }

    return MDB_INVARIANT_OK;
}

int
mdb_check_invariants()
{
    return mdb_check_subtree_invariants(root)
}

/*
 * general internal helpers
 */

static void
mdb_update_end(struct cte *cte)
{
    if (!cte) {
        return;
    }
    struct mdbnode *node = N(cte);
    if (!caps_has_address(cte)) {
        node->end = 0;
        return;
    }
    node->end = caps_address(cte) + caps_size(cte);
    if (node->left) {
        node->end = MAX(node->end, N(node->left)->end);
    }
    if (node->right) {
        node->end = MAX(node->end, N(node->right)->end);
    }
}

static cte*
mdb_skew(struct cte *node)
{
    /* transform invalid state
     *
     *               |
     *      |L|<---|T|
     *     /   \      \
     *   |A|   |B|    |R|
     *
     * to valid equivalent state
     *
     *       |
     *      |L|--->|T|
     *     /      /   \
     *   |A|    |B|   |R|
     *
     */
    if (!node || !N(node)->left) {
        return node;
    }
    else if (N(node)->level == N(N(node)->left)->level) {
        struct cte *left = N(node)->left;
        N(node)->left = N(left)->right;
        N(left)->right = node;
        mdb_update_end(node);
        mdb_update_end(left);
        return left;
    }
    else {
        return node;
    }
}

static cte*
mdb_split(struct cte *node)
{
    /* transform invalid state
     *
     *       |
     *      |T|--->|R|-->|X|
     *     /      /
     *   |A|    |B|
     *
     * to valid equivalent state
     *
     *             |
     *            |R|
     *           /   \
     *         |T|   |X|
     *        /   \
     *      |A|   |B|
     *
     */
    if (!node || !N(node)->right || !N(N(node)->right)->right) {
        return node;
    }
    else if (N(node)->level == N(N(N(node)->right)->right)->level) {
        struct cte *right = N(node)->right;
        N(node)->right = N(right)->left;
        N(right)->left = cte;
        N(right)->level += 1;
        mdb_update_end(node);
        mdb_update_end(right);
        return right;
    }
    else {
        return node;
    }
}

static void
mdb_decrease_level(struct cte *node)
{
    assert(node);

    mdb_level_t expected;
    if (!N(node)->left || !N(node)->right) {
        expected = 0;
    }
    else {
        expected = MIN(N(N(node)->left)->level, N(N(node)->right)->level) + 1;
    }

    if (expected < N(node)->level) {
        N(node)->level = expected;
        if (N(node)->right && expected < N(N(node)->right)->level) {
            N(N(node)->right)->level = expected;
        }
    }
}

static cte*
mdb_rebalance(struct cte *node)
{
    assert(node);
    mdb_update_end(node);
    mdb_decrease_level(node);
    node = mdb_skew(node);
    N(node)->right = mdb_skew(N(node)->right);
    if (N(node)->right) {
        N(N(node)->right)->right = mdb_skew(N(N(node)->right)->right);
    }
    node = mdb_split(node);
    N(node)->right = mdb_split(N(node)->right);
    return node;
}

static bool
mdb_is_child(struct cte *child, struct cte *parent)
{
    if (!parent) {
        return root == child;
    }
    else {
        return N(parent)->left == child || N(parent)->right == child;
    }
}

static bool
mdb_is_inside(genpaddr_t outer_begin, genpaddr_t outer_end,
              genpaddr_t inner_begin, genpaddr_t inner_end)
{
    return
        (inner_begin >= outer_begin && inner_end < outer_end) ||
        (inner_begin > outer_begin && inner_end <= outer_end);
}

/*
 * operations and operation-specific helpers
 */

static errval_t
mdb_sub_insert(struct cte *new_node, struct cte **current)
{
    assert(new_node);
    assert(current);

    struct cte *current_ = *current;

    if (!current_) {
        *current = new_node;
        return SYS_ERR_OK;
    }

    int compare = caps_compare(C(new_node), C(current_));
    if (compare < 0) {
        // new_node < current
        return mdb_sub_insert(new_node, &N(current_)->left);
    }
    else if (compare > 0) {
        // new_node > current
        return mdb_sub_insert(new_node, &N(current_)->right);
    }
    else {
        return MDB_ERR_DUPLICATE;
    }

    mdb_update_end(current_);
    current_ = mdb_skew(current_);
    current_ = mdb_split(current_);
    *current = current_;

    return SYS_ERR_OK;
}

errval_t
mdb_insert(struct cte *new_node)
{
    return mdb_sub_insert(new_node, &root);
}

static void
mdb_exchange_child(struct cte *first, struct cte *first_parent,
                   struct cte *second)
{
    assert(mdb_is_child(first, first_parent));

    if (!first_parent) {
        root = second;
    }
    else if (N(first_parent)->left == first) {
        N(first_parent)->left = second;
    }
    else if (N(first_parent)->right == first) {
        N(first_parent)->right = second;
    }
    else {
        USER_PANIC("first is not child of first_parent");
    }
}

static void
mdb_exchange_nodes(struct cte *first, struct cte *first_parent,
                   struct cte *second, struct cte *second_parent)
{
    struct cte *tmp_node;
    mdb_level_t tmp_level;

    mdb_exchange_child(first, first_parent, second);
    mdb_exchange_child(second, second_parent, first);

    tmp_node = N(first)->left;
    N(first)->left = N(second)->left;
    N(second)->left = tmp_node;

    tmp_node = N(first)->right;
    N(first)->right = N(second)->right;
    N(second)->right = tmp_node;

    tmp_level = N(first)->level;
    N(first)->level = N(second)->level;
    N(second)->level = tmp_level;

    mdb_update_end(first);
    mdb_update_end(second);
}

static void
mdb_exchange_remove(struct cte *target, struct cte *target_parent,
                    struct cte **current, struct cte *parent,
                    int dir, struct cte **ret_target)
{
    errval_t err;
    assert(current);
    assert(target);
    assert(*current);
    assert(parent)
    assert(ret_target);
    assert(!*ret_target);
    assert(dir != 0);
    assert(mdb_check_subtree_invariants(*current) == 0);
    assert(caps_compare(C(target), C(*current)) != 0);
    assert(mdb_is_child(target, target_parent));
    assert(mdb_is_child(*current, parent));

    struct cte *current_ = *current;

    if (dir > 0) {
        if (parent == target) {
            assert(N(parent)->left == current_);
        }
        else {
            assert(N(parent)->right == current_);
        }

        if (N(current_)->right) {
            mdb_exchange_remove(target, target_parent, &N(current_)->right,
                                current_, dir, ret_target);
            assert(mdb_check_subtree_invariants(N(current_)->right));
        }
    }
    else if (dir < 0) {
        if (parent == target) {
            assert(N(parent)->right == current_);
        }
        else {
            assert(N(parent)->left == current_);
        }

        if (current_->left) {
            mdb_exchange_remove(target, target_parent, &N(current_)->left,
                                current_, dir, ret_target);
            assert(mdb_check_subtree_invariants(N(current_)->left));
        }
        else if (current_->right) {
            // right is null, left non-null -> current is level 0 node with
            // horizontal right link, and is also the successor of the target.
            // in this case, exchange current and current right, then current
            // (at its new position) and the target.
            struct cte *new_current = N(current_)->right;
            mdb_exchange_nodes(current_, parent, N(current_)->right, current_);
            mdb_exchange_nodes(target, target_parent, current_, new_current);
            // "current" is now located where the target was, further up in the
            // tree. "new_current" is the node where current was. "target" is
            // where current->right was, and is a leaf, so can be dropped.
            assert(N(new_current)->right == target);
            new_current->right = NULL;
            assert(mdb_check_subtree_invariants(new_current));
            *ret_target = current_;
            *current = new_current;
            return;
        }
    }

    if (*ret_target) {
        // implies we recursed further down to find a leaf. need to rebalance.
        current_ = mdb_rebalance(current_);
        assert(mdb_check_subtree_invariants(current_));
        *current = current_;
    }
    else {
        // found successor/predecessor leaf, exchange with target
        assert(!current->right && !current->left);
        mdb_exchange_nodes(target, target_parent, current, parent);

        // "current" is now where target was, so set as ret_target
        *ret_target = current;
        // target would be the new current, but we're removing it, so set
        // current to null. This also sets parent's corresponding child to
        // null by recursion.
        *current = NULL;
    }
}

static errval_t
mdb_subtree_remove(struct cte *target, struct cte **current, struct cte *parent)
{
    errval_t err;
    assert(current);
    struct cte *current_ = *current;
    assert(mdb_check_subtree_invariants(current_));
    if (!current_) {
        return MDB_ERR_NOTFOUND;
    }

    int compare = caps_compare(C(target), C(current_));
    if (compare > 0) {
        return mdb_subtree_remove(target, &N(current_)->right, current_);
    }
    else if (compare < 0) {
        return mdb_subtree_remove(target, &N(current_)->right, current_);
    }
    else {
        assert(current_ == target);
        if (!N(current_)->left && !N(current_)->right) {
            // target is leaf, just remove
            *current = NULL;
            return SYS_ERR_OK;
        }
        else if (!N(current_)->left) {
            // move to right child then go left (dir=-1)
            struct cte *new_current = NULL;
            struct cte *new_right = N(current_)->right;
            mdb_exchange_remove(target, parent, &new_right, current_, -1,
                                &new_current);
            assert(new_current);
            current_ = new_current;
            N(current_)->right = new_right;
        }
        else {
            // move to left child then go right (dir=1)
            struct cte *new_current = NULL;
            struct cte *new_left = N(current_)->left;
            mdb_exchange_remove(target, parent, &new_left, current_, -1,
                                &new_current);
            assert(new_current);
            current_ = new_current;
            N(current_)->left = new_left;
        }
    }

    // rebalance after remove from subtree
    current_ = mdb_rebalance(current_);
    assert(mdb_check_subtree_invariants(current_));
    *current = current_;
}

errval_t
mdb_remove(struct cte *target)
{
    return mdb_subtree_remove(target, &root, NULL);
}

static struct cte*
mdb_sub_find_last_le(struct capability *cap, struct cte* current)
{
    if (!current) {
        return NULL
    }
    int compare = caps_compare(cap, C(current));
    if (compare < 0) {
        // current is gt key, look for smaller node
        return mdb_sub_find_last_le(cap, N(current)->left);
    }
    else if (compare > 0) {
        // current is lt key, attempt to find bigger current
        struct mdbnode *res = mdb_sub_find_last_le(cap, N(current)->right);
        if (res) {
            return res;
        }
        // bigger child exceeded key
        return current;
    }
    else {
        // found equal element
        return current;
    }
}

struct cte*
mdb_find_last_le(struct capability *cap)
{
    return mdb_sub_find_last_le(cap, root);
}

static cte*
mdb_sub_find_first_gt(struct capability *cap, struct cte *current)
{
    if (!current) {
        return NULL;
    }
    int compare = caps_compare(cap, C(current));
    if (compare < 0) {
        // current is gt key, attempt to find smaller node
        struct cte *res = mdb_sub_find_first_gt(cap, N(current)->left);
        if (res) {
            return res;
        }
        // smaller was lte key
        return current;
    }
    else if (compare >= 0) {
        // current is lte key, look for greater node
        return mdb_sub_find_first_gt(cap, N(current)->right);
    }
}

struct cte*
mdb_find_first_gt(struct capability *cap)
{
    return mdb_sub_find_first_gt(cap, root);
}

static cte*
mdb_choose_surrounding(genpaddr_t address, size_t size,
                       struct capability* first, struct capability* second)
{
    assert(first);
    assert(second);
#ifndef NDEBUG
    genpaddr_t beg = address, end = address + size;
    genpaddr_t fst_beg = cap_address(first);
    genpaddr_t snd_beg = cap_address(second);
    genpaddr_t fst_end = fst_beg + cap_size(first);
    genpaddr_t snd_end = snd_beg + cap_size(second);
    assert(fst_beg <= beg && fst_end >= end);
    assert(snd_beg <= beg && snd_end >= end);
#endif

    if (cap_compare(first, second) >= 0) {
        return first;
    }
    else {
        return second;
    }
}

static cte*
mdb_choose_inner(genpaddr_t address, size_t size, struct capability* first,
                 struct capability* second)
{
    assert(first);
    assert(second);
#ifndef NDEBUG
    genpaddr_t beg = address, end = address + size;
    genpaddr_t fst_beg = cap_address(first);
    genpaddr_t snd_beg = cap_address(second);
    genpaddr_t fst_end = fst_beg + cap_size(first);
    genpaddr_t snd_end = snd_beg + cap_size(second);
    assert(mdb_is_inside(address, size, fst_beg, fst_end));
    assert(mdb_is_inside(address, size, snd_beg, snd_end));
#endif

    if (cap_compare(first, second) <= 0) {
        return first;
    }
    else {
        return second;
    }
}

static cte*
mdb_choose_partial(genpaddr_t address, size_t size, struct capability* first,
                   struct capability *second)
{
    assert(first);
    assert(second);
#ifndef NDEBUG
    genpaddr_t beg = address, end = address + size;
    genpaddr_t fst_beg = cap_address(first);
    genpaddr_t snd_beg = cap_address(second);
    genpaddr_t fst_end = fst_beg + cap_size(first);
    genpaddr_t snd_end = snd_beg + cap_size(second);
    assert(fst_beg < end);
    assert(snd_beg < end);
    assert(fst_end > beg);
    assert(snd_end > beg);
    assert(fst_beg != beg);
    assert(snd_beg != beg);
    assert(fst_end != end);
    assert(snd_end != end);
    assert((fst_beg < beg) == (fst_end < end));
    assert((snd_beg < beg) == (snd_end < end));
#endif

    if (fst_beg < beg && snd_beg > beg) {
        return first;
    }
    else if (snd_beg < beg && fst_beg > beg) {
        return second;
    }
    else {
        if (cap_compare(first, second) >= 0) {
            return first;
        }
        else {
            return second;
        }
    }
}

static struct cte*
mdb_choose_result(genpaddr_t address, size_t size, int ret, struct cte* first,
                  struct cte* second)
{
    struct cte* result = NULL;
    switch (ret) {
    case MDB_RANGE_NOT_FOUND:
        break;
    case MDB_RANGE_FOUND_SURROUNDING:
        result = mdb_choose_surrounding(address, size, first, second);
        break;
    case MDB_RANGE_FOUND_INNER:
        result = mdb_choose_inner(address, size, first, second);
        break;
    case MDB_RANGE_FOUND_PARTIAL:
        result = mdb_choose_partial(address, size, first, second);
        break;
    default:
        USER_PANIC_ERR("Unhandled enum value for mdb_find_range result");
        break;
    }
    return result;
}

static void
mdb_sub_find_range_merge(genpaddr_t address, size_t size, int max_precision,
                         struct cte *sub, int *ret, struct cte **result)
{
    assert(sub);
    assert(ret);
    assert(result);
    assert(*ret <= max_precision);
    assert((!*result) == (*ret == MDB_RANGE_NOT_FOUND));

    struct cte *sub_result = NULL;
    int sub_ret = mdb_sub_find_range(address, size, max_precision,
                                     sub, &sub_result);
    if (sub_ret > max_precision) {
        *ret = NULL;
        return sub_ret;
    }
    else if (sub_ret > *ret) {
        *result = sub_result;
        *ret = sub_ret;
    }
    else if (sub_ret == *ret) {
        switch (ret) {
        case MDB_RANGE_NOT_FOUND:
            break;
        case MDB_RANGE_FOUND_SURROUNDING:
            *result = mdb_choose_surrounding(address, size, *result, sub_result);
            break;
        case MDB_RANGE_FOUND_INNER:
            *result = mdb_choose_inner(address, size, *result, sub_result);
            break;
        case MDB_RANGE_FOUND_PARTIAL:
            *result = mdb_choose_partial(address, size, *result, sub_result);
            break;
        default:
            USER_PANIC_ERR("Unhandled enum value for mdb_find_range result");
            break;
        }
    }
    // else ret > sub_ret, keep ret & result as is
}

static int
mdb_sub_find_range(genpaddr_t address, size_t size, int max_precision,
                   struct cte *current, struct cte **ret_node)
{
    assert(max_precision >= 0);

    if (!current) {
        *ret_node = NULL;
        return MDB_RANGE_NOT_FOUND;
    }

    if (N(current)->end <= address) {
        *ret_node = NULL;
        return MDB_RANGE_NOT_FOUND;
    }

    genpaddr_t current_address = cap_address(C(current));
    genpaddr_t current_end = current_address + cap_size(C(current));
    genpaddr_t search_end = address + size;

    struct cte *result = NULL;
    int ret = MDB_RANGE_NOT_FOUND;

    if (ret < MDB_RANGE_FOUND_PARTIAL &&
        current_address > address &&
        current_address < search_end &&
        current_end > search_end)
    {
        result = current;
        ret = MDB_RANGE_FOUND_PARTIAL;
    }
    if (ret < MDB_RANGE_FOUND_PARTIAL &&
        current_end > address &&
        current_end < search_end &&
        current_address < address)
    {
        result = current;
        ret = MDB_RANGE_FOUND_PARTIAL;
    }
    if (ret < MDB_RANGE_FOUND_INNER &&
        mdb_is_inside(address, search_end, current_address, current_end))
    {
        result = current;
        ret = MDB_RANGE_FOUND_INNER;
    }
    if (ret < MDB_RANGE_FOUND_SURROUNDING &&
        (current_address <= address && current_end >= search_end))
    {
        result = current;
        ret = MDB_RANGE_FOUND_SURROUNDING;
    }
    if (ret > max_precision) {
        *ret_node = NULL;
        return ret;
    }

    if (N(current)->left) {
        mdb_sub_find_range_merge(address, size, max_precision,
                                 N(current)->left, &ret, &result);
        if (ret > max_precision) {
            *ret_node = NULL;
            return ret;
        }
    }

    if (N(current)->right && search_end > current_address) {
        mdb_sub_find_range_merge(address, size, max_precision,
                                 N(current)->right, &ret, &result);
        if (ret > max_precision) {
            *ret_node = NULL;
            return ret;
        }
    }

    *ret_node = result;
    return ret;

}

errval_t
mdb_find_range(genpaddr_t address, size_t size, int max_result,
               struct cte **ret_node, int *result)
{
    if (max_result < MDB_RANGE_NOT_FOUND || max_result > MDB_RANGE_FOUND_PARTIAL) {
        return SYS_ERR_INVALID_ARGS;
    }
    if (max_result > MDB_RANGE_NOT_FOUND && !ret_node) {
        return SYS_ERR_INVALID_ARGS;
    }

    struct cte *alt_ret_node;
    if (!ret_node) {
        ret_node = &alt_ret_node;
    }

    *result = mdb_sub_find_range(address, size, max_result, root, ret_node);
    return SYS_ERR_OK;
}
