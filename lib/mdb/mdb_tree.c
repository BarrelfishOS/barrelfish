
#include "mdb.h"

#ifndef MIN
#define MIN ((a)<(b)?(a):(b))
#endif

struct mdb_node *root = NULL;

static int
check_subtree_invariants(mdb_node *node)
{
    int err;

    if (!node) {
        return MDB_INVARIANT_OK;
    }

    if (node->level > 0 && !(node->left && node->right)) {
        return MDB_INVARIANT_BOTHCHILDREN;
    }
    if (node->left && !(node->left->level < node->level)) {
        return MDB_INVARIANT_LEFT_LEVEL_LESS;
    }
    if (node->right && !(node->right->level <= node->level)) {
        return MDB_INVARIANT_RIGHT_LEVEL_LEQ;
    }
    if (node->right && node->right->right &&
        !(node->right->right->level < node->level)) {
        return MDB_INVARIANT_RIGHTRIGHT_LEVEL_LESS;
    }
    if (node->right && node->right->left &&
        !(node->right->left->level < node->level)) {
        return MDB_INVARIANT_RIGHTLEFT_LEVEL_LESS;
    }

    err = check_subtree_invariants(node->left);
    if (err) {
        return err;
    }

    err = check_subtree_invariants(node->right);
    if (err) {
        return err;
    }

    return MDB_INVARIANT_OK;
}

int
check_invariants()
{
    return check_subtree_invariants(root)
}

static mdb_node*
skew(struct mdb_node *node)
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
    if (!node || !node->left) {
        return node;
    }
    else if (node->level == node->left->level) {
        struct mdb_node *left = node->left;
        node->left = left->right;
        left->right = node;
        return left;
    }
    else {
        return node;
    }
}

static mdb_node*
split(struct mdb_node *node)
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
    if (!node || !node->right || !node->right->right) {
        return node;
    }
    else if (node->level == node->right->right->level) {
        struct mdb_node *right = node->right;
        node->right = right->left;
        right->left = node;
        right->level += 1;
        return right;
    }
    else {
        return node;
    }
}

static void
decrease_level(struct mdb_node *node)
{
    assert(node);

    mdb_level_t expected;
    if (!node->left || !node->right) {
        expected = 0;
    }
    else {
        expected = MIN(node->left->level, node->right->level) + 1;
    }

    if (expected < node->level) {
        node->level = expected;
        if (node->right && expected < node->right->level) {
            node->right->level = expected;
        }
    }
}

static mdb_node*
rebalance(mdb_node *node)
{
    decrease_level(node);
    node = skew(node);
    node->right = skew(node->right);
    if (node->right) {
        node->right->right = skew(node->right->right);
    }
    node = split(node);
    node->right = split(node->right);
    return node;
}

static errval_t
subtree_insert(struct mdb_node *new_node, struct mdb_node **current)
{
    assert(new_node);
    assert(current);

    struct mdb_node *curr = *current;

    if (!curr) {
        *current = new_node;
        return SYS_ERR_OK;
    }

    int compare = COMPARE(KEY(new_node), KEY(curr));
    if (compare < 0) {
        // new_node < current
        return subtree_insert(new_node, &curr->left);
    }
    else if (compare > 0) {
        // new_node > current
        return subtree_insert(new_node, &curr->right);
    }
    else {
        return MDB_ERR_DUPLICATE;
    }

    curr = skew(curr);
    curr = split(curr);
    *current = curr;

    return SYS_ERR_OK;
}

errval_t
insert(struct mdb_node *new_node)
{
    return subtree_insert(new_node, &root);
}

static bool
is_child(struct mdb_node *child,
                     struct mdb_node *parent)
{
    if (!parent) {
        return root == child;
    }
    else {
        return parent->left == child || parent->right == child;
    }
}

static mdb_node*
rebalance(mdb_node *node)
{
    decrease_level(node);
    node = skew(node);
    node->right = skew(node->right);
    if (node->right) {
        node->right->right = skew(node->right->right);
    }
    node = split(node);
    node->right = split(node->right);
    return node;
}

static void
exchange_child(struct mdb_node *first, struct mdb_node *first_parent,
               struct mdb_node *second)
{
    assert(is_child(first, first_parent));

    if (!first_parent) {
        root = second;
    }
    else if (first_parent->left == first) {
        first_parent->left = second;
    }
    else if (first_parent->right == first) {
        first_parent->right = second;
    }
    else {
        USER_PANIC("first is not child of first_parent");
    }
}

static void
exchange_nodes(struct mdb_node *first, struct mdb_node *first_parent,
               struct mdb_node *second, struct mdb_node *second_parent)
{
    struct mdb_node *tmp_node;
    mdb_level_t tmp_level;

    exchange_child(first, first_parent, second);
    exchange_child(second, second_parent, first);

    tmp_node = first->left;
    first->left = second->left;
    second->left = tmp_node;

    tmp_node = first->right;
    first->right = second->right;
    second->right = tmp_node;

    tmp_level = first->level;
    first->level = second->level;
    second->level = tmp_level;
}

static void
exchange_remove(struct mdb_node *target, struct mdb_node *target_parent,
                struct mdb_node **current, struct mdb_node *parent,
                int dir, struct mdb_node **ret_target)
{
    errval_t err;
    assert(current);
    assert(target);
    assert(*current);
    assert(parent)
    assert(ret_target);
    assert(!*ret_target);
    assert(dir != 0);
    assert(check_subtree_invariants(*current) == 0);
    assert(COMPARE(KEY(target), KEY(*current)) != 0);
    assert(is_child(target, target_parent));
    assert(is_child(*current, parent));

    struct mdb_node *current_ = *current;

    if (dir > 0) {
        if (parent == target) {
            assert(parent->left == current_);
        }
        else {
            assert(parent->right == current_);
        }

        if (current_->right) {
            exchange_remove(target, target_parent, &current_->right,
                            current_, dir, ret_target);
            assert(check_subtree_invariants(current_->right));
        }
    }
    else if (dir < 0) {
        if (parent == target) {
            assert(parent->right == current_);
        }
        else {
            assert(parent->left == current_);
        }

        if (current_->left) {
            exchange_remove(target, target_parent, &current_->left,
                            current_, dir, ret_target);
            assert(check_subtree_invariants(current_->left));
        }
        else if (current_->right) {
            // right is null, left non-null -> current is level 0 node with
            // horizontal right link, and is also the successor of the target.
            // in this case, exchange current and current right, then current
            // (at its new position) and the target.
            struct mdb_node *new_current = current_->right;
            exchange_nodes(current_, parent, current_->right, current_);
            exchange_nodes(target, target_parent, current_, new_current);
            // "current" is now located where the target was, further up in the
            // tree. "new_current" is the node where current was. "target" is
            // where current->right was, and is a leaf, so can be dropped.
            assert(new_current->right == target);
            new_current->right = NULL;
            assert(check_subtree_invariants(new_current));
            *ret_target = current_;
            *current = new_current;
            return;
        }
    }

    if (*ret_target) {
        // implies we recursed further down to find a leaf. need to rebalance.
        current_ = rebalance(current_);
        assert(check_subtree_invariants(current_));
        *current = current_;
    }
    else {
        // found successor/predecessor leaf, exchange with target
        assert(!current->right && !current->left);
        exchange_nodes(target, target_parent, current, parent);

        // "current" is now where target was, so set as ret_target
        *ret_target = current;
        // target would be the new current, but we're removing it, so set
        // current to null. This also sets parent's corresponding child to
        // null by recursion.
        *current = NULL;
    }
}

static errval_t
subtree_remove(struct mdb_node *target, struct mdb_node **current,
               struct mdb_node *parent)
{
    errval_t err;
    assert(current);
    struct mdb_node *current_ = *current;
    assert(check_subtree_invariants(current_));
    if (!current_) {
        return MDB_ERR_NOTFOUND;
    }

    int compare = COMPARE(KEY(target), KEY(*current));
    if (compare > 0) {
        return subtree_remove(target, &current_->right, current_);
    }
    else if (compare < 0) {
        return subtree_remove(target, &current_->right, current_);
    }
    else {
        assert(current_ == target);
        if (!current_->left && !current_->right) {
            // target is leaf, just remove
            *current = NULL;
            return SYS_ERR_OK;
        }
        else if (!current_->left) {
            // move to right child then go left (dir=-1)
            struct mdb_node *new_current = NULL;
            struct mdb_node *new_right = current_->right;
            exchange_remove(target, parent, &new_right, current_, -1, &new_current);
            assert(new_current);
            current_ = new_current;
            current_->right = new_right;
        }
        else {
            // move to left child then go right (dir=1)
            struct mdb_node *new_current = NULL;
            struct mdb_node *new_left = current_->left;
            exchange_remove(target, parent, &new_left, current_, -1, &new_current);
            assert(new_current);
            current_ = new_current;
            current_->left = new_left;
        }
    }

    // rebalance after remove from subtree
    current_ = rebalance(current_);
    assert(check_subtree_invariants(current_));
    *current = current_;
}

errval_t
remove(struct mdb_node *target)
{
    return subtree_remove(target, &root, NULL);
}

static mdb_node*
subtree_find_last_le(void *key, struct mdb_node* current)
{
    if (!current) {
        return NULL
    }
    int compare = COMPARE(key, KEY(current));
    if (compare < 0) {
        // current is gt key, look for smaller node
        return subtree_find_last_le(key, current->left);
    }
    else if (compare > 0) {
        // current is lt key, attempt to find bigger current
        struct mdb_node *res = subtree_find_last_le(key, current->right);
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

mdb_node*
find_last_le(void *key)
{
    return subtree_find_last_le(key, root);
}

static mdb_node*
subtree_find_first_gt(void *key, struct mdb_node *current)
{
    if (!current) {
        return NULL;
    }
    int compare = COMPARE(key, KEY(current));
    if (compare < 0) {
        // current is gt key, attempt to find smaller node
        struct mdb_node *res = subtree_find_first_gt(key, current->left);
        if (res) {
            return res;
        }
        // smaller was lte key
        return current;
    }
    else if (compare >= 0) {
        // current is lte key, look for greater node
        return subtree_find_first_gt(key, current->right);
    }
}

mdb_node*
find_first_gt(void *key)
{
    return subtree_find_first_gt(key, root);
}
