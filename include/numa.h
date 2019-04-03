/**
 * \file
 * \brief header specifying the interface of libnuma
 *
 * This is derived from:
 *
 * Linux man pages "numa"
 * libnuma from http://oss.sgi.com/projects/libnuma/
 *
 */

/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstrasse 6, CH-8092 Zurich.
 * Attn: Systems Group.
 */

#ifndef __NUMA_H
#define __NUMA_H 1

#ifdef __cplusplus
extern "C" {
#endif

///< the maximum number of nodes supported
#define NUMA_MAX_NUMNODES 16

#if NUMA_MAX_NUMNODES > MAX_NODEID
#error maximum node bigger than maximum nodeid
#endif

///< specify the local node for allocation
#define NUMA_NODE_LOCAL ((nodeid_t)-1)

///< error value for the numa node size
#define NUMA_NODE_INVALID ((uintptr_t)-1)

///< error value for invalid cores
#define NUMA_CORE_INVALID ((coreid_t)-1);


typedef enum numa_policy {
    NUMA_POLICY_DEFAULT,   ///< default numa policy
    NUMA_POLICY_STRICT,    ///< strict numa policy
    NUMA_POLICY_PREFERRED  ///< preferred memory policy
} numa_policy_t;

///< typedef for the nodemask
typedef struct bitmask nodemask_t;

/**
 * \brief checks if numa support is available
 *
 * \returns NUMA_ERR_NOT_AVAILABLE  value all other functions are undefined
 *          SYS_ERR_OK:             NUMA functionality is available
 *
 * this function must be called before any of the other functions of libnuma.
 * during the call to numa_available the library also gets initialized
 */
errval_t numa_available(void);

/**
 * \brief returns the highest node number available on the current system.
 *
 * \returns ID of the max NUMA node
 */
nodeid_t numa_max_node(void);

/**
 * \brief returns the highest ID of the present cores
 *
 * \returns the maximum coreID in the system
 */
coreid_t numa_max_core(void);

/**
 * \brief returns the current node the domain is running on
 *
 * \return ID of the current node
 */
nodeid_t numa_current_node(void);

/**
 * \brief returns the size of the node mask
 *
 * \return size of the node mask
 */
nodeid_t numa_num_possible_nodes(void);

/**
 * \brief Obtains the maximum number of nodes the system can handle
 *
 * \return maximum nodes supported
 *
 * returns the number of the highest possible node in a system. In other words,
 * the size of a kernel type nodemask_t (in bits) minus 1
 */
static inline nodeid_t numa_max_possible_node(void)
{
    return numa_num_possible_nodes() - 1;
}

/**
 * \brief Obtains the number of all memory nodes in the system
 *
 * \return number of memory nodes in the system
 *
 * returns the number of memory nodes in the system. This count includes any nodes
 * that are currently disabled.
 */
nodeid_t numa_num_configured_nodes(void);

/**
 * \brief obtains the nodes the domain is allowed to allocate memory from
 *
 * \returns bitmask representing the allowing nodes
 *
 * returns the mask of nodes from which the process is allowed to allocate memory
 * in it's current cpuset context.
 */
struct bitmap *numa_get_mems_allowed(void);

/**
 * \brief returns the total numberof CPUs in the system
 *
 * \returns total number of CPUs in the system
 *
 * returns the number of cpus in the system. This count includes any cpus that are
 * currently disabled.
 */
coreid_t numa_num_configured_cpus(void);

/**
 * \brief bitmask that is allocated by the library with bits representing all nodes
 *        on which the calling task may allocate memory.
 */
extern struct bitmap *numa_all_nodes_ptr;

/**
 * \brief points to a bitmask that is allocated by the library and left all zeroes.
 */
extern struct bitmap *numa_no_nodes_ptr;

/**
 * \brief points to a bitmask that is allocated by the library with bits
 *        representing all cpus on which the calling task may execute.
 */
extern struct bitmap *numa_all_cpus_ptr;

/**
 * \brief returns the number of cpus that the calling domain is allowed to use.
 *
 * \returns number of CPUs the domain is allowed to use
 */
coreid_t numa_num_task_cpus(void);

/**
 * \brief returns the number of nodes on which the calling domain is allowed to
 *        allocate memory
 *
 * \returns number of nodes the domain is allowed to use
 */
nodeid_t numa_num_task_nodes(void);

/**
 * \brief parses line , which is a character string
 *
 * \param line  character string to parse
 * \param mask  bitmap to store the result
 *
 * \returns SYS_ERR_OK            on SUCCESS
 *          NUMA_ERR_BITMAP_PARSE on FAILURE
 *
 * The string contains the hexadecimal representation of a bit map.
 *
 * XXX according to the man pages this function is only used internally
 */
errval_t numa_parse_bitmap(char *line, struct bitmap *mask);

/**
 * \brief parses a character string list of nodes into a bit mask.
 *
 * \param string character string to parse
 *
 * \returns NUMA bitmask on SUCCESS
 *          NULL if the string is invalid
 *
 * The string is a comma-separated list of node numbers or node ranges
 * Examples: 1-5,7,10 !4-5 +0-3
 *
 * If the string length is zero, then the numa_no_nodes_ptr is returned
 */
struct bitmap *numa_parse_nodestring(char *string);

/**
 * \brief parses a character string list of cpus into a bit mask.
 *
 * \param string character string to parse
 *
 * \returns NUMA bitmask on SUCCESS
 *          NULL if the string is invalid
 *
 * The string is a comma-separated list of cpu numbers or cpu ranges
 * Examples: 1-5,7,10 !4-5 +0-3
 */
struct bitmap *numa_parse_cpustring(char *string);

/**
 * \brief obtains the size of a node
 *
 * \param node  ID of the NUMA node
 * \param freep
 *
 * \returns size of the node in bytes
 *
 * returns the memory size of a node. If the argument freep is not NULL, it used
 * to return the amount of free memory on the node. On error it returns
 * NUMA_NODE_INVALID
 */
size_t numa_node_size(nodeid_t node, uintptr_t *freep);

///< alias for NUMA node size 64bit variants
#define numa_node_size64(_node, _freep) numa_node_size(_node, _freep)

/**
 * \brief obtains the base address of the numa node
 *
 * \returns physical address of the start of the numa node
 */
lpaddr_t numa_node_base(nodeid_t node);

/**
 * \brief returns the preferred node of the current task.
 *
 * \returns node ID where memory is preferably allocated
 */
nodeid_t numa_preferred(void);

/**
 * \brief  sets the preferred node for the current task to node
 *
 * \param node  ID of the node to set preferred
 *
 * The system will attempt to allocate memory from the preferred node, but will
 * fall back to other nodes if no memory is available on the the preferred node
 *
 * Passing a node of -1 argument specifies local allocation
 */
void numa_set_preferred(nodeid_t node);

/**
 * \brief   returns the current interleave mask
 *
 * \returns bitmask representing the current interleave state
 *
 * returns the current interleave mask if the task's memory allocation policy is
 * page interleaved. Otherwise, this function returns an empty mask.
 */
struct bitmap *numa_get_interleave_mask(void);

/**
 * \brief sets the memory interleave mask for the current task to nodemask
 *
 * \param nodemask bitmask representing the nodes
 *
 * All new memory allocations are page interleaved over all nodes in the interleave
 * mask. Interleaving can be turned off again by passing an empty mask.
 *
 * This bitmask is considered to be a hint. Fallback to other nodes may be possible
 */
void numa_set_interleave_mask(struct bitmap *nodemask);

/**
 * \brief binds the current task and its children to the nodes specified in nodemask.
 *
 * \param nodemask  bitmap representing the nodes
 */
void numa_bind(struct bitmap *nodemask);

/**
 * \brief sets the memory allocation policy for the calling task to local allocation.
 */
void numa_set_localalloc(void);

/**
 * \brief sets the memory allocation mask.
 *
 * \param nodemask  bitmap representing the nodes
 *
 * The task will only allocate memory from the nodes set in nodemask.
 *
 * an empty mask or not allowed nodes in the mask will result in an error
 */
errval_t numa_set_membind(struct bitmap *nodemask);

/**
 * \brief returns the mask of nodes from which memory can currently be allocated.
 *
 * \return bitmap of nodes from which can be allocated
 */
struct bitmap *numa_get_membind(void);

/**
 * \brief allocates memory on a specific node.
 *
 * \param size      size of the region in bytes
 * \param node      ID of the node to allocate from
 * \param pagesize  page size to be used for the mapping
 *
 * \returns pointer to memory region
 *
 * The size argument will be rounded up to a multiple of the system page size.
 * if the specified node is externally denied to this process, this call will fail.
 * The memory must be freed with numa_free(). On errors NULL is returned.
 */
void *numa_alloc_onnode(size_t size, nodeid_t node, size_t pagesize);

/**
 * \brief allocates size bytes of memory on the local node
 *
 * \param size  size of the memory region in bytes
 * \param pagesize  page size to be used for the mapping
 *
 * \returns pointer to memory region
 *
 * The memory must be freed with numa_free(). On errors NULL is returned.
 */
void *numa_alloc_local(size_t size, size_t pagesize);

/**
 * \brief allocates size bytes of memory page interleaved on all nodes.
 *
 * \param size      size of the memory region in bytes
 * \param pagesize  page size to be used for the mapping
 *
 * \returns pointer to the mapped memory region
 *
 * should only be used for large areas consisting of multiple pages.
 * The memory must be freed with numa_free(). On errors NULL is returned.
 */
void *numa_alloc_interleaved(size_t size, size_t pagesize);

/**
 * \brief allocates size bytes of memory page interleaved the nodes specified in
 *        the nodemask.
 *
 * \param size     size of the memory region in bytes
 * \param nodemask subset of nodes to consider for allocation
 * \param pagesize  page size to be used for the mapping
 *
 * \returns pointer to the mapped memory region
 *
 * should only be used for large areas consisting of multiple pages.
 * The memory must be freed with numa_free(). On errors NULL is returned.
 */
void *numa_alloc_interleaved_subset(size_t size, size_t pagesize,
                                    struct bitmap *nodemask);

/**
 * \brief allocates size bytes of memory with the current NUMA policy.
 *
 * \param size      size of the memory region in bytes
 * \param pagesize  page size to be used for the mapping
 * \returns pointer to the mapped memory region
 *
 * The memory must be freed with numa_free(). On errors NULL is returned.
 */
void *numa_alloc(size_t size, size_t pagesize);

/**
 * \brief changes the size of the memory area.
 *
 * \param old_addr  pointer ot the old memory region
 * \param old_size  size of the old memory region
 * \param new_size  new size to allocate
 */
void *numa_realloc(void *old_addr, size_t old_size, size_t new_size);

/**
 * \brief frees size bytes of memory starting at start
 *
 * \param start start of the memory region
 * \param size  number of bytes to free
 *
 * the memory must be previously allocated by one of the numa_alloc* functions
 */
void numa_free(void *start, size_t size);

/**
 * \brief allocates RAM on a specific node
 *
 * \param dest      capref to store the RAM cap
 * \param size      size of the RAM region to allocated
 * \param node      node on which the frame should be allocated
 * \param ret_size  returned size of the frame capability
 *
 * \returns SYS_ERR_OK on SUCCESS
 *          errval on FAILURE
 */
errval_t numa_ram_alloc_on_node(struct capref *dest, size_t size,
                                nodeid_t node, size_t *ret_size);

/**
 * \brief allocates a frame on a specific node
 *
 * \param dest      capref to store the frame
 * \param size      size of the frame to allocated
 * \param node      node on which the frame should be allocated
 * \param ret_size  returned size of the frame capability
 *
 * \returns SYS_ERR_OK on SUCCESS
 *          errval on FAILURE
 */
errval_t numa_frame_alloc_on_node(struct capref *dest,
                                  size_t size,
                                  nodeid_t node,
                                  size_t *ret_size);

/**
 * \brief allocates a frame on the local node
 *
 * \param dest      capref to store the frame
 * \param size      size of the frame to allocated
 * \param ret_size  returned size of the frame capability
 *
 * \returns SYS_ERR_OK on SUCCESS
 *          errval on FAILURE
 */
static inline errval_t numa_frame_alloc_local(struct capref *dest,
                                              size_t size,
                                              size_t *ret_size)
{
    return numa_frame_alloc_on_node(dest, size, numa_current_node(), ret_size);
}

/**
 * \brief frees a previously allocated frame
 *
 * \param frame capability to free
 */
errval_t numa_frame_free(struct capref frame);

/**
 * \brief runs the current domain on a specific node.
 *
 * \param node  ID of the node to run the domain on
 *
 * \returns SYS_ERR_OK on SUCCESS
 *          errval on FAILURE
 *
 * Passing -1 permits the kernel to schedule on all nodes again
 */
errval_t numa_run_on_node(nodeid_t node);

/**
 * \brief runs the current domain only on nodes specified in nodemask.
 *
 * \param nodemask bitmap representing the nodes to run the domain on
 *
 * \returns SYS_ERR_OK on SUCCESS
 *          errval on FAILURE
 */
errval_t numa_run_on_node_mask(struct bitmap *nodemask);

/**
 * \brief returns a mask of CPUs on which the current task is allowed to run.
 *
 * \returns bitmap represening the coreids the domain is allowed to run
 */
struct bitmap *numa_get_run_node_mask(void);

/**
 * \brief specify the memory bind policy
 *
 * \param strict numa policy to apply
 *
 * specifies whether calls that bind memory to a specific node should use the preferred policy or a strict policy.
 */
void numa_set_bind_policy(numa_policy_t strict);

/**
 * \brief enable or disable the strict allocation policy
 *
 * \param strict numa policy to apply
 *
 * s a flag that says whether the functions allocating on specific nodes should
 * use a strict policy. Strict means the allocation will fail if the memory cannot
 * be allocated on the target node.
 */
void numa_set_strict(numa_policy_t strict);

/**
 * \brief reports the distance in the machine topology between two nodes
 *
 * \param from source node to measure the distance
 * \param to   target node to measure the distance
 *
 * \returns distance between two nodes
 *          0 iff cannot be deterimed
 *
 * The factors are a multiple of 10.  A node has distance 10 to itself.
 */
uint32_t numa_distance(nodeid_t from, nodeid_t to);

/**
 * \brief retrieves a bitmask of the cpus on which a domain may run
 *
 * \param did   domain ID
 * \param mask  returned bitmask
 *
 * \returns SYS_ERR_OK on success
 *          errval on FAILURE
 */
errval_t numa_sched_getaffinity(domainid_t did, struct bitmap *mask);

/**
 * \brief sets a domain's allowed cpu's to those cpu's specified in mask.
 *
 * \param did   domain ID
 * \param mask  bitmap representing the CPUs
 *
 * \returns SYS_ERR_OK on success
 *          errval on FAILURE
 */
errval_t numa_sched_setaffinity(domainid_t did, struct bitmap *mask);

/**
 * \brief returns the page size
 *
 * \returns the number of bytes in a page
 */
size_t numa_pagesize(void);

/**
 * \brief converts a node number to a bitmask of CPUs
 *
 * \param node  the ID of the node
 * \param mask  bitmap representing the CPUs of this node
 *
 * \return  SYS_ERR_OK on SUCCESS
 *          NUMA_ERR_BITMAP_RANGE on FAILURE (too small bitmap)
 *
 * The user must pass a bitmask structure with a mask buffer long enough to
 * represent all possible cpu's
 */
errval_t numa_node_to_cpus(nodeid_t node, struct bitmap *mask);


/**
 * \brief gets the number of cores for the given numa node
 *
 * \param node NUMA node to get the number of cores
 *
 * \returns number of cores for the node
 */
coreid_t numa_num_node_cpus(nodeid_t node);

/**
 * \brief gets the system's core ID for a node/local core id configuration
 *
 * \param
 */
coreid_t numa_node_get_core(nodeid_t node, coreid_t local_core_id);


/**
 * \brief returns the node that a cpu belongs to
 *
 * \param cpu   ID of the core
 *
 * \returns node ID on SUCCESS
 *          NUMA_NODE_INVALID on FAILURE
 */
nodeid_t numa_node_of_cpu(coreid_t cpu);

/**
 * \brief allocates a bit mask to represent the cores in the system
 *
 * \returns pointer to a new bitmask
 *          NULL on failure
 */
struct bitmap *numa_allocate_cpumask(void);

/**
 * \brief frees a previously allocated CPU bitmask
 *
 * \param cpumask pointer to a previously allocated CPU bitmask
 */
void numa_free_cpumask(struct bitmap *cpumask);

/**
 * \brief allocates a bit mask to represent the nodes in the system
 *
 * \returns pointer to a new bitmask
 *          NULL on failure
 */
struct bitmap *numa_allocate_nodemask(void);

/**
 * \brief frees a previously allocated node bitmask
 *
 * \param nodemask pointer to a previously allocated node bitmask
 */
void numa_free_nodemask(struct bitmap *nodemask);

/**
 * \brief allocates a bitmask structure and its associated bit mask
 *
 * \param n the number of bits
 *
 * \returns pointer to the bitmask
 *          NULL on error
 */
struct bitmap *numa_bitmask_alloc(unsigned int n);

/**
 * \brief sets all bits in the bit mask to 0.
 *
 * \param bmp   pointer to the bitmap
 *
 * \returns pointer to the cleared bit map
 */
struct bitmap *numa_bitmask_clearall(struct bitmap *bmp);

/**
 * \brief clears the n-th bit of a bitmask
 *
 * \param bmp   the bitmask
 * \param n     the bit to clear
 *
 * \returns pointer to the bitmask
 */
struct bitmap *numa_bitmask_clearbit(struct bitmap *bmp, unsigned int n);

/**
 * \brief checks if two bitmasks are equal
 *
 * \param bmp1  bitmask 1
 * \param bmp2  bitmask 2
 *
 * \return TRUE if the bitmasks are equal
 *         FALSE if the are distinct
 */
bool numa_bitmask_equal(const struct bitmap *bmp1, const struct bitmap *bmp2);

/**
 * \brief frees the memory of a bitmask
 *
 * \param bmp the bitmask to be freed
 */
void numa_bitmask_free(struct bitmap *bmp);

/**
 * \brief checks if the n-th bit is set in the bitmask
 *
 * \param bmp   the bitmap
 * \param n     which bit to check
 *
 * \returns TRUE if the n-th bit is set
 *          FALSE otherwise
 */
bool numa_bitmask_isbitset(const struct bitmap *bmp, unsigned int n);

/**
 * \brief returns the size (in bytes) of the bit mask
 *
 * \param bmp   the bitmask
 *
 * \returns the size of the memory in bytes rounded up to a multiple of wordsize
 */
size_t numa_bitmask_nbytes(struct bitmap *bmp);

/**
 * \brief sets all bits of a bitmask to 1
 *
 * \param bmp the bitmask
 *
 * \returns the bitmask
 */
struct bitmap *numa_bitmask_setall(struct bitmap *bmp);

/**
 * \brief sets the n-th bit of a bitmask to 1
 *
 * \param bmp   the bitmask
 * \param n     which bit to activate
 *
 * \returns the bitmask
 */
struct bitmap *numa_bitmask_setbit(struct bitmap *bmp, unsigned int n);

/**
 * \brief copies the bitmask to a nodemask
 *
 * \param bmp       the bitmask to copy
 * \param nodemask  the destination nodemask
 *
 * If the two areas differ in size, the copy is truncated to the size of the
 * receiving field or zero-filled.
 */
void copy_bitmask_to_nodemask(struct bitmap *bmp, nodemask_t *nodemask);

/**
 * \brief copies the contents of a nodemask into the bitmask
 *
 * \param nodemask  node mask to copy from
 * \param bmp       bitmap to copy into
 *
 * If the two areas differ in size, the copy is truncated to the size of the
 * receiving field or zero-filled.
 */
void copy_nodemask_to_bitmask(nodemask_t *nodemask, struct bitmap *bmp);

/**
 * \brief copies one bitmask into another
 *
 * \param bmpfrom   the source bitmask
 * \param bmpto     the destination bitmask
 *
 * If the two areas differ in size, the copy is truncated to the size of the
 * receiving field or zero-filled.
 */
void copy_bitmask_to_bitmask(struct bitmap *bmpfrom, struct bitmap *bmpto);

/**
 * \brief returns a count of the bits that are set in the body of the bitmask
 *
 * \param bmp   the bitmask to count the set bits
 *
 * \return number of set bits in this bitmask
 */
uint32_t numa_bitmask_weight(const struct bitmap *bmp);

/**
 * \brief  moves a list of pages in the address space of the current domain
 *
 * \param did    the domain ID
 * \param count  number of pages to move
 * \param pages  list of pages
 * \param nodes  list of nodes to which the pages can be moved
 * \param status returns the outcome for each page
 * \param flags  flags for moving the pages
 *
 * \returns SYS_ERR_OK on SUCCESS
 */
errval_t numa_move_pages(domainid_t did,
                         size_t count,
                         void **pages,
                         const nodeid_t *nodes,
                         errval_t *status,
                         int flags);
/**
 * \brief migrate a domain from one set of nodes to another
 *
 * \param did        the domain ID
 * \param fromnodes  bitmap representing the current nodes
 * \param tonodes    bitmap representing the
 *
 * \returns SYS_ERR_OK on SUCCESS
 */
errval_t numa_migrate_pages(domainid_t did,
                            struct bitmap *fromnodes,
                            struct bitmap *tonodes);

/**
 * is a libnuma internal function that can be overridden by the user program. This
 * function is called with a char * argument when a libnuma function fails.
 * Overriding the library internal definition makes it possible to specify a
 * different error handling strategy when a libnuma function fails. It does not
 * affect numa_available(). The numa_error() function defined in libnuma prints an
 * error on stderr and terminates the program if numa_exit_on_error is set to a
 * non-zero value. The default value of numa_exit_on_error is zero.
 *
 * \param where
 */
void numa_error(char *where);

extern int numa_exit_on_error;
extern int numa_exit_on_warn;

/**
 * is a libnuma internal function that can be also overridden by the user program.
 * It is called to warn the user when a libnuma function encounters a non-fatal
 * error. The default implementation prints a warning to stderr. The first argument
 * is a unique number identifying each warning. After that there is a printf(3)-style
 * format string and a variable number of arguments. numa_warn exits the program
 * when numa_exit_on_warn is set to a non-zero value. The default value of
 * numa_exit_on_warn is zero.
 *
 * \param number
 * \param where
 */
void numa_warn(int number, char *where, ...);

#ifdef __cplusplus
}
#endif

#endif /* __NUMA_H */
