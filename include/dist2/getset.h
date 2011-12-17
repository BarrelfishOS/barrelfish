#ifndef DIST2_GETSET_H_
#define DIST2_GETSET_H_

typedef uint64_t dist_mode_t;

// TODO
#define SET_DEFAULT		(0x0)
#define SET_SEQUENTIAL	(0x1)
#define SET_TRANSIENT	(0x1 << 1)

/**
 * Retrieve all record names matching a given query.
 *
 * @param names array of strings containing the names.
 * The client is responsible to free the array and individual strings
 * (@see dist_free_names()).
 * @param size Number of names returned
 * @param query Record
 *
 * @retval SYS_ERR_OK
 * TODO all retvals here
 */
errval_t dist_get_names(char*** names, size_t*, char*, ...);

void dist_free_names(char**, size_t);

/**
 * Retrieves a record matching the query.
 *
 * @param record Contains the record found or NULL in case of error.
 * @param query
 *
 * @retval SYS_ERR_OK
 * @retval DIST2_ERR_NO_RECORD
 * TODO retvals
 */
errval_t dist_get(const char*, char**);

/**
 * Stores a record in the DB.
 *
 * @param mode 0 for default behavior
 * 			   SET_SEQUENTIAL Postfixes the record name with a
 * 			   monotonically increasing number
 * @param query
 *
 * @retval SYS_ERR_OK
 * TODO retvals
 */
errval_t dist_set(dist_mode_t, char*, ...);

/**
 *  Stores a record in the DB and retrieves the stored record.
 *  @note This is useful in combination with SET_SEQUENTIAL to
 *  know what name your record has.
 *
 *  @param mode
 *  @param record Record that has been set
 *  @retval SYS_ERR_OK
 */
errval_t dist_set_get(dist_mode_t, char**, char*, ...);

/**
 * Deletes all records matching the given query.
 *
 * @param query
 */
errval_t dist_del(char*, ...);

/**
 * Tests if a given query has a result.
 *
 * @param watch In case this is true, block until the query has a result.
 * Otherwise return immediate result.
 *
 * @retval SYS_ERR_OK
 * @retval DIST2_ERR_NO_RECORD
 */
errval_t dist_exists(bool, char**, char*, ...);

/**
 * Tests if a given query has no result.
 *
 * @param watch In case this is true block until the query has no more results.
 * Otherwise return immediate result.
 * @param query
 *
 * @retval SYS_ERR_OK
 */
errval_t dist_exists_not(bool, char*, ...);

/**
 * Reads attributes from a given record.
 * %s for string
 * %f for double
 * %d for integer?
 *
 * @param record
 * @param format
 *
 * @retval SYS_ERR_OK
 */
errval_t dist_read(char*, char*, ...);


// TODO Watch not really nice yet
#define DIST_ON_SET (0x1)
#define DIST_ON_DEL (0x1 << 1)

typedef uint64_t watchid_t;
typedef void(*dist_watch_fn)(watchid_t id, char* record, void* arg);

errval_t dist_wait_for(dist_mode_t, char**, char*, ...);
//errval_t dist_watch_async(dist_mode_t, dist_watch_fn*, void* state, watchid_t* id, char*, ...);
errval_t dist_unwatch(uint64_t id);
// end todo


#endif /* DIST2_GETSET_H_ */
