#ifndef DIST2_GETSET_H_
#define DIST2_GETSET_H_

typedef uint64_t dist_mode_t;

// TODO use enum
#define SET_DEFAULT		(0x0)
#define SET_SEQUENTIAL	(0x1)
#define SET_TRANSIENT	(0x1 << 1)

errval_t dist_get_names(char*** names, size_t*, char*, ...);
void dist_free_names(char**, size_t);

errval_t dist_get(char*, char**);
errval_t dist_set(dist_mode_t, char*, ...);
errval_t dist_set_get(dist_mode_t, char**, char*, ...);

errval_t dist_del(char*, ...);

errval_t dist_exists(bool, char**, char*, ...);
errval_t dist_exists_not(bool, char*, ...);

#define DIST_ON_SET (0x1)
#define DIST_ON_DEL (0x1 << 1)

typedef uint64_t watchid_t;
typedef void(*dist_watch_fn)(watchid_t id, char* record, void* arg);

errval_t dist_wait_for(dist_mode_t, char**, char*, ...);
errval_t dist_watch_async(dist_mode_t, dist_watch_fn*, void* state, watchid_t* id, char*, ...);
errval_t dist_unwatch(uint64_t id);

errval_t dist_read(char*, char*, ...);

#endif /* DIST2_GETSET_H_ */
