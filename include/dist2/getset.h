#ifndef DIST2_GETSET_H_
#define DIST2_GETSET_H_

typedef uint64_t dist_mode_t;

#define SET_DEFAULT		(0x0)
#define SET_SEQUENTIAL	(0x1)
#define SET_TRANSIENT	(0x1 << 1)

#define WATCH_NONE		(0x0)
#define WATCH_DEL		(0x1)
#define WATCH_CREATE	(0x1 << 1)
#define WATCH_UPDATE	(0x1 << 2)


errval_t dist_get_names(char*** names, size_t*, char*, ...);
void dist_free_names(char**, size_t);

errval_t dist_get(char*, char**);
errval_t dist_set(dist_mode_t, char*, ...);
errval_t dist_del(char*, ...);

errval_t dist_exists(bool, char*, ...);
errval_t dist_wait_for(char*, char**);

#endif /* DIST2_GETSET_H_ */
