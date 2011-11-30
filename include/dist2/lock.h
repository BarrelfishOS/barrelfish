#ifndef DIST2_LOCK_H_
#define DIST2_LOCK_H_

errval_t dist_lock(char*, char**);

errval_t dist_unlock(char* name);

// Optional
errval_t dist_is_locked(char* name);


#endif /* DIST2_LOCK_H_ */
