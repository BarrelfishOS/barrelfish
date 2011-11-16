#ifndef DIST2_GETSET_H_
#define DIST2_GETSET_H_

errval_t dist_get_all(char*, char**); // TODO
errval_t dist_get(char*, char**);

errval_t dist_set(char*, ...);

errval_t dist_del(char*, ...);

// exists

#endif /* DIST2_GETSET_H_ */
