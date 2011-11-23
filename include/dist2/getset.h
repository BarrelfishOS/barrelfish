#ifndef DIST2_GETSET_H_
#define DIST2_GETSET_H_

errval_t dist_get_all(char*, char**); // TODO

errval_t dist_get(char*, char**);
errval_t dist_set(char*, ...);
errval_t dist_del(char*, ...);

errval_t dist_exists(bool, char*, ...);
errval_t dist_wait_for(char*, char**);

#endif /* DIST2_GETSET_H_ */
