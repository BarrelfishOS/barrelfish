#ifndef DIST2_BARRIER_H_
#define DIST2_BARRIER_H_

errval_t barrier_init(char* trigger);

errval_t barrier_enter();
errval_t barrier_leave();

errval_t barrier_destroy();

#endif /* DIST2_BARRIER_H_ */
