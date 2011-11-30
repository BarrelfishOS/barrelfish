#ifndef COMMON_H_
#define COMMON_H_

#define ASSERT_STRING(a, b) (assert(strcmp((a), (b)) == 0))
#define ASSERT_ERR_OK(e) (assert(err_is_ok((e))))


#endif /* COMMON_H_ */
