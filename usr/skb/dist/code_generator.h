#ifndef CODE_GENERATOR_H_
#define CODE_GENERATOR_H_

#include <barrelfish/barrelfish.h>
#include <dist2/parser/ast.h>

struct writer {
    char* output;
    size_t pos;
    size_t length;
};

struct skb_record {
	struct writer name;
	struct writer attributes;
	struct writer constraints;
};

void emit(struct writer*, const char*, ...);
errval_t transform_record(struct ast_object*, struct skb_record**);
void free_parsed_object(struct skb_record*);


#endif // GENERATOR_H_
