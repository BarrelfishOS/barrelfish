#ifndef CODE_GENERATOR_H_
#define CODE_GENERATOR_H_

#include <barrelfish/barrelfish.h>
#include <dist2/parser/ast.h>
#include <eclipse.h>

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




struct skb_ec_terms {
    pword name;
    pword attribute_list;
    pword constraint_list;
};


errval_t transform_record2(struct ast_object* ast, struct skb_ec_terms* record);

#endif // GENERATOR_H_
