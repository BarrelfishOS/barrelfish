#ifndef CODE_GENERATOR_H_
#define CODE_GENERATOR_H_

#include "ast.h"

struct writer {
    char* output;
    size_t pos;
    size_t length;
};

struct parsed_object {
	struct writer name;
	struct writer attributes;
	struct writer constraints;
};

void emit(struct writer*, const char*, ...);

int ex(struct nodeObject*);
struct parsed_object* transform_query(const char*);
void free_parsed_object(struct parsed_object*);


#endif // GENERATOR_H_
