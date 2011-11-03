#ifndef GENERATOR_H_
#define GENERATOR_H_

#include "ast.h"

char* transform_query(const char*);
int ex(struct nodeObject*);

struct writer {
    size_t pos;
    size_t length;
    char* output;
};

#endif // GENERATOR_H_
