/**
 * \file
 * \brief Code generator header file
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

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
