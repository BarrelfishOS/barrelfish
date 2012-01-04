/**
 * \file
 * \brief Function to generate/manipulate abstract syntax tree for records.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <assert.h>

#ifdef TEST_PARSER
#include "../../../include/dist2/parser/ast.h"
#else
#include <dist2/parser/ast.h>
#endif

#include "y.tab.h"
#include "flex.h"

void free_ast(struct ast_object* p)
{
    if (!p)
        return;

    switch (p->type) {
    case nodeType_Object:
        free_ast(p->on.name);
        free_ast(p->on.attrs);
        break;

    case nodeType_Attribute:
        free_ast(p->an.attr);
        free_ast(p->an.next);
        break;

    case nodeType_String:
        free(p->sn.str);
        p->sn.str = NULL;
        break;

    case nodeType_Ident:
        free(p->in.str);
        p->in.str = NULL;
        break;

    case nodeType_Constraint:
        free_ast(p->cnsn.value);
        break;

    case nodeType_Pair:
        free_ast(p->pn.left);
        free_ast(p->pn.right);
        break;

    case nodeType_Unset:
        assert(!"nodeType_Unset encountered in free_ast!");
        abort();
        break;

    default:
        // Nothing special to do for value nodes
        break;
    }

    free(p);
}

void ast_append_attribute(struct ast_object* ast, struct ast_object* to_insert)
{
    struct ast_object** attr = &ast->on.attrs;
    for (; *attr != NULL; attr = &(*attr)->an.next) {
        // continue
    }

    struct ast_object* new_attr = ast_alloc_node();
    new_attr->type = nodeType_Attribute;
    new_attr->an.attr = to_insert;
    new_attr->an.next = NULL;
    *attr = new_attr;
}

struct ast_object* ast_find_attribute(struct ast_object* ast, char* name)
{
    struct ast_object** attr = &ast->on.attrs;

    for (; *attr != NULL; attr = &(*attr)->an.next) {

        assert((*attr)->type == nodeType_Attribute);
        if (strcmp((*attr)->an.attr->pn.left->in.str, name) == 0) {
            return (*attr)->an.attr;
        }

    }

    return NULL;
}

struct ast_object* ast_remove_attribute(struct ast_object* ast, char* name)
{
    struct ast_object** attr = &ast->on.attrs;

    for (; *attr != NULL; attr = &(*attr)->an.next) {

        assert((*attr)->type == nodeType_Attribute);
        struct ast_object* pair = (*attr)->an.attr;
        struct ast_object* left = pair->pn.left;

        if (strcmp(left->in.str, name) == 0) {
            struct ast_object* current_attr = *attr;

            *attr = current_attr->an.next;

            current_attr->an.next = NULL;
            current_attr->an.attr = NULL;
            free_ast(current_attr);

            return pair;
        }

    }

    return NULL;
}

errval_t generate_ast(const char* input, struct ast_object** record)
{
    // Save re-entrant state for Flex/Bison
    struct dist_parser_state p;
    struct string_buffer buf;

    int res = yylex_init_extra(&buf, &p.scanner);
    if (res != 0) {
        goto out;
    }

    // Run Lexer and Parser
    yy_scan_string(input, p.scanner);
    res = yyparse((void*) &p);
    yylex_destroy(p.scanner);

    if (res == 0) {
        *record = p.ast;
        p.ast = NULL;
        return SYS_ERR_OK;
    }

    out:
    // Memory got cleaned up by bison destructors...
    *record = NULL;
    return DIST2_ERR_PARSER_FAIL;
}
