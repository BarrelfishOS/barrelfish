/*
 * prolog_query.h
 *
 *  Created on: Nov 14, 2011
 *      Author: gz
 */

#ifndef SKB_QUERY_H_
#define SKB_QUERY_H_

#include <barrelfish/barrelfish.h>
#include <include/skb_server.h>

#include "code_generator.h"

errval_t get_record(struct parsed_object*, struct skb_query_state*);
errval_t set_record(struct parsed_object*, struct skb_query_state*);
errval_t del_record(struct parsed_object*, struct skb_query_state*);

#endif /* SKB_QUERY_H_ */

