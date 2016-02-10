/* BEGIN LICENSE BLOCK
 * Version: CMPL 1.1
 *
 * The contents of this file are subject to the Cisco-style Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file except
 * in compliance with the License.  You may obtain a copy of the License
 * at www.eclipse-clp.org/license.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
 * the License for the specific language governing rights and limitations
 * under the License. 
 * 
 * The Original Code is  The ECLiPSe Constraint Logic Programming System. 
 * The Initial Developer of the Original Code is  Cisco Systems, Inc. 
 * Portions created by the Initial Developer are
 * Copyright (C) 1997-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
 * ECLiPSe LIBRARY MODULE
 *
 * $Id: eclipse_cc.cc,v 1.4 2015/01/14 01:31:09 jschimpf Exp $
 *
 *
 * IDENTIFICATION:	eclipse_cc.cc
 *
 * AUTHOR:		Joachim Schimpf
 * AUTHOR:		Stefano Novello
 *
 * DESCRIPTION:
 *	C++ embedding interface classes
 *
 *	This file is preprocessed at eclipse-build-time to produce
 *	eclipse_cc.h. The main reason to do this is to not pollute
 *	the user's namespace with all the define's in our .h files.
 */

#define EC_EXTERNAL
#define EC_EMBED

#include "config.h"
#include "ec_public.h"
#include "types.h"
#include "embed.h"

enum EC_status
{
	EC_succeed = PSUCCEED,
	EC_fail = PFAIL,
	EC_throw = PTHROW,
	EC_yield = PYIELD,
	EC_running = PRUNNING,
	EC_waitio = PWAITIO,
	EC_flushio = PFLUSHIO
};

/*
 * classes
 */

class EC_atom;
class EC_functor;
class EC_word;
class EC_ref;
class EC_refs;

/*----------------------------------------------------------------------*/
class EC_atom
{
    public:
	dident d;

	EC_atom() {}

	EC_atom(char * s) { d = ec_did(s,0); }

	EC_atom(dident did)
	{
	    if (DidArity(did))
		ec_panic("atom arity != 0", "EC_atom::EC_atom(dident d)");
	    d = did;
	}

	char * Name() { return DidName(d); }
	char * name() { return DidName(d); }
};

/*----------------------------------------------------------------------*/
class EC_functor
{
    public:
	dident d;

	EC_functor() {}

	EC_functor(const char * s,int arity)
	{
	    if (!arity)
		ec_panic("functor arity == 0", "EC_functor::EC_functor(char*,int)");
	    d = ec_did(s, arity);
	}

	EC_functor(dident did)
	{
	    if (!DidArity(did))
		ec_panic("functor arity == 0", "EC_functor::EC_functor(dident d)");
	    d = did;
	}

	char * Name() { return DidName(d); }
	char * name() { return DidName(d); }
	int Arity() { return DidArity(d); }
	int arity() { return DidArity(d); }
};


/*----------------------------------------------------------------------*/
class EC_word
{
	friend class EC_ref;
	friend class EC_refs;

	pword w;

    public:
    	EC_word(const pword& pw)
	{
	    w = pw;
	}


    	EC_word()
	{
	}

    	EC_word&
	operator=(const EC_word& ew)
	{
	    w = ew.w;
	    return *this;
	}

    	EC_word(const char *s)
	{
	    w = ec_string(s);
	}

    	EC_word(const int l, const char *s)
	{
	    w = ec_length_string(l, s);
	}

    	EC_word(const EC_atom did)
	{
	    w = ec_atom(did.d);
	}

    	EC_word(const long l)
	{
	    w = ec_long(l);
	}

    	EC_word(const long long l)
	{
	    w = ec_long_long(l);
	}

    	EC_word(const int i)
	{
	    w = ec_long((long)i);
	}

    	EC_word(const double d)
	{
	    w = ec_double(d);
	}

    	EC_word(const EC_ref& ref);

    	friend EC_word
	term(const EC_functor functor,const EC_word args[]);

	friend EC_word
	term(const EC_functor functor,	const EC_word arg1,
					const EC_word arg2,
					const EC_word arg3,
					const EC_word arg4);
	friend EC_word
	term(const EC_functor functor,	const EC_word arg1,
					const EC_word arg2,
					const EC_word arg3,
					const EC_word arg4,
					const EC_word arg5,
					const EC_word arg6,
					const EC_word arg7,
					const EC_word arg8,
					const EC_word arg9,
					const EC_word arg10);

	friend EC_word
	list(const EC_word hd, const EC_word tl);

	/*
    	EC_word&
	list(const EC_word hd, const EC_word tl)
	{
	    w = ec_list(hd.w,tl.w);
	    return *this;
	}
	*/


    /* Type testing */

	int
	is_atom(EC_atom* did)
	{
		return ec_get_atom(w,(dident*) did);
	}
	
	int
	is_string(char **s)
	{
		return ec_get_string(w,s);
	}
	
	int
	is_string(char **s, long *len)
	{
		return ec_get_string_length(w,s,len);
	}
	
	int
	is_long(long * l)
	{
		return ec_get_long(w,l);
	}
	
	int
	is_long_long(long long * l)
	{
		return ec_get_long_long(w,l);
	}
	
	int
	is_double(double * d)
	{
		return ec_get_double(w,d);
	}

	int
	is_handle(const t_ext_type *cl, t_ext_ptr *data)
	{
		return ec_get_handle(w,cl,data);
	}
	
	int
	free_handle(const t_ext_type *cl)
	{
		return ec_free_handle(w,cl);
	}
	
	int
	is_list(EC_word& hd, EC_word& tl)
	{
		return ec_get_list(w, &hd.w, &tl.w);
	}

	int
	is_nil()
	{
		return ec_get_nil(w);
	}

	int
	is_var()
	{
		return ec_is_var(w);
	}

	int
	arity()
	{
		return ec_arity(w);
	}

	int
	functor(EC_functor* did)
	{
		return ec_get_functor(w, (dident*) did);
	}

	int
	arg(const int n,EC_word& arg)
	{
		return ec_get_arg(n, w, &arg.w);
	}

	friend int
	compare(const EC_word& term1, const EC_word& term2);

	friend int
	operator==(const EC_word& term1, const EC_word& term2);

	friend int
	unify(EC_word term1, EC_word term2);

	int
	unify(EC_word term)
	{
	    return ec_unify(w, term.w);
	}

	int
	schedule_suspensions(int n)
	{
	    return ec_schedule_suspensions(w, n);
	}

	friend void
	post_goal(const EC_word term);

	friend int
	EC_resume(EC_word term, EC_ref& chp);

	friend int
	EC_resume(EC_word term);
	
	friend int
	post_event(EC_word term);

};


inline int
compare(const EC_word& term1, const EC_word& term2)
{
    return ec_compare(term1.w, term2.w);
}

inline int
operator==(const EC_word& term1, const EC_word& term2)
{
    return ec_compare(term1.w, term2.w) == 0;
}

inline int
unify(EC_word term1, EC_word term2)
{
    return ec_unify(term1.w, term2.w);
}

inline void
post_goal(const EC_word term)
{
	ec_post_goal(term.w);
}

inline int
EC_resume(EC_word term)
{
    return ec_resume2(term.w,0);
}

inline int
post_event(EC_word term)
{
    return ec_post_event(term.w);
}

inline void
post_goal(const char * s)
{
	ec_post_string(s);
}


/*----------------------------------------------------------------------*/
class EC_refs
{

    protected:
	ec_refs r;

    public:
    	EC_refs(int size)
	{
		r = ec_refs_create_newvars(size);
	}

    	EC_refs(int size,EC_word init)
	{
		r = ec_refs_create(size,init.w);
	}

    	~EC_refs()
	{
		ec_refs_destroy(r);
	}

	int size()
	{
		return ec_refs_size(r);
	}

	EC_word
	operator[](int index)
	{
		return EC_word(ec_refs_get(r,index));
	}

	friend EC_word
	list(EC_refs& array);

	void set(int index, EC_word new_value)
	{
	    ec_refs_set(r, index, new_value.w);
	}

};


inline EC_word
list(EC_refs& array)
{
	return EC_word(ec_listofrefs(array.r));
}


/*----------------------------------------------------------------------*/
class EC_ref
{
	friend class EC_word;

    protected:
	ec_refs r;

    public:
    	EC_ref()
	{
		r = ec_refs_create_newvars(1);
	}

    	EC_ref(EC_word init)
	{
		r = ec_refs_create(1,init.w);
	}

    	~EC_ref()
	{
		ec_refs_destroy(r);
	}

    	EC_ref& operator=(const EC_word word);


	void cut_to()
	{
	    ec_cut_to_chp(r);
	}

	friend int
	EC_resume(EC_ref& chp);

	friend int
	EC_resume(EC_word term, EC_ref& chp);

};


inline int
EC_resume()
{
    return ec_resume1(0);
}

inline int
EC_resume(EC_ref& chp)
{
    return ec_resume1(chp.r);
}

inline int
EC_resume(EC_word term, EC_ref& chp)
{
    return ec_resume2(term.w,chp.r);
}

inline EC_word
EC_arg(int n)
{
    return EC_word(ec_arg(n));
}

inline EC_word::EC_word(const EC_ref& ref)
{
	w = ec_refs_get(ref.r,0);
}

inline EC_ref&
EC_ref::operator=(const EC_word word)
{
	ec_refs_set(r,0,word.w);
	return *this;
}
	

/*----------------------------------------------------------------------
 * More EC_word constructors
 *----------------------------------------------------------------------*/

inline EC_word
term(const EC_functor functor,const EC_word args[])
{
    EC_word t(ec_term_array(functor.d,(pword *) args));
    return t;
}

inline EC_word
term(const EC_functor functor,	const EC_word arg1,
				const EC_word arg2 = 0,
				const EC_word arg3 = 0,
				const EC_word arg4 = 0)
{
    EC_word the_term(ec_term(functor.d,arg1.w,arg2.w,arg3.w,arg4.w));
    return the_term;
}

inline EC_word
term(const EC_functor functor,	const EC_word arg1,
				const EC_word arg2,
				const EC_word arg3,
				const EC_word arg4,
				const EC_word arg5,
				const EC_word arg6 = 0,
				const EC_word arg7 = 0,
				const EC_word arg8 = 0,
				const EC_word arg9 = 0,
				const EC_word arg10 = 0)
{
    EC_word the_term(ec_term(functor.d,arg1.w,arg2.w,arg3.w,arg4.w,
				arg5.w,arg6.w,arg7.w,arg8.w,arg9.w,arg10.w));
    return the_term;
}

inline EC_word
list(const EC_word hd, const EC_word tl)
{
    EC_word t(ec_list(hd.w,tl.w));
    return t;
}

inline EC_word
list(int size, double* array)
{
	return EC_word(ec_listofdouble(size, array));
}

inline EC_word
list(int size, long* array)
{
	return EC_word(ec_listoflong(size, array));
}

inline EC_word
list(int size, char* array)
{
	return EC_word(ec_listofchar(size, array));
}

inline EC_word
array(int size, double* array)
{
	return EC_word(ec_arrayofdouble(size, array));
}

inline EC_word
matrix(int rows, int cols, double* array)
{
	return EC_word(ec_matrixofdouble(rows, cols, array));
}


inline EC_word
handle(const t_ext_type *cl, const t_ext_ptr data)
{
    return ec_handle(cl, data);
}

inline EC_word
newvar()
{
    EC_word t(ec_newvar());
    return t;
}

inline EC_word
nil()
{
    EC_word t(ec_nil());
    return t;
}

	
#define OP2(CNAME,DID)					\
inline EC_word CNAME(const EC_word a,const EC_word b)	\
{							\
	return term(EC_functor((DID)),a,b);		\
}

#define OP1(CNAME,DID)			\
inline EC_word CNAME(const EC_word a)	\
{					\
	return term(EC_functor((DID)),a);	\
}

OP2(operator+, ec_.d.plus)
OP1(operator+, ec_.d.plus1)
OP2(operator-, ec_.d.minus)
OP1(operator-, ec_.d.minus1)
OP2(operator*, ec_.d.times)
OP2(operator/, ec_.d.quotient)
OP2(operator%, ec_.d.modulo)
OP2(operator>>, ec_.d.rshift)
OP2(operator<<, ec_.d.lshift)
OP2(operator&, ec_.d.and2)
OP2(operator|, ec_.d.or2)
OP2(pow, ec_.d.power)
OP1(operator~, ec_.d.bitnot)
OP1(abs, ec_.d.abs)
OP1(sin, ec_.d.sin)
OP1(cos, ec_.d.cos)
OP1(tan, ec_.d.tan)
OP1(asin, ec_.d.asin)
OP1(acos, ec_.d.acos)
OP1(atan, ec_.d.atan)
OP1(sqrt, ec_.d.sqrt)
OP1(ln, ec_.d.ln)
OP1(fix, ec_.d.fix)
OP1(round, ec_.d.round)
/* The following cause conflict with system-defined macros:
OP2(min, ec_.d.min)
OP2(max, ec_.d.max)
*/
