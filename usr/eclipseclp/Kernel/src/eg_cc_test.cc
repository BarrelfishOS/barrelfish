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
 * Copyright (C) 1998-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
 * ECLiPSe SAMPLE CODE
 *
 * $Id: eg_cc_test.cc,v 1.3 2012/02/25 13:47:56 jschimpf Exp $
 *
 * AUTHOR:		Joachim Schimpf
 *
 * DESCRIPTION:
 *	Test all features of the embedding interface.
 */


#include	"eclipseclass.h"
#include	<iostream>

long	longs[] = {1,2,3,4,5};
double	doubles[] = {1.1,2.2,3.3,4.4,5.5};
char	chars[] = "chars";
EC_word	pwords[5] = {0,1,2,3,4};


static void
echo_stream(int stream)
{
    char buf[1024];
    int n;

    while ((n = ec_queue_read(stream, buf, 1023)) > 0)
    {
	buf[n] = 0;
	std::cout << buf;
    }
}

static int
EC_resume_flush()
{
    int res;
    long arg;
    for(;;)
    {
    	res = ec_resume_long(&arg);
	switch (res)
	{
	case EC_flushio:
	    echo_stream((int) arg);
	    break;
	default:
	    return res;
	}
    }
}

static int
EC_handle_events_flush()
{
    int res;
    long arg;
    for(;;)
    {
    	res = ec_handle_events(&arg);
	switch (res)
	{
	case EC_flushio:
	    echo_stream((int) arg);
	    break;
	default:
	    return res;
	}
    }
}



EC_word uni(const EC_word a, const EC_word b)
{
    return term(EC_functor("=",2), a, b);
}

int
main(int argc, char **argv)
{
    char 	*s;
    EC_atom	a;
    EC_functor	f;
    long	n;
    double	d;
    EC_word 	pw1,pw2,pw3,pw4;
    
    ec_set_option_int(EC_OPTION_ARGC, argc);
    ec_set_option_ptr(EC_OPTION_ARGV, argv);
#define TEST_MEMORY_IO
#ifdef TEST_MEMORY_IO
    ec_set_option_int(EC_OPTION_IO, MEMORY_IO);
#endif

    if (ec_init())
    	goto _problem_;

  {
    /*
     * All EC_refs must be in a scope that is entered after ec_init()
     * and exited before ec_cleanup()!
     */
    EC_ref	X;
    EC_refs	YZ(2);

    /*----------------------------------------*/
    std::cout << "Testing post_goal()\n";
    /*----------------------------------------*/

    post_goal(uni(X, EC_atom("hello")));
    post_goal(term(EC_functor("atom_string",2), X, YZ[0]));
    post_goal(term(EC_functor("is",2), YZ[1],
    		term(EC_functor("string_length",1),YZ[0]) + 1));
    post_goal("writeln(done)");

    if (EC_succeed == EC_resume_flush() &&
    	EC_succeed == EC_word(X).is_atom(&a) &&
    	EC_succeed == YZ[0].is_string(&s) &&
    	EC_succeed == YZ[1].is_long(&n)
	)
    {
	std::cout << "Answer was X=" << a.name() << ", Y=" << s << ", n=" << n << "\n";
    }
    else
    	goto _problem_;

    /*----------------------------------------*/
    std::cout << "Testing constructors\n";
    /*----------------------------------------*/
    X = newvar();
    post_goal(uni(X,
    	list(list(5,doubles),
    	list(list(5,longs),
    	list(list(5,chars),
    	list("hello",
	list(EC_atom("world"),
	list(123456,
	list(3.14,
	list(term(EC_functor("five",5),pwords),
	list(list(YZ),
	list(newvar(),
	list(array(5,doubles),
	list(matrix(1,5,doubles),
	list(matrix(2,2,doubles),
    	nil())))))))))))))));
    if (EC_succeed != EC_resume())
    	goto _problem_;

    post_goal(EC_atom("garbage_collect"));
    if (EC_succeed != EC_resume())
    	goto _problem_;
    post_goal(term(EC_functor("writeln",1), X));
    if (EC_succeed != EC_resume_flush())
    	goto _problem_;

    /*----------------------------------------*/
    std::cout << "Testing assignment\n";
    /*----------------------------------------*/
    YZ.set(1,77);
    if (!(YZ[1].is_long(&n) == EC_succeed && n == 77))
    	goto _problem_;

    /*----------------------------------------*/
    std::cout << "Testing checking and decomposition\n";
    /*----------------------------------------*/
    pw1 = X;
    if (pw1.is_list(pw2,pw1) == EC_succeed &&
    	pw2.is_list(pw3,pw4) == EC_succeed &&
    	pw3.is_double(&d) == EC_succeed
    )
	std::cout << "d = " << d << "\n";
    else
    	goto _problem_;

    if (pw1.is_list(pw2,pw1) == EC_succeed &&
    	pw2.is_list(pw3,pw4) == EC_succeed &&
    	pw3.is_long(&n) == EC_succeed
    )
	std::cout << "n = " << n << "\n";
    else
    	goto _problem_;

    if (pw1.is_list(pw2,pw1) == EC_succeed &&
    	pw2.is_list(pw3,pw4) == EC_succeed &&
    	pw3.is_long(&n) == EC_succeed
    )
	std::cout << "n = " << n << "\n";
    else
    	goto _problem_;

    if (pw1.is_list(pw2,pw1) == EC_succeed &&
    	pw2.is_string(&s) == EC_succeed
    )
	std::cout << "s = " << s << "\n";
    else
    	goto _problem_;

    if (pw1.is_list(pw2,pw1) == EC_succeed &&
    	pw2.is_atom(&a) == EC_succeed
    )
	std::cout << "a = " << a.name() << "\n";
    else
    	goto _problem_;

    if (pw1.is_list(pw2,pw1) == EC_succeed &&
        pw1.is_list(pw2,pw1) == EC_succeed &&
        pw1.is_list(pw2,pw1) == EC_succeed &&
	pw2.functor(&f) == EC_succeed &&
	pw2.arg(3,pw3) == EC_succeed &&
	pw3.is_long(&n) == EC_succeed
    )
	std::cout << "functor = " << f.name() << "/" << f.arity() << ", n = " << n << "\n";
    else
    	goto _problem_;

    if (pw1.is_list(pw2,pw1) == EC_succeed &&
    	pw1.is_list(pw2,pw1) == EC_succeed &&
    	pw1.is_list(pw2,pw1) == EC_succeed &&
    	pw1.is_list(pw2,pw1) == EC_succeed &&
    	pw1.is_list(pw2,pw1) == EC_succeed &&
    	pw1.is_nil() == EC_succeed
    )
	std::cout << "end\n";
    else
    	goto _problem_;

    post_event(EC_atom("hello"));
    if (EC_handle_events_flush() != EC_succeed)
    	goto _problem_;

  }

    ec_cleanup();
    return 0;

_problem_:
    std::cout << "PROBLEM!!!\n";
    return -1;
}

