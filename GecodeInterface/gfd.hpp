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
 * Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): Kish Shen, CrossCore Optimization. 
 * 
 * END LICENSE BLOCK */

#include <gecode/int.hh>
#include <gecode/minimodel.hh>
#include <gecode/search.hh>
/* not used in Gecode >= 3.6.0
#include <gecode/graph.hh>
#include <gecode/scheduling.hh>
*/
#include <vector>

#if defined(__APPLE__) && defined(__MACH__)
/* Mac OS X */

# define VisAtt __attribute__((visibility ("default")))

#else

# define VisAtt

#endif

using namespace Gecode;

class GecodeSpace : public Gecode::MinimizeSpace {
public:
    Gecode::IntVarArgs vInt;   // vInt[0] not used, to match ECLiPSe 
    Gecode::BoolVarArgs vBool; // for linked booleans
    Gecode::BoolVar booltrue;  // true and false constants, for convenience
    Gecode::BoolVar boolfalse;
    Gecode::IntVar vCost;      // objective val for min

    GecodeSpace(void) : vInt(*this, 1, 0, 0), vBool(*this, 0, 0, 0), 
			booltrue(*this, 1, 1), boolfalse(*this, 0, 0),
			vCost(*this, 0, 0),
			first(true),
			d_args(NULL),
			actp(NULL),
			valid_snapshot(false) {}

    std::vector<int>  dom_snapshot;

    void set_snapshot() {valid_snapshot = true;}
    void clear_snapshot() {valid_snapshot = false;}
    bool snapshot_valid() {return valid_snapshot;}

    void stop_caching() {first = false;}
    void start_caching() {first = true;}
    bool is_first() {return first;}

    void set_d_args(double* m) {delete d_args; d_args = m;}
    double get_d_args(int i) const {
	return ((d_args != NULL) ? d_args[i] : 0.0);
    }

    // for using activity for var selection in C++
    bool init_select_activity(IntVarArgs& vars, double decay, IntBranchMerit initf) {
	if (actp == NULL) {
	    actp = new IntActivity(*this, vars, decay, initf);
	    return true;
	} else
	    return false;
    }
    double get_select_activity(int idx) {
	return ((actp != NULL) ? (*actp)[idx] : -1);
    }

  GecodeSpace(bool share, GecodeSpace& s) : vInt(s.vInt.size()), vBool(s.vBool.size()),
					    valid_snapshot(s.valid_snapshot),
					    first(true),
	                                    booltrue(*this, 1, 1), boolfalse(*this, 0, 0),
					    d_args(NULL),
					    actp(NULL),
					    Gecode::MinimizeSpace(share,s) {
//	valid_snapshot = s.valid_snapshot;
	if (snapshot_valid()) dom_snapshot = s.dom_snapshot;
	for (int i=vInt.size(); i--;)
	  vInt[i].update(*this, share, s.vInt[i]);
	for (int i=vBool.size(); i--;)
	  vBool[i].update(*this, share, s.vBool[i]);
	vCost.update(*this, share, s.vCost);
	if (s.actp != NULL) actp = new IntActivity(*(s.actp));
	
  }

    virtual Gecode::MinimizeSpace*
    copy(bool share) {
	return new GecodeSpace(share,*this);
    }

    virtual Gecode::IntVar cost(void)  const {
	return vCost;
    }

    ~GecodeSpace(void) {
	delete d_args;
	delete actp;
    }

private:
    bool valid_snapshot;
    bool first;  // true if not recomputing 
    double* d_args; // args for passing information for activity calls 
    Gecode::IntActivity* actp; // for using IntActivity outside search engines
    // change to using vector to lift limit of 1 IntActivity per space
};

enum SearchMethod {METHOD_COMPLETE, 
		   METHOD_RESTART, 
		   METHOD_CONTINUE_BAB,
		   METHOD_RESTART_BAB,
                   METHOD_RESTART_RBAB};

// taken from gecode/driver/script.hpp, by Christian Schulte
class Cutoff : public Gecode::Search::Stop {
private:
    Search::NodeStop* ns; ///< Used node stop object
    Search::FailStop* fs; ///< Used fail stop object
    Search::TimeStop* ts; ///< Used time stop object
    //    Search::MemoryStop* ms; ///< Used memory stop object
    long stop_reason;
public:
    /// Initialize stop object
    Cutoff(unsigned int node, unsigned int fail, unsigned int time/*, size_t mem*/)
	: ns((node > 0) ? new Search::NodeStop(node) : NULL),
	  fs((fail > 0) ? new Search::FailStop(fail) : NULL),
	  ts((time > 0) ? new Search::TimeStop(time) : NULL),
	  //  ms((mem  > 0) ? new Search::MemoryStop(mem) : NULL),
	  stop_reason(0) {}
    /// Reason why search has been stopped
    enum {
	SR_NODE = 1 << 2, ///< Node limit reached
	SR_FAIL = 1 << 3, ///< Fail limit reached
	SR_TIME = 1 << 4 ///< Time limit reached
	//SR_MEM  = 1 << 5  ///< Memory limit reached
    };
    /// Test whether search must be stopped
    virtual bool stop(const Search::Statistics& s, const Search::Options& o) {
	bool stopping;
	stopping =
	    ((ns != NULL) && ns->stop(s,o)) ||
	    ((fs != NULL) && fs->stop(s,o)) ||
	    //    ((ms != NULL) && ms->stop(s,o)) ||
	    ((ts != NULL) && ts->stop(s,o));
	if (stopping) {
	    this->stop_reason =
		(((ns != NULL) && ns->stop(s,o)) ? SR_NODE : 0) |
		(((fs != NULL) && fs->stop(s,o)) ? SR_FAIL : 0) |
		//(((ms != NULL) && ms->stop(s,o)) ? SR_MEM  : 0) |
		(((ts != NULL) && ts->stop(s,o)) ? SR_TIME : 0);
	}
	return stopping;
    }
    /// Report reason why search has been stopped
    long reason(void) {
	return this->stop_reason;
    }
    /// Reset (currently only for timer)
    void reset(void) {
	if (ts != NULL) ts->reset();
    }
    /// Destructor
    ~Cutoff(void) {
	delete ns; delete fs; delete ts; //delete ms;
    }
};

/* based on code from Christian Schulte posted on Gecode mailing list, 
   2013-08-27
*/
class GecodeEngineBase {
public:
    GecodeEngineBase(void) {}
    virtual GecodeSpace* next(void) = 0;
    virtual Search::Statistics statistics(void) const = 0;
    virtual bool stopped(void) const = 0;
    virtual ~GecodeEngineBase(void) {}
};

class GecodeDFS : public GecodeEngineBase {
protected:
    DFS<GecodeSpace> e;

public:
    GecodeDFS(GecodeSpace* solver, const Search::Options& o) : e(solver,o) {}
    virtual GecodeSpace* next(void) {
	return e.next();
    }
    virtual Search::Statistics statistics(void) const {
	return e.statistics();
    }
    virtual bool stopped(void) const {
	return e.stopped();
    }
    virtual ~GecodeDFS(void) {}
};

class GecodeBAB : public GecodeEngineBase {
protected:
    BAB<GecodeSpace> e;

public:
    GecodeBAB(GecodeSpace* solver, const Search::Options& o) : e(solver,o) {}
    virtual GecodeSpace* next(void) {
	return e.next();
    }
    virtual Search::Statistics statistics(void) const {
	return e.statistics();
    }
    virtual bool stopped(void) const {
	return e.stopped();
    }
    virtual ~GecodeBAB(void) {}
};

template<template<class> class E>
class GecodeRBS : public GecodeEngineBase {
protected:
    RBS<E,GecodeSpace> e;

public:
    GecodeRBS(GecodeSpace* solver, const Search::Options& o) : e(solver,o) {}
    virtual GecodeSpace* next(void) {
	return e.next();
    }
    virtual Search::Statistics statistics(void) const {
	return e.statistics();
    }
    virtual bool stopped(void) const {
	return e.stopped();
    }
    virtual ~GecodeRBS(void) {}
};


class GecodeSearch {
private:
    GecodeEngineBase* sengine;
public:
    SearchMethod method;
    Cutoff* stopp;

    GecodeSearch(GecodeSpace* solver, Search::Options o, unsigned extra,
		 Cutoff* cutoffp, 
		 const SearchMethod& m) : stopp(cutoffp), method(m) {
	switch (m) {
	case METHOD_COMPLETE:
	    sengine = new GecodeDFS(solver,o);
	    break;
	case METHOD_CONTINUE_BAB:
	    solver->vCost = solver->vInt[extra];
	    sengine = new GecodeBAB(solver,o);
	    break;
	case METHOD_RESTART_BAB:
	    solver->vCost = solver->vInt[extra];
	    o.cutoff = Search::Cutoff::constant(ULONG_MAX);
	    sengine = new GecodeRBS<BAB>(solver, o);
	    break;
	case METHOD_RESTART_RBAB:
	    // cutoff and nogoods limit set already for o
	    solver->vCost = solver->vInt[extra];
	    sengine = new GecodeRBS<BAB>(solver, o);
	    break;
	case METHOD_RESTART:
	    sengine = new GecodeRBS<DFS>(solver, o);
	    break;
	    /*
	default:
	    printf("Error\n");
	    break;*/
	}
    }

    GecodeSpace* next(void) {
	//	return static_cast<Gecodeace*>(sengine->next());
	return sengine->next();
    }

    Search::Statistics statistics(void) const {
	return sengine->statistics();
    }

    bool stopped(void) const {
	return sengine->stopped();
    }

    ~GecodeSearch(void) {
	delete sengine;
	delete stopp;
    }
};

class LDSBSymsStore {
public:

    std::queue<SymmetryHandle>* symsp; // symmetries
    std::queue<IntVar*>* intsp;        // dummy IntVars representing integers

    LDSBSymsStore(void) : symsp(new std::queue<SymmetryHandle>()),
			  intsp(new std::queue<IntVar*>()) {}

    ~LDSBSymsStore(void) {
	delete symsp;
	delete intsp;
    }
}; 


class VarSelectH {
public :
    int* idxs;
    bool act_inited;

    VarSelectH(void) : idxs(NULL), act_inited(false) {}

    ~VarSelectH(void) {
	delete idxs;
    }

};

class Ec2gcException {};

