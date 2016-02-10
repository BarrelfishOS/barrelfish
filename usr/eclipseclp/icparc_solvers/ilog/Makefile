% BEGIN LICENSE BLOCK
% Version: CMPL 1.1
%
% The contents of this file are subject to the Cisco-style Mozilla Public
% License Version 1.1 (the "License"); you may not use this file except
% in compliance with the License.  You may obtain a copy of the License
% at www.eclipse-clp.org/license.
% 
% Software distributed under the License is distributed on an "AS IS"
% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
% the License for the specific language governing rights and limitations
% under the License. 
% 
% The Original Code is  The ECLiPSe Constraint Logic Programming System. 
% The Initial Developer of the Original Code is  Cisco Systems, Inc. 
% Portions created by the Initial Developer are
% Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): 
% 
% END LICENSE BLOCK
# $Id: Makefile,v 1.1 2006/09/23 01:54:04 snovello Exp $

.SUFFIXES : .so


SYSTEM=sparc_5_4.2
CCC=/opt/SUNWspro/bin/CC4.1
CCLIBDIR=/opt/SUNWspro/SC4.0/lib
# g++ -fhandle-exceptions: internal error on intexp.h !
CC=/opt/SUNWspro/bin/cc
LD=/usr/ccs/bin/ld

LIBFORMAT=static

#------------------------------------------------------------
# File extension
#------------------------------------------------------------
EXT=cc

#------------------------------------------------------------
# Librairies used with examples
#------------------------------------------------------------
LIBS= -lsocket -lnsl -lm -lsunmath


#------------------------------------------------------------
# Debug flag
#------------------------------------------------------------

DEBUG=-O -DNDEBUG
#DEBUG=-g -DDEBUG
# -O -DNDEBUG
# -g -DDEBUG

#------------------------------------------------------------
#
# When you adapt this makefile to compile your SOLVER programs
# please copy this makefile and set SOLVERDIR to the directory
# where SOLVER was installed. 
#
#------------------------------------------------------------

#SOLVERDIR=/opt/ilog/solver421
#SCHEDULERDIR=/opt/ilog/sched41
SOLVERDIR=/opt/ilog/solver43
SCHEDULERDIR=/opt/ilog/sched43
ECLIPSEDIR=../..


#------------------------------------------------------------
# Compilation flags
# LDFLTFLAGS is needed to link with the Solver Floating Point
# library
#------------------------------------------------------------

SOLVERIncDir=$(SOLVERDIR)/include
SCHEDULERIncDir=$(SCHEDULERDIR)/include
ECLIPSEIncDir=$(ECLIPSEDIR)/include/sparc_sunos5

CFLAGS=$(DEBUG) $(NEWOPT) -I$(SOLVERIncDir) -I$(SCHEDULERIncDir) -I$(ECLIPSEIncDir)
LDFLAGS=-L$(SOLVERDIR)/lib/$(SYSTEM)/$(LIBFORMAT) -L$(SCHEDULERDIR)/lib/$(SYSTEM)/$(LIBFORMAT) -L$(ECLIPSEDIRLIB)
LDFLTFLAGS=-L$(SOLVERDIR)/lib/$(SYSTEM)/$(LIBFORMAT) -lilcfloat -lsolver $(LIBS) 

# ------------------------------------------------------------


# ------------------------------------------------------------

all:	$(ARCH)/ilog.so

# The examples


.cc.o :
	$(CCC) -c $(CFLAGS) $< -o $@
.c.o :
	$(CC) -c $(CFLAGS) $< -o $@
.o.so :
	$(CC) -G -o $@ $<


OFILES = stdecil.o classes.o outof.o ec2il.o ilog.o stdsched.o \
	sched_cstrs.o scheduler.o


stdecil.o : stdecil.h
classes.o : stdecil.h classes.h
ec2il.o : stdecil.h ec2il.h stdecil.h
ilog.o : stdecil.h classes.h ec2il.h


# As a result of endless tries, we use LD here instead of CCC.
# 1.	With CCC, -z text is automatically passed to the linker and
#	makes it impossible to create the .so
# 2.	To get exception handling working, we additionally link with
#	crti.o (first) and crtn.o (last)

sparc_sunos5/ilog.so : $(OFILES)
	$(LD) -dy -G -z textoff \
	    /opt/SUNWspro/SC4.0/lib/crti.o \
	    $(OFILES) \
	    -L$(SCHEDULERDIR)/lib/$(SYSTEM)/$(LIBFORMAT) -lschedule \
	    -L$(SOLVERDIR)/lib/$(SYSTEM)/$(LIBFORMAT) -lsolver \
	    -L$(CCLIBDIR) -lC /opt/SUNWspro/SC4.0/lib/crtn.o \
	    -lsunmath -o $@


clean :
	/bin/rm -rf *.o *~ 


#----------------------------------------------------------------------
# Install
#----------------------------------------------------------------------

PERM = a+r,u+w,go-w
DIRPERM = 2755
PREFIX = $(ECLIPSEDIR)
ECLIPSE = $(ECLIPSEDIR)/bin/$(ARCH)/eclipse

PUBLIC_FILES =	ilog.pl alldiff_strong.pl fd_ilog.pl scheduler.pl \
	fd_min_max.pl fd.pl fd_fd.pl

OBJ_SUFFIX =	$(OBJ_SUFFIX_$(ARCH))

OBJ_SUFFIX_sparc_sunos5 = so
OBJ_SUFFIX_sun4 = o
OBJ_SUFFIX_i386_linux = so
OBJ_SUFFIX_alpha_linux = so
OBJ_SUFFIX_i386_nt = dll


OBJ_FILES =  $(ARCH)/ilog.$(OBJ_SUFFIX)


PRIVATE_DEST =	$(PRIVATE_FILES:%=$(PREFIX)/lib_private/ilog/%) \
		$(PUBLIC_FILES:%=$(PREFIX)/lib_private/ilog/%)
PUBLIC_DEST =	$(PRIVATE_FILES:%.pl=$(PREFIX)/lib/ilog/%.sd) \
		$(PUBLIC_FILES:%=$(PREFIX)/lib/ilog/%)
OBJ_DEST =	$(OBJ_FILES:%=$(PREFIX)/lib/ilog/%) \
		$(OBJ_FILES:%=$(PREFIX)/lib_private/ilog/%) 


install:	$(PRIVATE_DEST) $(PUBLIC_DEST) $(OBJ_DEST) $(ECH_DEST)

install_obj:	$(OBJ_DEST)

%/.stamp:
	mkdir -p $(@D)
	@/bin/chmod $(DIRPERM) $(@D)
	@touch $@; /bin/chmod $(PERM) $@


$(PREFIX)/lib/ilog/%.sd:	%.pl $(PREFIX)/lib/ilog/.stamp
	-$(ECLIPSE) -e 'dump("$(*F).pl")'
	-mv $(*F).sd $@
	@/bin/chmod $(PERM) $@

$(PREFIX)/lib/ilog/%.pl:	%.pl $(PREFIX)/lib/ilog/.stamp
	/bin/cp $< $@
	@/bin/chmod $(PERM) $@

$(PREFIX)/lib_private/ilog/%.pl:	%.pl $(PREFIX)/lib_private/ilog/.stamp
	/bin/cp $< $@
	@/bin/chmod $(PERM) $@


$(PREFIX)/lib/ilog/$(ARCH)/%.$(OBJ_SUFFIX):	$(ARCH)/%.$(OBJ_SUFFIX) $(PREFIX)/lib/ilog/$(ARCH)/.stamp
	/bin/cp $< $@
	@/bin/chmod $(PERM) $@

$(PREFIX)/lib_private/ilog/$(ARCH)/%.$(OBJ_SUFFIX):	$(ARCH)/%.$(OBJ_SUFFIX) $(PREFIX)/lib_private/ilog/$(ARCH)/.stamp
	/bin/cp $< $@
	@/bin/chmod $(PERM) $@



#----------------------------------------------------------------------
# Experimental
#----------------------------------------------------------------------

donald: donald.o
	$(CCC)    $(CFLAGS) donald.o -o donald $(LDFLAGS) -lsolver -lsocket -ldl -lnsl
donald.o: donald.cc
	$(CCC) -c $(CFLAGS) donald.cc



ECLIPSEDIRLIB = $(ECLIPSEDIR)/lib/sparc_sunos5
eclipse : $(OFILES) $(ECLIPSEDIRLIB)/standalone.o
	$(CCC) -v $(DEBUG) $(ECLIPSEDIRLIB)/standalone.o $(LDFLAGS) $(OFILES) \
		-lschedule -lsolver $(ECLIPSEDIRLIB)/libeclipse.a -lgmp -lshm -ldummies \
		-lsocket -ldl -lnsl -lsunmath -o $@




cmainstat:	cmainstat.o mylib.o
	$(CCC) cmainstat.o mylib.o -ldl -o $@

cmainstat.o:	cmainstat.c
	$(CC) -c cmainstat.c -o $@


cmain.o:	cmain.c
	$(CC) -c cmain.c -o $@


# works
#cmain:	cmain.o
#	$(CCC) -v cmain.o -ldl -o $@
#
#mylib.so:	mylib.cc
#	$(CCC) -G mylib.cc -o $@


cmain:	cmain.o
	$(CC) cmain.o -ldl -o $@

mylib.so:	mylib.o
	$(LD) -dy -G /opt/SUNWspro/SC4.0/lib/crti.o mylib.o -L$(CCLIBDIR) -lC /opt/SUNWspro/SC4.0/lib/crtn.o -o $@
#	$(LD) -dy -G /opt/SUNWspro/SC4.0/lib/crti.o mylib.o -L$(CCLIBDIR) -lC -lw -lcx -lc /opt/SUNWspro/SC4.0/lib/crtn.o -o $@
#	$(LD) -dy -G /opt/SUNWspro/SC4.0/lib/crti.o /opt/SUNWspro/SC4.0/lib/values-xa.o -Y P,/opt/SUNWspro/lib:/opt/SUNWspro/SC4.0/lib:/usr/ccs/lib:/usr/lib mylib.o -lC -lC_mtstubs -lw -lcx -lc /opt/SUNWspro/SC4.0/lib/crtn.o -o $@
#	$(LD) -dy -G /opt/SUNWspro/SC4.0/lib/crti.o /opt/SUNWspro/SC4.0/lib/crt1.o /opt/SUNWspro/SC4.0/lib/values-xa.o -Y P,/opt/SUNWspro/lib:/opt/SUNWspro/SC4.0/lib:/usr/ccs/lib:/usr/lib mylib.o -lC -lC_mtstubs -lw -lcx -lc /opt/SUNWspro/SC4.0/lib/crtn.o -o $@


#mylib.so:	mylib.o
#	$(CCC) -G mylib.o -o $@
##	$(LD) -dy -G mylib.o -L$(CCLIBDIR) -lC -o $@
#
#
#mylib.o:	mylib.cc
#	$(CCC) mylib.cc -o $@
