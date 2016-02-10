# BEGIN LICENSE BLOCK
# Version: CMPL 1.1
#
# The contents of this file are subject to the Cisco-style Mozilla Public
# License Version 1.1 (the "License"); you may not use this file except
# in compliance with the License.  You may obtain a copy of the License
# at www.eclipse-clp.org/license.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
# the License for the specific language governing rights and limitations
# under the License. 
# 
# The Original Code is  The ECLiPSe Constraint Logic Programming System. 
# The Initial Developer of the Original Code is  Cisco Systems, Inc. 
# Portions created by the Initial Developer are
# Copyright (C) 1994-2006 Cisco Systems, Inc.  All Rights Reserved.
# 
# Contributor(s): ECRC GmbH.
# 
# END LICENSE BLOCK
#
# Make a copy of the files from the current directory to
# the library directories
#
# Environment variable ARCH is needed
#

# An eclipse for making the .sd files
#AUX_ECLIPSE = $(ROOT)/bin/$(ARCH)/eclipse
AUX_ECLIPSE = eclipse

ROOT=/usr/local/eclipse/prerelease

GRACE_SD = expr.sd grace.sd label.sd matrices.sd options.sd tkint.sd \
		toolbox.sd wake.sd
GRACE_PUB = matrix_util.pl
GRACE_PL = $(GRACE_SD:%.sd=%.pl) $(GRACE_PUB)
GRACE_TCL = control matrix varstack var constr add_menu request
GRACE_CTRL = Options.txt Makefile prop.pl

install :	grace.tar.gz

grace.tar :	$(GRACE_SD) $(GRACE_PUB)
		tar cof grace.tar $(GRACE_SD) $(GRACE_TCL) $(GRACE_PUB)
		/bin/rm *.sd

grace.src.tar:	$(GRACE_PL) $(GRACE_TCL) $(GRACE_CTRL)
		tar cof grace.src.tar $(GRACE_PL) $(GRACE_TCL) $(GRACE_CTRL)

src:		grace.src.tar.gz

# How to make .sd files from .pl files

.SUFFIXES: .pl .sd .Z .gz $(SUFFIXES)

.pl.sd:
	-$(AUX_ECLIPSE) -e 'set_error_handler(139, true/0), set_flag(variable_names,off),dump("$<")'

.Z:
	compress $<

%.gz:	%
	gzip $<
