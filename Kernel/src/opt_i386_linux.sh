#!/bin/sh
#
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
# Copyright (C) 1992-2006 Cisco Systems, Inc.  All Rights Reserved.
# 
# Contributor(s): ___________________________________. 
# 
# END LICENSE BLOCK
#
# IDENTIFICATION	opt_i386_linux.sh
#
# AUTHOR		Joachim Schimpf
#
# DESCRIPTION		A postprocessor to optimize the assembler source
#			that gcc 3.x creates for i386_linux.  The threaded
#			code is compiled with indirect jumps like:
#
#			    jmplabel:
#				jmp	*%eax
#
#				<code for wam instruction>
#				jmp	jmplabel
#
#			and this postprocessor replaces every
#				jmp	jmplabel
#			with
#				jmp	*%eax
#
# USAGE			opt_i386_linux.sh <infile> <outfile>
#

trap 'rm -f /tmp/opt?.$$' 0

cat > /tmp/opt0.$$ <<\'PASS1a\'

BEGIN {
	jmplabel = ""
}

/^\.?L[0-9]+:/ {
	if (jmplabel == "") {
	    print
	    thislabel = $1
	    getline
	    if ($0 ~ /jmp	\*%/) {
		# found the threaded code jump, remember label and instruction
		jmplabel = substr(thislabel,1,length(thislabel)-1)
		jmpinstr = $0
	    }
	}
}

/jmp	\.?L[0-9]+/ {
	if ($2 == jmplabel) {
	    # replace indirect by direct jump
	    print jmpinstr
	    next
	}
}

{
	print
}
'PASS1a'


awk -f /tmp/opt0.$$ $1 > $2
