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
# IDENTIFICATION	opt_sun3.sh
#
# AUTHOR		Joachim Schimpf
#
# DESCRIPTION		A postprocessor to optimize the assembler source
#			that the SUN-3 C compiler generates from emu.c.
#			The main purpose is to turn the normal emulator
#			into a threaded code emulator.
#
#			In order to make this postprocessor work,
#			emu.c must be compiled with
#
#				cc [-O[12]] -DTHREADED ... -S emu.c
#
#			-DNO_ESCAPE is not allowed!
#
#			Haven't tried with GNU yet.
#
# USAGE			opt_sun3.sh <infile> <outfile>
#
# NOTE	Since the C compiler generates switch tables with relative offsets,
#	it cannot be used directly as the op_addr[] array. Therefore
#	we insert the label _opswitch_addr for the switch table.
#	When sepia is booted, the op_addr[] array must be computed from
#	the switch table (function opaddr_init()). This function is dependent
#	of the offset size (16 or 32 bit, according to cc's -J option)
#	and must be updated accordingly. This postprocessor can handle both.

trap 'rm -f /tmp/opt?.$$' 0 2

#
# Optimizer pass 1 (analysis)
#
# find:
#	- the code labels of the opcode switch
#	- the label of the switch table
#	- the register that holds PP
#

cat > /tmp/opt1.$$ <<\'PASS1\'

/L.+:/ {
	thislabel = $1
	words = 0			# check if a table follows
	getline
	while ($0 ~ /^\t\.(short|long)/) {
		words++
		getline
	}
	if (words == 0) {		# not a table
	    line1 = thislabel		# this might be the switch label
	} else if (words > 200) {	# switch table found
	    loop1 = line1		# remember all the information
	    optable = thislabel
	}
}

/^\tmovl\t.*_bip_error_code_.*/ {	# an assignment to pp, remember the register
	pp = substr($2, length($2)-1, 2)
}

END {
	print pp			# pass results to pass2
	print loop1
	print optable
}
'PASS1'

#
# Optimizer pass 2 (transformation)
#
# - replace jumps to the switch labels by threaded code jumps
# - add the op_addr label to the switch table
#

cat > /tmp/opt2.$$ <<\'PASS2\'

BEGIN	{
	getline
	pp = $0					# e.g. "a5"
	getline
	loop1def = $0
	loop1 = substr($0,1,length($0)-1)	# e.g. "L504"
	getline
	optable = $0				# e.g. "L2000118:"
}

{
	if ($2 == loop1) {
	    if ($1 == "jra") {
		print "\tmovl\t" pp "@+,a0"
		print "\tjmp\ta0@"
	    } else {
		print "\t" $1 "\tLnewloop1"
	    }
	} else if ($1 == loop1def) {
	    print "Lnewloop1:"
	    print "\tmovl\t" pp "@+,a0"
	    print "\tjmp\ta0@"
	    print
	} else if ($1 == optable) {
	    print "_opswitch_table:"
	    print
	} else if (!already_printed && $1 == ".globl" && $2 == "_emulc") {
	    already_printed = 1
	    print "\t.globl\t_opswitch_table"
	    print
	} else
	    print
}
'PASS2'

# echo pass1
awk -f /tmp/opt1.$$ $1 > $1.par
# echo pass2
awk -f /tmp/opt2.$$ $1.par $1 > $2
# echo done
