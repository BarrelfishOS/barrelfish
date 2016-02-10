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
# Copyright (C) 1993-2006 Cisco Systems, Inc.  All Rights Reserved.
# 
# Contributor(s): ___________________________________. 
# 
# END LICENSE BLOCK
#
# IDENTIFICATION	opt_svr4_sparc.sh
#
# AUTHOR		Joachim Schimpf
#
# DESCRIPTION		A postprocessor to optimize the assembler source
#			that the SUN-4 C compiler generates from emu.c.
#			The main purpose is to turn the normal emulator
#			into a threaded code emulator.
#
#			In order to make this postprocessor work,
#			emu.c must be compiled with
#
#				cc [-O[12]] -DTHREADED -DPOSTPRO -S emu.c
#
#			-DNO_ESCAPE is not allowed!
#
# USAGE			opt_sun4.sh emu.s <infile> <outfile>
#

trap 'rm -f /tmp/opt?.$$' 0

#
# Optimizer pass 0 (analysis)
#
# finds:
#	- the label of the switch table
#

cat > /tmp/opt0.$$ <<\'PASS1a\'

/L.+:/ {
	thislabel = $1
	words = 0			# check if a table follows
	getline
	while ($0 ~ /^	\.word/) {
		words++
		getline
	}
	if (words > 200) {		# switch table found
		optable = thislabel
	}
}

END {
	print optable			# pass table label to pass1
}
'PASS1a'

#
# Optimizer pass 1 (analysis)
#
# finds:
#	- the 2 code labels of the opcode switch
#	- the register that holds PP
#

cat > /tmp/opt1.$$ <<\'PASS1b\'

BEGIN {
	getline
	optable = $0				# e.g. "L2000118:"
	table_label = substr($0,1,length($0)-1)
}

$1 ~ /L.+:/ {
	line1 = $1
	getline
	line2 = $0
	getline
	if ($1 ~ /L.+:/) {
		label1 = line1
		instr = line2
		label2 = $1
	}
}
    
$2 ~ /L.+/ {
	if (index($2, table_label)) {
		loop1 = label1
		slot = instr
		loop2 = label2
	}
}

/^\tld\t.*bip_error_code_.*/ {		# an assignment to pp, remember the register
	pp = substr($2, length($2)-2, 3)
}

END {
	print pp			# pass results to pass2
	print loop1
	print slot
	print loop2
	print optable
}
'PASS1b'

#
# Optimizer pass 2 (transformation)
#
# - replace jumps to the switch labels by threaded code jumps
# - add the op_addr label to the switch table
#

cat > /tmp/opt2.$$ <<\'PASS2\'

BEGIN	{
	getline
	pp = $0					# e.g. "%i0"
	getline
	loop1def = $0
	loop1 = substr($0,1,length($0)-1)	# e.g. "L504"
	getline
	slot = $0				# add or mov instruction
	getline
	loop2 = substr($0,1,length($0)-1)	# e.g. "LY224" or "no label"
	getline
	optable = $0				# e.g. "L2000118:"
}

{
	if ($2 == loop1) {
	    if ($1 == "b") {
		getline
		if ($1 != "nop") {
		    if ($2 ~ /%o0/ || substr($2,length($2)-2,3) == pp) {
			print
			print "\tld\t[" pp "],%o0"
		    } else {
			print "\tld\t[" pp "],%o0"
			print
		    }
		} else {
		    print "\tld\t[" pp "],%o0"
		}
		print "\tjmpl\t%o0,%g0"
		print "\tadd\t" pp ",4," pp
	    } else {
		print "\t" $1 "\t.Lnewloop1"	# works even for .word
	    }
	} else if ($2 == loop2) {
	    if ($1 == "b") {
		print "\tld\t[" pp "],%o0"
		print "\tjmpl\t%o0,%g0"
		print "\tadd\t" pp ",4," pp
	    } else {
		if ($1 !~ /,a$/) {
		    print "OPTIMIZER: anull flag expected in " $0
		    exit
		}
		print "\t" $1 "\t.Lnewloop2"
		print "\tld\t[" pp "],%o0"
	    }
	    getline		# skip the useless delay slot instruction
	    if (!index($0, slot)) {
		print "OPTIMIZER: unexpected instruction " $0
		exit
	    }
	} else if ($1 == loop1def) {
	    print ".Lnewloop1:"
	    print "\tld\t[" pp "],%o0"
	    print ".Lnewloop2:"
	    print "\tjmpl\t%o0,%g0"
	    print "\tadd\t" pp ",4," pp
	    print
	} else if ($1 == optable) {
	    print "op_addr:"
	    print
	} else if ($1 == ".global" && !already_printed) {
	    already_printed = 1
	    print "\t.global\top_addr"
	    print
	} else
	    print
}
'PASS2'

#echo pass0
awk -f /tmp/opt0.$$ $1 > /tmp/opt9.$$
#echo pass1
awk -f /tmp/opt1.$$ /tmp/opt9.$$ $1 > $1.par

#echo pass2
#awk -f /tmp/opt2.$$ $1.par $1 > $2
awk -f /tmp/opt2.$$ $1.par $1 | sed -e "s/`head -1 $1.par`/%g5/g" > $2

#
# The sed script above replaces the register that holds pp by
# a global sparc register (good choices are g5, g6 or g7).
#
# Now create 2 functions to set and return the value of this register.
#

cat >> $2 <<\'EOF\'
	.section	"text"
	.proc 04
	.global get_pp
get_pp:
	retl
	mov	%g5,%o0

	.proc 04
	.global set_pp
set_pp:
	retl
	mov	%o0,%g5
'EOF'

#echo done
