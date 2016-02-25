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
# The Original Code is  CPViz Constraint Visualization System
# The Initial Developer of the Original Code is  Helmut Simonis
# Portions created by the Initial Developer are
# Copyright (C) 2009-2010 Helmut Simonis
# 
# Contributor(s): 	Helmut Simonis, 4C, Univerity College Cork, Cork
#			
# 
# END LICENSE BLOCK
# ----------------------------------------------------------------------
EXAMPLES = bibd bin bridge car costas mix nqueen party path rooms sbno schedule sendmore sonet sudoku wave
COPY = bibd/RESULT/tree48 bibd/RESULT/viz13 bin/CREDIT/viz132 car/result/viz51 costas/MODEL/viz41 mix/RESULT/viz7 nqueen/CREDIT/viz44 \
party/CREDIT/viz143 rooms/NAIVE/viz45 sendmore/FULL/viz5 sonet/T23/viz666 sudoku/FC/viz38 wave/RESULT/viz16


all: $(EXAMPLES)

.PHONY: all $(EXAMPLES)

$(EXAMPLES): 
	$(MAKE) -C $@

documentation:
	eclipse -b "documentation.ecl" -e "documentation:top"
pdf:
	for dir in $(EXAMPLES); do \
	$(MAKE) -C $$dir pdf ;\
	done

validate:
	for dir in $(EXAMPLES); do \
	$(MAKE) -C $$dir validate ;\
	done

clean:
	for dir in $(EXAMPLES); do \
	$(MAKE) -C $$dir clean ;\
	done

copy:
	for file in $(COPY);do \
	svg2pdf $$file.svg ;\
	cp $$file.pdf .;\
	done




