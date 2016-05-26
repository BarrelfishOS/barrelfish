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
# Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
# 
# Contributor(s): 
# 
# END LICENSE BLOCK

DESTFILE=knowledgebase

ROOTFILE=manual

CHAPTERS = $(ROOTFILE).tex intro-sec.tex database-sec.tex knowbase-sec.tex \
	multiuser-sec.tex backwards-sec.tex
#	kernel-lst.tex database-lst.tex knowbase-lst.tex


#------------------ generic part -----------------

# the files that are touched by latex as a side effect
CHAPAUX = $(CHAPTERS:%.tex=%.aux) $(ROOTFILE).toc $(ROOTFILE).idx


all:	$(ROOTFILE).dvi

install:	$(ROOTFILE).ps.Z
	cp $(ROOTFILE).ps.Z ../../doc/$(DESTFILE).ps.Z
	chmod 644 ../../doc/$(DESTFILE).ps.Z


$(ROOTFILE).dvi:	$(ROOTFILE).bbl indexfile.tex $(CHAPAUX)
	@echo ======== UPDATE-PASS =========
	@for f in $(CHAPAUX); do \
		mv $$f $$f.old; cp $$f.old $$f; \
	    done
	latex $(ROOTFILE)
	# The touches below are supposed to work around nfs clock skew problems:
	# The latex run may leave the dvi file with a modification time in the
	# future. Touch it to make sure the time is right relative to the
	# subsequent touches of the aux files. Sleep in between to make sure
	# the dvi is strictly older than the touched aux files.
	touch $(ROOTFILE).dvi 
	sleep 1
	@for f in $(CHAPAUX); do \
		if cmp -s $$f $$f.old; then mv $$f.old $$f; \
		else rm $$f.old; echo ==== $$f was modified ====; touch $$f; \
		fi \
	    done
	$(MAKE) $(ROOTFILE).dvi

$(ROOTFILE).aux:	$(CHAPTERS)
	@echo ======== PASS 1 =========
	latex $(ROOTFILE)

$(ROOTFILE).idx:	$(ROOTFILE).aux
	touch $(ROOTFILE).idx

$(ROOTFILE).bbl:	$(ROOTFILE).aux
	bibtex $(ROOTFILE)

indexfile.tex:	$(ROOTFILE).idx
	-makeindex -o indexfile.tex $(ROOTFILE).idx

$(ROOTFILE).ps:	$(ROOTFILE).dvi
	dvips -o $(ROOTFILE).ps $(ROOTFILE)

$(ROOTFILE).ps.Z:	$(ROOTFILE).ps
	compress -f $(ROOTFILE).ps

$(ROOTFILE)/$(ROOTFILE).html:	$(CHAPTERS)
	latex2html -init_file ../texinputs/latex2html-init -html_version 3.0 $(ROOTFILE)

clean:
	rm -f *.idx *.ind *.bbl *.blg *.aux *.dvi *.toc *.log *.ilg indexfile.tex
	rm -f $(ROOTFILE).ps $(ROOTFILE).ps.Z
	rm -rf $(ROOTFILE)
