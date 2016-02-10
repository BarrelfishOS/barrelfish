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

DESTFILE = intro

ROOTFILE = eclipse

CHAPTERS = $(ROOTFILE).tex

EPSFIGURES = house.ps

#-------------------  generic part -------------------

# the files that are touched by latex as a side effect
#CHAPAUX = $(CHAPTERS:%.tex=%.aux) $(ROOTFILE).toc $(ROOTFILE).idx
CHAPAUX = $(ROOTFILE).aux $(ROOTFILE).toc $(ROOTFILE).idx


all:	$(ROOTFILE).dvi $(ROOTFILE)/$(ROOTFILE).html
	echo Do make install to install

install:	$(ROOTFILE).pdf $(ROOTFILE)/$(ROOTFILE).html
	rm -rf ../../doc/$(DESTFILE)
	mv $(ROOTFILE) ../../doc/$(DESTFILE)


# tex -> dvi -> ps

../../doc/$(DESTFILE).pdf:	$(ROOTFILE).pdf
	cp $(ROOTFILE).pdf ../../doc/$(DESTFILE).pdf
	chmod 644 ../../doc/$(DESTFILE).pdf

# tex -> dvi -> pdf

$(ROOTFILE).dvi:	$(CHAPAUX) $(ROOTFILE).ind $(ROOTFILE).bbl
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
	-bibtex $(ROOTFILE)

$(ROOTFILE).ind:	$(ROOTFILE).idx
	-makeindex $(ROOTFILE)

$(ROOTFILE).pdf:	$(ROOTFILE).ps $(EPSFIGURES)
	ps2pdf $(ROOTFILE).ps

$(ROOTFILE).ps:	$(ROOTFILE).dvi $(EPSFIGURES)
	dvips -Ppdf -t a4 -o $(ROOTFILE).ps $(ROOTFILE)


# This preprocesses all *.tex files
# to insert links to the bip pages
biprefs:
	eclipse -e "[makerefs],do" -- *.tex


# HTML version

$(ROOTFILE)/$(ROOTFILE).html:	$(CHAPTERS)
	mkdir -p $(ROOTFILE)
	hevea -fix underscore.hva hyperref.hva url.hva $(ROOTFILE)
	# replace the hard link index.html -> $(ROOTFILE).html by a soft link
	# because of problems with tar on Windows
	hacha -tocbis $(ROOTFILE).html
        # copy the files needed for the html to $(ROOTFILE) directory
	cp $(ROOTFILE)[0-9]??.* $(ROOTFILE)/
	cp $(ROOTFILE).css $(ROOTFILE)/
	cp contents_motif.gif next_motif.gif previous_motif.gif $(ROOTFILE)
	cp index.html $(ROOTFILE)/$(ROOTFILE).html
	ln -s $(ROOTFILE).html $(ROOTFILE)/index.html

clean:
	rm -f *.bak *.idx *.ind *.bbl *.aux *.dvi *.toc *.log *.ilg *.blg
	rm -f *.hind *.haux *.hind *.htoc $(ROOTFILE).image.* index.html
	rm -f $(ROOTFILE)[0-9]??.*
	rm -f $(ROOTFILE).ps $(ROOTFILE).ps.Z $(ROOTFILE).pdf $(ROOTFILE).html
	rm -rf $(ROOTFILE)
