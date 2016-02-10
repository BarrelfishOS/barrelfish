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

PERM = a+r,u+w,go-w
DIRPERM = 2755

DESTFILE = tutorial

ROOTFILE = tutorial

CHAPTERS = $(ROOTFILE).tex \
	umsusing.tex \
	prolog.tex eprolog.tex umsterm.tex \
	icintro.tex real.tex solversintro.tex repairtut.tex \
	modelling.tex search.tex eplex.tex colgen.tex \
	constimpl.tex propiachr.tex hybrid.tex tkdebug.tex setsolver.tex \
	programanalysis.tex

EPSFIGURES = \
	appenddiff.ps mapdisplay2.ps tkinspectann.eps tktracerann.eps \
	bbs.eps mipnode.ps tkpredann.eps tktraceroptions.ps \
	boxmodel.ps resolv.eps tkpredbrowser.ps tktracerpopup.ps \
	clpexec.eps tkdelayedann.eps tkpredspyon.ps tktracerpopup2.ps \
	consnet.eps credit.eps tkfilter.ps tkquery.ps tpprob.ps \
	dbsbbs.eps tkfilterann.eps tkrefreshedgs.ps \
	eclipselogo.eps tkfiltercond.ps tktoolsmenu.ps \
	lds.eps tkinspect.ps tktopann.eps \
	mapdisplay.ps tkinspect2.ps tktracer.ps \
	locate2.eps squash2.eps interval-compare.eps lake-fields.eps

EXAMPLE_FILES = \
	debugdemo.ecl mapcolour.ecl mapdebugdemo.tcl buggy_data.map \
	eplex_eg1.ecl eplex_eg2.ecl eplex_eg3.ecl eplex_eg4.ecl \
	eplex_eg5.ecl eplex_eg6.ecl \
	lp_cut_stock.ecl \
	bags.ecl family.ecl farm.ecl greeting.ecl queens_ic.ecl	\
	queens_prolog.ecl sendmore.ecl shelves.ecl steiner.ecl

EXAMPLE_DEST =	$(EXAMPLE_FILES:%=../../doc/examples/tutorial/%) 


#-------------------  generic part -------------------

# the files that are touched by latex as a side effect
CHAPAUX = $(ROOTFILE).aux $(ROOTFILE).toc $(ROOTFILE).idx


all:	$(ROOTFILE).ps
	echo Do make install to install

#install:	../../doc/$(DESTFILE).pdf $(ROOTFILE)/$(ROOTFILE).html
#	rm -rf ../../doc/$(DESTFILE)
#	mv $(ROOTFILE) ../../doc/$(DESTFILE)


install:	../../doc/$(DESTFILE).pdf $(ROOTFILE)/$(ROOTFILE).html $(EXAMPLE_DEST)
	rm -rf ../../doc/$(DESTFILE)
	mv $(ROOTFILE) ../../doc/$(DESTFILE)

%/.stamp:
	mkdir -p $(@D)
	@/bin/chmod $(DIRPERM) $(@D)
	@touch $@; /bin/chmod $(PERM) $@

../../doc/$(DESTFILE).pdf:	$(ROOTFILE).pdf
	cp $(ROOTFILE).pdf ../../doc/$(DESTFILE).pdf
	chmod 644 ../../doc/$(DESTFILE).pdf

../../doc/examples/tutorial/%.ecl:	%.ecl ../../doc/examples/tutorial/.stamp
	/bin/cp $< $@
	-@/bin/chmod $(PERM) $@

../../doc/examples/tutorial/%.tcl:	%.tcl ../../doc/examples/tutorial/.stamp
	/bin/cp $< $@
	-@/bin/chmod $(PERM) $@

../../doc/examples/tutorial/%.map:	%.map ../../doc/examples/tutorial/.stamp
	/bin/cp $< $@
	-@/bin/chmod $(PERM) $@

# tex -> dvi -> pdf

$(ROOTFILE).dvi:	$(CHAPAUX) $(ROOTFILE).bbl $(ROOTFILE).ind $(EPSFIGURES)
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

$(ROOTFILE).pdf:	$(ROOTFILE).ps
	ps2pdf $(ROOTFILE).ps

$(ROOTFILE).ps:	$(ROOTFILE).dvi
	dvips -Ppdf -t a4 -o $(ROOTFILE).ps $(ROOTFILE)

$(ROOTFILE).ps.Z:	$(ROOTFILE).ps
	compress -f $(ROOTFILE).ps

# This preprocesses all *.tex files
# to insert links to the bip pages
biprefs:
	eclipse -e "[makerefs],do" -- *.tex


# HTML version

$(ROOTFILE)/$(ROOTFILE).html:	$(CHAPTERS)
#	/homes/ks15/latex2html/latex2html -init_file ../texinputs/latex2html-init  $(ROOTFILE)
	mkdir -p $(ROOTFILE)
	hevea -fix underscore.hva url.hva $(ROOTFILE)
	imagen -mag 3000 $(ROOTFILE)
	# replace the hard link index.html -> $(ROOTFILE).html by a soft link
	# because of problems with tar on Windows
	hacha -tocbis $(ROOTFILE).html
        # copy the files needed for the html to $(ROOTFILE) directory
	rm -f $(ROOTFILE)/index.html
	cp $(ROOTFILE)[0-9]??.* $(ROOTFILE)/
	cp $(ROOTFILE).css $(ROOTFILE)/
	cp contents_motif.gif next_motif.gif previous_motif.gif $(ROOTFILE)
	cp index.html $(ROOTFILE)/$(ROOTFILE).html
	ln -s $(ROOTFILE).html $(ROOTFILE)/index.html

clean:
	rm -f *.bak *.idx *.ind *.bbl *.aux *.dvi *.toc *.log *.ilg *.blg
	rm -f *.hind *.haux *.hind *.htoc $(ROOTFILE).image.* index.html
	rm -f $(ROOTFILE)[0-9]??.*
	rm -f $(ROOTFILE).ps $(ROOTFILE).ps.Z $(ROOTFILE).pdf
	rm -rf $(ROOTFILE)
	rm -f $(EXAMPLE_DEST)
