###############################################################################
#               Erssical                                                      #
#                                                                             #
#   Copyright (C) 2013 Institut National de Recherche en Informatique         #
#   et en Automatique. All rights reserved.                                   #
#                                                                             #
#   This program is free software; you can redistribute it and/or modify      #
#   it under the terms of the GNU Lesser General Public License version       #
#   3 as published by the Free Software Foundation.                           #
#                                                                             #
#   This program is distributed in the hope that it will be useful,           #
#   but WITHOUT ANY WARRANTY; without even the implied warranty of            #
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             #
#   GNU Library General Public License for more details.                      #
#                                                                             #
#   You should have received a copy of the GNU Library General Public         #
#   License along with this program; if not, write to the Free Software       #
#   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                  #
#   02111-1307  USA                                                           #
#                                                                             #
#   Contact: Maxence.Guesdon@inria.fr                                         #
#                                                                             #
#                                                                             #
###############################################################################

#
VERSION=0.1

# do not forget to change in META file too
PACKAGES=rss,str,cohttp.lwt,xtmpl,lwt.ppx,lwt.unix,uri,lru-cache

OF_FLAGS=-package $(PACKAGES)
OCAMLFIND=ocamlfind
OCAML_COMPFLAGS= -annot -rectypes
OCAMLC=$(OCAMLFIND) ocamlc $(OF_FLAGS) $(OCAML_COMPFLAGS)
OCAMLOPT=$(OCAMLFIND) ocamlopt $(OF_FLAGS) $(OCAML_COMFLAGS)
OCAMLDOC=OCAMLFIND_COMMANDS="ocamldoc=ocamldoc.opt" $(OCAMLFIND) ocamldoc -predicates native $(OF_FLAGS) -v
OCAMLDEP=ocamldep
BINDIR=$(shell dirname `which ocamlc`)
CP=cp -f

EXE=erssical
EXE_BYTE=$(EXE).byte

HTTPSERVER=erssical-httpd
HTTPSERVER_BYTE=$(HTTPSERVER).byte

all: byte opt
byte: erssical.cma $(EXE_BYTE) $(HTTPSERVER_BYTE)
opt: erssical.cmxa erssical.cmxs $(EXE) $(HTTPSERVER)

CMOFILES= \
	ers_types.cmo \
	ers_log.cmo \
	ers_io.cmo \
	ers_fetch.cmo \
	ers_ical.cmo \
	ers_filter.cmo \
	ers_auth.cmo \
	ers_xtmpl.cmo \
	ers_do.cmo

CMXFILES=$(CMOFILES:.cmo=.cmx)
CMIFILES=$(CMOFILES:.cmo=.cmi)

erssical.cma: $(CMIFILES) $(CMOFILES)
	$(OCAMLC) -o $@ -a $(CMOFILES)

erssical.cmxa: $(CMIFILES) $(CMXFILES)
	$(OCAMLOPT) -o $@ -a $(CMXFILES)

$(EXE): erssical.cmxa ers_main.cmx
	$(OCAMLOPT) -o $@ -package $(PACKAGES) -linkpkg $^

$(EXE_BYTE): erssical.cma ers_main.cmo
	$(OCAMLC) -o $@ -package $(PACKAGES) -linkpkg $^

$(HTTPSERVER): erssical.cmxa ers_http.cmx
	$(OCAMLOPT) -o $@ -package $(PACKAGES),$(HTTPPACKAGES) -linkpkg $^

$(HTTPSERVER_BYTE): erssical.cma ers_http.cmo
	$(OCAMLC) -o $@ -package $(PACKAGES),$(HTTPPACKAGES) -linkpkg $^

.PHONY: doc depend

doc: all
	mkdir -p html
	$(OCAMLDOC) -rectypes -t "Erssical library reference" -d html -html $(CMOFILES:.cmo=.mli)

depdoc: all
	mkdir -p html
	$(OCAMLDOC) -rectypes -t "Erssical library reference" -d html \
	-intro ocamldoc_index.text -g odoc_depgraph.cmxs $(CMOFILES:.cmo=.mli) $(CMOFILES:.cmo=.ml)

webdoc:
	cd web && make

.depend depend:
	$(OCAMLDEP) ers*.ml ers*.mli > .depend

erstest: erssical.cmxa ers_test.ml
	$(OCAMLOPT) -package $(PACKAGES) -linkpkg -o $@ $(OCAML_COMPFLAGS) $^


# installation :
################
install: install-lib install-bin

install-lib:
	$(OCAMLFIND) install erssical META LICENSE ers*.mli \
	$(wildcard erssical.cmi erssical.cma erssical.cmxa erssical.a erssical.cmxs erssical.mli erssical.cmx)

install-bin:
	for i in $(EXE) $(EXE_BYTE) $(HTTPSERVER) $(HTTPSERVER_BYTE); do \
		if test -f $$i; then $(CP) $$i $(BINDIR)/; fi; \
	done

uninstall: uninstall-lib uninstall-bin

uninstall-lib:
	ocamlfind remove erssical

uninstall-bin:
	for i in $(EXE) $(EXE_BYTE) $(HTTPSERVER) $(HTTPSERVER_BYTE); do \
		rm -f $(BINDIR)/$$i ; done

# archive :
###########
archive:
	git archive --prefix=erssical-$(VERSION)/ HEAD | gzip > ../erssical-gh-pages/erssical-$(VERSION).tar.gz

# Cleaning :
############
clean:
	-$(RM) *.cm* *.a *.annot *.o
	-$(RM) -r html
	-$(RM) $(EXE) $(EXE_BYTE)
	-$(RM) $(HTTPSERVER) $(HTTPSERVER_BYTE)

# headers :
###########
HEADFILES=Makefile *.ml *.mli
.PHONY: headers noheaders
headers:
	headache -h header -c .headache_config $(HEADFILES)

noheaders:
	headache -r -c .headache_config $(HEADFILES)

# generic rules :
#################
.SUFFIXES: .mli .ml .cmi .cmo .cmx .mll .mly .sch .html .mail

%.cmi:%.mli
	$(OCAMLC) -c $(OCAML_COMPFLAGS) $<

%.cmo:%.ml
	$(OCAMLC) -c $(OCAML_COMPFLAGS) $<

%.cmi %.cmo:%.ml
	$(OCAMLC) -c $(OCAML_COMPFLAGS) $<

%.cmx %.o:%.ml
	$(OCAMLOPT) -c $(OCAML_COMPFLAGS) $<

%.cmxs: %.cmxa
	$(OCAMLOPT) -I . -shared -linkall -o $@ $<

include .depend

