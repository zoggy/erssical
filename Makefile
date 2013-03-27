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

PACKAGES=rss,str,curl
HTTPPACKAGES=nethttpd

OF_FLAGS=-package $(PACKAGES),$(HTTPPACKAGES)
OCAMLFIND=ocamlfind
OCAML_COMPFLAGS= -annot
OCAMLC=$(OCAMLFIND) ocamlc $(OF_FLAGS) $(OCAML_COMPFLAGS)
OCAMLOPT=$(OCAMLFIND) ocamlopt $(OF_FLAGS) $(OCAML_COMFLAGS)
OCAMLDOC=$(OCAMLFIND) ocamldoc $(OF_FLAGS)
OCAMLDEP=ocamldep

EXE=erssical
EXE_BYTE=$(EXE).byte

HTTPSERVER=erssical-httpd
HTTPSERVER_BYTE=$(HTTPSERVER).byte

all: byte opt
byte: erssical.cma $(EXE_BYTE) $(HTTPSERVER_BYTE)
opt: erssical.cmxa erssical.cmxs $(EXE) $(HTTPSERVER)

CMOFILES= \
	ers_types.cmo \
	ers_curl.cmo \
	ers_io.cmo \
	ers_ical.cmo \
	ers_filter.cmo \
	ers_do.cmo

CMXFILES=$(CMOFILES:.cmo=.cmx)
CMIFILES=$(CMOFILES:.cmo=.cmi)

erssical.cma: $(CMIFILES) $(CMOFILES)
	$(OCAMLC) -o $@ -a $(CMOFILES)

erssical.cmxa: $(CMIFILES) $(CMXFILES)
	$(OCAMLOPT) -o $@ -a $(CMXFILES)

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
	$(OCAMLDOC) -t "Erssical library reference" -d html -html $(CMOFILES:.cmo=.mli)

webdoc: doc
	mkdir -p ../erssical-gh-pages/refdoc
	cp html/* ../erssical-gh-pages/refdoc/
	cp web/index.html web/style.css ../erssical-gh-pages/

.depend depend:
	$(OCAMLDEP) erss*.ml erss*.mli > .depend

erstest: erssical.cmxa ers_test.ml
	$(OCAMLOPT) -package $(PACKAGES) -linkpkg -o $@ $(OCAML_COMPFLAGS) $^


# installation :
################
install:
	$(OCAMLFIND) install erssical META LICENSE \
	$(wildcard erssical.cmi erssical.cma erssical.cmxa erssical.a erssical.cmxs erssical.mli erssical.cmx)

uninstall:
	ocamlfind remove erssical

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

